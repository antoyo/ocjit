(*
 * Copyright (C) 2014  Boucher, Antoni <bouanto@gmail.com>
 * 
 * This program is free software: you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation, either
 * version 3 of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 *)

open Ctypes
open Ocjit

let compile_mul_add jit_function =
    let x = jit_value_get_param jit_function 0 in
    let y = jit_value_get_param jit_function 1 in
    let z = jit_value_get_param jit_function 2 in

    let temp1 = jit_insn_mul jit_function x y in
    let temp2 = jit_insn_add jit_function temp1 z in
    jit_insn_return jit_function temp2;
    1

let () =
    let context = jit_context_create () in

    let params = [ jit_type_sys_int; jit_type_sys_int; jit_type_sys_int ] in
    let params_array = CArray.of_list jit_type params in
    let signature = jit_type_create_signature JitAbiCdecl jit_type_sys_int (CArray.start params_array) (List.length params) 1 in
    let jit_function = jit_function_create context signature in
    jit_function_set_recompilable jit_function;
    jit_function_set_on_demand_compiler jit_function compile_mul_add;

    let result = allocate int 0 in
    let arguments = List.map to_voidp [ allocate int 3; allocate int 5; allocate int 2 ] in
    let arguments_array = CArray.of_list (ptr void) arguments in
    jit_function_apply jit_function (CArray.start arguments_array) (to_voidp result);

    print_string "mul_add(3, 5, 2) = ";
    print_int (!@ result);
    print_endline "";

    jit_context_build_start context;
    let compiler = jit_function_get_on_demand_compiler jit_function in
    let _ = compiler jit_function in
    jit_function_compile jit_function;
    jit_context_build_end context;

    let result = allocate int 0 in
    let arguments = List.map to_voidp [ allocate int 4; allocate int 6; allocate int 3 ] in
    let arguments_array = CArray.of_list (ptr void) arguments in
    jit_function_apply jit_function (CArray.start arguments_array) (to_voidp result);

    print_string "mul_add(4, 6, 3) = ";
    print_int (!@ result);
    print_endline "";

    jit_function_clear_recompilable jit_function;

    jit_context_destroy context
