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

let () =
    let context = jit_context_create () in
    jit_context_build_start context;

    let params = [ jit_type_sys_int; jit_type_sys_int ] in
    let params_array = CArray.of_list jit_type params in
    let signature = jit_type_create_signature JitAbiCdecl jit_type_sys_int (CArray.start params_array) (List.length params) 1 in
    let jit_function = jit_function_create context signature in

    let x = jit_value_get_param jit_function 0 in
    let y = jit_value_get_param jit_function 1 in

    let temp1 = jit_insn_eq jit_function x y in

    let label1 = allocate jit_label jit_label_undefined in

    jit_insn_branch_if_not jit_function temp1 label1;

    jit_insn_return jit_function x;

    jit_insn_label jit_function label1;

    let label2 = allocate jit_label jit_label_undefined in
    let temp2 = jit_insn_lt jit_function x y in
    jit_insn_branch_if_not jit_function temp2 label2;

    let temp = jit_insn_sub jit_function y x in
    let temp_arguments = CArray.of_list jit_value [ x; temp ] in
    let temp3 = jit_insn_call jit_function "gcd" jit_function 0 (CArray.start temp_arguments) (CArray.length temp_arguments) 0 in
    jit_insn_return jit_function temp3;

    jit_insn_label jit_function label2;
    let temp = jit_insn_sub jit_function x y in
    let temp_arguments = CArray.of_list jit_value [ temp; y ] in
    let temp4 = jit_insn_call jit_function "gcd" jit_function 0 (CArray.start temp_arguments) (CArray.length temp_arguments) 0 in
    jit_insn_return jit_function temp4;

    jit_function_compile jit_function;
    jit_context_build_end context;

    let result = allocate int 0 in
    let arguments = List.map to_voidp [ allocate int 375; allocate int 625 ] in
    let arguments_array = CArray.of_list (ptr void) arguments in
    jit_function_apply jit_function (CArray.start arguments_array) (to_voidp result);

    print_string "gcd(375, 625) = ";
    print_int (!@ result);
    print_endline "";

    jit_context_destroy context
