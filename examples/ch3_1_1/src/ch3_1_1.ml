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

open OcjitBind

let () =
    let context = jit_context_create () in
    jit_context_build_start context;

    let signature = jit_type_create_signature jit_type_float32 [ jit_type_float32; jit_type_float32; jit_type_float32 ] in
    let jit_function = jit_function_create context signature in

    let x = jit_value_get_param jit_function 0 in
    let y = jit_value_get_param jit_function 1 in
    let z = jit_value_get_param jit_function 2 in

    let temp1 = jit_insn_mul jit_function x y in
    let temp2 = jit_insn_add jit_function temp1 z in
    jit_insn_return jit_function temp2;

    jit_function_compile jit_function;
    jit_context_build_end context;

    let result = float32_return in
    jit_function_apply jit_function [ float32_param 3.5; float32_param 5.5; float32_param 2.5 ] result;

    print_string "mul_add(3.5, 5.5, 2.5) = ";
    print_float (get_return result);
    print_endline "";

    jit_context_destroy context
