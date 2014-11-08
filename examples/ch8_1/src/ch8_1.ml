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

let print_newline () = print_endline ""

let () =
    let context = jit_context_create () in
    jit_context_build_start context;

    let signature = jit_type_create_signature jit_type_void [ jit_type_sys_int ] in
    let jit_function = jit_function_create context signature in

    let print_int_signature = jit_type_create_signature jit_type_void [ jit_type_sys_int ] in
    (*let print_newline_signature = jit_type_create_signature jit_type_void [] in*)

    let x = jit_value_get_param jit_function 0 in

    let _ = jit_insn_call_native jit_function "print_int" print_int print_int_signature [ x ] in

    (*let _ = jit_insn_call_native jit_function "print_newline" print_newline print_newline_signature [ x ] in*)

    jit_function_compile jit_function;
    jit_context_build_end context;

    jit_function_apply jit_function [ sys_int_param 42 ] no_return;

    jit_context_destroy context
