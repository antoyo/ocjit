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

(* Types *)

type 'a return_type = (jit_type * 'a any_ptr)
type 'a typ = (jit_type * ('a -> void_ptr))

type 'a signature = {
    signature_return_type: 'a return_type;
    signature_parameter_types : ('a typ) list;
}

(* Context *)

let context = jit_context_create ()

(* Values *)

let return_float32 = (jit_type_float32, float32_return)
let return_sys_char = (jit_type_sys_char, sys_char_return)
let return_sys_double = (jit_type_sys_double, sys_double_return)
let return_sys_int = (jit_type_sys_int, sys_int_return)

let float32 = (jit_type_float32, float32_param)
let sys_char = (jit_type_sys_char, sys_char_param)
let sys_double = (jit_type_sys_double, sys_double_param)
let sys_int = (jit_type_sys_int, sys_int_param)

(* Functions *)

let (+) operand1 operand2 jit_function = jit_insn_add jit_function operand1 operand2
let ( * ) operand1 operand2 jit_function = jit_insn_mul jit_function operand1 operand2

let create_function1 signature func = 
    jit_context_build_start context;

    let { signature_return_type; signature_parameter_types } = signature in
    let signature = jit_type_create_signature (fst signature_return_type) (List.map fst signature_parameter_types) in

    let jit_function = jit_function_create context signature in
    let param1 = jit_value_get_param jit_function 0 in
    let executed_function = func param1 in
    let returned_value = executed_function jit_function in
    jit_insn_return jit_function returned_value;

    jit_function_compile jit_function;
    jit_context_build_end context;

    let result = snd signature_return_type in
    (fun arg1 ->
        jit_function_apply jit_function [ snd (List.nth signature_parameter_types 0) arg1 ] result;
        get_return result
    )

let create_function3 signature func = 
    jit_context_build_start context;

    let { signature_return_type; signature_parameter_types } = signature in
    let signature = jit_type_create_signature (fst signature_return_type) (List.map fst signature_parameter_types) in

    let jit_function = jit_function_create context signature in
    let param1 = jit_value_get_param jit_function 0 in
    let param2 = jit_value_get_param jit_function 1 in
    let param3 = jit_value_get_param jit_function 2 in
    let executed_function = func param1 param2 param3 in
    let returned_value = executed_function jit_function in
    jit_insn_return jit_function returned_value;

    jit_function_compile jit_function;
    jit_context_build_end context;

    let result = snd signature_return_type in
    (fun arg1 arg2 arg3 ->
        jit_function_apply jit_function [ snd (List.nth signature_parameter_types 0) arg1; snd (List.nth signature_parameter_types 1) arg2; snd (List.nth signature_parameter_types 2) arg3 ] result;
        get_return result
    )

let create_signature signature_return_type signature_parameter_types =
    { signature_return_type; signature_parameter_types }

(* Monad *)

type 'a t = jit_function -> 'a

let return x : 'a t = (fun _ -> x)

let (>>=) monad func = (fun jit_function -> (func (monad jit_function)) jit_function)
let (>>|) monad func = (fun jit_function -> func (monad jit_function))
