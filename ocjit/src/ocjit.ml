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

(* Instruction functions *)

let (+) operand1 operand2 jit_function = jit_insn_add jit_function operand1 operand2
let (-) operand1 operand2 jit_function = jit_insn_sub jit_function operand1 operand2
let ( * ) operand1 operand2 jit_function = jit_insn_mul jit_function operand1 operand2
let (==) operand1 operand2 jit_function = jit_insn_eq jit_function operand1 operand2
let (<) operand1 operand2 jit_function = jit_insn_lt jit_function operand1 operand2

let if_ condition instructions jit_function =
    let temp = condition jit_function in
    let label = jit_label_undefined () in
    jit_insn_branch_if_not jit_function temp label;
    instructions jit_function;
    jit_insn_label jit_function label

let int value jit_function = jit_value_create_nint_constant jit_function jit_type_sys_int value

let ret value jit_function = jit_insn_return jit_function value

(* Functions *)

let compile_function0 func callable_function jit_function =
    let executed_function = func callable_function in
    executed_function jit_function;
    1

let compile_function1 func callable_function jit_function =
    let param1 = jit_value_get_param jit_function 0 in
    let executed_function = func callable_function param1 in
    executed_function jit_function;
    1

let compile_function2 func callable_function jit_function =
    let param1 = jit_value_get_param jit_function 0 in
    let param2 = jit_value_get_param jit_function 1 in
    let executed_function = func callable_function param1 param2 in
    executed_function jit_function;
    1

let compile_function3 func callable_function jit_function =
    let param1 = jit_value_get_param jit_function 0 in
    let param2 = jit_value_get_param jit_function 1 in
    let param3 = jit_value_get_param jit_function 2 in
    let executed_function = func callable_function param1 param2 param3 in
    executed_function jit_function;
    1

let create_function0 signature func = 
    let { signature_return_type } = signature in
    let signature = jit_type_create_signature (fst signature_return_type) [] in

    let jit_function = jit_function_create context signature in

    let callable_function = (fun jfunc ->
        jit_insn_call jfunc "" jit_function []
    ) in

    jit_function_set_on_demand_compiler jit_function (compile_function0 func callable_function);

    let result = snd signature_return_type in
    ((fun () ->
        jit_function_apply jit_function [] result;
        get_return result
    ), callable_function)

let create_function1 signature func = 
    let { signature_return_type; signature_parameter_types } = signature in
    let signature = jit_type_create_signature (fst signature_return_type) (List.map fst signature_parameter_types) in

    let jit_function = jit_function_create context signature in

    let callable_function = (fun arg1 jfunc ->
        jit_insn_call jfunc "" jit_function [ arg1 ]
    ) in

    jit_function_set_on_demand_compiler jit_function (compile_function1 func callable_function);

    let result = snd signature_return_type in
    ((fun arg1 ->
        jit_function_apply jit_function [ snd (List.nth signature_parameter_types 0) arg1 ] result;
        get_return result
    ), callable_function)

let create_function2 signature func = 
    let { signature_return_type; signature_parameter_types } = signature in
    let signature = jit_type_create_signature (fst signature_return_type) (List.map fst signature_parameter_types) in

    let jit_function = jit_function_create context signature in

    let callable_function = (fun arg1 arg2 jfunc ->
        jit_insn_call jfunc "" jit_function [ arg1; arg2 ]
    ) in

    jit_function_set_on_demand_compiler jit_function (compile_function2 func callable_function);

    let result = snd signature_return_type in
    ((fun arg1 arg2 ->
        jit_function_apply jit_function [ snd (List.nth signature_parameter_types 0) arg1; snd (List.nth signature_parameter_types 1) arg2 ] result;
        get_return result
    ), callable_function)

let create_function3 signature func = 
    let { signature_return_type; signature_parameter_types } = signature in
    let signature = jit_type_create_signature (fst signature_return_type) (List.map fst signature_parameter_types) in

    let jit_function = jit_function_create context signature in

    let callable_function = (fun arg1 arg2 arg3 jfunc ->
        jit_insn_call jfunc "" jit_function [ arg1; arg2; arg3 ]
    ) in

    jit_function_set_on_demand_compiler jit_function (compile_function3 func callable_function);

    let result = snd signature_return_type in
    ((fun arg1 arg2 arg3 ->
        jit_function_apply jit_function [ snd (List.nth signature_parameter_types 0) arg1; snd (List.nth signature_parameter_types 1) arg2; snd (List.nth signature_parameter_types 2) arg3 ] result;
        get_return result
    ), callable_function)

let create_signature signature_return_type signature_parameter_types =
    { signature_return_type; signature_parameter_types }

(* Monad *)

type 'a t = jit_function -> 'a

let return x : 'a t = (fun _ -> x)

let (>>=) monad func = (fun jit_function -> (func (monad jit_function)) jit_function)
let (>>) monad func = monad >>= (fun _ -> func)
let (>>|) monad func = (fun jit_function -> func (monad jit_function))
