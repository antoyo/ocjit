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
open Foreign
open PosixTypes

(* Types *)

type jit_abi = JitAbiCdecl | JitAbiVararg | JitAbiStdcall | JitAbiFastcall

let int_of_jit_abi = function
    | JitAbiCdecl -> 0
    | JitAbiVararg -> 1
    | JitAbiStdcall -> 2
    | JitAbiFastcall -> 3

let jit_abi_of_int = function
    | 0 -> JitAbiCdecl
    | 1 -> JitAbiVararg
    | 2 -> JitAbiStdcall
    | 3 -> JitAbiFastcall
    | _ -> invalid_arg "jit_abi"

let jit_abi = view int ~read: jit_abi_of_int ~write: int_of_jit_abi

type jit_context = unit ptr
let jit_context : jit_context typ = ptr void

type jit_function = unit ptr
let jit_function : jit_function typ = ptr void

type jit_label = int
let jit_label : Unsigned.uint64 typ = uint64_t

type jit_type = unit ptr
let jit_type : jit_type typ = ptr void

type jit_value = unit ptr
let jit_value : jit_value typ = ptr void

(* Values *)

let jit_call_nothrow = 1
let jit_call_noreturn = 2
let jit_call_tail = 4

let jit_label_undefined' = Unsigned.UInt64.of_int64 (Int64.of_int 4294967295)

let jit_type_sys_int' = foreign_value "jit_type_sys_int" jit_type

(* Wrapper values *)

let jit_label_undefined () = allocate jit_label jit_label_undefined'

let jit_type_sys_int = !@ jit_type_sys_int'

(* Functions *)

let get_return = (!@)

let int_param value = allocate int value
    |> to_voidp

let int_return = allocate int 0

(* Bind functions *)

let jit_context_build_end = foreign "jit_context_build_end" (jit_context @-> returning void)

let jit_context_build_start = foreign "jit_context_build_start" (jit_context @-> returning void)

let jit_context_create = foreign "jit_context_create" (void @-> returning jit_context)

let jit_context_destroy = foreign "jit_context_destroy" (jit_context @-> returning void)

let jit_function_apply' = foreign "jit_function_apply" (jit_function @-> ptr (ptr void) @-> ptr void @-> returning void)

let jit_function_clear_recompilable = foreign "jit_function_clear_recompilable" (jit_function @-> returning void)

let jit_function_compile = foreign "jit_function_compile" (jit_function @-> returning void)

let jit_function_create = foreign "jit_function_create" (jit_context @-> jit_type @-> returning jit_function)

let compilation_function = jit_function @-> returning int
let jit_function_get_on_demand_compiler = foreign "jit_function_get_on_demand_compiler" (jit_function @-> returning (funptr compilation_function))

let jit_function_set_on_demand_compiler = foreign "jit_function_set_on_demand_compiler" (jit_function @-> funptr compilation_function @-> returning void)

let jit_function_set_recompilable = foreign "jit_function_set_recompilable" (jit_function @-> returning void)

let jit_insn_add = foreign "jit_insn_add" (jit_function @-> jit_value @-> jit_value @-> returning jit_value)

let jit_insn_branch_if_not = foreign "jit_insn_branch_if_not" (jit_function @-> jit_value @-> ptr jit_label @-> returning void)

let jit_insn_call' = foreign "jit_insn_call" (jit_function @-> string @-> jit_function @-> int @-> ptr jit_value @-> int @-> int @-> returning jit_value)

let jit_insn_eq = foreign "jit_insn_eq" (jit_function @-> jit_value @-> jit_value @-> returning jit_value)

let jit_insn_label = foreign "jit_insn_label" (jit_function @-> ptr jit_label @-> returning void)

let jit_insn_lt = foreign "jit_insn_lt" (jit_function @-> jit_value @-> jit_value @-> returning jit_value)

let jit_insn_mul = foreign "jit_insn_mul" (jit_function @-> jit_value @-> jit_value @-> returning jit_value)

let jit_insn_return = foreign "jit_insn_return" (jit_function @-> jit_value @-> returning void)

let jit_insn_sub = foreign "jit_insn_sub" (jit_function @-> jit_value @-> jit_value @-> returning jit_value)

let jit_type_create_signature' = foreign "jit_type_create_signature" (jit_abi @-> jit_type @-> ptr jit_type @-> int @-> int @-> returning jit_type)

let jit_value_get_param = foreign "jit_value_get_param" (jit_function @-> int @-> returning jit_value)

(* Wrapper functions *)

let jit_function_apply jit_function arguments result =
    let arguments = List.map to_voidp arguments in
    let arguments_array = CArray.of_list (ptr void) arguments in
    jit_function_apply' jit_function (CArray.start arguments_array) (to_voidp result)

let jit_insn_call jit_function function_name function_to_call arguments =
    let temp_arguments = CArray.of_list jit_value arguments in
    jit_insn_call' jit_function function_name function_to_call 0 (CArray.start temp_arguments) (CArray.length temp_arguments) 0

let jit_insn_call_tail jit_function function_name function_to_call arguments =
    let temp_arguments = CArray.of_list jit_value arguments in
    jit_insn_call' jit_function function_name function_to_call 0 (CArray.start temp_arguments) (CArray.length temp_arguments) jit_call_tail

let jit_type_create_signature return_type params =
    let params_array = CArray.of_list jit_type params in
    jit_type_create_signature' JitAbiCdecl return_type (CArray.start params_array) (List.length params) 1
