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

type jit_type = unit ptr
let jit_type : jit_type typ = ptr void

type jit_value = unit ptr
let jit_value : jit_value typ = ptr void

let jit_context_build_end = foreign "jit_context_build_end" (jit_context @-> returning void)

let jit_context_build_start = foreign "jit_context_build_start" (jit_context @-> returning void)

let jit_context_create = foreign "jit_context_create" (void @-> returning jit_context)

let jit_context_destroy = foreign "jit_context_destroy" (jit_context @-> returning void)

let jit_function_apply = foreign "jit_function_apply" (jit_function @-> ptr (ptr void) @-> ptr void @-> returning void)

let jit_function_compile = foreign "jit_function_compile" (jit_function @-> returning void)

let jit_function_create = foreign "jit_function_create" (jit_context @-> jit_type @-> returning jit_function)

let jit_insn_add = foreign "jit_insn_add" (jit_function @-> jit_value @-> jit_value @-> returning jit_value)

let jit_insn_mul = foreign "jit_insn_mul" (jit_function @-> jit_value @-> jit_value @-> returning jit_value)

let jit_insn_return = foreign "jit_insn_return" (jit_function @-> jit_value @-> returning void)

let jit_type_create_signature = foreign "jit_type_create_signature" (jit_abi @-> jit_type @-> ptr jit_type @-> int @-> int @-> returning jit_type)

let jit_type_sys_int' = foreign_value "jit_type_sys_int" jit_type
let jit_type_sys_int = !@ jit_type_sys_int'

let jit_value_get_param = foreign "jit_value_get_param" (jit_function @-> int @-> returning jit_value)
