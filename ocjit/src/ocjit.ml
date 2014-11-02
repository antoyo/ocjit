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

type jit_abi = JitAbiCdecl | JitAbiVararg | JitAbiStdcall | JitAbiFastcall

type jit_context

type jit_function

type jit_type

type jit_value

type void

external jit_context_build_end : jit_context -> unit = "jit_context_build_end_c"

external jit_context_build_start : jit_context -> unit = "jit_context_build_start_c"

external jit_context_create : unit -> jit_context = "jit_context_create_c"

external jit_context_destroy : jit_context -> unit = "jit_context_destroy_c"

external jit_function_apply : jit_function -> void array -> void ref -> unit = "jit_function_apply_c"

external jit_function_compile : jit_function -> unit = "jit_function_compile_c"

external jit_function_create : jit_context -> jit_type -> jit_function = "jit_function_create_c"

external jit_insn_add : jit_function -> jit_value -> jit_value -> jit_value = "jit_insn_add_c"

external jit_insn_mul : jit_function -> jit_value -> jit_value -> jit_value = "jit_insn_mul_c"

external jit_insn_return : jit_function -> jit_value -> unit = "jit_insn_return_c"

external jit_type_create_signature : jit_abi -> jit_type -> jit_type array -> int -> jit_type = "jit_type_create_signature_c"

external jit_value_get_param : jit_function -> int -> jit_value = "jit_value_get_param_c"

external jit_type_sys_int_c : unit -> jit_type = "jit_type_sys_int_c"
let jit_type_sys_int = jit_type_sys_int_c ()
