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
    jit_init ();
    let supports_threads = jit_supports_threads () in
    print_int supports_threads;
    print_endline "";
    let supports_virtual_memory = jit_supports_virtual_memory () in
    print_int supports_virtual_memory;
    print_endline "";
    let uses_interpreter = jit_uses_interpreter () in
    print_int uses_interpreter;
    print_endline ""
