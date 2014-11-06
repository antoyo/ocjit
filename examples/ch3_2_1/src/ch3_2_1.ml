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

open Ocjit

let () =
    let signature = create_signature return_sys_int [ sys_int; sys_int ] in
    
    let (compiled_max, max) = create_function2 signature (fun _ a b ->
        if_(a < b) (
            ret b
        ) >>
        ret a
    ) in
    let result = compiled_max 375 625 in
    print_string "max(375, 625) = ";
    print_int result;
    print_endline "";

    let result = compiled_max 625 375 in
    print_string "max(625, 375) = ";
    print_int result;
    print_endline "";

    let signature = create_signature return_sys_int [ sys_int; sys_int ] in
    
    let (compiled_gcd, gcd) = create_function2 signature (fun gcd x y ->
        if_(x == y) (
            ret x
        ) >>
        if_(x < y) (
            y - x >>= fun temp ->
            gcd x temp >>= fun result ->
            ret result
        ) >>
        x - y >>= fun temp ->
        gcd temp y >>= fun result ->
        ret result
    ) in
    let result = compiled_gcd 375 625 in
    print_string "gcd(375, 625) = ";
    print_int result;
    print_endline ""
