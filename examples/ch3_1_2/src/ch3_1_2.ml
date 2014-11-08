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
    let signature = create_signature return_sys_int [ sys_int ] in

    let (compiled_identity, identity) = create_function1 signature (fun _ x ->
        ret x
    )
    in

    let result = compiled_identity 3 in
    print_int result;
    print_endline "";

    let signature = create_signature return_sys_int [ sys_int; sys_int; sys_int ] in

    let (compiled_mul_add, mul_add) = create_function3 signature (fun _ x y z ->
        x * y >>= (+) z >>= ret
    ) in
    let result = compiled_mul_add 3 5 2 in

    print_string "mul_add(3, 5, 2) = ";
    print_int result;
    print_endline "";

    let signature = create_signature return_sys_char [ sys_char; sys_char; sys_char ] in

    let (compiled_second, second) = create_function3 signature (fun _ a b c ->
        ret b
    )
    in

    let result = compiled_second 'a' 'b' 'c' in
    print_char result;
    print_endline "";

    let signature = create_signature return_float32 [ float32 ] in

    let (compiled_identity, identity) = create_function1 signature (fun _ x ->
        ret x
    )
    in

    let result = compiled_identity 3.5 in
    print_float result;
    print_endline "";

    let signature = create_signature return_float32 [ float32; float32; float32 ] in

    let (compiled_mul_add, mul_add) = create_function3 signature (fun _ x y z ->
        x * y >>= (+) z >>= ret
    ) in
    let result = compiled_mul_add 3.5 5.5 2.5 in

    print_string "mul_add(3.5, 5.5, 2.5) = ";
    print_float result;
    print_endline "";

    let signature = create_signature return_sys_double [ sys_double; sys_double; sys_double ] in

    let (compiled_mul_add, mul_add) = create_function3 signature (fun _ x y z ->
        x * y >>= (+) z >>= ret
    ) in
    let result = compiled_mul_add 3.5 5.5 2.5 in

    print_string "mul_add(3.5, 5.5, 2.5) = ";
    print_float result;
    print_endline "";

    let signature = create_signature return_sys_int [] in

    let (compiled_return42, return42) = create_function0 signature (fun _ ->
        int 42 >>= ret
    )
    in

    let result = compiled_return42 () in
    print_int result;
    print_endline "";
