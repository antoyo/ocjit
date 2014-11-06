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

    let my_function = create_function1 signature (fun x ->
        return x
    )
    in

    let result = my_function 3 in
    print_int result;
    print_endline "";

    let signature = create_signature return_sys_int [ sys_int; sys_int; sys_int ] in

    let my_function = create_function3 signature (fun x y z ->
        x * y >>= (+) z
    ) in
    let result = my_function 3 5 2 in

    print_string "mul_add(3, 5, 2) = ";
    print_int result;
    print_endline "";

    let signature = create_signature return_sys_char [ sys_char; sys_char; sys_char ] in

    let my_function = create_function3 signature (fun a b c ->
        return b
    )
    in

    let result = my_function 'a' 'b' 'c' in
    print_char result;
    print_endline "";

    let signature = create_signature return_float32 [ float32 ] in

    let my_function = create_function1 signature (fun x ->
        return x
    )
    in

    let result = my_function 3.5 in
    print_float result;
    print_endline "";

    let signature = create_signature return_float32 [ float32; float32; float32 ] in

    let my_function = create_function3 signature (fun x y z ->
        x * y >>= (+) z
    ) in
    let result = my_function 3.5 5.5 2.5 in

    print_string "mul_add(3.5, 5.5, 2.5) = ";
    print_float result;
    print_endline "";

    let signature = create_signature return_sys_double [ sys_double; sys_double; sys_double ] in

    let my_function = create_function3 signature (fun x y z ->
        x * y >>= (+) z
    ) in
    let result = my_function 3.5 5.5 2.5 in

    print_string "mul_add(3.5, 5.5, 2.5) = ";
    print_float result;
    print_endline ""
