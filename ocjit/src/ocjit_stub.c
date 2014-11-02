/*
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
 */

#include <stdio.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <jit/jit.h>

CAMLprim value jit_context_build_end_c(value context) {
    CAMLparam1(context);

    jit_context_t c = (jit_context_t) Field(context, 0);
    jit_context_build_end(c);

    CAMLreturn(Val_unit);
}

CAMLprim value jit_context_build_start_c(value context) {
    CAMLparam1(context);

    jit_context_t c = (jit_context_t) Field(context, 0);
    jit_context_build_start(c);

    CAMLreturn(Val_unit);
}

CAMLprim value jit_context_create_c(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(context);

    context = caml_alloc(1, Abstract_tag);
    Store_field(context, 0, (value) jit_context_create());

    CAMLreturn(context);
}

CAMLprim value jit_context_destroy_c(value context) {
    CAMLparam1(context);

    jit_context_t c = (jit_context_t) Field(context, 0);
    jit_context_destroy(c);

    CAMLreturn(Val_unit);
}

CAMLprim value jit_function_apply_c(value function, value arguments, value result) {
    CAMLparam3(function, arguments, result);
    CAMLlocal1(argument);

    jit_function_t c_function = (jit_function_t) Field(function, 0);
    jit_type_t c_signature = jit_function_get_signature(c_function);
    jit_type_t c_return_type = jit_type_get_return(c_signature);

    int num_arguments = caml_array_length(arguments);

    void** c_arguments = malloc(sizeof(void*) * num_arguments);
    int i;
    for(i = 0 ; i < num_arguments ; i++) {
        jit_type_t c_param_type = jit_type_get_param(c_signature, i);

        if(c_param_type == jit_type_sys_int) {
            argument = Field(arguments, i);
            int* int_argument = malloc(sizeof(int*));
            *int_argument = Int_val(argument);
            c_arguments[i] = int_argument;
        }
    }

    if(c_return_type == jit_type_sys_int) {
        int int_result;
        jit_function_apply(c_function, c_arguments, &int_result);
        Store_field(result, 0, Val_int(int_result));
    }

    for(i = 0 ; i < num_arguments ; i++) {
        free(c_arguments[i]);
    }
    free(c_arguments);

    CAMLreturn(Val_unit);
}

CAMLprim value jit_function_compile_c(value function) {
    CAMLparam1(function);

    jit_function_t c_function = (jit_function_t) Field(function, 0);
    jit_function_compile(c_function);

    CAMLreturn(Val_unit);
}

CAMLprim value jit_function_create_c(value context, value signature) {
    CAMLparam2(context, signature);
    CAMLlocal1(function);

    jit_context_t c_context = (jit_context_t) Field(context, 0);
    jit_type_t c_signature = (jit_type_t) Field(signature, 0);

    function = caml_alloc(1, Abstract_tag);
    Store_field(function, 0, (value) jit_function_create(c_context, c_signature));

    CAMLreturn(function);
}

CAMLprim value jit_insn_add_c(value function, value operand1, value operand2) {
    CAMLparam3(function, operand1, operand2);
    CAMLlocal1(result);

    result = caml_alloc(1, Abstract_tag);
    jit_function_t c_function = (jit_function_t) Field(function, 0);
    jit_value_t c_operand1 = (jit_value_t) Field(operand1, 0);
    jit_value_t c_operand2 = (jit_value_t) Field(operand2, 0);
    Store_field(result, 0, (value) jit_insn_add(c_function, c_operand1, c_operand2));

    CAMLreturn(result);
}

CAMLprim value jit_insn_mul_c(value function, value operand1, value operand2) {
    CAMLparam3(function, operand1, operand2);
    CAMLlocal1(result);

    result = caml_alloc(1, Abstract_tag);
    jit_function_t c_function = (jit_function_t) Field(function, 0);
    jit_value_t c_operand1 = (jit_value_t) Field(operand1, 0);
    jit_value_t c_operand2 = (jit_value_t) Field(operand2, 0);
    Store_field(result, 0, (value) jit_insn_mul(c_function, c_operand1, c_operand2));

    CAMLreturn(result);
}

CAMLprim value jit_insn_return_c(value function, value result) {
    CAMLparam2(function, result);

    jit_function_t c_function = (jit_function_t) Field(function, 0);
    jit_value_t c_result = (jit_value_t) Field(result, 0);
    jit_insn_return(c_function, c_result);

    CAMLreturn(Val_unit);
}

CAMLprim value jit_type_create_signature_c(value abi, value return_type, value params, value incref) {
    CAMLparam4(abi, return_type, params, incref);
    CAMLlocal2(signature, param);

    signature = caml_alloc(1, Abstract_tag);
    int c_abi = Int_val(abi);
    jit_type_t c_return_type = (jit_type_t) Field(return_type, 0);
    int num_params = caml_array_length(params);

    jit_type_t* c_params = malloc(sizeof(jit_type_t) * num_params);
    int i;
    for(i = 0 ; i < num_params ; i++) {
        param = Field(params, i);
        jit_type_t c_param = (jit_type_t) Field(param, 0);
        c_params[i] = c_param;
    }

    int c_incref = Int_val(incref);
    Store_field(signature, 0, (value) jit_type_create_signature(c_abi, c_return_type, c_params, num_params, c_incref));
    free(c_params);

    CAMLreturn(signature);
}

CAMLprim value jit_value_get_param_c(value function, value param_num) {
    CAMLparam2(function, param_num);
    CAMLlocal1(jit_value);

    jit_value = caml_alloc(1, Abstract_tag);
    jit_function_t c_function = (jit_function_t) Field(function, 0);
    int c_param_num = Int_val(param_num);
    Store_field(jit_value, 0, (value) jit_value_get_param(c_function, c_param_num));

    CAMLreturn(jit_value);
}

CAMLprim value jit_type_sys_int_c(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(type);

    type = caml_alloc(1, Abstract_tag);
    Store_field(type, 0, (value) jit_type_sys_int);

    CAMLreturn(type);
}
