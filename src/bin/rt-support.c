#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

// This will be linked with the main bit code, hence the function names should contain:
//    _prelude_ prefix, that is the name of the mangling scheme used for the components
//    _n_{function_name}_t_{type} suffix, that is the name of the mangling scheme used to support overloaded functions


// Print newline
void _prelude_n_print_t_i(int n){
  printf("%d\n", n);
}

void _prelude_n_print_t_f(float n){
  printf("%0.2f\n", n);
}

void _prelude_n_print_t_b(bool n){
  printf("%s\n", n ? "true" : "false");
}

void _prelude_n_print_t_c(char n){
  printf("%c\n", n);
}


// Print without newline
void _prelude_n_put_t_i(int n){
  printf("%d", n);
}

void _prelude_n_put_t_f(float n){
  printf("%0.4f", n);
}

void _prelude_n_put_t_b(bool n){
  printf("%s", n ? "true" : "false");
}

void _prelude_n_put_t_c(char n){
  printf("%c", n);
}


// Get primitive
int _prelude_n_getint_t_v(){
    char buffer[32];
    if(fgets(buffer, 32, stdin) == NULL)
      return 0;
    return atoi(buffer);
}


float _prelude_n_getfloat_t_v(){
    char buffer[32];
    if(fgets(buffer, 32, stdin) == NULL)
      return 0;
    return atof(buffer);
}


char _prelude_n_getchar_t_v(){
    char buffer[32];
    if(fgets(buffer, 32, stdin) == NULL)
      return 0;
    return buffer[0];
}


bool _prelude_n_getbool_t_v(){
    char buffer[32];
    if(fgets(buffer, 32, stdin) == NULL)
      return false;
    return buffer[0] == 't' || buffer[0] == 'T';
}

float _prelude_n_int_to_float_t_i(int n){
  return (float) n;
}

int _prelude_n_float_to_int_t_f(float n){
  return (int) n;
}

// command to compile with clang and produce bit code
// clang -emit-llvm -c rt-support.c -o rt-support.bc

// as above but emit with the following data layout
// 'e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128
// clang -emit-llvm -c rt-support.c -o rt-support.bc -mtriple=x86_64-pc-linux-gnu -mcpu=core2 -march=x86-64 -m64 -mno-red-zone -mno-mmx -mno-sse -mno-sse2 -mno-3dnow -mno-avx -mno-avx2 -mno-avx512f -mno-avx512cd -mno-avx512er -mno-avx512pf -mno-avx512bw -mno-avx512dq -mno-avx512vl -mno-avx512ifma -mno-avx512vbmi -mno-avx512vbmi2 -mno-avx512vpopcntdq -mno-avx512vnni -mno-avx512bitalg -mno-avx512vbmivl -mno-avx512bf16 -mno-avx512vp2intersect -mno-avx512vnniw -mno-avx512vpclmulqdq -mno-avx512gfni -mno-avx512vaes -mno-avx512vnni -mno-avx512vpclmulqdq -mno-avx512vbmi -mno-avx512vpopcntdq -mno-avx512bitalg -mno-avx512vbmivl -mno-avx512bf16 -mno-avx512vp2intersect -mno-avx512vnniw -mno-avx512vpclmulqdq -mno-avx512gfni -mno-avx512vaes -mno-avx512vnni -mno-avx512vpclmulqdq -mno-avx512vbmi -mno-avx512vpopcntdq -mno-avx512bitalg -mno-avx512vbmivl -mno-avx512bf16 -mno-avx512vp2intersect -mno-avx512vnniw -mno-avx512vpclmulqdq -mno-av

// command to link the bit code with the main bit code
// llvm-link rt-support.bc main.bc -o main-linked.bc