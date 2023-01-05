#include <stdio.h>
#include <stdlib.h>

int _prelude_getint(){
    char buffer[32];
    if(fgets(buffer, 32, stdin) == NULL)
      return 0;
    return atoi(buffer);
}

void _prelude_print(int n){
  printf("%d\n", n);
}

// command to compile with clang and produce bit code
// clang -emit-llvm -c rt-support.c -o rt-support.bc

// as above but emit with the following data layout
// 'e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128
// clang -emit-llvm -c rt-support.c -o rt-support.bc -mtriple=x86_64-pc-linux-gnu -mcpu=core2 -march=x86-64 -m64 -mno-red-zone -mno-mmx -mno-sse -mno-sse2 -mno-3dnow -mno-avx -mno-avx2 -mno-avx512f -mno-avx512cd -mno-avx512er -mno-avx512pf -mno-avx512bw -mno-avx512dq -mno-avx512vl -mno-avx512ifma -mno-avx512vbmi -mno-avx512vbmi2 -mno-avx512vpopcntdq -mno-avx512vnni -mno-avx512bitalg -mno-avx512vbmivl -mno-avx512bf16 -mno-avx512vp2intersect -mno-avx512vnniw -mno-avx512vpclmulqdq -mno-avx512gfni -mno-avx512vaes -mno-avx512vnni -mno-avx512vpclmulqdq -mno-avx512vbmi -mno-avx512vpopcntdq -mno-avx512bitalg -mno-avx512vbmivl -mno-avx512bf16 -mno-avx512vp2intersect -mno-avx512vnniw -mno-avx512vpclmulqdq -mno-avx512gfni -mno-avx512vaes -mno-avx512vnni -mno-avx512vpclmulqdq -mno-avx512vbmi -mno-avx512vpopcntdq -mno-avx512bitalg -mno-avx512vbmivl -mno-avx512bf16 -mno-avx512vp2intersect -mno-avx512vnniw -mno-avx512vpclmulqdq -mno-av

// command to link the bit code with the main bit code
// llvm-link rt-support.bc main.bc -o main-linked.bc