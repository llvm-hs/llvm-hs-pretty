; ModuleID = 'saxpy.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

; Function Attrs: nounwind uwtable
define void @saxpy(i32 %n, float %a, float* %x, float* %y) #0 {
  %1 = alloca i32, align 4
  %2 = alloca float, align 4
  %3 = alloca float*, align 8
  %4 = alloca float*, align 8
  %i = alloca i32, align 4
  store i32 %n, i32* %1, align 4
  store float %a, float* %2, align 4
  store float* %x, float** %3, align 8
  store float* %y, float** %4, align 8
  store i32 0, i32* %i, align 4
  br label %5

; <label>:5                                       ; preds = %27, %0
  %6 = load i32* %i, align 4
  %7 = load i32* %1, align 4
  %8 = icmp slt i32 %6, %7
  br i1 %8, label %9, label %30

; <label>:9                                       ; preds = %5
  %10 = load float* %2, align 4
  %11 = load i32* %i, align 4
  %12 = sext i32 %11 to i64
  %13 = load float** %3, align 8
  %14 = getelementptr inbounds float* %13, i64 %12
  %15 = load float* %14, align 4
  %16 = fmul float %10, %15
  %17 = load i32* %i, align 4
  %18 = sext i32 %17 to i64
  %19 = load float** %4, align 8
  %20 = getelementptr inbounds float* %19, i64 %18
  %21 = load float* %20, align 4
  %22 = fadd float %16, %21
  %23 = load i32* %i, align 4
  %24 = sext i32 %23 to i64
  %25 = load float** %4, align 8
  %26 = getelementptr inbounds float* %25, i64 %24
  store float %22, float* %26, align 4
  br label %27

; <label>:27                                      ; preds = %9
  %28 = load i32* %i, align 4
  %29 = add nsw i32 %28, 1
  store i32 %29, i32* %i, align 4
  br label %5

; <label>:30                                      ; preds = %5
  ret void
}

attributes #0 = { nounwind uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Ubuntu clang version 3.4-1ubuntu3 (tags/RELEASE_34/final) (based on LLVM 3.4)"}
