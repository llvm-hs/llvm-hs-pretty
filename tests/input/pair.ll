; ModuleID = 'simple module'

@"%f" = global [3 x i8] c"%f\00"
@"%i" = global [3 x i8] c"%i\00"

declare i32 @printf(i8*, ...)

declare i32 @printf1(i8*, ...)

define i32 @main() {
entry:
  %0 = alloca { i32, float }
  %1 = getelementptr inbounds { i32, float }* %0, i32 0, i32 0
  %2 = getelementptr inbounds { i32, float }* %0, i32 0, i32 1
  store i32 100, i32* %1
  store float 2.000000e+02, float* %2
  %3 = getelementptr inbounds { i32, float }* %0, i32 0, i32 0
  %4 = load i32* %3
  %5 = getelementptr inbounds { i32, float }* %0, i32 0, i32 1
  %6 = load float* %5
  %7 = call i32 (i8*, ...)* @printf1(i8* getelementptr inbounds ([3 x i8]* @"%i", i32 0, i32 0), i32 %4)
  %8 = call i32 (i8*, ...)* @printf1(i8* getelementptr inbounds ([3 x i8]* @"%f", i32 0, i32 0), float %6)
  ret i32 %8
}
