; ModuleID = 'simple module'

@"%i" = global [3 x i8] c"%i\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %0 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @"%i", i32 0, i32 0), i32 42)
  ret i32 %0
}
