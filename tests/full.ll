; ModuleID = 'simple module'

@"%i" = global [3 x i8] c"%i\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %0 = alloca i32
  store i32 0, i32* %0
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %1 = load i32* %0
  %2 = icmp ult i32 %1, 15
  br i1 %2, label %for.loop, label %for.exit

for.loop:                                         ; preds = %for.cond
  %3 = load i32* %0
  %4 = alloca i32
  store i32 0, i32* %4
  br label %for.cond1

for.inc:                                          ; preds = %for.exit1
  %5 = add i32 1, %3
  store i32 %5, i32* %0
  br label %for.cond

for.exit:                                         ; preds = %for.cond
  ret i32 0

for.cond1:                                        ; preds = %for.inc1, %for.loop
  %6 = load i32* %4
  %7 = icmp ult i32 %6, 15
  br i1 %7, label %for.loop1, label %for.exit1

for.loop1:                                        ; preds = %for.cond1
  %8 = load i32* %4
  %9 = add i32 %3, %8
  %10 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @"%i", i32 0, i32 0), i32 %9)
  br label %for.inc1

for.inc1:                                         ; preds = %for.loop1
  %11 = add i32 1, %8
  store i32 %11, i32* %4
  br label %for.cond1

for.exit1:                                        ; preds = %for.cond1
  br label %for.inc
}
