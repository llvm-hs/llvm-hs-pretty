; ModuleID = 'simple module'

declare i32 @foo(i32, i32)

define i32 @main() {
entry:
  %0 = alloca i32
  store i32 0, i32* %0
  %1 = load i32* %0
  %2 = alloca i32
  store i32 0, i32* %2
  %3 = load i32* %2
  %4 = alloca i32
  store i32 0, i32* %4
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %5 = load i32* %4
  %6 = icmp ult i32 %5, 10
  br i1 %6, label %for.loop, label %for.exit

for.loop:                                         ; preds = %for.cond
  %7 = load i32* %4
  %8 = alloca i32
  store i32 0, i32* %8
  br label %for.cond1

for.inc:                                          ; preds = %for.exit1
  %9 = add i32 %7, 1
  store i32 %9, i32* %4
  br label %for.cond

for.exit:                                         ; preds = %for.cond
  ret i32 0

for.cond1:                                        ; preds = %for.inc1, %for.loop
  %10 = load i32* %8
  %11 = icmp ult i32 %10, 20
  br i1 %11, label %for.loop1, label %for.exit1

for.loop1:                                        ; preds = %for.cond1
  %12 = load i32* %8
  %13 = call i32 @foo(i32 %1, i32 %3)
  br label %for.inc1

for.inc1:                                         ; preds = %for.loop1
  %14 = add i32 %12, 1
  store i32 %14, i32* %8
  br label %for.cond1

for.exit1:                                        ; preds = %for.cond1
  br label %for.inc
}
