; ModuleID = 'simple module'

declare i32 @foo()

define i32 @forloop() {
entry:
  %0 = alloca i32
  store i32 0, i32* %0
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %1 = load i32* %0
  br i1 true, label %for.loop, label %for.exit

for.loop:                                         ; preds = %for.cond
  %2 = load i32* %0
  %3 = alloca i32
  store i32 0, i32* %3
  br label %for.cond1

for.inc:                                          ; preds = %for.exit1
  %4 = add i32 1, %2
  store i32 %4, i32* %0
  br label %for.cond

for.exit:                                         ; preds = %for.cond
  ret i32 0

for.cond1:                                        ; preds = %for.inc1, %for.loop
  %5 = load i32* %3
  br i1 true, label %for.loop1, label %for.exit1

for.loop1:                                        ; preds = %for.cond1
  %6 = load i32* %3
  %7 = call i32 @foo()
  br label %for.inc1

for.inc1:                                         ; preds = %for.loop1
  %8 = add i32 1, %6
  store i32 %8, i32* %3
  br label %for.cond1

for.exit1:                                        ; preds = %for.cond1
  br label %for.inc
}
