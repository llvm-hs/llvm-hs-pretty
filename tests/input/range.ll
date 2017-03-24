; ModuleID = 'simple module'

declare i32 @foo(i32)

define i32 @rangeloop() {
entry:
  %0 = alloca i32
  store i32 0, i32* %0
  br label %for.cond

for.cond:                                         ; preds = %for.inc, %entry
  %1 = load i32* %0
  %2 = icmp ult i32 %1, 100
  br i1 %2, label %for.loop, label %for.exit

for.loop:                                         ; preds = %for.cond
  %3 = load i32* %0
  %4 = call i32 @foo(i32 %3)
  br label %for.inc

for.inc:                                          ; preds = %for.loop
  %5 = add i32 %3, 1
  store i32 %5, i32* %0
  br label %for.cond

for.exit:                                         ; preds = %for.cond
  ret i32 0
}
