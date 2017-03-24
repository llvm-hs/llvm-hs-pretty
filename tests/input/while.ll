; ModuleID = 'simple module'

declare i32 @foo()

define i32 @whileloop() {
entry:
  br label %while.cond

while.cond:                                       ; preds = %while.loop, %entry
  br i1 true, label %while.loop, label %while.exit

while.loop:                                       ; preds = %while.cond
  %0 = call i32 @foo()
  br label %while.cond

while.exit:                                       ; preds = %while.cond
  ret i32 0
}
