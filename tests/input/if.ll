; ModuleID = 'simple module'

define i32 @foo(i1 %x) {
entry:
  %x.addr = alloca i1
  store i1 %x, i1* %x.addr
  %0 = load i1* %x.addr
  br i1 %0, label %if.then, label %if.else

if.then:                                          ; preds = %entry
  br label %if.exit

if.else:                                          ; preds = %entry
  br label %if.exit

if.exit:                                          ; preds = %if.else, %if.then
  %1 = phi i32 [ 1, %if.then ], [ 2, %if.else ]
  ret i32 %1
}
