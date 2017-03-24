; ModuleID = 'simple module'

define i32 @foo(i32 %x) {
entry:
  %x.addr = alloca i32
  store i32 %x, i32* %x.addr
  ret i32 1001
}
