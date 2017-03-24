; ModuleID = 'simple module'

define i1 @main(i32 %x) {
entry:
  %x.addr = alloca i32
  store i32 %x, i32* %x.addr
  %0 = load i32* %x.addr
  %1 = icmp ult i32 %0, 1
  ret i1 %1
}
