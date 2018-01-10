; ModuleID = 'sample'

target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux"

define i32 @foo(i32 %x) {
entry:
  %x.addr = alloca i32
  store i32 %x, i32* %x.addr
  ret i32 1001
}
