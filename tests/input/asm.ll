; ModuleID = '<string>'


define external ccc i64 @main(){
entry:
  %X = alloca i64
  store i64 1, i64* %X
  %Y = load i64, i64* %X
  %A = call i64 (i64) asm "addq $0, $0", "=r,r"(i64 %Y)
  %B = call i64 (i64) asm alignstack inteldialect "sub $0, $0", "=r,r"(i64 %Y)
  %C = call i64 asm alignstack "movq $$1, $0", "=r"()
  %D = call i64 (i64) asm sideeffect "add $$1, $0", "=r,r"(i64  %Y)
  ret i64 %D
}