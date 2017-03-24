; ModuleID = 'simple module'

define i32 @main() {
entry:
  %0 = alloca { i32, float }
  %1 = getelementptr inbounds { i32, float }* %0, i32 0, i32 0
  %2 = load i32* %1
  ret i32 %2
}
