; ModuleID = 'simple module'

; Function Attrs: nounwind readonly
declare double @llvm.sqrt.f64(double) #0

define double @main() {
entry:
  %0 = call double @llvm.sqrt.f64(double 2.000000e+00)
  ret double %0
}

attributes #0 = { nounwind readonly }
