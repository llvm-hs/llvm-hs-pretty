define double @foo(double %a, double %b)  {
entry:
  %0 = fmul double %a, %a
  %1 = fmul double %a, 2.000000e+00
  %2 = fmul double %1, %b
  %3 = fadd double %0, %2
  %4 = fmul double %b, %b
  %5 = fadd double %4, %3
  ret double %5
}

define double @bar(double %a) {
entry:
  %0 = alloca double
  store double %a, double* %0
  %1 = load double* %0
  %2 = call double @foo(double %1, double 4.000000e+00)
  %3 = call double @bar(double 3.133700e+04)
  %4 = fadd double %2, %3
  ret double %4
}
