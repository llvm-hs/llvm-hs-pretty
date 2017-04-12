define void @main() {
  %X = uitofp i32 257 to float;
  %Y = uitofp i8 -1 to double;
  ret void
}
