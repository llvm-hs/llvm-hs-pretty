define void @main() prefix i32 123 {
  ret void
}

define void @tuple() prefix {i32,i32} {i32 123, i32 321} {
  ret void
}
