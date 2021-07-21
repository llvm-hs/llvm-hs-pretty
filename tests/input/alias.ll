; ModuleID = 'simple module'

define i32 @foo() {
  ret i32 42
}

@bar = external alias i32 (), i32 ()* @foo
