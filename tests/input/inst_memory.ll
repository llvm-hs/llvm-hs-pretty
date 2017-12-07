; --- [ Memory instructions ] --------------------------------------------------

@x = global i32 42

; ~~~ [ alloca ] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

define i32* @alloca_1() {
	; Plain instruction.
	%result = alloca i32
	ret i32* %result
}

define i32* @alloca_2() {
	; Number of elements operand.
	%result = alloca i32, i32 10
	ret i32* %result
}

define i32* @alloca_3() {
	; Alignment operand.
	%result = alloca i32, align 8
	ret i32* %result
}

define i32* @alloca_4() {
	; Metadata.
	%result = alloca i32, !foo !{!"bar"}, !baz !{!"qux"}
	ret i32* %result
}

define i32* @alloca_5() {
	; Full instruction.
	%result = alloca i32, i32 10, align 8, !foo !{!"bar"}, !baz !{!"qux"}
	ret i32* %result
}

; ~~~ [ load ] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

define i32 @load_1(i32* %x) {
	; Plain instruction.
	%result = load i32, i32* %x
	ret i32 %result
}

define i32 @load_2() {
	; Global operand.
	%result = load i32, i32* @x
	ret i32 %result
}

define i32 @load_3(i32* %x) {
	; Volatile.
	%result = load volatile i32, i32* %x
	ret i32 %result
}

define i32 @load_4(i32* %x) {
	; Alignment operand.
	%result = load i32, i32* %x, align 8
	ret i32 %result
}

define i32 @load_5(i32* %x) {
	; Metadata.
	%result = load i32, i32* %x, !foo !{!"bar"}, !baz !{!"qux"}
	ret i32 %result
}

define i32 @load_6(i32* %x) {
	; Full instruction.
	%result = load volatile i32, i32* %x, align 8, !foo !{!"bar"}, !baz !{!"qux"}
	ret i32 %result
}

; ~~~ [ store ] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

define void @store_1(i32* %x) {
	; Plain instruction.
	store i32 42, i32* %x
	ret void
}

define void @store_2() {
	; Global operand.
	store i32 42, i32* @x
	ret void
}

define void @store_3(i32* %x) {
	; Volatile.
	store volatile i32 42, i32* %x
	ret void
}

define void @store_4(i32* %x) {
	; Alignment operand.
	store i32 42, i32* %x, align 8
	ret void
}

define void @store_5(i32* %x) {
	; Metadata.
	store i32 42, i32* %x, !foo !{!"bar"}, !baz !{!"qux"}
	ret void
}

define void @store_6(i32* %x) {
	; Full instruction.
	store volatile i32 42, i32* %x, align 8, !foo !{!"bar"}, !baz !{!"qux"}
	ret void
}

; ~~~ [ fence ] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

define void @fence() {
    fence syncscope("singlethread") seq_cst
    fence acquire
    fence release
    fence acq_rel
    ret void
}

; ~~~ [ cmpxchg ] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

define void @cmpxchg_1(i32* %ptr) {
  entry:
    %orig = load atomic i32, i32* %ptr unordered, align 4
    br label %loop

  loop:
    %cmp = phi i32 [ %orig, %entry ], [%value_loaded, %loop]
    %squared = mul i32 %cmp, %cmp
    %val_success = cmpxchg i32* %ptr, i32 %cmp, i32 %squared acq_rel monotonic
    %value_loaded = extractvalue { i32, i1 } %val_success, 0
    %success = extractvalue { i32, i1 } %val_success, 1
    br i1 %success, label %done, label %loop
  done:
    ret void
}

; ~~~ [ atomicrmw ] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

define void @atomicrmw(i8* %Q, i32* %word, i32* %x) {
    atomicrmw add i8* %Q, i8 1 monotonic
    atomicrmw add i32* %x, i32 10 seq_cst
    atomicrmw volatile umin i32* %word, i32 22 syncscope("singlethread") monotonic
    ret void
}

; ~~~ [ resume ] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

declare i32 @__gxx_personality_v0(...)

define void @resume() personality i32 (...)* @__gxx_personality_v0 {
 resume { i8*, i32 } { i8* bitcast (void ()* @resume to i8*), i32 42 }
}

; ~~~ [ getelementptr ] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

define i32* @getelementptr_1(i32* %x) {
	; Plain instruction.
	%result = getelementptr i32, i32* %x
	ret i32* %result
}

define i32* @getelementptr_2({ i32, { [2 x i32], i8 } }* %x) {
	; Indices.
	%result = getelementptr { i32, { [2 x i32], i8 } }, { i32, { [2 x i32], i8 } }* %x, i32 0, i32 1, i32 0, i32 1
	ret i32* %result
}

define i32* @getelementptr_3(i32* %x) {
	; Inbounds.
	%result = getelementptr inbounds i32, i32* %x
	ret i32* %result
}

define i32* @getelementptr_4(i32* %x) {
	; Metadata.
	%result = getelementptr i32, i32* %x, !foo !{!"bar"}, !baz !{!"qux"}
	ret i32* %result
}

define i32* @getelementptr_5({ i32, { [2 x i32], i8 } }* %x) {
	; Full instruction.
	%result = getelementptr inbounds { i32, { [2 x i32], i8 } }, { i32, { [2 x i32], i8 } }* %x, i32 0, i32 1, i32 0, i32 1, !foo !{!"bar"}, !baz !{!"qux"}
	ret i32* %result
}
