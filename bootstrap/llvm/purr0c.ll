; purr0c.ll - hand-written LLVM IR seed "compiler" (emitter stub)
;
; This file is an intentionally tiny, auditable LLVM IR module that acts as a
; *seed compiler*: when linked into an executable (`purr0c`), it emits a small
; LLVM IR module (text) representing the compiled output for a trivial purr0
; program. The real bootstrap compiler can start here and grow gradually.
;
; Usage (example):
;  $ llc -filetype=obj purr0c.ll -o purr0c.o
;  $ clang -o purr0c purr0c.o -lm
;  $ ./purr0c > program.ll
;  $ llc -filetype=obj program.ll -o program.o
;  $ clang -o program program.o -lm
;
; The emitter below simply prints a small LLVM IR module that defines
; `@purr_main` returning 0. Replace or extend this with a real emitter that
; reads purr0 source and outputs corresponding LLVM IR.

declare i32 @puts(i8*)

a = private unnamed_addr constant [112 x i8] c"; ModuleID = 'purr0_emitted'\0Asource_filename = \"purr0_emitted\"\0A\0Adefine i32 @purr_main() {\0Aentry:\0A  ret i32 0\0A}\0A\00"

; entry point of the seed compiler executable
define i32 @main(i32 %argc, i8** %argv) {
entry:
  %ptr = getelementptr [112 x i8], [112 x i8]* @a, i32 0, i32 0
  call i32 @puts(i8* %ptr)
  ret i32 0
}
