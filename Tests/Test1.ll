; ModuleID = 'Test1.c'
target datalayout = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.9.0"

@i = common global i32 0, align 4
@j = common global i32 0, align 4

; Function Attrs: nounwind ssp uwtable
define void @main1() #0 {
  %1 = load i32* @i, align 4
  %2 = icmp ne i32 %1, 0
  br i1 %2, label %6, label %3

; <label>:3                                       ; preds = %0
  %4 = load i32* @j, align 4
  %5 = icmp ne i32 %4, 0
  br label %6

; <label>:6                                       ; preds = %3, %0
  %7 = phi i1 [ true, %0 ], [ %5, %3 ]
  %8 = zext i1 %7 to i32
  store i32 %8, i32* @i, align 4
  ret void
}

attributes #0 = { nounwind ssp uwtable "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "stack-protector-buffer-size"="8" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.ident = !{!0}

!0 = metadata !{metadata !"Apple LLVM version 5.1 (clang-503.0.40) (based on LLVM 3.4svn)"}
