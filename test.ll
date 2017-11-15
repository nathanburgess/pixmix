; ModuleID = 'PixMix'
source_filename = "PixMix"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@tmp = private unnamed_addr constant [3 x i8] c"hi\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.3 = private unnamed_addr constant [4 x i8] c"%s\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %a = alloca i8*
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @tmp, i32 0, i32 0), i8** %a
  %a1 = load i8*, i8** %a
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), i8* %a1)
  ret i32 0
}

define i32 @add(i32 %x, i32 %y) {
entry:
  %x1 = alloca i32
  store i32 %x, i32* %x1
  %y2 = alloca i32
  store i32 %y, i32* %y2
  %x3 = load i32, i32* %x1
  %y4 = load i32, i32* %y2
  %tmp = add i32 %x3, %y4
  ret i32 %tmp
}
