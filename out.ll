; ModuleID = 'tox'
source_filename = "tox"

@stringcalltmpdivtmpmultmpsubtmpaddtmpcmptmp = internal constant [1 x i8] c"a"
@stringcalltmpdivtmpmultmpsubtmpaddtmpcmptmp.1 = internal constant [1 x i8] c"a"

define i64 @main() {
entry:
  ret [1 x i8]* fadd ([1 x i8]* @stringcalltmpdivtmpmultmpsubtmpaddtmpcmptmp, [1 x i8]* @stringcalltmpdivtmpmultmpsubtmpaddtmpcmptmp.1)
}
