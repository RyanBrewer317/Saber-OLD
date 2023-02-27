; ModuleID = 'saber'


 


declare external ccc  i64* @malloc(i64)    


declare external ccc  void @printInt(i64)    


define external ccc  i64 @x10(i64  %x8, i64  %x2)    {
entry:
  %x26 = inttoptr i64 %x8 to i64* 
  %x25 = getelementptr  i64, i64* %x26, i64 1 
  %x1 = load  i64, i64* %x25, align 64 
  ret i64 %x1 
}


define external ccc  i64 @x12(i64  %x7, i64  %x1)    {
entry:
  %x18 =  call fastcc  i64*  @malloc(i64  16)  
  %x5 = ptrtoint i64* %x18 to i64 
  %x20 = getelementptr  i64, i64* %x18, i64 0 
  %x21 = ptrtoint i64 (i64, i64)* @x10 to i64 
  store  i64 %x21, i64* %x20, align 64 
  %x19 = getelementptr  i64, i64* %x18, i64 1 
  store  i64 %x1, i64* %x19, align 64 
  %x23 = inttoptr i64 %x5 to i64* 
  %x22 = getelementptr  i64, i64* %x23, i64 0 
  %x9 = load  i64, i64* %x22, align 64 
  %x24 = inttoptr i64 %x9 to i64 (i64, i64)* 
  %x6 =  call fastcc  i64  %x24(i64  %x5, i64  2)  
  ret i64 %x6 
}


define external ccc  i64 @main()    {
entry:
  %x27 =  call fastcc  i64*  @malloc(i64  8)  
  %x3 = ptrtoint i64* %x27 to i64 
  %x28 = getelementptr  i64, i64* %x27, i64 0 
  %x29 = ptrtoint i64 (i64, i64)* @x12 to i64 
  store  i64 %x29, i64* %x28, align 64 
  %x31 = inttoptr i64 %x3 to i64* 
  %x30 = getelementptr  i64, i64* %x31, i64 0 
  %x11 = load  i64, i64* %x30, align 64 
  %x32 = inttoptr i64 %x11 to i64 (i64, i64)* 
  %x4 =  call fastcc  i64  %x32(i64  %x3, i64  3)  
   call fastcc  void  @printInt(i64  %x4)  
  ret i64 0 
}