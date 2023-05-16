; ModuleID = 'saber'


 


declare external ccc  i64* @malloc(i64)    


declare external ccc  void @printInt(i64)    


define external ccc  i64 @x30(i64  %x53, i64  %x22)    {
entry:
  %x29 = inttoptr i64 %x53 to {i64, i64 (i64)*}* 
  ret i64 %x22 
}


define external ccc  i64 @x27(i64  %x52, i64  %x20)    {
entry:
  %x26 = inttoptr i64 %x52 to {i64, i64 (i64)*}* 
  ret i64 %x20 
}


define external ccc  i64 (i64)* @x25(i64  %x51, i64 (i64)*  %x18)    {
entry:
  %x24 = inttoptr i64 %x51 to {i64, i64 (i64)* (i64 (i64)*)*}* 
  ret i64 (i64)* %x18 
}


define external ccc  i64 @main()    {
entry:
  %x36 =  call ccc  i64*  @malloc(i64  8)  
  %x17 = ptrtoint i64* %x36 to i64 
  %x37 = getelementptr  {i64}*, {i64}** %x36, i64 0 
  %x38 = ptrtoint i64 (i64)* (i64 (i64)*)* @x25 to i64 
  store  i64 %x38, i64* %x37, align 64 
  %x39 =  call ccc  i64*  @malloc(i64  8)  
  %x19 = ptrtoint i64* %x39 to i64 
  %x40 = getelementptr  {i64}*, {i64}** %x39, i64 0 
  %x41 = ptrtoint i64 (i64)* @x27 to i64 
  store  i64 %x41, i64* %x40, align 64 
  %x43 = inttoptr {i64}* %x17 to i64* 
  %x42 = getelementptr  i64, i64* %x43, i64 0 
  %x28 = load  i64, i64* %x42, align 64 
  %x6 =  call ccc  i64 (i64)*  @x28({i64}*  %x17, {i64}*  %x19)  
  %x44 =  call ccc  i64*  @malloc(i64  8)  
  %x21 = ptrtoint i64* %x44 to i64 
  %x45 = getelementptr  {i64}*, {i64}** %x44, i64 0 
  %x46 = ptrtoint i64 (i64)* @x30 to i64 
  store  i64 %x46, i64* %x45, align 64 
  %x48 = inttoptr {i64}* %x21 to i64* 
  %x47 = getelementptr  i64, i64* %x48, i64 0 
  %x31 = load  i64, i64* %x47, align 64 
  %x9 =  call ccc  i64  @x31({i64}*  %x21, i64  3)  
  %x50 = inttoptr i64 (i64)* %x6 to i64* 
  %x49 = getelementptr  i64, i64* %x50, i64 0 
  %x32 = load  i64, i64* %x49, align 64 
  %x5 =  call ccc  i64  @x32(i64 (i64)*  %x6, i64  %x9)  
   call ccc  void  @printInt(i64  %x5)  
  ret i64 0 
}