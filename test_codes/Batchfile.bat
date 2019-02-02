@echo off

setlocal EnableDelayedExpansion

set /A INIT_A=0x67452301
set /A INIT_B=0xEFCDAB89
set /A INIT_C=0x98BADCFE
set /A INIT_D=0x10325476

set /A SHIFT_AMTS[1]=7
set /A SHIFT_AMTS[2]=12
set /A SHIFT_AMTS[3]=17
set /A SHIFT_AMTS[4]=22
set /A SHIFT_AMTS[5]=5
set /A SHIFT_AMTS[6]=9
set /A SHIFT_AMTS[7]=14
set /A SHIFT_AMTS[8]=20
set /A SHIFT_AMTS[9]=4
set /A SHIFT_AMTS[10]=11
set /A SHIFT_AMTS[11]=16
set /A SHIFT_AMTS[12]=23
set /A SHIFT_AMTS[13]=6
set /A SHIFT_AMTS[14]=10
set /A SHIFT_AMTS[15]=15
set /A SHIFT_AMTS[16]=21

set /A TABLE_T[1]=0xd76aa478
set /A TABLE_T[2]=0xe8c7b756
set /A TABLE_T[3]=0x242070db
set /A TABLE_T[4]=0xc1bdceee
set /A TABLE_T[5]=0xf57c0faf
set /A TABLE_T[6]=0x4787c62a
set /A TABLE_T[7]=0xa8304613
set /A TABLE_T[8]=0xfd469501
set /A TABLE_T[9]=0x698098d8
set /A TABLE_T[10]=0x8b44f7af
set /A TABLE_T[11]=0xffff5bb1
set /A TABLE_T[12]=0x895cd7be
set /A TABLE_T[13]=0x6b901122
set /A TABLE_T[14]=0xfd987193
set /A TABLE_T[15]=0xa679438e
set /A TABLE_T[16]=0x49b40821
set /A TABLE_T[17]=0xf61e2562
set /A TABLE_T[18]=0xc040b340
set /A TABLE_T[19]=0x265e5a51
set /A TABLE_T[20]=0xe9b6c7aa
set /A TABLE_T[21]=0xd62f105d
set /A TABLE_T[22]=0x02441453
set /A TABLE_T[23]=0xd8a1e681
set /A TABLE_T[24]=0xe7d3fbc8
set /A TABLE_T[25]=0x21e1cde6
set /A TABLE_T[26]=0xc33707d6
set /A TABLE_T[27]=0xf4d50d87
set /A TABLE_T[28]=0x455a14ed
set /A TABLE_T[29]=0xa9e3e905
set /A TABLE_T[30]=0xfcefa3f8
set /A TABLE_T[31]=0x676f02d9
set /A TABLE_T[32]=0x8d2a4c8a
set /A TABLE_T[33]=0xfffa3942
set /A TABLE_T[34]=0x8771f681
set /A TABLE_T[35]=0x6d9d6122
set /A TABLE_T[36]=0xfde5380c
set /A TABLE_T[37]=0xa4beea44
set /A TABLE_T[38]=0x4bdecfa9
set /A TABLE_T[39]=0xf6bb4b60
set /A TABLE_T[40]=0xbebfbc70
set /A TABLE_T[41]=0x289b7ec6
set /A TABLE_T[42]=0xeaa127fa
set /A TABLE_T[43]=0xd4ef3085
set /A TABLE_T[44]=0x04881d05
set /A TABLE_T[45]=0xd9d4d039
set /A TABLE_T[46]=0xe6db99e5
set /A TABLE_T[47]=0x1fa27cf8
set /A TABLE_T[48]=0xc4ac5665
set /A TABLE_T[49]=0xf4292244
set /A TABLE_T[50]=0x432aff97
set /A TABLE_T[51]=0xab9423a7
set /A TABLE_T[52]=0xfc93a039
set /A TABLE_T[53]=0x655b59c3
set /A TABLE_T[54]=0x8f0ccc92
set /A TABLE_T[55]=0xffeff47d
set /A TABLE_T[56]=0x85845dd1
set /A TABLE_T[57]=0x6fa87e4f
set /A TABLE_T[58]=0xfe2ce6e0
set /A TABLE_T[59]=0xa3014314
set /A TABLE_T[60]=0x4e0811a1
set /A TABLE_T[61]=0xf7537e82
set /A TABLE_T[62]=0xbd3af235
set /A TABLE_T[63]=0x2ad7d2bb
set /A TABLE_T[64]=0xeb86d391

set fileName=%~1

set messageLenBytes=%~z1
echo messageLenBytes=%messageLenBytes%

set /A numBlocks_1="messageLenBytes + 8"
CALL :rfrs %numBlocks_1% 6 numBlocks_2
set /A numBlocks="numBlocks_2 + 1"

echo numBlocks=%numBlocks%
set /a totalLen="numBlocks << 6"

echo totalLen %totalLen%
set /a paddingSize="totalLen - messageLenBytes"

echo paddingSize %paddingSize%
set /a paddingBytes[0]="-128"

:: will not work for files greater than a size
set /a messageLenBits="messageLenBytes << 3"
echo messageLenBits=%messageLenBits%

for /l %%m in (0,1,%paddingSize%) do (
  set index=%%m
  set "paddingBytes[!index!]=0"
  )


for /l %%m in (0,1,7) do (
  set /a paddingIndex="paddingSize - 8 + %%m"
  call :byte_cast !messageLenBits! castedByte
  set /a "paddingBytes[!paddingIndex!]=!castedByte!"
  call :rfrs !messageLenBits! 8 messageLenBitsShifted
  set /a messageLenBits="!messageLenBitsShifted!"
  )

echo paddingBytes[40]=%paddingBytes[40]%
echo paddingBytes[41]=%paddingBytes[41]%
echo paddingBytes[42]=%paddingBytes[42]%
echo paddingBytes[43]=%paddingBytes[43]%
echo paddingBytes[44]=%paddingBytes[44]%
echo paddingBytes[45]=%paddingBytes[45]%
echo paddingBytes[46]=%paddingBytes[46]%
echo paddingBytes[47]=%paddingBytes[47]%


set a=%INIT_A%
set b=%INIT_B%
set c=%INIT_C%
set d=%INIT_D%

:: read a file and store the byte array data in message[index]

set dummy="!temp!\md5batch%random%.txt"
<nul >%dummy% set /p ".=A"
set dummySize=1
for /l %%n in (1,1,32) do (if !dummySize! lss %messageLenBytes% set /A "dummySize*=2" & type !dummy! >>!dummy!)
set startOffset=0
set endOffset=%messageLenBytes%
set index=0
set fileName="%~dpf1"
set skipStart=1
for /f "eol=F usebackq tokens=1,2 skip=1 delims=:[] " %%A in (`fc /b  "%fileName%" %dummy%`) do (
    set /a skipEnd=0x%%A && (
        if !skipEnd! geq %startOffset% if !skipStart! leq %endOffset% (
        for /l %%n in (!skipStart!,1,!skipEnd!) do (
          set /a "message[!index!]=0x41" 
          set /a index+=1
          )
        set /a "message[!index!]=0x%%B" 
        set /a index+=1
        set /a skipStart=skipEnd+2
      )
    )
  )
  
for /l %%n in (%skipStart%,1,%endOffset%) do (
    set /a "message[!index!]=0x41" 
    set /a index+=1
    echo %message[!index!]%
  )

del %dummy%


:: done reading

set /a nblm1="numBlocks - 1"
::for (int i = 0; i < numBlocks; i ++)
for /l %%i in (0,1,nblm1) do ( 
  set /a index="i << 6"
  echo i = %%i, index = !index!
  for /l %%j in (0,1,63) do (
    set /a index="!index! + 1"
    set /a j_rsh_2="%%j >> 2"
    
    ::echo !j_rsh_2!, !j!, j, %%j
    echo index = !index!
    echo messageLenBytes = !messageLenBytes!
    echo j_rsh_2 = !j_rsh_2!
    
    if !index! LSS !messageLenBytes! (
        rem this has no sense in batch, but it is what I wanted 
        rem set /a "buffer[!j_rsh_2!]=%message[!index!]%" 
        echo buffer[!j_rsh_2!]= %buffer[!j_rsh_2!]%
      ) else (
        echo buffer[!j_rsh_2!]= %buffer[!j_rsh_2!]%
      )
    
    ) 
  )

GOTO :EOF
for /l %%i in (0,1,15) do (
    echo buffer[%%i]= %buffer[!i!]%
    )

GOTO :EOF

:rfrs
:: java's >>> 
SET /A value=%~1
SET /A moves=%~2
SET /A usedIfNegative="(%value% >> 1 ) & 0x7fffffff"
if %moves% LEQ 0 (
    %~3=%value%
) ELSE (
    IF %value% GEQ 0 (
        set /A %~3="value >> moves" 
    ) ELSE (
        set /A %~3="usedIfNegative >> (moves - 1)"
    ) 
)
GOTO :EOF

:byte_cast
set /a x="%~1 & 0xff"
set /a isNegative="x & 0x80"
if %isNegative% GTR 0 (
    set /a x="(-1)*((~(x - 1))&0xff)"
    )
set /a "%~2=x"
exit /b
goto :EOF