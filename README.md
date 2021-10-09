# BSGS cuda (nvidia card only) and Windows x64
Using:<br />
-h Help<br />
-t  Number of GPU threads, ex. -t 256<br />
-b  Number of GPU blocks, ex. -b 68<br />
-p  Number of pparam(value 256 is optimal), ex. -p 256<br />
-d  Select GPU IDs, ex. -d 1,2,3<br />
-pb  Set uncompressed Pubkey<br />
-pk  Range start from, ex. -pk 0x1<br />
-w  Set number of baby points 2^, ex. -w 22  mean 2^22 points <br />
<br />
Example for bat file:<br />
SET  pub=59A3BFDAD718C9D3FAC7C187F1139F0815AC5D923910D516E186AFDA28B221DC994327554CED887AAE5D211A2407CDD025CFC3779ECB9C9D7F2F1A1DDF3E9FF8<br />
SET  rangestart=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5e0000000000000000<br />
SET thread_size=512<br />
SET block_size=68<br />
SET pparam_size=256<br />
SET items_size=26<br />

bsgscudaHT2.exe -t %thread_size% -b %block_size% -p %pparam_size% -pb %pub% -pk %rangestart% -w %items_size%<br /> 
pause<br />

# Compilation
To compile the program you need Purebasic v5.31, download from https://www.purebasic.com <br />
Also need external program https://github.com/arnaud-lb/binsort <br />
This program is using to quick sort baby array. <br />
Put binsort.exe to the lib folder. <br />



