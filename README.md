# BSGS cuda (nvidia card only) and Windows x64
Usage:<br />
-h   Help<br />
-t   Number of GPU threads, ex. -t 256<br />
-b   Number of GPU blocks, ex. -b 68, set equil to the SM number of your card<br />
-p   Number of pparam(value 256 is optimal), ex. -p 256<br />
-d   Select GPU IDs, ex. -d 1,2,3<br />
-pb  Set single uncompressed pubkey for searching<br />
-pk  Range start from, ex. -pk 0x1<br />
-pke End range, ex. -pke 0x100<br />
-w     Set number of baby items (maximum value 31) , ex. -w 22  mean 2^22 points <br />
-htsz  Set number of HashTable 2^, ex. -htsz 25, default 25 <br />
-infile  Set file with pubkey for searching in uncompressed format (search sequential) <br />

All arrays(Baby, Baby sorted, Giant) and hashtable saved to the disk for fast spinup solver next time (if parameters will not changed).
<br />
Example bat file for single pubkey searching:<br />
SET  pub=59A3BFDAD718C9D3FAC7C187F1139F0815AC5D923910D516E186AFDA28B221DC994327554CED887AAE5D211A2407CDD025CFC3779ECB9C9D7F2F1A1DDF3E9FF8<br />
SET  rangestart=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5e0000000000000000<br />
SET thread_size=512<br />
SET block_size=68<br />
SET pparam_size=256<br />
SET items_size=26<br />

bsgscudaHT2.exe -t %thread_size% -b %block_size% -p %pparam_size% -pb %pub% -pk %rangestart% -w %items_size%<br /> 
pause<br />

Example bat file for sequential searching pubkeys from file:<br />
SET  rangestart=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5e0000000000000000 <br />
SET  rangeend=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5effffffffffffffff <br />
SET thread_size=512 <br />
SET block_size=68 <br />
SET pparam_size=256 <br />
SET items_size=26 <br />

bsgscudaHT2.exe -t %thread_size% -b %block_size% -p %pparam_size%  -pk %rangestart% -pke %rangeend% -w %items_size% -infile PUBS.txt <br />
pause <br />

Solved pubkeys will be saved to the win.txt file

# Compilation
To compile the program you need Purebasic v5.31, download from https://www.purebasic.com <br />
Also need external program https://github.com/arnaud-lb/binsort <br />
This program is using to quick sort baby array. <br />
Put binsort.exe to the lib folder. <br />



