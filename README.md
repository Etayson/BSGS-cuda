# BSGS cuda (nvidia card only) and Windows x64
Usage:<br />
-h   Help<br />
-t   Number of GPU threads, ex. -t 256<br />
-b   Number of GPU blocks, ex. -b 68, set equil to the SM number of your card<br />
-p   Number of pparam(value 256 is optimal), ex. -p 256<br />
-d   Select GPU IDs, ex. -d 1,2,3<br />
-pb  Set single uncompressed/compressed pubkey for searching<br />
-pk  Range start from, ex. -pk 0x1<br />
-pke End range, ex. -pke 0x100<br />
-w     Set number of baby items 2^, ex. -w 22  mean 2^22 points <br />
-htsz  Set number of HashTable 2^, ex. -htsz 25, default 25 <br />
-infile  Set file with pubkey for searching in uncompressed/compressed format (search sequential), one pubkey per line. <br />
-wl      Set recovery file from which the state will be loaded.<br />
-wt      Set timer for autosaving current state, default 180s.<br />
Current state is always saved to file currentwork.txt
If app crash or you stop app, you can start working from the last saved state. Provided the launch configuration has not been changed.

Note! set minimal -htsz value depending on -w <br />
-w 31  -htsz 29 need around 64GB of RAM to generate all arrays<br />
-w 30  -htsz 28 need around 32GB of RAM to generate all arrays<br />
-w 29  -htsz 28 <br />
-w 28  -htsz 27 <br />
-w 27  -htsz 25 <br />
if you set low -htsz value then you can get error during sorting HT, increase -htsz to solve issue.<br />

-w 30 good for GPU with memory 11Gb<br />
-w 29 good for GPU with memory 8Gb<br />
-w 28 or -w27 good for GPU with memory 6Gb<br />

All arrays(Baby, Giant) and hashtable saved to the disk for fast spinup solver next time (if parameters will not changed).
After you have the arrays saved, you will need less RAM to launch.
<br />
Example bat file for single pubkey searching:<br />
SET  pub=59A3BFDAD718C9D3FAC7C187F1139F0815AC5D923910D516E186AFDA28B221DC994327554CED887AAE5D211A2407CDD025CFC3779ECB9C9D7F2F1A1DDF3E9FF8<br />
SET  rangestart=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5e0000000000000000<br />
SET thread_size=512<br />
SET block_size=68<br />
SET pparam_size=256<br />
SET items_size=26<br />

bsgscudaHT_1_7_2.exe -t %thread_size% -b %block_size% -p %pparam_size% -pb %pub% -pk %rangestart% -w %items_size%<br /> 
pause<br />

Example bat file for sequential searching pubkeys from file:<br />
SET  rangestart=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5e0000000000000000 <br />
SET  rangeend=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5effffffffffffffff <br />
SET thread_size=512 <br />
SET block_size=68 <br />
SET pparam_size=256 <br />
SET items_size=26 <br />

bsgscudaHT_1_7_2.exe -t %thread_size% -b %block_size% -p %pparam_size%  -pk %rangestart% -pke %rangeend% -w %items_size% -infile PUBS.txt <br />
pause <br />

Solved pubkeys will be saved to the win.txt file

# Compilation
To compile the program you need Purebasic v5.31, download from https://www.purebasic.com <br />




