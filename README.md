# BSGS cuda (nvidia card only) and Windows x64
Usage:<br />
-h   Help<br />
-t   Number of GPU threads, ex. -t 256<br />
-b   Number of GPU blocks, ex. -b 68, set equil to the SM number of your card<br />
-p   Number of keys per gpu thread, ex. -p 256<br />
-d   Select GPU IDs, ex. -d 1,2,3<br />
-pb  Set single uncompressed/compressed pubkey for searching<br />
-pk  Range start from, ex. -pk 0x1<br />
-pke End range, ex. -pke 0x100<br />
-w       Set number of hash table items 2^ or decimal representation, ex. -w 22 equil to -w 4194304  and mean 2^22 points <br />
-htsz    Set number of hash table size 2^, ex. -htsz 25, default 25 <br />
-infile  Set file with pubkey for searching in uncompressed/compressed format (search sequential), one pubkey per line<br />
-wl      Set recovery file from which the state will be loaded<br />
-wt      Set timer for autosaving current state, default 180s<br />
Current state is always saved to file currentwork.txt<br />
If app crash or you stop app, you can start working from the last saved state. Provided the launch configuration has not been changed.<br />

Recommendation for -htsz value depending on -w <br />
-htsz 27 value -w should be less or equil To 1331331443 Or 2^30.310222637591963<br />
-htsz 28 value -w should be less or equil to 1777178603 Or 2^30.726941530690112<br />
-htsz 29 value -w should be less or equil to 3069485950 Or 2^31.515349920643907<br />
-htsz 30 value -w should be less or equil to 3069485951 Or 2^31.515349920643907<br />
-htsz 31 value -w should be less or equil to 3069485951 Or 2^31.515349920643907<br />

Example options for some cards:<br />
gtx 1660super [memory used: 5036MB]<br /> 
-t 256 -b 88 -p 130 -w 29.87 -htsz 28<br />
<br /> 
rtx 2080ti [memory used: 9243MB]<br /> 
-t 256 -b 272 -p 220 -w 30.5 -htsz 29<br />
<br /> 
rtx 3070 [memory used: 6684MB]<br /> 
-t 256 -b 138 -p 244 -w 30.25 -htsz 28<br />
<br /> 
Creating htCPU, htGPU, G2 files depending on the configuration may require a significant amount of host RAM.
You can use onlygen_1_9_6File0.exe to generate files on a PC with a large amount of memory and then copy htCPU, htGPU, G2 files to a computer where video cards are installed.
<br />
All arrays packed htCPU, htGPU, G2 saved to the disk for fast spinup solver next time (if parameters -w, -htsz, -t, -b, -p  will not changed).
After you have the arrays saved on disk, you will need less RAM to launch.<br />
Change -w, -htsz impact _htCPUv and _htGPUv files<br />
Change -t, -b, -p, -w impact _g2 file<br />
 
<br />
Example bat file for single pubkey searching:<br />
SET  pub=59A3BFDAD718C9D3FAC7C187F1139F0815AC5D923910D516E186AFDA28B221DC994327554CED887AAE5D211A2407CDD025CFC3779ECB9C9D7F2F1A1DDF3E9FF8<br />
SET  rangestart=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5e0000000000000000<br />
SET thread_size=512<br />
SET block_size=68<br />
SET pparam_size=256<br />
SET items_size=26<br />
SET htsz_size=25<br />
bsgscudaHT_1_9_6file.exe -t %thread_size% -b %block_size% -p %pparam_size% -pb %pub% -pk %rangestart% -w %items_size%<br -htsz %htsz_size%/> 
pause<br />

Example bat file for sequential searching pubkeys from file:<br />
SET  rangestart=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5e0000000000000000 <br />
SET  rangeend=0x49dccfd96dc5df56487436f5a1b18c4f5d34f65ddb48cb5effffffffffffffff <br />
SET thread_size=512 <br />
SET block_size=68 <br />
SET pparam_size=256 <br />
SET items_size=26 <br />
SET htsz_size=25<br />

bsgscudaHT_1_9_6file.exe -t %thread_size% -b %block_size% -p %pparam_size%  -pk %rangestart% -pke %rangeend% -w %items_size% -htsz %htsz_size%/ -infile PUBS.txt <br />
pause <br />

Solved pubkeys will be saved to the win.txt file

# Compilation
To compile the program you need Purebasic v5.31, download from https://www.purebasic.com <br />




