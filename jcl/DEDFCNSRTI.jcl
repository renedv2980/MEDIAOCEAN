*                                                                       00001   
* This is an ICETOOL utility intended to catch problems with the        00002   
* Nielsen National Cable Weekly MIT file. It has not been tested        00003   
* universally; i.e., we don't know if it's safe to run with every type  00004   
* of Nielsen MIT file.                                                  00005   
*                                                                       00006   
* The components in this utility are found in Panvalet members:         00007   
*                                                                       00008   
* DEDFCNSRC1 (DDNAME COP1CNTL)                                          00009   
* DEDFCNSRJ1 (DDNAME JON1CNTL)                                          00010   
* DEDFCNSRJ3 (DDNAME JON3CNTL)                                          00011   
* DEDFCNSRJ4 (DDNAME JON4CNTL)                                          00012   
* DEDFCNSRSY (DDNAME SYMNAMES)                                          00013   
* DEDFCNSRS1 (DDNAME SRT1CNTL)                                          00014   
* DEDFCNSRTI (DDNAME TOOLIN)                                            00015   
*                                                                       00016   
* TOOLIN control cards                                                  00017   
*                                                                       00018   
* Break up the MIT file into sub-files so we can work with it.          00019   
COPY FROM(FILIN) USING(COP1)                                            00020   
* Isolate any "04"/"P" records without corresponding "04"/"D" records.  00021   
COPY JKFROM TO(MISSING) USING(JON1)                                     00022   
* Isolate any "06"/"P" records without corresponding "06"/"C" records.  00023   
COPY JKFROM TO(MISSING) USING(JON3)                                     00024   
* Remove the unmatched records from the original file.                  00025   
COPY JKFROM TO(T1) USING(JON4)                                          00026   
* Reconstruct the original file without the bad records.                00027   
SORT FROM(T1) TO(MITOUT) USING(SRT1)                                    00028   
* Print a report of the unmatched records.                              00029   
DISPLAY FROM(MISSING) LIST(SHOWBAD) NOCC BLANK -                        00030   
        TITLE('UNMATCHED RECORD REPORT') -                              00031   
        DATE TIME PAGE -                                                00032   
        HEADER('MIT RECORD (FIRST 84 COLUMNS)') ON(MIT_KEY) -           00033   
        HEADER('MIT FILE REC#') ON(SEQUENCE_NUMBER,UFF)                 00034   
* Set RC=4 if there are any unmatched records.                          00035   
COUNT FROM(MISSING) NOTEMPTY RC4                                        00036   
*                                                                       00037   
