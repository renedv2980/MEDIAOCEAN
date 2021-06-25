* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLNTI (ICETOOL).                              00003   
*                                                                       00004   
 OUTREC IFOUTLEN=132,                       FORCE LRECL=132             00005   
    IFTHEN=(WHEN=(2,5,CH,NE,C'DATE='),      DATA REC: JUST REMOVE CC    00006   
             BUILD=(2,132)),                                            00007   
    IFTHEN=(WHEN=NONE,                      DATE= RECORD                00008   
             BUILD=(C'HDR',X'05',           BUILD HEADER RECORD         00009   
             JP1,C'CWHDR',X'05',                                        00010   
             JP2,X'05',                     SYSTEM                      00011   
             JP3,X'05',                     2-CHAR AGENCY ALPHA         00012   
             7,2,10,2,13,2))                CHANGE MM/DD/YY TO MMDDYY   00013   
 OUTFIL FNAMES=T1,                                                      00014   
    INCLUDE=(1,1,CH,EQ,C'"')                DATA RECORDS                00015   
 OUTFIL FNAMES=T3,                                                      00016   
    INCLUDE=(1,3,CH,EQ,C'HDR'),             HEADER RECORD               00017   
     OVERLAY=(20:20,6,Y2W,DT=(4MD-),396:X)  CHANGE MMDDYY TO YYYY-MM-DD 00018   
