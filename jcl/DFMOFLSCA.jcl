* THESE ARE DFSORT CONTROL CARDS USED IN THE MONEYFLOW CASHFLOW JOB     00001   
* STREAM. SEE THE POST-WRITER JCL FOR DETAILS.                          00002   
* ALSO SEE PAN MEMBER DFMOFLSTI (ICETOOL).                              00003   
*                                                                       00004   
 OUTREC IFOUTLEN=132,                       FORCE LRECL=132             00005   
    IFTHEN=(WHEN=(2,5,CH,NE,C'DATE='),      DATA REC: JUST REMOVE CC    00006   
             BUILD=(2,132)),                                            00007   
    IFTHEN=(WHEN=NONE,                      DATE= RECORD                00008   
*                                           LEAVE ROOM FOR 2-CHAR AGY   00009   
             BUILD=(3:C'HDR',X'05',         BUILD HEADER RECORD         00010   
                    JP1,C'CWHDR',X'05',                                 00011   
                    JP2,X'05',              SYSTEM                      00012   
                    JP3,X'05',              2-CHAR AGENCY ALPHA         00013   
                    7,2,10,2,13,2))         CHANGE MM/DD/YY TO MMDDYY   00014   
 OUTFIL FNAMES=T1A,                                                     00015   
    INCLUDE=(1,1,CH,EQ,C'"')                DATA RECORDS                00016   
 OUTFIL FNAMES=T3,                                                      00017   
    INCLUDE=(3,3,CH,EQ,C'HDR'),             HEADER RECORD               00018   
     OVERLAY=(22:22,6,Y2W,DT=(4MD-),528:X)  CHANGE MMDDYY TO YYYY-MM-DD 00019   
