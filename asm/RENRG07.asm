*          DATA SET RENRG07    AT LEVEL 114 AS OF 05/01/02                      
*          DATA SET RENRG02    AT LEVEL 102 AS OF 08/02/00                      
*PHASE T81502C                                                                  
         TITLE 'T81502 RRGON REC DISPLAY AND LIST'                              
***********************************************************************         
* PROFILE BIT USE:                                                    *         
* BYTE 0, X'80'  -  SUPPRESS BUDGET COLUMN                            *         
* BYTE 0, X'40'  -  SUPPRESS BUDGET IF REQUEST IS FROM STATION SIDE.  *         
* BYTE 0, X'20'  -  USE STRAIGHT MONTH TO MONTH, NOT 4/5 WK PACING    *         
* BYTE 0, X'10'  -  USE COMPANY/OFFICE BUDGET FOR OFFICE LIST         *         
* BYTE 0, X'08'  -  NEW PACING                                        *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
* HISTORY OF CHANGES                                                  *         
***********************************************************************         
*                                                                     *         
* FEB11/97 (BG ) 4-5 ADD HARD CODED *& QLCON/QLDCT SET                *         
*                                                                               
* FEB12/97 (BG )  6  CK FOR FULL STEREO VS EMULATOR                   *         
*                                                                               
* FEB25/97 (BG )  7  ADD LISTS OF LONG NAMES                          *         
*                                                                               
* MAR04/97 (BG )  8  ADD NEW SEQ                                      *         
*                                                                     *         
* MAR04/97 (BG )  9  ADD TOP N                                        *         
*                                                                     *         
* MAR09/97 (BG ) 10  ADD SEQUENCES, IRNY CODE                         *         
*                                                                     *         
* MAR11/97 (BG ) 11  FIX TOP N CODE                                   *         
*                                                                     *         
* MAR14/97 (BG ) 12  PUT IN SAVE FOR TOP N ABOVE 13                   *         
*                                                                     *         
* MAR21/97 (BG ) 13  SAVE MONTH OF REC READ IN TOP TABLE              *         
*                    DO ROUNDING FOR L=GS, DON'T SEND CLAS CODE - CAT *         
*                                                                     *         
* APR03/97 (BG ) 14/15 FIX L=MON 2ND SCRN BUG, ROUND FOR STEREO LST   *         
*                                                                     *         
* APR10/97 (BG ) 16 STOP IRNY FROM DOING L=STA, FILTERING ON OWNER    *         
*                        KRGNY                                        *         
*                                                                     *         
* APR14/97 (BG ) 17 FIX GROUP/SUB GROUP NAMES 1 BLK AWAY              *         
*                                                                     *         
* APR22/97 (BG ) 18 STEREO - SHOW PACING, SHOW BOOKED WITH NAMES      *         
*                                                                     *         
* MAY06/97 (BG ) 19 FIX SEL FOR TOPN                                  *         
*                                                                     *         
* MAY16/97 (BG ) 20 FIX STA LIST BOOKED AND MONTHLY - BYPASS LIST     *         
*                                                                     *         
* MAY21/97 (BG ) 21 IF NO DATA DISPLAY ER/0249 NO RRGON DATA TO DISP  *         
*                                                                     *         
* MAY23/97 (BG ) 22 DROP HB FROM HARD CODE                            *         
*                                                                     *         
* MAY28/97 (BG ) 23 STOP STATION SETS WITH STATION LIST               *         
*                                                                     *         
* MAY28/97 (BG ) 24 SET OF SETS/EXCLUDE FIX SHORT STATIONS TO NULL    *         
*                                                                     *         
* JUN09/97 (BG ) 25 SET OF SETS EXCLUDE, * FOR ROUNDING               *         
*                                                                     *         
* JUN25/97 (BG ) 26 CHANGE CATEGORY TO CATEGOR FOR STEREO             *         
*                                                                     *         
* JUL01/97 (BG ) 27 BYPASS GROUP R DOLLP WITH ONLY BUDGET $           *         
*                                                                     *         
* JUL02/97 (BG ) 28 CHANGE CATEGOR  TO CTGRY FOR STEREO               *         
*                                                                     *         
* JUL21/97 (BG ) 29 CHANGE FILTER ON STATION FOR TEAM TO DIRECT READ  *         
*                   ADD TOTALS LINE FOR LIST FUNCTION                 *         
*                   SHOW STATION MARKET NAME IF STATION FILTER        *         
*                   PERIOD FLD FROM 11 TO 14, SHOW ROUNDED (000)      *         
* JUL21/97 (BG ) 30 ADD TEMP FIX FOR GRP/OFF/REG                      *         
* JUL24/97 (BG ) 31 MAKE ALL EDITS SHOW MINUS TO LEFT                 *         
*                   FIX TOTALS LINE CHG FROM L=MON AND STEREO         *         
*                   MAKE ALL EDITS LEADING MINUS SIGN, NOT TRAILING   *         
*                   ELIM TOTALS LINE FOR STEREO                       *         
*                   LEAVE MKT/STA DESCRIP UP THRU TOTALS LINE         *         
* AUG11/97 (BG ) 32 STEREO - SHOW MKT NAME FOR STATION LIST           *         
*                   FIX ASTERISK FOR BUDGET                           *         
*                   STOP LIST ACTIONS CHANGE/DELETE                   *         
* AUG15/97 (BG ) 34 ABORTED TWA SAVE                                  *         
* AUG22/97 (BG ) 35 CATCHIOS, MORE MEMORY FOR REFRESH LIST            *         
*                   COMPANY BUDGETS, KILL LASTFILT                    *         
* SEP02/97 (BG ) 36 PUT IN GROUP SUB TEST FOR SETS                    *         
* SEP30/97 (BG ) 37 FIX COBUD RESTORE KEYS, NEW SRT SEQ - STA/GROUP   *         
*                   SAVE CODETABL IN TWA'S                            *         
*                   FIX SAME LINES REPEATING ON MAX I/O ERROR         *         
*                   FIX BUDGET IN TOTALS LINE                         *         
*                   FIX COBUD SETTING OFF COMPANY BUDGETS FLAG        *         
*                   SHOW OFFICE SECURITY ERR AS SEC ERR, NOT INVAL OFF*         
* OCT21/97 (BG ) 38 CHANGE TOPNMS FRON 1-50 TO 1-100                  *         
* NOV20/97 (BG ) 39 ADD CAGENCY - CORE AGENCY - 4 CHARACTER           *         
*                   ADD SET NAMES FOR STEREO REFRESH                  *         
* NOV26/97 (BG ) 40 FIX PRIOR YEAR BUG - BUT WHERE WAS ZERO MONTH GEND*         
*                   ADD DEV CON TYPE TO @@ NAMES                      *         
* DEC01/97 (BG ) 41 FIX KATZ K3GRPTBL LOOKUP, SFIL FOR QLSTA LIST     *         
* DEC02/97 (BG ) 42 FIX LIMIT ACCESS LOOP                             *         
* DEC19/97 (BG ) 43 ADD AFFIL TO CODES RETURNED TO STEREO             *         
*                   SEND GROUP WITH SET INPUT, SHOW SET NAMES         *         
* JAN06/98 (BG ) 44 DON'T SEND ZERO MONTH FOR STEREO                  *         
*                   ALLOW COMBO DATA EVEN IF NO MEDIA ENTERED         *         
* JAN15/98 (BG ) 45 IF QLAGENCY NOT FOUND, TRY QLAGY                  *         
*                   FIX MKT FOR 2/3 CHAR MKT CODES                    *         
* FEB03/98 (BG ) 46 PRINT TOTALS FOR NOW REPORT, ERROR FOR NOW L=MON  *         
* FEB05/98 (BG ) 47 REMOVE COUNTS & FIX HEADHK BUG                    *         
* MAR30/98 (BU )    ADD L7/CABALLERO TV TO COMBREPS TABLE             *         
* APR16/98 (BG ) 49 ADD USAGE COUNTS                                  *         
* MAY01/98 (BG ) 50 MOVE THIS WEEK LAST YEAR BOOKED FOR STEREO        *         
*                                                                     *         
* MAY19/98 (BU )    ADD IB/ABC RADIO    TO COMBREPS TABLE             *         
*                   MOVE STEREO TOTALS                                *         
* JUL10/98 (BG ) 52 ADD NEW NEX PACING ON PROFILE, SALESPERSON        *         
*                   FIX WARNDATE MSG TO SHOW WHEN 12 MONTHS REQUESTED *         
*                   FIX K3GRPCT USING R0 / MAKE CODETABL SOFT         *         
* JUL10/98 (BG)  53 BLAIR, FOX, & PETNY - ALL DIRECT                  *         
* JUL13/98 (BG ) 54 DO UNKNOWN FOR NO OFFICE RECORD                   *         
*                                                                     *         
* AUG21/98 (BG ) 55 PULL SALESPERSON, CHANGE TO NEW SCHEME OF THIS    *         
*                    YEAR/PREV YEAR                                   *         
*                    THIS YEAR IS SHOWN AS AT TODAY, LAST ASAT YR-1   *         
*                    PREV YR IS SHOWN AS AT TODAY, PREV-1 ASAT YR-1   *         
*                    ADD COMPANY/OFFICE BUDGETS TO LIST               *         
* SEP17/98 (BG ) 56 FIX MAX I/OS ERR MSG & CO/OFF BUDGETS             *         
* OCT08/98 (BG ) 57 MAKE PACING AUTO MATIC - TRIGERED ON YEAR 99      *         
* OCT21/98 (BG ) 58 ADD SECURITY WITH PASS BACK                       *         
* NOV02/98 (BG ) 59 BYPASS YTD LAST YEAR AS AT TODAY - USE PERIOD     *         
*                   ALSO BYPASS ALL YTD, USING PERIOD INSTEAD         *         
* NOV11/98 (BG ) 60 MASTER REP - LOOK ON ALL SUB-REPS FOR PASSIVE     *         
*                   STA/OWN/MKT RECS                                  *         
* NOV19/98 (BG ) 61 FIX PRIOR BOOKED FOR LIST                         *         
* JAN15/99 (BG ) 62 FIX 3 CHAR STATION CALL TO CKSEC                  *         
* JAN18/99 (BG ) 63 FIX LIST STA, FILTER ON OWNER                     *         
* JAN26/99 (BG ) 64 MAKE OWNER NG FOR MSTR REP MSG, FIX HDG NOW RPTS  *         
* FEB09/99 (BG ) 65 FIX DESCRIPTOR MOVE FROM 7 TO 8 SFIL0334          *         
*                   FIX NOW REPORT NOT STARTING AT BEGINNING          *         
* MAR18/99 (BG ) 66 FIX BAD PCTCOMP RTN,                              *         
*                    AND FIX KATZ BUDGETS, ER MSG FOR STATYPE         *         
* MAR25/99 (BU )    ADD NX/PUB RADIO    TO COMBREPS TABLE             *         
* MAR31/99 (BG ) 68 CK STATYPE ON THE FLY, ENABLE THIS YR             *         
*                   ADD STA SECURITY - NO BUDGET CKG                  *         
* APR13/99 (BG ) 69 ADD CUMULUS TO COMBO REPS                         *         
* MAY07/99 (BG ) 70 SHOW TOTALS LINES FOR STEREO, SALESPERSON         *         
* MAY11/99 (BG ) 71 FIX DOUBLE TOTALS LINE                            *         
*                   ADD V5 NPRANY NATLN PUBLIC RADIO TO COMBREPS      *         
* JUN29/99 (BG ) 72 BYPASS NEW MASTER REP STATION RECS                *         
* AUG16/99 (BU ) 73 ADD SPECTRUM TO COMBO REPS                        *         
* SEP28/99 (BG ) 74 FIX VKEY0564 DATCON RETURN X'FA'                  *         
*                   AND ???? GETTING WRONG BUCKETS                    *         
* NOV23/99 (BG ) 75 IF LISTING STATIONS, FILTER ON GRP IF PRESENT     *         
* DEC06/99 (BU ) 76 ADD CLEAR CHANNEL TO COMBO REPS FOR KATZ          *         
*          (BG ) 77 ADD SUB-REPS TO KATZ                              *         
* JAN21/00 (BG ) 78 CHANGE BUDGET KEY FROM F0 TO FA                   *         
* FEB24/00 (BG ) 79 ADD SETS FOR MARKET                               *         
* MAR08/00 (BG ) 80 CLEAR BLOCK IN CKSEC, FIX SP SET                  *         
* APR03/00 (BG ) 81 FIX PACING                                        *         
* APR24/00 (BU ) 82 ADD KATZ INTERACTIVE MEDIA TO COMBO REPS          *         
* JUL06/00 (BG ) 83 FIX COMPANY BUDGETS, CLEAR TOPNCT AT END          *         
* JUL14/00 (BU )    L=OFF, WITH AN OFFICE FILTER (NOT KEY)            *         
* AUG10/00 (BU )    SAME AS JUL14, PERMIT GROUP/SUBGROUP              *         
* AUG29/00 (BU )    FIX STEREO ##END PROBLEM                          *         
* nov30/00 (Bg )113 FIX company budgets for list                      *         
*                                                                     *         
*                                                                     *         
****>== NOTE BOOKED FOR REPORT NOT DONE YET                           *         
*                                                                     *         
*                    ***  END TOMBSTONE  ***                          *         
***********************************************************************         
*                                                                     *         
* TSAR IS USED TO GENERATE TOP N LISTS                                *         
*      0 -  3 SORTING VALUE - CURR BILL/PREV BILL/FINAL               *         
*      4 - 27 KEY VALUES                                              *         
*     27 - 50 BUCKETS                                                 *         
*     51 - 51 MONTH FOUND                                             *         
*                                                                     *         
***********************************************************************         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CUSOR     *         
*          R3 - WORK                                                  *         
*          R4 - WORK REG & KEY DSECT POINTER                          *         
*          R5 - WORK REG                                              *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SECOND BASE - NOTE DO NOT USE IN MNODS                *         
*          R9 - POINTER TO SYSD                                       *         
*          RA - POINTER TO ATWA                                       *         
*          RB - FIRST BASE                                            *         
*          RC - POINTER TO GEND                                       *         
*                                                                     *         
***********************************************************************         
T81502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T81502,R8,RR=R2                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO                                                          
         ST    RC,SVRC                                                          
         SPACE 3                                                                
         OI    GENSTAT5,NOCHGLST   NO CHANGES FROM LIST                         
         OI    GENSTAT4,NODELLST   DELETE FROM LIST NOT ALLOWED                 
         SPACE                                                                  
*        CLC   LASTFILT+2(2),=C'**' SPECIAL REFRESH FLAG?                       
*        BNE   MAIN0010            NO                                           
*        XC    LASTFILT,LASTFILT   CLEAR LAST FILTER VALUE                      
*                                  DON'T DO ANY OTHER MODES                     
*        B     EXIT                                                             
MAIN0010 DS   0H                                                                
         CLI   MODE,RECADD         ADD REC - NO WAY!                            
         BE    BADTRY               YES                                         
         CLI   MODE,RECDEL         DEL REC - NO WAY!                            
         BE    BADTRY               YES                                         
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         SPACE                                                                  
         TM    CONWHENH+4,X'20'    IS VALIDATED BIT STILL ON?                   
         BO    MAIN0014             YES                                         
         SPACE                                                                  
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    VK                                                               
         SPACE                                                                  
MAIN0014 DS   0H                                                                
         CLI   LSTTYPE,QLMON       L=MONTH OPTION?                              
         BNE   MAIN0020            NO  - PROCESS NORMALLY                       
         CLI   MODE,DISPKEY        DISPLAY KEY OPTION?                          
         BE    EXIT                YES - SKIP IT                                
MAIN0020 DS   0H                                                                
*        CLC   LASTFILT+2(2),=C'**' SPECIAL REFRESH FLAG?                       
*        BE    EXIT                YES                                          
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   MAIN0030                                                         
         GOTO1 =A(DK),RR=RELO                                                   
         B     EXIT                                                             
MAIN0030 DS   0H                                                                
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BNE   EXIT                                                             
*                                                                               
         GOTO1 =A(BLDMON),(RC),RR=RELO                                          
*                                  BUILD MONTH TABLE                            
*                                  WITHIN FILE PERIOD                           
         GOTO1 =A(DR),RR=RELO                                                   
EXIT     DS   0H                                                                
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        VALIDATE KEY ROUTINE                                                   
*                                                                               
VK       DS   0H                                                                
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    VKEY0003             YES                                         
         SPACE                                                                  
         CLI   LISTEND,C'Y'        END OF DATA FROM PRIOR PASS?                 
         BNE   VKEY0003            NO                                           
         CLI   QREFRESH,C'Y'       YES - 'REFRESH' REQUEST?                     
         BE    EXIT                YES - DON'T REVALIDATE                       
         OC    TSARCT,TSARCT                                                    
         BNZ   EXIT                                                             
VKEY0003 DS   0H                                                                
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   VKEY0005            NO                                           
         GOTO1 =A(REFRESH),RR=RELO                                              
*                                  YES - CHECK FOR SPECIAL OPTION               
VKEY0005 DS   0H                                                                
         MVI   COMPBUDG,C'Y'       SET 'RETRIEVE COMPANY BUDGET' ON             
         SPACE                                                                  
* CLEAR SAVED SETS AREAS *                                                      
         SPACE                                                                  
         XC    SETCDES,SETCDES     ALL SET CODES                                
         LR    R0,R9                                                            
         AHI   R0,SET1TAB-SYSD                                                  
         LA    R1,(L'SET1TAB*3)                                                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
         XC    LMONKEY,LMONKEY     CLEAR LIST=MONTH KEY                         
         XC    TSARCT,TSARCT                                                    
         SPACE                                                                  
         MVI   FULLSTER,C'N'                                                    
         CLI   OFFLINE,C'Y'        OFFLINE                                      
         BE    VKEY0007             YES, NO STEREO                              
*                                                                               
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   VKEY0007             NO                                          
*                                                                               
         MVC   DMCB(4),XFF                                                      
         XC    DMCB+4(4),DMCB+4                                                 
         LA    R1,DMCB                                                          
         GOTO1 SWITCH                                                           
         L     RE,0(R1)                                                         
         SPACE                                                                  
         TM    TSTAT6-UTLD(RE),TST6STRO+TST6STFU THIS A FULL STEREO             
         BNO   VKEY0007                           NO                            
         MVI   FULLSTER,C'Y'                                                    
*                                                                               
VKEY0007 DS   0H                                                                
         MVC   RPINBTS,DMINBTS     SAVE DATAMGR IN/OUT BITS                     
         MVC   RPOUTBTS,DMOUTBTS                                                
         BAS   RE,SETREPFL         SET UP REP FILE VALUES                       
*                                                                               
         XC    AFSTFLD,AFSTFLD                                                  
*                                                                               
* CLEAR ALL FILTERS                                                             
*                                                                               
         XC    QREGION(QLIST-QREGION),QREGION                                   
*                                                                               
         LA    R2,RRGREGH          REGION                                       
         CLI   5(R2),0                                                          
         BE    VKEY0010                                                         
* DOES NOT GET COMPANY BUDGET                                                   
         GOTO1 VALIRGN                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0010 LA    R2,RRGOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VKEY0020                                                         
         GOTO1 VALIOFF                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0020 LA    R2,RRGPYH           PERIOD/YTD                                   
         GOTO1 ANY                                                              
         GOTO1 VALIPY                                                           
*                                                                               
         LA    R2,RRGPERH          PERIOD                                       
         GOTO1 ANY                                                              
         GOTO1 VALIPRD                                                          
*                                                                               
         LA    R2,RRGGRPH          GROUP                                        
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKEY0030            NO                                           
         CLI   5(R2),1             YES - SINGLE CHARACTER GROUP?                
         BE    VKEY0025            YES - CAN GET 'COMPANY BUDGET'               
         SPACE                                                                  
         CLC   AGENCY,=C'K3'       AND KATZ (FOR NOW)                           
         BE    VKEY0025             YES - CAN GET 'COMPANY BUDGET'              
         SPACE                                                                  
         MVI   COMPBUDG,C'N'       SET 'COMPANY BUDGET' OFF                     
         SPACE                                                                  
VKEY0025 EQU   *                                                                
         GOTO1 VALIGRP                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0030 LA    R2,RRGSTAH          STATION                                      
         MVI   NCOMBOS,0                                                        
         XC    COMBOSTA,COMBOSTA                                                
         CLI   5(R2),0                                                          
         BE    VKEY0040                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALISTA                                                          
         BAS   RE,SETFLD                                                        
         CLI   ACTNUM,ACTDIS       TEST ACTION = DISPLAY                        
         BE    VKEY0035                                                         
         CLI   ACTNUM,ACTLIST      TEST ACTION = LIST                           
         BNE   VKEY0040                                                         
VKEY0035 DS   0H                                                                
         CLC   QSTA+3(2),=C'-C'    AND '-C' STATION                             
         BE    *+14                                                             
         CLC   QSTA+4(2),=C'-C'                                                 
         BNE   VKEY0040                                                         
         GOTO1 =A(GETCOMBO),RR=RELO    YES-GET THE COMBINED STATIONS            
*                                                                               
VKEY0040 LA    R2,RRGSTYH          STATION TYPE                                 
         MVI   QSTATY,0                                                         
         CLI   5(R2),0                                                          
         BE    VKEY0050                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALISTYP                                                         
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0050 LA    R2,RRGTVBH          TVB REGION                                   
         CLI   5(R2),0                                                          
         BE    VKEY0060                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALITVB                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0060 LA    R2,RRGOWNH          OWNERSHIP                                    
         CLI   5(R2),0                                                          
         BE    VKEY0070                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALIOWN                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0070 LA    R2,RRGTEMH          TEAM                                         
         CLI   5(R2),0                                                          
         BE    VKEY0080                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALITEM                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0080 LA    R2,RRGCATH          CATEGORY                                     
         CLI   5(R2),0                                                          
         BE    VKEY0100                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALICTGY                                                         
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0100 LA    R2,RRGCTYH          CONTRACT TYPE                                
         CLI   5(R2),0                                                          
         BE    VKEY0110                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALICONT                                                         
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0110 LA    R2,RRGRNKH          MARKET RANK                                  
         CLI   5(R2),0                                                          
         BE    VKEY0120                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALIRANK                                                         
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0120 LA    R2,RRGMKTH          MARKET                                       
         CLI   5(R2),0                                                          
         BE    VKEY0130                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALIMKT                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0130 LA    R2,RRGCLSH          CLASS                                        
         CLI   5(R2),0                                                          
         BE    VKEY0140                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALICLS                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0140 LA    R2,RRGAFFH          AFFILIATE                                    
         CLI   5(R2),0                                                          
         BE    VKEY0142                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALIAFF                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0142 LA    R2,RRGADVH          ADVERTISER                                   
         CLI   5(R2),0                                                          
         BE    VKEY0144                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALIADV                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0144 LA    R2,RRGAGYH          AGENCY                                       
         CLI   5(R2),0                                                          
         BE    VKEY0146                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALIAGY                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0146 LA    R2,RRGDCTH          DEVELOPMENTAL CONTRACT TYPE                  
         CLI   5(R2),0                                                          
         BE    VKEY0150                                                         
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         GOTO1 VALIDCT                                                          
         BAS   RE,SETFLD                                                        
*                                                                               
VKEY0150 LA    R2,RRGOPTH          OPTIONS                                      
         GOTO1 =A(VOPT),RR=RELO    YES-GET THE COMBINED STATIONS                
         SPACE                                                                  
         MVI   LIMACC,0            ASSUME NO LIMITED ACCESS                     
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE                                   
         BE    VKEY0270                                                         
         CLI   TWAOFFC,C'*'        TEST IF DDS TERMINAL                         
         BE    VKEY0270                                                         
         SPACE                                                                  
         CLC   TWAACCS(2),=C'O='    TEST FOR OFFICE RESTRICTION                 
         BNE   VKEY0240                                                         
         TM    TWAAUTH,X'80'       TEST IF TERMINAL ALLOWED ACCESS              
         BO    VKEY0270            TO ALL OFFICES                               
         CLC   QOFF,TWAACCS+2      OTHERWISE, COMPARE OFFICES                   
         BNE   VKOFFLOC                                                         
         OI    LIMACC,LIMACOFF                                                  
         B     VKEY0270                                                         
*                                                                               
VKEY0240 CLI   TWAACCS,C'$'        TEST FOR STATION LIMITED ACCESS              
         BNE   VKEY0270                                                         
         OC    QSTA,QSTA                                                        
         BZ    VKSTAERR                                                         
         MVC   KEY,SVSTAKEY                                                     
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RSTAD,R4                                                         
         LA    R5,RSTAREC                                                       
         SR    R7,R7                                                            
         ICM   R7,3,RSTALEN                                                     
         AR    R7,R5                                                            
         BCTR  R7,0         R7 = A(LAST BYTE OF STATION REC), FOR BXLE          
         LA    R5,RSTAELEM                                                      
         SR    R6,R6                                                            
*                                                                               
VKEY0250 CLI   0(R5),6      SEARCH FOR ELEMENT FOR VALID SIGNON ID'S            
         BNE   VKEY0260                                                         
         USING RSTASOEL,R5                                                      
         CLC   TWAORIG,RSTASID    COMPARE USER ID TO STATION SIGN ON ID         
         BNE   VKEY0260                                                         
         OI    LIMACC,LIMACSTA    OK - BUT TURN ON LIMITED ACCES SWITCH         
         B     VKEY0270                                                         
*                                                                               
VKEY0260 CLI   0(R5),0            END OF RECORD                                 
         BE    VKSTAERR            NOT OK                                       
         IC    R6,1(R5)                                                         
         BXLE  R5,R6,VKEY0250                                                   
         B     VKSTAERR           NOT OK                                        
*                                                                               
         DROP  R4,R5                                                            
*                                                                               
VKEY0270 CLI   QLIST,QLOFF         TEST LIST=OFFICE                             
         BNE   VKEY0274                                                         
         TM    LIMACC,LIMACOFF     YES-TEST LIMITED OFFICE ACCESS               
         BO    VKOLAERR                                                         
         B     VKEY0280                                                         
*                                                                               
VKEY0274 CLI   QLIST,QLSTA         TEST LIST=STATION (FOR COMBO ALSO)           
         BNE   VKEY0280                                                         
         TM    LIMACC,LIMACSTA     YES-TEST LIMITED STATION ACCESS              
         BO    VKSLAERR                                                         
*                                                                               
         CLI   QSTA,C'*'           SET REQUESTED                                
         BE    SETLSTER             NO SET FOR LIST                             
*                                                                               
         TM    QLOPT,QLCOMBO       AND LIST=COMBO                               
         BZ    VKEY0280                                                         
         OC    QSTA,QSTA                                                        
         BZ    MISCOMER                                                         
         CLC   QSTA+3(2),=C'-C'    AND '-C' STATION                             
         BE    VKEY0280                                                         
         CLC   QSTA+4(2),=C'-C'                                                 
         BNE   MISCOMER                                                         
*                                                                               
VKEY0280 MVI   ROUNDSW,C'Y'        ASSUME DOLLAR ROUNDING                       
         CLI   QLIST,QLMON         LIST = MONTHS?                               
         BE    VKEY0290            YES - ROUND                                  
         CLI   QLIST,QLGRP         LIST = G/S                                   
         BE    VKEY0290             YES - ROUND                                 
         SPACE                                                                  
         CLI   ACTNUM,ACTDIS       ACTION = DISPLAY?                            
         BE    *+8                 YES - ROUND                                  
         MVI   ROUNDSW,C'N'        NO  - LIST - DON'T ROUND                     
*                                                                               
* DO SECURITY CHECK HERE PROGRAM CALLED IS SOURCE REPFACS                       
*                                                                               
VKEY0290 DS   0H                                                                
         GOTO1 =A(CKSEC),RR=RELO                                                
         SPACE                                                                  
         CLI   BYTE,1              WAS ANY NEW DATA PASSED BACK                 
         BNE   VKEY0294             NO                                          
         SPACE                                                                  
         BAS   RE,RSETRRGO         BACK TO RRGON FILE VALUES                    
         B     VKEY0005             RETRY VALIDATION                            
         SPACE                                                                  
VKEY0294 DS   0H                                                                
         XC    ELEM,ELEM           BUILD 1ST PART OF RRGON KEY IN ELEM          
         LA    R4,ELEM                                                          
         LA    R5,ELEM+8                                                        
         OC    QGROUP,QGROUP                                                    
         BZ    VKEY0310                                                         
         CLI   QGROUP,C'*'         SETS                                         
         BE    VKEY0300             YES                                         
         CLI   QGROUP+1,0                                                       
         BNE   VKEY0300                                                         
         MVI   0(R4),QLGRGRP       GRGRP (1 CHARACTER)                          
         MVC   0(1,R5),QGROUP                                                   
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
         B     VKEY0310                                                         
*                                                                               
VKEY0300 MVI   0(R4),QLGRP         GROUP (2 CHARACTERS)                         
         MVC   0(2,R5),QGROUP                                                   
         SPACE                                                                  
         CLI   QGROUP,C'*'         SET                                          
         BNE   VKEY0304                                                         
         GOTO1 =A(SETK),RR=RELO    GO SET KEY FROM TABLE                        
         SPACE                                                                  
VKEY0304 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0310 OC    QSTA,QSTA                                                        
         BZ    VKEY0320                                                         
         MVI   ROUNDSW,C'N'                                                     
         MVI   0(R4),QLSTA         STATION                                      
         MVC   0(7,R5),QSTA                                                     
         SPACE                                                                  
         CLI   QSTA,C'*'                                                        
         BNE   VKEY0312                                                         
         GOTO1 =A(SETK),RR=RELO    GO SET KEY FROM TABLE                        
         SPACE                                                                  
VKEY0312 CLI   NCOMBOS,0           LIST=COMBO                                   
         BE    VKEY0314             NO                                          
*                                                                               
         MVC   0(7,R5),COMBOSTA    USE FIRST COMBO STATION                      
         XC    QSTA,QSTA                                                        
*                                                                               
VKEY0314 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0320 OC    QREGION,QREGION                                                  
         BZ    VKEY0330                                                         
         MVI   0(R4),3             REGION                                       
         MVC   0(2,R5),QREGION                                                  
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0330 OC    QOFF,QOFF                                                        
         BZ    VKEY0340                                                         
         MVI   ROUNDSW,C'N'                                                     
         SPACE                                                                  
         MVI   0(R4),4             OFFICE                                       
         MVC   0(2,R5),QOFF                                                     
         SPACE                                                                  
         CLI   QOFF,C'*'                                                        
         BNE   VKEY0334                                                         
         GOTO1 =A(SETK),RR=RELO    GO SET KEY FROM TABLE                        
         SPACE                                                                  
VKEY0334 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0340 OC    QTEAM,QTEAM                                                      
         BZ    VKEY0344                                                         
         MVI   0(R4),QLTEM         TEAM                                         
         MVC   0(2,R5),QTEAM                                                    
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0344 OC    QADV,QADV                                                        
         BZ    VKEY0346                                                         
         MVI   0(R4),QLADV         ADVERTISER                                   
         MVC   0(L'QADV,R5),QADV                                                
         SPACE                                                                  
         CLI   QADV,C'*'                                                        
         BNE   VKEY0345                                                         
         GOTO1 =A(SETK),RR=RELO    GO SET KEY FROM TABLE                        
         SPACE                                                                  
VKEY0345 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0346 OC    QAGY,QAGY                                                        
         BZ    VKEY0350                                                         
         MVI   0(R4),QLAGENCY      CORE AGENCY                                  
         MVC   0(L'QAGY-2,R5),QAGY                                              
         SPACE                                                                  
         CLI   QAGY,C'*'           IF SET, BY AGENCY                            
         BE    *+12                                                             
         CLI   QAGY+4,C' '         IS THIS CORE AGENCY?                         
         BNH   VKEY0348                                                         
         SPACE                                                                  
         MVI   0(R4),QLAGY         AGENCY/OFFICE                                
         MVC   0(L'QAGY,R5),QAGY                                                
         SPACE                                                                  
VKEY0348 CLI   QAGY,C'*'                                                        
         BNE   VKEY0349                                                         
         GOTO1 =A(SETK),RR=RELO    GO SET KEY FROM TABLE                        
         SPACE                                                                  
VKEY0349 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0350 OC    QAFF,QAFF                                                        
         BZ    VKEY0360                                                         
         SPACE                                                                  
         CLI   QLIST,QLSTA         IF LISTING STATIONS, ONLY A FILTER           
         BE    VKEY0360                                                         
         SPACE                                                                  
         MVI   0(R4),QLAFF         AFFILIATE                                    
         MVC   0(3,R5),QAFF                                                     
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0360 OC    QCLASS,QCLASS                                                    
         BZ    VKEY0370                                                         
         MVI   0(R4),QLCLS         CLASS                                        
         MVC   0(2,R5),QCLASS                                                   
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0370 OC    QCTGY,QCTGY                                                      
         BZ    VKEY0380                                                         
         MVI   0(R4),QLCAT         CATEGORY                                     
         MVC   0(2,R5),QCTGY                                                    
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0380 CLI   QSTATY,0                                                         
         BE    VKEY0390                                                         
         MVI   0(R4),QLSTY         STATION TYPE                                 
         MVC   0(1,R5),QSTATY                                                   
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0390 OC    QTVB,QTVB                                                        
         BZ    VKEY0400                                                         
         SPACE                                                                  
         CLI   QLIST,QLSTA         IF LISTING STATIONS, ONLY A FILTER           
         BE    VKEY0400                                                         
         SPACE                                                                  
         MVI   0(R4),QLTVB         TVB                                          
         MVC   0(2,R5),QTVB                                                     
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0400 OC    QOWNER,QOWNER                                                    
         BZ    VKEY0410                                                         
         SPACE                                                                  
         CLI   QLIST,QLSTA         IF LISTING STATIONS, ONLY A FILTER           
         BE    VKEY0410                                                         
         SPACE                                                                  
         CLI   QLIST,QLMKT         IF LISTING MARKETS, AND HAVE OWNER           
         BNE   VKEY0404             NO                                          
         SPACE                                                                  
         CLI   CKSECOWN,1          IS THIS SPECIAL OWNER FILTER                 
         BE    VKEY0410             YES, DO NOT PUT IN KEY                      
         SPACE                                                                  
VKEY0404 DS   0H                                                                
         MVI   0(R4),QLOWN         OWNER                                        
         MVC   0(3,R5),QOWNER                                                   
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0410 OC    QCONTY,QCONTY                                                    
         BZ    VKEY0420                                                         
         MVI   0(R4),QLCON         CONTRACT TYPE                                
         MVC   0(L'QCONTY,R5),QCONTY                                            
         SPACE                                                                  
         CLI   QCONTY,C'*'                                                      
         BNE   VKEY0414                                                         
         GOTO1 =A(SETK),RR=RELO    GO SET KEY FROM TABLE                        
         CLI   RRGCTY+1,C'&&'      THIS SPECIAL HARD CODE CONTRACT TYPE         
         BNE   VKEY0414                                                         
         SPACE                                                                  
         CLI   QLIST,QLMON         ONLY FOR MON                                 
         BNE   CONDCTER                                                         
         SPACE                                                                  
         OI    DISPFLAG,OPCONDCT   SET ON HARDCODE QLCON/QLDCT                  
         SPACE                                                                  
VKEY0414 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
         SPACE                                                                  
VKEY0420 CLI   QRANK,0                                                          
         BE    VKEY0430                                                         
         SPACE                                                                  
         CLI   QLIST,QLSTA         IF LISTING STATIONS, ONLY A FILTER           
         BE    VKEY0430                                                         
         SPACE                                                                  
         MVI   0(R4),QLRNK         MARKET RANK                                  
         MVC   0(1,R5),QRANK                                                    
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0430 OC    QMKT,QMKT                                                        
         BE    VKEY0434                                                         
         SPACE                                                                  
*        CLI   QLIST,QLSTA         IF LISTING STATIONS, ONLY A FILTER           
*        BE    VKEY0434                                                         
         SPACE                                                                  
         MVI   0(R4),QLMKT         MARKET                                       
         MVC   0(4,R5),QMKT                                                     
         SPACE                                                                  
         CLI   QMKT,C'*'                                                        
         BNE   VKEY0432                                                         
         GOTO1 =A(SETK),RR=RELO    GO SET KEY FROM TABLE                        
         SPACE                                                                  
VKEY0432 DS    0H                                                               
         LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0434 OC    QDCT,QDCT                                                        
         BE    VKEY0438                                                         
         TM    DISPFLAG,OPCONDCT   HARDCODE QLCON/QLDCT                         
         BO    CONENTER             YES, CAN'T ENTER DCT                        
         SPACE                                                                  
         MVI   0(R4),QLDCT         DEVELOPMENTAL CONTRACT TYPE                  
         MVC   0(L'QDCT,R5),QDCT                                                
         SPACE                                                                  
         CLI   QDCT,C'*'                                                        
         BNE   VKEY0436                                                         
         GOTO1 =A(SETK),RR=RELO    GO SET KEY FROM TABLE                        
         SPACE                                                                  
VKEY0436 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
         SPACE                                                                  
VKEY0438 OC    QSAL,QSAL                                                        
         BE    VKEY0440                                                         
         SPACE                                                                  
         MVI   0(R4),QLSAL         SALESPERSON                                  
         MVC   0(L'QSAL,R5),QSAL                                                
         SPACE                                                                  
         CLI   QSAL,C'*'                                                        
         BNE   VKEY0439                                                         
         GOTO1 =A(SETK),RR=RELO    GO SET KEY FROM TABLE                        
         SPACE                                                                  
VKEY0439 LA    R4,1(R4)                                                         
         LA    R5,8(R5)                                                         
*                                                                               
VKEY0440 MVC   DUB,ELEM                SAVE POSSIBLE SETS                       
         MVC   SETKEY,ELEM                                                      
         NC    ELEM(5),=X'7F7F7F7F7F'  SET OFF POSSIBLE SETS                    
         SPACE                                                                  
         CLI   ELEM,QLGRGRP                  TEMP FIX                           
         BE    *+12                                                             
         CLI   ELEM,QLGRP                    TEMP FIX                           
         BNE   VKEY0450                                                         
*                                                                               
*   L=OFF W/OFFICE FILTER OR SET - TREAT AS L=OFF REQUEST                       
*                                                                               
         CLI   QLIST,QLOFF         L=OFF REQUEST?                               
         BNE   VKEYB440            NO                                           
         CLC   =X'070400',ELEM     YES - GRP-SUBGRP/OFFICE ENTERED?             
         BE    VKEYA440            YES                                          
         CLC   =X'190400',ELEM     NO  - GROUP/OFFICE ENTERED?                  
         BNE   VKEYB440            NO                                           
VKEYA440 EQU   *                                                                
         MVI   ELEM+1,0            YES - CLEAR OFFICE FROM KEY                  
VKEYB440 EQU   *                                                                
*                                                                               
         CLC   ELEM+1(2),=AL1(QLREG,QLOFF)   TEMP FIX                           
         BNE   TEMPFIX                                                          
         MVC   ELEM+1(2),=AL1(QLOFF,QLREG)                                      
         MVC   DUB,ELEM+11                                                      
         MVC   ELEM+11(8),ELEM+19                                               
         MVC   ELEM+19(8),DUB                                                   
TEMPFIX  EQU   *                                                                
         CLI   ELEM,QLGRGRP        TEST GRGRP IN KEY                            
         BE    VKEY0442                                                         
         SPACE                                                                  
         CLC   ELEM(3),=AL1(QLGRP,QLSTA,0) THIS HAS TO BE CHANGED               
         BNE   VKEY0442                                                         
         SPACE                                                                  
         CLC   AGENCY,=C'K3'       THIS KRGNY MASTER REP                        
         BE    KATZ20                                                           
         CLC   AGENCY,=C'BL'       THIS BLAIR MASTER REP                        
         BE    KATZ20                                                           
         CLC   AGENCY,=C'IR'       THIS IRNY MASTER REP                         
         BE    KATZ20                                                           
         SPACE                                                                  
         LA    RE,K3GRPCT                                                       
         L     RF,=A(K3GRPTBL)                                                  
         A     RF,RELO                                                          
         SPACE                                                                  
KATZ10   CLC   AGENCY,2(RF)                                                     
         BE    KATZ20                                                           
         LA    RF,4(,RF)                                                        
         BCT   RE,KATZ10                                                        
         B     VKEY0442                                                         
KATZ20   DS    0H                                                               
         SPACE                                                                  
VKEY0441 DS   0H                                                                
         MVC   ELEM(2),=AL1(QLSTA,QLGRP) CHANGE SEQUENCE                        
         MVC   DUB,ELEM+8                                                       
         MVC   ELEM+8(8),ELEM+16   PUT STATION HIGH                             
         MVC   ELEM+16(8),DUB      THEN SAVED GRP/SUB                           
         B     VKEY0450                                                         
*                                                                               
VKEY0442 CLI   ELEM+1,0            AND OTHERS ALSO IN KEY                       
         BE    VKEY0450             NO, LEAVE IT ALONE                          
*                                                                               
         CLI   QLIST,QLOWN         LISTING ON OWNER                             
         BNE   VKEY0446                                                         
         CLC   =AL1(QLMKT,00,00)(3),ELEM+1     FILTER ON MKT                    
         BE    VKEY0449                                                         
         CLC   =AL1(QLOFF,QLMKT,00)(3),ELEM+1            OFF/MKT                
         BE    VKEY0449                                                         
*                                                                               
VKEY0446 DS    0H                                                               
         CLI   QLIST,QLOFF         LISTING ON OFFICE                            
         BNE   VKEY0448                                                         
         CLC   =AL1(QLOWN,QLMKT,00)(3),ELEM+1  FILTER ON OFF/MKT                
         BE    VKEY0449                                                         
*                                                                               
VKEY0448 DS    0H                                                               
         CLI   QLIST,QLSTA         IF LISTING BY STA                            
         BE    VKEY0449                                                         
*                                                                               
         CLI   ELEM+1,QLSTA        OR STATION IS A FILTER                       
         BNE   VKEY0450             NO                                          
*                                                                               
         CLI   QGROUP,C'*'         IF GROUP A SET, THIS IS NOT VALID            
         BE    VKFTRERR                                                         
*                                                                               
VKEY0449 MVC   ELEM(7),ELEM+1                                                   
         MVI   ELEM+7,0                                                         
         MVC   SETKEY,DUB+1                                                     
         BCTR  R4,0                                                             
         MVC   ELEM+8(24),ELEM+16                                               
         XC    ELEM+40(8),ELEM+40                                               
*                                                                               
VKEY0450 LA    RE,ELEM+L'ROKDTLTY                                               
         CR    R4,RE               CHECK NO MORE THAN MAX ROWS                  
         BH    VKKEYERR                                                         
         SPACE                                                                  
         CLI   ELEM,0              CHECK FOR NO ROWS                            
         BNE   VKEY0460                                                         
         SPACE                                                                  
         CLI   QLIST,QLGRP         GRP DOES NOT REQUIRE ANY FIELDS              
         BNE   VKEY0454                                                         
         MVI   ELEM,QLGRP                                                       
         B     VKEY0460                                                         
         SPACE                                                                  
VKEY0454 DS    0H                                                               
         CLI   QLIST,QLSTA         GRP DOES NOT REQUIRE ANY FIELDS              
         BNE   VKEY0456                                                         
         MVI   ELEM,QLSTA                                                       
         B     VKEY0460                                                         
         SPACE                                                                  
VKEY0456 CLI   QLIST,QLOWN         OWNER DOES NOT REQUIRE ANY FIELDS            
         BNE   VKMISERR                                                         
         SPACE                                                                  
         MVI   ELEM,QLOWN                                                       
         SPACE                                                                  
VKEY0460 MVC   SVKEY(L'ROKDTLTY),ELEM   SAVE THE KEY                            
         MVC   SVKEY+L'ROKDTLTY(24),ELEM+8                                      
*                                                                               
         BAS   RE,RSETRRGO         BACK TO RRGON FILE VALUES                    
*                                                                               
         XC    KEY,KEY             READ RRGON RECORD TYPE HEADER                
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         MVC   ROKREP,AGENCY                                                    
         MVC   ROKHD2RQ,TYPEKEY    ALL/CONFIRMED/DIRECT/UNCONFIRMED             
         MVI   ROKHD2+1,X'02'                                                   
         MVC   ROKHD2TY,SVKEY                                                   
         CLI   ACTNUM,ACTLIST      ACTION = LIST?                               
         BNE   VKEY0500            NO                                           
         CLI   QLIST,QLMON         LIST = MONTHS?                               
         BE    VKEY0500            YES - TREAT AS DISPLAY                       
         SPACE                                                                  
         L     R1,=A(LCOMBOS)      YES - FIND RECORD TYPE FOR LIST              
         A     R1,RELO                                                          
         MVI   BYTE,0                                                           
         USING LCOMBOSD,R1                                                      
VKEY0470 CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   VKEY0472             NOT YET                                     
*                                                                               
         CLI   BYTE,0              TYPE NOT FOUND-TEST LIST OPTION REQ          
         BE    VKKEYERR            NO                                           
         CLI   QLIST,0                                                          
         BNE   VKFTRERR            YES-INVALID FILTER COMBINATIONS              
         DC    H'0'                                                             
*                                                                               
VKEY0472 DS   0H                                                                
         CLC   ROKHD2TY,LFTRFLDS   ENTRY EQUAL TO FILTERS ENTERED               
         BNE   VKEY0474             NO                                          
         MVI   BYTE,1                                                           
         CLI   QLIST,0             FOUND PREVIOUS ENTRY                         
         BNE   VKEY0476             YES                                         
         TM    LFLAG,X'80'         USE ALTERNATE KEY STRUCTURE                  
         BO    VKEY0478             YES, USE ALTERNATE                          
*                                                                               
VKEY0474 LA    R1,LCNEXT                                                        
         B     VKEY0470                                                         
         SPACE                                                                  
VKEY0476 CLC   LSTTYP,QLIST        SAME LIST TYPE                               
         BNE   VKEY0474             NO                                          
         SPACE                                                                  
VKEY0478 MVC   ROKHD2TY,LALTKEY    MOVE IN KEY STRUCTURE TO USE                 
         DROP  R1                                                               
         SPACE                                                                  
* NOW CHECK NO SETS OR STATION COMBOS PRECEDE LIST TYPE *                       
         SPACE                                                                  
         LA    R0,3                                                             
         LA    R1,ROKHD2TY                                                      
VKEY0480 CLI   0(R1),0             END OF TYPES                                 
         BE    VKEY0500                                                         
         CLC   QLIST,0(R1)         TEST LISTING ON THIS TYPE                    
         BE    VKEY0500             YES, DONE                                   
         SPACE                                                                  
         L     RF,=A(FILTAB)       CHECK DATA                                   
         A     RF,RELO                                                          
         SPACE                                                                  
VKEY0484 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R1),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,L'FILTAB(RF)                                                  
         B     VKEY0484                                                         
         SPACE                                                                  
         LH    RE,2(RF)                                                         
         LA    RE,SYSD(RE)                                                      
         CLI   0(RE),C'*'          THIS A SET                                   
         BE    VKFTRERR            YES, CAN'T PRECEDE LIST TYPE                 
         CLI   0(R1),QLSTA         THIS STATION FILTER                          
         BNE   *+12                                                             
         CLI   NCOMBOS,0                                                        
         BNE   VKFTRERR            YES, CAN'T PRECEDE LIST TYPE                 
         LA    R1,1(,R1)                                                        
         BCT   R0,VKEY0480                                                      
         SPACE                                                                  
VKEY0500 DS   0H                                                                
         L     R1,AIO                                                           
         XC    0(L'ROREC,R1),0(R1) CLEAR I/O AREA                               
         GOTO1 HIGH                                                             
         MVI   TRCTYPE,01                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   ROKEY(ROKHD2ST-ROKEY),KEYSAVE                                    
         BNE   VKKEYER                                                          
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        IF OFFLINE, TESTING, SKIP WRITE              
         BE    VKEY0501                                                         
         SPACE                                                                  
         L     R3,AIO                                                           
         L     RF,48(R3)                                                        
         LA    RF,1(,RF)                                                        
         ST    RF,48(R3)                                                        
*        GOTO1 WRITE                                                            
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'RRGNEW',ROKEY,(R3)                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
VKEY0501 DS   0H                                                                
         TM    DISPFLAG,OPCONDCT   LOOKING FOR HARD CODE CON/DCT                
         BZ    VKEY0502             NO                                          
         SPACE                                                                  
         CLC   =X'022100',ROKHD2TY  ONLY STATION/CONTYPE                        
         BE    VKEY0502                                                         
         CLC   =X'072100',ROKHD2TY  OR GRP/CONTYPE                              
         BE    VKEY0502                                                         
         CLC   =X'192100',ROKHD2TY  OR GRGRP/CONTYPE                            
         BE    VKEY0502              OKAY NOW                                   
         SPACE                                                                  
         L     R2,AFSTFLD                                                       
         L     R1,=A(VKFTRERM-1)                                                
         NI    GENSTAT2,X'FF'-USMYOK                                            
         B     USRERRMS                                                         
*                                                                               
VKEY0502 MVI   YRSW,0                                                           
*                                  CHECK RRGON HAS REQUESTED PERIOD             
**       CLC   QSTART,ROKHD2ST                                                  
**       BL    VKPERERR                                                         
**       CLC   QEND,ROKHD2EN                                                    
**       BH    VKPERERR                                                         
         CLC   QSTART,ROKHD2EN     START VS FILE END                            
         BH    VKPERERR            STARTS AFTER FILE ENDS = ERROR               
*                                  STARTS BEFORE FILE ENDS                      
         CLC   QEND,ROKHD2ST       END VS FILE START                            
         BNL   VKEY0506                                                         
*                                                                               
         MVC   DUB(2),QSTART                                                    
         MVI   DUB+2,15                                                         
         GOTO1 DATCON,DMCB,(3,DUB),(0,WORK)                                     
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'365'                                    
         GOTO1 DATCON,(R1),(0,WORK+6),(3,FULL)                                  
*                                  WITHIN FILE PERIOD                           
         CLC   FULL(2),ROKHD2EN     START VS FILE END                           
         BH    VKPERERR            STARTS AFTER FILE ENDS = ERROR               
         MVC   FULL+1(1),QEND+1                                                 
         CLC   FULL(2),ROKHD2ST     END VS FILE START                           
         BL    VKPERERR            ENDS BEFORE FILE STARTS = ERROR              
         MVC   QSTART(1),FULL                                                   
         MVC   QEND(1),FULL                                                     
         MVI   YRSW,C'P'           SET TO PRIOR YEAR                            
*                                  WITHIN FILE PERIOD                           
VKEY0506 DS   0H                                                                
         CLI   QLIST,QLSTY         LIST BY STATYPE                              
         BE    *+12                                                             
         CLI   QSTATY,0            FILTERING ON STATION TYPE                    
         BE    VKEY0507             NO, OKAY                                    
         SPACE                                                                  
         CLI   YRSW,C'P'           NOT VALID FOR PRIOR YEAR                     
         BE    STYPERR                                                          
         SPACE                                                                  
VKEY0507 DS   0H                                                                
         CLI   QLIST,QLMON         LIST FOR MONTHS?                             
         BNE   VKEY0508             NO                                          
         CLI   QSTATY,0            ANY FILTER ON STATION TYPE                   
         BE    VKEY0508             NO                                          
         CLI   YRSW,C'P'           NOT VALID FOR PRIOR YEAR                     
         BE    STYPERR                                                          
         SPACE                                                                  
VKEY0508 DS   0H                                                                
         CLI   QLIST,QLMON         LIST FOR MONTHS?                             
         BE    VKEY0510            YES - TREAT AS DISPLAY                       
         CLI   ACTNUM,ACTLIST      NO  - ACTION = LIST?                         
         BE    VKEY0560             YES                                         
*                                                                               
VKEY0510 DS   0H                                                                
         OI    DMINBTS,X'08'       PASS DELETED RRGON RECS                      
         NI    DMOUTBTS,X'FD'      IGNORE DELETED RRGON RECS                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   ROKREP,AGENCY                                                    
         MVC   ROKDTLRQ,TYPEKEY    ALL/CONFIRMED/DIRECT/UNCONFIRMED             
         MVC   ROKDTLTY(ROKDTLYM-ROKDTLTY),SVKEY                                
         CLI   NCOMBOS,0           TEST COMBINED STATIONS                       
         BE    VKEY0530                                                         
         LA    RE,ROKDTLTY         YES-FIND DISPLACEMENT INTO KEY OF            
         LA    RF,ROKDTLVL             STATION VALUE                            
         LA    R0,L'ROKDTLTY                                                    
*                                                                               
VKEY0520 CLI   0(RE),2                                                          
         BE    VKEY0524                                                         
         LA    RE,1(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,VKEY0520                                                      
         DC    H'0'                                                             
VKEY0524 SR    RF,R7                                                            
         ST    RF,STADISP                                                       
         LA    R4,COMBOSTA         BRANCH THROUGH STATIONS UNTIL AN             
         ZIC   R5,NCOMBOS          RRGON RECORD IS FOUND                        
*                                                                               
VKEY0530 DS    0H                                                               
         MVC   KEY+ROKDTLYM-ROKEY(2),QSTART                                     
         GOTO1 HIGH                READ FIRST RRGON DETAIL RECORD               
         MVI   TRCTYPE,02                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         SPACE                                                                  
         CLC   ROKDTLTY,SVKEY      SEE IF RIGHT DATA TYPES                      
         BNE   VKRECERR                                                         
         SPACE                                                                  
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE                                      
         BE    VKEY0560                                                         
*                                                                               
         CLI   NCOMBOS,0           NOT FOUND - TEST COMBINED STATIONS           
         BE    VKEY0531             NO, SEE IF SETS                             
         CLI   SET1CDE,0           USING SETS AS FILTERS                        
         BNE   VKEY0531             YES, CHECK OUT AS SETS                      
         SPACE                                                                  
         LA    R4,7(R4)            YES-TRY NEXT COMBINED STATION                
         BCT   R5,*+8                                                           
         B     VKRECERR                                                         
         XC    KEY,KEY                                                          
         MVC   ROKREP,AGENCY                                                    
         MVC   ROKDTLTY(ROKDTLYM-ROKDTLTY),SVKEY                                
         L     R1,STADISP                                                       
         LA    R1,KEY(R1)                                                       
         MVC   0(7,R1),0(R4)                                                    
         B     VKEY0530                                                         
         SPACE                                                                  
VKEY0531 DS   0H                                                                
         CLI   SET1CDE,0           USING SETS AS FILTERS                        
         BE    VKRECERR             NO-RECORD NOT FOUND                         
         SPACE                                                                  
         GOTO1 =A(CSETM),RR=RELO                                                
         BNE   VKRECERR             NO-RECORD NOT FOUND                         
         SPACE                                                                  
         LA    R3,3                                                             
         LA    R2,KEY+ROKDTLTY-ROKEY                                            
         LA    R6,KEY+ROKDTLVL-ROKEY                                            
VKEY0532 LA    R0,3                                                             
         LA    R1,SET1CDE                                                       
VKEY0533 CLC   0(1,R2),0(R1)                                                    
         BE    VKEY0554                                                         
         LA    R1,L'SETCDE(,R1)                                                 
         BCT   R0,VKEY0533                                                      
         SPACE                                                                  
         CLC   LSTTYPE,0(R2)       TEST LISTING ON THIS TYPE                    
         BE    VKEY0550                                                         
         L     RF,=A(FILTAB)       CHECK NON-SET DATA                           
         A     RF,RELO                                                          
*                                                                               
VKEY0534 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(RF)                                                    
         BE    *+12                                                             
         LA    RF,L'FILTAB(RF)                                                  
         B     VKEY0534                                                         
         SPACE                                                                  
         LH    R1,2(RF)                                                         
         LA    R1,SYSD(R1)                                                      
         ZIC   RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,VKEYCLCB         TEST FILTER IS SET (COMP TO SPACES)          
         BH    VKEY0547                                                         
         SPACE                                                                  
         CLI   0(R2),QLSTA                                                      
         BNE   VKEY0546                                                         
         CLI   NCOMBOS,0                                                        
         BE    VKEY0546                                                         
         LA    R4,COMBOSTA         BRANCH THROUGH STATIONS UNTIL AN             
         ZIC   R5,NCOMBOS          RRGON RECORD IS FOUND                        
VKEY0540 CLC   0(7,R6),0(R4)                                                    
         BE    VKEY0550                                                         
         BL    VKEY0544                                                         
         LA    R4,7(,R4)                                                        
         BCT   R5,VKEY0540                                                      
         B     VKRECERR             NO-RECORD NOT FOUND                         
         SPACE                                                                  
VKEY0544 MVC   0(7,R6),0(R4)                                                    
         B     VKEY0530                                                         
         SPACE                                                                  
VKEY0546 DC    H'0'                                                             
         SPACE                                                                  
VKEY0547 EX    RE,VKEYCLCD         SEE IF EQUAL                                 
         BL    VKEY0548                                                         
         BE    VKEY0550                                                         
         EJECT                                                                  
* NEED TO FIND PREV SET AND BUMP IT                                             
         SPACE                                                                  
         GOTO1 =A(PSET),RR=RELO                                                 
         BNE   VKRECERR             NO-RECORD NOT FOUND                         
         B     VKEY0530                                                         
         SPACE                                                                  
VKEY0548 EX    RE,VKEYMVCD         SET TO FILTER                                
         B     VKEY0550                                                         
         SPACE                                                                  
VKEYCLCB CLC   0(0,R1),BLANKS                                                   
VKEYCLCD CLC   0(0,R6),0(R1)                                                    
VKEYMVCD MVC   0(0,R6),0(R1)                                                    
         SPACE                                                                  
VKEY0550 LA    R2,1(,R2)           NEXT KEY FLD                                 
         LA    R6,8(,R6)           NEXT KEY FLD                                 
         CLI   0(R2),0             ANOTHER CODE                                 
         BE    VKEY0560             NO                                          
         BCT   R3,VKEY0532                                                      
         SPACE                                                                  
         B     VKEY0560                                                         
         SPACE                                                                  
VKEY0554 DS   0H                                                                
         SPACE                                                                  
         LA    R0,KEY+5                                                         
         ST    R0,SETKEYAD                                                      
         SPACE                                                                  
         GOTO1 =A(FSET),RR=RELO    FILTER ON SET                                
         BNE   VKEY0530             GO TRY RDHI                                 
         LTR   R3,R3               ANY MORE FIELDS                              
         BZ    VKEY0560            NO                                           
         B     VKEY0550                                                         
*                                                                               
VKEY0560 DS   0H                                                                
         CLI   NCOMBOS,0           TEST COMBO STATION LIST                      
         BE    VKEY0561                                                         
         STC   R5,NCOMBOS          YES-SAVE N'ACTIVE STATIONS                   
         ST    R4,ACOMBOS                   AND A(FIRST ACTIVE ONE)             
*                                                                               
VKEY0561 DS   0H                                                                
         CLI   QLIST,QLMON         MONTH LIST OPTION?                           
         BNE   VKEY0562            NO                                           
*                                                                               
         MVC   LMONKEY,KEY         SAVE KEY FOR SUBROUTINE                      
*                                                                               
VKEY0562 DS   0H                                                                
         CLC   QSTART(1),QEND      TEST FOR PERIOD START/END YEARS SAME         
         BNE   VKEY0570                                                         
         MVC   FULL(2),QSTART      YES - SET UP WEEKS PER MONTH                 
         MVI   FULL+2,15                                                        
         CLI   YRSW,C'P'           THIS PRIOR YEAR                              
         BNE   VKEY0564                                                         
         ZIC   R0,FULL                                                          
         BCTR  R0,0                                                             
         STC   R0,FULL                                                          
         SPACE                                                                  
VKEY0564 GOTO1 DATCON,DMCB,(3,FULL),(0,WORK)                                    
         SPACE                                                                  
         MVC   FULL(2),WORK        SAVE CURRENT YEAR IN FULL                    
         LA    R4,CURWKS           WEEKS/MONTH CURRENT YEAR                     
         LA    R5,CURWKSY          YTD WEEKS CURRENT YEAR                       
         SR    R0,R0                                                            
         SR    R6,R6                                                            
         ZAP   HALF,=P'1'          START FROM JANUARY                           
*                                                                               
VKEY0566 OI    HALF+1,X'0F'                                                     
         UNPK  WORK+2(2),HALF                                                   
         GOTO1 GETBROAD,DMCB,(1,WORK),WORK+6,GETDAY,ADDAY                       
         IC    R0,0(R1)            WEEKS PER BROADCAST MONTH                    
         AR    R6,R0                                                            
         STC   R0,0(R4)                                                         
         STC   R6,0(R5)                                                         
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         AP    HALF,=P'1'          NEXT MONTH                                   
         CP    HALF,=P'12'         TEST DONE WHOLE YEAR                         
         BNH   VKEY0566                                                         
         CLC   WORK(2),FULL        YES -                                        
         BNE   VKEY0570                                                         
         GOTO1 ADDAY,(R1),(C'Y',WORK),WORK,F'-1'                                
         LA    R4,PRIWKS                                                        
         LA    R5,PRIWKSY                                                       
         SR    R6,R6                                                            
         ZAP   HALF,=P'1'                                                       
         B     VKEY0566                                                         
         SPACE                                                                  
VKEY0570 DS   0H                                                                
         MVI   ANYDATA,C'N'        SET ANY DATA FOUND TO NO                     
         SPACE                                                                  
* IF CONWHEN CHANGED, AND ACTION=LIST, AND MODE=PRINTREP, DO REPORT             
         SPACE                                                                  
         TM    CONWHENH+4,X'20'    SET ON VALIDATED BIT                         
         BO    VKEY0574                                                         
         SPACE                                                                  
         CLI   ACTNUM,ACTLIST      ACTION=LIST?                                 
         BNE   VKEY0574                                                         
         CLI   LSTTYPE,QLMON       LIST=MONTH REQUEST?                          
         BE    VKEY0574                                                         
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BNE   VKEY0574                                                         
         SPACE                                                                  
         OI    CONWHENH+4,X'20'    SET ON VALIDATED BIT                         
         B     LR                                                               
         SPACE                                                                  
VKEY0574 DS   0H                                                                
         CLI   ACTNUM,ACTLIST      ACTION=LIST?                                 
         BNE   EXIT                NO  - EXIT                                   
         CLI   LSTTYPE,QLMON       LIST=MONTH REQUEST?                          
         BNE   EXIT                NO                                           
         SPACE                                                                  
         L     RF,AFRSTREC         YES - CLEAR ALL SELECTS                      
         LA    RE,RGELAST          SEND END OF SCREEN                           
VKEY0580 DS   0H                                                                
         MVI   5(RF),0             CLEAR FIELD LENGTH VALUE                     
         OI    6(RF),X'80'         TURN ON TRANSMIT BIT                         
         MVC   8(3,RF),BLANKS      CLEAR SELECT FIELD                           
         LA    RF,RGESEL2H-RGESELH(RF)                                          
*                                  BUMP TO NEXT FIELD                           
         CR    RF,RE               END OF SCREEN REACHED?                       
         BL    VKEY0580            NO  - GO BACK AND CLEAR NEXT                 
         B     EXIT                                                             
*                                                                               
SETFLD   OC    AFSTFLD,AFSTFLD     SET A(FIRST VALID FIELD)                     
         BNZR  RE                                                               
         ST    R2,AFSTFLD                                                       
         BR    RE                                                               
         EJECT                                                                  
* BUILD MONTH TABLE                                                             
*                                                                               
         SPACE                                                                  
LR       DS    0H                                                               
         SPACE                                                                  
         MVI   NLISTS,13           MAX PER SCREEN                               
         SPACE                                                                  
         GOTO1 =A(SETSCR),RR=RELO                                               
         SPACE                                                                  
         CLI   QREFRESH,C'Y'       'REFRESH' REQUEST?                           
         BNE   LREC0008            NO                                           
         CLI   LISTEND,C'Y'        LIST END SET?                                
         BNE   LREC0008            NO                                           
*                                                                               
         LA    R2,RGESELH          CLEAR THE SCREEN                             
         BAS   RE,CLRSCRN                                                       
         MVI   SCRNCT,0                                                         
*                                                                               
         OC    TSARCT,TSARCT       THIS CONTINUTATION OF TOP N                  
         BNZ   LREC0012             YES                                         
*                                                                               
         SPACE                                                                  
         GOTO1 =A(SETFILTS),RR=RELO                                             
         B     EXIT                                                             
*                                                                               
*        ONLINE LIST OR MONTHLY TOTALS ROUTINE?                                 
*                                                                               
LREC0008 DS   0H                                                                
         OC    LMONKEY,LMONKEY     ANY LIST=MONTH KEY?                          
         BZ    LREC0010            NO  - PROCESS NORMALLY                       
*                                                                               
         GOTO1 =A(BLDMON),(RC),RR=RELO                                          
*                                  BUILD MONTH TABLE                            
*                                  WITHIN FILE PERIOD                           
         MVI   LSTTYPE,QLMON       YES - SET LIST TYPE TO MONTHS                
         GOTO1 =A(LISTMONS),RR=RELO                                             
         B     LREC0270            DO 'REFRESH' IF NEEDED                       
*                                                                               
LREC0010 DS   0H                                                                
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LREC0014             YES, START FROM SCRATCH                     
         SPACE                                                                  
         CLI   LISTEND,C'Y'        LIST END SET?                                
         BNE   LREC0014             NO                                          
         OC    TSARCT,TSARCT       THIS CONTINUTATION OF TOP N                  
         BNZ   LREC0012             YES                                         
         SPACE                                                                  
*        SEE IF STILL HAVE TOTALS LINE TO PRINT                                 
         SPACE                                                                  
         OC    MONTOTS+4(24),MONTOTS+4                                          
         BZ    LREC0014                                                         
         SPACE                                                                  
         MVC   LISTAR,BLANKS                                                    
         MVC   LISTAR(6),=C'TOTALS'                                             
         SPACE                                                                  
         CLI   FULLSTER,C'Y'       THIS FULL STEREO                             
         BNE   LREC001A             YES                                         
         MVI   LISTEND,C'Y'        YES-END OF LIST                              
         MVC   LISTAR(2),=C'@@'                                                 
         MVC   LISTAR+2(6),=C'TOTALS'                                           
LREC001A EQU   *                                                                
         SPACE                                                                  
         LA    R2,LISTAR+9                                                      
         MVC   TOTALS(24),MONTOTS+4                                             
         GOTO1 =A(DOLLPRNT),RR=RELO                                             
         XC    KEY,KEY                                                          
         XC    DMDSKADD,DMDSKADD                                                
         XC    MONTOTS+4(24),MONTOTS+4   SET OFF - TOTALS LINE PRINTED          
         SPACE                                                                  
         CLI   MODE,LISTRECS                                                    
         BNE   LREC0011                                                         
         SPACE                                                                  
         GOTO1 LISTMON                                                          
         B     EXIT                                                             
         SPACE                                                                  
LREC0011 DS   0H                                                                
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         MVC   P+18(6),LISTAR                                                   
         MVC   P+27(71),LISTAR+9                                                
         SPACE                                                                  
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         DROP  R3                                                               
         B     EXIT                                                             
         SPACE                                                                  
LREC0012 DS   0H                                                                
         LA    R2,TSARBLK                                                       
         USING TSARD,R2                                                         
         SPACE                                                                  
* TSAR CLEAR HERE                                                               
         SPACE                                                                  
*        XC    TSARBLK(TSARDL2),TSARBLK                                         
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,4                                                         
         MVI   TSRECL+1,52                                                      
*        OI    TSINDS,TSINODSK                                                  
         MVI   TSPAGL,3         LOW TEMPSTOR PAGE TO USE                        
         MVI   TSPAGN,1         NUMBER OF TEMPSTR PAGES TO BE USED              
         SPACE                                                                  
* TSAR CALLOV HERE                                                              
         SPACE                                                                  
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A5D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR                                                         
         SPACE                                                                  
         MVI   TSACTN,TSARES                                                    
         GOTO1 ATSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         BE    LREC0230                                                         
         DC    H'0'                                                             
         DROP  R2                                                               
LREC0014 OI    DMINBTS,X'08'       PASS DELETED RRGON RECS                      
         NI    DMOUTBTS,X'FD'      IGNORE DELETED RRGON RECS                    
         SPACE                                                                  
         LA    R2,RGESELH          CLEAR THE SCREEN                             
         BAS   RE,CLRSCRN                                                       
*        MVI   NLISTS,13           MAX PER SCREEN                               
         SPACE                                                                  
         CLI   MODE,PRINTREP       OFFLINE REPORT                               
         BE    LREC0020             YES, START FROM SCRATCH                     
         SPACE                                                                  
         OC    KEY(L'ROKEY),KEY    TEST KEY IS ZERO                             
         BZ    LREC0020            YES-FIRST TIME THROUGH                       
         SPACE                                                                  
         MVI   ANYDATA,C'Y'        SET DATA FOUND FOR REQUEST                   
         SPACE                                                                  
         LA    R7,KEY              SET UP DSECT                                 
         USING RORECD,R7                                                        
         MVI   ROKDTLYM,X'FF'      NEXT RECORD PAST THIS KEY COMBO              
         GOTO1 HIGH                                                             
         MVI   TRCTYPE,03                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         B     LREC0110                                                         
*                                                                               
LREC0020 DS   0H                                                                
         L     RF,ASPOOLD                                                       
         LM    R0,R1,=A(HDHK,HEADING)                                           
         A     R0,RELO                                                          
         A     R1,RELO                                                          
         ST    R0,HEADHOOK-SPOOLD(RF)                                           
         ST    R1,SPECS-SPOOLD(RF)                                              
         XC    TOTALS(24),TOTALS           CLEAR TOTALS                         
         XC    MONTOTS+4(24),MONTOTS+4     CLEAR TOTALS                         
         XC    TOTLCT,TOTLCT       CLEAR TOTAL LINES COUNT                      
         MVI   TMPSTRCT,0          SET TEMPSTOR PGS USED TO ZERO                
         MVI   TMPSTRPG,0          SET CURR TEMPSTOR PAGE TO ZERO               
         SPACE                                                                  
* CLEAR SAVED CODE TABLE *                                                      
         SPACE                                                                  
         LA    R0,CODETABL                                                      
         LHI   R1,CODETBLN                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
         L     R1,=A(LCOMBOS)                                                   
         A     R1,RELO                                                          
*                                                                               
LREC0030 CLI   0(R1),X'FF'         FIND THE KEY COMBINATION                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(3,R1),SVKEY       SVKEY IS SAVED FROM VALKEY                   
         BNE   LREC0050                                                         
         CLI   QLIST,0             TEST LIST OPTION SET                         
         BNE   LREC0040                                                         
         TM    4(R1),X'80'         NO-ONLY ACCEPT DEFAULT LIST                  
         BZ    LREC0050                                                         
         TM    LIMACC,LIMACSTA     TEST LIMITED STATION ACCESS                  
         BZ    LREC0060                                                         
         CLI   3(R1),QLSTA         YES-TEST LIST STATIONS                       
         BNE   LREC0060                                                         
         LA    R2,CONACTH          YES-ERROR                                    
         MVC   GERROR,=AL2(STALIM)                                              
         B     REPERR                                                           
*                                                                               
LREC0040 CLC   QLIST,3(R1)         MATCH THE LIST OPTION                        
         BE    LREC0060                                                         
*                                                                               
LREC0050 LA    R1,L'LCOMBOS(R1)                                                 
         B     LREC0030                                                         
*                                                                               
LREC0060 MVC   LSTTYPE,3(R1)       SAVE LIST TYPE                               
         MVC   LSTKEY,5(R1)        SAVE LIST KEY                                
         SPACE                                                                  
         CLC   LSTKEY,=AL1(QLSTA,QLGRP,0)  THIS HAS TO BE CHANGED               
         BNE   KATZ550                                                          
         CLC   AGENCY,=C'K3'       THIS KRGNY MASTER REP                        
         BE    KATZ550                                                          
         CLC   AGENCY,=C'BL'       THIS BLAIR MASTER REP                        
         BE    KATZ550                                                          
         CLC   AGENCY,=C'IR'       THIS IRNY MASTER REP                         
         BE    KATZ550                                                          
         SPACE                                                                  
         LA    RE,K3GRPCT                                                       
         L     RF,=A(K3GRPTBL)                                                  
         A     RF,RELO                                                          
         SPACE                                                                  
KATZ530  CLC   AGENCY,2(RF)                                                     
         BE    KATZ550                                                          
         LA    RF,4(,RF)                                                        
         BCT   RE,KATZ530                                                       
         MVI   LSTKEY+1,0          THIS HAS TO BE CHANGED                       
KATZ550  DS    0H                                                               
         SPACE                                                                  
         L     RF,=A(FILTAB)       CHECK NON-SET DATA                           
         A     RF,RELO                                                          
LREC0064 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   LSTTYPE,0(RF)                                                    
         BE    *+12                                                             
         LA    RF,L'FILTAB(RF)                                                  
         B     LREC0064                                                         
         SPACE                                                                  
         MVC   LSTTYPLN,1(RF)       SAVE LIST TYPE LENGTH                       
         LA    RE,ROKDTLVL-ROKEY                                                
         LA    RF,LSTKEY                                                        
         LA    R0,L'LSTKEY                                                      
*                                                                               
LREC0070 CLI   0(RF),0                                                          
         BE    LREC0080                                                         
         CLC   LSTTYPE,0(RF)                                                    
         BNE   *+8                                                              
         STC   RE,LSTDISP          SAVE DISPLACEMENT TO LISTED VARIABLE         
         LA    RE,8(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,LREC0070                                                      
*                                                                               
LREC0080 BCTR  RE,0                SAVE LENGTH FOR EXECUTED KEY COMPARE         
         STC   RE,LSTKEYEX                                                      
         MVI   LISTEND,C'N'                                                     
         MVI   FOUNDSET,C'N'                                                    
         MVI   FIRSTR,C'Y'                                                      
         MVI   QSKIPLR,0                                                        
         LA    R7,KEY              SET FIRST PART OF KEY                        
         USING RORECD,R7                                                        
         MVC   ROKREP,AGENCY                                                    
         MVC   ROKDTLRQ,TYPEKEY    ALL/CONFIRMED/DIRECT/UNCONFIRMED             
         MVC   ROKDTLTY,LSTKEY                                                  
*                                                                               
LREC0100 DS   0H                                                                
         MVC   KEYSAVE,KEY                                                      
DIE      EQU   *                                                                
         GOTO1 HIGH                                                             
         SPACE                                                                  
         MVI   TRCTYPE,04                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         SPACE                                                                  
         MVI   ERROR,0                                                          
         OI    GENSTAT1,CATCHIOR   I WANT CONTROL BACK                          
         GOTO1 CATCHIOS            DON'T TASKNEXT OUT!                          
         CLI   ERROR,0             ANY ERROR?                                   
         BNE   MAXIOERR                                                         
         SPACE                                                                  
LREC0110 CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BNE   LREC0154                                                         
         SPACE                                                                  
         L     R7,AIO              ADDRESS THE RECORD                           
*                                                                               
         LA    R0,3                CHECK RECORD AGAINST REQUEST FILTERS         
         LA    R2,ROKDTLTY                                                      
         LA    R3,ROKDTLVL                                                      
         LA    R4,RODATA-ROKDTLVL-1                                             
         LA    R5,RODATA-ROKDTLVL-9                                             
         LA    R6,KEY+ROKDTLVL-ROKEY                                            
*                                                                               
LREC0130 CLI   0(R2),0                                                          
         BE    LREC0210                                                         
         CLC   LSTTYPE,0(R2)       TEST LISTING ON THIS TYPE                    
         BNE   *+14                                                             
         CLC   0(8,R3),BLANKS      YES-TEST SIGNIFICANT VALUE                   
         BNH   LREC0190            NO-SKIP                                      
         L     RF,=A(FILTAB)                                                    
         A     RF,RELO                                                          
*                                                                               
LREC0140 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(RF)                                                    
         BE    LREC0142                                                         
         LA    RF,L'FILTAB(RF)                                                  
         B     LREC0140                                                         
         SPACE                                                                  
LREC0142 LH    R1,2(RF)                                                         
         LA    R1,SYSD(R1)                                                      
         ZIC   RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,LREX1            TEST FILTER IS SET (COMP TO SPACES)          
         BNH   LREC0160             NO                                          
         SPACE                                                                  
         CLI   0(R1),C'*'          IS THIS A SETS REQUEST?                      
         BNE   LREC0144             NO                                          
         SPACE                                                                  
         LA    RF,5(,R7)                                                        
         ST    RF,SETKEYAD                                                      
         SPACE                                                                  
         GOTO1 =A(FSET),RR=RELO    FILTER ON SET                                
         BNE   LREC0143            NOT IN SET                                   
         CLI   LSTTYPE,QLOFF       OFFICE LISTING?                              
         BE    LREC0160            YES -                                        
         B     LREC0200            NO                                           
LREC0143 EQU   *                                                                
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BE    LREC0230             YES                                         
         SPACE                                                                  
         B     LREC0100                                                         
         SPACE                                                                  
LREC0144 EX    RE,LREX2            YES-TEST RECORD VALUE AGAINST FILTER         
         BE    LREC0160            EQUAL-OK                                     
         BH    LREC0150            HIGH                                         
         XC    0(8,R6),0(R6)       LOW-SKIP TO FILTER VALUE                     
         EX    RE,LREX3                                                         
         SPACE                                                                  
         EX    R5,LREX4            NEXT FIELD TO NULLS                          
         SPACE                                                                  
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BE    LREC0230             YES                                         
         SPACE                                                                  
         B     LREC0100                                                         
*                                                                               
LREC0150 EQU   *                                                                
         CLI   LSTTYPE,QLOFF       OFFICE LISTING?                              
         BE    LREC0155            YES - DON'T TEST HIGH FOR OFFICE             
LREC0152 EQU   *                                                                
         CLC   LSTTYPE,0(R2)       HIGH-  TEST LISTING THIS TYPE                
         BE    LREC0160            YES-   OK (FILTER IS 'START AT')             
LREC0155 EQU   *                                                                
         CHI   R0,3                TEST 1ST ROW TYPE                            
         BNE   LREC0156                                                         
         SPACE                                                                  
LREC0154 MVI   LISTEND,C'Y'        YES-END OF LIST                              
         SPACE                                                                  
         LA    R1,CODETABL                                                      
         SR    R1,R9                                                            
         ST    R1,CDTBADR          SAVE FIRST ADDR IN CODE TABLE                
         B     LREC0230                                                         
         SPACE                                                                  
LREC0156 EX    R4,LREX5            NO-SKIP                                      
         XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         SPACE                                                                  
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BE    LREC0230             YES                                         
         SPACE                                                                  
         B     LREC0100                                                         
*                                                                               
LREC0160 CLI   LSTTYPE,QLOFF       TEST LISTING OFFICES                         
         BNE   LREC0170                                                         
*                                                                               
*   TEST DIE                                                                    
***      MVC   DIE,=X'0000'                                                     
*   TEST DIE END                                                                
*                                                                               
         CLI   0(R2),QLOFF         YES-TEST ON OFFICE NOW                       
         BNE   LREC0200                                                         
         OC    0(2,R3),0(R3)       YES-TEST NULL OFFICE                         
         BZ    LREC0190            YES-SKIP                                     
         OC    QREGION,QREGION     TEST REGION FILTER                           
         BZ    LREC0200                                                         
         BAS   RE,CHKRGN           YES-CHECK OFFICE IN REGION                   
         BNE   LREC0190            NO-SKIP                                      
         B     LREC0200                                                         
*                                                                               
LREC0170 CLI   LSTTYPE,QLSTA       TEST LISTING STATIONS                        
         BNE   LREC0180                                                         
         CLI   0(R2),QLSTA         YES-TEST ON STATION NOW                      
         BNE   LREC0200                                                         
*                                                                               
         CLI   NCOMBOS,0                                                        
         BE    LREC0174                                                         
         LA    RE,COMBOSTA          ADDRESS OF COMBO STATION TABLE              
         ZIC   RF,NCOMBOS           N'STATIONS                                  
LREC0172 CLC   0(7,R3),0(RE)       THIS ONE OF THE ONES WANTED                  
         BE    LREC0176             YES                                         
         LA    RE,7(,RE)                                                        
         BCT   RF,LREC0172                                                      
         GOTO1 SEQ                                                              
         MVI   TRCTYPE,X'11'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
         B     LREC0110            NO-SKIP                                      
*                                                                               
LREC0174 DS    0H                                                               
         CLC   =C'PV',AGENCY       THIS PETRY                                   
         BE    LREC0176             YES                                         
         CLI   QGROUP,C'*'         SET                                          
         BE    LREC0175             YES                                         
         CLI   QGROUP+1,C' '       YES-TEST SUBGROUP FILTER                     
         BNH   LREC0178                                                         
         SPACE                                                                  
         CLI   1(R2),QLGRP         YES-TEST NEXT IS GRP                         
         BE    LREC0175                                                         
         CLI   1(R2),QLGRGRP       YES-TEST NEXT IS GRP                         
         BNE   LREC0176                                                         
         SPACE                                                                  
LREC0175 DS    0H                                                               
         CLC   AGENCY,=C'K3'       THIS KRGNY MASTER REP                        
         BE    KATZ220                                                          
         CLC   AGENCY,=C'BL'       THIS BLAIR MASTER REP                        
         BE    KATZ220                                                          
         CLC   AGENCY,=C'IR'       THIS IRNY MASTER REP                         
         BE    KATZ220                                                          
         SPACE                                                                  
         LA    RE,K3GRPCT                                                       
         L     RF,=A(K3GRPTBL)                                                  
         A     RF,RELO                                                          
         SPACE                                                                  
KATZ210  CLC   AGENCY,2(RF)                                                     
         BE    KATZ220                                                          
         LA    RF,4(,RF)                                                        
         BCT   RE,KATZ210                                                       
         SPACE                                                                  
LREC0176 DS    0H                                                               
         SPACE                                                                  
         CLI   QGROUP,0            IF NO ENTRY                                  
         BE    LREC0178             IGNORE                                      
         SPACE                                                                  
         GOTO1 =A(CHKGRP),RR=RELO  YES-CHECK STATION IN GROUP                   
         BNE   LREC0190            NO-SKIP                                      
KATZ220  DS   0H                                                                
         SPACE                                                                  
LREC0178 OC    QAFF,QAFF           TEST AFFILIATE FILTER                        
         BZ    *+12                                                             
         BAS   RE,AFFCHK           YES-CHECK STATION IN AFFILIATE               
         BNE   LREC0190                                                         
         OC    QTVB,QTVB           TEST TVB FILTER                              
         BZ    *+12                                                             
         BAS   RE,TVBCHK           YES-CHECK STATION IN TVB REGION              
         BNE   LREC0190                                                         
         CLI   QRANK,0             TEST MARKET RANK FILTER                      
         BE    *+12                                                             
         BAS   RE,RNKCHK           YES-CHECK STATION IN MKT RANK                
         BNE   LREC0190                                                         
         OC    QOWNER,QOWNER       TEST OWNER FILTER                            
         BZ    *+12                                                             
         BAS   RE,OWNCHK           YES-CHECK STATION IN OWNERSHIP               
         BNE   LREC0190                                                         
******** OC    QMKT,QMKT           TEST MARKET FILTER                           
******** BZ    *+12                                                             
******** BAS   RE,CHKMKT           YES-CHECK STATION IN MARKET                  
******** BNE   LREC0190                                                         
*        OC    QTEAM,QTEAM         TEST TEAM FILTER                             
*        BZ    LREC0200            NO                                           
*        OC    QOFF,QOFF           YES-OFFICE FILTER MUST BE SET                
*        BNZ   *+6                                                              
*        DC    H'0'                                                             
*        BAS   RE,CHKTEM           CHECK STATION IN TEAM                        
*        BNE   LREC0190            NO-SKIP                                      
*        B     LREC0200                                                         
*                                                                               
LREC0180 CLI   LSTTYPE,QLSTY       TEST LISTING STATION TYPES                   
         BNE   LREC0184                                                         
         CLI   0(R2),QLSTY         YES-TEST ON STATION TYPE NOW                 
         BNE   LREC0200                                                         
         CLI   0(R3),C'4'          YES-IGNORE 'ALL' STATION TYPES               
         BNE   LREC0200                                                         
         B     LREC0190                                                         
*                                                                               
LREC0184 CLI   LSTTYPE,QLMKT       TEST LISTING MARKETS                         
         BNE   LREC0200                                                         
         CLI   0(R2),QLMKT         YES-TEST ON MARKET NOW                       
         BNE   LREC0200                                                         
         OC    QOWNER,QOWNER       FILTERING ON OWNER?                          
         BZ    LREC0200                                                         
         CLI   CKSECOWN,1          SPECIAL SECURITY FILTER ON OWNER             
         BNE   LREC0200                                                         
         SPACE                                                                  
         GOTO1 =A(CKOWNMKT),RR=RELO                                             
         BE    LREC0200                                                         
*                                                                               
LREC0190 EX    R5,LREX6            SKIP - FORCE NEXT ROW VALUE                  
         SPACE                                                                  
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BE    LREC0230             YES                                         
         SPACE                                                                  
         B     LREC0100                                                         
*                                                                               
LREC0200 LA    R2,1(R2)            NEXT ROW TYPE                                
         LA    R3,8(R3)                                                         
         LR    R4,R5                                                            
         AHI   R5,-8               SUBTRACT 8                                   
         LA    R6,8(R6)                                                         
         BCT   R0,LREC0130                                                      
*                                                                               
LREC0210 ZIC   R1,LSTDISP                                                       
         LA    R1,ROREC(R1)        R1=A(LIST LINE VALUE)                        
         MVC   LSTVAL,0(R1)        SAVE THE VALUE                               
         SPACE                                                                  
         CLI   LSTTYPE,QLSTY       TEST LISTING STATION TYPES                   
         BNE   LREC0220                                                         
         SPACE                                                                  
         BAS   RE,DSTY             DISPLAY STATION TYPE                         
         SPACE                                                                  
LREC0220 BAS   RE,LRTOTAL          TOTAL ALL THE MONTHS                         
         BNE   LREC0110            NO RECORDS FOUND                             
         SPACE                                                                  
         CLI   SET1CDE,0           USING SETS AS FILTERS                        
         BE    LREC0230             NO                                          
         CLI   SET1CDE,4           SETCODE = OFFICE?                            
         BNE   LREC0225            NO                                           
         CLI   LSTTYPE,QLOFF       OFFICE LISTING?                              
         BE    LREC0230            YES -                                        
LREC0225 EQU   *                                                                
         MVI   FOUNDSET,C'Y'       SET WAS FOUND                                
         SPACE                                                                  
         GOTO1 =A(CSETM),RR=RELO                                                
         BE    LREC0110                                                         
         SPACE                                                                  
LREC0230 CLI   LISTEND,C'Y'                                                     
         BNE   *+12                                                             
         CLI   FOUNDSET,C'Y'       SET WAS FOUND                                
         BNE   LREC0260             DONE                                        
*                                                                               
         OC    TOTALS(24),TOTALS   ANY TOTALS TO PRINT                          
         BZ    LREC0250                                                         
*                                                                               
         CLI   TOPNCT,0            WE LISTING TOP N                             
         BE    LREC0235             NO                                          
         SPACE                                                                  
         XC    ELEM,ELEM                                                        
         MVC   ELEM(4),TOTALS+4          CURR BILLING                           
         CLI   TOPTYPE,C'C'                                                     
         BE    LREC0231                                                         
         SPACE                                                                  
         MVC   ELEM(4),TOTALS            PRIOR BILLING                          
         CLI   TOPTYPE,C'P'                                                     
         BE    LREC0231                                                         
         SPACE                                                                  
         MVC   ELEM(4),TOTALS+8          PRIOR FINAL                            
         CLI   TOPTYPE,C'F'                                                     
         BE    LREC0231                                                         
         SPACE                                                                  
         MVC   ELEM(4),TOTALS+12         BUDGET                                 
         CLI   TOPTYPE,C'B'                                                     
         BE    LREC0231                                                         
         DC    H'0'                                                             
         SPACE                                                                  
LREC0231 TM    ELEM,X'80'            THIS A MINUS AMOUNT                        
         BO    LREC0250               YES, BYPASS                               
         OC    ELEM(4),ELEM          ZERO                                       
         BZ    LREC0250               YES, BYPASS                               
         SPACE                                                                  
         MVC   ELEM+4(24),KEYSAVE+6      KEY VALUES                             
         MVC   ELEM+28(24),TOTALS                                               
         MVC   ELEM+52(1),SVKEY+31     FIRST MONTH FOUND                        
         SPACE                                                                  
         LA    R2,TSARBLK                                                       
         USING TSARD,R2                                                         
         CLC   TOPNCT,TSARCT+1     SEE IF WE HAVE MAX RECORDS                   
         BH    LREC0232             NOT YET                                     
         SPACE                                                                  
         MVI   TSRNUM+1,1                                                       
         MVI   TSACTN,TSAGET                                                    
         LA    R1,WORK                                                          
         ST    R1,TSAREC                                                        
         GOTO1 ATSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(4),ELEM        COMPARE LOWEST TO NEW                        
         BH    LREC0250             BYPASS THIS, NOT NEEDED                     
         SPACE                                                                  
         MVI   TSACTN,TSADEL                                                    
         GOTO1 ATSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         BE    LREC0234                                                         
         DC    H'0'                                                             
         SPACE                                                                  
LREC0232 LH    RE,TSARCT                                                        
         LA    RE,1(,RE)                                                        
         STH   RE,TSARCT                                                        
         SPACE                                                                  
LREC0234 MVI   TSACTN,TSAADD                                                    
         MVI   TSRNUM+1,1                                                       
         LA    R1,ELEM                                                          
         ST    R1,TSAREC                                                        
         GOTO1 ATSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         BE    LREC0250                                                         
         CLI   TSERRS,TSEDUP       DUPLICATE IS OKAY                            
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   R1,15,ELEM          BUT FORCE 1 HIGHER AND TRY AGAIN             
         LA    R1,1(,R1)                                                        
         STCM  R1,15,ELEM                                                       
         B     LREC0234                                                         
         DROP  R2                                                               
*                                                                               
LREC0235 MVI   FIRSTR,C'N'                                                      
*                                                                               
         LA    R2,LISTAR+9                                                      
         MVC   LISTAR,BLANKS                                                    
         MVC   LISTAR(8),LSTVAL    FORMAT LISTED VARIABLE TO SCREEN             
*                                                                               
         CLI   MODE,LISTRECS                                                    
         BE    LREC0236                                                         
*                                                                               
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         LA    R2,P                                                             
         MVC   18(8,R2),LSTVAL                                                  
         LA    R2,27(R2)                                                        
*                                     FORMAT DOLLARS TO SCREEN                  
LREC0236 DS   0H                                                                
         GOTO1 =A(DOLLPRNT),RR=RELO                                             
         SPACE                                                                  
         OC    TOTALS(12),TOTALS      PRIOR/CURR BILLING/PRIOR FINAL            
         BNZ   LREC0237                                                         
         OC    TOTALS+16(8),TOTALS+16 PRIOR/CURR BOOKED                         
         BNZ   LREC0237                                                         
         CLC   =C'R ',LSTVAL          IF MEDIA R ONLY, BYPASS                   
         BE    LREC0250                                                         
         SPACE                                                                  
LREC0237 MVI   ANYDATA,C'Y'        SET DATA FOUND FOR REQUEST                   
         SPACE                                                                  
* ACCUMULATE TOTALS OF LIST                                                     
         SPACE                                                                  
         LA    R0,6                                                             
         LA    RE,MONTOTS+4                                                     
         LA    RF,TOTALS                                                        
LREC0238 L     R1,0(,RE)                                                        
         A     R1,0(,RF)                                                        
         ST    R1,0(,RE)                                                        
         LA    RE,4(,RE)                                                        
         LA    RF,4(,RF)                                                        
         BCT   R0,LREC0238                                                      
         SPACE                                                                  
         CLI   MODE,LISTRECS                                                    
         BNE   LREC0240                                                         
         SPACE                                                                  
         MVC   SVKEY2,KEY          SAVE KEY                                     
         MVC   KEY,SVKEY           SET KEY BACK TO THE RECORD WITH              
         SPACE                                                                  
         SPACE                                                                  
LREC0239 DS   0H                                                                
         LH    R1,TOTLCT           COUNT OF LINES DISPLAYED                     
         LA    R1,1(,R1)                                                        
         STH   R1,TOTLCT                                                        
         SPACE                                                                  
         GOTO1 LISTMON              THE FIRST MONTH (FOR DISPREC)               
         SPACE                                                                  
         MVC   KEY,SVKEY2          RESTORE KEY                                  
         SPACE                                                                  
         CLI   QSKIPLR,X'FF'       END OF TABLE SET IN DOLLPRNT(STEREO)         
         BE    LREC0154                                                         
         B     LREC0250                                                         
*                                                                               
LREC0240 CLI   MODE,PRINTREP                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  FORMAT DOLLARS TO SCREEN                     
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         DROP  R3                                                               
*                                                                               
LREC0250 XC    TOTALS(24),TOTALS   CLEAR TOTALS                                 
         SPACE                                                                  
         CLI   LISTEND,C'Y'                                                     
         BE    LREC0260             DONE                                        
         MVI   FOUNDSET,C'N'       SET OFF FOUND                                
         SPACE                                                                  
         CLI   SET1CDE,0           DOING SETS                                   
         BE    LREC0110             NO, LIST NEXT RECORD                        
         SPACE                                                                  
         CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BE    LREC0100                      SAME, DO READ HIGH                 
         SPACE                                                                  
LREC0260 CLI   TOPNCT,0            THIS A TOP N REQUEST                         
         BE    LREC0266             NO                                          
         SPACE                                                                  
         LA    R2,TSARBLK                                                       
         USING TSARD,R2                                                         
         OC    TSRNUM,TSRNUM       ANY RECS FOUND                               
         BZ    VKNODATA             NO                                          
         SPACE                                                                  
         SR    R1,R1                                                            
         ICM   R1,3,TSARCT         ARE THERE ANY RECORDS                        
         BZ    LREC0264             NO, NONE OR DONE                            
         SPACE                                                                  
* SET FOR TSAR RETRIEVE                                                         
         SPACE                                                                  
         MVI   TSACTN,TSAGET                                                    
         STH   R1,TSRNUM                                                        
         BCTR  R1,0                                                             
         STH   R1,TSARCT                                                        
         CLC   TOPNCT,TSRNUM+1                                                  
         BL    LREC0266                                                         
         SPACE                                                                  
         LA    R1,ELEM                                                          
         ST    R1,TSAREC                                                        
         GOTO1 ATSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVKEY+6(24),ELEM+4                                               
         MVC   SVKEY+31(1),ELEM+52       MONTH                                  
         MVC   TOTALS(24),ELEM+28                                               
         ZIC   RE,LSTDISP                                                       
         AHI   RE,-6               SUBTRACT 6                                   
         LA    R1,ELEM+4(RE)                                                    
         MVC   LSTVAL,0(R1)                                                     
         CLI   LSTTYPE,QLSTY       TEST LISTING STATION TYPES                   
         BNE   LREC0262                                                         
         SPACE                                                                  
         BAS   RE,DSTY             DISPLAY STATION TYPE                         
         SPACE                                                                  
LREC0262 ZIC   R1,SCRNCT                                                        
         LA    R1,1(,R1)                                                        
         STC   R1,SCRNCT                                                        
         CLI   SCRNCT,13                                                        
         BNE   LREC0235                                                         
         MVI   TSACTN,TSASAV                                                    
         GOTO1 ATSAR,TSARBLK                                                    
         CLI   TSERRS,0                                                         
         BE    LREC0235                                                         
         DC    H'0'                                                             
         DROP  R2                                                               
LREC0264 MVI   TOPNCT,0                                                         
         MVI   SCRNCT,0                                                         
         XC    TSARCT,TSARCT                                                    
         SPACE                                                                  
LREC0266 DS   0H                                                                
         CLI   MODE,PRINTREP       OFFLINE?                                     
         BNE   LREC0270            NO                                           
         CLI   FIRSTR,C'Y'         ANY DATA?                                    
*        BE    EXIT                 NO                                          
         BNE   LREC0270             YES                                         
*                                   NO  - SHOW ERROR MESSAGE ON REPORT          
*        MVC   P(35),=C'NO RRGON RECORDS FOR THIS KEY'                          
*        GOTO1 SPOOL,DMCB,ASPOOLD                                               
*        B     EXIT                                                             
         SPACE 2                                                                
LREC0270 DS   0H                                                                
         CLI   ANYDATA,C'N'        SET DATA FOUND FOR REQUEST                   
         BE    VKNODATA                                                         
         SPACE                                                                  
*        SEE IF STILL HAVE TOTALS LINE TO PRINT                                 
         SPACE                                                                  
         OC    MONTOTS+4(24),MONTOTS+4                                          
         BZ    LREC0280                                                         
         SPACE                                                                  
         MVC   LISTAR,BLANKS                                                    
         MVC   LISTAR(6),=C'TOTALS'                                             
         SPACE                                                                  
         CLI   FULLSTER,C'Y'       THIS FULL STEREO                             
         BNE   LREC0272             YES                                         
         MVI   LISTEND,C'Y'                                                     
         MVC   LISTAR(2),=C'@@'                                                 
         MVC   LISTAR+2(6),=C'TOTALS'                                           
LREC0272 EQU   *                                                                
         SPACE                                                                  
         LA    R2,LISTAR+9                                                      
         MVC   TOTALS(24),MONTOTS+4                                             
         GOTO1 =A(DOLLPRNT),RR=RELO                                             
         XC    KEY,KEY                                                          
         XC    DMDSKADD,DMDSKADD                                                
         XC    MONTOTS+4(24),MONTOTS+4   SET OFF - TOTALS LINE PRINTED          
         SPACE                                                                  
         CLI   MODE,PRINTREP       OFFLINE?                                     
         BE    LREC0276             YES                                         
         SPACE                                                                  
         GOTO1 LISTMON                                                          
         B     LREC0280                                                         
         SPACE                                                                  
LREC0276 DS   0H                                                                
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         MVC   P,SPACES                                                         
         MVC   P+18(6),LISTAR                                                   
         MVC   P+27(71),LISTAR+9                                                
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         DROP  R3                                                               
         SPACE                                                                  
LREC0280 DS   0H                                                                
         CLI   QREFRESH,C'Y'       'REFRESH' OPTION?                            
         BNE   EXIT                NO  - GET OUT                                
         CLI   LISTEND,C'Y'        YES - END OF REAL DATA?                      
         BNE   EXIT                NO                                           
         SPACE                                                                  
         CLI   TMPSTRCT,0          ANY TEMPSTR USED                             
         BE    LREC0290             NO                                          
         SPACE                                                                  
* NEED TO WRITE TWAN WITH CODETABL AND CLEAR FOR MORE ENTRIES *                 
         SPACE                                                                  
         CLI   TMPSTRCT,MAXPAGE    MAX PAGES                                    
         BL    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         ST    RF,PARAS            SAVE DATA SIZE                               
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    SET TERMINAL NUMBER                          
         ZIC   R1,TMPSTRCT                                                      
         LA    R1,1(,R1)                                                        
         STC   R1,TMPSTRCT                                                      
         STC   R1,DMCB+8           SET PAGE                                     
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,CODETABL                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
LREC0290 DS   0H                                                                
         LA    R2,CONACTH          YES - ASK FOR ANOTHER GO-ROUND               
         MVI   GMSGTYPE,C'I'       SET 'INFO MESSAGE'                           
         MVC   GERROR(2),=H'9'     INSERT 'HIT ENTER' ERROR MSG                 
         B     REPERR               EXIT AS 'ERROR'                             
*                                                                               
*                                                                               
*                                                                               
LREX1    CLC   0(0,R1),BLANKS      EXECUTED INSTRUCTIONS                        
LREX2    CLC   0(0,R3),0(R1)                                                    
LREX3    MVC   0(0,R6),0(R1)                                                    
LREX4    XC    8(0,R6),8(R6)                                                    
LREX5    MVC   0(0,R6),XFF                                                      
LREX6    MVC   8(0,R6),XFF                                                      
         EJECT                                                                  
LRTOTAL  ST    RE,SVREGE                                                        
*                                                                               
*                                                                               
*        ROUTINE TO BUILD TOTAL LINE FOR LISTRECS                               
*        LSTKEYEX = LENGTH FOR KEY COMPARE                                      
*                                                                               
         ZIC   R4,LSTKEYEX                                                      
         MVC   SVKEY,KEY           SAVE KEY OF FIRST MONTH RECORD               
         CLI   SET1CDE,0           USING SETS AS FILTERS                        
         BNE   *+10                 YES                                         
         XC    TOTALS(24),TOTALS                                                
         SPACE                                                                  
         LA    R5,TOTALS                                                        
         L     R7,AIO                                                           
         USING RORECD,R7                                                        
*                                                                               
LT010    CLC   ROKDTLYM(1),QSTART  CHECK THE YEAR                               
         BNL   LT020                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         MVI   TRCTYPE,X'12'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
         EX    R4,LTCOMPK                                                       
         BNE   LT900               NO DATA                                      
         B     LT010                                                            
*                                                                               
LT020    CLC   ROKDTLYM(1),QSTART                                               
         BH    LT800               YEAR HIGH - NO DATA                          
         CLI   ROKDTLYM+1,0        CHECK FOR PRIOR MONTH                        
         BNE   LT040                                                            
*                                                                               
LT030    MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                 SKIP TO NON-PRIOR MONTH                      
         MVI   TRCTYPE,X'13'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
         EX    R4,LTCOMPK                                                       
         BNE   LT900               NO DATA                                      
         B     LT020                                                            
*                                                                               
LT040    CLC   ROKDTLYM,QSTART     CHECK MON AGAINST START MON                  
         BL    LT030               LOW - GET NEXT                               
         CLC   ROKDTLYM,QEND       CHECK MON AGAINS END MON                     
         BH    LT800               HIGH - NO DATA                               
*                                                                               
LT050    DS    0H                                                               
         LA    R6,RODPER           PERIOD COLS                                  
*        CLI   QPY,C'Y'            TEST FOR YTD                                 
*        BNE   *+8                                                              
*        LA    R6,RODYTD           YES - POINT TO YTD FIGURES                   
         GOTO1 =A(DRTOTAL),RR=RELO TOTAL UP                                     
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                 NEXT REC                                     
         CLI   TRCTYPE,X'14'                                                    
         BE    LT060                                                            
         MVI   TRCTYPE,X'14'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
LT060    EX    R4,LTCOMPK                                                       
         BNE   LT999               DONE                                         
         CLC   ROKDTLYM,QEND       CHECK REACHED END MONTH                      
         BNH   LT050                                                            
         LA    R7,KEY              GET NEXT RECORD PAST THIS KEY COMBO          
         MVI   ROKDTLYM,X'FF'                                                   
         GOTO1 HIGH                                                             
*        MVI   TRCTYPE,6                                                        
*        GOTO1 =A(TRACE),RR=RELO                                                
         B     LT999                                                            
*                                                                               
LT800    LA    R7,KEY              GET NEXT RECORD PAST THIS KEY COMBO          
         MVI   ROKDTLYM,X'FF'                                                   
         GOTO1 HIGH                                                             
         MVI   TRCTYPE,7                                                        
         GOTO1 =A(TRACE),RR=RELO                                                
*                                                                               
LT900    LTR   RB,RB               CC NE - NO RECORDS FOUND                     
         B     LTX                                                              
*                                                                               
LT999    CR    RB,RB               CC EQ - RECORDS FOUND                        
*                                                                               
LTX      L     RE,SVREGE                                                        
         BR    RE                                                               
         SPACE 2                                                                
LTCOMPK  CLC   KEY(0),KEYSAVE      ** EXECUTED                                  
         EJECT                                                                  
CHKRGN   LR    R0,RE                                                            
         BAS   RE,SETREPFL         REPFILE VALUES                               
         XC    KEY,KEY                                                          
         MVI   KEY,4                                                            
         MVC   KEY+23(2),AGENCY                                                 
         MVC   KEY+25(2),0(R3)                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   CHKRGN20                                                         
*        GOTO1 READ                GET OFFICE RECORD                            
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         USING ROFFD,R1                                                         
         BAS   RE,RSETRRGO         BACK TO RRGON FILE VALUES                    
         CLC   ROFFREG,QREGION     CHECK THE OFFICE REGION                      
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE                                                                  
CHKRGN20 DS    0H                                                               
         BAS   RE,RSETRRGO         BACK TO RRGON FILE VALUES                    
         SPACE                                                                  
         LTR   RE,R0               SET COND CODE NE                             
         BR    RE                                                               
         DROP  R1                                                               
         SPACE 2                                                                
* CHANGE STATION TYPE FROM 1/2/3 TO READABLE                                    
         SPACE                                                                  
DSTY     MVC   LSTVAL,BLANKS       YES-CONVERT STATION TYPE CODE                
         MVC   LSTVAL(3),=C'NEW'                                                
         CLI   0(R1),C'2'                                                       
         BER   RE                                                               
         MVC   LSTVAL(3),=C'OLD'                                                
         CLI   0(R1),C'3'                                                       
         BER   RE                                                               
         MVC   LSTVAL(4),=C'COMP'                                               
         CLI   0(R1),C'1'                                                       
         BER   RE                                                               
         DC    H'0'                                                             
         EJECT                                                                  
AFFCHK   DS   0H                                                                
         MVI   DUB+2,1             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE 1                                                                
TVBCHK   DS   0H                                                                
         MVI   DUB+2,2             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE 1                                                                
OWNCHK   DS   0H                                                                
         MVI   DUB+2,3             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE 1                                                                
*HKMKT   NTR1  ,                                                                
*        GOTO1 =A(GETSTA),RR=RELO                                               
*        BNE   EXIT                                                             
*        L     R6,AIO2                                                          
*        MVI   ELCODE,8            LOOK FOR EXTRA DESCRIPTION ELEMENT           
*        BAS   RE,GETEL                                                         
*        BNE   EXIT                                                             
*        USING RSTAXXEL,R6                                                      
*        CLC   RSTAMKTC,QMKT       CHECK THE STATION'S MARKET                   
*        B     EXIT                                                             
*        DROP  R6                                                               
         SPACE                                                                  
RNKCHK   DS   0H                                                                
         MVI   DUB+2,4             SET COMPARISON FLAG                          
         B     ALLCHK                                                           
         SPACE                                                                  
STYCHK   DS   0H                                                                
         MVI   DUB+2,5             SET COMPARISON FLAG FOR STATION TYPE         
         SPACE                                                                  
ALLCHK   NTR1                                                                   
         GOTO1 =A(GESTATS),RR=RELO                                              
         B     EXIT                                                             
         SPACE                                                                  
*HKTEM   NTR1  ,                                                                
*        GOTO1 =A(GETSTA),RR=RELO                                               
*        L     R6,AIO2                                                          
*        MVI   ELCODE,4            LOOK FOR OFFICE/TEAM ELEMENT                 
*        BAS   RE,GETEL                                                         
*        BNE   EXIT                                                             
*        USING RSTAOTEL,R6                                                      
*                                                                               
*T010    CLC   RSTAOTOF,QOFF       CHECK THE OFFICE                             
*        BE    CT020                                                            
*        BAS   RE,NEXTEL                                                        
*        BNE   EXIT                                                             
*        B     CT010                                                            
*                                                                               
*T020    CLC   RSTAOTTM,QTEAM      CHECK THE TEAM                               
*        B     EXIT                                                             
*        DROP  R6                                                               
         EJECT                                                                  
*                                                                               
*        ROUTINE TO SET UP REP FILE VALUES                                      
*                                                                               
SETREPFL DS    0H                                                               
         MVC   FILENMSV,FILENAME   SAVE RRGON I/O VALUES                        
         MVC   SVAIO,AIO                                                        
         MVC   SVKEY2,KEY                                                       
         MVC   LKEYSV,LKEY                                                      
         MVC   USEIOSV,USEIO                                                    
         MVC   DMINBTS,RPINBTS     REP IN/OUT DATAMGR BITS                      
         MVC   DMOUTBTS,RPOUTBTS                                                
         XC    FILENAME,FILENAME   SWITCH TO REP I/O VALUES                     
         MVC   AIO,AIO2                                                         
         MVI   LKEY+1,27                                                        
         MVI   USEIO,C'N'                                                       
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
*        ROUTINE TO RESET RRGON FILE VALUES                                     
*                                                                               
RSETRRGO DS    0H                                                               
         MVC   FILENAME,FILENMSV   SWITCH BACK TO RRGON I/O VALUES              
         MVC   AIO,SVAIO                                                        
         MVC   KEY,SVKEY2                                                       
         MVC   LKEY,LKEYSV                                                      
         MVC   USEIO,USEIOSV                                                    
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         NI    DMOUTBTS,X'FD'      IGNORE DELETED RECS                          
         BR    RE                                                               
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
VKOLAERR LA    R2,CONACTH                                                       
         MVC   GERROR,=AL2(OFFLIM)                                              
         B     REPERR                                                           
*                                                                               
VKSLAERR LA    R2,CONACTH                                                       
         MVC   GERROR,=AL2(STALIM)                                              
         B     REPERR                                                           
*                                                                               
VKLSTERR LA    R2,RRGOPTH                                                       
         MVI   ERROR,INVLIST                                                    
         B     TRAPERR                                                          
*                                                                               
MISCOMER LA    R2,RRGSTAH                                                       
         CLI   5(R2),0                                                          
         BE    MISSERR                                                          
         L     R1,=A(MISCOMMS-1)                                                
         B     USRERRMS                                                         
*                                                                               
VKMISERR LA    R2,RRGGRPH                                                       
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     TRAPERR                                                          
*                                                                               
VKFTRERR LA    R2,RRGOPTH                                                       
         L     R1,=A(VKFTRERM-1)                                                
         B     USRERRMS                                                         
*                                                                               
CONDCTER LA    R2,RRGCTYH                                                       
         L     R1,=A(CONDCTEM-1)                                                
         B     USRERRMS                                                         
*                                                                               
MAXIOERR DS    0H                                                               
         MVI   ERROR,0             RESET ERROR                                  
         OI    GENSTAT2,USMYOK                                                  
         L     R1,=A(MAXIOMSG-1)                                                
         B     USRERRMS                                                         
*                                                                               
SETLSTER LA    R2,RRGSTAH                                                       
         L     R1,=A(SETLSTMS-1)                                                
         B     USRERRMS                                                         
*                                                                               
STYPERR  DS   0H                                                                
         L     R1,=A(STYPERMS-1)                                                
         B     USRERRMS                                                         
*                                                                               
CONENTER LA    R2,RRGDCTH                                                       
         L     R1,=A(CONENTEM-1)                                                
*                                                                               
USRERRMS XC    CONHEAD,CONHEAD                                                  
*                                                                               
USRERRMA A     R1,RELO                                                          
         ZIC   RF,0(R1)                                                         
         EX    RF,ERRMVC                                                        
         S     R1,RELO                                                          
         SPACE                                                                  
         C     R1,=A(MAXIOMSG-1)   THIS IS SET BY MAX I/O'S EXCEEDED            
         BE    MAXIOSCR                                                         
         SPACE                                                                  
         GOTO1 ERREX2                                                           
ERRMVC   MVC   CONHEAD(0),1(R1)                                                 
         SPACE                                                                  
* THIS WILL JUST APPEAR TO FILL THE SCREEN (WITH BLANKS)  *                     
* TO ALLOW NORMAL PROCESSING TO RESUME AFTER TOO MANY IOS *                     
         SPACE                                                                  
MAXIOSCR MVC   LISTAR,BLANKS                                                    
MAXIOSC  GOTO1 LISTMON                                                          
         B     MAXIOSC                                                          
*                                                                               
VKLSTMIS LA    R2,RRGOPTH                                                       
         MVC   GERROR,=AL2(NOLIST)                                              
*                                                                               
REPERR   GOTO1 RRGERR                                                           
*                                                                               
VKKEYER  LA    RE,KEYSAVE+5                                                     
         LA    RF,3                                                             
         CLI   0(RE),QLAGENCY                                                   
         BE    *+16                                                             
         LA    RE,1(,RE)                                                        
         BCT   RF,*-12                                                          
         B     VKKEYERR                                                         
         MVI   0(RE),QLAGY                                                      
*                                                                               
         LA    RE,SVKEY                                                         
         LA    RF,3                                                             
         CLI   0(RE),QLAGENCY                                                   
         BE    *+14                                                             
         LA    RE,1(,RE)                                                        
         BCT   RF,*-12                                                          
         DC    H'0'                                                             
         MVI   0(RE),QLAGY                                                      
         MVC   KEY,KEYSAVE                                                      
*        MVC   SVKEY(3),KEYSAVE+5                                               
         B     VKEY0500                                                         
         SPACE                                                                  
VKKEYERR L     R2,AFSTFLD                                                       
         MVI   ERROR,INVKEY                                                     
         B     TRAPERR                                                          
*                                                                               
VKPERERR LA    R2,RRGPERH                                                       
         MVI   ERROR,INVRPER                                                    
         B     TRAPERR                                                          
*                                                                               
VKRECERR L     R2,AFSTFLD                                                       
         MVI   ERROR,INVRGREC                                                   
         B     TRAPERR                                                          
*                                                                               
VKOFFLOC LA    R2,RRGOFFH                                                       
         MVI   ERROR,SECLOK                                                     
         B     TRAPERR                                                          
*                                                                               
VKSTAERR LA    R2,RRGSTAH                                                       
         MVI   ERROR,SECLOK                                                     
         B     TRAPERR                                                          
*                                                                               
BADTRY   LA    R2,CONACTH                                                       
         MVI   ERROR,INVACT                                                     
         B     TRAPERR                                                          
*                                                                               
VKNODATA LA    R2,CONRECH                                                       
         MVI   ERROR,NODATA                                                     
         MVI   QREFRESH,C'N'       SET OFF 'REFRESH' REQUEST                    
         MVI   LISTEND,C'N'                                                     
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
         EJECT                                                                  
CLRSCRN  LR    R0,RE                                                            
*                                                                               
*        ROUTINE TO CLEAR THE SCREEN                                            
*        FROM FIELD AT R2                                                       
*                                                                               
         SR    RE,RE                                                            
*                                                                               
CS010    IC    RE,0(R2)                                                         
         AHI   RE,-9               SUBTRACT 9                                   
         EX    RE,CSCLC                                                         
         BE    CS020                                                            
         EX    RE,CSOC                                                          
         BZ    CS020                                                            
         EX    RE,CSXC                                                          
         OI    6(R2),X'80'                                                      
*                                                                               
CS020    LA    R2,9(RE,R2)                                                      
         CLI   0(R2),9                                                          
         BH    CS010                                                            
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
CSCLC    CLC   8(0,R2),BLANKS                                                   
CSOC     OC    8(0,R2),8(R2)                                                    
CSXC     XC    8(0,R2),8(R2)                                                    
*                                                                               
         EJECT                                                                  
ROUND    DS    0H                                                               
*                                                                               
*        ROUTINE TO ROUND UNITS TO THOUSANDS                                    
*        INPUT:  R5 = UNITS                                                     
*        OUTPUT: R5 = ROUNDED TO THOUSANDS                                      
*                CC = 0 IF ROUNDED IS ZERO                                      
*                                                                               
         LR    R0,R5                                                            
         SR    R1,R1                                                            
         SRDA  R0,31                                                            
         D     R0,=F'1000'                                                      
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                                                             
         SRA   R1,1                                                             
         LTR   R5,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
STEREO   DC    C'815'                                                           
         DROP  RB,RC                                                            
         EJECT                                                                  
*                                                                               
XFF      DC    XL24'FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'           
*                                                                               
BLANKS   DC    CL80' '                                                          
*                                                                               
         DC    AL1(L'VKFTRERM-1)                                                
VKFTRERM DC    C'* ERROR * BAD FILTER COMBINATION *'                            
         DC    AL1(L'CONDCTEM-1)                                                
CONDCTEM DC    C'* ERROR * ONLY L=MON WITH *&& CONTYPE *'                       
         DC    AL1(L'MAXIOMSG-1)                                                
MAXIOMSG DC    C'* NOTE * PRESS ENTER TO CONTINUE PROCESSING *'                 
         DC    AL1(L'CONENTEM-1)                                                
CONENTEM DC    C'* ERROR * NO DEVTYPE WITH *&& CONTYPE *'                       
         DC    AL1(L'SETLSTMS-1)                                                
SETLSTMS DC    C'* ERROR * CAN''T ENTER SET FOR LIST OF SAME TYPE *'            
         DC    AL1(L'STYPERMS-1)                                                
STYPERMS DC    C'ER/0901 STATYPE ONLY AVAILABLE FOR THIS YEAR'                  
         DC    AL1(L'MISCOMMS-1)                                                
MISCOMMS DC    C'* ERROR * MUST ENTER COMBO STATION *'                          
*                                                                               
HEADING  SSPEC H1,3,C'REPPAK RRGON SYSTEM'                                      
         SSPEC H1,47,C'SALES REPORT'                                            
         SSPEC H1,73,AGYNAME                                                    
         SSPEC H2,3,C'-------------------'                                      
         SSPEC H2,47,C'------------'                                            
         SSPEC H2,73,AGYADD                                                     
         SSPEC H3,35,C'FOR THE PERIOD FROM'                                     
         SSPEC H3,62,C'TO'                                                      
         SSPEC H4,73,REPORT                                                     
         SSPEC H4,85,RUN                                                        
         SSPEC H5,93,PAGE                                                       
         SSPEC H5,73,REQUESTOR                                                  
         SSPEC H8,27,C'.   PRIOR  .  CURRENT . CUR  .   PRIOR  .PCT TO'         
         SSPEC H8,74,C'.  CURRENT .PCT TO.'                                     
         SSPEC H9,27,C'   BILLING    BILLING  PACING    FINAL    FINAL'         
         SSPEC H9,77,C'BUDGET   BUDGET'                                         
         SSPEC H9,19,C'-------'                                                 
         DC    X'00'               END MARKER FOR SSPEC                         
         EJECT                                                                  
*  LISTOPTS TABLE:  FIELD DEFINITIONS                                           
*        DISPLACEMENT 0:  FULL FIELD NAME                                       
*                     8:  SHORT FIELD NAME                                      
*                    12:  MINIMUM COMPARISON LEN:  FULL NAME                    
*                    13:  MINIMUM COMPARISON LEN:  SHORT NAME                   
*                    14:  OPTION ACTION VALUE                                   
*                    15:  SPECIAL ACTION VALUE                                  
*                    16:  SECURITY BIT                                          
*                                                                               
LISTOPTS DS   0CL17                                                             
         DC    CL8'GRPSUB  ',CL4'GS  ',AL1(1,1),AL1(QLGRP),AL1(0)               
         DC    AL1(GRPBREAK)                                                    
         DC    CL8'OFFICE  ',CL4'OFC ',AL1(1,1),AL1(QLOFF),AL1(0)               
         DC    AL1(OFFBREAK)                                                    
         DC    CL8'REGION  ',CL4'RGN ',AL1(2,2),AL1(QLREG),AL1(0)               
         DC    AL1(RGNBREAK)                                                    
         DC    CL8'STATION ',CL4'STN ',AL1(1,1),AL1(QLSTA),AL1(0)               
         DC    AL1(STABREAK)                                                    
         DC    CL8'COMBO   ',CL4'    ',AL1(3,0),AL1(QLSTA),AL1(QLCOMBO)         
         DC    AL1(0)                                                           
         DC    CL8'CLASS   ',CL4'    ',AL1(2,0),AL1(QLCLS),AL1(0),X'00'         
         DC    CL8'CATEGORY',CL4'CTGY',AL1(2,2),AL1(QLCAT),AL1(0),X'00'         
         DC    CL8'STATYPE ',CL4'STYP',AL1(5,3),AL1(QLSTY),AL1(0),X'00'         
         DC    CL8'TVB     ',CL4'    ',AL1(1,0),AL1(QLTVB),AL1(0),X'00'         
         DC    CL8'OWNER   ',CL4'    ',AL1(1,0),AL1(QLOWN),AL1(0)               
         DC    AL1(OWNBREAK)                                                    
         DC    CL8'RANK    ',CL4'RNK ',AL1(1,1),AL1(QLRNK),AL1(0),X'00'         
         DC    CL8'CONTYPE ',CL4'    ',AL1(2,0),AL1(QLCON),AL1(0),X'00'         
         DC    CL8'MARKET  ',CL4'MKT ',AL1(2,2),AL1(QLMKT),AL1(0)               
         DC    AL1(MKTBREAK)                                                    
         DC    CL8'AFFIL   ',CL4'    ',AL1(2,0),AL1(QLAFF),AL1(0),X'00'         
         DC    CL8'ADVRTSR ',CL4'    ',AL1(2,0),AL1(QLADV),AL1(0),X'00'         
         DC    CL8'AGENCY  ',CL4'AGY ',AL1(2,2),AL1(QLAGY),AL1(0),X'00'         
         DC    CL8'DEVTYPE ',CL4'DCT ',AL1(2,2),AL1(QLDCT),AL1(0),X'00'         
         DC    CL8'CAGENCY ',CL4'CAGY',AL1(2,2),AL1(QLAGENCY),AL1(0,0)          
         DC    CL8'SALESPER',CL4'SP  ',AL1(2,1),AL1(QLSAL),AL1(0),X'00'         
         DC    CL8'MONTHS  ',CL4'MON ',AL1(2,2),AL1(QLMON),AL1(0),X'00'         
         SPACE                                                                  
         DC    AL1(0)              END OF TABLE                                 
         EJECT                                                                  
* TABLE OF REQUEST FILTERS                                                      
*                                                                               
*        DEFINITION                                                             
*   BYTE 0 - REQUEST TYPE - CHECKED AGAINST FIRST 3 BYTES OF KEY                
*        1 - LENGTH OF FILTER FIELD                                             
*        2 - DISPLACEMENT ADDRESS OF FILTER FIELD                               
*                                                                               
         DS    0F                                                               
FILTAB   DS    0XL4                                                             
         DC    AL1(QLSTA),AL1(L'QSTA),AL2(QSTA-SYSD)                            
         DC    AL1(QLREG),AL1(L'QREGION),AL2(QREGION-SYSD)                      
         DC    AL1(QLOFF),AL1(L'QOFF),AL2(QOFF-SYSD)                            
         DC    AL1(QLTEM),AL1(L'QTEAM),AL2(QTEAM-SYSD)                          
         DC    AL1(QLGRP),AL1(L'QGROUP),AL2(QGROUP-SYSD)                        
         DC    AL1(QLADV),AL1(L'QADV),AL2(QADV-SYSD)                            
         DC    AL1(QLAGY),AL1(L'QAGY),AL2(QAGY-SYSD)                            
         DC    AL1(QLAFF),AL1(L'QAFF),AL2(QAFF-SYSD)                            
         DC    AL1(QLCLS),AL1(L'QCLASS),AL2(QCLASS-SYSD)                        
         DC    AL1(QLCAT),AL1(L'QCTGY),AL2(QCTGY-SYSD)                          
         DC    AL1(QLSTY),AL1(L'QSTATY),AL2(QSTATY-SYSD)                        
         DC    AL1(QLTVB),AL1(L'QTVB),AL2(QTVB-SYSD)                            
         DC    AL1(QLOWN),AL1(L'QOWNER),AL2(QOWNER-SYSD)                        
         DC    AL1(QLGRGRP),AL1(1),AL2(QGROUP-SYSD)                             
         DC    AL1(QLRNK),AL1(L'QRANK),AL2(QRANK-SYSD)                          
         DC    AL1(QLCON),AL1(L'QCONTY),AL2(QCONTY-SYSD)                        
         DC    AL1(QLMKT),AL1(L'QMKT),AL2(QMKT-SYSD)                            
         DC    AL1(QLDCT),AL1(L'QDCT),AL2(QDCT-SYSD)                            
         DC    AL1(QLAGENCY),AL1(L'QAGY-2),AL2(QAGY-SYSD)                       
         DC    AL1(QLSAL),AL1(L'QSAL),AL2(QSAL-SYSD)                            
         DC    AL1(0)                                                           
         EJECT                                                                  
       ++INCLUDE RENRGLTBLN                                                     
         EJECT                                                                  
* CHECK SECURITY *                                                              
         SPACE                                                                  
BLDMON   NMOD1 0,*BLDMON*                                                       
         L     RC,0(R1)            RESET A(WORKSPACE)                           
         LA    R4,MONTOTS                                                       
         LA    R5,12                                                            
         LR    R1,R4                                                            
         LR    RF,R5                                                            
         XC    0(MONTOTLN,R1),0(R1)                                             
         LA    R1,MONTOTLN(,R1)                                                 
         BCT   RF,*-10                                                          
         ZIC   RE,QSTART+1                                                      
         ZIC   RF,QEND+1                                                        
         MVC   0(2,R4),QSTART                                                   
         CLI   QPY,C'Y'            THIS YEAR TO DATE                            
         BNE   BLDMON4              NO, PERIOD                                  
         CLI   QSTART+1,1                                                       
         BE    BLDMON4                                                          
         MVI   1(R4),0                                                          
         LA    R4,MONTOTLN(,R4)                                                 
         BCTR  R5,0                                                             
         MVC   0(2,R4),QSTART                                                   
         B     BLDMON4                                                          
*                                                                               
BLDMON2  CR    RE,RF                                                            
         BH    BLDMONX                                                          
         MVC   0(1,R4),QSTART                                                   
         STC   RE,1(R4)                                                         
*                                                                               
BLDMON4  LA    R4,MONTOTLN(,R4)                                                 
         LA    RE,1(,RE)                                                        
         BCT   R5,BLDMON2                                                       
*                                                                               
BLDMONX  EQU   *                                                                
         XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
CKSEC    NMOD1 0,**+SEC**                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         MVC   RFBLOCK(4),ACOMFACS                                              
         MVC   RFBLOCK+4(2),AGENCY                                              
         XC    BLOCK(SBREPCDE+2-SBLOCK),BLOCK                                   
         LA    R3,BLOCK                                                         
         USING SBLOCKD,R3                                                       
         MVC   SBGROUP,RRGGRP                                                   
         MVC   SBOFFICE,RRGOFF                                                  
         SPACE                                                                  
         MVC   SBSTATN(4),QSTA                                                  
         LA    R1,QSTA+5                                                        
         CLI   SBSTATN+3,C'-'                                                   
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         MVI   SBSTATN+3,C' '                                                   
         SPACE                                                                  
         MVC   SBSTATN+4(1),0(R1)                                               
         CLI   SBSTATN+4,C'T'                                                   
         BNE   *+8                                                              
         MVI   SBSTATN+4,C' '                                                   
         SPACE                                                                  
         MVC   SBOWNER(L'RRGOWN),RRGOWN                                         
         MVC   SBMARKET,RRGMKT                                                  
         MVC   SBREGION,RRGREG                                                  
*        MVC   SBSALES,RRGSAL                                                   
         MVC   SBBREAKS,SECBRK                                                  
         MVC   SBREPCDE,AGENCY                                                  
         SPACE                                                                  
         XC    DMCB(24),DMCB                                                    
*                                  ESTABLISH A(REPFACS)                         
         GOTO1 CALLOV,DMCB,0,X'D9000AAC',0                                      
         SPACE                                                                  
         MVC   DMCB+16(4),0(R1)    EXTERNAL ROUTINE, MISC SUBROUTINES           
*                                                                               
* ACTUAL SECURITY SOURCE PROGRAM IS CALLED RECKSEC                              
*                                                                               
         LA    R2,RRGGRPH                                                       
         GOTOX (RFCKSEC,DMCB+16),DMCB,(R3),CONHEADH,,RFBLOCK                    
         BNE   SECERR                                                           
         SPACE                                                                  
         MVI   BYTE,0                                                           
         B     CKSEC200                                                         
         SPACE                                                                  
         CLC   RRGGRP,SBGROUP                                                   
         BE    CKSEC100                                                         
         MVC   WORK(L'RRGGRP),RRGGRP                                            
         OC    WORK(L'RRGGRP),BLANKS                                            
         CLC   WORK(L'RRGGRP),SBGROUP                                           
         BE    CKSEC100                                                         
         MVI   BYTE,1                                                           
         MVC   RRGGRP,SBGROUP                                                   
         OI    RRGGRPH+6,X'80'                                                  
         SPACE                                                                  
CKSEC100 DS   0H                                                                
         CLC   RRGOFF,SBOFFICE                                                  
         BE    CKSEC120                                                         
         MVC   WORK(L'RRGOFF),RRGOFF                                            
         OC    WORK(L'RRGOFF),BLANKS                                            
         CLC   WORK(L'RRGOFF),SBOFFICE                                          
         BE    CKSEC120                                                         
         SPACE                                                                  
         MVI   BYTE,1                                                           
         MVC   RRGOFF,SBOFFICE                                                  
         OI    RRGOFFH+6,X'80'                                                  
         SPACE                                                                  
CKSEC120 DS   0H                                                                
         CLC   QSTA(4),SBSTATN                                                  
         BNE   CKSEC130                                                         
         CLC   QSTA+5(1),SBSTATN+4                                              
         BE    CKSEC140                                                         
         SPACE                                                                  
CKSEC130 DS   0H                                                                
         MVI   BYTE,1                                                           
         XC    RRGSTA,RRGSTA                                                    
         MVC   RRGSTA(4),SBSTATN                                                
         MVI   RRGSTA+4,C'-'                                                    
         MVC   RRGSTA+5(1),SBSTATN+4                                            
         OI    RRGSTAH+6,X'80'                                                  
         SPACE                                                                  
CKSEC140 DS   0H                                                                
         CLC   RRGOWN,SBOWNER                                                   
         BE    CKSEC160                                                         
         MVC   WORK(L'RRGOWN),RRGOWN                                            
         OC    WORK(L'RRGOWN),BLANKS                                            
         CLC   WORK(L'RRGOWN),SBOWNER                                           
         BE    CKSEC160                                                         
         SPACE                                                                  
         MVI   BYTE,1                                                           
         MVC   RRGOWN,SBOWNER                                                   
         OI    RRGOWNH+6,X'80'                                                  
         SPACE                                                                  
CKSEC160 DS   0H                                                                
         CLC   RRGMKT,SBMARKET                                                  
         BE    CKSEC180                                                         
         MVC   WORK(L'RRGMKT),RRGMKT                                            
         OC    WORK(L'RRGMKT),BLANKS                                            
         CLC   WORK(L'RRGMKT),SBMARKET                                          
         BE    CKSEC180                                                         
         SPACE                                                                  
         MVI   BYTE,1                                                           
         MVC   RRGMKT,SBMARKET                                                  
         OI    RRGMKTH+6,X'80'                                                  
         SPACE                                                                  
CKSEC180 DS   0H                                                                
         CLC   SBREGION,RRGREG                                                  
         BE    CKSEC200                                                         
         MVC   WORK(L'RRGREG),RRGREG                                            
         OC    WORK(L'RRGREG),BLANKS                                            
         CLC   SBREGION,WORK                                                    
         BE    CKSEC200                                                         
         SPACE                                                                  
         MVI   BYTE,1                                                           
         MVC   RRGREG,SBREGION                                                  
         OI    RRGREGH+6,X'80'                                                  
         SPACE                                                                  
CKSEC200 DS   0H                                                                
*        CLC   RRGSAL,SBSALES      FUTURE                                       
         SPACE                                                                  
         TM    DMCB+4,X'20'        WAS STATION A SECURITY ITEM                  
         BZ    CKSEC500                                                         
         CLI   QLIST,QLSTA         LISTING BY STATION                           
         BE    STALSTER                                                         
         SPACE                                                                  
CKSEC500 TM    DMCB,X'60'          THIS SPECIAL CASE OWNER CODE?                
         BNO   CKSECX                                                           
         SPACE                                                                  
         MVI   CKSECOWN,1                                                       
         SPACE                                                                  
CKSECX   DS    0H                                                               
         XIT1                                                                   
STALSTER LA    R2,RRGOPTH                                                       
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'STALSTMS),STALSTMS                                     
SECERR   DS    0H                                                               
         GOTO1 ERREX2                                                           
STALSTMS DC    C'* ERROR * CAN''T LIST STATIONS WITH SECURITY ON STATION        
               '                                                                
         DROP  RB,RC                                                            
         EJECT                                                                  
* CHECK TO SEE IF OWNER HAS A STATION IN THIS MARKET *                          
         SPACE                                                                  
CKOWNMKT NMOD1 0,**+COM**                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         SPACE                                                                  
         BAS   RE,SETREPFL         SET UP REP FILE VALUES                       
         SPACE                                                                  
         SR    R0,R0                                                            
         SPACE                                                                  
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING RSTAD,R4                                                         
         MVI   RST6KTYP,X'83'                                                   
         MVI   RST6KSTP,X'04'                                                   
         MVC   RST6KREP,AGENCY                                                  
         MVC   RST6KOWN,QOWNER                                                  
         MVC   RST6KMKT,0(R3)                                                   
         DROP  R4                                                               
         SPACE                                                                  
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(22),KEYSAVE                                                  
         BE    CKOWN200                                                         
         SPACE                                                                  
         XC    KEY,KEY             SEE IF THIS IS A MASTER REP                  
         LA    R4,KEY                                                           
         USING RREPREC,R4                                                       
         MVI   RREPKTYP,01                                                      
         MVC   RREPKREP,AGENCY                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,02                                                        
         BAS   RE,GETEL                                                         
         BNE   CKOWN180                                                         
         SPACE                                                                  
         ZIC   R5,2(R6)                                                         
         LA    R2,10(,R6)                                                       
CKOWN100 DS    0H                                                               
         XC    KEY,KEY                                                          
         USING RSTAD,R4                                                         
         MVI   RST6KTYP,X'83'                                                   
         MVI   RST6KSTP,X'04'                                                   
         MVC   RST6KREP,0(R2)                                                   
         MVC   RST6KOWN,QOWNER                                                  
         MVC   RST6KMKT,0(R3)                                                   
         DROP  R4                                                               
         SPACE                                                                  
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(22),KEYSAVE                                                  
         BE    CKOWN200                                                         
         SPACE                                                                  
         LA    R2,2(,R2)                                                        
         BCT   R5,CKOWN100                                                      
         SPACE                                                                  
CKOWN180 DS   0H                                                                
         LA    R0,1                                                             
         SPACE                                                                  
CKOWN200 DS   0H                                                                
         BAS   RE,RSETRRGO         SWITCH BACK TO RRGON FILE VALUES             
         SPACE                                                                  
         LTR   R0,R0               SET COND CODE FOR RETURN                     
         XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* CHECK FILTERS BY READING THE STATION REC & COMPARING TO IT *                  
         SPACE                                                                  
CHKGRP   NMOD1 0,**+CHK**                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         GOTO1 =A(GETSTA),RR=RELO                                               
         BNE   CHKGRPX                                                          
         L     R4,AIO2                                                          
         USING RSTAD,R4                                                         
         CLI   QGROUP,C'*'         IF SET, MORE CHECKING                        
         BE    CHKGRP20                                                         
         SPACE                                                                  
         CLI   QGROUP+1,C' '                                                    
         BNH   CHKGRP10                                                         
         SPACE                                                                  
         CLC   RSTAGRUP,QGROUP     CHECK THE STATION GROUP                      
         B     CHKGRPX                                                          
         SPACE                                                                  
CHKGRP10 CLC   RSTAGRUP(1),QGROUP  CHECK THE STATION GROUP                      
         B     CHKGRPX                                                          
         SPACE                                                                  
CHKGRP20 LA    R0,3                                                             
         LA    RE,SET1CDE                                                       
         LR    RF,R9                                                            
         AHI   RF,SET1TAB-SYSD                                                  
CHKGRP30 CLI   0(RE),QLGRP       THIS THE CODE                                  
         BE    CHKGRP34                                                         
         LA    RE,L'SETCDE(,RE)    NEXT CODE                                    
         LA    RF,L'SET1TAB(,RF)   NEXT TABLE                                   
         BCT   R0,CHKGRP30                                                      
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
         SPACE                                                                  
CHKGRP34 ZIC   R1,1(RE)            GET THE LENGTH                               
         BCTR  R1,0                                                             
         SPACE                                                                  
CHKGRP40 CLC   RSTAGRUP,0(RF)      CHECK THE STATION GROUP                      
         BE    CHKGRP50                                                         
         SPACE                                                                  
         LA    RF,0(R1,RF)                                                      
         OC    0(2,RF),0(RF)       ANOTHER ENTRY                                
         BNZ   CHKGRP40                                                         
         TM    SET1FLG-SET1CDE(RE),X'08'  EXCLUDE SET                           
         BO    CHKGRPEQ                                                         
         SPACE                                                                  
CHKGRP36 LTR   RB,RB               SET COND CODE                                
         B     CHKGRPX                                                          
         SPACE                                                                  
CHKGRP50 TM    SET1FLG-SET1CDE(RE),X'08'  EXCLUDE SET                           
         BO    CHKGRP36                                                         
         SPACE                                                                  
CHKGRPEQ CR    RB,RB                                                            
         SPACE                                                                  
CHKGRPX  XIT1                                                                   
         DROP  R4,RB,RC                                                         
         EJECT                                                                  
* FILL IN INITIAL KEY FROM SET TABLE                                            
         SPACE                                                                  
SETK     NMOD1 0,**+SETK*                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         LA    R0,3                                                             
         LA    RE,SET1CDE                                                       
         LR    RF,R9                                                            
         AHI   RF,SET1TAB-SYSD                                                  
SETK20   CLC   0(1,R4),0(RE)       THIS THE CODE                                
         BE    SETK40                                                           
         LA    RE,L'SETCDE(,RE)    NEXT CODE                                    
         LA    RF,L'SET1TAB(,RF)   NEXT TABLE                                   
         BCT   R0,SETK20                                                        
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
SETK40   ZIC   R1,1(RE)            GET CODE LEN                                 
         BCTR  R1,0                                                             
         SPACE                                                                  
         XC    0(8,R5),0(R5)       ZERO OUT KEY FOR POSSIBLE EXCLUDE            
         SPACE                                                                  
         TM    SET1FLG-SET1CDE(RE),X'08'  EXCLUDE SET                           
         BO    SETK50               YES, SET FIELD TO 1                         
         SPACE                                                                  
         EX    R1,SETKMVC                                                       
         CLI   0(R4),QLGRP         THIS G/S                                     
         BNE   SETK60                                                           
         CLI   1(R5),C' '          THIS A BLANK                                 
         BH    SETKX                                                            
         MVI   1(R5),0                                                          
         MVI   0(R4),QLGRGRP                                                    
         B     SETKX                                                            
         SPACE                                                                  
SETK50   MVI   7(R5),01            BYPASS NULLS RECS                            
         B     SETKX                                                            
         SPACE                                                                  
SETK60   CLI   0(R4),QLSTA         THIS STATION                                 
         BNE   SETK70                                                           
         MVI   6(R5),0             FORCE SHORT STATIONS TO NULL                 
         LA    R1,3(,R5)                                                        
         CLI   3(R5),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(RF)                                                    
         CLI   1(R1),C' '                                                       
         BH    SETK64                                                           
         MVC   1(2,R1),=C'TV'                                                   
         B     SETKX                                                            
SETK64   MVI   2(R1),C'M'                                                       
         B     SETKX                                                            
         SPACE                                                                  
SETK70   CLI   0(R4),QLCON         THIS CONTRACT TYPE                           
         BNE   SETK80                                                           
         OI    1(R5),X'40'         SET ON SPACE IN SECOND BYTE                  
         B     SETKX                                                            
         SPACE                                                                  
SETK80   DS    0H                                                               
         TM    DISPFLAG,OPCONDCT   LOOKING FOR HARD CODE CON/DCT                
         BZ    SETKX                NO                                          
         CLI   0(R4),QLDCT         THIS DEV CON TYP                             
         BNE   SETKX                                                            
         XC    0(8,R5),0(R5)                                                    
         SPACE                                                                  
SETKX    OI    0(R4),SET           SET UP FOR TABLE LOOK-UP                     
         XIT1                                                                   
         SPACE                                                                  
SETKMVC  MVC   0(0,R5),0(RF)                                                    
         DROP  RB,RC                                                            
         EJECT                                                                  
* VALIDATE OPTIONS                                                              
         SPACE                                                                  
         DS    0F                                                               
VOPT     NMOD1 0,**VOPT**                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         MVI   QLIST,0                                                          
         MVI   QLOPT,0                                                          
         MVI   BYTE,0                                                           
         MVI   DISPFLAG,0                                                       
         MVI   TYPEKEY,0                                                        
         MVI   TOPNCT,0                                                         
         CLI   5(R2),0                                                          
         BE    MISSERRV                                                         
*                                                                               
*   OPTION SHOULD COME IN AS:  KEY=OPTION, WHERE                                
*        KEY=     IS L(I(S(T)))=                                                
*        OPTION   IS ANY VALID LIST ENTRY                                       
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(5,BLOCK),0                                    
         SR    R5,R5                                                            
         ICM   R5,1,4(R1)                                                       
         BZ    VOPT0240                                                         
         SR    R3,R3                                                            
         LA    R4,BLOCK            SET TO A(SCANNER OUTPUT BLOCK)               
*                                                                               
VOPT0100 DS   0H                                                                
         MVI   ERROPT,C'N'                                                      
         CLI   0(R4),0             ANYTHING ON LINE?                            
         BE    VKOPTERR            NO  - ERROR                                  
         ZIC   R3,0(R4)            CHECK VALUE OF KEYWORD                       
         BCTR  R3,0                                                             
         SPACE                                                                  
         CLI   1(R4),0             ANYTHING IN SECOND HALF?                     
         BNE   VOPT0110            YES - CONTINUE                               
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   VOPT0110            NO  - 'REFRESH' NOT ALLOWED                  
         CLI   0(R4),7             NO  - FIRST HALF 7 CHARS LONG?               
         BNE   VOPT0110            NO  - NOT 'REFRESH' - ERROR                  
         CLC   =C'REFRESH',12(R4)  IS IT 'REFRESH'?                             
*                                  YES - ACCEPT AND SKIP OVER IT                
*                                     FLAG WAS SET EARLIER                      
         BE    VOPT0230            LOOP TO NEXT OPTION                          
*                                                                               
VOPT0110 DS   0H                                                                
         CLI   0(R4),2             THIS SHORT VERSION                           
         BNE   VOPT0120                                                         
         SPACE                                                                  
         EX    R3,COMPBK           BOOKED                                       
         BE    VOPT0210                                                         
         EX    R3,COMPCN           CONFIRMED                                    
         BE    VOPT0214                                                         
         EX    R3,COMPDR           DIRECT                                       
         BE    VOPT0216                                                         
         EX    R3,COMPUN           UNCONFIRMED                                  
         BE    VOPT0218                                                         
         EX    R3,COMPCB           COMPANY BUDGET                               
         BE    VOPT0220                                                         
         EX    R3,COMPSB           STATION BUDGET                               
         BE    VOPT0224                                                         
         EX    R3,COMPSP           SP (SALESPERSON)                             
         BE    VOPT0290                                                         
         SPACE                                                                  
* NOW LOOK AT LONG VERSIONS                                                     
         SPACE                                                                  
VOPT0120 DS   0H                                                                
         EX    R3,COMPBOOK         BOOKED                                       
         BE    VOPT0210                                                         
         SPACE                                                                  
         EX    R3,COMPCONF         CONFIRMED                                    
         BE    VOPT0214                                                         
         SPACE                                                                  
         EX    R3,COMPDIR          DIRECT                                       
         BE    VOPT0216                                                         
         SPACE                                                                  
         EX    R3,COMPUNCN         UNCONFIRMED                                  
         BE    VOPT0218                                                         
         SPACE                                                                  
         EX    R3,COMPSAL          SALESPERSON                                  
         BE    VOPT0290                                                         
         SPACE                                                                  
         EX    R3,COMPTRC          TRACE                                        
         BNE   VOPT0170             NO                                          
         SPACE                                                                  
         CLI   OFFLINE,C'Y'        MUST BE OFFLINE                              
         BNE   VKOPTERR             NO  - ERROR                                 
         OI    DISPFLAG,OPTRACE                                                 
         SPACE                                                                  
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         MVC   P,SPACES                                                         
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         B     VOPT0230                                                         
         DROP  R3                                                               
         SPACE                                                                  
VOPT0170 LR    R1,R3               SAVE LENGTH                                  
         CLI   0(R4),4             REQUEST FOR TOP N 4 CHAR MAX                 
         BH    VOPT0180                                                         
         BL    VOPT0174                                                         
         BCTR  R3,0                                                             
         SPACE                                                                  
VOPT0174 EX    R3,COMPTOP          REQUEST FOR TOP N                            
         LR    R3,R1                                                            
         BNE   VOPT0180                                                         
         SPACE                                                                  
         CLI   1(R4),0             MUST BE A SECOND ENTRY                       
         BE    VKTOPERR                                                         
         TM    3(R4),X'80'         MUST BE NUMERIC                              
         BZ    VKTOPERR                                                         
         CLC   8(4,R4),=F'100'                                                  
         BH    VKTOPERR                                                         
         MVC   TOPNCT,11(R4)       SAVE REQUESTED COUNT                         
         MVI   TOPTYPE,C'C'        STD IS CURR BILL                             
         CLI   0(R4),4             REQUEST FOR TOP N 4 CHAR MAX                 
         BL    VOPT0176                                                         
         SPACE                                                                  
         CLI   15(R4),C'C'         CURR                                         
         BE    VOPT0176                                                         
         SPACE                                                                  
         MVI   TOPTYPE,C'P'        PRIOR BILL                                   
         CLI   15(R4),C'P'                                                      
         BE    VOPT0176                                                         
         SPACE                                                                  
         MVI   TOPTYPE,C'F'        FINAL                                        
         CLI   15(R4),C'F'                                                      
         BE    VOPT0176                                                         
         SPACE                                                                  
         MVI   TOPTYPE,C'B'        BUDGET                                       
         CLI   15(R4),C'B'                                                      
         BNE   VKTOPERR                                                         
         SPACE                                                                  
* TSAR CLEAR HERE                                                               
         SPACE                                                                  
VOPT0176 XC    TSARBLK(TSARDL2),TSARBLK                                         
         LA    R6,TSARBLK                                                       
         USING TSARD,R6                                                         
         MVC   TSACOM,ACOMFACS                                                  
         MVI   TSKEYL,4                                                         
         MVI   TSRECL+1,53                                                      
         MVI   TSPAGL,3         LOW TEMPSTOR PAGE TO USE                        
         MVI   TSPAGN,1         NUMBER OF TEMPSTR PAGES TO BE USED              
         SPACE                                                                  
* TSAR CALLOV HERE                                                              
         SPACE                                                                  
         GOTO1 CALLOV,DMCB,0,(C'R',X'00000A5D')                                 
         ICM   RF,15,0(R1)                                                      
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    RF,ATSAR                                                         
         SPACE                                                                  
         MVI   TSACTN,TSAINI       SET 1ST TSAR FOR INIT                        
         GOTO1 ATSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         BE    VOPT0230                                                         
         DC    H'0'                                                             
         DROP  R6                                                               
         SPACE                                                                  
VOPT0180 EX    R3,COMPLIST         LIST?'                                       
         BNE   VKOPTERR             NO  - ERROR                                 
*                                                                               
         CLI   ACTNUM,ACTLIST      ONLY VALID FOR ACTION LIST                   
         BNE   VKOPTERR            ACTION CODE NOT = LIST                       
         CLI   QLIST,0                                                          
         BNE   VKOPTERR                                                         
         CLI   1(R4),8             CHECK SIZE OF OPTION                         
         BH    VKOPTERR            TOO BIG - ERROR                              
         IC    R3,1(R4)            FIND OPTION IN LIST                          
         BCTR  R3,0                                                             
         L     R1,=A(LISTOPTS)     A(LIST OF OPTIONS)                           
         A     R1,RELO                                                          
*                                                                               
VOPT0190 CLI   0(R1),0             END OF LIST?                                 
         BE    VKLSTERR            YES - NOT FOUND - ERROR                      
         CLC   1(1,R4),12(R1)      CHECK MINIMUM COMPARE LEN (LONG)             
         BL    VOPT0194                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),22(R4)      COMPARE FOR LONG NAME                        
         BE    VOPT0200            FOUND - PROCESS IT                           
*                                                                               
VOPT0194 CLI   1(R4),4                                                          
         BH    VOPT0196                                                         
         CLI   13(R1),0                                                         
         BE    VOPT0196                                                         
         CLC   1(1,R4),13(R1)      CHECK MINIMUM COMPARE LEN (SHORT)            
         BL    VOPT0196                                                         
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R1),22(R4)      COMPARE FOR SHORT NAME                       
         BE    VOPT0200            FOUND - PROCESS IT                           
*                                                                               
VOPT0196 LA    R1,L'LISTOPTS(R1)   BUMP TO NEXT ENTRY                           
         B     VOPT0190            GO BACK AND CHECK                            
*                                                                               
VOPT0200 MVC   QLIST,14(R1)        SET VALID OPTION ACTION VALUE                
         OC    QLOPT,15(R1)        SET SPECL OPTION ACTION VALUE                
         MVC   SECBRK,16(R1)       SET SECURITY BIT ON                          
         SPACE                                                                  
         CLI   QLIST,QLSTA         THIS L=STA                                   
         BNE   VOPT0230                                                         
         OC    QOWNER,QOWNER       FILTERING ON OWNER                           
         BZ    VOPT0230                                                         
         CLC   AGENCY,=C'K3'       THIS KRGNY MASTER REP                        
         BE    VOPT0204                                                         
         CLC   AGENCY,=C'MR'       THIS KTV MASTER REP                          
         BE    VOPT0204                                                         
         CLC   AGENCY,=C'IR'       THIS IRNY MASTER REP                         
         BNE   VOPT0230                                                         
         SPACE                                                                  
*        INVALID RRGON COMBINATION FOR MASTER REP                               
         SPACE                                                                  
VOPT0204 XC    CONHEAD,CONHEAD                                                  
         LA    R2,RRGOWNH                                                       
         MVC   GERROR,=Y(INVMREP)                                               
         GOTO1 RRGERR                                                           
         SPACE                                                                  
VOPT0210 OI    DISPFLAG,DISPBOOK                                                
         B     VOPT0230                                                         
*                                                                               
VOPT0214 TM    DISPFLAG,DISPDIR+DISPUNCN                                        
         BNZ   MULTIERR                                                         
         OI    DISPFLAG,DISPCONF                                                
         MVI   TYPEKEY,C'C'                                                     
         B     VOPT0230                                                         
*                                                                               
VOPT0216 TM    DISPFLAG,DISPCONF+DISPUNCN                                       
         BNZ   MULTIERR                                                         
         MVI   TYPEKEY,C'D'                                                     
         OI    DISPFLAG,DISPDIR                                                 
         B     VOPT0230                                                         
*                                                                               
VOPT0218 TM    DISPFLAG,DISPCONF+DISPDIR                                        
         BNZ   MULTIERR                                                         
         MVI   TYPEKEY,C'U'                                                     
         OI    DISPFLAG,DISPUNCN                                                
         B     VOPT0230                                                         
*                                                                               
VOPT0220 LR    RF,RA               SET PROFILE TO COMPANY BUDGET                
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         OI    SVPGPBIT,X'10'      SET FOURTH BIT ON                            
         MVI   BYTE,1              SET FLAG FOR OPTION ENTERED                  
         B     VOPT0230                                                         
         DROP  RF                                                               
         SPACE                                                                  
VOPT0224 LR    RF,RA               SET PROFILE TO STATION BUDGET                
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         NI    SVPGPBIT,X'FF'-X'10'  SET FOURTH BIT OFF                         
         MVI   BYTE,2              SET FLAG FOR OPTION ENTERED                  
         DROP  RF                                                               
*                                                                               
VOPT0230 LA    R4,32(R4)                                                        
         BCT   R5,VOPT0100         NEXT OPTION FIELD                            
         MVI   ERROPT,C'N'                                                      
         SPACE                                                                  
VOPT0240 CLI   QLIST,0                                                          
         BE    VKOPTERR             - ERROR                                     
         SPACE                                                                  
         CLI   QLIST,QLSTA         THIS LIST BY STA                             
         BNE   VOPT0250                                                         
         TM    QLOPT,QLCOMBO       AND LIST=COMBO                               
         BZ    VOPT0250                                                         
         CLI   TOPNCT,0                                                         
         BNE   COMTOPER                                                         
*                                                                               
VOPT0250 BAS   RE,RSETRRGO         SWITCH BACK TO RRGON FILE VALUES             
*                                                                               
         XC    KEY,KEY             READ RRGON HEADER RECORD                     
         LA    R7,KEY                                                           
         USING RORECD,R7                                                        
         MVC   ROKREP,AGENCY                                                    
         MVI   ROKHD1+1,X'01'                                                   
         GOTO1 HIGH                                                             
         CLC   ROKEY,KEYSAVE                                                    
         BNE   VONODATA                                                         
         SPACE                                                                  
         L     R7,AIO                                                           
         SPACE                                                                  
* ALWAYS USE LAST YEAR CURRENT PACING *                                         
         SPACE                                                                  
         CLC   RODHD1ST(1),QSTART  SET ON PACING OPTION FOR PREV YR             
         BE    VOPT0254             BUT NOT CURRENT YEAR                        
         SPACE                                                                  
         LR    RF,RA               CHECK PROFILE: SUPPRESS BUDGET?              
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         OI    SVPGPBIT,X'08'      USE PRIOR YR BILLING-CURR AS/AT DATE         
         DROP  RF                                                               
         SPACE                                                                  
VOPT0254 DS   0H                                                                
         MVC   SVTYPES,RODHDOPT    SAVE DOLLAR TYPES AVAILABLE                  
         SPACE                                                                  
         LA    R0,L'SVTYPES                                                     
         LA    R1,SVTYPES                                                       
         CLI   TYPEKEY,0           WAS ANY KEY REQUESTED                        
         BNE   VOPT0270             YES                                         
         SPACE                                                                  
VOPT0260 CLI   0(R1),0             THIS A VALID TYPE                            
         BNE   VOPT0264             YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,VOPT0260                                                      
         DC    H'0'                                                             
VOPT0264 MVC   TYPEKEY,0(R1)       SAVE WHATEVER IT IS                          
         CLI   TYPEKEY,C'A'        ALL DOLLARS BECOMES NULL                     
         BNE   VOPT0280             NO                                          
         MVI   TYPEKEY,0                                                        
         B     VOPT0280                                                         
         SPACE                                                                  
VOPT0270 CLC   TYPEKEY,0(R1)       THIS A VALID TYPE                            
         BE    VOPT0280             YES                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,VOPT0270                                                      
         B     VKTYPERR                                                         
         SPACE                                                                  
VOPT0280 GOTO1 DATCON,DMCB,(3,RODHD1DT),(0,PARAS)   RRGON DATE                  
         GOTO1 (RF),(R1),(5,0),(0,PARAS+6)        TODAY                         
         GOTO1 GETDAY,(R1),PARAS+6,PARAS+12                                     
         CLC   PARAS+12(3),BLANKS                                               
         BNE   *+6                                                              
         DC    H'0'                                                             
         SR    RE,RE               DETERMINE LAST BUSINESS DAY                  
         BCTR  RE,0                                                             
         CLI   DMCB,7                                                           
         BNE   *+6                                                              
         BCTR  RE,0                SUNDAY: SUBTRACT 2 DAYS                      
         CLI   DMCB,1                                                           
         BNE   *+6                                                              
         BCTR  RE,0                MONDAY: SUBTRACT 2 DAYS                      
         ST    RE,DMCB+8                                                        
         GOTO1 ADDAY,(R1),PARAS+6,PARAS+12                                      
         MVI   ERREX2SW,C'N'                                                    
         CLC   PARAS(6),PARAS+12   COMPARE RRGON DATE TO BUSINESS DATE          
         BNL   VOPT0300                                                         
         MVI   ERREX2SW,C'Y'       OUTPUT A WARNING WHEN TIME TO EXIT           
         GOTO1 DATCON,(R1),(0,PARAS),(8,WARNDATE)                               
         SPACE                                                                  
         XC    CONHEAD,CONHEAD                                                  
         L     R1,=A(WARNING)                                                   
         A     R1,RELO                                                          
         MVC   CONHEAD(L'WARNING),0(R1)                                         
         MVC   CONHEAD+L'WARNING(8),WARNDATE                                    
         OI    GENSTAT2,USMYOK                                                  
         B     VOPT0300                                                         
         SPACE                                                                  
VOPT0290 DS    0H                                                               
         IC    R0,BYTE             SAVE POSSIBLE COMPANY BUDGET                 
         MVC   BYTE,1(R4)          SAVE LENGTH                                  
         MVC   QSAL,22(R4)                                                      
         CLI   22(R4),C'*'         IS THIS A SET?                               
         BNE   VOPT0294                                                         
         MVC   QSALSET,22+1(R4)    SAVE THE SET                                 
         OC    QSALSET,BLANKS                                                   
         MVC   WORK+1(L'QSALSET),QSALSET                                        
         SPACE                                                                  
VOPT0294 DS    0H                                                               
         OC    QSAL,BLANKS                                                      
         SPACE                                                                  
* DOES NOT GET COMPANY BUDGET                                                   
         SPACE                                                                  
         MVI   ROUNDSW,C'N'        NO  - LIST - DON'T ROUND                     
         SPACE                                                                  
         GOTO1 VALISAL                                                          
         SPACE                                                                  
         STC   R0,BYTE             RESTORE POSSIBLE COMPANY BUDGET              
         OC    AFSTFLD,AFSTFLD     SET A(FIRST VALID FIELD)                     
         BNZ   VOPT0230                                                         
         ST    R2,AFSTFLD                                                       
         B     VOPT0230                                                         
         SPACE                                                                  
VOPT0300 BAS   RE,SETREPFL         SET UP REP FILE VALUES                       
         SPACE                                                                  
         CLI   QLIST,QLOFF         COMPANY BUDGET OKAY                          
         BE    VOPTX                YES                                         
         SPACE                                                                  
         CLI   BYTE,0              WAS OPTION ENTERED                           
         BNE   VOPTCBER             YES                                         
         SPACE                                                                  
         CLI   QLIST,QLGRP         LIST = GS CODE OKAY                          
         BE    VOPTX                YES                                         
         SPACE                                                                  
         CLI   QLIST,QLMON         L=MON OKAY                                   
         BE    VOPTX                YES                                         
         MVI   COMPBUDG,C'N'                                                    
         SPACE                                                                  
VOPTX    XIT1                                                                   
         SPACE                                                                  
         DROP  R7                                                               
*                                                                               
COMPLIST CLC   12(0,R4),=C'LIST'   ** EXECUTED INSTRUCTIONS                     
COMPTOP  CLC   12(0,R4),=C'TOP'    ** EXECUTED INSTRUCTIONS                     
COMPBK   CLC   12(0,R4),=C'BK'                                                  
COMPCN   CLC   12(0,R4),=C'CN'                                                  
COMPUN   CLC   12(0,R4),=C'UN'                                                  
COMPDR   CLC   12(0,R4),=C'DR'                                                  
COMPCB   CLC   12(0,R4),=C'CB'                                                  
COMPSB   CLC   12(0,R4),=C'SB'                                                  
COMPBOOK CLC   12(0,R4),=C'BOOKED'                                              
COMPCONF CLC   12(0,R4),=C'CONFIRMED'                                           
COMPUNCN CLC   12(0,R4),=C'UNCONFIRMED'                                         
COMPDIR  CLC   12(0,R4),=C'DIRECT'                                              
COMPTRC  CLC   12(0,R4),=C'TRACE'                                               
COMPPAC  CLC   12(0,R4),=C'PACE '                                               
COMPSP   CLC   12(0,R4),=C'SP'                                                  
COMPSAL  CLC   12(0,R4),=C'SALESPERSON'                                         
*                                                                               
*                                                                               
VKTYPERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'VKTYPERM),VKTYPERM                                     
         CLI   TYPEKEY,C'A'                                                     
         BE    VOPTERX2                                                         
         MVC   CONHEAD+10(3),=C'CON'                                            
         CLI   TYPEKEY,C'C'                                                     
         BE    VOPTERX2                                                         
         MVC   CONHEAD+10(3),=C'DIR'                                            
         CLI   TYPEKEY,C'D'                                                     
         BE    VOPTERX2                                                         
         DC    H'0'                                                             
         SPACE                                                                  
VKTOPERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'TOPNMS),TOPNMS                                         
         B     VOPTERX2                                                         
         SPACE                                                                  
COMTOPER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'COMTOPMS),COMTOPMS                                     
         B     VOPTERX2                                                         
         SPACE                                                                  
MULTIERR XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MULTIMS),MULTIMS                                       
         B     VOPTERX2                                                         
         SPACE                                                                  
VOPTCBER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'CMPBDMS),CMPBDMS                                       
         CLI   BYTE,1                                                           
         BE    VOPTERX2                                                         
         MVI   CONHEAD+10,C'S'                                                  
         SPACE                                                                  
VOPTERX2 GOTO1 ERREX2                                                           
         SPACE                                                                  
VONODATA LA    R2,CONRECH                                                       
         MVI   ERROR,NODATA                                                     
         MVI   QREFRESH,C'N'       SET OFF 'REFRESH' REQUEST                    
         MVI   LISTEND,C'N'                                                     
         B     VOPTERX                                                          
         SPACE                                                                  
MISSERRV MVI   ERROR,MISSING                                                    
         B     VOPTERX                                                          
         SPACE                                                                  
VKOPTERR LA    R2,RRGOPTH                                                       
         MVI   ERROR,INVALID                                                    
         SPACE                                                                  
VOPTERX  GOTO1 ERREX                                                            
         SPACE                                                                  
VKTYPERM DC    C'* ERROR * ALL OPTION NOT VALID FOR THIS FILE *'                
MULTIMS  DC    C'* ERROR * CAN ONLY USE 1 OF CONFIRMED/DIRECT *'                
TOPNMS   DC    C'* ERROR * ENTER TOP=N WHERE N IS 1 - 100 *'                    
COMTOPMS DC    C'* ERROR * CAN''T USE TOP WITH COMBO *'                         
CMPBDMS  DC    C'* ERROR * CB OPTION VALID ONLY WITH L=OFFICE *'                
         DROP  RB,RC                                                            
*                                                                               
         EJECT                                                                  
* ROUTINE TO CHECK IF -CM COULD BE COMBO STATION                                
* IF SO, BUILD LIST OF COMBINED STATIONS                                        
*                                                                               
         SPACE                                                                  
         DS    0F                                                               
GETCOMBO NMOD1 0,**GTCO**                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         SPACE                                                                  
         CLI   FULLSTER,C'Y'       THIS FULL STEREO                             
         BE    *+12                 YES                                         
         SPACE                                                                  
         CLI   QGROUP,0            WAS GROUP ENTERED                            
         BE    MISSGRP              YES                                         
         SPACE                                                                  
         CLI   QGROUP,0            WAS GROUP ENTERED                            
         BNE   *+8                  NO                                          
         MVI   QGROUP,C'R'         FAKE IT FOR COMBOS                           
         SPACE                                                                  
         LA    R1,COMBREPS         CHECK THE REP                                
*                                                                               
GETCOMB2 CLI   0(R1),0                                                          
         BE    GETCOMBX                                                         
         CLC   AGENCY,0(R1)                                                     
         BE    *+12                                                             
         LA    R1,2(R1)                                                         
         B     GETCOMB2                                                         
         MVC   KEY,SVSTAKEY        READ STATION RECORD                          
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         USING RSTAD,R4                                                         
         LA    R3,RSTAELEM         LOOK FOR COMBINED STATION ELEMENTS           
         SR    RE,RE                                                            
         LA    R1,COMBOSTA                                                      
         SR    R5,R5                                                            
         XC    COMBOSTA,COMBOSTA                                                
*                                                                               
GETCOMB4 CLI   0(R3),0                                                          
         BE    GETCOMB8                                                         
         CLI   0(R3),X'0A'                                                      
         BNE   GETCOMB6                                                         
         USING RSTACSEL,R3                                                      
         CLI   RSTACPRF,C'-'       STATION MINUS'D OUT?                         
         BE    GETCOMB6            YES - SKIP IT                                
         OC    COMBOSTA,COMBOSTA   FIRST STATION IS REQUESTED STATION           
         BNZ   *+18                                                             
         MVC   COMBOSTA,QSTA                                                    
         LA    R1,7(R1)                                                         
         LA    R5,1(R5)                                                         
         MVC   0(4,R1),RSTACS      FOUND-SAVE THE STATION                       
         LA    RF,4(R1)                                                         
         CLI   3(R1),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         MVI   6(R1),0                                                          
         MVC   0(3,RF),=C'-TV'                                                  
         CLI   RSTACS+4,C'T'                                                    
         BE    *+14                                                             
         MVC   1(1,RF),RSTACS+4                                                 
         MVI   2(RF),C'M'                                                       
         LA    R1,7(R1)                                                         
         LA    R5,1(R5)                                                         
*                                                                               
GETCOMB6 IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     GETCOMB4                                                         
*                                                                               
GETCOMB8 STC   R5,NCOMBOS          SAVE N'COMBO STATIONS                        
         CLI   NCOMBOS,CSTAMAX                                                  
         BNH   *+6                                                              
         DC    H'0'                BOMB IF TOO MANY                             
         SPACE                                                                  
* GET QSORT ADDRESS                                                             
         SPACE                                                                  
         MVC   DMCB+4(4),=X'D9000A50'  QSORT IS T00A50                          
         GOTO1 CALLOV,DMCB                                                      
         ICM   RF,15,DMCB                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 (RF),DMCB,COMBOSTA,(R5),7,7,0                                    
*                                                                               
GETCOMBX XIT1                                                                   
         SPACE                                                                  
MISSGRP  LA    R2,RRGGRPH          GROUP                                        
         MVI   ERROR,MISSING                                                    
         GOTO1 ERREX                                                            
         DROP  R3,R4,RB,RC                                                      
         SPACE                                                                  
COMBREPS DC    CL2'IR'             TABLE OF REPS THAT COULD HAVE                
         DC    CL2'TO'             COMBO STATIONS                               
         DC    CL2'D4'             NEW DARNY                                    
         DC    CL2'I1'                                                          
         DC    CL2'IB'             NEW ABC RADIO                                
         DC    CL2'NX'             NEW PUB RADIO                                
         DC    CL2'HN'                                                          
         DC    CL2'MG'                                                          
         DC    CL2'DI'                                                          
         DC    CL2'GP'                                                          
         DC    CL2'I2'                                                          
         DC    CL2'I8'                                                          
         DC    CL2'I9'                                                          
         DC    CL2'CN'                                                          
         DC    CL2'CM'                                                          
         DC    CL2'UO'             CUMULUS RADIO SALES                          
         DC    CL2'IF'             NEW INFINITY                                 
         DC    CL2'AQ'             NEW ALLIED RADIO PARTNERS                    
         DC    CL2'S1'             NEW SHAMROCK                                 
         DC    CL2'CM'             NEW CONCERT MUSIC                            
         DC    CL2'L7'             NEW CABALLERO TV                             
         DC    CL2'V5'             NATIONAL PUBLIC RADIO NPRANY                 
         DC    CL2'SJ'                                                          
* KATZ REPS ADDED                                                               
         DC    CL2'BF'                                                          
         DC    CL2'CR'                                                          
         DC    CL2'EA'                                                          
         DC    CL2'KF'                                                          
         DC    CL2'KU'                                                          
         DC    CL2'K4'                                                          
         DC    CL2'RS'                                                          
         DC    CL2'S3'                                                          
         DC    CL2'QD'             SPECTRUM SALES                               
         DC    CL2'NU'             CLEAR CHANNEL (KATZ RADIO)                   
         DC    CL2'G8'             INTERACTIVE   (KATZ RADIO)                   
*                                                                               
         DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*   'DISPLAY' ACTION AS A FUNCTION OF LIST                                      
*                                                                               
         DS    0F                                                               
LISTMONS NMOD1 0,**MONS**                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         SPACE                                                                  
         CLI   MODE,PRINTREP       OFFLINE?                                     
         BE    MONPRTER            YES                                          
         SPACE                                                                  
*                                                                               
         MVI   LISTEND,C'Y'        SET 'NO MORE DATA' IN                        
*                                                                               
         LA    R1,MONTOTS                                                       
         SR    R1,R9                                                            
         ST    R1,CDTBADR          SAVE FIRST ADDR IN CODE TABLE                
         MVI   CDTBADR,12          SET UP FOR 12 MONTHS                         
         SPACE                                                                  
*                                     ALL CASES FOR 'LISTMONS' RTN              
         LA    R2,RGESELH          CLEAR LIST SCREEN FOR MONTHS                 
         BAS   RE,CLRSCRN                                                       
         MVC   KEYSAVE,LMONKEY     RESET KEYSAVE                                
         MVC   KEY,LMONKEY         RESET KEY                                    
*                                                                               
         XC    TOTALS(24),TOTALS                                                
         LA    R1,MONTOTS                                                       
         LA    RF,12                                                            
         XC    4(MONTOTLN-4,R1),4(R1)                                           
         LA    R1,MONTOTLN(,R1)                                                 
         BCT   RF,*-10                                                          
         OI    DMINBTS,X'08'       PASS DELETED RRGON RECS                      
         NI    DMOUTBTS,X'FD'      IGNORE DELETED RRGON RECS                    
*                                                                               
         L     R7,AIO                                                           
         USING RORECD,R7                                                        
         MVI   FIRSTSTA,C'Y'                                                    
         MVI   LASTLN,C'N'                                                      
         SPACE                                                                  
         OC    QSTA,QSTA           IS STATION A FILTER                          
         BZ    LMON0010                                                         
         CLI   QSTA,C'*'           SET                                          
         BE    LMON0010             YES                                         
         SPACE                                                                  
         MVC   RGEHDG(4),STAMKT                                                 
         MVC   RGEHDG+6(L'EXMKTNAM),EXMKTNAM                                    
         OI    RGEHDGH+6,X'80'      FORCE TRANS                                 
         SPACE                                                                  
LMON0010 CLI   NCOMBOS,0           TEST COMBO STATION                           
         BE    LMON0044             NO                                          
         SPACE                                                                  
         L     R2,ACOMBOS          YES-R2=A(CURRENT STATION)                    
         ZIC   R3,NCOMBOS              R3=N'STATIONS                            
*                                                                               
LMON0020 XC    KEY,KEY             READ RECORDS FOR NEXT COMBO STATION          
         MVC   KEY(L'ROKEY),KEYSAVE                                             
         LA    R7,KEY                                                           
         XC    ROKDTLYM,ROKDTLYM                                                
         L     R1,STADISP                                                       
         LA    R1,KEY(R1)                                                       
         MVC   0(7,R1),0(R2)                                                    
         SPACE                                                                  
         CLI   SET1CDE,0           ANY SETS INVOLVED                            
         BE    LMON0030             NO                                          
         SPACE                                                                  
         LA    R2,KEY+ROKDTLTY-ROKEY  START OF DATA TYPES                       
         LA    R3,KEY+ROKDTLVL-ROKEY  START OF DATA VALUES                      
         LR    R0,R3                                                            
         SPACE                                                                  
LMON0024 CR    R1,R2               SEE IF AT STATION FIELD                      
         BL    LMON0026             PAST STATION                                
         LA    R2,1(,R2)                                                        
         LA    R3,8(,R3)                                                        
         B     LMON0024                                                         
         SPACE                                                                  
LMON0026 CR    R0,R2               SEE IF PAST DATA TYPES                       
         BNH   LMON0030                                                         
         CLI   0(R2),0             THIS VALID FILTER                            
         BE    LMON0030             NO, DONE                                    
         SPACE                                                                  
         GOTO1 =A(SETD),RR=RELO    SET DATA FIELD                               
         SPACE                                                                  
         LA    R2,1(,R2)                                                        
         LA    R3,8(,R3)                                                        
         B     LMON0026                                                         
         SPACE                                                                  
LMON0030 DS   0H                                                                
         GOTO1 HIGH                READ FIRST RRGON DETAIL RECORD               
         MVI   TRCTYPE,8                                                        
         GOTO1 =A(TRACE),RR=RELO                                                
         SPACE                                                                  
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE                                      
         BE    LMON0040                                                         
         SPACE                                                                  
         CLI   SET1CDE,0           IS THIS A SETS REQUEST?                      
         BE    LMON0240             NO, DONE SEARCH                             
         SPACE                                                                  
         BAS   RE,CSETM            CK IF DONE, OR NEXT SET                      
         BNE   LMON0240             YES, NEXT SET                               
         SPACE                                                                  
LMON0040 L     R7,AIO                                                           
*                                                                               
LMON0044 CLC   ROKDTLYM(1),QSTART  CHECK THE YEAR                               
         BNL   LMON0060                                                         
         SPACE                                                                  
LMON0046 MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         MVI   TRCTYPE,X'15'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE                                      
         BNE   LMON0240                                                         
         SPACE                                                                  
LMON0060 CLC   ROKDTLYM(1),QSTART                                               
         BNE   LMON0230                                                         
*                                                                               
         LA    R6,RODPER           POINT TO PERIOD FIGURES                      
*        CLI   QPY,C'Y'            TEST FOR YTD                                 
*        BNE   *+8                                                              
*        LA    R6,RODYTD           YES - POINT TO YTD FIGURES                   
*                                                                               
         CLI   ROKDTLYM+1,0        CHECK FOR PRIOR RECORD                       
         BE    LMON0046                                                         
*        BNE   LMON0120                                                         
         SPACE                                                                  
*        DC    H'0'                SHOULD NOT BE ANY 'PRIOR' RECORDS            
         SPACE                                                                  
*                                                                               
LMON0120 CLC   ROKDTLYM,QSTART     CHECK MON AGAINST START MON                  
         BL    LMON0140            LOW - STILL PRIOR                            
         CLC   ROKDTLYM,QEND       CHECK MON AGAINST END MON                    
         BH    LMON0230             HIGH - NO DATA                              
         B     LMON0180             ELSE OK                                     
*                                                                               
LMON0140 CLI   QPY,C'Y'            ONLY YTD NEEDS PRIOR                         
         BNE   LMON0160                                                         
         CLI   MONTOTS+1,0         TEST FIRST MONTH IS PRIOR                    
         BNE   LMON0160                                                         
         LA    R5,MONTOTS+4        YES-ADD TO EXISTING PRIOR                    
*                                                                               
         GOTO1 =A(DRTOTAL),RR=RELO TOTAL UP                                     
*                                                                               
LMON0160 MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                 READ NEXT                                    
         MVI   TRCTYPE,X'16'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE                                      
         BE    LMON0120                                                         
         B     LMON0240                                                         
         SPACE                                                                  
LMON0180 LA    R1,MONTOTS          MOVE DOLLARS TO THE CORRECT MONTH            
         LA    R0,12               LINE                                         
         CLC   ROKDTLYM,0(R1)                                                   
         BE    *+14                                                             
         LA    R1,MONTOTLN(,R1)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         CLI   FIRSTSTA,C'Y'                                                    
         BNE   LMON0200                                                         
*                                                                               
         LR    R0,R6                                                            
*                                                                               
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BE    *+8                                                              
         LA    R6,4(,R6)           POINT TO THIS YEAR                           
*                                                                               
         SR    RE,RE               CLR BOTH REGISTERS                           
         SR    RF,RF                                                            
         ICM   RF,15,0(R6)         PRIOR BILLING                                
         SPACE                                                                  
         ICM   RE,15,4(R1)                                                      
         AR    RE,RF                                                            
         STCM  RE,15,4(R1)                                                      
*                                                                               
         ICM   RF,15,4(R6)         CURRENT BILLING                              
*                                                                               
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   LMON0196             NO                                          
         LR    RE,RA                                                            
         AHI   RE,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT,X'08'      USE PRIOR YR BILLING-CURR AS/AT DATE         
         BZ    LMON0196             NO                                          
         DROP  RE                                                               
         SPACE                                                                  
         ICM   RF,15,RODPPBLC                                                   
*        CLI   QPY,C'Y'            CHECK FOR YTD                                
*        BNE   LMON0196                                                         
*        ICM   RF,15,RODYPBLC                                                   
*                                                                               
LMON0196 DS    0H                                                               
         ICM   RE,15,8(R1)                                                      
         AR    RE,RF                                                            
         STCM  RE,15,8(R1)                                                      
*        MVC   4(8,R1),0(R6)       BILLING                                      
         LA    R6,12(,R6)                                                       
         SPACE                                                                  
         ICM   RE,15,12(R1)                                                     
         ICM   RF,15,0(R6)                                                      
         AR    RE,RF                                                            
         STCM  RE,15,12(R1)                                                     
*        MVC   12(4,R1),0(R6)      FINAL                                        
*                                                                               
         LA    R6,8(,R6)                                                        
*                                                                               
         ICM   RE,15,16(R1)                                                     
         ICM   RF,15,0(R6)                                                      
         AR    RE,RF                                                            
         STCM  RE,15,16(R1)                                                     
*        MVC   16(4,R1),0(R6)      BUDGET                                       
*                                                                               
         ICM   RE,15,20(R1)                                                     
         ICM   RF,15,RODWK                                                      
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   *+8                                                              
         ICM   RF,15,RODYPBLC                                                   
         SPACE                                                                  
         AR    RE,RF                                                            
         STCM  RE,15,20(R1)                                                     
         ICM   RE,15,24(R1)                                                     
         ICM   RF,15,RODWK+4                                                    
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   *+8                                                              
         ICM   RF,15,RODYPBLC+4                                                 
         SPACE                                                                  
         AR    RE,RF                                                            
         STCM  RE,15,24(R1)                                                     
*                                                                               
         LR    R6,R0                                                            
         B     LMON0210                                                         
*                                                                               
LMON0200 LA    R5,4(R1)                                                         
*                                                                               
         GOTO1 =A(DRTOTAL),RR=RELO TOTAL UP                                     
*                                                                               
LMON0210 DS   0H                                                                
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         MVI   TRCTYPE,X'17'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
         SPACE                                                                  
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE  TEST SAME KEY                       
         BNE   LMON0240                      NO                                 
         SPACE                                                                  
         CLC   ROKDTLYM,QEND       YES- CHECK THE MONTH                         
         BNH   LMON0180                                                         
         SPACE                                                                  
LMON0230 MVI   ROKDTLYM-ROKEY+KEY,X'FF'                                         
*                                                                               
LMON0240 CLI   SET1CDE,0           DOING SETS?                                  
         BE    LMON0250             NO                                          
         SPACE                                                                  
         BAS   RE,CSETM            CK IF DONE, OR NEXT SET                      
         BE    LMON0040             YES, NEXT SET                               
         SPACE                                                                  
         TM    DISPFLAG,OPCONDCT   LOOKING FOR HARD CODE CON/DCT                
         BZ    LMON0250             NO                                          
         MVC   KEY,KEYSAVE                                                      
         SPACE                                                                  
         LA    RE,SET1CDE                                                       
         LR    RF,R9                                                            
         AHI   RF,SET1TAB-SYSD                                                  
LMON0242 CLI   0(RE),QLCON       THIS THE CODE                                  
         BE    LMON0244                                                         
         LA    RE,L'SETCDE(,RE)    NEXT CODE                                    
         LA    RF,L'SET1TAB(,RF)   NEXT TABLE                                   
         BCT   R0,LMON0242                                                      
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
         SPACE                                                                  
LMON0244 CLI   0(RF),C'S'           THIS LAST PASS                              
         BE    LMON0260              YES                                        
         SPACE                                                                  
         MVC   0(5,RF),=X'E200000000'    RESET ADFNY TO S                       
         MVC   L'SETCDE(2,RE),=X'2D02'   ADD QLDCT SET                          
         LA    RF,L'SET1TAB(,RF)         NEXT TABLE                             
*        MVC   0(6,RF),=C'HBNBUB'        QLDCT HB NB UB ONLY                    
         MVC   0(4,RF),=C'NBUB'          QLDCT NB UB ONLY                       
         BAS   RE,SETCNDT                                                       
         XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         SPACE                                                                  
         GOTO1 HIGH                                                             
         MVI   TRCTYPE,12                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         SPACE                                                                  
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE  TEST SAME KEY                       
         BE    LMON0040                      YES, NEXT SET                      
         B     LMON0240                                                         
         SPACE                                                                  
         SPACE                                                                  
LMON0250 CLI   NCOMBOS,0           TEST COMBO STATION                           
         BE    LMON0260             NO                                          
         SPACE                                                                  
         L     R2,ACOMBOS          YES-R2=A(CURRENT STATION)                    
         ZIC   R3,NCOMBOS              R3=N'STATIONS                            
         LA    R2,7(R2)            YES-ADVANCE TO NEXT STATION                  
         MVI   FIRSTSTA,C'N'                                                    
         BCT   R3,*+8                                                           
         B     LMON0260                                                         
         SPACE                                                                  
         ST    R2,ACOMBOS          YES-R2=A(CURRENT STATION)                    
         STC   R3,NCOMBOS              R3=N'STATIONS                            
         B     LMON0020                                                         
         SPACE                                                                  
LMON0260 LA    R1,MONTOTS          TEST ANY DATA                                
         LA    R0,12                                                            
         OC    4(MONTOTLN-4,R1),4(R1)                                           
         BNZ   LMON0270            YES - FORMAT AND DISPLAY DATA                
         LA    R1,MONTOTLN(,R1)                                                 
         BCT   R0,*-14                                                          
         B     LMON0880            BYPASS EMPTY REQUEST                         
*                                                                               
LSCRNLIN EQU   RGESEL2H-RGESELH                                                 
*                                                                               
LMON0270 EQU   *                                                                
         MVI   RGEHD2+65,C' '      COMPANY BUDGET FLAG                          
         OI    RGEHD2H+6,X'80'     FORCE TRANS                                  
         SPACE                                                                  
         CLI   COMPBUDG,C'Y'       RETRIEVE COMPANY BUDGET RECORD?              
         BNE   LMON0300            NO  - SKIP IT                                
*                                                                               
*   RETRIEVE BUDGET RECORD, INSERT INTO APPROPRIATE SLOTS PRIOR TO              
*        DISPLAY OF DATA.                                                       
*                                                                               
         BAS   RE,SETREPFL         SET UP REP FILE VALUES                       
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'13'           SET BUDGET RECORD TYPE                       
         MVC   KEY+16(2),AGENCY    INSERT REP CODE                              
         SPACE                                                                  
         CLC   AGENCY,=C'K3'       AND KATZ (FOR NOW)                           
         BNE   LMON0278                                                         
         SPACE                                                                  
         CLI   QGROUP,C'*'        NOT FOR SETS                                  
         BE    LMON0278                                                         
         CLI   QGROUP,C' '        FILTER ON GROUP                               
         BNH   LMON0278                                                         
         CLI   QGROUP+1,C' '      MUST INCLUDE SUB GROUP                        
         BNH   LMON0278                                                         
         SPACE                                                                  
         LA    RE,K3GRPCT                                                       
         L     RF,=A(K3GRPTBL)                                                  
         A     RF,RELO                                                          
         SPACE                                                                  
LMON0274 CLC   QGROUP,0(RF)                                                     
         BE    LMON0276                                                         
         LA    RF,4(,RF)                                                        
         BCT   RE,LMON0274                                                      
         B     LMON0278                                                         
         SPACE                                                                  
LMON0276 MVC   KEY+16(2),2(RF)     INSERT REP CODE                              
         SPACE                                                                  
LMON0278 DS   0H                                                                
         MVC   FULL(2),QSTART                                                   
         MVI   FULL+2,15                                                        
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS PRIOR YEAR                              
         BNE   *+12                                                             
         SR    R0,R0                                                            
         IC    R0,FULL                                                          
         BCTR  R0,0                                                             
         STC   R0,FULL                                                          
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,FULL),(0,WORK)                                    
         MVC   KEY+18(2),WORK                                                   
         SPACE                                                                  
*        EDIT  (R0),(2,KEY+18),FILL=0                                           
*                                  INSERT YEAR OF BUDGET                        
         MVC   KEY+20(5),=C'C*MP ' INSERT 'MASTER BUDGET STATION'               
*                                                                               
         LA    R2,RRGOFFH          OFFICE SCREEN FIELD                          
         CLI   5(R2),0             IS THERE ANY OFFICE?                         
         BE    LMON0280            NO                                           
         MVC   KEY+25(2),8(R2)     YES - INSERT INTO KEY                        
*                                                                               
LMON0280 EQU   *                                                                
         GOTO1 HIGH                RETRIEVE BUDGET RECORD                       
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   LMON0300            NO  -                                        
         SPACE                                                                  
         MVI   RGEHD2+65,C'*'                                                   
         OI    RGEHD2H+6,X'80'     FORCE TRANS                                  
         SPACE                                                                  
* CLEAR BUDGET FIGURES FROM TABLE *                                             
         SPACE                                                                  
         LA    R0,12                                                            
         LA    R1,MONTOTS                                                       
         XC    MTCURBUD(4,R1),MTCURBUD(R1)                                      
         LA    R1,MONTOTLN(,R1)                                                 
         BCT   R0,*-10                                                          
         SPACE                                                                  
         GOTO1 GETREC                                                           
         BAS   RE,BUDSETUP         SET BUDGETS INTO DISPLAY ARRAY               
LMON0300 EQU   *                                                                
         LA    R2,LISTAR-8         SET TO DETAIL OF DISPLAY LINE                
*                                  ROUTINE IS SET TO INSERT INTO A              
*                                     SCREEN LINE, AND EVERY ADDR               
*                                     IS 8 CHARACTERS HIGH                      
         MVC   KEYSAVE,LMONKEY     RESET KEYSAVE                                
         MVC   KEY,LMONKEY         RESET KEY                                    
         LA    R4,MONTOTS                                                       
         LA    R3,12                                                            
*                                                                               
LMON0320 DS   0H                                                                
         MVC   LISTAR,BLANKS       CLEAR DISPLAY LINE                           
         STM   R3,R4,SVREGS                                                     
         OC    4(MONTOTLN-4,R4),4(R4)                                           
         BZ    LMON0800                                                         
         MVC   MONTH,1(R4)         SAVE THE MONTH NUMBER                        
         CLI   1(R4),0             CHECK FOR PRIOR                              
         BNE   LMON0340                                                         
         MVC   8(5,R2),=C'PRIOR'           YES-FORMAT PRIOR LINE                
         MVC   TOTALS(MONTOTLN-4),4(R4)    MOVE PRIOR LINE TO TOTALS            
         B     LMON0400                                                         
*                                                                               
LMON0340 MVC   DUB(2),0(R4)        NOT PRIOR                                    
         MVI   DUB+2,1                                                          
*                                                                               
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   LMON0360                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(0,WORK)    SET BACK DATE 1 YEAR             
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'-365'                                   
         GOTO1 DATCON,(R1),(0,WORK+6),(3,DUB)                                   
*                                                                               
LMON0360 DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(6,8(R2))    MONTH/YR TO PERIOD COL          
         SPACE                                                                  
         CLI   QPY,C'Y'            CHECK FOR YTD                                
         BNE   LMON0400            IF IT WORKS DELETE FOLLOWING CODE            
         SPACE                                                                  
         LA    R5,TOTALS                                                        
         LA    R6,4(R4)                                                         
*                                                                               
         LA    R0,6                                                             
*                                                                               
LMON0380 DS    0H                                                               
         ICM   R1,15,0(R6)                                                      
         BZ    *+14                                                             
         ICM   RF,15,0(R5)                                                      
         AR    RF,R1                                                            
         STCM  RF,15,0(R5)                                                      
         LA    R6,4(R6)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,LMON0380                                                      
*                                                                               
LMON0400 LA    R6,4(R4)                                                         
         CLI   QPY,C'Y'            CHECK FOR YTD                                
         BNE   LMON0420                                                         
         LA    R6,TOTALS           YES - FORMAT MONTH LINE FROM TOTALS          
*                                                                               
LMON0420 L     R3,0(R6)            PRIOR BILLING                                
         LTR   R5,R3                                                            
         BZ    LMON0460                                                         
         SPACE                                                                  
         CLI   LASTLN,C'Y'         NO PCT ADJUSTMENT FOR TOTALS                 
         BE    LMON0440                                                         
         SPACE                                                                  
         CLI   ROUNDSW,C'Y'                                                     
         BNE   LMON0440                                                         
         BAS   RE,ROUND                                                         
         BZ    LMON0460                                                         
*                                                                               
LMON0440 EDIT  (R5),(10,18(R2)),FLOAT=-                                         
         SPACE                                                                  
         CLI   QPY,C'Y'            CHECK FOR YTD                                
         BE    LMON0460                                                         
         SPACE                                                                  
         ICM   R0,15,TOTALS                                                     
         AR    R0,R5                                                            
         STCM  R0,15,TOTALS                                                     
*                                                                               
LMON0460 L     R4,4(R6)            CURRENT BILLING                              
         LTR   R5,R4                                                            
         BZ    LMON0540                                                         
         SPACE                                                                  
         CLI   LASTLN,C'Y'         NO PCT ADJUSTMENT FOR TOTALS                 
         BE    LMON0480                                                         
         SPACE                                                                  
         CLI   ROUNDSW,C'Y'                                                     
         BNE   LMON0480                                                         
         BAS   RE,ROUND                                                         
         BZ    LMON0530                                                         
*                                                                               
LMON0480 EDIT  (R5),(10,29(R2)),FLOAT=-                                         
         SPACE                                                                  
         CLI   QPY,C'Y'            CHECK FOR YTD                                
         BE    LMON0490                                                         
         SPACE                                                                  
         ICM   R0,15,TOTALS+4                                                   
         AR    R0,R5                                                            
         STCM  R0,15,TOTALS+4                                                   
*                                                                               
LMON0490 DS   0H                                                                
         GOTO1 =A(PCTCOMP),RR=RELO CALCULATE CUR PACING                         
         BNE   LMON0520                                                         
         LTR   R5,R5               R5 HAS PCT                                   
         BZ    LMON0540                                                         
         SPACE                                                                  
         CLI   MONTH,0             NO PCT ADJUSTMENT FOR PRIOR                  
         BE    LMON0500                                                         
         CLI   LASTLN,C'Y'         NO PCT ADJUSTMENT FOR TOTALS                 
         BE    LMON0500                                                         
         SPACE                                                                  
         LR    RF,RA               CHECK PROFILE: SUPPRESS BUDGET?              
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'20'      THIRD BIT ON?                                
         BO    LMON0500             BYPASS 4/5 WK ADJ                           
         DROP  RF                                                               
         SPACE                                                                  
         GOTO1 =A(PCTADJ),RR=RELO                                               
         BNE   LMON0520                                                         
*                                                                               
LMON0500 EDIT  (R5),(6,40(R2)),1                                                
         B     LMON0540                                                         
*                                                                               
LMON0520 MVC   42(4,R2),=C'HIGH'   MORE THAN 999 PCT                            
         B     LMON0540                                                         
         SPACE                                                                  
LMON0530 SR    R4,R4               SET TO ZERO                                  
*                                                                               
* BOOKED IS NEVER ON FOR STEREO-IT ALWAYS GETS IT, BUT IN LONG NAME LST         
*                                                                               
LMON0540 TM    DISPFLAG,DISPBOOK                                                
         BZ    LMON0580                                                         
         MVC   WORK(28),18(R2)                                                  
         MVC   37(28,R2),WORK                                                   
         MVC   18(19,R2),BLANKS                                                 
*                                                                               
         ICM   R3,15,16(R6)               PRIOR BOOKED                          
         BZ    LMON0560                                                         
         EDIT  (R3),(9,18(R2)),FLOAT=-                                          
         SPACE                                                                  
         CLI   QPY,C'Y'            CHECK FOR YTD                                
         BE    LMON0560                                                         
         SPACE                                                                  
         A     R3,TOTALS+16                                                     
         ST    R3,TOTALS+16                                                     
*                                                                               
LMON0560 ICM   R3,15,20(R6)               CURRENT BOOKED                        
         BZ    LMON0680                                                         
         EDIT  (R3),(9,28(R2)),FLOAT=-                                          
         SPACE                                                                  
         CLI   QPY,C'Y'            CHECK FOR YTD                                
         BE    LMON0680                                                         
         SPACE                                                                  
         A     R3,TOTALS+20                                                     
         ST    R3,TOTALS+20                                                     
         B     LMON0680                                                         
*                                                                               
LMON0580 L     R3,8(R6)            PRIOR FINAL                                  
         LTR   R5,R3                                                            
         BZ    LMON0680                                                         
         SPACE                                                                  
         CLI   LASTLN,C'Y'         NO PCT ADJUSTMENT FOR TOTALS                 
         BE    LMON0600                                                         
         SPACE                                                                  
         CLI   ROUNDSW,C'Y'                                                     
         BNE   LMON0600                                                         
         BAS   RE,ROUND                                                         
         BZ    LMON0680                                                         
*                                                                               
LMON0600 EDIT  (R5),(10,47(R2)),FLOAT=-                                         
         SPACE                                                                  
         CLI   QPY,C'Y'            CHECK FOR YTD                                
         BE    LMON0620                                                         
         SPACE                                                                  
         ICM   R0,15,TOTALS+8                                                   
         AR    R0,R5                                                            
         STCM  R0,15,TOTALS+8                                                   
*                                                                               
LMON0620 DS   0H                                                                
         LTR   R4,R4               ANY CURR $                                   
         BZ    LMON0680             NO                                          
         SPACE                                                                  
         GOTO1 =A(PCTCOMP),RR=RELO CALCULATE PCT TO FINAL                       
         BE    *+14                                                             
         MVC   60(4,R2),=C'HIGH'                                                
         B     LMON0680                                                         
         LTR   R5,R5                                                            
         BZ    LMON0680                                                         
         EDIT  (R5),(6,58(R2)),1                                                
*                                                                               
LMON0680 DS   0H                                                                
         LR    RF,RA               CHECK PROFILE: SUPPRESS BUDGET?              
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'80'      FIRST BIT ON?                                
         BO    LMON0780            YES - SKIP BUDGET DISPLAY                    
*                                                                               
         CLI   TWAACCS,C'$'        STATION SIDE?                                
         BNE   *+12                 NO                                          
         TM    SVPGPBIT,X'40'      YES - SUPPRESS BUDGET?                       
         BO    LMON0780            YES - SKIP BUDGET DISPLAY                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         ICM   R3,15,12(R6)        CURRENT BUDGET                               
         LTR   R5,R3                                                            
         BZ    LMON0720                                                         
         SPACE                                                                  
         CLI   LASTLN,C'Y'         NO PCT ADJUSTMENT FOR TOTALS                 
         BE    LMON0700                                                         
         SPACE                                                                  
         CLI   ROUNDSW,C'Y'                                                     
         BNE   LMON0700                                                         
         BAS   RE,ROUND                                                         
         BZ    LMON0720                                                         
*                                                                               
LMON0700 EDIT  (R5),(10,64(R2)),FLOAT=-                                         
         SPACE                                                                  
         CLI   QPY,C'Y'            CHECK FOR YTD                                
         BE    LMON0720                                                         
         SPACE                                                                  
         ICM   R0,15,TOTALS+12                                                  
         AR    R0,R5                                                            
         STCM  R0,15,TOTALS+12                                                  
*                                                                               
LMON0720 DS   0H                                                                
         GOTO1 =A(PCTCOMP),RR=RELO CALCULATE PCT TO BUDGET                      
         BE    *+14                                                             
         MVC   78(4,R2),=C'HIGH'                                                
         B     LMON0780                                                         
         LTR   R5,R5                                                            
         BZ    LMON0780                                                         
         EDIT  (R5),(6,76(R2)),1                                                
*                                                                               
LMON0780 DS   0H                                                                
         MVI   ANYDATA,C'Y'        SET DATA FOUND FOR REQUEST                   
*                                                                               
         CLI   MODE,PRINTREP                                                    
         BE    LMON0790                                                         
         SPACE                                                                  
         GOTO1 LISTMON                                                          
         B     LMON0800                                                         
*                                                                               
LMON0790 L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         MVC   P,SPACES                                                         
         MVC   P+10(80),LISTAR                                                  
*                                                                               
         GOTO1 SPOOL,DMCB,ASPOOLD                                               
         DROP  R3                                                               
*                                                                               
*                                  FORMAT DOLLARS TO SCREEN                     
LMON0800 LM    R3,R4,SVREGS        NEXT MONTH                                   
         LA    R4,MONTOTLN(,R4)                                                 
         BCT   R3,LMON0320                                                      
*                                                                               
         CLI   LASTLN,C'Y'         LAST LINE SHOWN?                             
         BE    LMON0850            YES - DON'T RESHOW                           
*                                                                               
LMON0820 EQU   *                                                                
         CLI   FULLSTER,C'Y'       THIS FULL STEREO                             
         BE    *+12                 YES                                         
         CLI   QPY,C'Y'            YTD GETS NO TOTAL LINE                       
         BE    LMON0850                                                         
         SPACE                                                                  
*                                                                               
* CLEAR OUT DISPLAYED TOTALS                                                    
*                                                                               
         CLI   FULLSTER,C'Y'       THIS FULL STEREO                             
         BE    *+10                 YES                                         
         XC    MONTOTS+4(24),MONTOTS+4                                          
*                                                                               
         MVC   LISTAR,BLANKS               CLEAR DISPLAY LINE                   
         MVC   8(6,R2),=C'TOTALS'                                               
         SPACE                                                                  
         CLI   FULLSTER,C'Y'       THIS FULL STEREO                             
         BNE   LMON0840             YES                                         
         MVC   LISTAR(2),=C'@@'                                                 
         MVC   LISTAR+2(6),=C'TOTALS'                                           
LMON0840 EQU   *                                                                
         SPACE                                                                  
         MVI   LASTLN,C'Y'                                                      
         LA    R6,TOTALS                                                        
         CLI   ROUNDSW,C'Y'                                                     
         BNE   LMON0420            FORMAT THE TOTALS LINE                       
         SPACE                                                                  
         CLI   QPY,C'Y'            YTD GETS ROUNDED                             
         BNE   LMON0420                                                         
         SPACE                                                                  
         ICM   R5,15,TOTALS                                                     
         BAS   RE,ROUND                                                         
         STCM  R5,15,TOTALS                                                     
         ICM   R5,15,TOTALS+4                                                   
         BAS   RE,ROUND                                                         
         STCM  R5,15,TOTALS+4                                                   
         ICM   R5,15,TOTALS+8                                                   
         BAS   RE,ROUND                                                         
         STCM  R5,15,TOTALS+8                                                   
         ICM   R5,15,TOTALS+12                                                  
         BAS   RE,ROUND                                                         
         STCM  R5,15,TOTALS+12                                                  
         B     LMON0420            FORMAT THE TOTALS LINE                       
*                                                                               
LMON0850 EQU   *                                                                
*                                                                               
* CLEAR OUT DISPLAYED TOTALS                                                    
*                                                                               
         XC    MONTOTS+4(24),MONTOTS+4                                          
*                                                                               
         CLI   ERREX2SW,C'Y'                                                    
         BNE   LMON0880                                                         
         CLI   ACTNUM,ACTLIST      WARNING MESSAGE ONLY FOR LIST                
         BNE   LMON0880                                                         
         XC    CONHEAD,CONHEAD                                                  
         L     R1,=A(WARNING)                                                   
         A     R1,RELO                                                          
         MVC   CONHEAD(L'WARNING),0(R1)                                         
         MVC   CONHEAD+L'WARNING(8),WARNDATE                                    
         B     LMONERX2                                                         
*                                                                               
MONPRTER XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD(L'MONPRTMS),MONPRTMS                                     
LMONERX2 L     R2,AFSTFLD                                                       
         GOTO1 ERREX2                                                           
*                                                                               
LMON0860 MVI   ERROR,NODATA                                                     
         LA    R2,CONRECH                                                       
         GOTO1 ERREX                                                            
*                                                                               
LMON0880 DS   0H                                                                
         OI    GENSTAT2,DISTHSPG   REDISPLAY THIS PAGE                          
         XIT1                                                                   
MONPRTMS DC    C'* ERROR * CAN''T PRINT L=MON *'                                
*                                                                               
         EJECT                                                                  
*                                                                               
*   BUDSETUP: INSERT MONTHLY BUDGETS FROM BUDGET RECORD INTO THE                
*        ARRAY BUILT FROM THE RRGON RECORDS.  THIS OVERLAYS THE                 
*        BUDGET VALUES TAKEN FROM THE RRGON RECORDS.                            
*                                                                               
BUDSETUP NTR1                                                                   
         L     R6,AIO2             SET A(IO AREA W/ BUDGET RECORD)              
         USING RBUDD,R6                                                         
         LA    R4,MONTOTS          SET A(SCREEN TABLE AREA)                     
         LA    R6,RBUDSTA          SET A(STATION BUDGETS)                       
         DROP  R6                                                               
         ZIC   RF,QSTART+1         BUDGET START MONTH                           
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SLA   RF,2                MULTIPLY BY 4 (FULL WORD LENGTH)             
         AR    R6,RF               SET D(STATION BUDGETS)                       
         ZIC   RF,QSTART+1         BUDGET START MONTH                           
         ZIC   RE,QEND+1           BUDGET END   MONTH                           
         CR    RE,RF               END VS START:  OUT OF YEAR?                  
         BNL   BDSU0040            START <= END: NO ADJUSTMENT                  
         LA    RE,12(RE)           END < START:  OUT OF YEAR                    
BDSU0040 EQU   *                                                                
         SR    RE,RF               SUBTRACT START FROM END                      
         LA    RE,1(RE)            ADD 1 FOR DIFFERENCING                       
BDSU0080 EQU   *                                                                
         MVC   MTCURBUD(4,R4),0(R6)                                             
*                                  MOVE BUDGET FROM REC TO TABLE                
         LA    R4,MONTOTLN(R4)     BUMP TO NEXT TABLE ENTRY                     
         LA    R6,4(R6)            BUMP TO NEXT RECORD ENTRY                    
         BCT   RE,BDSU0080         GO BACK FOR NEXT                             
         XIT1                                                                   
         EJECT                                                                  
* RESET KEY FOR HARD CODED QLCON/QLDCT FOR SECOND PASS WIT S/NB                 
         SPACE                                                                  
SETCNDT  NTR1                                                                   
         SPACE                                                                  
         LA    R2,KEY+ROKDTLTY-ROKEY  START OF DATA TYPES                       
         LA    R3,KEY+ROKDTLVL-ROKEY  START OF DATA VALUES                      
         LR    R0,R3                                                            
         SPACE                                                                  
SETCN020 CR    R0,R2               SEE IF PAST DATA TYPES                       
         BNH   SETCN030                                                         
         CLI   0(R2),0             THIS VALID FILTER                            
         BE    SETCN030             NO, DONE                                    
         SPACE                                                                  
         CLI   0(R2),QLCON         IF CONTRACT TYPE                             
         BNE   SETCN024                                                         
         MVI   1(R2),QLDCT         SET NEXT TO DEV CON TYPE                     
         SPACE                                                                  
SETCN024 GOTO1 =A(SETD),RR=RELO    SET DATA FIELD                               
         SPACE                                                                  
         LA    R2,1(,R2)                                                        
         LA    R3,8(,R3)                                                        
         B     SETCN020                                                         
         SPACE                                                                  
SETCN030 XIT1                                                                   
         SPACE                                                                  
*                                                                               
*        ROUTINE TO TOTAL 6 BUCKETS AT R6 TO 6 BUCKETS AT R5                    
*                                                                               
         SPACE                                                                  
DRTOTAL  NMOD1 0,**+DRT**                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         SPACE                                                                  
*                                                                               
         CLI   YRSW,C'P'           THIS PRIOR YEAR                              
         BE    *+8                                                              
         LA    R6,4(,R6)                                                        
*                                                                               
*  GET BILLING (PRIOR & CURRENT)                                                
*                                                                               
         ICM   R1,15,0(R6)         PRIOR BILLING                                
         SPACE                                                                  
         ICM   RF,15,0(R5)                                                      
         AR    RF,R1                                                            
         STCM  RF,15,0(R5)                                                      
         LA    R6,4(,R6)                                                        
         LA    R5,4(,R5)                                                        
         SPACE                                                                  
         ICM   R1,15,0(R6)         CURRENT BILLING                              
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   DT010                                                            
         LR    RF,RA                                                            
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'08'      USE PRIOR YR BILLING-CURR AS/AT DATE         
         BZ    DT010                NO                                          
         DROP  RF                                                               
         SPACE                                                                  
         ICM   R1,15,RODPPBLC                                                   
*        CLI   QPY,C'Y'            CHECK FOR YTD                                
*        BNE   DT010                                                            
*        ICM   R1,15,RODYPBLC                                                   
         SPACE                                                                  
DT010    DS    0H                                                               
         ICM   RF,15,0(R5)                                                      
         AR    RF,R1                                                            
         STCM  RF,15,0(R5)                                                      
         SPACE                                                                  
         LA    R6,4(R6)                                                         
         LA    R5,4(R5)                                                         
*                                                                               
         LA    R6,4(,R6)                                                        
*                                                                               
*  GET FINAL                                                                    
*                                                                               
         ICM   R1,15,0(R6)                                                      
         BZ    *+14                                                             
         ICM   RF,15,0(R5)                                                      
         AR    RF,R1                                                            
         STCM  RF,15,0(R5)                                                      
*                                                                               
         LA    R5,4(R5)                                                         
         LA    R6,8(,R6)                                                        
*                                                                               
*  GET BUDGET (PRIOR OR CURR)                                                   
*                                                                               
         ICM   R1,15,0(R6)                                                      
         BZ    *+14                                                             
         ICM   RF,15,0(R5)                                                      
         AR    RF,R1                                                            
         STCM  RF,15,0(R5)                                                      
*                                                                               
         LA    R6,4(,R6)                                                        
         CLI   YRSW,C'P'           THIS PRIOR YEAR                              
         BNE   DT020                                                            
         LA    R6,4(,R6)                                                        
*                                                                               
*  GET BOOKED (PRIOR AND CURR)                                                  
*                                                                               
DT020    LA    R6,28(,R6)          BYPASS YTD CTRS                              
         SPACE                                                                  
         ICM   R1,15,0(R6)         THIS WEEK LAST YEAR                          
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   *+8                                                              
         ICM   R1,15,RODYPBLC                                                   
         SPACE                                                                  
         LTR   R1,R1                                                            
         BZ    *+14                                                             
         ICM   RF,15,4(R5)                                                      
         AR    RF,R1                                                            
         STCM  RF,15,4(R5)                                                      
         SPACE                                                                  
         ICM   R1,15,4(R6)         THIS WEEK THIS YEAR                          
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   *+8                                                              
         ICM   R1,15,RODYPBLC+4                                                 
         SPACE                                                                  
         LTR   R1,R1                                                            
         BZ    *+14                                                             
         ICM   RF,15,8(R5)                                                      
         AR    RF,R1                                                            
         STCM  RF,15,8(R5)                                                      
*                                                                               
         XIT1                                                                   
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* CHECK IF KEY CHANGE IS END, OR NEEDS NEXT SET MEMBER *                        
         SPACE                                                                  
CSETM    NMOD1 0,*CSETM**                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         SPACE                                                                  
CSETM010 CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BNE   CSETMX                        NO, AT END                         
         SPACE                                                                  
         CLC   LSTTYPE,KEY+3       TEST LISTING ON THIS TYPE                    
         BNE   CSETM014                                                         
         OC    KEY+4(2),KEY+4      AND NO OTHER FIELDS                          
         BZ    CSETMEQ                                                          
         SPACE                                                                  
         CLC   KEY+6(8),KEYSAVE+6  CHANGE IN LISTING KEY FIELD                  
         BNE   CSETMX               YES, GO DISPLAY IT                          
         SPACE                                                                  
CSETM014 LA    R2,KEY+3                                                         
         LA    R3,KEY+6                                                         
         MVI   BYTE,0              SET NO SETS FOUND YET                        
         SPACE                                                                  
CSETM020 LA    R0,3                                                             
         LA    R1,SET1CDE                                                       
         LR    RF,R9                                                            
         AHI   RF,SET1TAB-SYSD                                                  
         SPACE                                                                  
CSETM030 CLC   0(1,R2),0(R1)       THIS A SET CODE                              
         BE    CSETM060                                                         
         LA    R1,L'SETCDE(,R1)                                                 
         LA    RF,L'SET1TAB(,RF)                                                
         BCT   R0,CSETM030                                                      
         SPACE                                                                  
         CLC   LSTTYPE,0(R2)       TEST LISTING ON THIS TYPE                    
         BE    CSETM050                                                         
         SPACE                                                                  
         L     RF,=A(FILTAB)                                                    
         A     RF,RELO                                                          
*                                                                               
CSETM032 CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(RF)                                                    
         BE    CSETM034                                                         
         LA    RF,L'FILTAB(RF)                                                  
         B     CSETM032                                                         
         SPACE                                                                  
CSETM034 CLI   0(R2),QLSTA         ON STATION                                   
         BNE   CSETM040                                                         
         CLI   NCOMBOS,0           THIS STATION COMBOS                          
         BE    CSETM040             NO                                          
         SPACE                                                                  
         LA    R1,COMBOSTA         STATIONS TABLE                               
         SPACE                                                                  
CSETM036 CLC   0(7,R3),0(R1)       THIS STA TO COMBO TABLE                      
         BE    CSETM050                                                         
         BL    CSETM038                                                         
         LA    R1,7(,R1)                                                        
         OC    0(7,R1),0(R1)       END OF TABLE                                 
         BNZ   CSETM036                                                         
         B     CSETMNE             END OF LIST                                  
         SPACE                                                                  
CSETM038 MVC   0(7,R3),0(R1)                                                    
         MVI   BYTE,1              THIS IS SET FOUND (SORT OF)                  
         B     CSETM050                                                         
         SPACE                                                                  
CSETM040 LH    R1,2(RF)                                                         
         LA    R1,SYSD(R1)                                                      
         ZIC   RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,CSETMCLS         TEST FILTER IS SET (COMP TO SPACES)          
         BH    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLI   BYTE,0              FOUND SET YET                                
         BNE   CSETM046             YES                                         
         EX    RE,CSETMCL1         CK CHANGE IN NON-SET KEY                     
         BNE   CSETMX               YES, DONE                                   
         B     CSETM050                                                         
         SPACE                                                                  
CSETM046 EX    RE,CSETMCL1         CHANGE IN NON-SET FILTER                     
         BE    CSETM050                                                         
         BL    CSETM048                                                         
         SPACE                                                                  
*        NEED TO JUMP PREVIOUS SET TO NEXT ENTRY                                
         SPACE                                                                  
         GOTO1 =A(PSET),RR=RELO                                                 
         BNE   CSETMNE                                                          
         LM    R2,R3,DUB                                                        
         B     CSETM250            GO SET FOLLOWING KEYS                        
         SPACE                                                                  
CSETMCL1 CLC   0(0,R3),0(R1)                                                    
CSETMCLS CLC   0(0,R1),BLANKS                                                   
         SPACE                                                                  
CSETM048 EX    RE,CSETMMF          MOVE IN FILTER                               
         SPACE                                                                  
         XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         SPACE                                                                  
CSETM050 LA    R2,1(,R2)                                                        
         LA    R3,8(,R3)                                                        
         LA    R0,KEY+5                                                         
         CR    R2,R0                                                            
         BH    CSETM300                                                         
         CLI   0(R2),0             AT END OF FILTERS                            
         BE    CSETM300                                                         
         B     CSETM020            CHECK NEXT FIELD                             
         SPACE                                                                  
CSETMMF  MVC   0(0,R3),0(R1)                                                    
         EJECT                                                                  
* CHECK SETS HERE                                                               
         SPACE                                                                  
CSETM060 LR    R4,R1                                                            
         LR    R5,RF                                                            
         SPACE                                                                  
         ZIC   R6,1(R4)            GET LENGTH OF ENTRY                          
         BCTR  R6,0                                                             
         CLI   0(R5),0             END OF TABLE                                 
         BNE   CSETM090                                                         
         DC    H'0'                EMPTY TABLE - NO WAY                         
         SPACE                                                                  
CSETM090 CLI   0(R5),0             END OF TABLE                                 
         BNE   CSETM100             NO                                          
         SPACE                                                                  
         TM    SET1FLG-SET1CDE(R4),X'08'  EXCLUDE SET                           
         BO    CSETM154                                                         
         SPACE                                                                  
         CLI   BYTE,0              WAS THERE A PREV SET                         
         BE    CSETMNE              NO, DONE                                    
         SPACE                                                                  
CSETM096 DS   0H                                                                
         GOTO1 =A(PSET),RR=RELO                                                 
         BNE   CSETMNE              NO, DONE                                    
         LM    R2,R3,DUB                                                        
         B     CSETM250            GO SET FOLLOWING KEYS                        
         SPACE                                                                  
CSETM100 CLI   0(R2),QLSTA         STATION COMPARE IS DIFFERENT                 
         BNE   CSETM120                                                         
         CLC   0(3,R3),0(R5)       1ST 3 OF STATION                             
         BNE   CSETM140                                                         
         SPACE                                                                  
         MVC   ACTUAL,3(R3)                                                     
         LA    R1,4(,R3)                                                        
         CLI   3(R3),C'-'                                                       
         BNE   CSETM104                                                         
         BCTR  R1,0                                                             
         MVI   ACTUAL,C' '                                                      
         SPACE                                                                  
CSETM104 CLC   ACTUAL,3(R5)                                                     
         BNE   CSETM140                                                         
         SPACE                                                                  
         MVC   ACTUAL,1(R1)                                                     
         CLI   ACTUAL,C'T'                                                      
         BNE   *+8                                                              
         MVI   ACTUAL,C' '                                                      
         CLC   ACTUAL,4(R5)        TEST BAND                                    
         B     CSETM140                                                         
         SPACE                                                                  
CSETM120 EX    R6,CSETMCL2         SEE IF A MATCH                               
         SPACE                                                                  
CSETM140 BE    CSETM150                                                         
         BL    CSETM200                                                         
         LA    R5,1(R5,R6)                                                      
         B     CSETM090                                                         
         SPACE                                                                  
CSETM150 DS   0H                                                                
         TM    SET1FLG-SET1CDE(R4),X'08'  EXCLUDE SET                           
         BO    CSETM160                    YES, BYPASS                          
         SPACE                                                                  
CSETM154 MVI   BYTE,1              INDICATE SET WAS FOUND                       
         B     CSETM050                                                         
         SPACE                                                                  
CSETM160 MVI   7(R3),X'FF'         FORCE TO NEXT ITEM                           
         B     CSETM250            GO SET REST OF FIELDS                        
         SPACE                                                                  
CSETMCL2 CLC   0(0,R3),0(R5)                                                    
         SPACE                                                                  
CSETM200 TM    SET1FLG-SET1CDE(R4),X'08'  EXCLUDE SET                           
         BO    CSETM154                                                         
         SPACE                                                                  
         EX    R6,CSETMMVC                                                      
         SPACE                                                                  
         CLI   0(R2),QLSTA         STATION MOVE IS DIFFERENT                    
         BNE   CSETM230                                                         
         MVI   6(R3),0             FORCE SHORT STATIONS TO NULL                 
         LA    R1,3(,R3)                                                        
         CLI   3(R3),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(R5)                                                    
         CLI   1(R1),C' '                                                       
         BH    CSETM220                                                         
         MVC   1(2,R1),=C'TV'                                                   
         B     CSETM250                                                         
         SPACE                                                                  
CSETM220 MVI   2(R1),C'M'                                                       
         B     CSETM250                                                         
         SPACE                                                                  
CSETM230 CLI   0(R2),QLCON         CONTRACT TYPE IS DIFFERENT                   
         BNE   CSETM250                                                         
         OI    1(R3),X'40'                                                      
         B     CSETM250                                                         
         SPACE                                                                  
CSETMMVC MVC   0(0,R3),0(R5)                                                    
         SPACE                                                                  
CSETM250 LA    R2,1(,R2)                                                        
         LA    R3,8(,R3)                                                        
         LA    R0,KEY+5                                                         
         CR    R2,R0                                                            
         BH    CSETM300                                                         
         CLI   0(R2),0                                                          
         BE    CSETM300                                                         
CSETM260 BAS   RE,SETD             GO SET THIS TO FIRST ENTRY                   
         B     CSETM250                                                         
         SPACE                                                                  
* NOW READ NEXT SET OF KEYS                                                     
         SPACE                                                                  
CSETM300 CLI   ROKDTLYM-ROKEY+KEY,X'FF'                                         
         BE    *+10                                                             
         XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         SPACE                                                                  
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         MVI   TRCTYPE,09                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE  TEST SAME KEY                       
         BE    CSETMEQ                                                          
         B     CSETM010                                                         
         SPACE                                                                  
CSETMNE  LTR   RB,RB                                                            
         B     CSETMX                                                           
         SPACE                                                                  
CSETMEQ  CR    RB,RB                                                            
CSETMX   XIT1                                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
* SET FIELD TO FILTER VALUE OR FIRST OF SET *                                   
         SPACE                                                                  
SETD     NMOD1 0,**SETD**                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         SPACE                                                                  
         LA    R0,3                                                             
         LA    RE,SET1CDE                                                       
         LR    RF,R9                                                            
         AHI   RF,SET1TAB-SYSD                                                  
SETD10   CLC   0(1,R2),0(RE)       THIS THE CODE                                
         BE    SETD20                                                           
         LA    RE,L'SETCDE(,RE)    NEXT CODE                                    
         LA    RF,L'SET1TAB(,RF)   NEXT TABLE                                   
         BCT   R0,SETD10                                                        
         B     SETD70              NOT IN TABLE? GO SET STD FILTER              
         SPACE                                                                  
SETD20   DS   0H                                                                
         XC    0(8,R3),0(R3)       ZERO OUT KEY FOR POSSIBLE EXCLUDE            
         SPACE                                                                  
         TM    SET1FLG-SET1CDE(RE),X'08'  EXCLUDE SET                           
         BZ    SETD24                      YES, SET TO 01                       
         SPACE                                                                  
         MVI   7(R3),01            BYPASS NULLS RECS                            
         B     SETDX                                                            
         SPACE                                                                  
SETD24   DS   0H                                                                
         ZIC   R1,1(RE)            GET CODE LEN                                 
         BCTR  R1,0                                                             
         EX    R1,SETDMVC                                                       
         SPACE                                                                  
         CLI   0(R2),QLGRP         THIS G/S                                     
         BNE   SETD30                                                           
         CLI   1(R3),C' '          THIS A BLANK                                 
         BH    SETDX                                                            
         MVI   1(R3),0                                                          
         MVI   0(R2),QLGRGRP                                                    
         B     SETDX                                                            
         SPACE                                                                  
SETD30   DS   0H                                                                
         CLI   0(R2),QLSTA         THIS STATION                                 
         BNE   SETD60                                                           
         MVI   6(R3),0             FORCE SHORT STATIONS TO NULL                 
         LA    R1,3(,R3)                                                        
         CLI   3(R3),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(RF)                                                    
         CLI   1(R1),C' '                                                       
         BH    SETD54                                                           
         MVC   1(2,R1),=C'TV'                                                   
         B     SETDX                                                            
SETD54   MVI   2(R1),C'M'                                                       
         B     SETDX                                                            
         SPACE                                                                  
SETD60   CLI   0(R2),QLCON         THIS CONTRACT TYPE                           
         BNE   SETDX                                                            
         OI    1(R3),X'40'         SET TO BLANK                                 
         B     SETDX                                                            
         SPACE                                                                  
SETD70   L     R1,=A(FILTAB)                                                    
         A     R1,RELO                                                          
*                                                                               
SETD74   CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(R1)                                                    
         BE    SETD80                                                           
         LA    R1,L'FILTAB(R1)                                                  
         B     SETD74                                                           
         SPACE                                                                  
SETD80   ZIC   RE,1(R1)            GET LENGTH                                   
         BCTR  RE,0                                                             
         LH    RF,2(R1)                                                         
         LA    RF,SYSD(RF)                                                      
         EX    RE,SETDCLC          TEST FILTER IS SET (COMP TO SPACES)          
         BH    SETD84                                                           
         CLI   0(R2),QLSTA         IF STATION                                   
         BNE   *+12                                                             
         OC    COMBOSTA,COMBOSTA   MAY BE COMBOS                                
         BNZ   SETD90                                                           
         DC    H'0'                                                             
SETD84   EX    RE,SETDMVC          MOVE IN FILTER                               
         B     SETDX                                                            
         SPACE                                                                  
SETD90   MVC   0(7,R3),COMBOSTA                                                 
         LA    R0,CSTAMAX                                                       
         SR    RE,RE                                                            
         LA    RF,COMBOSTA                                                      
         ST    RF,ACOMBOS                                                       
SETD94   OC    0(7,RF),0(RF)                                                    
         BZ    SETD96                                                           
         LA    RE,1(,RE)                                                        
         LA    RF,7(,RF)                                                        
         BCT   R0,SETD94                                                        
SETD96   STC   RE,NCOMBOS                                                       
         SPACE                                                                  
SETDX    DS    0H                                                               
         XIT1                                                                   
SETDCLC  CLC   0(0,RF),BLANKS                                                   
SETDMVC  MVC   0(0,R3),0(RF)                                                    
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        DISPLAY KEY ROUTINE                                                    
*                                                                               
DK       NMOD1 0,***DK***                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         SPACE                                                                  
         XC    RRGOPT,RRGOPT       CLEAR OPTIONS FIELD (IT WOULD ONLY           
         OI    RRGOPTH+6,X'80'     HAVE HAD THE LIST OPTION)                    
*                                                                               
         L     R7,AIO                                                           
         USING RORECD,R7                                                        
         ZIC   R1,LSTDISP                                                       
         LA    R1,ROREC(R1)                                                     
         CLI   LSTTYPE,QLOFF          TEST LISTING OFFICES                      
         BNE   DK20                                                             
         MVC   RRGOFF,0(R1)           YES-MOVE OFFICE TO HEADLINE               
         OI    RRGOFFH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK20     CLI   LSTTYPE,QLSTA          TEST LISTING STATIONS                     
         BNE   DK30                                                             
         MVC   RRGSTA,0(R1)           YES-MOVE STATION TO HEADLINE              
         OI    RRGSTAH+6,X'80'                                                  
         MVI   NCOMBOS,0                                                        
         CLI   ACTNUM,ACTSEL       TEST ACTION = SELECT                         
         BNE   DKX                                                              
         TM    QLOPT,QLCOMBO       AND LIST=COMBO                               
         BZ    DKX                                                              
         LA    R1,6                                                             
         CLC   RRGSTA+3(2),=C'-C'  AND '-C' STATION                             
         BE    *+18                                                             
         LA    R1,7                                                             
         CLC   RRGSTA+4(2),=C'-C'                                               
         BNE   DKX                                                              
         LA    R2,RRGSTAH          YES-VALIDATE THE STATION                     
         STC   R1,5(R2)                                                         
         BAS   RE,SETREPFL                                                      
         GOTO1 VALISTA                                                          
         GOTO1 =A(GETCOMBO),RR=RELO    AND GET THE COMBINED STATIONS            
         BAS   RE,RSETRRGO                                                      
         CLI   NCOMBOS,0           TEST ANY COMBINED STATIONS                   
         BE    DKX                                                              
         LA    R1,COMBOSTA         YES-SET FIRST STATION TO THIS ONE            
         ST    R1,ACOMBOS                                                       
         LA    RE,ROKDTLTY         FIND DISPLACEMENT INTO KEY OF                
         LA    RF,ROKDTLVL         STATION VALUE                                
         LA    R0,L'ROKDTLTY                                                    
*                                                                               
DK24     CLI   0(RE),2                                                          
         BE    *+18                                                             
         LA    RE,1(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,DK24                                                          
         DC    H'0'                                                             
         SR    RF,R7                                                            
         ST    RF,STADISP                                                       
         B     DKX                                                              
*                                                                               
DK30     CLI   LSTTYPE,QLCLS          TEST LISTING CLASSES                      
         BNE   DK34                                                             
         MVC   RRGCLS,0(R1)           YES-MOVE CLASS TO HEADLINE                
         OI    RRGCLSH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK34     CLI   LSTTYPE,QLGRP          TEST LISTING GROUP/SUB                    
         BNE   DK40                                                             
         MVC   RRGGRP,0(R1)           YES-MOVE GRP/SUB TO HEADLINE              
         OI    RRGGRPH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK40     CLI   LSTTYPE,QLCAT          TEST LISTING CATEGORIES                   
         BNE   DK50                                                             
         MVC   RRGCAT,0(R1)           YES-MOVE CTGY TO HEADLINE                 
         OI    RRGCATH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK50     CLI   LSTTYPE,QLSTY          TEST LISTING STATION TYPES                
         BNE   DK60                                                             
         OI    RRGSTYH+6,X'80'                                                  
         MVI   RRGSTY,C'C'                                                      
         CLI   0(R1),C'1'                                                       
         BE    DKX                                                              
         MVI   RRGSTY,C'N'                                                      
         CLI   0(R1),C'2'                                                       
         BE    DKX                                                              
         MVI   RRGSTY,C'O'                                                      
         CLI   0(R1),C'3'                                                       
         BE    DKX                                                              
         DC    H'0'                                                             
*                                                                               
DK60     CLI   LSTTYPE,QLTVB          TEST LISTING TVB REGIONS                  
         BNE   DK70                                                             
         MVC   RRGTVB,0(R1)           YES-MOVE TVB TO HEADLINE                  
         OI    RRGTVBH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK70     CLI   LSTTYPE,QLOWN          TEST LISTING OWNERS                       
         BNE   DK74                                                             
         MVC   RRGOWN,0(R1)           YES-MOVE OWNER TO HEADLINE                
         OI    RRGOWNH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK74     CLI   LSTTYPE,QLCON          TEST LISTING CONTRACT TYPES               
         BNE   DK76                                                             
         MVC   RRGCTY,0(R1)           YES-MOVE TYPE TO HEADLINE                 
         OI    RRGCTYH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK76     CLI   LSTTYPE,QLMKT       MARKET                                       
         BNE   DK80                                                             
         MVC   RRGMKT,0(R1)                                                     
         OI    RRGMKTH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK80     CLI   LSTTYPE,QLRNK       RANK                                         
         BNE   DK84                                                             
         MVC   RRGRNK,0(R1)                                                     
         OI    RRGRNKH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK84     CLI   LSTTYPE,QLREG       REGION                                       
         BNE   DK86                                                             
         MVC   RRGREG,0(R1)                                                     
         OI    RRGREGH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK86     CLI   LSTTYPE,QLAFF       AFFILIATE                                    
         BNE   DK90                                                             
         MVC   RRGAFF,0(R1)                                                     
         OI    RRGAFFH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK90     CLI   LSTTYPE,QLADV       ADVERTISER                                   
         BNE   DK94                                                             
         MVC   RRGADV,0(R1)                                                     
         OI    RRGADVH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK94     CLI   LSTTYPE,QLAGY       AGENCY                                       
         BNE   DK96                                                             
         MVC   RRGAGY,0(R1)                                                     
         OI    RRGAGYH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK96     CLI   LSTTYPE,QLDCT       DEVELOPMENTAL CONTRACT TYPE                  
         BNE   DK98                                                             
         MVC   RRGDCT,0(R1)                                                     
         OI    RRGDCTH+6,X'80'                                                  
         B     DKX                                                              
*                                                                               
DK98     CLI   LSTTYPE,QLSAL       DEVELOPMENTAL CONTRACT TYPE                  
         BE    *+6                                                              
         DC    H'0'                WHAT CODE IS THIS?                           
         SPACE                                                                  
         LA    RE,30                                                            
         LA    RF,RRGOPT                                                        
DK990    DS   0H                                                                
         CLC   =C'SP=',0(RF)                                                    
         BE    DK992                                                            
         CLI   0(RF),C' '                                                       
         BNH   DK992                                                            
         LA    RF,1(,RF)                                                        
         BCT   RE,DK990                                                         
DK992    DS   0H                                                                
         MVC   0(3,RF),=C'SP='                                                  
DK994    DS   0H                                                                
         MVC   3(3,RF),0(R1)                                                    
*                                                                               
DKX      XIT1                                                                   
         EJECT                                                                  
*                                                                               
*        DISPLAY RECORD ROUTINE                                                 
*                                                                               
DR       NMOD1 0,***DR***                                                       
         L     RC,SVRC             RESET A(WORKAREA)                            
         USING GEND,RC                                                          
         SPACE                                                                  
         LA    R2,RRGLN1H                                                       
         BAS   RE,CLRSCRN                                                       
         SPACE                                                                  
         XC    RRGHDG,RRGHDG                                                    
         OI    RRGHDGH+6,X'80'                                                  
         SPACE                                                                  
         OC    QSTA,QSTA           IS STATION A FILTER                          
         BZ    DR0006                                                           
         CLI   QSTA,C'*'           SET                                          
         BE    DR0006               YES                                         
         CLI   QLIST,QLSTA         IF LISTING BY STA                            
         BE    DR0006               YES                                         
         SPACE                                                                  
         MVC   RRGHDG(4),STAMKT                                                 
         MVC   RRGHDG+6(L'EXMKTNAM),EXMKTNAM                                    
         SPACE                                                                  
DR0006   TM    DISPFLAG,DISPBOOK                                                
         BZ    DR010                                                            
         SPACE                                                                  
* SET HEADINGS FOR BOOKED OPTION                                                
         SPACE                                                                  
         MVC   RRGHD1+12(L'BHD1),BHD1                                           
         MVC   RRGHD2+12(L'BHD2),BHD2                                           
         SPACE                                                                  
         CLI   ROUNDSW,C'Y'        THIS THOUSANDS                               
         BNE   DR012                                                            
         MVC   RRGHDG+33(5),=C'(000)'                                           
         MVC   RRGHDG+43(5),=C'(000)'                                           
         MVC   RRGHDG+60(5),=C'(000)'                                           
         B     DR012                                                            
         SPACE                                                                  
* SET HEADINGS TO STANDARD                                                      
         SPACE                                                                  
DR010    DS   0H                                                                
         MVC   RRGHD1+12(L'SHD1),SHD1                                           
         MVC   RRGHD2+12(L'SHD2),SHD2                                           
         SPACE                                                                  
         CLI   ROUNDSW,C'Y'        THIS THOUSANDS                               
         BNE   DR012                                                            
         MVC   RRGHDG+14(5),=C'(000)'                                           
         MVC   RRGHDG+24(5),=C'(000)'                                           
         MVC   RRGHDG+43(5),=C'(000)'                                           
         MVC   RRGHDG+60(5),=C'(000)'                                           
         SPACE                                                                  
DR012    DS   0H                                                                
         OI    RRGHD1H+6,X'80'                                                  
         OI    RRGHD2H+6,X'80'                                                  
         SPACE                                                                  
         MVC   RRGHD1+9(1),TYPEKEY  DISPLAY TYPE DATA                           
         MVI   RRGHD1+10,C' '                                                   
         CLI   TYPEKEY,0                                                        
         BNE   *+8                                                              
         MVI   RRGHD1+9,C'A'                                                    
         SPACE                                                                  
         CLI   TYPEKEY,C'D'        DIRECT IS NOW CONFIRMED DIRECT               
         BNE   DR016                                                            
         MVC   RRGHD1+9(2),=C'CD'                                               
         SPACE                                                                  
         CLC   AGENCY,=C'BL'                                                    
         BE    DR014                                                            
         CLC   AGENCY,=C'FN'                                                    
         BE    DR014                                                            
         CLC   AGENCY,=C'PV'       THIS PETRY                                   
         BNE   DR016                                                            
DR014    DS    0H                                                               
         MVI   RRGHD1+9,C'A'       PETRY IS ALL DIRECT                          
         SPACE                                                                  
DR016    DS    0H                                                               
         XC    TOTALS(24),TOTALS                                                
         LA    R1,MONTOTS                                                       
         LA    RF,12                                                            
         XC    4(MONTOTLN-4,R1),4(R1)                                           
         LA    R1,MONTOTLN(,R1)                                                 
         BCT   RF,*-10                                                          
         OI    DMINBTS,X'08'       PASS DELETED RRGON RECS                      
         NI    DMOUTBTS,X'FD'      IGNORE DELETED RRGON RECS                    
*                                                                               
         L     R7,AIO                                                           
         USING RORECD,R7                                                        
         MVI   FIRSTSTA,C'Y'                                                    
         MVI   LASTLN,C'N'                                                      
         CLI   NCOMBOS,0           TEST COMBO STATION                           
         BE    DR030                                                            
         L     R2,ACOMBOS          YES-R2=A(CURRENT STATION)                    
         ZIC   R3,NCOMBOS              R3=N'STATIONS                            
         B     DR030                                                            
*                                                                               
DR020    XC    KEY,KEY             READ RECORDS FOR NEXT COMBO STATION          
         MVC   KEY(L'ROKEY),KEYSAVE                                             
         LA    R7,KEY                                                           
         XC    ROKDTLYM,ROKDTLYM                                                
         L     R1,STADISP                                                       
         LA    R1,KEY(R1)                                                       
         MVC   0(7,R1),0(R2)                                                    
         GOTO1 HIGH                READ FIRST RRGON DETAIL RECORD               
         MVI   TRCTYPE,10                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE                                      
         BNE   DR140                                                            
         L     R7,AIO                                                           
*                                                                               
DR030    LA    R6,RODPER           POINT TO PERIOD FIGURES                      
*        CLI   QPY,C'Y'            TEST FOR YTD                                 
*        BNE   *+8                                                              
*        LA    R6,RODYTD           YES - POINT TO YTD FIGURES                   
*                                                                               
DR040    DS   0H                                                                
         CLC   ROKDTLYM(1),QSTART  CHECK THE YEAR                               
         BNL   DR050                                                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
         MVI   TRCTYPE,25                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE                                      
         BNE   DR140               NO DATA                                      
         B     DR040                                                            
*                                                                               
DR050    CLC   ROKDTLYM(1),QSTART                                               
         BH    DR140               YEAR HIGH - NO DATA                          
         CLI   ROKDTLYM+1,0        CHECK FOR PRIOR RECORD                       
         BNE   DR080                                                            
         CLI   QPY,C'Y'            ONLY YTD NEEDS PRIOR                         
         BNE   DR070                                                            
         CLI   MONTOTS+1,0         TEST FIRST MONTH IS PRIOR                    
         BNE   DR070                                                            
         CLI   FIRSTSTA,C'Y'       YES-ADD IN PRIOR FIGURES                     
         BNE   DR060                                                            
*                                                                               
         LR    RF,R6                                                            
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BE    *+8                  YES                                         
         LA    R6,4(,R6)           POINT TO THIS YEAR                           
*                                                                               
*        MVC   MONTOTS+4(8),0(R6)  BILLING                                      
         ICM   R1,15,0(R6)         PRIOR BILLING                                
*                                                                               
         ICM   R0,15,MONTOTS+4                                                  
         AR    R0,R1                                                            
         STCM  R0,15,MONTOTS+4                                                  
*                                                                               
         ICM   R1,15,4(R6)         CURRENT BILLING                              
*                                                                               
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   DR052                                                            
         LR    RE,RA                                                            
         AHI   RE,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT,X'08'      USE PRIOR YR BILLING-CURR AS/AT DATE         
         BZ    DR052                NO                                          
         DROP  RE                                                               
         SPACE                                                                  
         ICM   R1,15,RODPPBLC                                                   
*        CLI   QPY,C'Y'            CHECK FOR YTD                                
*        BNE   DR052                                                            
*        ICM   R1,15,RODYPBLC                                                   
         SPACE                                                                  
DR052    EQU   *                                                                
         ICM   R0,15,MONTOTS+8                                                  
         AR    R0,R1                                                            
         STCM  R0,15,MONTOTS+8                                                  
         SPACE                                                                  
         LA    R6,12(,R6)                                                       
*                                                                               
         ICM   R1,15,0(R6)                                                      
*                                                                               
         ICM   R0,15,MONTOTS+12                                                 
         AR    R0,R1                                                            
         STCM  R0,15,MONTOTS+12                                                 
*        MVC   MONTOTS+12(4),0(R6) FINAL                                        
*                                                                               
         LA    R6,8(,R6)                                                        
         SPACE                                                                  
* BUDGET                                                                        
         SPACE                                                                  
         CLI   QLIST,QLGRP         FOR GRP/SUB LIST ONLY                        
         BNE   DR054                                                            
         CLC   AGENCY,=C'K3'       AND KATZ (FOR NOW)                           
         BE    DR056                                                            
         SPACE                                                                  
DR054    EQU   *                                                                
         ICM   R0,15,MONTOTS+16                                                 
         ICM   R1,15,0(R6)                                                      
         AR    R0,R1                                                            
         STCM  R0,15,MONTOTS+16                                                 
*        MVC   MONTOTS+16(4),0(R6) BUDGET                                       
*                                                                               
DR056    EQU   *                                                                
         ICM   R0,15,MONTOTS+20                                                 
         ICM   R1,15,RODWK                                                      
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   *+8                                                              
         ICM   R1,15,RODYPBLC                                                   
         SPACE                                                                  
         AR    R0,R1                                                            
         STCM  R0,15,MONTOTS+20                                                 
         ICM   R0,15,MONTOTS+24                                                 
         ICM   R1,15,RODWK+4                                                    
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   *+8                                                              
         ICM   R1,15,RODYPBLC+4                                                 
         SPACE                                                                  
         AR    R0,R1                                                            
         STCM  R0,15,MONTOTS+24                                                 
*        MVC   MONTOTS+20(8),RODWK PRIOR & CURR WEEKLY BOOKED                   
*                                                                               
         LR    R6,RF                                                            
         B     DR070                                                            
*                                                                               
DR060    LA    R5,MONTOTS+4                                                     
         SPACE                                                                  
         GOTO1 =A(DRTOTAL),RR=RELO TOTAL UP                                     
*                                                                               
DR070    MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                 READ NEXT                                    
         MVI   TRCTYPE,26                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE                                      
         BNE   DR140                                                            
         B     DR050                                                            
*                                                                               
DR080    CLC   ROKDTLYM,QSTART     CHECK MON AGAINST START MON                  
         BL    DR100               LOW - STILL PRIOR                            
         CLC   ROKDTLYM,QEND       CHECK MON AGAINST END MON                    
         BNH   DR120                ELSE OK                                     
         CLI   SET1CDE,0           IF SET CONTINUE                              
         BNE   DR110                                                            
         B     DR140               HIGH - NO DATA                               
*                                                                               
DR100    CLI   QPY,C'Y'            ONLY YTD NEEDS PRIOR                         
         BNE   DR110                                                            
         CLI   MONTOTS+1,0         TEST FIRST MONTH IS PRIOR                    
         BNE   DR110                                                            
         LA    R5,MONTOTS+4        YES-ADD TO EXISTING PRIOR                    
         GOTO1 =A(DRTOTAL),RR=RELO TOTAL UP                                     
*                                                                               
DR110    MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                 READ NEXT                                    
         MVI   TRCTYPE,27                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE                                      
         BE    DR080                                                            
         B     DR140                                                            
*                                                                               
DR120    LA    R1,MONTOTS          MOVE DOLLARS TO THE CORRECT MONTH            
         LA    R0,12               LINE                                         
         CLC   ROKDTLYM,0(R1)                                                   
         BE    *+14                                                             
         LA    R1,MONTOTLN(,R1)                                                 
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
         CLI   FIRSTSTA,C'Y'                                                    
         BNE   DR130                                                            
*                                                                               
         LR    RF,R6                                                            
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BE    *+8                  YES                                         
         LA    R6,4(,R6)           POINT TO THIS YEAR                           
*                                                                               
         MVC   4(8,R1),0(R6)       PRIOR & CURRENT BILLING                      
*                                                                               
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   DR126                NO                                          
         LR    RE,RA                                                            
         AHI   RE,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RE                                                       
         TM    SVPGPBIT,X'08'      USE PRIOR YR BILLING-CURR AS/AT DATE         
         BZ    DR126                NO                                          
         DROP  RE                                                               
         SPACE                                                                  
         MVC   8(4,R1),RODPPBLC                                                 
*        CLI   QPY,C'Y'            CHECK FOR YTD                                
*        BNE   DR126                                                            
*        MVC   8(4,R1),RODYPBLC                                                 
         SPACE                                                                  
DR126    DS    0H                                                               
         LA    R6,12(,R6)                                                       
         MVC   12(4,R1),0(R6)      FINAL                                        
*                                                                               
         LA    R6,8(,R6)           POINT TO PRIOR YEAR BUDGET                   
*                                                                               
         MVC   16(4,R1),0(R6)      GET BUDGET                                   
*                                                                               
         MVC   20(8,R1),RODWK      PRIOR & CURR WEEKLY BOOKED                   
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   *+16                                                             
         MVC   20(4,R1),RODYPBLC   PRIOR                                        
         MVC   24(4,R1),RODYPBLC+4 PRIOR BOOKED THIS WEEK                       
         SPACE                                                                  
         LR    R6,RF                                                            
         B     DR134                                                            
*                                                                               
DR130    LA    R5,4(R1)                                                         
         GOTO1 =A(DRTOTAL),RR=RELO TOTAL UP                                     
*                                                                               
DR134    MVC   KEYSAVE,KEY                                                      
         MVI   TRCTYPE,28                                                       
         GOTO1 =A(TRACE),RR=RELO                                                
         GOTO1 SEQ                 GET NEXT RRGON RECORD                        
*                                                                               
         CLC   KEY(ROKDTLYM-ROKEY),KEYSAVE  TEST SAME KEY                       
         BNE   DR140                         NO                                 
         CLC   ROKDTLYM,QEND                YES- CHECK THE MONTH                
         BNH   DR120                                                            
*                                                                               
DR140    CLI   NCOMBOS,0           TEST COMBO STATION                           
         BE    *+16                                                             
         LA    R2,7(R2)            YES-ADVANCE TO NEXT STATION                  
         MVI   FIRSTSTA,C'N'                                                    
         BCT   R3,DR020                                                         
*                                                                               
         CLI   QLIST,QLOFF         TEST LIST=OFFICE                             
         BE    DR142               YES - IGNORE SETS TEST                       
         CLI   SET1CDE,0           IS THIS A SETS REQUEST?                      
         BE    DR142                                                            
         BAS   RE,CSETD            GO TRY TO FIND ANOTHER                       
         BE    DR030                                                            
*                                                                               
DR142    LA    R1,MONTOTS          TEST ANY DATA                                
         LA    R0,12                                                            
         OC    4(MONTOTLN-4,R1),4(R1)                                           
         BNZ   *+16                                                             
         LA    R1,MONTOTLN(,R1)                                                 
         BCT   R0,*-14                                                          
         B     DRERR               NO-ERROR                                     
*                                                                               
         LA    R2,RRGLN1H                                                       
         LA    R4,MONTOTS                                                       
         LA    R3,12                                                            
         SPACE                                                                  
         MVI   RRGHD2+12+53,C' '                                                
         SPACE                                                                  
         CLI   COMPBUDG,C'Y'       RETRIEVE COMPANY BUDGET RECORD?              
         BNE   DR150                NO  - SKIP IT                               
         SPACE                                                                  
         LR    RF,RA               CHECK PROFILE: SUPPRESS BUDGET?              
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'10'      FOURTH BIT ON?                               
         BZ    DR150                NO, BYPASS COMPANY/OFFICE BUDGET            
         DROP  RF                                                               
         SPACE                                                                  
         CLI   QLIST,QLGRP         FOR GRP/SUB LIST ONLY                        
         BNE   DR150                                                            
         SPACE                                                                  
         LA    R0,K3GRPCT                                                       
         L     R1,=A(K3GRPTBL)                                                  
         A     R1,RELO                                                          
         SPACE                                                                  
DR144    CLC   RRGGRP(2),0(R1)                                                  
         BE    DR146                                                            
         LA    R1,4(,R1)                                                        
         BCT   R0,DR144                                                         
         SPACE                                                                  
         CLC   AGENCY,=C'K3'       AND KATZ (FOR NOW)                           
         BNE   DR150                                                            
         LA    R1,AGENCY-2                                                      
         SPACE                                                                  
DR146    DS   0H                                                                
         MVI   RRGHD2+12+53,C'*'                                                
         SPACE                                                                  
         BAS   RE,SETREPFL         SET UP REP FILE VALUES                       
         SPACE                                                                  
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'13'           SET BUDGET RECORD TYPE                       
         MVC   KEY+16(2),2(R1)     GET REP CODE                                 
         SPACE                                                                  
         MVC   FULL(2),QSTART                                                   
         MVI   FULL+2,15                                                        
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS PRIOR YEAR                              
         BNE   *+12                                                             
         SR    R0,R0                                                            
         IC    R0,FULL                                                          
         BCTR  R0,0                                                             
         STC   R0,FULL                                                          
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,FULL),(0,WORK)                                    
         MVC   KEY+18(2),WORK                                                   
         SPACE                                                                  
*        EDIT  (R0),(2,KEY+18),FILL=0                                           
*                                  INSERT YEAR OF BUDGET                        
         MVC   KEY+20(5),=C'C*MP ' INSERT 'MASTER BUDGET STATION'               
         GOTO1 HIGH                RETRIEVE BUDGET RECORD                       
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   DR150                NO  -                                       
         SPACE                                                                  
         GOTO1 GETREC                                                           
         SPACE                                                                  
*        GET MONTHLY BUDGETS FROM BUDGET RECORD                                 
*        STORE INTO TABLE. THIS OVERRIDES THE BUDGET                            
*        VALUES TAKEN FROM RRGON RECORDS.                                       
*                                                                               
         L     R6,AIO2             SET A(IO AREA W/ BUDGET RECORD)              
         USING RBUDD,R6                                                         
         LA    R6,RBUDSTA          SET A(STATION BUDGETS)                       
         DROP  R6                                                               
         LR    R1,R4                                                            
         ZIC   RF,QSTART+1         BUDGET START MONTH                           
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SLA   RF,2                MULTIPLY BY 4 (FULL WORD LENGTH)             
         AR    R6,RF               SET D(STATION BUDGETS)                       
         ZIC   RF,QSTART+1         BUDGET START MONTH                           
         ZIC   RE,QEND+1           BUDGET END   MONTH                           
         CR    RE,RF               END VS START:  OUT OF YEAR?                  
         BNL   DR147               START <= END: NO ADJUSTMENT                  
         LA    RE,12(RE)           END < START:  OUT OF YEAR                    
DR147    EQU   *                                                                
         SR    RE,RF               SUBTRACT START FROM END                      
         LA    RE,1(RE)            ADD 1 FOR DIFFERENCING                       
DR148    MVC   16(4,R1),0(R6)                                                   
         LA    R1,MONTOTLN(R1)                                                  
         LA    R6,4(,R6)                                                        
         BCT   RE,DR148                                                         
         SPACE                                                                  
DR150    STM   R3,R4,SVREGS                                                     
         OC    4(MONTOTLN-4,R4),4(R4)                                           
         BZ    DR440                                                            
         MVC   MONTH,1(R4)         SAVE THE MONTH NUMBER                        
         CLI   1(R4),0             CHECK FOR PRIOR                              
         BNE   DR160                                                            
         MVC   8(5,R2),=C'PRIOR'   YES-FORMAT PRIOR LINE                        
         MVC   TOTALS(24),4(R4)    MOVE PRIOR LINE TO TOTALS                    
         B     DR180                                                            
*                                                                               
DR160    MVC   DUB(2),0(R4)        NOT PRIOR                                    
         MVI   DUB+2,1                                                          
*                                                                               
         CLI   YRSW,C'P'           THIS REALLY PRIOR YEAR                       
         BNE   DR170                                                            
*                                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(0,WORK)    SET BACK DATE 1 YEAR             
         GOTO1 ADDAY,(R1),WORK,WORK+6,F'-365'                                   
         GOTO1 DATCON,(R1),(0,WORK+6),(3,DUB)                                   
DR170    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(6,8(R2))    MONTH/YR TO PERIOD COL          
         LA    R5,TOTALS                                                        
         LA    R6,4(R4)                                                         
*                                                                               
         LA    R0,6                4 CTS + BOOKED                               
DR174    ICM   R1,15,0(R6)                                                      
         BZ    *+14                                                             
         ICM   RF,15,0(R5)                                                      
         AR    RF,R1                                                            
         STCM  RF,15,0(R5)                                                      
         LA    R6,4(R6)                                                         
         LA    R5,4(R5)                                                         
         BCT   R0,DR174                                                         
*                                                                               
DR180    LA    R6,4(R4)                                                         
         CLI   QPY,C'Y'            CHECK FOR YTD                                
         BNE   DR190                                                            
         LA    R6,TOTALS           YES - FORMAT MONTH LINE FROM TOTALS          
*                                                                               
DR190    L     R3,0(R6)            PRIOR BILLING                                
         LTR   R5,R3                                                            
         BZ    DR220                                                            
         CLI   ROUNDSW,C'Y'                                                     
         BNE   DR200                                                            
         BAS   RE,ROUND                                                         
         BZ    DR220                                                            
*                                                                               
DR200    EDIT  (R5),(10,18(R2)),FLOAT=-                                         
*                                                                               
DR220    L     R4,4(R6)            CURRENT BILLING                              
         LTR   R5,R4                                                            
         BZ    DR300                                                            
         CLI   ROUNDSW,C'Y'                                                     
         BNE   DR240                                                            
         BAS   RE,ROUND                                                         
         BZ    DR300                                                            
*                                                                               
DR240    EDIT  (R5),(10,29(R2)),FLOAT=-                                         
*                                                                               
         GOTO1 =A(PCTCOMP),RR=RELO CALCULATE CUR PACING                         
         BNE   DR280                                                            
         SPACE                                                                  
         LTR   R5,R5               R5 HAS PCT                                   
         BZ    DR300                                                            
         CLI   MONTH,0             NO PCT ADJUSTMENT FOR PRIOR                  
         BE    DR260                                                            
         CLI   LASTLN,C'Y'         NO PCT ADJUSTMENT FOR TOTALS                 
         BE    DR260                                                            
         GOTO1 =A(PCTADJ),RR=RELO                                               
*                                  ADJUST PERCENT FOR PRIOR/CUR WEEKS           
         BNE   DR280                                                            
*                                                                               
DR260    EDIT  (R5),(6,40(R2)),1                                                
         B     DR300                                                            
*                                                                               
DR280    MVC   42(4,R2),=C'HIGH'   MORE THAN 999 PCT                            
*                                                                               
DR300    TM    DISPFLAG,DISPBOOK                                                
         BZ    DR310                                                            
         MVC   WORK(28),18(R2)                                                  
         MVC   37(28,R2),WORK                                                   
         MVC   18(19,R2),BLANKS                                                 
*                                                                               
         ICM   R3,15,16(R6)               PRIOR BOOKED                          
         BZ    DR304                                                            
         EDIT  (R3),(10,17(R2)),FLOAT=-                                         
*                                                                               
DR304    ICM   R3,15,20(R6)               CURRENT BOOKED                        
         BZ    DR360                                                            
         EDIT  (R3),(10,27(R2)),FLOAT=-                                         
         B     DR360                                                            
*                                                                               
DR310    L     R3,8(R6)            PRIOR FINAL                                  
         LTR   R5,R3                                                            
         BZ    DR340                                                            
         CLI   ROUNDSW,C'Y'                                                     
         BNE   DR320                                                            
         BAS   RE,ROUND                                                         
         BZ    DR340                                                            
*                                                                               
DR320    EDIT  (R5),(10,47(R2)),FLOAT=-                                         
*                                                                               
DR340    GOTO1 =A(PCTCOMP),RR=RELO CALCULATE PCT TO FINAL                       
         BE    *+14                                                             
         MVC   60(4,R2),=C'HIGH'                                                
         B     DR360                                                            
         LTR   R5,R5                                                            
         BZ    DR360                                                            
         EDIT  (R5),(6,58(R2)),1                                                
*                                                                               
DR360    DS   0H                                                                
         LR    RF,RA               CHECK PROFILE: SUPPRESS BUDGET?              
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'80'      FIRST BIT ON?                                
         BO    DR420               YES - SKIP BUDGET DISPLAY                    
*                                                                               
         CLI   TWAACCS,C'$'        STATION SIDE?                                
         BNE   DR370                NO                                          
         TM    SVPGPBIT,X'40'      YES - SUPPRESS BUDGET?                       
         BO    DR420               YES - SKIP BUDGET DISPLAY                    
*                                                                               
         DROP  RF                                                               
*                                                                               
DR370    ICM   R3,15,12(R6)        CURRENT BUDGET                               
         LTR   R5,R3                                                            
         BZ    DR400                                                            
         CLI   ROUNDSW,C'Y'                                                     
         BNE   DR380                                                            
         BAS   RE,ROUND                                                         
         BZ    DR400                                                            
*                                                                               
DR380    EDIT  (R5),(10,65(R2)),FLOAT=-                                         
         SPACE                                                                  
DR400    GOTO1 =A(PCTCOMP),RR=RELO CALCULATE PCT TO BUDGET                      
         BE    *+14                                                             
         MVC   78(4,R2),=C'HIGH'                                                
         B     DR420                                                            
         LTR   R5,R5                                                            
         BZ    DR420                                                            
         EDIT  (R5),(6,76(R2)),1                                                
*                                                                               
DR420    OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         CLI   LASTLN,C'Y'         TEST LAST LINE                               
         BE    DRX                 YES-DONE                                     
         LA    R2,86(R2)           NO-NEXT SCREEN LINE                          
*                                                                               
DR440    LM    R3,R4,SVREGS        NEXT MONTH                                   
         LA    R4,MONTOTLN(,R4)                                                 
         BCT   R3,DR150                                                         
*                                                                               
         XC    MONTOTS+4(24),MONTOTS+4                                          
*                                                                               
         CLI   QPY,C'Y'            YTD GETS NO TOTAL LINE                       
         BE    DRX                                                              
         MVC   8(6,R2),=C'TOTALS'                                               
         MVI   LASTLN,C'Y'                                                      
         LA    R6,TOTALS                                                        
         B     DR190               FORMAT THE TOTALS LINE                       
*                                                                               
DRX      CLI   ERREX2SW,C'Y'                                                    
         BNE   DRXIT                                                            
         CLI   ACTNUM,ACTLIST      WARNING MESSAGE ONLY FOR LIST                
         BNE   DRXIT                                                            
         L     R1,=A(WARNING-1)                                                 
         XC    CONHEAD,CONHEAD                                                  
         MVC   CONHEAD+L'WARNING(8),WARNDATE                                    
         L     R2,AFSTFLD                                                       
         B     USRERRMA                                                         
*                                                                               
DRXIT    XIT1                                                                   
*                                                                               
DRERR    MVI   ERROR,NODATA                                                     
         LA    R2,CONRECH                                                       
         GOTO1 ERREX                                                            
         EJECT                                                                  
* CK IF KEY CHANGE IS END, OR NEEDS NEXT SET MEMBER *                           
* IF ANY ARE UNEQUAL, END OF LIST (NE RET CODE)     *                           
         SPACE                                                                  
CSETD    NTR1                                                                   
         SPACE                                                                  
CSETD010 CLC   KEY(ROKDTLVL-ROKEY),KEYSAVE  TEST CORRECT RECORD TYPE            
         BNE   CSETDX                        NO, AT END                         
         SPACE                                                                  
         LA    R2,KEY+3                                                         
         LA    RE,KEY+6                                                         
         LA    RF,KEYSAVE+6                                                     
         SPACE                                                                  
CSETD020 LA    R0,3                                                             
         LA    R1,SET1CDE                                                       
         LR    R4,R9                                                            
         AHI   R4,SET1TAB-SYSD                                                  
         SPACE                                                                  
CSETD040 CLC   0(1,R2),0(R1)       THIS A SET CODE                              
         BE    CSETD060                                                         
         LA    R1,L'SETCDE(,R1)                                                 
         LA    R4,L'SET1TAB(,R4)                                                
         BCT   R0,CSETD040                                                      
         SPACE                                                                  
         CLC   0(8,RE),0(RF)       CHANGE IN NON-SET KEY                        
         BNE   CSETDX               YES                                         
         LA    RE,8(,RE)                                                        
         LA    RF,8(,RF)                                                        
         LA    R2,1(,R2)                                                        
         B     CSETD020                                                         
         SPACE                                                                  
* NOW CHECK OUT SET VALUE IN KEY                                                
         SPACE                                                                  
CSETD060 LR    R3,R1                                                            
         SPACE                                                                  
CSETD080 DS   0H                                                                
         ZIC   R5,1(R3)            GET LENGTH OF ENTRY                          
         BCTR  R5,0                                                             
         CLI   0(R4),0             END OF TABLE                                 
         BNE   CSETD090                                                         
         DC    H'0'                EMPTY TABLE - NO WAY                         
         SPACE                                                                  
CSETD090 CLI   0(R4),0             END OF TABLE                                 
         BE    CSETDNE              DONE                                        
         SPACE                                                                  
         CLI   0(R3),QLSTA         STATION COMPARE DIFFERENT                    
         BNE   CSETD100                                                         
         CLC   0(3,RE),0(R5)       1ST 3 OF STATION                             
         BNE   CSETD110                                                         
         LA    R1,4(,RE)                                                        
         CLI   3(RE),C'-'                                                       
         BE    CSETD096                                                         
         LA    R1,1(,R1)                                                        
         CLC   3(1,RE),3(R4)       CK 4TH LETTER                                
         BNE   CSETD110                                                         
CSETD096 CLI   4(R4),C' '          IF NOTHING, TV, ALL OKAY                     
         BNH   CSETDEQ                                                          
         CLC   0(1,R1),4(R4)       CK BAND                                      
         B     CSETD110                                                         
         SPACE                                                                  
CSETD100 EX    R5,CSETDCLC         SEE IF A MATCH                               
         SPACE                                                                  
CSETD110 BE    CSETD140                                                         
         BL    CSETD200                                                         
         LA    R4,1(R4,R5)                                                      
         B     CSETD100                                                         
         SPACE                                                                  
CSETD140 TM    SET1FLG-SET1CDE(R4),X'08'  EXCLUDE SET                           
         BZ    CSETDEQ                                                          
         MVI   7(RE),X'FF'                                                      
         B     CSETD240                                                         
         SPACE                                                                  
CSETD200 TM    SET1FLG-SET1CDE(R4),X'08'  EXCLUDE SET                           
         BO    CSETDEQ                                                          
         SPACE                                                                  
         EX    R5,CSETDMVC                                                      
         SPACE                                                                  
         CLI   0(R3),QLSTA         STATION COMPARE DIFFERENT                    
         BNE   CSETD240                                                         
         MVI   6(RE),0             FORCE SHORT STATIONS TO NULL                 
         LA    R1,3(,RE)                                                        
         CLI   3(RE),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(R4)                                                    
         CLI   1(R1),C' '                                                       
         BH    CSETD220                                                         
         MVC   1(2,R1),=C'TV'                                                   
         B     CSETD240                                                         
CSETD220 MVI   2(R1),C'M'                                                       
         SPACE                                                                  
CSETD240 XC    8(8,RE),8(RE)                                                    
         XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         MVC   KEYSAVE,KEY                                                      
         GOTO1 HIGH                                                             
         MVI   TRCTYPE,X'0B'                                                    
         GOTO1 =A(TRACE),RR=RELO                                                
         B     CSETD010                                                         
         SPACE                                                                  
CSETDNE  LTR   RB,RB                                                            
         B     CSETDX                                                           
         SPACE                                                                  
CSETDEQ  CR    RB,RB                                                            
CSETDX   XIT1                                                                   
CSETDCLC CLC   0(0,RE),0(R4)                                                    
CSETDMVC MVC   0(0,RE),0(R4)                                                    
         EJECT                                                                  
         LTORG                                                                  
SHD1     DC    CL66'  Prior .  Current    Cur .    Prior   Pct to  CurrC        
               ent  Pct to'                                                     
SHD2     DC    CL66'Billing    Billing  Pacing     Final   Final   BudgC        
               et   Budget'                                                     
BHD1     DC    CL66' Prior  .  Current   Prior.   Current   Cur    CurrC        
               ent .Pct to.'                                                    
BHD2     DC    CL66' Booked    Booked  Billing    Billing  Pacing  BudgC        
               et   Budget'                                                     
         DC    AL1(L'WARNING-1)                                                 
WARNING  DC    C'WARNING: RRGON DATA IS AS OF '                                 
*ARNING  DC    C'ER/0900 WARNING: RRGON DATA IS AS OF '                         
         DROP  RB,RC                                                            
         EJECT                                                                  
DOLLPRNT NMOD1 0,**LLRT**                                                       
*                                                                               
*        ROUTINE TO FORMAT ONLINE AND OFFLINE LISTS                             
*        R2 POINTS TO 'PRIOR BILLING' FIELD                                     
*                                                                               
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         MVI   QSKIPLR,0                                                        
         LA    R6,TOTALS                                                        
         ICM   R3,15,0(R6)         PRIOR BILLING                                
         LR    R5,R3                                                            
         BZ    DPRN0010                                                         
         CLI   ROUNDSW,C'Y'                                                     
         BNE   DPRN0006                                                         
         BAS   RE,ROUND                                                         
         BZ    DPRN0010                                                         
         SPACE                                                                  
DPRN0006 EDIT  (R5),(11,0(R2)),FLOAT=-                                          
*                                                                               
DPRN0010 ICM   R4,15,4(R6)         CURRENT BILLING                              
         BZ    DPRN0020                                                         
         LR    R5,R4                                                            
         CLI   ROUNDSW,C'Y'                                                     
         BNE   DPRN0014                                                         
         BAS   RE,ROUND                                                         
         BZ    DPRN0020                                                         
         SPACE                                                                  
DPRN0014 EDIT  (R5),(10,11(R2)),FLOAT=-                                         
*                                                                               
         GOTO1 =A(PCTCOMP),RR=RELO CURRENT PACING                               
         BE    *+14                                                             
         MVC   24(4,R2),=C'HIGH'                                                
         B     DPRN0020                                                         
         SPACE                                                                  
         LTR   R5,R5                                                            
         BZ    DPRN0020                                                         
         EDIT  (R5),(6,23(R2)),1                                                
*                                                                               
*                                                                               
DPRN0020 TM    DISPFLAG,DISPBOOK                                                
         BZ    DPRN0026                                                         
         MVC   WORK(29),0(R2)                                                   
         MVC   19(29,R2),WORK                                                   
         MVC   0(19,R2),BLANKS                                                  
*                                                                               
         ICM   R3,15,16(R6)               PRIOR BOOKED                          
         BZ    DPRN0022                                                         
         EDIT  (R3),(9,0(R2)),FLOAT=-                                           
*                                                                               
DPRN0022 ICM   R3,15,20(R6)               CURRENT BOOKED                        
         BZ    DPRN0040                                                         
         SPACE                                                                  
         EDIT  (R3),(9,10(R2)),FLOAT=-                                          
         B     DPRN0040                                                         
*                                                                               
DPRN0026 ICM   R3,15,8(R6)         PRIOR FINAL                                  
         BZ    DPRN0030                                                         
         LR    R5,R3                                                            
         CLI   ROUNDSW,C'Y'                                                     
         BNE   DPRN0028                                                         
         BAS   RE,ROUND                                                         
         BZ    DPRN0030                                                         
         SPACE                                                                  
DPRN0028 DS   0H                                                                
         EDIT  (R5),(10,29(R2)),FLOAT=-                                         
*                                                                               
DPRN0030 DS   0H                                                                
         GOTO1 =A(PCTCOMP),RR=RELO                                              
         BE    *+14                                                             
         MVC   42(4,R2),=C'HIGH'                                                
         B     DPRN0040                                                         
         SPACE                                                                  
         LTR   R5,R5                                                            
         BZ    DPRN0040                                                         
         EDIT  (R5),(6,41(R2)),1                                                
*                                                                               
DPRN0040 DS   0H                                                                
         LR    RF,RA               CHECK PROFILE: SUPPRESS BUDGET?              
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'80'      FIRST BIT ON?                                
         BO    DPRN0070            YES - SKIP BUDGET DISPLAY                    
*                                                                               
         CLI   TWAACCS,C'$'        STATION SIDE?                                
         BNE   *+12                 NO                                          
         TM    SVPGPBIT,X'40'      YES - SUPPRESS BUDGET?                       
         BO    DPRN0070            YES - SKIP BUDGET DISPLAY                    
*                                                                               
         DROP  RF                                                               
*                                                                               
         ICM   R3,15,12(R6)                                                     
         SPACE                                                                  
         CLC   LISTAR(6),=C'TOTALS'  THIS TOTALS LINE ONLY                      
         BE    DPRN0044                                                         
         SPACE                                                                  
         CLC   LISTAR+2(6),=C'TOTALS'  THIS TOTALS LINE ONLY (STEREO)           
         BE    DPRN0044                                                         
         SPACE                                                                  
         MVI   RGEHD2+65,C' '      COMPANY BUDGET FLAG                          
         OI    RGEHD2H+6,X'80'     FORCE TRANS                                  
         SPACE                                                                  
         CLI   COMPBUDG,C'Y'       RETRIEVE COMPANY BUDGET RECORD?              
         BNE   DPRN0044             NO  - SKIP IT                               
         SPACE                                                                  
         LR    RF,RA               CHECK PROFILE: SUPPRESS BUDGET?              
         AHI   RF,RRGPROFS-CONHEADH                                             
         USING SVDSECT,RF                                                       
         TM    SVPGPBIT,X'10'      FOURTH BIT ON?                               
         BZ    DPRN0044             NO, BYPASS COMPANY/OFFICE BUDGET            
         DROP  RF                                                               
         SPACE                                                                  
         CLI   QLIST,QLOFF         FOR OFFICE, OKAY                             
         BE    DPRN0043                                                         
         CLI   QLIST,QLGRP         FOR GRP/SUB LIST ONLY                        
         BNE   DPRN0044                                                         
         SPACE                                                                  
         CLI   QGROUP+1,C' '       IF GRP-SUB AND SUB-REP                       
         BH    DPRN0042             ONLY K3 GETS CO BUD                         
         LA    R0,K3IRPCT          CHECK JOINT INTEREP/KATZ                     
         LA    R1,K3IRBUDS         BUDGET COMPANY TABLE                         
         SPACE                                                                  
DPRN0041 EQU   *                                                                
         MVC   HALF,4(R1)          SAVE MASTER REP FROM TABLE                   
*                                     FOR COBUD'S USE                           
         CLC   AGENCY,2(R1)                                                     
         BE    DPRN0043            SUB-REP FOUND IN LIST                        
         LA    R1,6(,R1)                                                        
         BCT   R0,DPRN0041                                                      
*                                  SUB-REP NOT IN LIST                          
DPRN0042 EQU   *                                                                
         MVC   HALF,AGENCY         SAVE REP FOR COBUD'S USE                     
         CLC   AGENCY,=C'IR'       IR   USES COMPANY BUDGET                     
         BE    DPRN0043                                                         
         CLC   AGENCY,=C'K3'       KATZ USES COMPANY BUDGET                     
         BNE   DPRN0044                                                         
         SPACE                                                                  
DPRN0043 DS   0H                                                                
*                                                                               
*                                  HALF CONTAINS EITHER THE REP FOUND           
*                                  IN THE TABLE, OR THE SIGNON                  
*                                                                               
         BAS   RE,COBUD            GET COMPANY BUD IF ANY                       
         SPACE                                                                  
DPRN0044 DS   0H                                                                
**       LTR   R5,R3                                                            
**       BZ    DPRN0050                                                         
         SPACE                                                                  
         STCM  R3,15,12(R6)        SAVE FOR COMPANY BUDGETS                     
*                                     EVEN IF ZERO                              
         LTR   R5,R3                                                            
         BZ    DPRN0050                                                         
         SPACE                                                                  
         CLI   ROUNDSW,C'Y'                                                     
         BNE   DPRN0046                                                         
         BAS   RE,ROUND                                                         
         BZ    DPRN0050                                                         
         SPACE                                                                  
DPRN0046 EDIT  (R5),(10,48(R2)),FLOAT=-                                         
*                                                                               
DPRN0050 DS   0H                                                                
         GOTO1 =A(PCTCOMP),RR=RELO                                              
         BE    *+14                                                             
         MVC   60(4,R2),=C'HIGH'                                                
         B     DPRN0070                                                         
         LTR   R5,R5                                                            
         BZ    DPRN0070                                                         
         EDIT  (R5),(6,59(R2)),1                                                
*                                                                               
DPRN0070 DS   0H                                                                
*        CLC   LISTAR(6),=C'TOTALS'                                             
*        BNE   DPRN0074                                                         
*        CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
*        BNE   DPRN0074                                                         
*        EDIT  TOTLCT,(9,LISTAR+57)                                             
         SPACE                                                                  
*PRN0074 DS    0H                                                               
         CLI   FULLSTER,C'Y'       THIS FULL STEREO                             
         BNE   DPRNX                                                            
         SPACE                                                                  
         CLC   LISTAR+2(6),=C'TOTALS'  THIS TOTALS LINE ONLY (STEREO)           
         BE    DPRNX                                                            
         SPACE                                                                  
         LA    R1,CODETABL                                                      
         LHI   R0,CODETBLN                                                      
         AR    R0,R1                                                            
         SH    R0,=H'12'           ALLOW ROOM FOR THIS ENTRY                    
         ZIC   RF,LSTTYPLN                                                      
         BCTR  RF,0                                                             
         CLI   QLIST,QLSTA         IF STATION, STORING 5                        
         BNE   DPRN0080                                                         
         LA    RF,4                                                             
         SPACE                                                                  
DPRN0080 CLI   BOOKSIZE(R1),0                                                   
         BE    DPRN0084                                                         
         LA    R1,1+BOOKSIZE(R1,RF)                                             
         CR    R1,R0                                                            
         BL    DPRN0080                                                         
         SPACE                                                                  
*        MVI   QSKIPLR,X'FF'       SET TABLE AT END FLAG                        
         SPACE                                                                  
* NEED TO WRITE TWAN WITH CODETABL AND CLEAR FOR MORE ENTRIES *                 
         SPACE                                                                  
         CLI   TMPSTRCT,MAXPAGE    MAX PAGE                                     
         BL    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         ST    RF,PARAS            SAVE DATA SIZE                               
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    SET TERMINAL NUMBER                          
         ZIC   R1,TMPSTRCT                                                      
         LTR   R1,R1                                                            
         BNZ   *+8                                                              
         LA    R1,STRPAGE          START PAGE +1                                
         LA    R1,1(,R1)                                                        
         STC   R1,TMPSTRCT                                                      
         STC   R1,DMCB+8           SET PAGE                                     
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,CODETABL                     
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*        MVC   LISTAR+10(23),=C'TEMPSTR PAGE XX WRITTEN'                        
*        ZIC   R0,TMPSTRCT                                                      
*        CVD   R0,DUB                                                           
*        OI    DUB+7,X'0F'                                                      
*        UNPK  LISTAR+23(2),DUB+6(2)                                            
         SPACE                                                                  
* NOW CLEAR CODETABL FOR NEXT PAGE                                              
         SPACE                                                                  
         LA    R0,CODETABL                                                      
         LHI   R1,CODETBLN                                                      
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         SPACE                                                                  
         L     RF,PARAS            RESTORE DATA SIZE                            
         LA    R1,CODETABL+BOOKSIZE                                             
         SPACE                                                                  
DPRN0084 EX    RF,DPRNMVC                                                       
         MVC   0(3,R1),17(R6)      SAVE BOOKED                                  
         MVC   3(3,R1),21(R6)      SAVE BOOKED                                  
         SPACE                                                                  
         CLI   QLIST,QLSTA                                                      
         BNE   DPRNX                                                            
         XC    BOOKSIZE+4(2,R1),BOOKSIZE+4(R1)                                  
         LA    RE,LSTVAL+5                                                      
         CLI   LSTVAL+3,C'-'                                                    
         BNE   DPRN0090                                                         
         BCTR  RE,0                                                             
         MVI   BOOKSIZE+3(R1),C' '                                              
         SPACE                                                                  
DPRN0090 MVC   BOOKSIZE+4(1,R1),0(RE)                                           
         SPACE                                                                  
DPRNX    XIT1                                                                   
         SPACE                                                                  
DPRNMVC  MVC   BOOKSIZE(0,R1),LSTVAL                                            
         DROP  RB,RC                                                            
         EJECT                                                                  
*                                                                               
*        ROUTINE TO CALCULATE A PERCENTAGE                                      
*        REG 4 AS A PERCENT OF R3 - RESULT IN R5                                
*        CONDITION CODE SET TO NOT EQUAL IF PCT IS GT 999                       
*                                                                               
PCTCOMP  NMOD1 0,**PCTCO*                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         SPACE                                                                  
         SR    R5,R5                                                            
         LTR   R3,R3                                                            
         BZ    PCTCX                                                            
*        ST    R4,DUB              SAVE R4                                      
         CVD   R3,PARAS                                                         
         MP    PARAS(8),=P'10'                                                  
         CVD   R4,PARAS+8                                                       
         CP    PARAS(8),PARAS+8(8)                                              
         BNH   PCTC010             PCT GT 999                                   
         LA    R5,2000                                                          
         MR    R4,R4                                                            
         DR    R4,R3                                                            
         LTR   R5,R5                                                            
         BM    *+8                                                              
         A     R5,=F'1'                                                         
         SRA   R5,1                                                             
*        L     R4,DUB              RESTORE R4                                   
         CR    RB,RB                                                            
         B     PCTCX                                                            
*                                                                               
PCTC010  LTR   RB,RB               CC NE                                        
         SPACE                                                                  
PCTCX    XIT1  REGS=(R5)                                                        
         DROP  RB,RC                                                            
         EJECT                                                                  
* GET COMPANY BUDGETS FOR GRP/SUB                                               
         SPACE                                                                  
COBUD    NMOD1 0,**COBUD*                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         SPACE                                                                  
         SR    R3,R3               SET BUDGET TO ZERO                           
         SPACE                                                                  
         BAS   RE,SETREPFL         SET UP REP FILE VALUES                       
         MVC   ELEM,KEYSAVE                                                     
         SPACE                                                                  
         XC    KEY,KEY             CLEAR KEY                                    
         MVI   KEY,X'13'           SET BUDGET RECORD TYPE                       
         SPACE                                                                  
         CLI   QLIST,QLGRP         LIST = G/S                                   
         BNE   COBUD040                                                         
         SPACE                                                                  
         SPACE                                                                  
         LA    R0,K3GRPCT                                                       
         LA    R1,K3GRPTBL         SET TABLE TO KATZ                            
         CLC   =C'K3',HALF         WAS SIGNON 'K3', OR A K3 SUBREP?             
         BE    COBUD020            YES                                          
         LA    R0,IRGRPCT          NO                                           
         LA    R1,IRGRPTBL         SET TABLE TO INTEREP                         
         SPACE                                                                  
COBUD020 CLC   0(2,R1),LSTVAL                                                   
         BE    COBUD024                                                         
         LA    R1,4(,R1)                                                        
         BCT   R0,COBUD020                                                      
         B     COBUDX                                                           
COBUD024 DS   0H                                                                
         MVC   KEY+16(2),2(R1)     GET REP CODE                                 
         B     COBUD044                                                         
         SPACE                                                                  
COBUD040 DS   0H                                                                
         MVC   KEY+16(2),AGENCY    GET REP CODE                                 
         SPACE                                                                  
         CLI   QLIST,QLOFF                                                      
         BNE   COBUD044                                                         
         SPACE                                                                  
         MVC   KEY+25(2),LISTAR    GET OFFICE CODE                              
         SPACE                                                                  
COBUD044 DS   0H                                                                
         MVC   FULL(2),QSTART                                                   
         MVI   FULL+2,15                                                        
         SPACE                                                                  
         CLI   YRSW,C'P'           THIS PRIOR YEAR                              
         BNE   *+12                                                             
         SR    R0,R0                                                            
         IC    R0,FULL                                                          
         BCTR  R0,0                                                             
         STC   R0,FULL                                                          
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,FULL),(0,WORK)                                    
         MVC   KEY+18(2),WORK                                                   
         SPACE                                                                  
*        EDIT  (R0),(2,KEY+18),FILL=0                                           
*                                  INSERT YEAR OF BUDGET                        
         MVC   KEY+20(5),=C'C*MP ' INSERT 'MASTER BUDGET STATION'               
         GOTO1 HIGH                RETRIEVE BUDGET RECORD                       
         CLC   KEY(27),KEYSAVE     KEY FOUND?                                   
         BNE   COBUD100             NO  -                                       
         SPACE                                                                  
         MVI   RGEHD2+65,C'*'                                                   
         OI    RGEHD2H+6,X'80'     FORCE TRANS                                  
         SPACE                                                                  
         GOTO1 GETREC                                                           
         SPACE                                                                  
*        GET MONTHLY BUDGETS FROM BUDGET RECORD AND TOTAL                       
*        INTO R3 THIS OVERRIDES THE BUDGET VALUES TAKEN FROM                    
*        RRGON RECORDS.                                                         
*                                                                               
         L     R6,AIO2             SET A(IO AREA W/ BUDGET RECORD)              
         USING RBUDD,R6                                                         
         LA    R6,RBUDSTA          SET A(STATION BUDGETS)                       
         DROP  R6                                                               
         ZIC   RF,QSTART+1         BUDGET START MONTH                           
         BCTR  RF,0                MAKE ZERO RELATIVE                           
         SLA   RF,2                MULTIPLY BY 4 (FULL WORD LENGTH)             
         AR    R6,RF               SET D(STATION BUDGETS)                       
         ZIC   RF,QSTART+1         BUDGET START MONTH                           
         ZIC   RE,QEND+1           BUDGET END   MONTH                           
         CR    RE,RF               END VS START:  OUT OF YEAR?                  
         BNL   COBUD050            START <= END: NO ADJUSTMENT                  
         LA    RE,12(RE)           END < START:  OUT OF YEAR                    
COBUD050 EQU   *                                                                
         SR    RE,RF               SUBTRACT START FROM END                      
         LA    RE,1(RE)            ADD 1 FOR DIFFERENCING                       
COBUD080 EQU   *                                                                
         A     R3,0(R6)                                                         
*                                  ADD BUDGET FROM REC TO YEARLY TOTAL          
         LA    R6,4(R6)            BUMP TO NEXT RECORD ENTRY                    
         BCT   RE,COBUD080         GO BACK FOR NEXT                             
         GOTO1 =A(BUDGET),DMCB,(R3),RR=RELO                                     
COBUD100 EQU   *                                                                
         BAS   RE,RSETRRGO         SWITCH BACK TO RRGON FILE VALUES             
         MVC   KEYSAVE,ELEM                                                     
         SPACE                                                                  
COBUDX   XIT1  REGS=(R3)                                                        
         SPACE                                                                  
* KATZ EQUATES FOR GROUP TO REP CODES                                           
* 1ST 2 ARE GROUP CODE, NEXT CODE REP CODE                                      
         SPACE                                                                  
K3GRPTBL DS    0H                                                               
         DC    CL4'RARS'                                                        
         DC    CL4'RBBF'                                                        
         DC    CL4'RCCR'                                                        
         DC    CL4'REEA'                                                        
         DC    CL4'RHKF'                                                        
         DC    CL4'RLNU'                                                        
         DC    CL4'RKKU'                                                        
         DC    CL4'RMS3'                                                        
         DC    CL4'RPQD'                                                        
         DC    CL4'RSK4'                                                        
K3GRPCT  EQU   (*-K3GRPTBL)/4                                                   
         SPACE                                                                  
* IRNY EQUATES FOR GROUP TO REP CODES                                           
* 1ST 2 ARE GROUP CODE, NEXT CODE REP CODE                                      
         SPACE                                                                  
IRGRPTBL DS    0H                                                               
         DC    CL4'RLAQ'                                                        
         DC    CL4'RFCN'                                                        
         DC    CL4'RDD4'                                                        
         DC    CL4'RAIB'                                                        
         DC    CL4'RIIF'                                                        
         DC    CL4'RNI2'                                                        
         DC    CL4'RCI8'                                                        
         DC    CL4'T L7'                                                        
         DC    CL4'RMMG'                                                        
         DC    CL4'RXNX'                                                        
         DC    CL4'RBRM'                                                        
         DC    CL4'RAS1'                                                        
         DC    CL4'RUUO'                                                        
IRGRPCT  EQU   (*-IRGRPTBL)/4                                                   
         SPACE                                                                  
K3IRBUDS DS    0H                                                               
         DC    CL4'RARSK3'         JOINT TABLE USED FOR INTEREP                 
         DC    CL4'RBBFK3'         AND KATZ COMPANY BUDGET PROCESSING           
         DC    CL4'RCCRK3'                                                      
         DC    CL4'REEAK3'                                                      
         DC    CL4'RHKFK3'                                                      
         DC    CL4'RLNUK3'                                                      
         DC    CL4'RKKUK3'                                                      
         DC    CL4'RMS3K3'                                                      
         DC    CL4'RPQDK3'                                                      
         DC    CL4'RSK4K3'                                                      
         DC    CL4'RLAQIR'                                                      
         DC    CL4'RFCNIR'                                                      
         DC    CL4'RDD4IR'                                                      
         DC    CL4'RAIBIR'                                                      
         DC    CL4'RIIFIR'                                                      
         DC    CL4'RNI2IR'                                                      
         DC    CL4'RCI8IR'                                                      
         DC    CL4'T L7IR'                                                      
         DC    CL4'RMMGIR'                                                      
         DC    CL4'RXNXIR'                                                      
         DC    CL4'RBRMIR'                                                      
         DC    CL4'RAS1IR'                                                      
         DC    CL4'RUUOIR'                                                      
K3IRPCT  EQU   (*-K3IRBUDS)/6                                                   
         DROP  RB,RC                                                            
         EJECT                                                                  
BUDGET   NMOD1 0,**BUDG**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         CLI   OFFLINE,C'Y'                                                     
         BNE   BUDGETX                                                          
         MVC   FULL,0(R1)          SET VALUE OF BUDGET                          
         TM    DISPFLAG,OPTRACE                                                 
         BZ    BUDGETX                                                          
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         MVC   P,SPACES                                                         
         MVC   P+1(7),=C'BUDGET:'                                               
         L     R6,AIO2             SET A(IO AREA W/ BUDGET RECORD)              
         USING RBUDD,R6                                                         
         MVC   P+10(27),RBUDREC    DISPLAY KEY                                  
         EDIT  FULL,(12,P+40)                                                   
         DROP  R6                                                               
         GOTO1 SPOOL,DMCB,(R3)                                                  
BUDGETX  XIT1                                                                   
         SPACE                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*        HEADLINE ROUTINE FOR OFFLINE REPORT                                    
*                                                                               
HDHK     NMOD1 0,**HDHK**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         MVC   DUB(2),QSTART                                                    
         MVI   DUB+2,1                                                          
         CLI   YRSW,C'P'           THIS PRIOR YR                                
         BNE   HH004                                                            
         ZIC   R0,DUB                                                           
         BCTR  R0,0                                                             
         STC   R0,DUB                                                           
         SPACE                                                                  
HH004    DS    0H                                                               
         GOTO1 DATCON,DMCB,(3,DUB),(6,H3+54)    PERIOD START                    
         MVC   DUB(2),QEND                                                      
         CLI   YRSW,C'P'           THIS PRIOR YR                                
         BNE   HH006                                                            
         ZIC   R0,DUB                                                           
         BCTR  R0,0                                                             
         STC   R0,DUB                                                           
         SPACE                                                                  
HH006    DS    0H                                                               
         GOTO1 (RF),(R1),(3,DUB),(6,H3+64)      PERIOD END                      
         MVC   H8+18(8),RGEHD1                  OFFICE/STATION/ETC              
         LA    R4,H6+2                                                          
         LR    R5,R4                                                            
         SPACE                                                                  
         CLI   LSTTYPE,QLSTA                    TEST LISTING STATIONS           
         BE    HH010                             YES                            
         OC    QSTA,QSTA                        NO-TEST STATION FILTER          
         BZ    HH010                                                            
         MVC   0(8,R4),=C'STATION='             YES-FORMAT STATION              
         MVC   8(L'QSTA,R4),QSTA                                                
         LA    R4,8+L'QSTA(,R4)                                                 
*                                                                               
HH010    DS   0H                                                                
         CLI   LSTTYPE,QLREG                    TEST LISTING TYPE               
         BE    HH020                             YES                            
         OC    QREGION,QREGION                  NO-TEST FILTER                  
         BZ    HH020                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(7,R4),=C'REGION='             YES-FORMAT                       
         MVC   8(L'QREGION,R4),QREGION                                          
         LA    R4,8+L'QREGION(,R4)                                              
*                                                                               
HH020    DS   0H                                                                
         CLI   LSTTYPE,QLOFF                    TEST LISTING TYPE               
         BE    HH030                             YES                            
         OC    QOFF,QOFF                        NO-TEST FILTER                  
         BZ    HH030                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(7,R4),=C'OFFICE='             YES-FORMAT                       
         MVC   7(L'QOFF,R4),QOFF                                                
         LA    R4,7+L'QOFF(,R4)                                                 
*                                                                               
HH030    DS   0H                                                                
         CLI   LSTTYPE,QLTEM                    TEST LISTING TYPE               
         BE    HH040                             YES                            
         OC    QTEAM,QTEAM                      NO-TEST FILTER                  
         BZ    HH040                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(5,R4),=C'TEAM='             YES-FORMAT                         
         MVC   5(L'QTEAM,R4),QTEAM                                              
         LA    R4,5+L'QTEAM(,R4)                                                
*                                                                               
HH040    DS   0H                                                                
         CLI   LSTTYPE,QLADV                    TEST LISTING TYPE               
         BE    HH050                             YES                            
         OC    QADV,QADV                        NO-TEST FILTER                  
         BZ    HH050                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(4,R4),=C'ADV='             YES-FORMAT                          
         MVC   4(L'QADV,R4),QADV                                                
         LA    R4,4+L'QADV(,R4)                                                 
*                                                                               
HH050    DS   0H                                                                
         CLI   LSTTYPE,QLAGY                    TEST LISTING TYPE               
         BE    HH060                             YES                            
         CLI   LSTTYPE,QLAGENCY                 TEST LISTING TYPE               
         BE    HH060                             YES                            
         OC    QAGY,QAGY                        NO-TEST FILTER                  
         BZ    HH060                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(4,R4),=C'AGY='             YES-FORMAT                          
         MVC   4(L'QAGY,R4),QAGY                                                
         LA    R4,4+L'QAGY(,R4)                                                 
*                                                                               
HH060    DS   0H                                                                
         CLI   LSTTYPE,QLAFF                    TEST LISTING TYPE               
         BE    HH070                             YES                            
         OC    QAFF,QAFF                        NO-TEST FILTER                  
         BZ    HH070                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(4,R4),=C'AFF='             YES-FORMAT                          
         MVC   4(L'QAFF,R4),QAFF                                                
         LA    R4,4+L'QAFF(,R4)                                                 
*                                                                               
HH070    DS   0H                                                                
         CLI   LSTTYPE,QLCLS                    TEST LISTING TYPE               
         BE    HH080                             YES                            
         OC    QCLASS,QCLASS                    NO-TEST FILTER                  
         BZ    HH080                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(6,R4),=C'CLASS='             YES-FORMAT                        
         MVC   6(L'QCLASS,R4),QCLASS                                            
         LA    R4,6+L'QCLASS(,R4)                                               
*                                                                               
HH080    DS   0H                                                                
         CLI   LSTTYPE,QLCAT                    TEST LISTING TYPE               
         BE    HH090                             YES                            
         OC    QCTGY,QCTGY                      NO-TEST FILTER                  
         BZ    HH090                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(4,R4),=C'CLS='             YES-FORMAT                          
         MVC   4(L'QCTGY,R4),QCTGY                                              
         LA    R4,4+L'QCTGY(,R4)                                                
*                                                                               
HH090    DS   0H                                                                
         CLI   LSTTYPE,QLSTY                    TEST LISTING TYPE               
         BE    HH100                             YES                            
         OC    QSTATY,QSTATY                    NO-TEST FILTER                  
         BZ    HH100                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(7,R4),=C'STATYP='         YES-FORMAT                           
         MVC   7(L'QSTATY,R4),QSTATY                                            
         LA    R4,7+L'QSTATY(,R4)                                               
*                                                                               
HH100    DS   0H                                                                
         CLI   LSTTYPE,QLTVB                    TEST LISTING OFFICES            
         BE    HH110                             YES                            
         OC    QTVB,QTVB                        NO-TEST OFFICE FILTER           
         BZ    HH110                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(4,R4),=C'TVB='              YES-FORMAT OFFICE                  
         MVC   4(L'QTVB,R4),QTVB                                                
         LA    R4,4+L'QTVB(,R4)                                                 
*                                                                               
HH110    DS   0H                                                                
         CLI   LSTTYPE,QLOWN                    TEST LISTING OFFICES            
         BE    HH120                             YES                            
         OC    QOWNER,QOWNER                    NO-TEST OFFICE FILTER           
         BZ    HH120                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(6,R4),=C'OWNER='            YES-FORMAT OFFICE                  
         MVC   6(L'QOWNER,R4),QOWNER                                            
         LA    R4,6+L'QOWNER(,R4)                                               
*                                                                               
HH120    DS   0H                                                                
         CLI   LSTTYPE,QLRNK                    TEST LISTING OFFICES            
         BE    HH130                             YES                            
         OC    QRANK,QRANK                      NO-TEST OFFICE FILTER           
         BZ    HH130                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(5,R4),=C'RANK='            YES-FORMAT OFFICE                   
         MVC   5(L'QRANK,R4),QRANK                                              
         LA    R4,5+L'QRANK(,R4)                                                
*                                                                               
HH130    DS   0H                                                                
         CLI   LSTTYPE,QLCON                    TEST LISTING OFFICES            
         BE    HH140                             YES                            
         OC    QCONTY,QCONTY                    NO-TEST OFFICE FILTER           
         BZ    HH140                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(8,R4),=C'CONTYPE='            YES-FORMAT OFFICE                
         MVC   8(L'QCONTY,R4),QCONTY                                            
         LA    R4,8+L'QCONTY(,R4)                                               
*                                                                               
HH140    DS   0H                                                                
         CLI   LSTTYPE,QLMKT                    TEST LISTING OFFICES            
         BE    HH150                             YES                            
         OC    QMKT,QMKT                        NO-TEST OFFICE FILTER           
         BZ    HH150                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(7,R4),=C'MARKET='            YES-FORMAT OFFICE                 
         MVC   7(L'QMKT,R4),QMKT                                                
         LA    R4,7+L'QMKT(,R4)                                                 
*                                                                               
HH150    DS   0H                                                                
         CLI   LSTTYPE,QLDCT                    TEST LISTING DEV TYPE           
         BE    HH160                             YES                            
         OC    QDCT,QDCT                        NO-TEST DEV CON TYPE            
         BZ    HH160                                                            
         CR    R4,R5                                                            
         BE    *+12                                                             
         MVI   0(R4),C','                                                       
         LA    R4,1(,R4)                                                        
         SPACE                                                                  
         MVC   0(8,R4),=C'DEVTYPE='            YES-FORMAT DEV CON TYPE          
         MVC   8(L'QDCT,R4),QDCT                                                
         LA    R4,8+L'QDCT(,R4)                                                 
         SPACE                                                                  
HH160    DS   0H                                                                
         SR    RE,RE               TEST ANY OPTIONS                             
         ICM   RE,1,RRGOPTH+5                                                   
         BZ    HHXIT                                                            
         MVC   H7+34(8),=C'OPTIONS:'  YES - FORMAT THE OPTIONS                  
         BCTR  RE,0                                                             
         EX    RE,HHMVC                                                         
HHXIT    XIT1                                                                   
         SPACE                                                                  
HHMVC    MVC   H7+42(0),RRGOPT                                                  
         DROP  R3,RB,RC                                                         
         EJECT                                                                  
* GET STATION RECORD TO COMPARE GROUP/AFFIL/MARKET                              
         SPACE 2                                                                
GETSTA   NMOD1 0,**GETS**          GET STATION RECORD                           
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         BAS   RE,SETSTA                                                        
         SPACE                                                                  
         LA    R2,AGENCY                                                        
         L     R5,AIO2                                                          
         SPACE                                                                  
         CLI   0(R5),2             TEST RECORD ALREADY IN CORE                  
         BNE   GETSTA10                                                         
         SPACE                                                                  
         CLC   STATION,22(R5)                                                   
         BNE   GETSTA10            NO, GO GET IT                                
         SPACE                                                                  
         OC    QGROUP,QGROUP                                                    
         BZ    GETSTA06                                                         
         SPACE                                                                  
         CLC   AGENCY,=C'K3'       AND KATZ (FOR NOW)                           
         BNE   GETSTA06             NO - CAN'T GET STATION BY REP               
         SPACE                                                                  
         CLI   QGROUP,C'*'         GROUP A SET?                                 
         BE    *+12                 YES                                         
         CLI   QGROUP+1,C' '       GROUP(2 CHAR) ENTERED                        
         BNH   GETSTA06             NO                                          
         SPACE                                                                  
         LA    R3,QGROUP                                                        
         SPACE                                                                  
         CLI   QGROUP,C'*'         GROUP A SET?                                 
         BNE   GETSTA04                                                         
         LA    R0,3                                                             
         LA    R1,SET1CDE                                                       
         LR    R3,R9                                                            
         AHI   R3,SET1TAB-SYSD                                                  
GETSTA02 CLI   0(R1),QLGRP         THIS THE CODE                                
         BE    GETSTA03                                                         
         LA    R1,L'SETCDE(,R1)    NEXT CODE                                    
         LA    R3,L'SET1TAB(,R3)   NEXT TABLE                                   
         BCT   R0,GETSTA02                                                      
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
         SPACE                                                                  
GETSTA03 LA    RE,K3GRPCT                                                       
         L     RF,=A(K3GRPTBL)                                                  
         A     RF,RELO                                                          
GETSTA04 CLC   0(2,R3),0(RF)       CK GROUP VS TABLE                            
         BE    GETSTA05                                                         
         LA    RF,4(,RF)                                                        
         BCT   RE,GETSTA04                                                      
         SPACE                                                                  
         CLI   QGROUP,C'*'         GROUP A SET?                                 
         BNE   GETSTA06                                                         
         SPACE                                                                  
         LA    R3,2(,R3)           NEXT SET                                     
         CLI   0(R3),0             END OF SET?                                  
         BNE   GETSTA03                                                         
         B     GETSTA06                                                         
         SPACE                                                                  
GETSTA05 LA    R2,2(RF)            POINT TO KATZ REP CODE                       
         SPACE                                                                  
GETSTA06 CLC   0(2,R2),20(R5)      REP MUST MATCH                               
         BE    GETSTAX                                                          
         SPACE                                                                  
         OC    QGROUP,QGROUP                                                    
         BZ    GETSTA10                                                         
         SPACE                                                                  
         CLC   AGENCY,=C'K3'       AND KATZ (FOR NOW)                           
         BNE   GETSTA10             NO - CAN'T GET STATION BY REP               
         SPACE                                                                  
         CLI   QGROUP,C'*'         GROUP A SET?                                 
         BNE   GETSTA10                                                         
         SPACE                                                                  
         LA    R3,2(,R3)           NEXT SET                                     
         CLI   0(R3),0             END OF SET?                                  
         BNE   GETSTA03                                                         
         SPACE                                                                  
GETSTA10 BAS   RE,SETREPFL         REPFILE VALUES                               
         MVC   WORK,KEYSAVE                                                     
         MVI   BYTE,0                                                           
*                                                                               
GETSTA11 XC    KEY,KEY             SET STATION RECORD PASSIVE KEY               
         LA    R4,KEY                                                           
         USING RST2KEY,R4                                                       
         MVI   RST2KTYP,X'82'                                                   
         MVC   RST2KSTA,STATION                                                 
         MVC   RST2KREP,0(R2)      MOVE IN AGENCY                               
         SPACE                                                                  
         CLC   AGENCY,=C'K3'       AND KATZ (FOR NOW)                           
         BNE   GETSTA18             NO - CAN'T GET STATION BY REP               
         SPACE                                                                  
         OC    QGROUP,QGROUP       GROUP A FILTER                               
         BZ    GETSTA18             NO                                          
         SPACE                                                                  
         LA    R3,QGROUP                                                        
         SPACE                                                                  
         CLI   QGROUP,C'*'         GROUP A SET?                                 
         BNE   GETSTA13             NO                                          
         SPACE                                                                  
         LA    R0,3                                                             
         LA    R1,SET1CDE                                                       
         LR    R3,R9                                                            
         AH    R3,=AL2(SET1TAB-SYSD)                                            
GETSTA12 CLI   0(R1),QLGRP         THIS THE CODE                                
         BE    GETSTA13                                                         
         LA    R1,L'SETCDE(,R1)    NEXT CODE                                    
         LA    R3,L'SET1TAB(,R3)   NEXT TABLE                                   
         BCT   R0,GETSTA12                                                      
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
         SPACE                                                                  
GETSTA13 LA    RE,K3GRPCT                                                       
         L     RF,=A(K3GRPTBL)                                                  
         A     RF,RELO                                                          
         SPACE                                                                  
GETSTA14 CLC   0(2,R3),0(RF)                                                    
         BE    GETSTA15                                                         
         LA    RF,4(,RF)                                                        
         BCT   RE,GETSTA14                                                      
         SPACE                                                                  
         CLI   QGROUP,C'*'         GROUP A SET?                                 
         BNE   GETSTA18                                                         
         SPACE                                                                  
         LA    R3,2(,R3)           NEXT SET                                     
         CLI   0(R3),0             END OF SET?                                  
         BNE   GETSTA13                                                         
         B     GETSTA18                                                         
         SPACE                                                                  
GETSTA15 MVC   RST2KREP,2(RF)      MOVE IN KATZ REP CODE                        
         SPACE                                                                  
GETSTA18 GOTO1 HIGH                                                             
*                                                                               
         CLC   KEY(27),KEYSAVE                                                  
         BE    GETSTA60                                                         
*                                                                               
* NOW SEE IF STATION AND REP MATCH - MAY HAVE LEAVE DATE *                      
*                                                                               
GETSTA20 CLC   KEY(RST2KEND-RST2KEY),KEYSAVE   CHECK FOR KEY UP TO STA          
         BE    GETSTA40                                                         
         SPACE                                                                  
         CLI   QGROUP,C'*'         ENTERED GROUP AS SET                         
         BNE   GETSTA24                                                         
         SPACE                                                                  
         MVC   KEY,KEYSAVE                                                      
         SPACE                                                                  
         LA    R3,2(,R3)                                                        
         CLI   0(R3),0                                                          
         BNE   GETSTA13                                                         
         SPACE                                                                  
GETSTA24 BAS   RE,RSETRRGO                                                      
         MVC   KEYSAVE,WORK                                                     
         CR    RB,RD               SET COND CODE                                
         B     GETSTAX                                                          
         CLI   BYTE,0              TEST SECOND SEARCH                           
         BNE   *+14                                                             
         CLC   AGENCY,=C'SJ'       OR AGENCY IS SJR (SJR MUST GET               
         BNE   *+14                                  SJR RECORDS)               
         MVC   GERROR,=AL2(INVSTA) YES-RECORD NOT FOUND                         
         B     REPERR                                                           
         MVI   BYTE,1              NO-START SECOND SEARCH TO FIND               
         B     GETSTA11               FIRST POINTER                             
*                                                                               
GETSTA40 CLC   RST2KREP,0(R2)      AGENCIES MATCH - FINE                        
         BE    GETSTA60                                                         
         SPACE                                                                  
         CLI   QGROUP,C'*'         ENTERED GROUP AS SET                         
         BE    GETSTA50                                                         
         SPACE                                                                  
         CLI   BYTE,0              TEST FIRST SEARCH                            
         BE    GETSTA50            YES-TRY TO FIND AGENCY MATCH                 
         SPACE                                                                  
         CLC   RST2KREP,=C'SJ'     NO-SKIP SJR RECORDS                          
         BE    GETSTA50                                                         
         SPACE                                                                  
         BAS   RE,RSETRRGO                                                      
         MVC   KEYSAVE,WORK                                                     
         CR    RB,RD               SET COND CODE                                
         B     GETSTAX                                                          
         SPACE                                                                  
GETSTA50 GOTO1 SEQ                                                              
         B     GETSTA20                                                         
*                                                                               
GETSTA60 GOTO1 GETREC                                                           
         BAS   RE,RSETRRGO                                                      
         MVC   KEYSAVE,WORK                                                     
*                                                                               
         CR    R0,R0               SET GOOD COND CODE                           
*                                                                               
GETSTAX  DS   0H                                                                
         XIT1                                                                   
         DROP  R4                                                               
*                                                                               
SETSTA   MVC   STATION(4),0(R3)                                                 
         LA    RF,5(R3)                                                         
         CLI   STATION+3,C'-'                                                   
         BNE   *+10                                                             
         MVI   STATION+3,C' '                                                   
         BCTR  RF,0                                                             
         MVI   STATION+4,C' '                                                   
         CLI   0(RF),C'T'                                                       
         BER   RE                                                               
         CLI   0(RF),0                                                          
         BER   RE                                                               
         MVC   STATION+4(1),0(RF)                                               
         BR    RE                                                               
         DROP  RB,RC                                                            
*                                                                               
*                                                                               
*   'REFRESH' CHECKS FOR OPTION 'REFRESH' TO SET SWITCH TO GET                  
*      EXPANSIONS.  THIS IS A PRE-SCAN OF THE OPTION FIELD WHICH                
*      DOESN'T SET ANYTHING OTHER THAN THE 'REFRESH' FLAG.  I KNOW              
*      IT'S REDUNDANT, BUT WHAT THE HELL.                                       
*                                                                               
REFRESH  NMOD1 0,*REFR*                                                         
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         LA    R2,RRGOPTH          SET A(OPTIONS FIELD)                         
         MVI   QREFRESH,C'N'       SET 'REFRESH' TO NO                          
*                                                                               
*   OPTION SHOULD COME IN AS:  KEY=OPTION, WHERE                                
*        KEY=     IS L(I(S(T)))=                                                
*        OPTION   IS ANY VALID LIST ENTRY                                       
*        POSSIBLY FOLLOWED BY (OR PRECEDED BY) ',REFRESH'                       
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK),0                                    
         SR    R5,R5                                                            
         ICM   R5,1,4(R1)                                                       
         BZ    REFR0120                                                         
         SR    R3,R3                                                            
         LA    R4,BLOCK            SET TO A(SCANNER OUTPUT BLOCK)               
*                                                                               
REFR0040 DS   0H                                                                
         CLI   0(R4),0             ANYTHING ON LINE?                            
         BE    REFR0120            NO  - FINISHED                               
         CLI   0(R4),7             YES - FIRST HALF 7 CHARS LONG?               
         BNE   REFR0080            NO  - CHECK NEXT BUCKET                      
         CLC   =C'REFRESH',12(R4)  YES - IS IT 'REFRESH'?                       
         BNE   REFR0080            NO  - CHECK NEXT BUCKET                      
         MVI   QREFRESH,C'Y'       YES - SET FLAG                               
         B     REFR0120            EXIT ROUTINE                                 
*                                                                               
REFR0080 LA    R4,32(R4)                                                        
         BCT   R5,REFR0040         NEXT OPTION FIELD                            
REFR0120 DS   0H                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,RC                                                            
*                                                                               
*   'SETFILTS' PASSES BACK EXPANSIONS TO A STEREO REQUEST, WHEN                 
*      OPTION 'REFRESH' IS REQUESTED.                                           
*                                                                               
SETFILTS NMOD1 0,*FILT*                                                         
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         MVI   LISTNUM,1           SET LISTNUM TO EMPTY SCREEN                  
         MVC   LISTAR,BLANKS                                                    
*                                                                               
         LA    R0,CODETABL                                                      
         CLI   QLIST,QLMON                                                      
         BNE   *+8                                                              
         LA    R0,MONTOTS                                                       
         SR    R0,R9                                                            
         CLM   R0,7,CDTBADR+1      IF HIGHER, IN LIST OF LONG NAMES             
         BL    SFIL0300             GO DO MORE                                  
         XC    TOTALS(24),TOTALS                                                
         XC    LSTOTALS(24),LSTOTALS                                            
*                                                                               
         LA    R2,RGEDETH          SET A(1ST LINE DETAIL FIELD)                 
         ST    R2,ATHISLST         SET FIELD FOR ROUTINE USE                    
         OC    QSTA,QSTA           ANY STATION FILTER?                          
         BZ    SFIL0020            NO                                           
         MVC   LISTAR(09),=C'@@STATION'                                         
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QSTA),QSTA                                           
         MVC   LISTAR+25(20),EXMKTNAM                                           
         CLI   QSTA,C'*'           IF SET, SHOW SET NAME                        
         BNE   *+10                                                             
         MVC   LISTAR+15(7),RRGSTA                                              
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0020 DS   0H                                                                
         OC    QOFF,QOFF           ANY OFFICE  FILTER?                          
         BZ    SFIL0040            NO                                           
         MVC   LISTAR(09),=CL9'@@OFFICE'                                        
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QOFF),QOFF                                           
         MVC   LISTAR+25(20),EXOFFNAM                                           
         SPACE                                                                  
         CLI   QOFF,C'*'           SET                                          
         BNE   *+10                 NO                                          
         MVC   LISTAR+15(6),RRGOFF                                              
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         EJECT                                                                  
SFIL0040 DS   0H                                                                
         OC    QGROUP,QGROUP       ANY GROUP   FILTER?                          
         BZ    SFIL0060            NO                                           
         SPACE                                                                  
         CLI   RRGGRPH+5,0         IF GROUP NOT ENTERED, MUST BE COMBO          
         BE    SFIL0060             YES, DON'T SHOW FAKED GROUP                 
         SPACE                                                                  
         CLI   QLIST,QLGRP         IS THIS GROUP LIST                           
         BE    SFIL0060             YES, DON'T SHOW FAKED GROUP                 
*        BNE   *+12                                                             
         SPACE                                                                  
*        CLI   QGROUP+1,0          IF GRP/SUB ENTERED, DON'T PRT TWICE          
*        BNE   SFIL0060                                                         
         SPACE                                                                  
         MVC   LISTAR(09),=CL9'@@GROUP'                                         
         SPACE                                                                  
         CLI   QGROUP,C'*'         SET                                          
         BE    SFIL0042             YES                                         
         SPACE                                                                  
         CLI   QGROUP+1,0          GRP/SUB                                      
         BE    SFIL0044             NO                                          
         SPACE                                                                  
         MVC   LISTAR+4(4),=C'PSUB'                                             
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QGROUP),QGROUP                                       
         SPACE                                                                  
SFIL0042 MVC   LISTAR+25(20),EXGRPNAM                                           
         SPACE                                                                  
         MVC   LISTAR+15(6),RRGGRP                                              
         SPACE                                                                  
         B     SFIL0046                                                         
         SPACE                                                                  
SFIL0044 DS    0H                                                               
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QGROUP),QGROUP                                       
         SPACE                                                                  
         MVC   LISTAR+25(10),=C'TELEVISION'                                     
         CLI   LISTAR+15,C'T'      TELEVISION?                                  
         BE    SFIL0046            YES                                          
         SPACE                                                                  
         MVC   LISTAR+25(10),=CL10'MED A'                                       
         CLI   LISTAR+15,C'A'                                                   
         BE    SFIL0046            YES                                          
         MVC   LISTAR+25(10),=CL10'INTER'                                       
         SPACE                                                                  
         CLI   LISTAR+15,C'I'                                                   
         BE    SFIL0046            YES                                          
         MVC   LISTAR+25(10),=CL10'PETRY'                                       
         SPACE                                                                  
         CLI   LISTAR+15,C'P'      PETRY                                        
         BE    SFIL0046            YES                                          
         MVC   LISTAR+25(10),=CL10'RADIO'                                       
         SPACE                                                                  
SFIL0046 DS   0H                                                                
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0060 DS   0H                                                                
         OC    QREGION,QREGION     ANY REGION  FILTER?                          
         BZ    SFIL0080            NO                                           
         SPACE                                                                  
         MVC   LISTAR(09),=CL9'@@REGION'                                        
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QREGION),QREGION                                     
         MVC   LISTAR+25(20),EXRGNNAM                                           
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0080 DS   0H                                                                
         OC    QTEAM,QTEAM         ANY TEAM    FILTER?                          
         BZ    SFIL0100            NO                                           
         MVC   LISTAR(09),=CL9'@@TEAM'                                          
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QTEAM),QTEAM                                         
         MVC   LISTAR+25(10),EXTEMNAM                                           
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0100 DS   0H                                                                
         OC    QOWNER,QOWNER       ANY OWNER   FILTER?                          
         BZ    SFIL0120            NO                                           
         MVC   LISTAR(09),=CL9'@@OWNER'                                         
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QOWNER),QOWNER                                       
         MVC   LISTAR+25(20),EXOWNNAM                                           
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0120 DS   0H                                                                
         OC    QCLASS,QCLASS       ANY CLASS   FILTER?                          
         BZ    SFIL0140            NO                                           
         MVC   LISTAR(09),=CL9'@@CLASS'                                         
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QCLASS),QCLASS                                       
         MVC   LISTAR+25(30),EXCLSNAM                                           
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0140 DS   0H                                                                
         OC    QCTGY,QCTGY         ANY CATEGORY FILTER?                         
         BZ    SFIL0160            NO                                           
         MVC   LISTAR(09),=CL9'@@CTGRY'                                         
         SPACE                                                                  
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QCTGY),QCTGY                                         
         MVC   LISTAR+25(30),EXCTGNAM                                           
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0160 DS   0H                                                                
         OC    QCONTY,QCONTY       ANY CON TYPE FILTER?                         
         BZ    SFIL0180            NO                                           
         SPACE                                                                  
         MVC   LISTAR(09),=CL9'@@CONTYPE'                                       
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QCONTY),QCONTY                                       
         MVC   LISTAR+25(20),EXCTYNAM                                           
         SPACE                                                                  
         CLI   QCONTY,C'*'         SET                                          
         BNE   *+10                 NO                                          
         MVC   LISTAR+15(6),RRGCTY                                              
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0180 DS   0H                                                                
         OC    QMKT,QMKT           ANY MARKET FILTER?                           
         BZ    SFIL0200            NO                                           
         MVC   LISTAR(09),=CL9'@@MARKET'                                        
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QMKT),QMKT                                           
         MVC   LISTAR+25(20),EXMK2NAM                                           
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0200 DS   0H                                                                
         OC    QAGY,QAGY           ANY AGENCY FILTER?                           
         BZ    SFIL0220            NO                                           
         SPACE                                                                  
         MVC   LISTAR(09),=CL9'@@AGENCY'                                        
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QAGY),QAGY                                           
         MVC   LISTAR+25(20),EXAGYNAM                                           
         SPACE                                                                  
         CLI   QAGY,C'*'           SET                                          
         BNE   *+10                 NO                                          
         MVC   LISTAR+15(6),RRGAGY                                              
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0220 DS   0H                                                                
         OC    QADV,QADV           ANY ADVERTISER FILTER?                       
         BZ    SFIL0240             NO                                          
         SPACE                                                                  
         MVC   LISTAR(10),=CL10'@@ADVRTSR'                                      
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QADV),QADV                                           
         MVC   LISTAR+25(20),EXADVNAM                                           
         SPACE                                                                  
         CLI   QADV,C'*'           SET                                          
         BNE   *+10                 NO                                          
         MVC   LISTAR+15(6),RRGADV                                              
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0240 DS   0H                                                                
         OC    QDCT,QDCT           ANY DEVELOPMENTAL CONTRACT TYPE              
         BZ    SFIL0250             NO                                          
         SPACE                                                                  
         MVC   LISTAR(10),=CL10'@@DEVTYPE'                                      
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QDCT),QDCT                                           
         MVC   LISTAR+25(20),EXDCTNAM                                           
         SPACE                                                                  
         CLI   QDCT,C'*'           SET                                          
         BNE   *+10                 NO                                          
         MVC   LISTAR+15(6),RRGDCT                                              
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0250 DS   0H                                                                
         OC    QSAL,QSAL           ANY SALESPERSON                              
         BZ    SFIL0260             NO                                          
         SPACE                                                                  
         MVC   LISTAR(13),=CL13'@@SALESPERSON'                                  
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QSAL),QSAL                                           
         MVC   LISTAR+25(20),EXSALNAM                                           
         SPACE                                                                  
         CLI   QSAL,C'*'           SET                                          
         BNE   *+14                 NO                                          
         MVI   LISTAR+15,C'*'      SET                                          
         MVC   LISTAR+16(4),QSALSET                                             
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0260 DS   0H                                                                
         OC    QAFF,QAFF           ANY AFFILIATE                                
         BZ    SFIL0300             NO                                          
         SPACE                                                                  
         MVC   LISTAR(10),=CL10'@@AFFIL'                                        
         MVC   LASTFILT,LISTAR     SAVE THIS CALL                               
         MVC   LISTAR+15(L'QAFF),QAFF                                           
         MVC   LISTAR+25(L'QAFF),QAFF                                           
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
SFIL0300 L     R3,CDTBADR          NEXT CODE TABLE ADDRESS                      
         LA    R3,0(R9,R3)         IF MON, HAS MONTH COUNTS                     
         SPACE                                                                  
         CLI   TMPSTRCT,0          IS THERE SAVED TABLE                         
         BE    SFIL0302             NO                                          
         SPACE                                                                  
* GET NEXT OR 1ST SAVED TEMPSTOR PAGE OF CODETABL *                             
         SPACE                                                                  
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         SPACE                                                                  
         ZIC   R1,TMPSTRPG                                                      
         LTR   R1,R1               IF FIRST TIME                                
         BNZ   *+8                                                              
         LA    R1,STRPAGE          START WITH PAGE +1                           
         SPACE                                                                  
         LA    R0,CODETABL         START OF TABLE                               
         CR    R3,R0                                                            
         BNE   *+8                  NO                                          
         LA    R1,1(,R1)           GET NEXT PAGE                                
         SPACE                                                                  
         STC   R1,TMPSTRPG                                                      
         STC   R1,DMCB+8            SET PAGE NUMBER                             
         SPACE                                                                  
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=AL2(CODETBLN)                                        
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,CODETABL                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
SFIL0302 DS   0H                                                                
         CLI   QLIST,QLMON         THIS MONTHLY                                 
         BNE   SFIL0304                                                         
         ZIC   R5,CDTBADR                                                       
         LTR   R5,R5               IF NONE LEFT, DONE                           
         BZ    SFIL0440                                                         
         B     SFIL0306                                                         
         SPACE                                                                  
SFIL0304 CLI   BOOKSIZE(R3),0             END OF LIST                           
         BE    SFIL0430                                                         
         SPACE                                                                  
SFIL0306 LA    R0,RECLSTCT                                                      
         LA    R4,RECLIST                                                       
SFIL0310 CLC   QLIST,0(R4)                                                      
         BE    SFIL0320                                                         
         LA    R4,RECLSTLN(,R4)                                                 
         BCT   R0,SFIL0310                                                      
         DC    H'0'                                                             
         SPACE                                                                  
SFIL0320 XC    KEY,KEY                                                          
         ZIC   R7,LSTTYPLN                                                      
         BCTR  R7,0                                                             
         SPACE                                                                  
         CLI   QLIST,QLSTA         IF STATION, LEN IS 5                         
         BNE   *+8                                                              
         LA    R7,4                                                             
         SPACE                                                                  
         CLI   3(R4),0             THIS NO REC REQ                              
         BE    SFIL0332                                                         
         SPACE                                                                  
         MVC   KEY(1),1(R4)        REC TYPE                                     
         ZIC   RE,2(R4)            GET REP DISPL                                
         LA    R5,KEY(RE)                                                       
         MVC   0(2,R5),AGENCY                                                   
         SPACE                                                                  
         ZIC   RF,3(R4)            GET CODE DISPL                               
         LA    R5,KEY(RF)                                                       
         SPACE                                                                  
SFIL0330 CLI   3(R4),0             THIS NO REC REQ                              
         BE    SFIL0332                                                         
         EX    R7,SFILMVCK         MOVE CODE TO KEY                             
         EX    R7,SFILOCK          SET TO SPACES                                
         SPACE                                                                  
         CLI   QLIST,QLAGENCY                                                   
         BNE   SFIL0332                                                         
         OC    0(6,R5),BLANKS                                                   
         SPACE                                                                  
SFIL0332 MVC   LISTAR(2),=C'@@'                                                 
         MVC   LISTAR+2(8),RGEHD1                                               
         SPACE                                                                  
         SPACE                                                                  
         CLI   QLIST,QLSAL         SALESPERSON LIST                             
         BNE   SFIL0334                                                         
         MVC   LISTAR+10(3),=C'SON'                                             
         SPACE                                                                  
SFIL0334 DS   0H                                                                
         CLI   QLIST,QLMON         THIS MONTHLY                                 
         BE    SFIL0400                                                         
         SPACE                                                                  
SFIL0336 EX    R7,SFILMVCS         MOVE CODE TO SCREEN                          
         LA    RF,1+BOOKSIZE(R7,R3)                                             
         SR    RF,R9                                                            
         ST    RF,CDTBADR                                                       
         MVC   FILENMSV,FILENAME   SAVE RRGON I/O VALUES                        
         MVC   LKEYSV,LKEY                                                      
         MVI   LKEY+1,27                                                        
         XC    FILENAME,FILENAME   SWITCH TO REP I/O VALUES                     
         LA    R6,=CL30'** UNKNOWN **'                                          
         BCTR  R6,0                                                             
         BCTR  R6,0                                                             
         SPACE                                                                  
         CLI   3(R4),0             THIS NO REC REQ                              
         BE    SFIL0340                                                         
         SPACE                                                                  
         CLI   QLIST,QLSTA         THIS STATION LIST                            
         BNE   SFIL0338                                                         
         SPACE                                                                  
         MVC   COMBOSTA(5),BOOKSIZE(R3)                                         
         XC    KEY+22(6),KEY+22             CLEAR STATION                       
         MVC   LISTAR+15(4),BOOKSIZE(R3)    MOVE IN CODE TO SCREEN              
         LA    R1,LISTAR+19                                                     
         CLI   LISTAR+18,C' '                                                   
         BH    *+6                                                              
         BCTR  R1,0                                                             
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4+BOOKSIZE(R3)                                           
         CLI   1(R1),C'T'                                                       
         BNE   *+12                                                             
         MVI   2(R1),C'V'                                                       
         B     SFIL0337                                                         
         CLI   1(R1),C'A'                                                       
         BE    *+12                                                             
         CLI   1(R1),C'F'                                                       
         BNE   SFIL0337                                                         
         MVI   2(R1),C'M'                                                       
         SPACE                                                                  
SFIL0337 EQU   *                                                                
         MVC   KEY+22(5),BOOKSIZE(R3)                                           
         CLI   KEY+26,C'T'                                                      
         BNE   SFIL0338                                                         
         MVI   KEY+26,C' '                                                      
         SPACE                                                                  
SFIL0338 EQU   *                                                                
         CLI   QLIST,QLMKT         THIS MARKET LIST                             
         BNE   SFIL0339                                                         
         SPACE                                                                  
         CLI   KEY+26,C' '                                                      
         BH    SFIL0339                                                         
         MVI   KEY+26,0                                                         
         SPACE                                                                  
         CLI   KEY+25,C' '                                                      
         BH    SFIL0339                                                         
         MVI   KEY+25,0                                                         
         SPACE                                                                  
SFIL0339 EQU   *                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   SFIL0360                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         LA    R6,34(,R6)                                                       
         CLI   0(R6),01                                                         
         BE    SFIL0340                                                         
         CLI   KEY,02              THIS A STATION RECORD                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,=CL30'** UNKNOWN **'                                          
         BCTR  R6,0                                                             
         BCTR  R6,0                                                             
         SPACE                                                                  
SFIL0340 CLI   QLIST,QLCAT         THIS CATEGORY                                
         BNE   *+8                                                              
         LA    R6,2(,R6)           JUMP OVER CLASS                              
         SPACE                                                                  
SFIL0360 MVC   FILENAME,FILENMSV                                                
         MVC   LKEY,LKEYSV                                                      
         SPACE                                                                  
         CLI   QLIST,QLGRP                                                      
         BNE   SFIL0380                                                         
         MVC   LISTAR+25(10),2(R6)                                              
         LA    R0,10                                                            
         LA    R1,LISTAR+25                                                     
SFIL0370 CLI   0(R1),C' '                                                       
         BE    SFIL0374                                                         
         LA    R1,1(,R1)                                                        
         BCT   R0,SFIL0370                                                      
         SPACE                                                                  
SFIL0374 MVC   1(10,R1),12(R6)                                                  
         B     SFIL0382                                                         
         SPACE                                                                  
SFIL0380 ZIC   RF,4(R4)            GET NAME LENGTH                              
         LTR   RF,RF                                                            
         BZ    SFIL0382                                                         
         SPACE                                                                  
         BCTR  RF,0                                                             
         EX    RF,SFILMVCN                                                      
         SPACE                                                                  
SFIL0382 SR    R0,R0                                                            
         TM    0(R3),X'80'         THIS A MINUS                                 
         BZ    *+12                                                             
         LA    R0,255                                                           
         SLL   R0,24                                                            
         SPACE                                                                  
         ICM   R0,7,0(R3)                 PRIOR BOOKED                          
         BZ    SFIL0384                                                         
         SPACE                                                                  
         LR    RF,R0                                                            
         A     RF,LSTOTALS                                                      
         ST    RF,LSTOTALS                                                      
         SPACE                                                                  
         EDIT  (R0),(8,LISTAR+56),FLOAT=-                                       
         SPACE                                                                  
SFIL0384 SR    R0,R0                                                            
         TM    3(R3),X'80'         THIS A MINUS                                 
         BZ    *+12                                                             
         LA    R0,255                                                           
         SLL   R0,24                                                            
         SPACE                                                                  
         ICM   R0,7,3(R3)                 CURRENT BOOKED                        
         BZ    SFIL0420                                                         
         SPACE                                                                  
         LR    RF,R0                                                            
         A     RF,LSTOTALS+4                                                    
         ST    RF,LSTOTALS+4                                                    
         SPACE                                                                  
* #$%%%( &)( STEREO FIX TEMP ?                                                  
         SPACE                                                                  
         EDIT  (R0),(8,LISTAR+64),FLOAT=-                                       
         B     SFIL0420                                                         
         SPACE                                                                  
* EDIT MONTH TOTALS HERE                                                        
         SPACE                                                                  
SFIL0400 DS    0H                                                               
         CLI   MONTOTS,X'FF'       ONCE WE FIND 1 DISPLAY THEM ALL              
         BE    SFIL0402                                                         
         SPACE                                                                  
         OC    4(MONTOTLN-4,R3),4(R3)  IF NO DATA, BYPASS                       
         BZ    SFIL0434                                                         
         SPACE                                                                  
         MVI   MONTOTS,X'FF'       SET FOUND ONE                                
SFIL0402 DS    0H                                                               
         LA    R0,MONTOTLN(,R3)                                                 
         SR    R0,R9                                                            
         ST    R0,CDTBADR                                                       
         LR    R0,R5                                                            
         BCTR  R0,0                                                             
         STC   R0,CDTBADR                                                       
         SPACE                                                                  
         ICM   R0,15,20(R3)               PRIOR BOOKED                          
         SPACE                                                                  
         LR    RF,R0                                                            
         A     RF,LSTOTALS                                                      
         ST    RF,LSTOTALS                                                      
         SPACE                                                                  
         CLI   QPY,C'Y'            AND YTD OPTION                               
         BNE   *+12                                                             
         A     R0,TOTALS                                                        
         ST    R0,TOTALS                                                        
         SPACE                                                                  
         LTR   R0,R0                                                            
         BZ    SFIL0404                                                         
         EDIT  (R0),(8,LISTAR+56),FLOAT=-                                       
         SPACE                                                                  
SFIL0404 ICM   R0,15,24(R3)               CURRENT BOOKED                        
         SPACE                                                                  
         LR    RF,R0                                                            
         A     RF,LSTOTALS+4                                                    
         ST    RF,LSTOTALS+4                                                    
         SPACE                                                                  
         CLI   QPY,C'Y'            AND YTD OPTION                               
         BNE   *+12                                                             
         A     R0,TOTALS+4                                                      
         ST    R0,TOTALS+4                                                      
         SPACE                                                                  
         LTR   R0,R0                                                            
         BZ    SFIL0406                                                         
         SPACE                                                                  
* #$%%%(*&)( STEREO FIX TEMP ?                                                  
         SPACE                                                                  
         EDIT  (R0),(8,LISTAR+64),FLOAT=-                                       
         SPACE                                                                  
SFIL0406 CLI   1(R3),0             THIS PRIOR                                   
         BNE   *+14                                                             
         MVC   LISTAR+15(5),=C'PRIOR'                                           
         B     SFIL0420                                                         
         SPACE                                                                  
         MVC   DUB(2),0(R3)                                                     
         MVI   DUB+2,01                                                         
         GOTO1 DATCON,DMCB,(3,DUB),(6,WORK)    MONTH/YR TO PERIOD COL           
         MVC   LISTAR+15(3),WORK                                                
         SPACE                                                                  
SFIL0420 CLI   QSKIPLR,X'FF'       FORCED BY END OF TABLE                       
         BNE   SFIL0424                                                         
         LA    RF,1+BOOKSIZE(R7,R3)                                             
         SPACE                                                                  
         CLI   BOOKSIZE(RF),0      THIS LAST OF LIST                            
         BNE   SFIL0424                                                         
         MVC   LISTAR+25(25),=CL25'*INCOMPLETE LIST CREATED*'                   
         SPACE                                                                  
SFIL0424 GOTO1 SCRNMOVE            PUT IT OUT                                   
         SPACE                                                                  
         CLC   LISTNUM,NLISTS      END OF SCREEN                                
         BH    SFILX                                                            
         SPACE                                                                  
         CLI   QLIST,QLMON         THIS MONTHLY                                 
         BE    SFIL0434                                                         
         SPACE                                                                  
         LA    R3,1+BOOKSIZE(R7,R3)                                             
         CLI   BOOKSIZE(R3),0      END OF LIST                                  
         BNE   SFIL0320             NO                                          
         SPACE                                                                  
SFIL0430 CLI   TMPSTRCT,0          IS THERE A SAVED TABLE                       
         BE    SFIL0440                                                         
         SPACE                                                                  
* GET 1ST SAVED TEMPSTOR PAGE OF CODETABL *                                     
         SPACE                                                                  
         XC    DMCB(24),DMCB                                                    
         L     RE,ATWA                                                          
         MVC   DMCB+10(2),2(RE)    MOVE TERMINAL NUMBER                         
         SPACE                                                                  
         CLC   TMPSTRPG,TMPSTRCT   IF AT LAST PAGE, DONE                        
         BE    SFIL0440                                                         
         SPACE                                                                  
         ZIC   R1,TMPSTRPG                                                      
         LTR   R1,R1               IF FIRST TIME                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,1(,R1)                                                        
         STC   R1,TMPSTRPG                                                      
         STC   R1,DMCB+8            SET PAGE NUMBER                             
         SPACE                                                                  
         MVC   DMCB+20(2),=C'L='                                                
         MVC   DMCB+22(2),=AL2(CODETBLN)                                        
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,CODETABL                    
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         LA    R3,CODETABL                                                      
         B     SFIL0302                                                         
         SPACE                                                                  
SFIL0434 LA    R3,MONTOTLN(,R3)                                                 
         BCT   R5,*+8                                                           
         B     SFIL0440                                                         
SFIL0436 LR    R0,R3                                                            
         SR    R0,R9                                                            
         ST    R0,CDTBADR          NEXT MON TABLE ADDRESS                       
         LR    R0,R5                                                            
         BCTR  R0,0                                                             
         STC   R0,CDTBADR          SAVE REMAINING MONTHS COUNT                  
         OC    4(MONTOTLN-4,R3),4(R3)                                           
         BNZ   SFIL0330                                                         
         LA    R3,MONTOTLN(,R3)                                                 
         BCT   R5,SFIL0436                                                      
         SPACE                                                                  
SFIL0440 DS   0H                                                                
         CLI   LISTNUM,14          AT END OF SCREEN                             
         BNL   SFILX                                                            
         TM    DISPFLAG,DISTOTSW   STEREO - SHOWN TOTALS LINE?                  
         BO    SFIL0444                                                         
         SPACE                                                                  
         MVC   LISTAR(8),=C'@@TOTALS'                                           
         SPACE                                                                  
         EDIT  LSTOTALS,(8,LISTAR+56),FLOAT=-                                   
         SPACE                                                                  
         EDIT  LSTOTALS+4,(8,LISTAR+64),FLOAT=-                                 
         SPACE                                                                  
         XC    LSTOTALS(24),LSTOTALS                                            
         SPACE                                                                  
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         OI    DISPFLAG,DISTOTSW   STEREO - SET SHOWN TOTALS LINE               
         SPACE                                                                  
SFIL0444 DS   0H                                                                
         CLI   LISTNUM,14          AT END OF SCREEN                             
         BNL   SFILX                                                            
         SPACE                                                                  
         MVC   LISTAR,BLANKS       CLEAR DISPLAY LINE FIELD                     
         MVC   LISTAR(5),=C'##END'                                              
*        CLI   TWAOFFC,C'*'        ONLY FOR DDS TERMS                           
*        BNE   SFIL0450                                                         
*        CLI   QLIST,QLMON         THIS MONTHLY                                 
*        BE    SFIL0450                                                         
*        EDIT  TOTLCT,(9,LISTAR+57)                                             
         SPACE                                                                  
*FIL0450 DS   0H                                                                
         GOTO1 SCRNMOVE            PUT IT OUT                                   
         MVI   LISTEND,C'N'        TURN OFF FLAG                                
         XC    MONTOTS+4(24),MONTOTS+4     CLEAR TOTALS LINE AREA               
SFILX    DS   0H                                                                
         XIT1                                                                   
SFILMVCK MVC   0(0,R5),BOOKSIZE(R3)         MOVE IN CODE TO KEY                 
SFILOCK  OC    0(0,R5),BLANKS               FORCE TO SPACES                     
SFILMVCA MVC   0(0,R5),AGENCY               MOVE IN REP TO KEY                  
SFILMVCS MVC   LISTAR+15(0),BOOKSIZE(R3)    MOVE IN CODE TO SCREEN              
SFILMVCN MVC   LISTAR+25(0),2(R6)           MOVE IN NAME                        
*                                                                               
* LIST TYPE                                                                     
* KEY CODE                                                                      
* DISP OF REP INTO KEY                                                          
* DISP OF CODE INTO KEY                                                         
*                                                                               
RECLIST  DS   0XL5                                                              
*                         REC     REP    CODE   NAME                            
*                         TYPE    DISP   DISP   LEN                             
         DC    AL1(QLGRP),X'07',AL1(23),AL1(25),AL1(20)                         
RECLSTLN EQU   *-RECLIST                                                        
         DC    AL1(QLOFF),X'04',AL1(23),AL1(25),AL1(20)                         
         DC    AL1(QLREG),X'03',AL1(23),AL1(25),AL1(20)                         
         DC    AL1(QLSTA),X'02',AL1(20),AL1(22),AL1(20)                         
         DC    AL1(QLCLS),X'0D',AL1(23),AL1(25),AL1(30)                         
         DC    AL1(QLCAT),X'0F',AL1(23),AL1(25),AL1(30)                         
* STATION TYPE NOT INTERESTING EITHER - ONLY 1, 2, OR 3                         
         DC    AL1(QLSTY),X'0F',AL1(0),AL1(0),AL1(0)                            
* TVB IS A LIST, NOT A RECORD                                                   
         DC    AL1(QLTVB),X'00',AL1(0),AL1(0),AL1(0)                            
         DC    AL1(QLOWN),X'2A',AL1(22),AL1(24),AL1(20)                         
* RANK IS 1 THRU 9 ONLY                                                         
         DC    AL1(QLRNK),X'00',AL1(0),AL1(0),AL1(0)                            
         DC    AL1(QLCON),X'32',AL1(24),AL1(26),AL1(20)                         
         DC    AL1(QLMKT),X'2B',AL1(21),AL1(23),AL1(20)                         
* AFFILIATE IS WHATEVER THE REP WANTS                                           
         DC    AL1(QLAFF),X'00',AL1(0),AL1(0),AL1(0)                            
         DC    AL1(QLADV),X'08',AL1(25),AL1(21),AL1(20)                         
         DC    AL1(QLAGY),X'0A',AL1(25),AL1(19),AL1(20)                         
         DC    AL1(QLAGENCY),X'0A',AL1(25),AL1(19),AL1(20)                      
         DC    AL1(QLDCT),X'3B',AL1(23),AL1(25),AL1(20)                         
         DC    AL1(QLSAL),X'06',AL1(22),AL1(24),AL1(20)                         
* QLMON IS NOT A RECORD, OF COURSE                                              
         DC    AL1(QLMON),X'00',AL1(0),AL1(0),AL1(0)                            
RECLSTCT EQU   (*-RECLIST)/L'RECLIST                                            
         DS    0H                                                               
         EJECT                                                                  
*                                                                               
*  SCRNMOVE:  MOVES LISTAR TO SCREEN FIELD,                                     
*     SETS TRANSMIT BIT, BUMPS TO NEXT FIELD, AND SETS 'ATHISLST'               
*     TO POSITION FOR FIRST SCREEN DETAIL DATA.                                 
*     R2  --->  A(DETAIL FIELD HEADER OF LINE)                                  
*                                                                               
SCRNMOVE NTR1                                                                   
*                                                                               
         L     R2,ATHISLST         SET A(LINE IN PROCESS)                       
         MVC   8(74,R2),LISTAR     LOAD FIELD TO SCREEN                         
         OI    6(R2),X'80'         TURN ON TRANSMIT BIT                         
         LA    R2,LSCRNLIN(R2)     BUMP TO NEXT POSITION                        
         ST    R2,ATHISLST                                                      
         ZIC   RF,LISTNUM          INCREMENT LIST COUNT                         
         LA    RF,1(RF)                                                         
         STC   RF,LISTNUM          SAVE IT BACK                                 
         MVC   LISTAR,BLANKS       CLEAR DISPLAY LINE FIELD                     
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,RC                                                            
*                                                                               
*        ROUTINE TO ADJUST A PERCENTAGE ACCORDING TO NUMBER OF                  
*        WEEKS IN PRIOR AND CURRENT MONTHS                                      
*        R5 CONTAINS THE PCT                                                    
*        CONDITION CODE SET TO NOT EQUAL IF PCT IS GT 999                       
*                                                                               
         DS    0F                                                               
PCTADJ   NMOD1 0,*PCTA*                                                         
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         CLC   QSTART(1),QEND      TEST FOR SAME START/END YEARS                
         BNE   PA010               NO - FORGET IT                               
         LA    RE,PRIWKS                                                        
         LA    RF,CURWKS                                                        
         CLI   QPY,C'Y'            TEST FOR YTD                                 
         BNE   *+12                                                             
         LA    RE,PRIWKSY          YES - USE YTD WEEKS                          
         LA    RF,CURWKSY                                                       
         SR    R4,R4                                                            
         ZIC   R1,MONTH            GET THE MONTH                                
         BCTR  R1,0                                                             
         ZIC   R6,0(R1,RE)         GET THE PRIOR WEEKS                          
         AR    R6,R6               X 2                                          
         MR    R4,R6               MULTIPLY BY PRIOR WEEKS                      
         ZIC   R6,0(R1,RF)         GET THE CURRENT WEEKS                        
         DR    R4,R6               DIVIDE BY CURRENT WEEKS                      
         A     R5,=F'1'                                                         
         SRA   R5,1                DIVIDE BACK BY 2                             
         CH    R5,=H'10000'        TEST FOR PCT GT 999                          
         BNL   PA020                                                            
*                                                                               
PA010    CR    RB,RB               CC EQ                                        
         B     PA999                                                            
*                                                                               
PA020    LTR   RB,RB               CC NE                                        
*                                                                               
PA999    XIT1  REGS=(R5)           PASS BACK PCT IN R5                          
         DROP  RB,RC                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
**                                                                              
*   NEED TO SAVE VALUE IN FIRST PARAM AS POINTER TO COMPARE                     
*                                                                               
GESTATS  NMOD1 0,**GEST**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
*                                  ONLY CLEAR 1ST 2 CHARS OF DUB                
*                                     FLAG IS IN 3RD POSITION                   
         XC    DUB(2),DUB          SET DUB = ZERO:  EQUAL                       
         MVC   FILENMSV,FILENAME   SAVE RRGON I/O VALUES                        
         MVC   SVAIO,AIO                                                        
         MVC   SVKEY2,KEY                                                       
         MVC   LKEYSV,LKEY                                                      
         MVC   USEIOSV,USEIO                                                    
         MVC   DMINBTS,RPINBTS     REP IN/OUT DATAMGR BITS                      
         MVC   DMOUTBTS,RPOUTBTS                                                
         XC    FILENAME,FILENAME   SWITCH TO REP I/O VALUES                     
         MVC   AIO,AIO2                                                         
         MVC   LKEY,=H'27'                                                      
         MVI   USEIO,C'N'                                                       
         MVI   BYTE,0                                                           
*                                                                               
         SPACE                                                                  
         MVC   STATION(4),0(R3)                                                 
         LA    RF,5(R3)                                                         
         CLI   STATION+3,C'-'                                                   
         BNE   *+10                                                             
         MVI   STATION+3,C' '                                                   
         BCTR  RF,0                                                             
         MVI   STATION+4,C' '                                                   
         CLI   0(RF),C'T'                                                       
         BE    GEST0020                                                         
         CLI   0(RF),0                                                          
         BE    GEST0020                                                         
         MVC   STATION+4(1),0(RF)                                               
         SPACE                                                                  
GEST0020 DS   0H                                                                
         L     R1,AIO2                                                          
         CLI   0(R1),2             TEST RECORD ALREADY IN CORE                  
         BNE   GEST0040                                                         
         CLC   STATION,22(R1)                                                   
         BE    GEST0160            YES                                          
*                                                                               
*   IF ALREADY IN CORE, BRANCH TO COMPARISON ROUTINE RATHER                     
*      THAN EXIT....                                                            
*                                                                               
GEST0040 DS   0H                                                                
         XC    KEY,KEY             SET STATION RECORD PASSIVE KEY               
         LA    R4,KEY                                                           
         USING RST2KEY,R4                                                       
         MVI   RST2KTYP,X'82'                                                   
         MVC   RST2KSTA,STATION                                                 
         GOTO1 HIGH                                                             
*                                                                               
GEST0060 CLC   KEY(RST2KEND-RST2KEY),KEYSAVE                                    
         BE    GEST0120                                                         
         CLI   BYTE,0              TEST SECOND SEARCH                           
         BNE   GEST0080                                                         
         CLC   AGENCY,=C'SJ'       OR AGENCY IS SJR (SJR MUST GET               
         BNE   GEST0100                              SJR RECORDS)               
GEST0080 DS   0H                                                                
         MVC   GERROR,=AL2(INVSTA) YES-RECORD NOT FOUND                         
         B     REPERR                                                           
GEST0100 DS   0H                                                                
         MVI   BYTE,1              NO-START SECOND SEARCH TO FIND               
         B     GEST0040               FIRST POINTER                             
*                                                                               
GEST0120 DS   0H                                                                
         CLC   RST2KREP,AGENCY     AGENCIES MATCH - FINE                        
         BE    GEST0150                                                         
         CLI   BYTE,0              TEST FIRST SEARCH                            
         BE    GEST0140            YES-TRY TO FIND AGENCY MATCH                 
         CLC   RST2KREP,=C'SJ'     NO-SKIP SJR RECORDS                          
         BNE   GEST0150                                                         
GEST0140 DS   0H                                                                
         GOTO1 SEQ                                                              
         B     GEST0060                                                         
*                                                                               
GEST0150 GOTO1 GETREC                                                           
*                                                                               
         DROP  R4                                                               
*                                                                               
GEST0160 L     R5,AIO2                                                          
         USING RSTAD,R5                                                         
*                                                                               
**  DO COMPARISON BASED ON VALUE PASSED IN...                                   
*                                                                               
         CLI   DUB+2,1             COMPARE AFFILIATE?                           
         BNE   GEST0162            NO                                           
         CLC   RSTAAFFL,QAFF       CHECK THE STATION'S AFFILIATE                
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
GEST0162 DS   0H                                                                
         CLI   DUB+2,2             COMPARE TVB REGION?                          
         BNE   GEST0164            NO                                           
         CLC   RSTATVB,QTVB        CHECK THE STATION'S TVB REGION               
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
GEST0164 DS   0H                                                                
         CLI   DUB+2,3             COMPARE OWNER?                               
         BNE   GEST0166            NO                                           
         CLC   RSTAOWN,QOWNER      SAME OWNER?                                  
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
         SPACE                                                                  
GEST0166 DS   0H                                                                
         CLI   DUB+2,4             COMPARE RANK?                                
         BNE   GEST0168            NO                                           
*                                  MUST BE RANK COMPARE!!                       
         CLC   RSTARANK,QRANK      CHECK THE STATION'S MARKET RANK              
         BE    GEST0200            YES -                                        
         B     GEST0172            NO                                           
         SPACE                                                                  
GEST0168 DS   0H                                                                
         CLI   DUB+2,5             COMPARE STATION TYPE?                        
         BNE   GEST0168             NO                                          
*                                  MUST BE RANK COMPARE!!                       
         MVC   FULL,QSTART         SAVE QSTART & QEND                           
         ZIC   RF,FULL                                                          
         BCTR  RF,0                                                             
         STC   RF,FULL                                                          
         ZIC   RF,FULL                                                          
         LA    RF,1(,RF)                                                        
         STC   RF,FULL+2                                                        
         SPACE                                                                  
         CLI   QSTATY,1            COMPARABLE                                   
         BNE   GEST016C                                                         
         SPACE                                                                  
* IF JOIN DATE MORE RECENT THAN 1 YR BEFORE REQUEST START DATE, NEW             
         SPACE                                                                  
         CLC   RSTAEND,FULL        IF START DATE WITHIN 1 YR OF REQ             
         BNL   GEST0172             BYPASS                                      
         SPACE                                                                  
         OC    RSTAEND,RSTAEND     NO LEAVE DATE                                
         BZ    GEST0200             OKAY                                        
         SPACE                                                                  
         CLC   RSTAEND,FULL+2      IF LEAVE DATE WITHIN 1 YR OF REQ             
         BNH   GEST0172             BYPASS                                      
         B     GEST0200            OKAY-                                        
         SPACE                                                                  
GEST016C DS   0H                                                                
         CLI   QSTATY,2            NEW (JOINED WITHIN 1 YEAR)                   
         BNE   GEST016E                                                         
         CLC   RSTASTRT,FULL       IF START MORE RECENT THEN 1 YR PRIOR         
         BH    GEST0172             BYPASS                                      
         SPACE                                                                  
         OC    RSTAEND,RSTAEND     NO LEAVE DATE                                
         BZ    GEST0200             OKAY                                        
         SPACE                                                                  
         CLC   RSTAEND,FULL+2      IF LEAVE DATE WITHIN 1 YR OF REQ             
         BNH   GEST0172             BYPASS                                      
         B     GEST0200            OKAY-                                        
         SPACE                                                                  
GEST016E DS   0H                                                                
         CLI   QSTATY,3            OLD (LEFT WITHIN 1 YEAR)                     
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         OC    RSTAEND,RSTAEND     NO LEAVE DATE                                
         BZ    GEST0200             OKAY                                        
         SPACE                                                                  
         CLC   RSTAEND,FULL+2      CHECK THE STATION'S LEAVE DATE               
         BH    GEST0200                                                         
         DROP  R5                                                               
GEST0172 DS   0H                                                                
         CLC   =C'K3',AGENCY       CROSS-COMPANY AGENCY?                        
         BE    GEST0176             YES                                         
         CLC   =C'MR',AGENCY       CROSS-COMPANY AGENCY?                        
         BE    GEST0176             YES                                         
         CLC   =C'IR',AGENCY       CROSS-COMPANY AGENCY?                        
         BNE   GEST0180            NO  - EXIT                                   
GEST0176 DS   0H                                                                
         GOTO1 SEQ                 GET NEXT RECORD IN LINE                      
         CLC   KEY(RST2KEND-RST2KEY),KEYSAVE                                    
*                                  SAME STATION FOUND?                          
         BNE   GEST0180            NO  - TREAT AS NOT FOUND                     
         SPACE                                                                  
         OC    KEY+RST2KEND-RST2KEY(3),KEY+RST2KEND-RST2KEY                     
*                                  IS THERE A LEAVE DATE?                       
         BZ    GEST0150            NO  - GO BACK AND COMPARE                    
*                                  YES - TREAT AS 'NOT FOUND'                   
GEST0180 DS   0H                                                                
         MVI   DUB,1               SET DUB NOT = ZERO:  NOT EQUAL               
GEST0200 DS   0H                                                                
         MVC   FILENAME,FILENMSV   SWITCH BACK TO RRGON I/O VALUES              
         MVC   AIO,SVAIO                                                        
         MVC   KEY,SVKEY2                                                       
         MVC   LKEY,LKEYSV                                                      
         MVC   USEIO,USEIOSV                                                    
         OI    DMINBTS,X'08'       PASS DELETED RECS                            
         NI    DMOUTBTS,X'FD'      IGNORE DELETED RECS                          
         CLC   DUB(1),DUB+1        SET FINAL CONDITION CODE                     
         XIT1                                                                   
         DROP  RB,RC,R7                                                         
         EJECT                                                                  
* FILTER ON SETS HERE *                                                         
         SPACE                                                                  
FSET     NMOD1 0,**FSET**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         LA    R3,SET1CDE                                                       
         LR    R4,R9                                                            
         AH    R4,=AL2(SET1TAB-SYSD)                                            
         LA    R0,3                                                             
FSET10   CLC   0(1,R2),0(R3)       LOOK FOR SET CODE                            
         BE    FSET20                                                           
         LA    R3,L'SETCDE(,R3)                                                 
         LA    R4,L'SET1TAB(,R4)                                                
         BCT   R0,FSET10                                                        
         DC    H'0'                                                             
FSET20   ZIC   R5,1(R3)            GET LENGTH OF ENTRY                          
         BCTR  R5,0                                                             
         CLI   0(R4),0             END OF TABLE                                 
         BNE   FSET30                                                           
         DC    H'0'                EMPTY TABLE - NO WAY                         
         SPACE                                                                  
FSET30   CLI   0(R4),0             END OF TABLE                                 
         BE    FSET90                                                           
         SPACE                                                                  
         CLI   0(R3),QLSTA         STATION COMPARE DIFFERENT                    
         BNE   FSET50                                                           
         CLC   0(3,R6),0(R4)       1ST 3 OF STATION                             
         BL    FSET40                                                           
         BH    FSET56                                                           
         SPACE                                                                  
         MVC   ACTUAL,3(R6)                                                     
         LA    R1,4(,R6)                                                        
         CLI   3(R6),C'-'                                                       
         BNE   FSET32                                                           
         BCTR  R1,0                                                             
         MVI   ACTUAL,C' '                                                      
         SPACE                                                                  
FSET32   CLC   ACTUAL,3(R4)                                                     
         BL    FSET40                                                           
         BH    FSET56                                                           
         SPACE                                                                  
         MVC   ACTUAL,1(R1)                                                     
         CLI   ACTUAL,C'T'                                                      
         BNE   *+8                                                              
         MVI   ACTUAL,C' '                                                      
         CLC   ACTUAL,4(R4)        TEST BAND                                    
         BL    FSET40                                                           
         BH    FSET56                                                           
         SPACE                                                                  
         TM    SET1FLG-SET1CDE(R3),X'08'  EXCLUDE SET                           
         BZ    FSETEQ                                                           
         SPACE                                                                  
         MVI   7(R6),X'FF'         GET NEXT VALUE THIS FIELD                    
         B     FSET64              SET NEXT FLDS TO 1ST OF SET/FILTER           
         SPACE                                                                  
FSET40   TM    SET1FLG-SET1CDE(R3),X'08'  EXCLUDE SET                           
         BO    FSETEQ                      YES, ACCEPT THIS                     
         SPACE                                                                  
         MVC   0(4,R6),0(R4)                                                    
         LA    R1,3(,R6)                                                        
         CLI   3(R4),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         SPACE                                                                  
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(R4)                                                    
         CLI   1(R1),C' '                                                       
         BH    FSET46                                                           
         MVC   1(2,R1),=C'TV'                                                   
         B     FSET64                                                           
FSET46   MVI   2(R1),C'M'                                                       
         B     FSET64                                                           
         SPACE                                                                  
FSET50   EX    R5,FSETCLC          SEE IF A MATCH                               
         SPACE                                                                  
FSET54   BH    FSET56                                                           
         BL    FSET60                                                           
         SPACE                                                                  
         TM    SET1FLG-SET1CDE(R3),X'08'  EXCLUDE SET                           
         BZ    FSETEQ                                                           
         MVI   7(R6),X'FF'         GET NEXT VALUE THIS FIELD                    
         B     FSET64                                                           
         SPACE                                                                  
* NOW LOOK AT NEXT ENTRY IN TABLE                                               
         SPACE                                                                  
FSET56   LA    R4,1(R4,R5)                                                      
         B     FSET30                                                           
         SPACE                                                                  
FSET60   TM    SET1FLG-SET1CDE(R3),X'08'  EXCLUDE SET                           
         BO    FSETEQ                      YES, ACCEPT THIS                     
         SPACE                                                                  
         EX    R5,FSETMVC                                                       
         SPACE                                                                  
* SET REST OF KEY TO 1ST OF TABLE FOR SETS, OR FILTER VALUE                     
         SPACE                                                                  
FSET64   LA    R2,1(,R2)                                                        
         LA    R6,8(,R6)                                                        
         C     R2,SETKEYAD                                                      
         BH    FSETNE                                                           
         SPACE                                                                  
         CLI   0(R2),0                                                          
         BE    FSETNE                                                           
         CLC   LSTTYPE,0(R2)       TEST LISTING ON THIS TYPE                    
         BE    FSET64                                                           
         L     RF,=A(FILTAB)                                                    
         A     RF,RELO                                                          
*                                                                               
FSET70   CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   0(1,R2),0(RF)                                                    
         BE    FSET74                                                           
         LA    RF,L'FILTAB(RF)                                                  
         B     FSET70                                                           
         SPACE                                                                  
FSET74   LH    R1,2(RF)                                                         
         LA    R1,SYSD(R1)                                                      
         ZIC   RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,FSET1            TEST FILTER IS SET (COMP TO SPACES)          
         BH    *+14                 YES                                         
         XC    0(8,R6),0(R6)                                                    
         B     FSET64                                                           
         SPACE                                                                  
         CLI   0(R1),C'*'          IS THIS A SETS REQUEST?                      
         BNE   FSET78               NO                                          
         SPACE                                                                  
         BAS   RE,SETM             SET TO FIRST OF SET                          
         B     FSET64                                                           
         SPACE                                                                  
FSET78   EX    RE,FSET3            MOVE IN FILTER VALUE                         
         B     FSET64                                                           
         SPACE                                                                  
FSET90   TM    SET1FLG-SET1CDE(R3),X'08'  EXCLUDE SET                           
         BO    FSETEQ                      YES, ACCEPT THIS                     
         SPACE                                                                  
         MVI   0(R6),X'FF'                                                      
         SPACE                                                                  
FSETNE   XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         LTR   RB,RB                                                            
*                                                                               
*   TEST DIE # 2                                                                
***      MVC   DIE,=X'0000'                                                     
*                                                                               
*                                                                               
         B     FSETX                                                            
         SPACE                                                                  
FSETEQ   CR    RB,RB                                                            
         SPACE                                                                  
FSETX    XIT1                                                                   
         SPACE                                                                  
FSETCLC  CLC   0(0,R6),0(R4)                                                    
FSETMVC  MVC   0(0,R6),0(R4)                                                    
FSET1    CLC   0(0,R1),BLANKS      SEE IF FILTER IS SET                         
FSET3    MVC   0(0,R6),0(R1)                                                    
         EJECT                                                                  
* FILL IN INITIAL KEY FROM SET TABLE                                            
         SPACE                                                                  
SETM     NTR1                                                                   
         LA    R0,3                                                             
         LA    RE,SET1CDE                                                       
         LR    RF,R9                                                            
         AH    RF,=AL2(SET1TAB-SYSD)                                            
SETM20   CLC   0(1,R2),0(RE)       THIS THE CODE                                
         BE    SETM40                                                           
         LA    RE,L'SETCDE(,RE)    NEXT CODE                                    
         LA    RF,L'SET1TAB(,RF)   NEXT TABLE                                   
         BCT   R0,SETM20                                                        
         DC    H'0'                NOT IN ANY TABLE? NO WAY!                    
         SPACE                                                                  
SETM40   XC    0(8,R6),0(R6)                                                    
         TM    SET1FLG-SET1CDE(R3),X'08'  EXCLUDE SET                           
         BO    SETM46                                                           
         ZIC   R1,1(RE)            GET CODE LEN                                 
         BCTR  R1,0                                                             
         EX    R1,SETMMVC                                                       
         SPACE                                                                  
         CLI   0(R2),QLSTA         THIS STATION                                 
         BNE   SETM50                                                           
         MVI   6(R6),0             FORCE SHORT STATIONS TO NULL                 
         LA    R1,3(,R6)                                                        
         CLI   3(R6),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    R1,1(,R1)                                                        
         MVI   0(R1),C'-'                                                       
         MVC   1(1,R1),4(RF)                                                    
         CLI   1(R1),C' '                                                       
         BH    SETM44                                                           
         MVC   1(2,R1),=C'TV'                                                   
         B     SETMX                                                            
SETM44   MVI   2(R1),C'M'                                                       
         B     SETMX                                                            
         SPACE                                                                  
SETM46   MVI   7(R1),01            BYPASS NULLS RECS                            
         B     SETMX                                                            
         SPACE                                                                  
SETM50   CLI   0(R2),QLCON         THIS CONTRACT TYPE                           
         BNE   SETMX                                                            
         OI    1(R6),X'40'                                                      
         SPACE                                                                  
SETMX    XIT1                                                                   
         SPACE                                                                  
SETMMVC  MVC   0(0,R6),0(RF)                                                    
         EJECT                                                                  
         DROP  RB,RC                                                            
         LTORG                                                                  
         EJECT                                                                  
* FIND PREVIOUS SET, AND SET TO NEXT VALUE IN TABLE *                           
* IF AT END OF TABLE, SET 1ST BYTE TO FF            *                           
         SPACE                                                                  
PSET     NMOD1 0,**PSET**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         SPACE                                                                  
         MVC   KEY(6),KEYSAVE      SET BASICS                                   
         SPACE                                                                  
         LA    R0,3                                                             
         LA    R1,KEY+6                                                         
         LA    RE,KEYSAVE+6                                                     
         SPACE                                                                  
PSET04   CLI   7(RE),X'FF'         THIS BYPASSED FIELD                          
         BE    PSET06                                                           
         MVC   0(8,R1),0(RE)                                                    
         SPACE                                                                  
PSET06   LA    R1,8(,R1)                                                        
         LA    RE,8(,RE)                                                        
         BCT   R0,PSET04                                                        
         SPACE                                                                  
         LR    R4,R2               SAVE ADDR OF 'CURR' DATA TYPE                
         BCTR  R4,0                GET PREV                                     
         SPACE                                                                  
PSET10   ST    R4,FULL             NEXT TRY                                     
         LA    R0,3                                                             
         LA    R2,KEY+ROKDTLTY-ROKEY                                            
         LA    R3,KEY+ROKDTLVL-ROKEY                                            
PSET20   CLC   0(1,R4),0(R2)       THIS THE CODE                                
         BE    PSET24                                                           
         LA    R2,1(,R2)                                                        
         LA    R3,8(,R3)                                                        
         BCT   R0,PSET20                                                        
         SPACE                                                                  
PSET22   L     R4,FULL             GET THIS                                     
         BCTR  R4,0                GO BACK                                      
         LA    RF,KEY+3                                                         
         CR    R4,RF               GOING TOO FAR?                               
         BL    PSETNE               YES, ALL DONE                               
         B     PSET10                                                           
         SPACE                                                                  
PSET24   MVC   LASTSET,0(R4)                                                    
         STM   R2,R3,DUB                                                        
         SPACE                                                                  
         LA    RF,3                                                             
         LA    R4,SET1CDE                                                       
         LR    R5,R9                                                            
         AH    R5,=AL2(SET1TAB-SYSD)                                            
PSET26   CLC   LASTSET,0(R4)          THIS THE CODE                             
         BE    PSET30                                                           
         LA    R4,L'SETCDE(,R4)       NEXT CODE                                 
         LA    R5,L'SET1TAB(,R5)      NEXT TABLE                                
         BCT   RF,PSET26                                                        
         B     PSET22                 NOT THERE GO BACK AGAIN                   
         SPACE                                                                  
PSET30   TM    SET1FLG-SET1CDE(R4),X'08'  EXCLUDE SET                           
         BZ    PSET34                                                           
         MVI   7(R3),X'FF'                                                      
         B     PSETEQ                                                           
         SPACE                                                                  
PSET34   CLI   0(R5),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         ZIC   R1,1(R4)            GET LENGTH                                   
         BCTR  R1,0                                                             
         SPACE                                                                  
PSET40   CLI   LASTSET,QLSTA       COMPARE IS DIFFERENT                         
         BNE   PSET50                                                           
         CLC   0(3,R5),0(R3)       1ST 3 OF STATION                             
         BNE   PSET52                                                           
         SPACE                                                                  
         MVC   ACTUAL,3(R3)                                                     
         LA    RE,4(,R3)                                                        
         CLI   3(R3),C'-'                                                       
         BNE   PSET44                                                           
         BCTR  RE,0                                                             
         MVI   ACTUAL,C' '                                                      
         SPACE                                                                  
PSET44   CLC   ACTUAL,3(R5)                                                     
         BNE   PSET52                                                           
         SPACE                                                                  
         MVC   ACTUAL,1(RE)                                                     
         CLI   ACTUAL,C'T'                                                      
         BNE   *+8                                                              
         MVI   ACTUAL,C' '                                                      
         CLC   ACTUAL,4(R5)        TEST BAND                                    
         BE    PSET54                                                           
         B     PSET52                                                           
         SPACE                                                                  
PSET50   EX    R1,PSETCLC                                                       
         BE    PSET54                                                           
PSET52   LA    R5,1(R1,R5)                                                      
         CLI   0(R5),0             SEE IF END OF TABLE                          
         BNE   PSET40               NOT YET                                     
         B     PSET22              LOOK FOR PREVIOUS SET                        
         SPACE                                                                  
PSET54   LA    R5,1(R1,R5)                                                      
         CLI   0(R5),0             END OF TABLE                                 
         BE    PSET22               SEE IF PREVIOUS SET TO THIS                 
         SPACE                                                                  
         XC    ROKDTLYM-ROKEY+KEY,ROKDTLYM-ROKEY+KEY                            
         SPACE                                                                  
         EX    R1,PSETMVC                                                       
         SPACE                                                                  
         CLI   LASTSET,QLSTA                                                    
         BNE   PSET70                                                           
         MVI   6(R3),0             FORCE SHORT STATIONS TO NULL                 
         LA    RE,3(,R3)                                                        
         CLI   3(R3),C' '          THIS A BLANK                                 
         BNH   *+8                                                              
         LA    RE,1(,RE)                                                        
         MVI   0(RE),C'-'                                                       
         MVC   1(1,RE),4(R5)                                                    
         CLI   1(RE),C' '                                                       
         BH    PSET64                                                           
         MVC   1(2,RE),=C'TV'                                                   
         B     PSETEQ                                                           
PSET64   MVI   2(RE),C'M'                                                       
         B     PSETEQ                                                           
         SPACE                                                                  
PSET70   CLI   LASTSET,QLCON       CONTRACT TYPE                                
         BNE   PSETEQ                                                           
         OI    1(R3),X'40'                                                      
         SPACE                                                                  
PSETEQ   CR    RB,RB                                                            
         B     PSETX                                                            
PSETNE   LTR   RB,RB                                                            
PSETX    XIT1                                                                   
PSETCLC  CLC   0(0,R3),0(R5)                                                    
PSETMVC  MVC   0(0,R3),0(R5)                                                    
         DROP  RB,RC                                                            
         EJECT                                                                  
* SET SCREEN HEADINGS *                                                         
         SPACE                                                                  
SETSCR   NMOD1 0,**SSCR**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         XC    RGEHDG,RGEHDG                                                    
         OI    RGEHDGH+6,X'80'                                                  
         TM    DISPFLAG,DISPBOOK                                                
         BZ    SETSCR20                                                         
         SPACE                                                                  
* SET HEADINGS FOR BOOKED OPTION                                                
         SPACE                                                                  
         MVC   RGEHD1+12(8),=X'C29240A38889A240'   BK THIS                      
         MVC   RGEHD2+12(8),=X'40E69240D7998940'    WK PRI                      
         MVC   RGEHD1+22(8),=X'C29240A38889A240'   BK THIS                      
         MVC   RGEHD2+23(7),=X'E69240C3A49940'      WK CUR                      
         MVC   RGEHD1+32(5),=X'D799899699'         PRIOR                        
         MVC   RGEHD2+31(7),=X'C2899393899587'     BILLING                      
         MVC   RGEHD1+42(7),=X'C3A499998595A3'     CURRENT                      
         MVC   RGEHD2+42(7),=X'C2899393899587'     BILLING                      
         MVC   RGEHD1+50(6),=X'4040C3A49940'       CUR                          
         MVC   RGEHD2+50(8),=X'40D7818389958740'   PACING                       
         SPACE                                                                  
         CLI   ROUNDSW,C'Y'        THIS THOUSANDS                               
         BNE   SETSCR40                                                         
         MVC   RGEHDG+36(5),=C'(000)'                                           
         MVC   RGEHDG+47(5),=C'(000)'                                           
         MVC   RGEHDG+64(5),=C'(000)'                                           
         B     SETSCR40                                                         
         SPACE                                                                  
* SET HEADINGS TO STANDARD                                                      
         SPACE                                                                  
SETSCR20 DS   0H                                                                
         MVC   RGEHD1+12(8),=X'4040D79989969940'   PRIOR                        
         MVC   RGEHD2+12(8),=X'40C2899393899587'   BILLING                      
         MVC   RGEHD1+22(8),=X'40C3A499998595A3'   CURRENT                      
         MVC   RGEHD2+23(7),=X'C2899393899587'     BILLING                      
         MVC   RGEHD1+32(5),=X'4040C3A499'         CUR                          
         MVC   RGEHD2+31(8),=X'40D7818389958740'   PACING                       
         MVC   RGEHD1+42(7),=X'40D79989969940'     PRIOR                        
         MVC   RGEHD2+42(7),=X'40C68995819340'     FINAL                        
         MVC   RGEHD1+50(6),=X'D783A340A396'       PCT TO                       
         MVC   RGEHD2+50(8),=X'40C6899581934040'   FINAL                        
         SPACE                                                                  
         CLI   ROUNDSW,C'Y'        THIS THOUSANDS                               
         BNE   SETSCR40                                                         
         MVC   RGEHDG+17(5),=C'(000)'                                           
         MVC   RGEHDG+28(5),=C'(000)'                                           
         MVC   RGEHDG+47(5),=C'(000)'                                           
         MVC   RGEHDG+64(5),=C'(000)'                                           
         SPACE                                                                  
SETSCR40 DS   0H                                                                
         OI    RGEHD1H+6,X'80'                                                  
         OI    RGEHD2H+6,X'80'                                                  
         SPACE                                                                  
         MVC   RGEHD1+9(1),TYPEKEY  DISPLAY TYPE DATA                           
         MVI   RGEHD1+10,C' '                                                   
         CLI   TYPEKEY,0                                                        
         BNE   *+8                                                              
         MVI   RGEHD1+9,C'A'                                                    
         SPACE                                                                  
         CLI   TYPEKEY,C'D'        DIRECT IS NOW CONFIRMED DIRECT               
         BNE   SETSCR50                                                         
         MVC   RGEHD1+9(2),=C'CD'                                               
         SPACE                                                                  
         CLC   AGENCY,=C'BL'                                                    
         BE    SETSCR50                                                         
         CLC   AGENCY,=C'FN'                                                    
         BE    SETSCR50                                                         
         CLC   AGENCY,=C'PV'       THIS PETRY                                   
         BNE   SETSCR54                                                         
SETSCR50 DS    0H                                                               
         MVI   RGEHD1+9,C'A'       PETRY IS ALL DIRECT                          
         SPACE                                                                  
SETSCR54 DS    0H                                                               
         L     R1,=A(LISTOPTS)     GET CORRECT LIST HEADING                     
         A     R1,RELO                                                          
*                                                                               
SETSCR60 CLC   QLIST,14(R1)                                                     
         BE    SETSCR64                                                         
         LA    R1,L'LISTOPTS(R1)                                                
         CLI   0(R1),0                                                          
         BNE   SETSCR60                                                         
         DC    H'0'                                                             
         SPACE                                                                  
SETSCR64 MVC   RGEHD1(8),0(R1)                                                  
         SPACE                                                                  
         OC    QSTA,QSTA           IS STATION A FILTER                          
         BZ    SETSCR70                                                         
         CLI   QSTA,C'*'           SET                                          
         BE    SETSCR70             YES                                         
         CLI   QLIST,QLSTA         IF LISTING BY STA                            
         BE    SETSCR70             YES                                         
         SPACE                                                                  
         MVC   RGEHDG(4),STAMKT                                                 
         MVC   RGEHDG+6(L'EXMKTNAM),EXMKTNAM                                    
         OI    RGEHDGH+6,X'80'      FORCE TRANS                                 
         SPACE                                                                  
SETSCR70 EQU   *                                                                
         CLC   STEREO,CONSERV+1    STEREO REQUEST?                              
         BNE   SETSCRX                                                          
         CLI   QLIST,QLCAT                                                      
         BNE   SETSCRX                                                          
         CLI   FULLSTER,C'Y'       THIS FULL STEREO                             
         BNE   SETSCRX                                                          
         SPACE                                                                  
         MVC   RGEHD1(8),=CL8'CTGRY'                                            
         SPACE                                                                  
SETSCRX  XIT1                                                                   
         DROP  RB,RC                                                            
* TRACE I/O                                                                     
         SPACE                                                                  
TRACE    NMOD1 0,**TRAC**                                                       
         L     RC,SVRC             GET A(GEND) GENCON WORK AREA                 
         USING GEND,RC                                                          
         CLI   OFFLINE,C'Y'                                                     
         BNE   TRACEX                                                           
         TM    DISPFLAG,OPTRACE                                                 
         BZ    TRACEX                                                           
         L     R3,ASPOOLD                                                       
         USING SPOOLD,R3                                                        
         MVC   P,SPACES                                                         
         MVI   P+1,C'S'                                                         
         CLI   TRCTYPE,16                                                       
         BH    *+8                                                              
         MVI   P+1,C'H'                                                         
         SPACE                                                                  
         LA    R0,TRCTABCT                                                      
         LA    R1,TRCTABL                                                       
TRC010   CLC   TRCTYPE,0(R1)                                                    
         BE    TRC020                                                           
         LA    R1,11(,R1)                                                       
         BCT   R0,TRC010                                                        
         DC    H'0'                                                             
         SPACE                                                                  
TRC020   MVC   P+3(2),1(R1)                                                     
         MVC   P+6(8),3(R1)                                                     
         SPACE                                                                  
         MVC   P+15(3),=C'KEYSAVE'                                              
         MVC   P+23(3),KEY                                                      
         GOTO1 HEXOUT,DMCB,KEY+3,P+27,3,0,0                                     
         MVC   P+34(24),KEY+6                                                   
         GOTO1 HEXOUT,DMCB,KEY+30,P+59,2,0,0                                    
         SPACE                                                                  
         L     R4,AIO                                                           
         SPACE                                                                  
         CLI   ROKDTLTY-ROKEY(R4),0    THIS CONTROL RECORDS                     
         BE    TRC040                   YES, BYPASS DOLLAR EDITS                
         SPACE                                                                  
         ICM   R0,15,RODP2BLG-ROKEY(R4)                                         
         EDIT  (R0),(10,P+63),0,COMMAS=YES,FLOAT=-                              
         SPACE                                                                  
         ICM   R0,15,RODPPBLG-ROKEY(R4)                                         
         EDIT  (R0),(10,P+73),0,COMMAS=YES,FLOAT=-                              
         SPACE                                                                  
         ICM   R0,15,RODPCBLG-ROKEY(R4)                                         
         EDIT  (R0),(10,P+83),0,COMMAS=YES,FLOAT=-                              
         SPACE                                                                  
         ICM   R0,15,RODPPBUD-ROKEY(R4)                                         
         EDIT  (R0),(10,P+93),0,COMMAS=YES,FLOAT=-                              
         SPACE                                                                  
         ICM   R0,15,RODPCBUD-ROKEY(R4)                                         
         EDIT  (R0),(10,P+103),0,COMMAS=YES,FLOAT=-                             
         SPACE                                                                  
TRC040   DS   0H                                                                
         GOTO1 SPOOL,DMCB,(R3)                                                  
         SPACE                                                                  
         MVC   P+15(7),=C'KEYSAVE'                                              
         MVC   P+23(3),KEYSAVE                                                  
         GOTO1 HEXOUT,DMCB,KEY+3,P+27,3,0,0                                     
         MVC   P+34(24),KEYSAVE+6                                               
         GOTO1 HEXOUT,DMCB,KEYSAVE+30,P+59,2,0,0                                
         GOTO1 SPOOL,DMCB,(R3)                                                  
TRACEX   XIT1                                                                   
         SPACE                                                                  
TRCTABL  DC    X'01',CL10'01VKEY0500'                                           
         DC    X'02',CL10'02VKEY0530'                                           
         DC    X'03',CL10'03LREC CON'                                           
         DC    X'04',CL10'04LREC0100'                                           
         DC    X'05',CL10'05AFT FSET'                                           
         DC    X'06',CL10'06LT060'                                              
         DC    X'07',CL10'07LT800'                                              
         DC    X'08',CL10'08LMON0030'                                           
         DC    X'09',CL10'09CSETM300'                                           
         DC    X'0A',CL10'0ADR020'                                              
         DC    X'0B',CL10'0BCSETD240'                                           
         DC    X'0C',CL10'0CLMON0254'                                           
         DC    X'11',CL10'11LREC0172'                                           
         DC    X'12',CL10'12LT010'                                              
         DC    X'13',CL10'13LT030'                                              
         DC    X'14',CL10'14LT050'                                              
         DC    X'15',CL10'15LMON0046'                                           
         DC    X'16',CL10'16LMON0160'                                           
         DC    X'17',CL10'17LMON0210'                                           
         DC    X'18',CL10'18XPLAN'                                              
         DC    X'19',CL10'19XPLAN'                                              
         DC    X'1A',CL10'1AXPLAN'                                              
         DC    X'1B',CL10'1BXPLAN'                                              
         DC    X'1C',CL10'1CXPLAN'                                              
TRCTABCT EQU   (*-TRCTABL)/9                                                    
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
* DSECT FOR LCOMBOS - LIST OF VALID FILTERS & KEY COMBINATIONS TABLE            
         SPACE                                                                  
LCOMBOSD DSECT                                                                  
LFTRFLDS DS    XL3                                                              
LSTTYP   DS    XL1                                                              
LFLAG    DS    XL1                                                              
LALTKEY  DS    XL3                                                              
LCNEXT   EQU   *                                                                
         SPACE 3                                                                
RORECD   DSECT                                                                  
*                                                                               
       ++INCLUDE REGENRRGOD                                                     
         EJECT                                                                  
ROFFD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENOFF                                                       
         PRINT ON                                                               
         SPACE 1                                                                
RSTAD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENSTA                                                       
         PRINT ON                                                               
RBUDD    DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENBUD                                                       
         PRINT ON                                                               
         SPACE                                                                  
         PRINT ON                                                               
RREPRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENREP                                                       
         PRINT ON                                                               
         SPACE                                                                  
* DDSPOOLD                                                                      
*        PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
         SPACE                                                                  
* DDTSARD                                                                       
*        PRINT OFF                                                              
       ++INCLUDE DDTSARD                                                        
         PRINT ON                                                               
         SPACE                                                                  
* REGENSBLK - USED FOR SECURITY CHECKING                                        
SBLOCKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE REGENSBLK                                                      
         PRINT ON                                                               
         SPACE                                                                  
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
       ++INCLUDE RENRGFFD                                                       
         EJECT                                                                  
         PRINT ON                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RENRGF2D                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE RENRGE2D                                                       
LMONKEY  DS    CL48                LIST=MONTH KEY                               
*                                  APPENDED TO LONGER SCREEN                    
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         ORG   LSTSCMAP+170                                                     
***************                                                                 
*  RERRGWTWA  *                                                                 
***************                                                                 
       ++INCLUDE RERRGWTWA                                                      
         EJECT                                                                  
       ++INCLUDE FAUTL                                                          
         EJECT                                                                  
       ++INCLUDE RENRGWKN                                                       
         PRINT ON                                                               
* PUT MY STORAGE DSECT HERE IF NEEDED                                           
*                                                                               
         ORG   APPLORG                                                          
         DS    0F                                                               
ACOMBOS  DS    A                                                                
STADISP  DS    F                                                                
SVREGS   DS    2F                                                               
SVR5R6   DS    2F                                                               
SVREGE   DS    F                                                                
RELO     DS    A                                                                
CDTBADR  DS    A                   FIRST/CURR CODE TABLE ADDRESS                
         SPACE                                                                  
STRPAGE  EQU   1                                                                
MAXPAGE  EQU   4                                                                
TMPSTRCT DS    XL1                 COUNT OF TEMPSTOR PAGES USED                 
TMPSTRPG DS    XL1                 CURRENT TEMPSTOR PAGE                        
         SPACE                                                                  
COMBOSTA DS    (CSTAMAX)CL7                                                     
CSTAMAX  EQU   8                                                                
NCOMBOS  DS    X                                                                
FIRSTSTA DS    C                                                                
LASTLN   DS    C                                                                
ERREX2SW DS    C                                                                
LISTEND  DS    C                                                                
FIRSTR   DS    C                                                                
OFFSTASW DS    C                                                                
ROUNDSW  DS    C                                                                
MONTH    DS    X                                                                
LIMACC   DS    X                                                                
LIMACOFF EQU   X'80'                                                            
LIMACSTA EQU   X'40'                                                            
LSTTYPE  DS    X                   TYPE DATA BEING LISTED (ADV/AGY/STA)         
LSTTYPLN DS    X                   LEN OF DATA LISTED                           
LSTKEY   DS    XL(L'ROKDTLTY)                                                   
LSTKEYEX DS    XL1                                                              
LSTDISP  DS    XL1                                                              
DISPFLAG DS    XL1                 DISPLAY FLAG                                 
DISPBOOK EQU   X'80'                                                            
DISPCONF EQU   X'40'                                                            
DISPDIR  EQU   X'20'                                                            
DISPUNCN EQU   X'10'                                                            
OPTRACE  EQU   X'08'               OFFLINE TRACE                                
OPCONDCT EQU   X'04'               CONTYPE/DEVTYPE HARD CODE ON                 
DISTOTSW EQU   X'02'               STEREO - SHOWN TOTALS LINE?                  
         SPACE                                                                  
SVTYPES  DS    CL5                 POSSIBLE TYPES ON THIS REPS FILE             
         SPACE                                                                  
TYPEKEY  DS    CL1                 NULL= ALL DOLLARS                            
*                                  C = CONFIRMED                                
*                                  U = UNCONFIRMED                              
*                                  D = DIRECT CONFIRMED                         
*                                  R = DIRECT                                   
LSTVAL   DS    CL8                                                              
         SPACE                                                                  
WARNDATE DS    CL8                                                              
STATION  DS    CL5                                                              
CURWKS   DS    XL12                                                             
CURWKSY  DS    XL12                                                             
PRIWKS   DS    XL12                                                             
PRIWKSY  DS    XL12                                                             
*                                  N  =  DON'T TRY TO RETRIEVE IT               
SETKEY   DS    XL3                 KEY OF FILTERS AND POSSIBLE L=               
*                                  WITH X'80' SET ON IF SETS                    
TRCTYPE  DS    XL1                                                              
FOUNDSET DS    CL1                                                              
ANYDATA  DS    CL1                 N=NO DATA FOUND, Y=DID FIND DATA             
FULLSTER DS    CL1                 Y IF FULL STEREO, N IF EMULATOR              
SETKEYAD DS    A                                                                
ATSAR    DS    A                                                                
TSARBLK  DS    XL34                TSAR BLOCK                                   
         SPACE                                                                  
TOPNCT   DS    XL1                 TOP N COUNT REQUESTED                        
SCRNCT   DS    CL1                 LINES DISPLAYED ON SCREEN                    
TSARCT   DS    H                   COUNT OF RECS IN TSAR                        
TOTLCT   DS    H                   COUNT OF LINES BEFORE TOTAL                  
*                                                                               
TOPTYPE  DS    CL1                 TYPE TO SORT FOR TOP                         
*                 C=CURR BILL, P=PRIOR BILL,F=FINAL, B=BUDGET                   
*                                                                               
         DS    CL37                SPARE                                        
CODETABL EQU   *                   TABLE OF CODES FOR LONG NAMES                
CODETBLN EQU   SET1TAB-CODETABL                                                 
         SPACE                                                                  
BOOKSIZE EQU   6                                                                
         SPACE                                                                  
* SET1TAB-SET2TAB-SET3TAB LIVE HERE FROM X'27E8' TO END OF SYSD X'3148'         
         SPACE                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114RENRG07   05/01/02'                                      
         END                                                                    
