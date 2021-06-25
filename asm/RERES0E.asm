*          DATA SET RERES0E    AT LEVEL 160 AS OF 04/14/16                      
*PHASE T8190EC                                                                  
*INCLUDE UPOUT                                                                  
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - HISTORY'                
*                                                                               
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*------------------------------------------------------------------*            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
*   JUN28/00 (BU ) --- REMOVE REFERENCES TO GLV1GOTO PER MEL H.    *            
*                                                                  *            
*   NOV10/00 (FJD) --- FIX UNIVERSES                               *            
*                      ADDED SOME RELATIVE BRANCHING AND NEW       *            
*                      NTR1 BASE=*,LABEL=* TO INCREASE             *            
*                      ADDRESSABILITY                              *            
*                                                                  *            
*   JUL12/01 (BU ) --- CHANGES TO 'DUMMY' WORKSPACE USE            *            
*                                                                  *            
*    APR/09  (SMY) --- SUPPORT NEW INVENTORY KEY                   *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - INIT'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS - INITIALIZATION         *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
T8190E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**190E**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     R7,ATWA                                                          
         USING CONHEADH-64,R7      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R2,RELO             SAVE PROGRAM RELOCATION FACTOR               
         ST    RB,MYBASE                                                        
*                                                                               
         LH    RF,=Y(TXTWRK-SYSD)                                               
         LA    RF,SYSD(RF)                                                      
         ST    RF,ATXTWRK                                                       
*                                                                               
         LH    RF,=Y(DPTBL-SYSD)                                                
         LA    RF,SYSD(RF)                                                      
         ST    RF,ADPTBL                                                        
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - CHKMODE'                
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        CHECK CALLING MODE                                        *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
CHKMODE  DS    0H                                                               
*                                                                               
         CLI   MODE,PROCPFK        HANDLE PF KEYS                               
         BNE   CHKM10                                                           
         BRAS  RE,PPFK                                                          
         B     CHKMX                                                            
                                                                                
CHKM10   CLI   MODE,VALKEY         HANDLE PF KEYS                               
         BNE   CHKM20                                                           
         BRAS  RE,VKEY                                                          
         B     CHKMX                                                            
                                                                                
CHKM20   CLI   MODE,VALREC         VALIDATE RECORD                              
         BNE   CHKM30                                                           
         BRAS  RE,VREC                                                          
         B     CHKMX                                                            
                                                                                
CHKM30   CLI   MODE,PRINTREP       PRINT REPORT                                 
         BNE   CHKMX                                                            
         BRAS  RE,PREP                                                          
                                                                                
CHKMX    XIT1                                                                   
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VKEY'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE KEY                                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VKEY     NTR1  BASE=*,LABEL=*                                                   
*                                     ANALYZE PFKEYS                            
         MVI   PRINTOPT,X'00'      CLEAR OUT PRINTOPT                           
         LA    RF,SAVELN                                                        
         LA    RE,SYSSPARE         INIT SAVEAREA                                
         XCEF                                                                   
         MVC   STAMP,=CL8'T8190E'  STAMP SAVEAREA                               
*                                                                               
********************************************************************            
*                                                                  *            
*     GET PARENT REP FROM REP RECORD FOR X'62' AND X'E2' LOOKUP.   *            
*         (USE GIVEN REP (AGENCY) FOR DEMO MENU LOOKUP)            *            
*                                                                  *            
********************************************************************            
         SPACE 1                                                                
         XC    KEY,KEY             GET PARENT REP FROM REP RECORD               
         LA    R4,KEY                                                           
         USING RREPKEY,R4                                                       
         MVI   RREPKTYP,X'01'                                                   
         MVC   RREPKREP,AGENCY                                                  
         DROP  R4                                                               
         GOTO1 READ                                                             
         GOTO1 GETREC                                                           
         MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BRAS  RE,GETEL                                                         
         USING RREPELEM,R6                                                      
         MVC   REPPAR,RREPPAR      SAVE PARENT REP                              
         DROP  R6                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - PPFK'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        ANALYZE PFKEY ENTRY                                       *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PPFK     NTR1  BASE=*,LABEL=*                                                   
*                                     ANALYZE PFKEYS                            
         CLI   PFAID,0             IF PFKEY ENTERED                             
         BE    PPFKX                                                            
*                                                                               
         GOTO1 =A(PFKEYS),RR=RELO     GO ANALYZE                                
         BE    PPFKX                  NO ERRORS                                 
*                                                                               
         GOTO1 ERREX                  CHECK FOR ERRORS                          
*                                                                               
PPFKX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VREC'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE RECORD                                           *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VREC     NTR1  BASE=*,LABEL=*                                                   
         MVI   PRINTOPT,X'00'      CLEAR OUT PRINTOPT                           
*                                                                               
         CLI   THISLSEL,REPSELQ    SKIP IF REPORT REQUESTED                     
         BE    *+8                                                              
         OI    GENSTAT2,RETEQSEL   RE-DISPLAY AFTER CHANGE                      
*                                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *           SOURCE               *           
*                                  *                                *           
*                                  **********************************           
         LA    R2,MASSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VBK'                    
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE DEMO BOOKS                                       *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VBK      DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *           BOOK(S)              *           
*                                  *                                *           
*                                  **********************************           
         LA    R2,MASBKSH          VALIDATE BOOK                                
         GOTO1 ANY                                                              
         MVC   DMCB+8(4),=C',=,-'                                               
         GOTO1 SCANNER,DMCB,(R2),(6,BLOCK1)                                     
         LA    R4,BLOCK1                                                        
         CLI   DMCB+4,1            MORE THAN 1 FIELD                            
         BH    BK10                                                             
         MVI   ERROR,2             INVALID INPUT FIELD                          
         JL    ERREND                                                           
         SPACE 1                                                                
         CLI   1(R4),0             1 FIELD MAY BE RANGE                         
         BE    BK10                OR JUST 1 BOOK                               
         SPACE 1                                                                
         OI    PRINTOPT,X'10'      INDICATE RANGE OF BOOKS                      
         ZIC   R5,0(R4)            LENGTH OF 1ST HALF OF FIELD                  
         LA    R5,8(R2,R5)                                                      
         CLI   0(R5),C'-'                                                       
         JNE   ERREND                                                           
         MVI   0(R5),C','          DUMMY FOR BOOKVAL (RESTORED BELOW)           
         SPACE 1                                                                
BK10     MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         MVC   NUMBOOK,ACTUAL      SAVE NUMBER OF BOOKS                         
         CLI   ACTUAL,6            REALLY ONLY 6 BOOKS ALLOWED                  
         BNH   BK20                                                             
         L     RF,=A(MANYBKS)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'MANYBKS),0(RF)      TOO MANY BOOKS                     
         J     MYEND                                                            
         SPACE 1                                                                
BK20     TM    PRINTOPT,X'10'      RANGE OF BOOKS CAN'T HAVE                    
         BZ    BK50                                                             
         MVI   0(R5),C'-'          (RESTORE HYPHEN)                             
         TM    BOOK+0,X'FF'-X'41'  SPECIAL BOOK FILTER                          
         BNZ   BK30                                                             
         TM    BOOK+4,X'FF'-X'41'                                               
         BZ    BK40                                                             
BK30     DS    0H                                                               
         L     RF,=A(BADBK1)                                                    
         A     RF,RELO                                                          
         MVC   CONHEAD(L'BADBK1),0(RF)                                          
         J     MYEND                                                            
         SPACE 1                                                                
BK40     CLC   BOOK+1(2),BOOK+5    START CANNOT BE GREATER THAN END             
         BNH   BK50                                                             
         L     RF,=A(BADBK3)                                                    
         A     RF,RELO                                                          
         MVC   CONHEAD(L'BADBK3),0(RF)                                          
         J     MYEND                                                            
*                                                                               
BK50     MVI   WORK,X'00'          BOOK(S) MUST BE SAME AS SOURCE               
         CLI   SVSOURCE,C'A'                                                    
         BE    BK55                ARB=X'00'  NSI=X'40'  SRC=X'41'              
         MVI   WORK,X'40'                                                       
         CLI   SVSOURCE,C'N'                                                    
         BE    BK55                                                             
         OI    WORK,X'01'                                                       
BK55     ZIC   R5,NUMBOOK          R5=LOOP CONTROL                              
         LA    R4,BOOK             R4=A(NEXT BOOK)                              
BK60     MVC   WORK+1(1),0(R4)                                                  
         NI    WORK+1,X'41'                                                     
         CLC   WORK(1),WORK+1                                                   
         BNE   BK90                DIFFERENT SOURCE                             
         LA    R4,4(R4)                                                         
         BCT   R5,BK60                                                          
         B     STA00                                                            
*                                                                               
BK90     DS    0H                                                               
         L     RF,=A(BADBK2)                                                    
         A     RF,RELO                                                          
         MVC   CONHEAD(L'BADBK2),0(RF)                                          
         J     MYEND                                                            
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VSTA'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE STATION                                          *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VSTA     DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *           STATION              *           
*                                  *                                *           
*                                  **********************************           
STA00    LA    R2,MASSTNH          VALIDATE STATION                             
*                                                                               
         MVI   STATSW,C'I'         WILLING TO ACCEPT INVALID STATIONS           
         GOTO1 VVALSTA             VALIDATE STATION ON DEMO FILE                
*                                                                               
         MVC   STATSV(5),ACTSTAT                                                
*                                                                               
*        GET MARKET NAME FROM STATION RECORD                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,X'02'                                                        
         MVC   KEY+20(2),AGENCY                                                 
         MVC   KEY+22(5),ACTSTAT                                                
         CLI   KEY+26,C'T'                                                      
         BE    STA05                                                            
         CLI   KEY+26,X'F0'                                                     
         BL    STA10                                                            
         CLI   KEY+26,X'F9'                                                     
         BH    STA10                                                            
*                                                                               
STA05    MVI   KEY+26,C' '                                                      
*                                                                               
STA10    GOTO1 HIGH                                                             
*                                                                               
         MVI   ERROR,112          STATION RECORD NOT ON FILE                    
         CLC   KEY(27),KEYSAVE                                                  
         JNE   ERREND                                                           
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DEMO00                                                           
         MVC   MKTSV(20),2(R6)    MARKET NAME                                   
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VDEM'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE DEMOS (& DEMO MENU)                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VDEM     DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *     DEMOS (& DEMO MENU)        *           
*                                  *                                *           
*                                  **********************************           
DEMO00   LA    R2,MASDEMH          VALIDATE DEMOS                               
         MVI   NFLDS,2             SET FOR 2 DEMO FIELDS                        
         MVI   ERROR,2             INVALID INPUT FIELD                          
         CLC   8(3,R2),=C'ALL'     DEFAULT MENU                                 
         BE    DEM10                                                            
         CLC   8(2,R2),=C'M='      OR SPECIFIC MENU                             
         BNE   DEM50                                                            
         CLI   5(R2),4             MENU IS 2 A/N CHARACTERS                     
         BE    DEM10                                                            
         L     RF,=A(BADMENU)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'BADMENU),0(RF)                                         
         J     MYEND                                                            
         SPACE 1                                                                
DEM10    XC    KEY,KEY             VALIDATE DEMO MENU                           
         MVI   KEY,X'23'                                                        
         MVC   KEY+23(2),REPPAR    USE MAIN REP (NOT PARENT REP)                
         MVC   KEY+25(2),=C'ZZ'    DEFAULT MENU                                 
         CLC   8(3,R2),=C'ALL'                                                  
         BE    *+10                                                             
         MVC   KEY+25(2),10(R2)    OR CHOSEN MENU                               
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         JNE   ERREND                                                           
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         USING RDEMREC,R6                                                       
         MVC   NUMDEMS,RDEMNUM     SAVE NUMBER OF DEMOS                         
         DROP  R6                                                               
         CLI   MASOP5,C'Y'         IF ROUNDING DEMOS                            
         BNE   DEM19               THEN DEM#RNDQ IS MAX NUM OF DEMOS            
         CLI   NUMDEMS,DEM#RNDQ                                                 
         BNH   *+8                                                              
         MVI   NUMDEMS,DEM#RNDQ                                                 
         B     DEM20                                                            
DEM19    EQU   *                                                                
         CLI   NUMDEMS,DEM#MAXQ    MAX DEM#MAXQ NORMAL DEMOS                    
         BNH   DEM20                                                            
         MVI   NUMDEMS,DEM#MAXQ                                                 
DEM20    EQU   *                                                                
         MVI   ELCODE,X'02'                                                     
         LA    R6,IO                                                            
         BRAS  RE,GETEL                                                         
         USING RDEMDEL,R6                                                       
         MVC   DEMLST,RDEMDEM      SAVE DEMOS                                   
         B     DEM80                                                            
         DROP  R6                                                               
DEM50    MVI   MAX,DEM#MAXQ+2      DUMMY SO I CAN DO ERROR MSG                  
*                                                                               
*        ALWAYS ALLOW UP TO DEM#MAXQ DEMOS-DISPLAY MAY LOOK ODD                 
*                                                                               
         CLI   MASOP5,C'Y'         IF ROUNDING DEMOS                            
         BNE   *+8                 THEN DEM#RNDQ IS MAX NUM OF DEMOS            
         MVI   MAX,DEM#RNDQ+2      DUMMY SO I CAN DO ERROR MSG                  
*                                                                               
         GOTO1 VVALDEM                                                          
*                                                                               
         CLI   MASOP5,C'Y'         IF ROUNDING DEMOS                            
         BNE   DEM52               THEN DEM#RNDQ IS MAX NUM OF DEMOS            
*                                                                               
         CLI   ACTUAL,DEM#RNDQ                                                  
         BNH   DEM60                                                            
         B     DEM55                                                            
*                                                                               
DEM52    DS    0H                                                               
         CLI   ACTUAL,DEM#MAXQ     CHECK AGAINST MAX NUMBER OF DEMOS            
         BNH   DEM60                                                            
*                                                                               
DEM55    DS    0H                                                               
         L     RF,=A(MANYDEM)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'MANYDEM),0(RF)                                         
         J     MYEND                                                            
*                                                                               
DEM60    MVC   NUMDEMS,ACTUAL      SAVE NUMBER OF DEMOS                         
         MVC   DEMLST,DEMOS                                                     
*                                                                               
*                                                                               
*        THE FOLLOWING IS A HORRIBLE KLUGE TO AVOID THE FORMATTING              
*        PROBLEM ENCOUNTERED WHEN LARGE UNIVERSE #S ARE REPORTED                
*        WITH ROUNDING OPTION AND/OR MORE THAN 17 DEMOS REQUESTED               
                                                                                
DEM80    BRAS  RE,CHKUNIV          WERE UNIVERSES REQUESTED?                    
         BNE   DEMVALX             NO, SKIP TESTS                               
         CLI   NUMDEMS,12          YES, IF MORE THAN 12 DEMOS                   
         BNH   DEM90                                                            
         LHI   R1,UNIVERR1              THEN, ERROR                             
         BRAS  RE,NEWERRS                                                       
         DC    H'0'                           SHOULD NOT RETURN                 
                                                                                
DEM90    BRAS  RE,CHKDMA                ELSE, IF DMA IMPS REQUESTED             
         BNE   DEM95                    THEN, ERROR                             
         LHI   R1,DMAERROR                                                      
         BRAS  RE,NEWERRS                                                       
         DC    H'0'                           SHOULD NOT RETURN                 
                                                                                
                                                                                
                                                                                
DEM95    CLI   MASOP5,C'Y'              ELSE, IF ROUNDING OPT='Y'               
         BNE   DEMVALX                                                          
         LHI   R1,UNIVERR2                    THEN, ERROR                       
         BRAS  RE,NEWERRS                                                       
         DC    H'0'                                 SHOULD NOT RETURN           
                                                                                
                                                                                
DEMVALX  DS    0H                                                               
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VRTCD'                  
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE RATE CODE                                        *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VRTCD    DS    0H                                                               
*                                                                               
         XC    SVRTCD,SVRTCD       INIT RATE CARD ID SAVEAREA                   
*                                                                               
         LA    R2,MASRTCDH         POINT TO RATE CODE FIELD                     
*                                                                               
         GOTO1 VVALRTCD,DMCB,MASRTCDH,MASRCDH,SVRTCD,SVYSQ                      
*                                                                               
         MVC   SVYSQN,DMCB+12      SAVE NUMBER YR/SLN/QTR TABLE                 
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VSLN'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE SPOT LENGTHS                                     *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VSLN     DS    0H                                                               
*                                                                               
         LA    R2,MASLENSH         POINT TO SPOT LENGTH FIELD                   
         XC    SVSLN,SVSLN         INIT SPOT LENGTHS SAVEAREA                   
*                                                                               
         GOTO1 VVALSLN,DMCB,MASLENSH,SVSLN                                      
         MVC   SVSLNN,DMCB+4       SAVE NUMBER SPOT LENGTHS                     
*                                                                               
         OC    SVRTCD,SVRTCD       IF RATE CODE ENTERED                         
         BZ    VSLN10                                                           
*                                                                               
         OC    SVSLNN,SVSLNN          INPUT REQUIRED                            
         BNZ   VSLN20                                                           
*                                                                               
         MVI   ERROR,1                MISSING INPUT                             
         J     ERREND                                                           
*                                                                               
VSLN10   DS    0H                                                               
*                                                                               
         OC    SVSLN,SVSLN         IF SPOT LENGTHS ENTERED                      
         BZ    VSLN20                 RATE CODE IS REQUIRED                     
*                                                                               
         LA    R2,MASRCDH                                                       
         MVI   ERROR,1                MISSING INPUT                             
         J     ERREND                                                           
*                                                                               
VSLN20   DS    0H                                                               
*                                                                               
VSLNX    DS    0H                                                               
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VQTR'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE QUARTERS                                         *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VQTR     DS    0H                                                               
*                                                                               
         LA    R2,MASQTRSH         POINT TO QUARTER FIELD                       
         XC    SVQTR,SVQTR         INIT QUARTERS SAVEAREA                       
*                                                                               
         GOTO1 VVALQTR,DMCB,MASQTRSH,SVQTR                                      
         MVC   SVQTRN,DMCB+4       SAVE NUMBER QUARTERS                         
*                                                                               
         OC    SVRTCD,SVRTCD       IF RATE CODE ENTERED                         
         BZ    VQTR10                                                           
*                                                                               
         OC    SVQTRN,SVQTRN          INPUT REQUIRED                            
         BNZ   VQTR20                                                           
*                                                                               
         MVI   ERROR,1                MISSING INPUT                             
         J     ERREND                                                           
*                                                                               
VQTR10   DS    0H                                                               
*                                                                               
         OC    SVQTR,SVQTR         IF QUARTERS ENTERED                          
         BZ    VQTR20                 RATE CODE IS REQUIRED                     
*                                                                               
         LA    R2,MASRCDH                                                       
         MVI   ERROR,1                MISSING INPUT                             
         J     ERREND                                                           
*                                                                               
VQTR20   DS    0H                                                               
*                                                                               
*                                                                               
VQTRX    DS    0H                                                               
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - DPTVAL'                 
*                                  **********************************           
*                                  *                                *           
*                                  *          DAYPART(S)            *           
*                                  *                                *           
*                                  **********************************           
DPTVAL   LA    R2,MASDPTH          VALIDATE DAYPART                             
*                                                                               
         XC    DPLIST,DPLIST       INIT DAYPART LIST                            
         XC    DPMENU,DPMENU       INIT DAYPART MENU CODE                       
*                                                                               
         CLI   5(R2),0             IF NOT ENTERED                               
         BE    *+10                                                             
         CLC   8(3,R2),=C'ALL'     OR ALL                                       
         BNE   *+14                                                             
         MVC   DPMENU,=C'ALL '        USE MENU 'ALL '                           
         B     DPTMENU                                                          
*                                                                               
         CLI   5(R2),1                                                          
         BNE   *+8                                                              
         OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
*                                                                               
         CLC   =C'M=',8(R2)        MENU IF IT STARTS 'M='                       
         BNE   DPT05                                                            
*                                                                               
         MVC   DPMENU,10(R2)       SAVE MENU CODE                               
         OC    DPMENU,SPACES       SPACE FILL                                   
*                                                                               
         B     DPTMENU                                                          
*                                                                               
DPT05    DS    0H                                                               
*                                                                               
         ZIC   RF,5(R2)            NUMBER OF DAYPARTS                           
         STC   RF,NOREQDPT         SAVE NUMBER OF REQUESTED DAYPARTS            
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),8(R2)     SAVE DAYPART LIST                            
*                                                                               
         B     DPTMENUX                                                         
*                                                                               
*        READ SET RECORD FOR DAYPART MENU                                       
*                                                                               
DPTMENU  DS    0H                                                               
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH SET RECORD KEY                     
         USING RSETKEY,R4                                                       
*                                                                               
         MVI   RSETKTYP,RSETKTYQ   SET AS SET RECORD RECORD                     
         MVC   RSETKREP,REPPAR     SET REP CODE                                 
         MVC   RSETKSET,=C'DP'     SET SET CODE                                 
         MVC   RSETKID,DPMENU      SET SET IDENTIFIER                           
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTMENUE                                                         
*                                                                               
         GOTO1 GETREC              READ IN SET RECORD                           
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,RSETMCDQ     FIND MEMBERS ELEMENT                         
         BRAS  RE,GETEL                                                         
         BNE   DPTMENUE            MUST FIND ELEMENT                            
*                                                                               
         USING RSETMEMD,R6         ESTABLISH MEMBERS ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSETMELN         GET ELEMENT LENGTH                           
         SH    RF,=Y(RSETMTOV)     DECREMENT BY OVERHEAD LENGTH                 
         BNP   DPTMENUE            MUST HAVE SOME MEMBERS                       
*                                                                               
         STC   RF,NOREQDPT         SET NUMBER OF DAYPARTS                       
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DPLIST(0),RSETMEMB  COPY DAYPARTS                                
*                                                                               
DPTMENUX DS    0H                                                               
*                                                                               
*        VALIDATE INDIVIDUALLY ENTERED DAYPARTS                                 
*                                                                               
         XC    KEY,KEY                                                          
         LA    R4,KEY              ESTABLISH DAYPART RECORD KEY                 
         USING RRDPKEY,R4                                                       
*                                                                               
         MVI   RRDPKTYP,RRDPKIDQ   SET AS RESEARCH DAYPART RECORD               
         MVC   RRDPKREP,REPPAR     SET REP CODE                                 
*                                                                               
         LA    R5,DPLIST           START OF INPUT                               
*                                                                               
         ZIC   R0,NOREQDPT         # OF REQUESTED DAYPARTS                      
*                                                                               
         L     R3,ADPTBL           ESTABLISH DAYPART TABLE                      
         USING DPTBLD,R3                                                        
         XC    DPTBLD(DPTBLL),DPTBLD   INIT FIRST ENTRY                         
*                                                                               
DPTLOOP  DS    0H                                                               
*                                                                               
         MVC   RRDPKDPT,0(R5)      SET NEXT DAYPART IN KEY                      
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
         CLC   KEY(27),KEYSAVE     MUST FIND RECORD                             
         BNE   DPTINVE                                                          
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
         LA    R6,IO               POINT TO FOUND RECORD                        
         MVI   ELCODE,X'01'        SEARCH FOR DAYPART ELEMENT                   
         BRAS  RE,GETEL                                                         
         BNE   DPTCONT             IGNORE IF NOT FOUND                          
*                                                                               
         USING RRDPELEM,R6         ESTABLISH DAYPART ELEMENT                    
*                                                                               
         MVC   DPTBCODE,RRDPKDPT   SAVE DAYPART CODE                            
         MVC   DPTBSNAM,RRDPSNAM   SAVE SHORT NAME                              
         MVC   DPTBLNAM,RRDPLNAM   SAVE LONG NAME                               
*                                                                               
         LA    R3,DPTBLL(R3)       BUMP TO NEXT ENTRY IN DPTBL                  
         XC    DPTBLD(DPTBLL),DPTBLD  INIT NEXT ENTRY                           
*                                                                               
DPTCONT  DS    0H                                                               
*                                                                               
         LA    R5,1(R5)            BUMP TO NEXT ENTERED DAYPART                 
         BCT   R0,DPTLOOP                                                       
*                                                                               
DPTDONE  DS    0H                                                               
*                                                                               
         B     DPTVALX                                                          
*                                                                               
DPTMENUE DS    0H                  INVALID DAYPART MENU ID                      
         L     RF,=A(DPMENUER)                                                  
         A     RF,RELO                                                          
         MVC   CONHEAD(L'DPMENUER),0(RF)                                        
         J     MYEND                                                            
*                                                                               
DPTINVE  DS    0H                  INVALID DAYPART                              
         L     RF,=A(DPINVER)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'DPINVER),0(RF)                                         
         J     MYEND                                                            
*                                                                               
DPTVALX  DS    0H                                                               
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VDAY'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE DAY                                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VDAY     DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *               DAY              *           
*                                  *                                *           
*                                  **********************************           
DAY      LA    R2,MASDAYH          DAY (OPTIONAL)                               
         MVI   DAYOPT,X'FF'        DAYOPT                                       
         CLI   5(R2),0                                                          
         BE    FTR                                                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    FTR                                                              
         SPACE 1                                                                
         GOTO1 VVALDAY                                                          
         OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
         MVC   DAYOPT,ACTUAL                                                    
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *             FILTER             *           
*                                  *                                *           
*                                  **********************************           
FTR      LA    R2,MASFTRH          FILTER (OPTIONAL)                            
         CLI   5(R2),0                                                          
         BE    FTRX                                                             
         OI    PRINTOPT,X'08'      INDICATE FILTERS SELECTED                    
         MVC   NUMFILT,5(R2)       SAVE NUMBER OF FILTERS                       
         CLI   5(R2),6             CAN HAVE UP TO 6                             
         BNH   FTRX                                                             
         L     RF,=A(MANYFLT)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'MANYFLT),0(RF)                                         
         J     MYEND                                                            
*                                                                               
FTRX     DS    0H                                                               
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VINV'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE INVENTORY NUMBER(S)                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *        INVENTORY NUMBER        *           
*                                  *                                *           
*                                  **********************************           
INVMAX   EQU   30                  MAX INVENTORY NUMBERS ALLOWED                
*                                                                               
VINV     LA    R2,MASINVH          INVENTORY NUMBER (OPTIONAL)                  
*                                                                               
         MVC   SVINV(8),SPACES     INIT INV SAVEAREA                            
*                                                                               
         CLI   5(R2),0             IF THERE IS NO INPUT                         
         BNE   VINV10                                                           
*                                                                               
         CLI   MASDPTH+5,0            WE MUST HAVE A DAYPART ENTERED            
         BNE   VINVDONE                                                         
*                                                                               
         LA    R2,MASDPTH                                                       
         MVI   ERROR,1                MISSING INPUT                             
         J     ERREND                                                           
*                                                                               
VINV10   DS    0H                                                               
*                                                                               
         GOTO1 ANY                 READ IN INVENTORY NUMBERS                    
*                                                                               
         LH    R1,=Y(BUFF-SYSD)    GET START OF BUFFER ADDRESS                  
         LA    R1,SYSD(R1)                                                      
         ST    R1,SBUFF            SAVE A(BUFFER START)                         
*                                                                               
         MVC   DMCB+8(4),=C',=,-'  SCAN FOR SINGLE AND RANGES                   
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),('INVMAX',SBUFF)                               
*                                                                               
         CLI   DMCB+4,0            MUST HAVE AT LEAST ONE INV NO.               
         BH    VINV20                                                           
*                                                                               
         MVI   ERROR,2             INVALID INPUT FIELD                          
         JL    ERREND                                                           
*                                                                               
VINV20   DS    0H                                                               
*                                                                               
         LA    R0,INVMAX           MAX NUMBER OF INVENTORY NUMBERS              
         L     R4,SBUFF            POINT TO SCANNER BLOCK                       
         LA    R5,SVINV            POINT TO INVENTORY SAVEAREA                  
*                                                                               
VINVLOOP DS    0H                                                               
*                                                                               
         MVC   0(4,R5),SPACES      INIT SAVED INVENTORY NUMBER                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,0(R4)          LENGTH OF FIRST INVENTORY NUMBER             
         BZ    VINVDONE            END OF INPUT                                 
*                                                                               
         CH    RF,=AL2(L'RINVKINV) CHECK ON MAX LENGTH                          
         BH    VINVLNGE                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),12(R4)      FIRST INVENTORY NUMBER                       
*                                                                               
         MVC   4(4,R5),0(R5)       COPY INV NUMBER                              
*                                                                               
         ICM   RF,1,1(R4)          LENGTH OF SECOND INVENTORY NUMBER            
         BZ    VINVLP10            SINGLE INVENTORY NUMBER                      
*                                                                               
         CH    RF,=AL2(L'RINVKINV) CHECK ON MAX LENGTH                          
         BH    VINVLNGE                                                         
*                                                                               
         MVC   4(4,R5),SPACES      INIT SECOND INVENTORY NUMBER                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   4(0,R5),22(R4)      SECOND INVENTORY NUMBER                      
*                                                                               
VINVLP10 DS    0H                                                               
*                                                                               
*        READ FOR GENERAL AVAILS POINTER                                        
*                                                                               
         XC    KEY,KEY             ESTABLISH INVENTORY RECORD KEY               
         LA    R6,KEY                                                           
         USING RINVKEY,R6                                                       
*                                                                               
         MVI   RINVKTYP,RINVKTYQ   SET INVENTORY RECORD TYPE                    
         MVC   RINVKREP,REPPAR     USE PARENT REP                               
         MVC   RINVKSTA,ACTSTAT    USE ACTIVE STATION                           
         MVC   RINVKINV,0(R5)      USE STARTING INVENTORY NUMBER                
*                                                                               
         GOTO1 HIGH                READ FOR POINTER                             
*                                                                               
         CLC   RINVKEY(RINVKINV-RINVKEY),KEYSAVE   SAME STATION                 
         BNE   VINVNFE                                                          
*                                                                               
         CLC   RINVKINV,0(R5)      INV NO. MUST BE IN RANGE                     
         BL    VINVNFE                                                          
*                                                                               
         CLC   RINVKINV,4(R5)      INV NO. MUST BE IN RANGE                     
         BH    VINVNFE                                                          
*                                                                               
VINVCONT DS    0H                                                               
*                                                                               
         LA    R4,32(R4)           NEXT SCANNED BLOCK                           
         LA    R5,8(R5)            NEXT SAVEAREA                                
         BCT   R0,VINVLOOP                                                      
*                                                                               
         B     VINVMAXE            TOO MANY INVENTORY NUMBERS                   
*                                                                               
VINVDONE DS    0H                  ALL INV NOS. VALID                           
*                                                                               
         OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
*                                                                               
         B     VINVX                                                            
*                                                                               
*        INVENTORY VALIDATION ERROR MESSAGES                                    
*                                                                               
VINVNFE  DS    0H                  INVENTORY NUMBER NOT FOUND                   
*                                                                               
         L     RF,=A(VINVNFM)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'VINVNFM),0(RF)                                         
*                                                                               
         B     VINVERR                                                          
*                                                                               
VINVNFM  DC    C'** ERROR ** INVENTORY NUMBER NOT ON FILE'                      
*                                                                               
VINVLNGE DS    0H                  INVENTORY NUMBER TOO LONG                    
*                                                                               
         MVC   CONHEAD(L'VINVLNGM),VINVLNGM                                     
*                                                                               
         B     VINVERR                                                          
*                                                                               
VINVLNGM DC    C'** ERROR ** INVENTORY NUMBER MUST BE AT MOST 4 LONG'           
*                                                                               
VINVMAXE DS    0H                  TOO MANY INVENTORY NUMBERS                   
*                                                                               
         MVC   CONHEAD(L'VINVMAXM),VINVMAXM                                     
*                                                                               
         B     VINVERR                                                          
*                                                                               
VINVMAXM DC    C'** ERROR ** TOO MANY INVENTORY NUMBERS'                        
*                                                                               
VINVERR  DS    0H                                                               
*                                                                               
         ST    R2,FADDR            A(FIELD IN ERROR)                            
*                                                                               
         LA    RF,INVMAX           MAXIMUM ALLOWED INV NOS.                     
         SR    RF,R0                                                            
         LA    RF,1(RF)            NUMBER OF INV NO. IN ERROR                   
         STC   RF,FADDR            PASS ITEM IN ERROR NUMBER                    
*                                                                               
         MVI   ERROR,SUPPLIED      ERROR MESSAGE SUPPLIED                       
*                                                                               
         GOTO1 VMYCURS                                                          
*                                                                               
VINVX    DS    0H                                                               
*                                                                               
         DROP  R6                                                               
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - VOPT'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        VALIDATE OPTIONS                                          *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
VOPT     DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *     PRINT SHARES & H/P/T       *           
*                                  *                                *           
*                                  **********************************           
OP1      LA    R2,MASOP1H          PRINT SHARES & HPT OPTION (OPTIONAL)         
         MVI   ERROR,2             INVALID INPUT                                
         CLI   5(R2),0             DEFAULT NO                                   
         BE    OP2                                                              
         CLI   8(R2),C'N'                                                       
         BE    OP2                                                              
         CLI   8(R2),C'Y'                                                       
         JNE   ERREND                                                           
         OI    PRINTOPT,X'40'                                                   
*                                                                               
         CLI   DDS,C'Y'                                                         
         BE    OP2                                                              
*                                                                               
         TM    PRINTOPT,X'20'      FILTER ON DAYPART OR DAY OR INV#             
         BO    OP2                 --------------------------------             
*                                                                               
         TM    WHEN,X'10'          OVERNITE                                     
         BO    OP2                 --------                                     
         TM    WHEN,X'20'          SOON                                         
         BO    OP2                 ----                                         
*                                                                               
         L     RF,=A(BADSHARE)                                                  
         A     RF,RELO                                                          
         MVC   CONHEAD(L'BADSHARE),0(RF)                                        
         J     MYEND                                                            
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *     SUPPRESS TEXT OPTION       *           
*                                  *                                *           
*                                  **********************************           
OP2      LA    R2,MASOP2H          TEXT (OPTIONAL)                              
*                                                                               
         CLI   5(R2),1             MAX ONE OPTION                               
         BNH   OP210                                                            
*                                                                               
         L     RF,=A(OP2ERR)                                                    
         A     RF,RELO                                                          
         MVC   CONHEAD(L'OP2ERR),0(RF)    ONLY ONE OPTION ALLOWED               
*                                                                               
         J     MYEND                                                            
*                                                                               
OP210    DS    0H                                                               
         MVI   ERROR,2             INVALID INPUT                                
*                                                                               
         CLI   5(R2),0             DEFAULT NO                                   
         BE    *+8                                                              
         CLI   8(R2),C'N'                                                       
         BE    *+8                                                              
         CLI   8(R2),C'M'          SUPPRESS MARKET         TEXT                 
         BE    *+8                                                              
         CLI   8(R2),C'S'          SUPPRESS STATION/MARKET TEXT                 
         BE    OP2X                                                             
*                                                                               
         CLI   8(R2),C'Y'                                                       
         JNE   ERREND                                                           
*                                                                               
         OI    PRINTOPT,X'80'      SUPPRESS TEXT                                
*                                                                               
OP2X     DS    0H                                                               
*                                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *     PRINT ONLY FIRST BOOK FOUND*           
*                                  *                                *           
*                                  **********************************           
OP7      LA    R2,MASOP7H          PRINT DATA FOR ONLY FIRST BOOK FOUND         
         MVI   ERROR,2             INVALID INPUT                                
         CLI   5(R2),0             DEFAULT NO                                   
         BE    OP7X                                                             
         CLI   8(R2),C'N'                                                       
         BE    OP7X                                                             
         SPACE 1                                                                
OP770    CLI   8(R2),C'Y'                                                       
         JNE   ERREND                                                           
OP7X     DS    0H                                                               
         SPACE                                                                  
*                                  **********************************           
*                                  *     SUPPRESS PRINTING FOR      *           
*                                  *     DUPLICATE DAYPART          *           
*                                  **********************************           
OPT3     MVI   OPTFLAG,0          DEFAULT                                       
         LA    R2,MASOP3H         SUPRRESS PRINTING FOR                         
         CLI   5(R2),0            MORE THAN ONE DAYPART                         
         BE    OPT4                                                             
         CLI   8(R2),C'N'                                                       
         BE    OPT4                                                             
         MVI   OPTFLAG,1                                                        
         CLI   8(R2),C'Y'                                                       
         BE    OPT4                                                             
         MVI   OPTFLAG,2                                                        
         CLI   8(R2),C'P'          PRIMARY DEMO ONLY                            
         BE    OPT4                                                             
         JNE   ERREND                                                           
         SPACE                                                                  
*                                                                               
OPT4     DS    0H                                                               
*                                  **********************************           
*                                  *     ROUNDED DEMOS?             *           
*                                  **********************************           
OPT5     LA    R2,MASOP5H                                                       
         CLI   5(R2),0                                                          
         BE    OPT5X                                                            
         CLI   8(R2),C'N'                                                       
         BE    OPT5X                                                            
         CLI   8(R2),C'Y'                                                       
         JNE   ERREND                                                           
OPT5X    DS    0H                                                               
         SPACE                                                                  
OPT6     LA    R2,MASOP6H                                                       
         CLI   5(R2),0                                                          
         BE    OPT6X                                                            
         CLI   8(R2),C'N'                                                       
         BE    OPT6X                                                            
         CLI   8(R2),C'Y'                                                       
         JNE   ERREND                                                           
OPT6X    DS    0H                                                               
         SPACE                                                                  
*                                  **********************************           
*                                  *                                *           
*                                  *     PRINT INVENTORY NUMBER     *           
*                                  *                                *           
*                                  **********************************           
OP9      LA    R2,MASOP9H          PRINT INVENTORY NUMBER?                      
         MVI   ERROR,2             INVALID INPUT                                
         CLI   5(R2),0             DEFAULT NO                                   
         BE    OP9X                                                             
         CLI   8(R2),C'N'                                                       
         BE    OP9X                                                             
         SPACE 1                                                                
OP970    CLI   8(R2),C'Y'                                                       
         JNE   ERREND                                                           
OP9X     DS    0H                                                               
         SPACE                                                                  
*                                  **********************************           
*                                  *                                *           
*                                  *     EFFECTIVE START DATE       *           
*                                  *                                *           
*                                  **********************************           
ESDTV    LA    R2,MASESDTH          EFFECTIVE START DATE (OPTIONAL)             
*                                                                               
         CLI   5(R2),0                                                          
         BE    ESDTX                                                            
*                                                                               
*        CHECK IF RFP SYMBOLIC PARAMETER ENTERED                                
*                                                                               
         OC    ARFPBLK,ARFPBLK     SKIP IF RFP NOT BEING USED                   
         BZ    ESDTRFPN                                                         
*                                                                               
         L     R4,ARFPBLK          ESTABLISH RFP BLOCK                          
         USING RFPBLK,R4                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,RFPVNUMS       NUMBER OF VIABLE SYMBOLIC NAMES              
         BZ    ESDTRFPN            NONE                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   MASESDT(0),RFPVSYMB MATCH INPUT TO SYMBOLIC PARAMETER            
         BE    *+10                   EXPANDED FORMAT                           
         CLC   RFPVSYME(L'RFPVSYME-1),MASESDT MATCH INPUT TO SYMB PARM          
         BE    *+16                  ESCAPE SEQ IN CASE LEFT OVER               
*                                     FROM LAST TRANSACTION                     
         LA    R4,RFPVSYML(R4)     BUMP TO NEXT SYMBOLIC PARAMETER              
         BCT   R0,*-32                                                          
         B     ESDTRFPN            NO SYMBOLIC PARAMETER FOUND                  
*                                                                               
         CLC   =AL2(RE#RFPSD),RFPVSYME+1 MUST BE SDATE SYMBOLIC                 
         BE    ESDTV10                                                          
         L     RF,=A(INVSYMB)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'INVSYMB),0(RF)                                         
         J     MYEND                                                            
*                                                                               
ESDTV10  DS    0H                                                               
         XC    MASESDT,MASESDT     INIT START FIELD                             
         MVC   8(L'RFPVSYME,R2),RFPVSYME REPLACE KEYWORD WITH ESC SEQ           
*                                                                               
         LA    RF,L'MASESDT        MAX LENGTH FOR START DATE                    
         STC   RF,5(R2)            CHANGE LENGTH TO MAX INPUT FOR RFP           
         STC   RF,L'RFPVSYME-1+8(R2)   CHG LEN IN ESCAPE SEQ                    
*                                                                               
         B     ESDTX               VALID PERIOD                                 
*                                                                               
         DROP  R4                                                               
*                                                                               
ESDTRFPN DS    0H                                                               
*                                                                               
         GOTO1 VVALDATE            VALIDATE DATE ENTRY                          
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,ESDT)   ESDT=2 BYTE COMPRESSED           
*                                                                               
ESDTX    DS    0H                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *       EFFECTIVE END DATE       *           
*                                  *                                *           
*                                  **********************************           
EEDTV    DS    0H                   EFFECTIVE END DATE (OPTIONAL)               
*                                                                               
         LA    R2,MASEEDTH                                                      
*                                                                               
         MVC   EEDT,=X'FFFF'                                                    
*                                                                               
         CLI   5(R2),0                                                          
         BE    EEDTX                                                            
*                                                                               
*        CHECK IF RFP SYMBOLIC PARAMETER ENTERED                                
*                                                                               
         OC    ARFPBLK,ARFPBLK     SKIP IF RFP NOT BEING USED                   
         BZ    EEDTRFPN                                                         
*                                                                               
         L     R4,ARFPBLK          ESTABLISH RFP BLOCK                          
         USING RFPBLK,R4                                                        
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,1,RFPVNUMS       NUMBER OF VIABLE SYMBOLIC NAMES              
         BZ    EEDTRFPN            NONE                                         
*                                                                               
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   MASEEDT(0),RFPVSYMB MATCH INPUT TO SYMBOLIC PARAMETER            
         BE    *+10                   EXPANDED FORMAT                           
         CLC   RFPVSYME(L'RFPVSYME-1),MASEEDT MATCH INPUT TO SYMB PARM          
         BE    *+16                  ESCAPE SEQ IN CASE LEFT OVER               
*                                     FROM LAST TRANSACTION                     
         LA    R4,RFPVSYML(R4)     BUMP TO NEXT SYMBOLIC PARAMETER              
         BCT   R0,*-32                                                          
         B     EEDTRFPN            NO SYMBOLIC PARAMETER FOUND                  
*                                                                               
         CLC   =AL2(RE#RFPEN),RFPVSYME+1 MUST BE EDATE SYMBOLIC                 
         BE    EEDTV50                                                          
         L     RF,=A(INVSYMB)                                                   
         A     RF,RELO                                                          
         MVC   CONHEAD(L'INVSYMB),0(RF)      INVALID SYMBOLIC                   
         J     MYEND                                                            
*                                                                               
*                                                                               
EEDTV50  XC    MASEEDT,MASEEDT     INIT START FIELD                             
         MVC   8(L'RFPVSYME,R2),RFPVSYME REPLACE KEYWORD WITH ESC SEQ           
*                                                                               
         LA    RF,L'MASEEDT        MAX LENGTH FOR START DATE                    
         STC   RF,5(R2)            CHANGE LENGTH TO MAX INPUT FOR RFP           
         STC   RF,L'RFPVSYME-1+8(R2)   CHG LEN IN ESCAPE SEQ                    
*                                                                               
         B     EEDTX               VALID PERIOD                                 
*                                                                               
         DROP  R4                                                               
*                                                                               
EEDTRFPN DS    0H                                                               
*                                                                               
         GOTO1 VVALDATE                                                         
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(2,EEDT)   EEDT=2 BYTE COMPRESSED           
*                                                                               
         CLC   ESDT,EEDT           START CANNOT BE GREATER THAN END             
         JH    ERREND                                                           
*                                                                               
EEDTX    DS    0H                                                               
*                                  **********************************           
VRECX    DS    0H                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - PREP'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        PRINT REPORT                                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
PREP     NTR1  BASE=*,LABEL=*                                                   
         L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         L     R1,=A(HOOK)                                                      
         A     R1,RELO                                                          
         ST    R1,HEADHOOK                                                      
         A     R1,RELO                                                          
         L     R1,=V(UPOUT)                                                     
         A     R1,RELO                                                          
         ST    R1,AUPOUT                                                        
         L     R1,=A(SVCLST)                                                    
         A     R1,RELO                                                          
         ST    R1,ASVCLST                                                       
                                                                                
         CLC   STAMP,=CL8'T8190E'  STOP IF STORAGE NOT STAMPED                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SPACE 1                                                                
         MVI   MAXPLINS,X'2F'      47 PRINT LINES FIT ON A PAGE                 
         SPACE 1                                                                
         LH    R1,=Y(BUFF-SYSD)    GET START OF BUFFER ADDRESS                  
         LA    R1,SYSD(R1)                                                      
         ST    R1,SBUFF            SAVE A(BUFFER START)                         
*                                                                               
         ZIC   RE,MAXPLINS                                                      
         MH    RE,=H'132'                                                       
         LA    R1,0(RE,R1)                                                      
         ST    R1,XBUFF            SAVE ADDRESS OF END OF 1 FULL PAGE           
         SPACE 1                                                                
         GOTO1 =A(DHED),DMCB,(RC),RR=RELO   CALC DEMO HEADS ONCE                
*                                                                               
         L     RE,SBUFF            START  OF BUFFER                             
         L     RF,=AL4(L'BUFF)     LENGTH OF BUFFER                             
*                                                                               
         XCEF  (RE),(RF)           CLEAR BUFFER AREA                            
*                                                                               
         MVI   RCSUBPRG,0                                                       
         L     R2,VADUMMY                                                       
         ST    R2,ASTACK                                                        
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,XTODAY)                                
         SPACE 4                                                                
*              DEAL WITH MARKET AND STATION FACTS                               
*              ----------------------------------                               
*                                                                               
         TM    PRINTOPT,X'80'      SUPPRESS TEXT                                
         BO    PMKTSTAX                                                         
*                                                                               
         CLI   MASOP2,C'S'         SKIP IF SUPPRESSING STA/MKT TEXT             
         BE    PMKTSTAX                                                         
*                                                                               
         CLI   MASOP2,C'M'         SKIP IF SUPPRESSING MARKET TEXT              
         BE    PMKTSTA1                                                         
*                                                                               
         MVI   OPTION,C'M'                                                      
         BRAS  RE,MAS300                                                        
*                                                                               
PMKTSTA1 DS    0H                                                               
*                                                                               
         MVI   OPTION,C'S'                                                      
         BRAS  RE,MAS300                                                        
*                                                                               
PMKTSTAX DS    0H                                                               
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - STKDPT'                 
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        LOOP THROUGH DAYPARTS AND PRINT REPORT FOR EACH           *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
STKDPT   DS    0H                                                               
*                                                                               
         LA    R2,DPLIST           POINT TO DAYPART LIST                        
         SR    R3,R3                                                            
         IC    R3,NOREQDPT         NUMBER OF DAYPARTS                           
*                                                                               
STKDPTLP DS    0H                                                               
*                                                                               
         CLI   0(R2),0             CHECK FOR END OF DAYPARTS                    
         BE    STKDPTDN                                                         
*                                                                               
         MVC   SVDPT,0(R2)         ACTIVATE NEXT DAYPART                        
*                                                                               
         MVI   FORCEHED,C'Y'       FORCE NEW PAGE                               
         MVC   PAGE,=H'1'          START RENUMBERING PAGES                      
*                                                                               
         BAS   RE,STKBLD           BUILD STACK OF D/A FOR DAYPART               
*                                                                               
STKDPTCN DS    0H                                                               
*                                                                               
         LA    R2,1(R2)            POINT TO NEXT DAYPART IN LIST                
         BCT   R3,STKDPTLP                                                      
*                                                                               
STKDPTDN DS    0H                                                               
*                                                                               
PREPX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - STKDPT'                 
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        BUILD STACK OF DISK ADDRESSES USING DAYPART PASSIVE PTR   *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
STKBLD   NTR1  BASE=*,LABEL=*      BUILD A STACK OF D/A                         
*                                                                               
         L     R5,ASTACK           POINT TO START OF D/A STACK                  
         SR    R6,R6               INIT STACK ITEM COUNTER                      
*                                                                               
         LA    R4,KEY              ESTABLISH DAYPART PASSIVE PTR                
         USING RIDPKEY,R4                                                       
         XC    KEY,KEY                                                          
*                                                                               
         MVI   RIDPKTYP,RIDPKTYQ   SET PASSIVE PTR ID                           
         MVC   RIDPKREP,REPPAR     USE PARENT REP                               
         MVC   RIDPKSTA,ACTSTAT    SET ACTIVE STATION                           
         MVC   RIDPKDPT,0(R2)      SET CURRENT DAYPART                          
*                                                                               
         GOTO1 HIGH                READ FIRST POINTER                           
*                                                                               
MASSTKLP DS    0H                                                               
*                                                                               
         CLC   RIDPKEY(RIDPKDAY-RIDPKEY),KEYSAVE DONE AT STA CHANGE             
         BNE   MASSTKDN                                                         
*                                                                               
*        FILTER ON INVENTORY NUMBER IF NEEDED                                   
*                                                                               
         LA    R3,SVINV            POINT TO INVENTORY NO. FILTERS               
*                                                                               
         CLC   0(L'RIDPKINV,R3),SPACES  SKIP FILTER IF NONE                     
         BNH   MASINVOK                                                         
*                                                                               
MASINVLP DS    0H                                                               
*                                                                               
         CLC   0(L'RIDPKINV,R3),SPACES  CHECK FOR EOL                           
         BNH   MASINVDN                                                         
*                                                                               
         CLC   RIDPKINV,0(R3)      INV NO MUST LIE IN RANGE                     
         BL    MASINVCN                                                         
         CLC   RIDPKINV,L'RIDPKINV(R3)                                          
         BH    MASINVCN                                                         
*                                                                               
         B     MASINVOK                                                         
*                                                                               
MASINVCN DS    0H                                                               
*                                                                               
         LA    R3,2*L'RIDPKINV(R3)  BUMP TO NEXT INV NO IN FILTER               
         B     MASINVLP                                                         
*                                                                               
MASINVOK DS    0H                                                               
*                                                                               
*        INVENTORY NUMBER PASSED FILTER                                         
*                                                                               
         MVC   0(4,R5),KEY+28      SAVE DISK ADDRESS IN STACK                   
*                                                                               
         LA    R5,4(R5)            BUMP STACK POINTER                           
         LA    R6,1(R6)            BUMP ITEM COUNTER                            
*                                                                               
MASINVDN DS    0H                                                               
*                                                                               
MASSTKCN DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 READ NEXT PASSIVE                            
*                                                                               
         B     MASSTKLP                                                         
*                                                                               
MASSTKDN DS    0H                                                               
*                                                                               
         LTR   R6,R6               DONE IF STACK OF DISK ADDR IS EMPTY          
         BZ    MASX                                                             
*                                                                               
         STM   R5,R6,SAVESTAK      SAVE STACK POINTERS                          
*                                                                               
         DROP  R4                                                               
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS  -MAS70'                  
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS                          *            
*                                                                  *            
*        LOOP THROUGH DISK ADDRESS STACK AND PRINT DATA FOR EACH   *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
*                                                                               
MAS70    L     R5,ASTACK                                                        
*                                                                               
MAS80    DS    0H                                                               
*                                                                               
         MVC   KEY+28(4),0(R5)     PUT NEXT DISK ADDR IN KEY                    
*                                                                               
         STM   R5,R6,SAVESTAK      SAVE STACK POINTERS                          
*                                                                               
         OI    DMINBTS,X'08'       SET PASS DELETED RECORDS                     
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
*                                                                               
         MVC   KEY(27),IO                                                       
*                                                                               
         TM    IO+29,X'80'         TEST RECORD IS DELETED                       
         BO    MAS165                                                           
*                                                                               
         USING RINVKEY,R4                                                       
*                                                                               
         OC    INVNO,INVNO         INVENTORY FILTER                             
         BZ    MAS90                                                            
*                                                                               
         CLC   RINVKINV,INVNO                                                   
         BNE   MAS165                                                           
*                                                                               
*        FILTER ON DAY                                                          
*                                                                               
MAS90    CLI   DAYOPT,X'FF'        SKIP IF NO DAY FILTER                        
         BE    MAS100                                                           
*                                                                               
         MVI   ELCODE,X'02'        FIND DAY/TIME ELEMENTS                       
         LA    R6,IO                                                            
         BRAS  RE,GETEL                                                         
         BNE   MAS165              NO ELEMENT FOUND                             
*                                                                               
MASDTMLP DS    0H                                                               
*                                                                               
         USING RIDTELEM,R6                                                      
*                                                                               
         CLC   DAYOPT,RIDTDAY      MATCH ON DAY                                 
         BE    MASDTMFD                                                         
*                                                                               
MASDTMCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BE    MASDTMLP                                                         
         B     MAS165              NO MATCH                                     
*                                                                               
MASDTMFD DS    0H                                                               
*                                                                               
         DROP  R4                                                               
*                                                                               
MAS100   MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BRAS  RE,GETEL                                                         
         USING RINVPEL,R6                                                       
*                                                                               
         CLI   OPTFLAG,0          OPTION-SUPRESS PRNT IN MULT DPTS              
         BE    MAS102                                                           
         ZIC   R1,NOREQDPT        # OF REQ. DAYPARTS                            
         CH    R1,=H'1'           NO CHANCE OF DUPLICATE IF ONLY 1 DPT          
         BE    MAS102                                                           
*                                                                               
         BRAS  RE,TSTDPT                                                        
         CLI   SKIPREC,0                                                        
         BNE   MAS165              DON'T PRINT IT IN THIS DAYPART               
*                                                                               
MAS102   TM    PRINTOPT,X'08'      ANY FILTER FILTERS                           
         BZ    MAS125                                                           
         SPACE 1                                                                
         ZIC   RE,NUMFILT                                                       
         LA    R5,MASFTR                                                        
         SPACE 1                                                                
MAS105   LA    R1,RINVPFLT                                                      
         LA    R0,6                                                             
         SPACE 1                                                                
MAS110   CLI   0(R5),C'A'          IF ANY FILTER SPECIFIED                      
         BL    MAS123                                                           
MAS115   CLC   0(1,R5),0(R1)       MATCHES ANY FILTER ON RECORD                 
         BE    MAS125              THEN WE WANT IT                              
         SPACE 1                                                                
MAS120   LA    R1,1(R1)                                                         
         BCT   R0,MAS115                                                        
MAS123   LA    R5,1(R5)                                                         
         BCT   RE,MAS105                                                        
         B     MAS165              NOTHING MATCHES-DON'T WANT IT                
         DROP  R6                                                               
         SPACE 2                                                                
MAS125   OC    ADT,ADT             WAS ACTIVITY DATE SPECIFIED                  
         BZ    MAS130                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'EF'                                                     
         BRAS  RE,GETEL            FIND ACTIVITY ELEMENT                        
         BNE   MAS165                                                           
         USING RINVAEL,R6                                                       
         CLC   RINVALST,ADT        AND SELECT CHANGES SINCE DATE                
         BL    MAS165                                                           
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
MAS130   LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         USING RINVPEL,R6                                                       
         CLC   ESDT(4),=X'0000FFFF' FILTER ON EFFECTIVE DATE(S)                 
         BE    MAS136                                                           
         CLC   RINVPEFF+0(2),EEDT  IGNORE ANY                                   
         BH    MAS165              STARTING AFTER SELECTED END DATE             
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    MAS140                                                           
         CLC   RINVPEFF+2(2),ESDT  IGNORE ANY                                   
         BL    MAS165              ENDING BEFORE SELECTED START DATE            
         B     MAS140                                                           
*                                                                               
MAS136   OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    MAS140                                                           
         CLC   RINVPEFF+2(2),XTODAY SEE IF IT ENDS BEFORE TODAY                 
         BL    MAS165                                                           
*                                                                               
MAS140   DS    0H                                                               
*&&DO                                                                           
         GOTO1 =A(BLDRATE),RR=RELO  BUILD TABLE OF RATES                        
*&&                                                                             
         GOTO1 =A(HDRNEW),DMCB,(RC),RR=RELO   USE NEW ROUTINE                   
*                                                                               
         TM    PRINTOPT,X'10'                RANGE OF BOOKS                     
         BO    MAS150                                                           
         BAS   RE,MAS170           GO AND LOOK FOR BOOKS                        
         B     *+8                                                              
MAS150   BAS   RE,RANGE            GO LOOK FOR RANGE OF BOOKS                   
         TM    PRINTOPT,X'80'      SUPPRESS TEXT                                
         BO    MAS160                                                           
*                                                                               
         MVI   OPTION,C'I'         INDICATE INVENTORY TEXT                      
         BRAS  RE,MAS300                                                        
*                                                                               
MAS160   EQU   *                                                                
         MVI   CONTSW,C'N'                                                      
                                                                                
MAS165   LM    R5,R6,SAVESTAK                                                   
         LA    R5,4(R5)                                                         
         BCT   R6,MAS80                                                         
                                                                                
MASX     XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                               
*              CONTROL READING OF BOOKS - RANGE                                 
*              ---------------------------------                                
*                                                                               
*                                                                               
* 1. PRINT HEADER                                                               
* 2. CREATE LIST (RNGLST) OF BOOKS(2), SOURCE(1), AND DISK ADDRESS(4)           
*         OF ALL INVENTORY THAT FITS RANGE (MAX 30 BOOKS)                       
* 3. SORT LIST IN BOOK, SOURCE ORDER (FILE IS IN SOURCE, BOOK ORDER)            
* 4. THEN GET BOOKS IN NON-RANGE CODE USING RNGLIST DISK ADDRESSES              
*                                                                               
**********************************************************************          
                                                                                
RANGE    NTR1                                                                   
         USING RINVKEY,R4                                                       
         MVI   HDRSW,C'Y'          PUT HDR LINE TO BUFF 1ST-TIME-THRU           
*                                                                               
         LA    R4,KEY                                                           
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         XC    RNGLST,RNGLST       RNGLST=SRC(1),BK(2),BKTP(1),D/A(4)           
         XC    RNGLST2,RNGLST2                                                  
*                                                                               
         LH    RF,=Y(RNGLSTND-SYSD)                                             
         LA    RF,SYSD(RF)                                                      
         MVI   0(RF),X'FF'         SET DELIMITER FOR EOT                        
*                                                                               
         LA    R3,RNGLST                                                        
         SR    R2,R2                                                            
*                                                                               
         GOTO1 HIGH                POINT TO INVENTORY HEADER                    
*                                                                               
         GOTO1 SEQ                 READ IN FIRST TRACK                          
*                                                                               
RANGLOOP DS    0H                                                               
*                                                                               
         CLC   RINVKEY(RINVKRTP-RINVKEY),KEYSAVE                                
         BNE   RANGDONE            DONE ON CHANGE IN INV HEADER                 
*                                                                               
         CLI   RINVKRSR,X'FF'      DONE IF RATIONALE RECORDS REACHED            
         BE    RANGDONE                                                         
         CLI   RINVKRSR,C'Z'       DONE IF RDETAIL RECORDS REACHED              
         BE    RANGDONE                                                         
*                                                                               
*        TEST RATING SERVICE, BOOKTYPE                                          
*                                                                               
         CLC   SVSOURCE,RINVKRSR   MUST BE SAME RATING SERVICE                  
         BNE   RANGCONT                                                         
         CLC   BOOKS+3(1),RINVKBTP  MUST BE SAME BOOKTYPE                       
         BNE   RANGCONT                                                         
*                                                                               
         CLC   BOOKS+1(2),RINVKBK          ************                         
         BH    RANGCONT                    *  WITHIN  *                         
         CLC   BOOKS+5(2),RINVKBK          *  RANGE?  *                         
         BL    RANGCONT                    ************                         
*                                                                               
*        SAVE BOOK, KSRC & DISK ADDRESS                                         
*                                                                               
         MVC   0(1,R3),RINVKQLF                                                 
         MVC   1(2,R3),RINVKBK                                                  
         MVC   3(1,R3),RINVKBTP                                                 
         MVC   4(1,R3),RINVKRSR                                                 
         MVC   5(4,R3),KEY+28      DISK ADDRESS                                 
*                                                                               
         LA    R3,9(R3)            BUMP LIST POINTER                            
         LA    R2,1(R2)            BUMP LIST ENTRY COUNTER                      
*                                                                               
         CH    R2,=H'56'           TABLE FULL?                                  
         BE    RANGDONE            YES - DON'T ALLOW MORE INPUT                 
*                                                                               
RANGCONT DS    0H                                                               
*                                                                               
         GOTO1 SEQ                 NEXT DEMO TRACK                              
         B     RANGLOOP                                                         
*                                                                               
RANGDONE DS    0H                                                               
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE HEADER PART OF KEY                   
*                                                                               
         STC   R2,BYTE             SAVE NUMBER OF DEMO TRACKS FOUND             
*                                                                               
         CH    R2,=H'1'                                                         
         BE    RNG50               SKIP XSORT FOR 1 BOOK                        
         CH    R2,=H'56'                                                        
         BNH   *+6                                                              
*                                                                               
*  NOTE:  THIS DUMP CAN NO LONGER HAPPEN....                                    
*                                                                               
         DC    H'0'                TOO MANY BOOKS IN RANGE                      
         GOTO1 XSORT,DMCB,RNGLST,(R2),9,5,0                                     
*                                                                               
RNG50    LA    R5,RNGLST                                                        
         ZIC   RE,BYTE                                                          
         MH    RE,=H'9'                                                         
         LA    R5,0(RE,R5)                                                      
         MVI   0(R5),X'FF'         MARK END OF TABLE                            
         LA    R5,RNGLST                                                        
         B     MAS200                                                           
         SPACE 1                                                                
RNGX     B     MAS240              NO BOOKS TO PRINT                            
         EJECT                                                                  
*              CONTROL READING OF BOOKS - NON RANGE                             
*              ------------------------------------                             
         SPACE 2                                                                
MAS170   NTR1                                                                   
         USING RINVKEY,R4                                                       
         MVI   HDRSW,C'Y'          PUT HDR LINE TO BUFF 1ST-TIME-THRU           
         MVI   RATESW,0            NO RATES PRINTED YET                         
         LA    R2,BOOKS                                                         
         ZIC   R3,NUMBOOK                                                       
         LA    R4,KEY                                                           
         MVC   KEYSAVE,KEY                                                      
*                                                                               
GTBLOOP  DS    0H                                                               
*                                                                               
         LA    R4,KEY                                                           
*                                                                               
         MVC   KEY,KEYSAVE         RESTORE HEADER KEY                           
*                                                                               
         MVC   RINVKRSR,SVSOURCE   SET RATING SERVICE                           
         MVC   RINVKQLF,0(R2)      SET BOOKVAL BITS                             
         MVC   RINVKBK,1(R2)       SET BOOK                                     
         MVC   RINVKBTP,3(R2)      SET BOOKTYPE                                 
*                                                                               
         GOTO1 HIGH                READ DIRECTORY POINTER                       
*                                                                               
*SMY*    CLC   RINVKEY(RINVKSRC-RINVKEY),KEYSAVE  DONE ON INV # CHANGE          
*SMY*    BNE   GTBCONT                                                          
*                                                                               
*SMY*    CLC   KEY+24(3),KEYSAVE+24     SAME SOURCE AND BOOK                    
*SMY*    BE    MAS200                                                           
*                                                                               
         CLC   KEY(L'RINVKEY),KEYSAVE   ENTIRE KEY MUST MATCH                   
         BE    MAS200                                                           
*                                                                               
MAS199   DS    0H                                                               
*                                                                               
GTBCONT  LA    R2,4(R2)            R2 POINTS TO NEXT BOOK                       
         BCT   R3,GTBLOOP                                                       
*                                                                               
         B     MAS240              ALL THROUGH                                  
*                                                                               
         EJECT                                                                  
*     NOW GETREC FOR BOTH RANGE/NON-RANGE BOOKS AND THEN PRINT                  
*     --------------------------------------------------------                  
         SPACE 2                                                                
MAS200   CLI   HDRSW,C'Y'                                                       
         BNE   MAS201                                                           
***           PUT HDR LINE TO BUFF 1ST-TIME-THRU                                
         BRAS  RE,BUFFADD                                                       
         BRAS  RE,BUFFFULL                                                      
         MVI   HDRSW,0                                                          
*                                                                               
MAS201   TM    PRINTOPT,X'10'      RANGE OF BOOKS                               
         BNO   MAS202                                                           
*                                                                               
         CLI   0(R5),X'FF'         END OF LIST                                  
         BE    MAS240                                                           
*                                                                               
*                       MAKE IT LOOK LIKE WE READ FILE TO GET RECORD            
         MVC   KEY(RINVKRSR-RINVKEY),KEYSAVE                                    
         MVC   RINVKRSR,4(R5)      RATING SERVICE                               
         MVC   RINVKBK,1(R5)       BOOK                                         
         MVC   RINVKQLF,0(R5)      QUALIFIER(BITS)                              
         MVC   RINVKBTP,3(R5)      BOOKTYPE                                     
         MVC   KEY+28(4),5(R5)     BUT REALLY USE SAVED DISK ADDRESS            
*                                                                               
MAS202   DS    0H                                                               
*                                                                               
         OI    DMINBTS,X'08'       SET PASS DELETED RECORDS                     
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
*                                                                               
         TM    IO+29,X'80'         TEST RECORD IS DELETED                       
         BO    MAS231                                                           
*                                                                               
*        FIND RATING SERVICE, BOOKTYPE                                          
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         LA    RF,IO               POINT TO FOUND RECORD                        
         MVC   GSIRSVC,RINVKRSR-RINVKEY(RF)    SET RATING SOURCE                
*                                                                               
*              ** BELOW ADDED FOR "NEW" GETKSRC **                              
         MVC   GSIQLF,RINVKQLF-RINVKEY(RF)     SET QUALIFIER                    
         MVC   GSIBKTYP,RINVKBTP-RINVKEY(RF)   SET BOOK TYPE                    
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',GSRCIN),GSRCOUT,ACOMFACS                     
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BE    *+6                 SHOULD NOT HAPPEN                            
         DC    H'0'                                                             
*                                                                               
         BAS   RE,DATA             FORMAT DEMOS                                 
*                                                                               
         GOTO1 =A(BOOKSQ),DMCB,(RC),ASVCLST,RR=RELO  FORMAT BOOK NAME           
         BRAS  RE,BUFFADD         POINT TO NEXT LINE                            
         BRAS  RE,BUFFFULL                                                      
         XC    BLOCK1,BLOCK1       USE BLOCK1 TO SAVE SHR/LVL VALUES            
         TM    PRINTOPT,X'40'      SHRS/LVLS ARE SUPPRESSED OPTIONALLY          
         BZ    MAS210                                                           
         CLI   MULTSW,C'Y'         OR ON COMBOS                                 
         BE    MAS210                                                           
         CLC   SAVECODE,=C'PA'     IF PROGRAM CODE IS PA, PRINT SHR/LVL         
         BE    MAS209                                                           
*                                  BUT SUPPRESS IF                              
         CLC   SAVECODE,=C'ES'     PROGRAM CODE IS ES                           
         BNE   MAS207                                                           
         TM    RMPPROF+RESMASTB,RESMHPTA                                        
         BZ    MAS209                                                           
         B     MAS210              (AND PROFILE BIT #18 ON)                     
*                                                                               
MAS207   TM    PRINTOPT,X'02'      OR UPGRADES                                  
         BNO   MAS208                                                           
         TM    RMPPROF+RESMASTB,RESMHPTA                                        
         BZ    MAS209                                                           
         B     MAS210              (AND PROFILE BIT #18 ON)                     
*                                                                               
MAS208   CLC   SAVECODE,=C'PE'     OR PE                                        
         BE    MAS210                                                           
         CLC   SAVECODE,=C'PJ'     OR PJ                                        
         BE    MAS210                                                           
MAS209   BAS   RE,SHRLVL           FORMAT (& PRINT) SHARES & LEVELS             
*                                                                               
MAS210   DS    0H                                                               
*                                                                               
         CLI   MASOP7,C'Y'         DONE IF ONLY FIRST BOOK TO PRINT             
         BNE   MAS215                                                           
*                                                                               
         TM    RATESW,X'80'        SKIP IF RATES ALREADY PRINTED                
         BO    MAS220                                                           
*                                                                               
MAS215   DS    0H                                                               
*                                                                               
         GOTO1 =A(PRTRATE),DMCB,ACTDEMOS,RR=RELO PRINT RATES                    
*                                                                               
         OI    RATESW,X'80'        INDICATE RATES FOR A BOOK PRINTED            
*                                                                               
MAS220   DS    0H                                                               
*                                                                               
         CLI   MASOP8,C'Y'         SUPPRESS FOOTNOTES IF ASKED                  
         BE    MAS235                                                           
*                                                                               
         MVI   OPTION,C'A'         PRINT ANY AUTO FOOTNOTES                     
         BRAS  RE,RAT                                                           
*                                                                               
MAS230   TM    PRINTOPT,X'02'      ANY UPGRADE                                  
         BZ    *+8                                                              
         BRAS  RE,UPPRNT           YES, PRINT UPGRADE EXPRESSION                
*                                                                               
MAS235   DS    0H                                                               
*                                                                               
         MVI   SPACING,1                                                        
         BRAS  RE,BUFFADD         SPACE AFTER EVERY GROUP                       
         BRAS  RE,BUFFFULL                                                      
*                                                                               
MAS231   DS    0H                                                               
*                                                                               
         TM    PRINTOPT,X'10'      RANGE OF BOOKS                               
         BNO   MAS199              NEXT BOOK-NON RANGE                          
*                                                                               
         LA    R5,9(R5)                                                         
         B     MAS200              NEXT BOOK-RANGE                              
*                                                                               
MAS240   CLI   HDRSW,C'Y'          HAS ANY DATA BEEN PRINTED                    
         BNE   MAS244                                                           
         BRAS  RE,BUFFADD          NO, SO POINT PAST HEADER LINE                
         BRAS  RE,BUFFFULL                                                      
         BRAS  RE,BUFFADD          THEN LEAVE SPACING LINE                      
         BRAS  RE,BUFFFULL                                                      
         SPACE 1                                                                
MAS244   L     RE,SBUFF                                                         
         LA    RF,15               COULD BE 15 HEADLINES                        
MAS247   OC    0(40,RE),0(RE)                                                   
         BZ    MAS248                                                           
         LA    RE,132(RE)                                                       
         BCT   RF,MAS247                                                        
         SPACE 1                                                                
MAS248   L     RF,ABUFF                                                         
         CR    RE,RF                                                            
         BH    *+6                                                              
         LR    RE,RF                                                            
         LA    RE,132(RE)          SPACING AFTER EVERY GROUP                    
         ST    RE,ABUFF                                                         
         BRAS  RE,BUFFFULL                                                      
         SPACE 1                                                                
         BRAS  RE,PRNTALL                                                       
         XIT1                                                                   
         EJECT                                                                  
*                                                                               
*              ROUTINES FOR DATA RECORDS                                        
*              -------------------------                                        
         SPACE 2                                                                
DATA     NTR1                                                                   
         LA    R6,IO               CHECK FOR CODE                               
         MVI   MULTSW,C'N'                                                      
         MVI   ELCODE,X'CD'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DAT30                                                            
         USING RINVCEL,R6                                                       
*                                                                               
         MVC   SAVECODE,RINVCODE   SAVE CODE FOR LATER PRINTING                 
*                                                                               
         TM    RINVCTYP,X'80'                                                   
         BNO   DAT30                                                            
         MVI   MULTSW,C'Y'                                                      
         SPACE 2                                                                
DAT30    NI    PRINTOPT,X'FD'      TURN OFF UPGRADE INDICATOR                   
         LA    R6,IO                                                            
         MVI   ELCODE,X'05'                                                     
         BRAS  RE,GETEL                                                         
         BNE   *+8                                                              
         OI    PRINTOPT,X'02'      INDICATE UPGRADE EXISTS                      
         LA    R5,ACTDEMOS         POINT AT OUTPUT FOR DEMO VALUES              
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO                                      
         XIT1                                                                   
         EJECT                                                                  
*              SHARES AND LEVELS -  FORMATTING AND PRINTING                     
*              --------------------------------------------                     
         SPACE 2                                                                
SHRLVL   NTR1                                                                   
         MVI   LINSW,C'L'          *** LEVELS ***                               
         MVC   SVDEMLST,DEMLST     SAVE DEMLST                                  
         BRAS  RE,EFFDEMS          DEVELOPE DEMO TYPES                          
         BRAS  RE,BUFFADD         LEVELS PRINT AFTER SHARES                     
         LA    R5,BLOCK1+100                                                    
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO    FORMAT THE DEMOS                  
         MVC   DEMLST,SVDEMLST                RESTORE DEMLST                    
         L     RE,ABUFF                                                         
         LA    RF,6(RE)                                                         
         MVC   0(5,RF),=C'H/P/T'                                                
         SH    RE,=H'132'          BACK UP A LINE TO PRINT SHARES               
         ST    RE,ABUFF                                                         
*                                                                               
         MVI   LINSW,C'S'          *** SHARES ***                               
         CLC   SAVECODE,=C'PA'                                                  
         BNE   SL45                                                             
         SPACE 1                                                                
SL40     BRAS  RE,RECALC                                                        
         OI    PRINTOPT,X'01'                  INDICATE RECALC DEMOS            
         LA    R5,BLOCK1                       POINT TO SHARES                  
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO     FORMAT DEMOS                     
         NI    PRINTOPT,X'FE'                                                   
         B     SL50                                                             
         SPACE 1                                                                
SL45     MVC   SVDEMLST,DEMLST                 SAVE DEMLST                      
         BRAS  RE,EFFDEMS                      DEVELOPE DEMO TYPES              
         LA    R5,BLOCK1                                                        
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO     FORMAT THE DEMOS                 
         MVC   DEMLST,SVDEMLST                 RESTORE DEMLST                   
SL50     BRAS  RE,PERCENT                      PERCENT SIGNS FOR SHARES         
         L     RE,ABUFF                                                         
         LA    RF,6(RE)                                                         
         MVC   0(5,RF),=C'SHARE'                                                
         LA    RE,132(RE)          SHARE LINE                                   
         LA    RE,132(RE)          LEVELS LINE                                  
         ST    RE,ABUFF                                                         
         BRAS  RE,BUFFFULL                                                      
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*                                                                               
* COMMON ROUTINES                                                               
*                                                                               
**********************************************************************          
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VERRXIT                                                          
         SPACE 2                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         XIT1                                                                   
*                                                                               
BUFFADD  L     R1,ABUFF                                                         
         LA    R1,132(R1)                                                       
         ST    R1,ABUFF                                                         
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*                        CONSTANTS                                              
*                                                                               
**********************************************************************          
         SPACE 2                                                                
*                                                                               
*                                                                               
*  MY OWN ERROR MESSAGES                                                        
*                                                                               
INVSYMB  DC    C'* ERROR * INVALID SYMBOLIC'                                    
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 6'                         
MANYDEM  DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 17'                        
MANYFLT  DC    C'* ERROR * TOO MANY FILTERS - LIMIT IS 6'                       
BADBK1   DC    C'* ERROR * BOOK CAN NOT HAVE PREFIX'                            
BADBK2   DC    C'* ERROR * BOOKS MUST BE THE SAME SOURCE'                       
BADBK3   DC    C'* ERROR * END BOOK LESS THAN START BOOK'                       
BADMENU  DC    C'* ERROR * MENU IS 2 A/N CHARACTERS'                            
DPMENUER DC    C'* ERROR * MENU NOT FOUND'                                      
DPINVER  DC    C'* ERROR * DAYPART NOT FOUND'                                   
BADSHARE DC   C'* ERROR * SHR-H/P/T MUST FILTER ON DYPT OR DAY OR INV#'         
OP2ERR DC C'ERROR - SELECT ONLY ONE OPTION - NOTE: ''S'' IMPLIES ''M'''         
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - BLDRATE'                
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS - BUILD RATE TABLE       *            
*                                                                  *            
*        BUILD TABLE OF RATES VALID FOR REPORTS                    *            
*              RATE MUST BE FOR A QUARTER AND SPOT LENGTH REQUESTED*            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*NTRY    IO    CONTAINS INVENTORY RECORD                           *            
*        SVQTR      TABLE OF REQUESTED YEARS/QUARTERS              *            
*        SVSLN      TABLE OF REQUESTED SPOT LENGTHS                *            
*        SVRTCD     RATE CODE                                      *            
*                                                                  *            
*EXIT    AIO3       CONTAINS TABLE OF VALID RATES (1 YR AT A TIME) *            
*                   CL8(RATE CODE)                                 *            
*                   XL1(YEAR)                                      *            
*                   XL2(SPOT LENGTH)                               *            
*                   52XL8(QTR/WEEK/COST)                           *            
*        SVRATEN    XL1 - NUMBER OF ENTRIES IN TABLE               *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
         DS    0D                                                               
BLDRATE  NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         ZIC   R3,SVSLNN           # OF ENTRIES                                 
         LA    RF,SVSLN            SPOT LENGTH TABLE                            
         GOTO1 XSORT,DMCB,(0,0(RF)),(R3),2,2,0                                  
*                                                                               
         ZIC   R3,SVQTRN           # OF ENTRIES                                 
         LA    RF,SVQTR            YEAR/QTR TABLE                               
         GOTO1 XSORT,DMCB,(0,0(RF)),(R3),2,2,0                                  
*                                                                               
         L     R3,AIO3                                                          
         XCEF  (R3),2000                                                        
         USING RATENTD,R3          ESTABLISH SAVED RATES                        
*                                                                               
         MVI   SVRATEN,0           INIT NUMBER OF ENTRIES                       
*                                                                               
         MVC   SAVEKEY,KEY                                                      
*                                  GET HEADER AGAIN                             
         XC    KEY+RINVKRTP-RINVKEY(6),KEY+RINVKRTP-RINVKEY                     
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   HDRKEY,KEY                                                       
*                                                                               
         LA    RF,SVQTR                                                         
         ST    RF,ASVQTR           POSITION IN YEAR/QTR TABLE                   
*                                                                               
BLDR10   LA    R6,IO               POINT TO INVENTORY RECORD                    
         MVI   ELCODE,X'06'        SET TO FIND RATE ELEMENTS                    
         BRAS  RE,GETEL            SEARCH FOR A RATE ELEMENT                    
*                                                                               
BRTELOOP DS    0H                                                               
         BNE   BRTENXTS            END OF RECORD - TRY NEXT SPOT                
         USING RIMAELEM,R6         ESTABLISH AVAIL RATE ELM                     
*                                                                               
         CLC   RIMAREP,AGENCY      MATCH ON REQUESTING REP                      
         BNE   BRTECONT                                                         
*                                                                               
         CLC   RIMACDE,SVRTCD      MATCH ON RATE CODE                           
         BNE   BRTECONT                                                         
*                                                                               
*        CHECK IF ELEMENT FOR A WANTED SPOT LENGTH                              
*                                                                               
         L     RF,ASVSLN           POS. IN SPOT LENGTH TABLE                    
         OC    0(L'SVSLN,RF),0(RF) ANY SPOTS HERE?                              
         BE    BRTEDONE                                                         
*                                                                               
         CLC   RIMALNTH,0(RF)      SAME LENGTH?                                 
         BNE   BRTECONT            NOT NEEDED                                   
*                                                                               
         MVC   SVEQUNUM,RIMANUM    EQUATE NUMBER FOR 'Z' REC LOOKUP             
*                                                                               
BLDR50   XC    KEY,KEY             BUILD RDETAIL RECORD KEY                     
         MVC   KEY(RINVKRTP-RINVKEY),HDRKEY                                     
         MVI   KEY+RINVKRTP-RINVKEY,C'Z'     RDETAIL TYPE RECORD                
*                              SVEQUNUM IS UNIQUE EQUATE NUM FROM X'06'         
         MVC   KEY+RINVKNUM-RINVKEY(1),SVEQUNUM                                 
*                                                                               
         L     RF,ASVQTR           YEAR/QTR TABLE                               
         MVC   KEY+RINVKYR-RINVKEY(1),0(RF)   YEAR TO LOOK FOR                  
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE     FOUND IT?                                    
         BNE   BRTENXTY            TRY NEXT YEAR                                
*                                                                               
         GOTO1 GETREC                                                           
*                                                                               
*    START BUILDING RATE DATA TABLE                                             
*                                                                               
         MVC   RATRTCD,SVRTCD      RATE CODE                                    
         MVC   RATYEAR,KEY+RINVKYR-RINVKEY    YEAR                              
*                                                                               
         L     RF,ASVSLN                                                        
         MVC   RATSLN,0(RF)        SPOT LENGTH                                  
*                                                                               
         LA    R4,RATQWRTE         QTR/WEEK/COST                                
         L     R5,ASVQTR           POS. IN YEAR/QTR TABLE                       
         XC    SVCOST,SVCOST                                                    
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'        GET WEEKLY RATES                             
         BRAS  RE,GETEL                                                         
*                                                                               
BLDR100  DS    0H                  FILL IN COSTS FOR ALL QUARTERS               
         BNE   BRTEQTRD            FINISHED WITH QUARTERS                       
         USING RIAVL,R6                                                         
*                                                                               
BLDR110  OC    0(2,R5),0(R5)       ANY YEAR/QTR ENTRY?                          
         BZ    BRTEQTRD            NO - DONE                                    
         CLC   RATYEAR,0(R5)       SAME YEAR?                                   
         BH    BRTEQTRD            NO - DONE                                    
*                                                                               
         CLC   RIAVQTR,1(R5)       GET THIS QUARTER'S RATE?                     
         BL    BLDR200             NO - GET NEXT ELEMENT                        
         BH    BLDR210                                                          
*                                                                               
         CLC   SVCOST,RIAVAMT      SAME COST AS PREVIOUS WEEK                   
         BE    BLDR200                                                          
*                                                                               
         OC    RIAVAMT,RIAVAMT     ANY COST FOR THIS WEEK?                      
         BNZ   BLDR120                                                          
         LA    RF,RATQWRTE                                                      
         CR    R4,RF               STILL ON 1ST RATE                            
         BE    BLDR200                                                          
*                                                                               
         GOTO1 DATCON,DMCB,(8,RIAVWEEK),(5,TEMPWK1)                             
         GOTO1 DATVAL,DMCB,TEMPWK1,TEMPWK2                                      
*                                                                               
         XC    TEMPWK1,TEMPWK1                                                  
         GOTO1 ADDAY,DMCB,TEMPWK2,TEMPWK1,F'-1'                                 
*                                                                               
         XC    TEMPWK2,TEMPWK2                                                  
         GOTO1 DATCON,DMCB,(0,TEMPWK1),(19,TEMPWK2)                             
*                                                                               
         SH    R4,=H'3'            POINT BACK TO END DATE FOR PREV WK           
         MVC   0(3,R4),TEMPWK2                                                  
         LA    R4,3(R4)            RESTORE POINTER TO CURRENT WK                
         XC    SVCOST,SVCOST                                                    
         B     BLDR200                                                          
*                                                                               
BLDR120  MVC   0(1,R4),RIAVQTR     QUARTER #                                    
         MVC   1(3,R4),RIAVWEEK    WEEK DATE (JULIAN)                           
*                                                                               
         MVC   4(4,R4),RIAVAMT     COST                                         
         MVC   SVCOST,RIAVAMT      SET PREVIOUS COST                            
*                                                                               
         ZIC   RF,SVRATEN                                                       
         LA    RF,1(RF)                                                         
         STC   RF,SVRATEN                                                       
*                                                                               
         LA    R4,L'RATQWRTE(R4)                                                
*                                                                               
BLDR200  DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
         B     BLDR100                                                          
*                                                                               
BLDR210  DS    0H                                                               
         LA    R5,L'SVQTR(R5)      GO TO NEXT YEAR/QTR ENTRY                    
         B     BLDR110                                                          
*                                                                               
BRTEQTRD DS    0H                                                               
         GOTO1 DATCON,DMCB,(8,RIAVWEEK),(5,TEMPWK1)                             
         GOTO1 DATVAL,DMCB,TEMPWK1,TEMPWK2                                      
*                                                                               
         XC    TEMPWK1,TEMPWK1                                                  
         GOTO1 ADDAY,DMCB,TEMPWK2,TEMPWK1,F'-1'                                 
*                                                                               
         XC    TEMPWK2,TEMPWK2                                                  
         GOTO1 DATCON,DMCB,(0,TEMPWK1),(19,TEMPWK2)                             
*                                                                               
         SH    R4,=H'3'            POINT BACK TO END DATE FOR PREV WK           
         MVC   0(3,R4),TEMPWK2                                                  
         LA    R4,3(R4)            RESTORE POINTER TO CURRENT WK                
*                                                                               
         L     RF,ASVSLN           POS. IN SPOT LENGTH TABLE                    
         LA    RF,L'SVSLN(RF)      BUMP TO NEXT ENTRY                           
         ST    RF,ASVSLN                                                        
         B     BRTEDONE                                                         
*                                                                               
BRTEQTRC DS    0H                                                               
*                                                                               
BRTECONT DS    0H                                                               
         BRAS  RE,NEXTEL           FIND NEXT RATE ELMENT                        
         B     BRTELOOP                                                         
*                                                                               
BRTENXTY DS    0H                                                               
         L     RF,ASVQTR           POS. IN YEAR/QTR TABLE                       
         LA    RF,2(RF)            ANY MORE YEAR/QTRS TO FILTER ON?             
         ST    RF,ASVQTR           INCREMENT POINTER                            
         OC    0(2,RF),0(RF)       ANY MORE YEAR/QTR FILTERS?                   
         BNZ   BLDR50                                                           
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(27),HDRKEY                                                   
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
BRTENXTS DS    0H                                                               
         L     RF,ASVSLN           POS. IN SPOT LENGTH TABLE                    
         LA    RF,2(RF)            ANY MORE LENGTHS TO FILTER ON?               
         ST    RF,ASVSLN           INCREMENT POINTER                            
         OC    0(2,RF),0(RF)                                                    
         BNZ   BLDR10                                                           
*                                                                               
BRTEDONE DS    0H                                                               
*                                                                               
BRTESRTX DS    0H                                                               
         XC    KEY,KEY                                                          
         MVC   KEY,SAVEKEY         RESTORE RECORD                               
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
*                                                                               
BLDRATEX DS    0H                                                               
         XIT1                                                                   
*                                                                               
SAVEKEY  DS    XL27                                                             
HDRKEY   DS    XL27                HEADER KEY                                   
*                                                                               
*                                                                               
SVEQUNUM DS    XL1                 SAVED EQUATE NUMBER FOR 'Z' REC.             
SVCOST   DS    F                   COST FOR PREVIOUS WEEK                       
NUMCOSTS DS    X                   NUMBER OF COSTS ENTERED                      
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  R3,R6                                                            
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - PRTRATE'                
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS - PRINT RATES            *            
*                                                                  *            
*        FOR EACH ENTRY IN RATE TABLE                              *            
*              PRINT LINE OF CPP/CPM AND RATE                      *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
*NTRY    P0         DEMO VALUES TO BE RATED                        *            
*        AIO3       CONTAINS TABLE OF VALID RATES                  *            
*                   CL8(RATE CODE)                                 *            
*                   XL1(YEAR)                                      *            
*                   XL2(SPOT LENGTH)                               *            
*                   52XL8(QTR/WEEK/COST)                           *            
*        SVRATEN    XL1 - NUMBER OF ENTRIES IN TABLE               *            
*                                                                  *            
*EXIT    PRINTED LINE                                              *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
         DS    0D                                                               
PRTRATE  NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         MVC   SVDEMPTR,0(R1)      SAVE POINTER TO DEMO VALUES                  
*                                                                               
         LA    RF,SVSLN                                                         
         ST    RF,ASVSLN                                                        
*                                                                               
PRTR10   DS    0H                                                               
         GOTO1 =A(BLDRATE),RR=RELO  BUILD TABLE OF RATES                        
*                                                                               
         SR    R6,R6                                                            
         ICM   R6,1,SVRATEN        GET NUMBER OF ENTRIES IN RATE TABLE          
         BZ    PRTRATEX            NOTHING TO PRINT                             
*                                                                               
         L     R3,ABUFF            A(NEXT AVAILABLE PRINT LINE)                 
         USING PRRLINED,R3         ESTABLISH RATE PRINT LINE                    
*                                                                               
         MVC   PRRLINED(11),=C' DATE RANGE'                                     
         MVC   PRRCST,=C'  COST   '                                             
         MVC   PRRLEN,=C'SPOT'                                                  
         MVC   PRRCPP(90),=90C'-'                                               
         MVC   PRRCPP+50(7),=C'CPP/CPM'                                         
*                                                                               
         L     R4,AIO3                                                          
         USING RATENTD,R4          ESTABLISH SAVED RATES                        
         LA    R4,RATQWRTE         FIRST COST                                   
         ST    R4,ARATES                                                        
*                                                                               
PRTELOOP DS    0H                                                               
         BRAS  RE,BUFFADD          BUMP TO NEXT LINE                            
*                                                                               
         L     R3,ABUFF            A(NEXT AVAILABLE PRINT LINE)                 
         USING PRRLINED,R3         ESTABLISH RATE PRINT LINE                    
*                                                                               
PRTEDUPX DS    0H                                                               
         L     R4,ARATES                                                        
         GOTO1 DATCON,DMCB,(8,1(R4)),(5,PRREFFST)                               
*                                                                               
         OC    8(3,R4),8(R4)       ANY END DATE?                                
         BZ    PRTE50              NO                                           
         MVI   PRREFFST+L'PRREFFST,C'-'                                         
         GOTO1 DATCON,DMCB,(8,8(R4)),(5,PRREFFEN)                               
         B     PRTEQTRX                                                         
*                                                                               
PRTE50   DS    0H                                                               
         OC    L'RATQWRTE+1(3,R4),L'RATQWRTE+1(R4)                              
         BZ    PRTEQTRX                                                         
*                                                                               
         MVI   PRREFFST+L'PRREFFST,C'-'                                         
         GOTO1 DATCON,DMCB,(8,L'RATQWRTE+1(R4)),(5,TEMPWK1)                     
         GOTO1 DATVAL,DMCB,TEMPWK1,TEMPWK2                                      
*                                                                               
         XC    TEMPWK1,TEMPWK1                                                  
         GOTO1 ADDAY,DMCB,TEMPWK2,TEMPWK1,F'-1'                                 
*                                                                               
         XC    TEMPWK2,TEMPWK2                                                  
         GOTO1 DATCON,DMCB,(0,TEMPWK1),(5,PRREFFEN)                             
*                                                                               
PRTEQTRX DS    0H                                                               
         L     R4,AIO3                                                          
*                                                                               
         MVC   HALF,RATSLN         COPY SPOT LENGTH                             
         NI    HALF,X'FF'-X'80'    KILL MINUTES INDICATOR                       
         EDIT  (2,HALF),(4,PRRLEN),ALIGN=LEFT  PRINT SPOT LENGTH                
*                                                                               
         TM    RATSLN,X'80'        SKIP IF NOT MINUTES                          
         BNO   PRTEMINX                                                         
*                                                                               
         LA    RF,PRRLEN+L'PRRLEN  END OF FIELD                                 
*                                                                               
         CLI   0(RF),C' '          FIND LAST PRINTED NUMBER                     
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         MVI   1(RF),C'M'          INDICATE LENGTH IN MINUTES                   
*                                                                               
PRTEMINX DS    0H                                                               
*                                                                               
*        PRINT CPP/CPM                                                          
*                                                                               
         L     R5,SVDEMPTR         POINT TO DEMO VALUES                         
*                                                                               
         SR    R2,R2                                                            
         ICM   R2,1,NUMDEMS        NUMBER OF DEMOS IN LIST                      
         BZ    PRTECPPX                                                         
*                                                                               
         LA    R1,PRRCPP           POINT TO CPP PRINT AREA                      
*                                                                               
PRTECPPL DS    0H                                                               
*                                                                               
         OC    0(4,R5),0(R5)       IF NO DEMO VALUE                             
         BNZ   PRTECPP1                                                         
*                                                                               
         CLI   MASOP5,C'Y'         IF ROUNDING DEMOS                            
         BE    PRTECPZX               NO DECIMAL POINT                          
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ    IF NORMAL DISPLAY                            
         BH    PRTECPZ1                                                         
*                                                                               
         MVI   6(R1),C'.'             SET  DECIMAL POINT                        
*                                                                               
         B     PRTECPZX                                                         
*                                                                               
PRTECPZ1 DS    0H                                                               
*                                  IF MAX NUMBER OF DEMOS                       
         MVI   5(R1),C'.'             SET DECIMAL POINT                         
*                                                                               
PRTECPZX DS    0H                                                               
*                                                                               
         B     PRTECPPC                                                         
*                                                                               
PRTECPP1 DS    0H                                                               
*                                                                               
         MVC   FULL,0(R5)          GET DEMO VALUE                               
*                                                                               
         L     R4,ARATES                                                        
         ICM   RF,15,4(R4)         COST                                         
         L     R4,AIO3                                                          
*                                                                               
         M     RE,=F'10'           SCALE UP FOR PRECISION                       
*                                                                               
         D     RE,FULL             CPP/CPM                                      
*                                                                               
         SLL   RE,1                DOUBLE REMAINDER                             
         C     RE,FULL             TEST FOR ROUNDING                            
         BL    *+8                                                              
         LA    RF,1(RF)                                                         
*                                                                               
         CLI   MASOP5,C'Y'         IF ROUNDING DEMOS                            
         BNE   PRTECPP5                                                         
*                                                                               
         SR    RE,RE                                                            
         D     RE,=F'1000'            ROUND DOWN TO WHOLE DOLLARS               
*                                                                               
         SLL   RE,1                   DOUBLE REMAINDER                          
         C     RE,=F'1000'            TEST FOR ROUNDING                         
         BL    *+8                                                              
         LA    RF,1(RF)                                                         
*                                                                               
PRTECPP5 DS    0H                                                               
*                                                                               
         CLI   MASOP5,C'Y'         IF ROUNDING DEMOS                            
         BNE   PRTERNDN                                                         
*                                                                               
         EDIT  (RF),(6,1(R1)),0       PRINT WITH NO DECIMALS                    
*                                                                               
         B     PRTECPP8                                                         
*                                                                               
PRTERNDN DS    0H                                                               
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ    IF NORMAL DISPLAY                            
         BH    PRTENRMN                                                         
*                                                                               
         EDIT  (RF),(9,(R1)),2        PRINT CPP/CPM                             
*                                                                               
         B     PRTECPP8                                                         
*                                                                               
PRTENRMN DS    0H                                                               
*                                  MAX NUMBER OF DEMOS                          
         EDIT  (RF),(8,(R1)),2        PRINT CPP/CPM                             
*                                                                               
PRTECPP8 DS    0H                                                               
*                                                                               
PRTECPPC DS    0H                                                               
*                                                                               
         LA    R1,L'PRRCPP-3(R1)   NEXT PRINT AREA FOR ROUNDED DEMOS            
*                                                                               
         CLI   MASOP5,C'Y'         DONE IF ROUNDING DEMOS                       
         BE    PRTECPC1                                                         
*                                                                               
         LA    R1,2(R1)            ROOM FOR DECIMALS                            
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ    DONE IF NOT NORMAL DISPLAY                   
         BH    *+8                                                              
         LA    R1,1(R1)            MORE SPACE IN LAYOUT                         
*                                                                               
PRTECPC1 DS    0H                                                               
*                                                                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R2,PRTECPPL                                                      
*                                                                               
PRTECPPD DS    0H                                                               
*                                                                               
PRTECPPX DS    0H                                                               
*                                                                               
         L     R4,ARATES                                                        
         EDIT  (4,4(R4)),(9,PRRCST),2,FLOAT=$     PRINT COST                    
*                                                                               
PRTECONT DS    0H                                                               
*                                                                               
         LA    R4,L'RATQWRTE(R4)   GET NEXT RATE                                
         ST    R4,ARATES                                                        
         L     R4,AIO3                                                          
         BCT   R6,PRTELOOP                                                      
*                                                                               
PRTEDONE DS    0H                                                               
*                                                                               
         LA    R3,132(R3)          BUMP OUTPUT POINTER                          
         ST    R3,ABUFF                                                         
*                                                                               
         BRAS  RE,BUFFFULL         CHECK IF PAGE IS FULL                        
         B     PRTR10                                                           
*                                                                               
PRTRATEX DS    0H                                                               
         XIT1                                                                   
*                                                                               
ARATES   DS    A                   A(RATES IN TABLE)                            
*                                                                               
         LTORG                                                                  
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - HOOK'                   
********************************************************************            
*                                                                  *            
*     RERES0E (T8190E) --- GENERAL AVAILS - HEADLINE HOOK          *            
*                                                                  *            
********************************************************************            
         SPACE 2                                                                
         DS    0D                                                               
HOOK     NTR1 BASE=*,LABEL=*                                                    
*                                                                               
*        REPORT TITLE - USER'S OR DEFAULT                                       
*                                                                               
         LA    R1,H1+51            POINT TO FIRST LINE OF TITLE                 
         LA    R2,MASTTL1H         POINT TO FIRST INPUT TITLE                   
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET TITLE INPUT LENGTH                       
         BZ    HKTTL1N             USE DEFAULT TITLE                            
*                                                                               
         MVC   0(30,R1),SPACES     INIT TITLE AREA                              
         MVC   132(30,R1),SPACES   INIT TITLE UNDERLINE AREA                    
*                                                                               
         LA    RE,L'MASTTL1        MAX TITLE LENGTH                             
         SR    RE,RF                                                            
         SRL   RE,1                TITLE DISPLACEMENT INTO TITLE AREA           
         LA    RE,0(R1,RE)         TITLE STARTING POINT IN LINE                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)       PRINT TITLE CENTERED                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   132(0,RE),DASHS     UNDERLINE TITLE                              
*                                                                               
HKTTL1N  DS    0H                                                               
*                                                                               
         LA    R1,H2+51            POINT TO SECOND LINE OF TITLE                
         LA    R2,MASTTL2H         POINT TO SECOND INPUT TITLE                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET TITLE INPUT LENGTH                       
         BZ    HKTTL2N             USE DEFAULT TITLE (UNDERLINE OF 1ST)         
*                                                                               
         MVC   0(30,R1),SPACES     INIT TITLE AREA                              
         MVC   132(30,R1),SPACES   INIT TITLE UNDERLINE AREA                    
*                                                                               
         LA    RE,L'MASTTL1        MAX TITLE LENGTH                             
         SR    RE,RF                                                            
         SRL   RE,1                TITLE DISPLACEMENT INTO TITLE AREA           
         LA    RE,0(R1,RE)         TITLE STARTING POINT IN LINE                 
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),8(R2)       PRINT TITLE CENTERED                         
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   132(0,RE),DASHS     UNDERLINE TITLE                              
*                                                                               
HKTTL2N  DS    0H                                                               
*                                                                               
         MVC   H3+10(6),MASSTN     STATION CALL LETTERS                         
         MVC   H3+17(29),MKTSV     MARKET NAME                                  
         MVC   H3+108(3),MASSRCE   SOURCE                                       
*                                                                               
         MVC   H4+12(8),SVRTCD     RATE CODE                                    
*                                                                               
*        PRINT DAYPART                                                          
*                                                                               
         L     R3,ADPTBL           LOOK UP DAYPART                              
         USING DPTBLD,R3           ESTABLISH DAYPART TABLE                      
*                                                                               
HKDPTLP  DS    0H                                                               
*                                                                               
         OC    DPTBLD(DPTBLL),DPTBLD  CHECK FOR END OF TABLE                    
         BZ    HKDPTDN                                                          
*                                                                               
         CLC   SVDPT,DPTBCODE      MATCH DAYPART CODE                           
         BE    HKDPTFD                                                          
*                                                                               
HKDPTCN  DS    0H                                                               
*                                                                               
         LA    R3,DPTBLL(R3)                                                    
         B     HKDPTLP                                                          
*                                                                               
HKDPTFD  DS    0H                                                               
*                                                                               
         MVC   H4+65(L'DPTBLNAM),DPTBLNAM  DAYPART NAME                         
         MVC   H5(2),=XL2'00'      FORCE PRINTING                               
*                                                                               
         DROP  R3                                                               
*                                                                               
HKDPTDN  DS    0H                                                               
*                                                                               
*        TEXT COMMENT                                                           
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,MASTXTH+5      LENGTH OF ENTERED TEXT                       
         BZ    HKTXTN              NONE TO PRINT                                
*                                                                               
         MVC   H4(10),=CL10'COMMENT - '                                         
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   H4+10(0),MASTXT     PRINT COMMENT TEXT                           
*                                                                               
HKTXTN   DS    0H                                                               
*                                                                               
*        DEMO HEADLINES                                                         
*                                                                               
         MVC   H9,SVH7             MOVE IN SAVED DEMO HEADLINES                 
         MVC   H10,SVH8                                                         
         MVC   H11,SVH9                                                         
*                                                                               
         CLI   MASOP9,C'Y'         SKIP IF INCLUDING INVENTORY NUMBER           
         BE    *+14                                                             
         MVC   H7+000(13),=C'DAY TIME     '                                     
         B     *+10                                                             
         MVC   H7+000(13),=C'INV  DAY TIME'                                     
*                                                                               
         MVC   H7+022(17),=C'PROGRAM/ADJACENCY'                                 
         MVC   H7+107(9),=C'EFFECTIVE'                                          
         MVC   H9+001(10),=C'BOOK    CD'                                        
*                                                                               
         CLI   CONTSW,C'Y'                                                      
         BNE   *+10                                                             
         MVC   H12(16),CONTINUE    NNNN (CONTINUED)                             
*                                                                               
HOOKX    DS    0H                                                               
         XIT1                                                                   
DASHS    DC    60C'-'                                                           
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
         TITLE 'RERES0E - MASTER REPORT - PFKEYS'                               
***********************************************************************         
*                                                                     *         
*        ANALYZE PFKEYS                                               *         
*                                                                     *         
*              PF10 - TITLES REPORT                                   *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYS   NTR1 BASE=*,LABEL=*                                                    
*                                                                               
         L     R3,SYSPARMS                                                      
         ICM   R3,15,0(R3)         POINT TO TIOB                                
         USING TIOBD,R3                                                         
*                                                                               
         CLI   TIOBAID,9           CHECK FOR TRANSFER TO TITLES REPORT          
         BE    *+8                                                              
         CLI   TIOBAID,21          CHECK FOR TRANSFER TO TITLES REPORT          
         BE    PFKTTL                                                           
*                                                                               
         B     PFKEYSX             IGNORE ALL OTHER KEYS                        
*                                                                               
***********************************************************************         
*                                                                     *         
*        TRANSFER TO TITLES REPORT USING GLOBBER                      *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKTTL   DS    0H                  NORMAL EXIT                                  
*                                                                               
         XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'REP'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'RSC'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'REP'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'RSC'    SET TO   PROGRAM                             
***>>>   OI    GLVXFLG1,GLV1GOTO+GLV1SEPS                                       
         OI    GLVXFLG1,GLV1SEPS                                                
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL  SEND XCTL ELM            
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'TITLES',6,GLVXREC  RECORD FLD          
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'REPORT',6,GLVXACT  ACTION              
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'DEFAULT',7,GLRKEY  KEY                 
         LA    R2,CONWHEN                                                       
         GOTO1 VGLOBBER,DMCB,=C'PUTD',=C'NOW',3,GLRWHEN  WHEN FIELD             
*                                                                               
*        BUILD VARIABLE DATA FOR TITLES REQUEST SCREEN                          
*                                                                               
         XC    WORK,WORK                                                        
         LA    R2,WORK                                                          
         USING TTLDATAD,R2         ESTABLISH TITLE DATA                         
*                                                                               
         MVC   TTLSTNL,MASSTNH+5   PASS STATION LENGTH                          
         MVC   TTLSTN,MASSTN       PASS STATION                                 
         MVC   TTLDPTL,MASDPTH+5   PASS DAYPART LENGTH                          
         MVC   TTLDPT,MASDPT       PASS DAYPART                                 
         MVC   TTLESDL,MASESDTH+5  PASS EFFECTIVE START DATE LENGTH             
         MVC   TTLESD,MASESDT      PASS EFFECTIVE START DATE                    
         MVC   TTLEEDL,MASEEDTH+5  PASS EFFECTIVE END   DATE LENGTH             
         MVC   TTLEED,MASEEDT      PASS EFFECTIVE END   DATE                    
         MVC   TTLFTRL,MASFTRH+5   PASS FILTERS LENGTH                          
         MVC   TTLFTR,MASFTR       PASS FILTERS                                 
         MVC   TTLOP3L,MASOP3H+5   PASS NO DUPLICATES OPTION LENGTH             
         MVC   TTLOP3,MASOP3       PASS NO DUPLICATES OPTION                    
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,TTLDATAL,GLRDATA  SEND DATA          
*                                                                               
         GOTO1 ERREX                                                            
*                                                                               
         TITLE 'RERES0E - MASTER REPORT - PFKEYS - EXITS'                       
***********************************************************************         
*                                                                     *         
*        EXIT ROUTINES                                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PFKEYSX  DS    0H                  NORMAL EXIT                                  
         CR    RB,RB               FORCE EQ CC                                  
         XIT1                                                                   
*                                                                               
PFKEYERX DS    0H                  ERROR EXIT                                   
         LTR   RB,RB               FORCE NE CC                                  
         XIT1                                                                   
                                                                                
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                               
*           ROUTINE TO PRINT '03' ELEMENT OUT                                   
*         ------------------------------------                                  
*********************************************************************           
         SPACE                                                                  
HISTDATA NTR1  BASE=*,LABEL=*                                                   
         L     RC,0(R1)                                                         
         L     R2,4(R1)           CONVERSION TABLE                              
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'                                                     
         BRAS  RE,GETEL                                                         
         BNE   HISTX              SHOULD NEVER HAPPEN                           
         USING RINVFREL,R6                                                      
*                                                                               
         L     R3,ABUFF           ADD LINE TO BUFFER                            
         LA    R5,28(R3)                                                        
         MVC   0(15,R5),SPACES                                                  
         MVC   0(5,R5),RINVFRST   MOVE STATION                                  
         LA    R5,6(R5)                                                         
*                                                                               
HIST20   CLC   RINVFRBK(1),3(R2)   FROM BOOKVAL FORMAT TO PRINTABLE FMT         
         BE    HIST30                                                           
         LA    R2,4(R2)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   HIST20                                                           
         DC    H'0'                                                             
HIST30   CLI   1(R2),C' '                                                       
         BE    HIST40                                                           
         MVC   0(1,R5),1(R2)                                                    
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
HIST40   ZIC   R1,RINVFRBK+2               MONTH                                
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R5),0(R1)                                                    
         MVI   3(R5),C'/'                                                       
         ZIC   R1,RINVFRBK+1                                                    
         EDIT  (R1),(2,4(R5))     YEAR                                          
         LA    R5,7(R5)                                                         
*                                                                               
         MVC   0(1,R5),RINVFRTY      TYPE (P/I)                                 
         MVC   1(1,R5),RINVFRPR      FUNCTION                                   
         MVC   2(1,R5),RINVFRBT      BOOK TYPE                                  
         LA    R5,4(R5)                                                         
*                                                                               
         ZIC   R2,RINVFRLN        MOVE FROM DATA IF ANY                         
         SH    R2,=H'16'                                                        
         LTR   R2,R2                                                            
         BZ    HISTADD            NO OTHER DATA TO BE ADDED                     
         CH    R2,=H'70'          WILL IT FIT ON ONE LINE?                      
         BNH   HIST45             NO                                            
*                                                                               
         LA    R4,70                                                            
         GOTO1 CHOPPER,DMCB,((R2),RINVFRDT),((R4),(R5)),(C'P',3)                
         B     HISTADD                                                          
*                                                                               
HIST45   BCTR  R2,0                                                             
         EX    R2,HISTMV                                                        
         B     HISTADD                                                          
*                                                                               
HISTMV   MVC   0(0,R5),RINVFRDT                                                 
HISTADD  LA    R3,132(R3)                                                       
         ST    R3,ABUFF                                                         
HISTX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS - HDRNEW'                 
***********************************************************************         
*                                                                     *         
*        PROCESSING HEADER RECORDS                                    *         
*        -------------------------                                    *         
*        R4, ON ENTRY, POINTS TO INVENTORY KEY                        *         
*        R6, ON ENTRY, POINTS TO INVENTORY 01 EL                      *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
HDRNEW   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         CLI   MASOP6,C'Y'         IF SINGLE INV PER PAGE                       
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'          FORCE NEW PAGE                            
*                                                                               
         MVC   P,SPACES            THEN PRINT                                   
         GOTO1 SPOOL,PARAS,(R8)    FORCE BLANK LINE                             
*                                                                               
         USING RINVKEY,R4          ESTABLISH INVENTORY KEY                      
*                                                                               
         NI    PRNTOPT2,X'3F'      RESET BUFFER MORE THAN 1 PAGE BIT            
*                                                                               
         MVC   P,SPACES            INIT WORKAREA                                
         MVC   P2,SPACES           INIT WORKAREA                                
*                                                                               
         L     R2,SBUFF            INIT PRINT BUFFER                            
         ST    R2,ABUFF                                                         
*                                                                               
*        INVENTORY ID                                                           
*                                                                               
         CLI   MASOP9,C'Y'         IF REPORTING INVENT NUMBER                   
         BNE   HDNINVN                                                          
*                                                                               
         MVC   P(L'RINVKINV),RINVKINV     PRINT INVENTORY ID                    
*                                                                               
         MVC   CONTINUE(4),P                                                    
         MVC   CONTINUE+5(11),=C'(CONTINUED)'                                   
         LA    R3,P+5              START OF REST OF LINE                        
*                                                                               
         B     HDNINVX                                                          
*                                                                               
HDNINVN  DS    0H                                                               
*                                                                               
         LA    R3,P                START OF PRINT LINE                          
         MVC   CONTINUE(11),=C'(CONTINUED)'                                     
*                                                                               
HDNINVX  DS    0H                                                               
*                                                                               
*        PROGRAM DAY/TIMES                                                      
*                                                                               
*        USE AVAIL DAY/TIMES IF PRESENT IN INVENTORY RECORD                     
*                                                                               
HDNWAV   DS    0H                                                               
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'04'        AVAIL DAY/TIMES USED IF FOUND                
*                                                                               
         SR    R2,R2               INIT ELEMENT COUNTER                         
*                                                                               
         BRAS  RE,GETEL                                                         
         BNE   HDNWAVN             NONE FOUND                                   
*                                                                               
HDNWAVLP DS    0H                                                               
*                                                                               
         AH    R2,=H'1'            BUMP ELEMENT COUNTER                         
*                                                                               
         USING RIAPELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         MVC   WORK(L'RIADAY),RIADAY   AVAIL DAYS                               
         MVC   WORK+L'RIADAY+1(L'RIATIME),RIATIME   AVAIL TIMES                 
*                                                                               
         OC    WORK,SPACES         MAKE UPPERCASE                               
*                                                                               
         LA    RF,L'RIADAY+L'RIATIME+1 TOTAL LENGTH                             
*                                                                               
         GOTO1 SQUASHER,PARAS,WORK,(RF)       SQUASH DAY/TIME                   
*                                                                               
         MVC   0(L'RIADAY+L'RIATIME+1,R3),WORK  PRINT DAY/TIME                  
*                                                                               
         LA    R0,L'RIADAY+L'RIATIME+1                                          
         LA    R3,L'RIADAY+L'RIATIME(R3) FIND LAST PRINTABLE CHARACTER          
*                                                                               
         CLI   0(R3),C' '                                                       
         BH    *+14                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10                                                          
         B     *+12                NO DATA TO PRINT                             
         MVI   1(R3),C','          SET SEPARATOR                                
         LA    R3,3(R3)            NEXT PRINT POSITION                          
*                                                                               
HDNWAVCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           NEXT DAY/TME ELEMENT                         
         BE    HDNWAVLP            ONE FOUND                                    
*                                                                               
HDNWAVDN DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         BCTR  R3,0                BACK UP A POSITION                           
         CLI   0(R3),C','          IF THERE IS A TRAILING COMMA                 
         BNE   *+10                                                             
         MVI   0(R3),C' '             ELIMINATE IT                              
         BCTR  R3,0                   RESET POINTER                             
*                                                                               
         LA    R3,2(R3)            RESET END OF DATA POINTER                    
*                                                                               
         B     HDNWDTX                                                          
*                                                                               
HDNWAVN  DS    0H                                                               
*                                                                               
*        PRINT DAY/TIME ELEMENTS                                                
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'02'        SET TO FIND A DAY/TIME ELEMENTS              
*                                                                               
         SR    R2,R2               INIT ELEMENT COUNTER                         
*                                                                               
         BRAS  RE,GETEL            FIND FIRST DAY/TIME ELEMENT                  
         BNE   HDNWDTX             NONE FOUND - SKIP PRINTING                   
*                                                                               
HDNWDTLP DS    0H                                                               
*                                                                               
         AH    R2,=H'1'            BUMP ELEMENT COUNTER                         
*                                                                               
         USING RIDTELEM,R6         ESTABLISH DAY/TIME ELEMENT                   
*                                                                               
         MVC   WORK,SPACES                                                      
         GOTO1 UNDAY,PARAS,RIDTDAY,WORK       DAY                               
*                                                                               
         OC    WORK,SPACES                                                      
         GOTO1 UNTIME,PARAS,RIDTTIME,WORK+20  TIME                              
*                                                                               
         GOTO1 SQUASHER,PARAS,WORK,40         SQUASH DAY/TIME                   
*                                                                               
         MVC   0(22,R3),WORK                  PRINT DAY/TIME                    
*                                                                               
         LA    R0,22                                                            
         LA    R3,21(R3)           FIND LAST PRINTABLE CHARACTER                
*                                                                               
         CLI   0(R3),C' '                                                       
         BH    *+14                                                             
         BCTR  R3,0                                                             
         BCT   R0,*-10                                                          
         B     *+12                NO DATA TO PRINT                             
         MVI   1(R3),C','          SET SEPARATOR                                
         LA    R3,3(R3)            NEXT PRINT POSITION                          
*                                                                               
HDNWDTCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           NEXT DAY/TME ELEMENT                         
         BE    HDNWDTLP            ONE FOUND                                    
*                                                                               
HDNWDTDN DS    0H                  END OF DAY/TIME ELEMENTS                     
*                                                                               
         CH    R2,=H'1'            IF MORE THAN ONE DAY-TIME ELEMENT            
         BNH   *+8                                                              
         MVI   PRNTDISP+1,0           USE ALTERNATE FORMAT                      
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         BCTR  R3,0                BACK UP A POSITION                           
         CLI   0(R3),C','          IF THERE IS A TRAILING COMMA                 
         BNE   *+10                                                             
         MVI   0(R3),C' '             ELIMINATE IT                              
         BCTR  R3,0                   RESET POINTER                             
*                                                                               
         LA    R3,2(R3)            RESET END OF DATA POINTER                    
*                                                                               
HDNWDTX  DS    0H                                                               
*                                                                               
*        PRINT PROGRAM NAMES                                                    
*                                                                               
*        IF NOT USING ALTERNATE FORMAT,                                         
*           USE IT ANYWAY IF THERE ARE 2 PROGRAM ELEMENTS                       
*                                                                               
         SR    R2,R2               INIT ELEMENT COUNTER                         
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'        FIND A PROGRAM ELEMENT                       
         BRAS  RE,GETEL                                                         
         BNE   HDNWPGX                                                          
*                                                                               
         LA    R2,1(R2)            BUMP ELEMENT COUNTER                         
         BRAS  RE,NEXTEL           NEXT PROGRAM ELEMENT                         
         BE    *-8                 ONE FOUND                                    
*                                                                               
         LA    RF,P+22                PROGRAMS START ON/AFTER COL 23            
         CR    R3,RF                                                            
         BNL   *+6                                                              
         LR    R3,RF                                                            
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'        FIND A PROGRAM ELEMENT                       
         BRAS  RE,GETEL                                                         
         BNE   HDNWPGX                                                          
*                                                                               
HDNWPGLP DS    0H                                                               
*                                                                               
         USING RIPGELEM,R6         ESTABLISH PROGRAM ELEMENT                    
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RIPGLEN          ELEMENT LENGTH                               
         SH    RF,=Y(RIPGNAME-RIPGELEM)  PROGRAM NAME LENGTH                    
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),RIPGNAME    PRINT PROGRAM NAME                           
*                                                                               
         LA    R3,1(RF,R3)         NEXT PRINT POSITION                          
         MVI   0(R3),C','          DATA SEPARATOR                               
         LA    R3,2(R3)            BUMP POINTER                                 
*                                                                               
HDNWPGCN DS    0H                                                               
*                                                                               
         BRAS  RE,NEXTEL           NEXT PROGRAM ELEMENT                         
         BE    HDNWPGLP            ONE FOUND                                    
*                                                                               
HDNWPGDN DS    0H                                                               
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         BCTR  R3,0                BACK UP A POSITION                           
         CLI   0(R3),C','          IF THERE IS A TRAILING COMMA                 
         BNE   *+10                                                             
         MVI   0(R3),C' '             ELIMINATE IT                              
         BCTR  R3,0                   RESET POINTER                             
*                                                                               
         LA    R3,1(R3)            RESET END OF DATA POINTER                    
*                                                                               
         LR    R0,R3               CALCULATE DATA LENGTH                        
         LA    RE,P                                                             
         SR    R0,RE                                                            
*                                                                               
         LTR   R0,R0               COPY LENGTH                                  
         BZ    HDNWPGX             NO DATA TO MOVE/CHOP                         
*                                                                               
*        PRINT DAY-TIMES/PROGRAMS ON 2 LINES                                    
*        CAN'T USE CHOPPER BECAUSE LENGTH CAN BE >256                           
*        KEEP PRINT TO FIRST 100 BYTES OF A LINE                                
*                                                                               
         L     R2,ABUFF            A(PRINT LINE)                                
*                                                                               
         MVC   0(5,R2),P           PRINT INVENTORY NUMBER PLUS SPACE            
         SH    R0,=H'5'            DECREMENT TOTAL LENGTH                       
*                                                                               
*        PRINT DAY-TIMES AND PROGRAM NAMES                                      
*                                                                               
         LA    R1,P+5              START OF DATA TO BE PRINTED                  
*                                                                               
HDNWPRLP DS    0H                                                               
*                                                                               
         CLI   0(R1),C','          DON'T START WITH COMMA OR SPACE              
         BE    *+12                                                             
         CLI   0(R1),C' '          DON'T START WITH COMMA OR SPACE              
         BH    *+12                                                             
         LA    R1,1(R1)                                                         
         BCT   R0,*-20                                                          
*                                                                               
         LA    RF,94(R1)           LAST OF DATA FOR A LINE                      
*                                                                               
         CLI   0(RF),C' '          SPLIT ON LAST SPACE OR COMMA                 
         BNH   *+16                                                             
         CLI   0(RF),C','                                                       
         BE    *+8                                                              
         BCT   RF,*-16                                                          
*                                                                               
         LA    RF,1(RF)            INCLUDE SPACE OR COMMA                       
*                                                                               
         SR    RF,R1               LENGTH OF DATA TO MOVE                       
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   5(0,R2),0(R1)       PRINT LINE                                   
*                                                                               
HDNWPRCN DS    0H                                                               
*                                                                               
         LA    R2,132(R2)          BUMP BUFFER POINTER                          
         LA    R1,1(RF,R1)         START OF NEXT PORTION                        
         SR    R0,RF               DECREMENT LENGTH COUNTER                     
         SH    R0,=H'1'            CONTINUE IF MORE DATA                        
         BP    HDNWPRLP                                                         
*                                                                               
HDNWPRDN DS    0H                                                               
*                                                                               
         ST    R2,ABUFF            UPDATE BUFFER POINTER                        
*                                                                               
         L     R3,ABUFF            NEXT AVAILABLE PRINT LINE                    
*                                                                               
         MVC   P,SPACES            INIT WORKAREA                                
         MVC   P2,SPACES           INIT WORKAREA                                
         MVC   P3,SPACES           INIT WORKAREA                                
*                                                                               
HDNWPGX  DS    0H                                                               
*                                                                               
*        EFFECTIVE DATE(S)                                                      
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'        FIND HEADER ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   HDNWEFX                                                          
*                                                                               
         USING RINVPEL,R6          ESTABLISH HEADER ELEMENT                     
*                                                                               
         MVC   P,SPACES            INIT WORKAREA                                
         MVC   P2,SPACES           INIT WORKAREA                                
         MVC   WORK,SPACES                    EFFECTIVE DATES                   
*                                                                               
         L     R3,SBUFF                USE FIRST LINE IN BUFFER                 
         LA    R3,107(R3)                                                       
*                                                                               
         CLC   RINVPEFF(2),RINVPEFF+2   CHECK FOR SINGLE DATE                   
         BNE   HDNW13                                                           
*                                                                               
         MVC   WORK(4),=C'ONLY'                                                 
*                                                                               
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,WORK+5)                             
*                                                                               
         B     HDNW16                                                           
*                                                                               
HDNW13   DS    0H                                                               
*                                                                               
         MVC   WORK(4),=C'FROM'                                                 
*                                                                               
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,WORK+5)                             
*                                                                               
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    HDNW16                                                           
*                                                                               
         MVC   WORK(8),WORK+5                                                   
         MVC   WORK+8(56),SPACES                                                
         MVI   WORK+8,C'-'                                                      
*                                                                               
         GOTO1 DATCON,PARAS,(2,RINVPEFF+2),(8,WORK+9)                           
*                                                                               
HDNW16   DS    0H                                                               
*                                                                               
         MVC   0(17,R3),WORK       EFFECTIVE DATE(S)                            
         LA    R3,17(R3)           NEXT PRINT POSITION                          
*                                                                               
HDNWEFX  DS    0H                                                               
*                                                                               
HDRNEWX  DS    0H                                                               
         XIT1                                                                   
*                                                                               
         DROP  R4,R6                                                            
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS'                          
*********************************************************************           
*              ------------------------------                                   
*              ONLY BUILD DEMO HEADLINES ONCE                                   
*              ------------------------------                                   
*********************************************************************           
         SPACE 2                                                                
         DS    0D                                                               
DHED     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         XC    SVH7,SVH7                                                        
         XC    SVH8,SVH8                                                        
         XC    SVH9,SVH9                                                        
*                                                                               
         LA    R3,SVH7+28          POINT TO WHERE 1ST DEMO SHOULD PRINT         
         LA    R5,SVH8+28                                                       
*                                                                               
         LA    R2,DEMLST                                                        
         ZIC   R6,NUMDEMS                                                       
*                                                                               
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
*                                                                               
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
*                                                                               
DHED40   CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
*                                                                               
         GOTO1 DEMOCON,PARAS,(0,(R2)),(4,WORK),(0,DBLOCKD)                      
*                                                                               
         CLI   1(R2),C'I'          RESET FOR NEXT TIME                          
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
*                                                                               
         MVC   0(4,R3),WORK        FORMAT - .WM.1849                            
         MVC   0(4,R5),WORK+4                                                   
*                                                                               
         LA    R1,TYPTAB                                                        
*                                                                               
DHED50   CLC   0(1,R1),1(R2)                                                    
         BE    DHED60                                                           
*                                                                               
         LA    R1,5(R1)                                                         
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BNE   DHED50                                                           
*                                                                               
DHED60   MVC   SVH9-SVH8(4,R5),1(R1)                                            
*                                                                               
         CLC   1(2,R2),=C'D702'    PUT=HUT FOR MET                              
         BNE   *+8                                                              
         MVI   132(R5),C'H'                                                     
         CLC   1(2,R2),=X'D703'    OR METB                                      
         BNE   *+8                                                              
         MVI   132(R5),C'H'                                                     
*                                                                               
         LA    R2,3(R2)                                                         
         LA    R3,6(R3)                                                         
         LA    R5,6(R5)                                                         
*                                                                               
         CLI   MASOP5,C'Y'         SKIP IF DOING ROUNDED DEMOS                  
         BE    DHED70                                                           
*                                                                               
         LA    R3,2(R3)                                                         
         LA    R5,2(R5)                                                         
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ    SKIP IF DOING > DEM#NRMQ DEMOS               
         BH    DHED70                                                           
*                                                                               
         LA    R3,1(R3)                                                         
         LA    R5,1(R5)                                                         
*                                                                               
DHED70   DS    0H                                                               
*                                                                               
         BCT   R6,DHED40                                                        
*                                                                               
         XIT1                                                                   
*                                                                               
TYPTAB   DC    C'TTSA '            TSA IMPS                                     
         DC    C'RRTG '            ADI/DMA RTGS                                 
         DC    C'SSHR '            ADI/DMA SHRS                                 
         DC    C'PPUT '            ADI/DMA LVLS (PUTS)                          
         DC    C'CCHR '                                                         
         DC    C'XTSH '            TSA SHRS                                     
         DC    C'QTOT '            TSA LVLS (TOTS)                              
         DC    C'UUNV '            UNVS                                         
         DC    X'FF'                                                            
         DC    C'    '                                                          
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*              FORMAT BOOK SEQUENCE                                             
*              --------------------                                             
**********************************************************************          
         SPACE 1                                                                
BOOKSQ   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RC,0(R1)                                                         
         L     R1,4(R1)            PASSED CONVERSION TABLE                      
*                                                                               
         L     R3,ABUFF                                                         
         LA    R3,1(R3)                                                         
*                                                                               
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
*                                                                               
         XC    GSRCIN(GSRCINL),GSRCIN INIT GETKSRC PARAMETERS                   
         XC    GSRCOUT(GSRCOUTL),GSRCOUT                                        
*                                                                               
*        FIND QUALIFIER AND BOOKTYPE                                            
*                                                                               
*        BUILD INPUT BLOCK FOR GETKSRC                                          
*                                                                               
         MVC   GSIRSVC,RINVKRSR    SET KSRC                                     
*                                                                               
*              ** BELOW ADDED FOR "NEW" GETKSRC **                              
         MVC   GSIQLF,RINVKQLF     SET QUALIFIER                                
         MVC   GSIBKTYP,RINVKBTP   SET BOOK TYPE                                
*                                                                               
         GOTO1 VGETKSRC,DMCB,(C'K',GSRCIN),GSRCOUT,ACOMFACS                     
         CLI   DMCB+4,0            CHECK FOR ERRORS                             
         BE    BKSQ200                                                          
*                                                                               
BKSQ100  CLI   HDRSW,C'Y'          HAS ANY DATA BEEN PRINTED                    
         BNE   BKSQ144                                                          
         BRAS  RE,BUFFADD          NO, SO POINT PAST HEADER LINE                
         BRAS  RE,BUFFFULL                                                      
         BRAS  RE,BUFFADD          THEN LEAVE SPACING LINE                      
         BRAS  RE,BUFFFULL                                                      
         SPACE 1                                                                
BKSQ144  L     RE,SBUFF                                                         
         LA    RF,15               COULD BE 15 HEADLINES                        
BKSQ147  OC    0(40,RE),0(RE)                                                   
         BZ    BKSQ148                                                          
         LA    RE,132(RE)                                                       
         BCT   RF,BKSQ147                                                       
         SPACE 1                                                                
BKSQ148  L     RF,ABUFF                                                         
         CR    RE,RF                                                            
         BH    *+6                                                              
         LR    RE,RF                                                            
         LA    RE,132(RE)          SPACING AFTER EVERY GROUP                    
         ST    RE,ABUFF                                                         
         BRAS  RE,BUFFFULL                                                      
         SPACE 1                                                                
         BRAS  RE,PRNTALL                                                       
         B     BKSQX                                                            
                                                                                
*                                                                               
BKSQ200  CLI   GSOQLF,C' '         IF THERE IS A QUALIFIER                      
         BNH   *+14                                                             
         MVC   0(1,R3),GSOQLF         PRINT IT                                  
         LA    R3,1(R3)                                                         
*                                                                               
         ZIC   R1,RINVKBK+1                                                     
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R3),0(R1)                                                    
*                                                                               
         CLI   RINVKBK+1,0             UNSPECIFIED MONTH                        
         BNE   BKSQ300                                                          
*                                                                               
         BCTR  R3,0                                                             
         MVC   0(3,R3),=C'EST'     E BECOMES EST                                
*                                                                               
BKSQ300  DS    0H                                                               
*                                                                               
         EDIT  (1,RINVKBK),(2,3(R3)),WRK=DMCB                                   
*                                                                               
         CLI   GSOBKTYP,C' '       IF THERE IS A BOOKTYPE                       
         BE    BKSQ305                                                          
         CLI   GSOBKTYP,0          IF THERE IS A BOOKTYPE                       
         BE    BKSQ305                                                          
*****                                                                           
*****    GOTO1 GETBTYPE,DMCB,(GSOBKTYP,0)                                       
*****    CLI   DMCB,0                                                           
*****    BE    BKSQ305                                                          
                                                                                
         MVI   5(R3),C'('                                                       
                                                                                
*****    ZIC   R1,DMCB                                                          
*****    BCTR  R1,0                                                             
*****    EX    R1,*+8                                                           
*****    B     *+10                                                             
*****    MVC   6(0,R3),DMCB+2                                                   
                                                                                
         MVC   6(1,R3),GSOBKTYP                                                 
                                                                                
         CLI   GSOBKTYP+1,C' '     IF SECOND CH                                 
         BNH   *+14                                                             
         MVC   7(1,R3),GSOBKTYP+1     PRINT 2ND CH                              
         LA    R3,1(R3)               BUMP POINTER                              
                                                                                
         LA    R3,7(0,R3)                                                       
         MVI   0(R3),C')'                                                       
*                                                                               
BKSQ305  DS    0H                                                               
*                                                                               
         L     R5,ABUFF                                                         
         LA    R5,10(R5)                                                        
*                                                                               
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    R5,29(R5)                                                        
******                                                                          
******   CLI   MASOP4,C'S'         IF SUPPRESSING CODES                         
******   BNE   BKSQ310                                                          
******                                                                          
******   CLI   GSOQLF,C'P'            AND QUALIFIER IS P                        
******   BE    *+8                                                              
******   CLI   GSOQLF,C'E'            OR E                                      
******   BE    BKSQ311                   SKIP PRINTING CODE                     
*                                                                               
BKSQ310  DS    0H                                                               
*                                                                               
         MVC   0(2,R5),SAVECODE       PRINT SAVECODE (FROM 'DATA' RTN)          
*                                                                               
BKSQ311  DS    0H                                                               
*                                                                               
BKSQX    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*              ROUTINES FOR HANDLING THE DEMO FORMATTING                        
*              -----------------------------------------                        
*      ON ENTRY, R5 POINTS TO OUTPUT FOR DEMO VALUES                            
*                                                                               
**********************************************************************          
         SPACE 2                                                                
DAT50    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         LA    R2,DEMLST           DEMO LIST                                    
         TM    PRINTOPT,X'01'      RECALCULATED DEMOS                           
         BO    DAT55               ONLY NEED FORMATING                          
         SPACE 1                                                                
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         LA    R6,IO                                                            
         ST    R6,DBAREC                                                        
         LA    R6,34(R6)                                                        
         ST    R6,DBAQUART                                                      
*                                                                               
         LA    R1,DBEXTRA1                                                      
         STCM  R1,15,DBEXTEND                                                   
         USING DBXTTID,R1                                                       
         XC    0(128,R1),0(R1)                                                  
*                                                                               
         MVC   DBXTID(4),=C'SPOT'                                               
*                                                                               
         CLI   MASOP5,C'Y'                                                      
         BE    DAT51A                                                           
*                                                                               
         MVI   DBXTTRP,X'01'                                                    
         MVI   DBXTTSP,X'01'                                                    
         MVI   DBXTTIP,X'02'                                                    
*                                                                               
         B     DAT51B                                                           
*                                                                               
DAT51A   EQU   *                                                                
*                                                                               
         MVI   DBXTTRP,X'00'                                                    
         MVI   DBXTTSP,X'00'                                                    
         MVI   DBXTTIP,X'03'                                                    
*                                                                               
DAT51B   EQU   *                                                                
*                                                                               
         DROP  R1                                                               
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKD,(R5)                             
*                                                                               
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         DROP  R4                                                               
*                                                                               
DAT55    L     R3,ABUFF                                                         
         LA    R3,25(R3)           POINT TO PRINT LINE                          
*                                                                               
         ZIC   R4,NUMDEMS                                                       
*                                                                               
DAT60    L     R1,0(R5)                                                         
*                                                                               
         TM    PRINTOPT,X'01'      RECALCULATED DEMOS                           
         BNO   DAT65               ALWAYS NEED TO DROP DECIMAL                  
*                                                                               
         SR    R0,R0               THEN DROP DECIMAL                            
*                                                                               
         CLI   MASOP5,C'Y'         ROUNDED DEMOS?                               
         BE    DAT62               YES - DROP TWO DECIMAL POSITIONS             
*                                  NO  - ONLY DROP 1 DECIMAL                    
         LA    R1,5(R1)            HALF-ROUND ON 10                             
         D     R0,=F'10'           DIVIDE BY 10                                 
*                                                                               
         B     DAT64                                                            
*                                                                               
DAT62    EQU   *                                                                
*                                                                               
         LA    R1,50(R1)           HALF-ROUND ON 100                            
         D     R0,=F'100'          DIVIDE BY 100                                
*                                                                               
DAT64    EQU   *                                                                
*                                                                               
         ST    R1,0(R5)            STORE ROUNDED VALUE                          
*                                                                               
DAT65    EQU   *                                                                
*                                                                               
         LTR   R1,R1               IF NO VALUE                                  
         BNZ   DAT70                                                            
*                                                                               
         CLI   MASOP5,C'Y'         AND NOT ROUNDING                             
         BE    DAT70                                                            
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ       IF MORE THAN DEM#NRMQ DEMOS               
         BNH   *+12                                                             
         MVI   3(R3),C'.'                SHOW '.'                               
         B     DAT80                                                            
*                                                                               
         MVI   4(R3),C'.'          SHOW '.'                                     
*                                                                               
         B     DAT80                                                            
*                                                                               
DAT70    EQU   *                                                                
*                                                                               
         CLI   1(R2),C'U'         IF UNIVERSE NUMBER                            
         BE    DAT77                                                            
*                                                                               
         CLI   MASOP5,C'Y'                                                      
         BE    DAT75                                                            
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ                                                 
         BH    DAT74                                                            
*                                                                               
DAT72    EQU   *                                                                
*                                                                               
         EDIT  (R1),(6,0(R3)),1                                                 
*                                                                               
         B     DAT80                                                            
*                                                                               
DAT74    EQU   *                                                                
*                                                                               
         BCTR  R3,0                BACK UP A POSITION                           
         EDIT  (R1),(6,0(R3)),1                                                 
         LA    R3,1(R3)            RESTORE POINTER                              
*                                                                               
         B     DAT80                                                            
*                                                                               
DAT75    EQU   *                                                                
*                                                                               
         EDIT  (R1),(4,1(R3)),0                                                 
         B     DAT80                                                            
*                                                                               
DAT77    EDIT  (R1),(6,0(R3)),0                                                 
*                                                                               
DAT80    EQU   *                                                                
*                                                                               
         CLC   1(2,R2),=X'D701'    IF HUT                                       
         BE    DAT90                                                            
         CLC   1(2,R2),=X'E201'    OR SHARE                                     
         BNE   DAT100                                                           
DAT90    CLI   MULTSW,C'Y'         AND IT'S A COMBO                             
         BNE   DAT100                                                           
*                                                                               
         CLI   MASOP5,C'Y'                                                      
         BNE   *+14                                                             
         MVC   0(4,R3),=C'  N   '  THEN DON'T SHOW VALUE                        
         B     DAT100                                                           
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ                                                 
         BH    *+14                                                             
         MVC   0(6,R3),=C'  N   '  THEN DON'T SHOW VALUE                        
         B     DAT100                                                           
*                                                                               
         MVC   0(5,R3),=C' N   '   THEN DON'T SHOW VALUE                        
*                                                                               
DAT100   LA    R2,3(R2)            NEXT DEMO                                    
*                                                                               
         LA    R3,6(R3)            NEXT DEMO PRINTING AREA                      
*                                                                               
         CLI   MASOP5,C'Y'         SKIP IF ROUNDED DECIMALS                     
         BE    DAT110                                                           
*                                                                               
         LA    R3,2(R3)            NEXT DEMO PRINTING AREA-EXTENDED             
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ                                                 
         BH    *+8                    NO NEED FOR EXTRA ROOM                    
         LA    R3,1(R3)            NEXT DEMO PRINTING AREA-EXTENDED             
*                                                                               
DAT110   DS    0H                                                               
*                                                                               
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R4,DAT60            LOOP FOR MORE DEMOS                          
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*****************************************************************               
*                                                                               
*    BUFFFULL ROUTINE -CHECKS TO SEE THAT                                       
*                      PRINT BUFFER HASN'T GOTTEN TOO BIG                       
*                                                                               
*****************************************************************               
         SPACE 2                                                                
BUFFFULL NTR1  BASE=*,LABEL=*                                                   
         L     RE,ABUFF                                                         
         C     RE,XBUFF                                                         
         BL    BFXIT                                                            
         OI    PRNTOPT2,X'80'      BUFFER BIGGER THAN 1 PAGE (P1)               
         BRAS  RE,PRNTALL          TOO BIG, SO PRINT IT                         
         OI    PRNTOPT2,X'40'      BUFFER BIGGER THAN 1 PAGE (P2)               
         L     RE,SBUFF                                                         
         ST    RE,ABUFF            AND START AT BEGINNING AGAIN                 
BFXIT    XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                               
*     PRINT ALL BOOKS THAT BELONG TO A PIECE OF INVENTORY TOGETHER              
*     ------------------------------------------------------------              
*                  (TEXT CAN BE ON A DIFFERENT PAGE)                            
*********************************************************************           
         SPACE 2                                                                
PRNTALL  NTR1  BASE=*,LABEL=*                                                   
         SR    R2,R2                                                            
         L     RE,SBUFF                                                         
         L     R3,ABUFF                                                         
         SR    R3,RE                                                            
         LTR   R3,R3                                                            
         BP    PR5                                                              
         TM    PRNTOPT2,X'40'      WAS BUFFER MORE THAN 1 PAGE (P2)             
         BO    PRXIT               THEN MAYBE THERE'S NO MORE TO PRINT          
         DC    H'0'                ELSE, SHOULD AT LEAST BE A HEADLINE          
PR5      D     R2,=F'132'          R3=NUMBER OF LINES                           
         SPACE 1                                                                
         LR    R5,R3                                                            
         STC   R3,BYTE                                                          
         L     R2,SBUFF                                                         
         MVI   HDRSW,C'Y'                                                       
         TM    PRNTOPT2,X'40'      BUFFER MORE THAN A PAGE                      
         BZ    *+8                 THEN DON'T RESET HDRSW FOR P2                
         MVI   HDRSW,C'N'                                                       
         ZIC   R4,MAXPLINS                                                      
PR10     CR    R5,R4                                                            
         BH    PR20                                                             
         LR    R3,R5               RESET R3 (MAYBE 2ND TIME THROUGH)            
         MVI   BYTE,0                                                           
         STC   R5,ALLOWLIN                                                      
         B     PR50                                                             
         SPACE 1                                                                
PR20     MVC   ALLOWLIN,MAXPLINS                                                
         SR    R5,R4                                                            
         STC   R5,BYTE       # OF LINES TO BE PRINTED 2ND TIME THROUGH          
         LR    R3,R4                                                            
         SPACE 1                                                                
PR50     MVC   P,0(R2)                                                          
         OC    P,SPACES                                                         
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         MVI   CONTSW,C'Y'                                                      
         XC    0(132,R2),0(R2)                                                  
         LA    R2,132(R2)                                                       
         BCT   R3,PR50                                                          
         SPACE 1                                                                
         CLI   BYTE,0              MORE LINES TO PRINT                          
         BNE   PR10                                                             
         MVI   CONTSW,C'N'                                                      
PRXIT    TM    PRNTOPT2,X'80'                                                   
         BZ    *+8                                                              
         MVI   HDRSW,C'N'                                                       
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                               
*              CONTROL HANDLING OF RATIONALE                                    
*              -----------------------------                                    
*********************************************************************           
         SPACE 2                                                                
MAS300   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         MVC   KEY,KEYSAVE                                                      
         CLI   OPTION,C'I'                                                      
         BE    MAS310                                                           
         XC    KEY,KEY                                                          
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,REPPAR     USE PARENT REP                               
         MVC   RINVKSTA(5),ACTSTAT                                              
         MVC   RINVKRTP,OPTION                                                  
         B     *+8                                                              
MAS310   MVI   RINVKRTP,X'FF'      FOR OPTION I                                 
         XC    RINVKTXT,RINVKTXT                                                
         GOTO1 HIGH                                                             
         B     MAS330                                                           
         SPACE 2                                                                
MAS320   GOTO1 SEQ                                                              
         SPACE 2                                                                
MAS330   DS    0H                                                               
         CLC   KEY(RINVKBK-RINVKEY),KEYSAVE                                     
         BNE   MAS300X                                                          
         BRAS  RE,RAT              RATIONALE                                    
         B     MAS320                                                           
MAS300X  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*              ROUTINES FOR RATIONALE RECORDS                                   
*              ------------------------------                                   
**********************************************************************          
         SPACE 2                                                                
RAT      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   SVWRPOPT,0          INIT WRAP OPTION                             
         TM    RMPPROF+RMPWWRPB,RMPWWRPA   IF DEFAULT IS Y                      
         BNO   *+8                                                              
         MVI   SVWRPOPT,C'Y'                  RESET WRAP OPTION                 
*                                                                               
         OI    DMINBTS,X'08'       SET PASS DELETED RECORDS                     
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
*                                                                               
         TM    IO+29,X'80'         TEST RECORD IS DELETED                       
         BO    RATX                                                             
*                                                                               
         L     R2,ASVCLST                                                       
         GOTO1 =A(HISTDATA),DMCB,(RC),(R2),RR=RELO                              
         BRAS  RE,BUFFFULL                                                      
*                                                                               
RATTRNSX DS    0H                                                               
*                                                                               
*        CHECK SUPPRESS OPTION                                                  
*                                                                               
         CLI   OPTION,C'A'         SKIP IF AUTO TEXT                            
         BE    RSTSPRSX                                                         
*                                                                               
         TM    PRINTOPT,POPTSTXQ   IF SUPPRESSING TEXT                          
         BNO   RSTSPRSX                                                         
*                                                                               
         MVI   ELCODE,RINVTCCQ        FIND TEXT CONTROL ELEMENT                 
         LA    R6,IO                                                            
         BRAS  RE,GETEL                                                         
         BNE   RATX                   NO ELEMENT AVAILABLE                      
*                                                                               
         USING RINVTCEL,R6            ESTABLISH TEXT CONTROL ELEMENT            
*                                                                               
         TM    RINVTCN1,RINVTCFQ      PRINT IF TEXT IS FORCED                   
         BNO   RATX                   ELSE DROP                                 
*                                                                               
RSTSPRSX DS    0H                                                               
*                                                                               
RAT30    CLI   OPTION,C'A'   AUTO FOOTNOTES DON'T NEED FILTER CHECK             
         BNE   RAT40                                                            
*                                                                               
         L     R3,ABUFF            PUT AUTO FOOTNOTES TO BUFF WITH THE          
         LA    R5,28(R3)           START PRINTING AT +28 INTO LINE              
         B     RAT110              REST OF THE BOOK STUFF                       
*                                                                               
RAT40    L     R3,SBUFF       OTHER TEXT PRINTS IN A CHUNK OF ITS OWN           
         MVC   0(132,R3),SPACES                                                 
         SR    R2,R2               LINE COUNTER                                 
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'02'        IS A FILTER APPLICABLE                       
         BRAS  RE,GETEL                                                         
         BNE   RAT110                                                           
*                                                                               
         USING RINVFEL,R6                                                       
*                                                                               
         CLI   RINVFWRP,0          IF WRAP OPTION PRESENT IN ELEMENT            
         BE    *+10                                                             
         MVC   SVWRPOPT,RINVFWRP      SAVE WRAP OPTION                          
*                                                                               
         CLC   REPPAR,AGENCY       IF NOT LOCAL REP                             
         BNE   RAT45                                                            
         CLI   RINVFLOC,C'Y'       AND TEXT IS LOCAL ONLY                       
         BE    RATX                THEN DON'T WANT IT FOR MAIN REP              
*                                                                               
RAT45    CLI   RINVFSRC,0          SOURCE FILTER                                
         BE    RAT50                                                            
         CLC   SVSOURCE,RINVFSRC                                                
         BNE   RATX                                                             
         OI    PRINTOPT,X'04'      PRINT TEXT FILTER                            
         SPACE 1                                                                
RAT50    OC    RINVFBK,RINVFBK     BOOK FILTER                                  
         BZ    RAT75                                                            
         LA    R1,BOOKS                                                         
         LA    R0,6                                                             
         SPACE 1                                                                
         TM    PRINTOPT,X'10'      RANGE OF BOOKS                               
         BNO   RAT60                                                            
         CLC   RINVFBK,1(R1)       YES, PRINT ALL TEXT BTWN START               
         BL    RATX                                                             
         CLC   RINVFBK,5(R1)       AND END RANGE                                
         BH    RATX                                                             
         OI    PRINTOPT,X'04'      PRINT TEXT FILTER                            
         B     RAT75                                                            
         SPACE 1                                                                
RAT60    CLC   RINVFBK,1(R1)       YEAR/MONTH                                   
         BNE   RAT70                                                            
         SPACE 1                                                                
         MVC   INVSRC,0(R1)        SAVE BOOK TYPE                               
         CLI   RINVFSRC,0          SO MUST BOOK TYPE                            
         BE    RAT68                                                            
         SPACE 1                                                                
         L     RE,ASVCLST           MATCH SOURCE                                
RAT62    CLC   RINVFSRC(1),0(RE)                                                
         BNE   RAT64                                                            
         CLC   INVSRC(1),3(RE)                                                  
         BE    RAT68                                                            
RAT64    LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   RAT62                                                            
         B     RAT70                                                            
         SPACE 1                                                                
RAT68    CLC   INVSRC,RINVFBKT     AND BOOK TYPE                                
         BE    RAT69                                                            
         CLI   RINVFSRC,0          IF NO FILTER SOURCE                          
         BNE   RAT70                                                            
         NI    INVSRC,X'BE'        COULD BE ARB, NSI OR SRC                     
         CLC   INVSRC,RINVFBKT                                                  
         BNE   RAT70                                                            
RAT69    OI    PRINTOPT,X'04'      PRINT TEXT FILTER                            
         B     RAT75                                                            
RAT70    LA    R1,4(R1)                                                         
         BCT   R0,RAT60                                                         
         B     RATX                                                             
         SPACE 1                                                                
RAT75    ZIC   R0,RINVFLEN         DEMO FILTERS                                 
         SH    R0,=H'10'                                                        
         BNP   RAT80                                                            
         SPACE 1                                                                
         LA    R1,RINVFDEM                                                      
RAT76    LA    R4,DEMLST                                                        
         ZIC   RE,NUMDEMS                                                       
RAT77    CLC   0(1,R1),2(R4)                                                    
         BNE   RAT79                                                            
         OI    PRINTOPT,X'04'      ANY FILTERS TO PRINT                         
         B     RAT80                                                            
         SPACE 1                                                                
RAT79    LA    R4,3(R4)                                                         
         BCT   RE,RAT77                                                         
         LA    R1,1(R1)                                                         
         BCT   R0,RAT76                                                         
         B     RATX                                                             
         SPACE 1                                                                
RAT80    TM    PRINTOPT,X'04'      ANY FILTERS TO PRINT                         
******   BZ    RAT110                                                           
         B     RAT110              DON'T PRINT FILTERS                          
*                                                                               
         MVC   4(12,R3),=C'TEXT FILTERS'                                        
         LA    R4,20(R3)                                                        
         CLI   RINVFSRC,0                                                       
         BE    RAT90                                                            
         MVC   0(7,R4),=C'SOURCE='                                              
         MVC   7(1,R4),RINVFSRC                                                 
         LA    R4,11(R4)                                                        
         SPACE 1                                                                
RAT90    OC    RINVFBK,RINVFBK                                                  
         BZ    RAT95                                                            
         MVC   0(5,R4),=C'BOOK='                                                
         LA    R5,5(R4)                                                         
         CLI   RINVFBKT,0                                                       
         BE    RAT94                                                            
         L     R1,ASVCLST          CONVERSION TABLE                             
RAT92    CLC   RINVFBKT(1),3(R1)   FROM BOOKVAL FORMAT TO PRINTABLE FMT         
         BE    RAT93                                                            
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   RAT92                                                            
         DC    H'0'                                                             
RAT93    CLI   1(R1),C' '                                                       
         BE    RAT94                                                            
         MVC   0(1,R5),1(R1)                                                    
         LA    R5,1(R5)                                                         
         SPACE 1                                                                
RAT94    ZIC   R1,RINVFMN                                                       
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R5),0(R1)                                                    
         MVI   3(R5),C'/'                                                       
         EDIT  (1,RINVFYR),(2,4(R5))                                            
         LA    R4,16(R4)                                                        
         SPACE 1                                                                
RAT95    ZIC   R0,RINVFLEN         PRINT OUT TEXT DEMO FILTERS                  
         SH    R0,=H'10'                                                        
         BNP   RAT100                                                           
         MVC   0(6,R4),=C'DEMOS='                                               
         LA    R4,6(R4)                                                         
         LA    R5,DBLOCKA1                                                      
         USING DBLOCKD,R5                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
         LA    R1,RINVFDEM                                                      
RAT97    ST    R1,FULL    SAVE WHERE WE ARE IN RINVFDEM LIST                    
         LTR   R0,R0                                                            
         BZ    RAT99                                                            
         MVI   WORK,0                                                           
         MVI   WORK+1,C'I'                                                      
         MVC   WORK+2(1),0(R1)                                                  
         GOTO1 DEMOCON,PARAS,(0,WORK),(6,0(R4)),(0,DBLOCKD)                     
         DROP  R5                                                               
         LA    R4,6(R4)                                                         
RAT98    CLI   0(R4),C' '                                                       
         BNE   RAT98K                                                           
         BCTR  R4,R0                                                            
         B     RAT98                                                            
         SPACE 1                                                                
RAT98K   MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
         SH    R0,=H'1'                                                         
         L     R1,FULL                                                          
         LA    R1,1(R1)                                                         
         B     RAT97                                                            
         SPACE 1                                                                
RAT99    BCTR  R4,0                                                             
         CLI   0(R4),C','                                                       
         BNE   *+8                                                              
         MVI   0(R4),C' '                                                       
         SPACE 1                                                                
RAT100   LA    R3,132(R3)                                                       
         MVC   0(132,R3),SPACES    SPACE BTWN FILTERS & TEXT                    
         LA    R3,132(R3)                                                       
         MVC   0(132,R3),SPACES                                                 
         LA    R2,2(R2)                                                         
RAT110   DS    0H                                                               
*                                                                               
*        PRINT LINES OF TEXT                                                    
*                                                                               
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
*                                                                               
         CLI   OPTION,C'A'                                                      
         BE    RAT145                                                           
*                                                                               
         LA    R5,20(R3)           PRINT TEXT AT +20                            
         B     RAT145              SKIP PRINTING TEXT NUMBER                    
*                                                                               
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
*                                                                               
         MVC   5(4,R3),=C'TEXT'                                                 
*                                                                               
         CLI   OPTION,C'M'                                                      
         BNE   *+10                                                             
         MVC   5(4,R3),=C'MKT.'                                                 
*                                                                               
         CLI   OPTION,C'S'                                                      
         BNE   *+10                                                             
         MVC   5(4,R3),=C'STN.'                                                 
*                                                                               
         EDIT  (2,RINVKTXT),(5,10(R3)),ALIGN=LEFT                               
*                                                                               
RAT120   LA    R5,11(R3)                                                        
*                                                                               
RAT130   CLC   0(3,R5),SPACES                                                   
         BE    RAT140                                                           
*                                                                               
         LA    R5,1(R5)                                                         
         B     RAT130                                                           
*                                                                               
RAT140   LA    R5,1(R5)            LEAVE SPACE                                  
*                                                                               
         DROP  R4                                                               
*                                                                               
RAT145   DS    0H                                                               
*                                                                               
         CLI   SVWRPOPT,C'Y'       IF USING WORD WRAP                           
         BNE   *+12                                                             
         BAS   RE,RATWRP              USE SPECIAL CODE                          
         B     RAT170                 GO DO PRINTING                            
*                                                                               
         USING RINVTEL,R6                                                       
         BRAS  RE,GETEL            FIND FIRST LINE OF TEXT                      
         B     RAT160                                                           
*                                                                               
RAT160   BNE   RAT170              END OF TEXT                                  
*                                                                               
         LA    RE,RINVTEXT                                                      
         ZIC   R1,RINVTLEN                                                      
         SH    R1,=H'7'                                                         
*                                                                               
         CLI   OPTION,C'A'                                                      
         BNE   RAT166                                                           
*                                                                               
         LA    R5,28(R3)                                                        
         SH    R1,=H'6'                                                         
         LA    RE,6(RE)            REMOVE MMMYY FROM FOOTNOTE                   
*                                                                               
RAT166   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RE)                                                    
*                                                                               
         LA    R3,132(R3)                                                       
         MVC   28(104,R3),SPACES   DON'T CREAM HEADLINE STUFF THERE             
         LA    R5,132(R5)                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         BRAS  RE,NEXTEL                                                        
*                                                                               
         B     RAT160                                                           
*                                                                               
RAT170   CLI   OPTION,C'A'                                                      
         BNE   RAT173                                                           
*                                                                               
         ST    R3,ABUFF                                                         
         BRAS  RE,BUFFFULL                                                      
         B     RATX                                                             
*                                                                               
RAT173   LTR   R2,R2               ANYTHING TO PRINT                            
         BNZ   RAT176                                                           
         CLC   P(40),SPACES        HD2-HD5 TO BE PRINTED                        
         BNE   RAT190                                                           
         J     RATX                                                             
RAT176   STC   R2,ALLOWLIN         FORCES ALL TEXT TO SAME PAGE                 
         L     R3,SBUFF                                                         
         SPACE 2                                                                
RAT180   EQU   *                                                                
         MVI   CONTSW,C'Y'                                                      
         MVC   P,0(R3)                                                          
         MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)                                                 
         XC    0(132,R3),0(R3)     CLEAR OUT BUFF                               
         MVI   ALLOWLIN,0                                                       
         LA    R3,132(R3)                                                       
         BCT   R2,RAT180                                                        
         MVI   CONTSW,C'N'                                                      
*                                                                               
         CLI   OPTION,C'A'                                                      
         BE    RATX                                                             
RAT190   MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)    SPACE AFTER RATIONALE                        
RATX     DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
**********************************************************************          
*                                                                               
*        ROUTINE FOR WORD WRAPPING. TEXT IS STORED                              
*        CONSECUTIVELY AND THEN CHOPPED                                         
*        NEW LINE OCCURS WHEN NEXT ELEMENT STARTS WITH 2 SPACES                 
*                                                                               
*NTRY  R3==> FIRST PRINT LINE IN BUFFER TO BE USED                              
*      R5==> FIRST POSITION ON LINE FOR PRINTING                                
*      R2==> NUMBER OF LINES IN BUFFER                                          
***********************************************************************         
RATWRP   NTR1                                                                   
*                                                                               
         L     R4,ATXTWRK          POINT TO TEXT WORKAREA                       
*                                                                               
         BRAS  RE,GETEL            FIND FIRST LINE OF TEXT                      
*                                                                               
RWRLOOP  BNE   RWRDONE             END OF TEXT                                  
*                                                                               
         USING RINVTEL,R6          ESTABLISH TEXT ELEMENT                       
*                                                                               
         CLC   =C'  ',RINVTEXT     IF ELEMENT STARTS WITH 2 SPACES              
         BNE   RWRLP10                                                          
*                                     NEED TO CHOP WHAT WE HAVE                 
*                                                                               
         MVC   DMCB+12(4),=C'LEN='    INDICATE LENGTH BEING PASSED              
*                                                                               
         L     RF,ATXTWRK          START OF PRINT AREA                          
         SR    R4,RF               LENGTH OF TEXT TO PRINT                      
         BNP   RWRLP05             NOTHING TO PRINT                             
         ST    R4,DMCB+16          PASS LENGTH TO CHOPPER                       
*                                                                               
         GOTO1 CHOPPER,DMCB,(0,ATXTWRK),(90,(R5)),(C'P',20)                     
*                                                                               
         ICM   R1,15,DMCB+8        NUMBER OF LINES PRINTED.                     
         BNZ   *+6                 MUST HAVE PRINTED SOMETHING                  
         DC    H'0'                                                             
*                                                                               
         AR    R2,R1               UPDATE BUFFER COUNTER                        
*                                                                               
         LA    RF,132              PRINT LINE WIDTH                             
         MR    RE,R1               DISPLACEMENT USED                            
*                                                                               
         AR    R5,RF               UPDATE PRINT START                           
*                                                                               
RWRLP05  DS    0H                                                               
*                                                                               
         L     R4,ATXTWRK          RESET PRINT POINTERS                         
*                                                                               
RWRLP10  DS    0H                                                               
*                                                                               
         LA    R1,RINVTEXT         POINT TO START OF TEXT                       
         SR    RF,RF                                                            
         IC    RF,RINVTLEN         TEXT ELEMENT LENGTH                          
         SH    RF,=H'7'            TEXT'S EXECUTE LENGTH                        
         BM    RWRCONT             MUST HAVE TEXT                               
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),0(R1)       MOVE TEXT TO PRINTAREA                       
*                                                                               
RWRCONT  DS    0H                                                               
*                                                                               
         LA    R4,1(RF,R4)         NEXT PRINT AREA                              
         MVI   0(R4),C' '          PUT SPACE BETWEEN TEXT LINES                 
         LA    R4,1(R4)            BUMP POINTER                                 
*                                                                               
         BRAS  RE,NEXTEL                                                        
*                                                                               
         B     RWRLOOP                                                          
*                                                                               
RWRDONE  DS    0H                                                               
*                                                                               
*        CHOP ANY REMANING TEXT                                                 
*                                                                               
         MVC   DMCB+12(4),=C'LEN='    INDICATE LENGTH BEING PASSED              
*                                                                               
         L     RF,ATXTWRK          START OF PRINT AREA                          
         SR    R4,RF               LENGTH OF TEXT TO PRINT                      
         BZ    RWRDN10             NOTHING TO PRINT                             
         ST    R4,DMCB+16          PASS LENGTH TO CHOPPER                       
*                                                                               
         GOTO1 CHOPPER,DMCB,(0,ATXTWRK),(90,(R5)),(C'P',20)                     
*                                                                               
         ICM   R1,15,DMCB+8        NUMBER OF LINES PRINTED.                     
         BNZ   *+6                 MUST HAVE PRINTED SOMETHING                  
         DC    H'0'                                                             
*                                                                               
         AR    R2,R1               UPDATE BUFFER COUNTER                        
*                                                                               
RWRDN10  DS    0H                                                               
*                                                                               
RATWRPX  DS    0H                                                               
         XIT1  REGS=(R2)                                                        
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*  RECALCULATE SHARES FOR PA                                                    
*                                                                               
*                    (RATING X 10000)                                           
*                    ---------------   = SHARE   (ROUND IN DAT50)               
*                         LEVEL                                                 
**********************************************************************          
         SPACE 2                                                                
RECALC   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,DEMLST                                                        
         LA    R3,BLOCK1                                                        
         LA    RE,BLOCK1+100                                                    
         LA    R5,ACTDEMOS                                                      
         SPACE 1                                                                
RC30     OC    0(4,RE),0(RE)       SKIP CALC IF LEVEL IS 0                      
         BZ    RC40                                                             
         L     R1,0(R5)            RATING                                       
         MH    R1,=H'10000'                                                     
         SR    R0,R0                                                            
         D     R0,0(RE)                                                         
         ST    R1,0(R3)                                                         
         SPACE 1                                                                
RC40     LA    R3,4(R3)                                                         
         LA    R5,4(R5)                                                         
         LA    RE,4(RE)                                                         
         LA    R2,3(R2)                                                         
         CLI   0(R2),X'FF'         MORE DEMOS                                   
         BNE   RC30                                                             
RECALX   XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*              PERCENT SIGN TACKED ONTO SHARES                                  
*              -------------------------------                                  
**********************************************************************          
         SPACE 2                                                                
PERCENT  NTR1  BASE=*,LABEL=*                                                   
         L     R3,ABUFF                                                         
         LA    R3,13(R3)           FORMATTED LINE                               
         AH    R3,PRNTDISP         ALTERNATE FORMAT                             
         LA    R2,DEMLST                                                        
*                                                                               
         ZIC   R4,NUMDEMS                                                       
*                                                                               
PC100    EQU   *                   NON-ZERO VALUES ONLY                         
*                                                                               
         CLI   1(R2),C'U'          IF UNIVERSE                                  
         BE    PC140               THEN NO % SIGN                               
*                                                                               
         CLI   MASOP5,C'Y'         DIFFERENT DISPLACEMENTS IF ROUNDING          
         BNE   PC110                                                            
*                                                                               
         CLI   3(R3),C' '                                                       
         BNH   *+8                                                              
         MVI   4(R3),C'%'          INSERT PERCENT SIGN                          
*                                                                               
         B     PC140                                                            
*                                                                               
PC110    DS    0H                                                               
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ    DIFFERENT DISPS IF >DEM#NRMQ DEMOS           
         BH    PC120                                                            
*                                                                               
         CLI   4(R3),C' '                                                       
         BNH   *+8                                                              
         MVI   5(R3),C'%'          INSERT PERCENT SIGN                          
*                                                                               
         B     PC140                                                            
*                                                                               
PC120    DS    0H                  >DEM#NRMQ DEMOS                              
*                                                                               
         CLI   3(R3),C' '                                                       
         BNH   *+8                                                              
         MVI   4(R3),C'%'          INSERT PERCENT SIGN                          
*                                                                               
PC140    LA    R2,3(R2)                                                         
         LA    R3,6(R3)                                                         
*                                                                               
         CLI   MASOP5,C'Y'         DIFFERENT DISPLACEMENTS IF ROUNDING          
         BE    PC150                                                            
*                                                                               
         LA    R3,2(R3)                                                         
*                                                                               
         CLI   NUMDEMS,DEM#NRMQ    DIFFERENT DISPS IF >DEM#NRMQ DEMOS           
         BH    *+8                                                              
         LA    R3,1(R3)                                                         
*                                                                               
PC150    DS    0H                  >DEM#NRMQ DEMOS                              
*                                                                               
         BCT   R4,PC100                                                         
*                                                                               
         L     R3,ABUFF                                                         
*                                                                               
PCX      DS    0H                                                               
*                                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*              WORK OUT EFFECTIVE DEMO TYPES FOR DIFFERENT LINES                
*              -------------------------------------------------                
**********************************************************************          
                                                                                
EFFDEMS  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   LINSW,C'I'                                                       
         BE    EFFDEMSX                                                         
*                                                                               
         LA    R3,DEMLST                                                        
         ZIC   R0,NUMDEMS                                                       
*                                                                               
EFF2     MVC   WORK(1),1(R3)       COLUMN TYPE                                  
         MVC   WORK+1(1),LINSW     LINE TYPE                                    
         LA    R1,EFFTABLE                                                      
*                                                                               
EFF4     CLI   0(R1),X'FF'                                                      
         BE    EFF6                                                             
*                                                                               
         CLC   WORK(2),0(R1)                                                    
         BE    EFF6                                                             
*                                                                               
         LA    R1,3(R1)                                                         
*                                                                               
         B     EFF4                                                             
*                                                                               
EFF6     MVC   1(1,R3),2(R1)       SUBSTITUTE TYPE                              
         LA    R3,3(R3)                                                         
         BCT   R0,EFF2                                                          
*                                                                               
EFFDEMSX DS    0H                                                               
         XIT1                                                                   
*                                                                               
EFFTABLE DS    0H                                                               
         DC    C'TRR'                                                           
         DC    C'TSX'                                                           
         DC    C'TLQ'                                                           
         DC    C'RSS'                                                           
         DC    C'RLP'                                                           
         DC    C'T2S'                                                           
         DC    C'T4P'                                                           
         DC    X'FFFF',C'-'                                                     
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                               
* TSTDPT -- CHECKS IF PROGRAM HAS BEEN PRINTED ALREADY                          
*           PROGRAMS PRINT IN REQUEST ORDER                                     
*           R6 MUST POINT TO PROGRAM ELEMENT                                    
*********************************************************************           
         SPACE                                                                  
TSTDPT   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         USING RINVPEL,R6                                                       
                                                                                
         MVI   SKIPREC,0          0=USE RECORD                                  
*                                                                               
         CLI   OPTFLAG,2           IF ONLY TO APPEAR IN PRIMARY DPT             
         BNE   TSTD15                                                           
*                                                                               
         CLC   0(1,R2),RINVDP         THEN CURRENT DPT MUST = PRIMARY           
         BE    *+8                                                              
         MVI   SKIPREC,1              ELSE SET TO SKIP RECORD                   
*                                                                               
         B     TSTDX                                                            
*                                                                               
TSTD15   DS    0H                                                               
*                                                                               
         LA    R1,DPLIST          REQUEST LIST                                  
TSTD20   CLC   0(1,R1),0(R2)      CURRENT DAYPART?                              
         BE    TSTDX              USE THIS RECORD                               
*                                                                               
         LA    R3,6               # OF DPTS IN RECORD                           
         LA    RE,RINVDP          DAYPARTS CODED FOR PROGRAM                    
TSTD50   CLI   0(RE),C' '         ANY MORE DPTS?                                
         BE    TSTD80             NO                                            
*                                                                               
         CLC   0(1,R1),0(RE)                                                    
         BNE   TSTD70                                                           
         MVI   SKIPREC,1          SKIP RECORD - ALREADY PRINTED                 
         B     TSTDX                                                            
*                                                                               
TSTD70   LA    RE,1(RE)           CHECK NEXT DPT IN PROGRAM                     
         BCT   R3,TSTD50                                                        
*                                                                               
TSTD80   LA    R1,1(R1)           CHECK NEXT DPT IN REQUEST                     
         B     TSTD20                                                           
                                                                                
TSTDX    XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*              PRINT UPGRADE COMMENTS                                           
*              ----------------------                                           
**********************************************************************          
                                                                                
UPPRNT   NTR1  BASE=*,LABEL=*                                                   
         LA    R6,IO                                                            
         MVI   ELCODE,X'05'        ANY UPGRADE ELEMENT                          
         BRAS  RE,GETEL                                                         
         BNE   UPPRNTX                                                          
         CLC   SAVECODE,=C'PJ'     ONLY SHOW UPGRADE IF                         
         BNE   UPPRNTX             CODE IS PJ                                   
         L     R3,ABUFF                                                         
         LA    R2,28(R3)                                                        
         GOTO1 AUPOUT,DMCB,(R6),(R2)    EDIT UPGRADE EXPRESSION                 
         LA    R3,132(R3)                                                       
         ST    R3,ABUFF                                                         
         BRAS  RE,BUFFFULL                                                      
UPPRNTX  XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*                                                                               
*  CHKUNIV ROUTINE - CHECKS IF ANY UNIVERSE DEMOS REQUESTED                     
*                                           SET CONDITION CODE                  
*                                                                               
***********************************************************************         
CHKUNIV  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R1,DEMLST                                                        
CHKU10   CLI   0(R1),X'FF'                                                      
         BE    CHKUNO                                                           
         CLI   1(R1),C'U'                                                       
         BE    CHKUYES                                                          
         LA    R1,3(R1)                                                         
         B     CHKU10                                                           
                                                                                
CHKUYES  SR    R1,R1          SET CC TO =                                       
CHKUNO   LTR   R1,R1          SET CC TO !=                                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
*                                                                               
*   CHKDMA  ROUTINE - CHECKS IF ANY DMA IMPS REQUESTED                          
*                                          SETS CONDITION CODE                  
*                                                                               
***********************************************************************         
CHKDMA   NTR1  BASE=*,LABEL=*                                                   
                                                                                
         LA    R1,DEMLST                                                        
CHKA10   CLI   0(R1),X'FF'                                                      
         BE    CHKANO                                                           
         CLI   1(R1),C'A'     DMA IMPRESSIONS?                                  
         BE    CHKAYES                                                          
         LA    R1,3(R1)                                                         
         B     CHKA10                                                           
                                                                                
CHKAYES  SR    R1,R1          SET CC TO =                                       
CHKANO   LTR   R1,R1          SET CC TO !=                                      
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
*                                                                               
*  NEWERRS ROUTINE - ALLOWS FOR OUTPUT OF NEW ERROR MSGS WITHOUT                
*                    HOGGING ADDRESSABILITY IN MAIN ROUTINE                     
*                    SHOULD NEVER RETURN TO CALLING ROUTINE!!!!                 
*                    R1 MUST CONTAIN ERROR EQUATE                               
**********************************************************************          
NEWERRS  NTR1  BASE=*,LABEL=*                                                   
                                                                                
         XC    CONHEAD,CONHEAD    ENSURE CLEAR FOR OUTPUT                       
                                                                                
         CHI   R1,UNIVERR1                                                      
         BNE   NEWERR10                                                         
         MVC   CONHEAD(L'UDEMLIM),UDEMLIM                                       
         B     NEWERRX                                                          
                                                                                
NEWERR10 CHI   R1,UNIVERR2                                                      
         BNE   NEWERR20                                                         
         MVC   CONHEAD(L'UROUND),UROUND                                         
         B     NEWERRX                                                          
                                                                                
NEWERR20 CHI   R1,DMAERROR                                                      
         BNE   NEWERR30                                                         
         MVC   CONHEAD(L'DMAREQ),DMAREQ                                         
         B     NEWERRX                                                          
                                                                                
NEWERR30 DC    H'0'               INVALID ERROR EQUATE IN R1                    
                                                                                
                                                                                
NEWERRX  MVI   ERROR,X'FE'        DISPLAYING MY OWN MESSAGE                     
         GOTO1 VERRXIT                                                          
                                                                                
         DC    H'0'               SHOULD NEVER GET HERE                         
         EJECT                                                                  
                                                                                
*        NEW ERROR MESSAGES                                                     
UDEMLIM  DC    C'DEMO REQUEST LIMIT IS 12 IF UNIVERSES INCLUDED'                
UROUND   DC    C'UNIVERSES NOT COMPATIBLE WITH ROUNDED DEMOS OPTION'            
DMAREQ   DC    C'DMA IMPRESSIONS NOT SUPPORTED HERE'                            
                                                                                
*        NEW ERROR EQUATES                                                      
UNIVERR1 EQU   1                                                                
UNIVERR2 EQU   2                                                                
DMAERROR EQU   3                                                                
                                                                                
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                               
*             REPORT HEADLINE SPECS                                             
*                                                                               
*********************************************************************           
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,052,C'    GENERAL AVAILS LISTING'                             
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,052,C'    ______________________'                             
         PSPEC H2,099,RUN                                                       
         PSPEC H3,001,C'STATION -'                                              
         PSPEC H3,099,C'SERVICE -'                                              
         PSPEC H4,001,C'RATE CARD -'                                            
         PSPEC H4,056,C'DAYPART -'                                              
         SPACE 1                                                                
         DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
*********************************************************************           
*                                                                               
*        SVCLST                                                                 
*                                                                               
*********************************************************************           
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
*********************************************************************           
*                                                                               
*        GETEL ROUTINE                                                          
*                                                                               
*********************************************************************           
         GETEL (R6),34,ELCODE                                                   
*                                                                               
         EJECT                                                                  
                                                                                
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
*********************************************************************           
*                                                                               
*        DSECT TO COVER SCREEN                                                  
*                                                                               
*********************************************************************           
         ORG   CONTAGH                                                          
       ++INCLUDE RERESFED                                                       
         SPACE 1                                                                
* SAVE AREA FIELDS FOLLOWING SCREEN                                             
*                                                                               
SVH7     DS    CL132               SAVE DEMO HEADLINES (H7)                     
SVH8     DS    CL132               SAVE DEMO HEADLINES (H8)                     
SVH9     DS    CL132               SAVE DEMO HEADLINES (H9)                     
*                                                                               
HD1      DS    CL40                INV   DAY TIME   DAYPART                     
HD2      DS    CL40                PROGRAM#1                                    
HD3      DS    CL40                PROGRAM#2                                    
HD4      DS    CL40                PROGRAM#3                                    
HD5      DS    CL40                EFFECTIVE                                    
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS-RATENTD'                  
***********************************************************************         
*                                                                     *         
*        DSECT FOR ENTRY IN RATE TABLE                                *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RATENTD  DSECT                                                                  
RATRTCD  DS    XL8          RATE CODE                                           
RATYEAR  DS    XL1          YEAR - BINARY                                       
RATSLN   DS    XL2          SPOT LENGTH                                         
RATQWRTE DS    52XL11       QTR / WEEK / COST / END DATE                        
*                           (1) / (3)  / (4)  /  (3)     = 11                   
*                                                                               
RATENTL  EQU   *-RATENTD    LENGTH OF ENTRY IN TABLE                            
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS-PRRLINED'                 
***********************************************************************         
*                                                                     *         
*        DSECT FOR CPP/CPM LINE                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
PRRLINED DSECT                                                                  
         DS    CL1                                                              
PRREFFST DS    CL8                 START DATE                                   
PRRHYPH  DS    CL1                 - SEPARATE START AND END                     
PRREFFEN DS    CL8                 END DATE                                     
         DS    CL2                                                              
PRRLEN   DS    CL4                 SPOT LENGTH                                  
         DS    CL2                                                              
PRRCPP   DS    10CL9               CPP/CPM                                      
         DS    CL2                                                              
PRRCST   DS    CL9                 COST                                         
PRRLINEL EQU   *-PRRLINED          LENGTH OF PRINT LINE                         
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS-TTLDATAD'                 
***********************************************************************         
*                                                                     *         
*        DSECT FOR DATA PASSED TO TITLES SCREEN                       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
TTLDATAD DSECT                                                                  
TTLSTNL  DS    XL1          STATION LENGTH                                      
TTLSTN   DS    CL8          STATION                                             
TTLDPTL  DS    XL1          DAYPART LENGTH                                      
TTLDPT   DS    CL19         DAYPART                                             
TTLESDL  DS    XL1          EFFECTIVE START DATE LENGTH                         
TTLESD   DS    CL9          EFFECTIVE START DATE                                
TTLEEDL  DS    XL1          EFFECTIVE END DATE LENGTH                           
TTLEED   DS    CL9          EFFECTIVE END   DATE                                
TTLFTRL  DS    XL1          FILTERS LENGTH                                      
TTLFTR   DS    CL7          FILTERS                                             
TTLOP3L  DS    XL1          NO DUPLICATES OPTION LENGTH                         
TTLOP3   DS    CL2          NO DUPLICATES OPTION                                
*                                                                               
                                                                                
TTLDATAL EQU   *-TTLDATAD          LENGTH OF PASSED DATA                        
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS-SYSSPARE'                 
***********************************************************************         
*                                                                     *         
*        SAVE AREA VALUES                                             *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
*                                                                               
*        NUMBER OF DEMOS PER LINE LIMITS                                        
*                                                                               
DEM#NRMQ EQU   10                  NORMAL NUMBER - 12 PER LINE                  
DEM#MAXQ EQU   12                  MAX DEMOS     - 14 PER LINE                  
DEM#RNDQ EQU   16                  ROUNDED DEMOS - 18 PER LINE                  
*                                                                               
SAVESTAK DS    2F                                                               
*                                                                               
PRNTDISP DS    H                   ALTERNATE FORMAT (18 DEMO MAX.)              
*                                   PRINT LINE DISPLACEMENT                     
HDLINE   DS    XL1                 CURRENT LINE (HD1-HD5)                       
*                                                                               
SBK      DS    CL4                 START BOOK IN RANGE                          
EBK      DS    CL4                 END BOOK IN RANGE                            
*                                                                               
DPLIST   DS    CL20                DAYPART LIST                                 
*                                                                               
PRINTOPT DS    XL1                 X'80'  SUPPRESS TEXT                         
POPTSTXQ EQU   X'80'               X'80'  SUPPRESS TEXT                         
POPTSHRQ EQU   X'40'               X'40'  PRINT SHARES AND H/P/T                
POPT1DPQ EQU   X'20'               X'20'  1 DPT/DAY/INV                         
POPTRBKQ EQU   X'10'               X'10'  RANGE OF BOOKS                        
POPTFLTQ EQU   X'08'               X'08'  FILTERS SPECIFIED                     
POPTPTFQ EQU   X'04'               X'04'  TEXT FILTERS TO PRINT                 
POPTUPGQ EQU   X'02'               X'02'  UPGRADE EXISTS                        
POPTRDMQ EQU   X'01'               X'01'  RECALCULATE DEMOS                     
*                                                                               
PRNTOPT2 DS    XL1                 X'80'  BUFFER MORE THAN 1 PAGE (P1)          
*                                  X'40'  BUFFER MORE THAN 1 PAGE (P2)          
DAYOPT   DS    CL1                                                              
*                                                                               
ESDT     DS    XL2                 EFFECTIVE START DATE (COMPRESSED)            
EEDT     DS    XL2                 EFFECTIVE END DATE (COMPRESSED)              
*                                                                               
ADT      DS    CL3                 ACTIVITY DATE (YMD BINARY)                   
SVWRPOPT DS    CL1                 'Y' - WRAP TEXT LINES                        
INVNO    DS    CL4                 INVENTORY FILTER                             
CSET     DS    CL1                 NOT USED                                     
SAVEKSRC DS    CL1                 SAVE RINVKSRC                                
XTODAY   DS    XL2                 TODAY'S DATE (COMPRESSED)                    
SAVECODE DS    CL2                 RINVCODE SAVED FOR PRINTING                  
CONTINUE DS    CL16                INV# CONTINUATION LEGEND                     
NUMFILT  DS    XL1                 NUMBER OF FILTER CODES                       
*                                                                               
HDRSW    DS    CL1                 HDR SWITCH                                   
LINSW    DS    CL1                 LINE SWITCH                                  
MULTSW   DS    CL1                 COMBO SWITCH                                 
CONTSW   DS    CL1                 PRINT CONTINUED MESSAGE IN HOOK SW           
*                                                                               
DEMLST   DS    CL73                DEMO LIST...ALLOW 24 DEMOS + X'FF'           
SVDEMLST DS    CL73                DEMO LIST...SAVE AREA                        
*                                                                               
REPPAR   DS    CL2                 PARENT REP                                   
SBUFF    DS    A                   START OF BUFFER                              
ABUFF    DS    A                   POINTER TO NEXT LINE IN BUFF                 
XBUFF    DS    A                   END OF 1 FULL PAGE IN BUFF                   
AUPOUT   DS    A                   REUPOUT                                      
SVLST    DS    CL10                VALID ARB, NSI OR SRC SOURCE CODES           
*                                                                               
*                                                                               
*        INPUT CONTROL BLOCK FOR GETKSRC                                        
*                                                                               
GSRCIN   DS    0C                  GETKSRC INPUT BLOCK                          
GSIRSVC  DS    CL1                 RATING SERVICE                               
GSIQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSIBITS  DS    XL1                 BOOKVAL BITS                                 
GSIBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                                                              
GSRCINL  EQU   *-GSRCIN            INPUT BLOCK LENGTH                           
*                                                                               
*        OUTPUT CONTROL BLOCK FOR GETKSRC                                       
*                                                                               
GSRCOUT  DS    0C                  GETKSRC OUTPUT BLOCK                         
GSORSVC  DS    CL1                 RATING SERVICE                               
GSOQLF   DS    CL1                 TRACK QUALIFIER/BK PREFIX                    
GSOBITS  DS    XL1                 BOOKVAL BITS                                 
GSOBKTYP DS    CL1                 BOOKTYPE                                     
         DS    XL1                                                              
GSRCOUTL EQU   *-GSRCOUT           OUTPUT BLOCK LENGTH                          
*                                                                               
INVSRC   DS    XL1                 1ST BYTE FROM BOOKVAL                        
ACTDEMOS DS    CL96                24 DEMO VALUES (4 BYTES EACH)                
MAXPLINS DS    XL1                 MAX NO. PRINT LINES (NOT HEADS)              
NOREQDPT DS    XL1                 # OF REQUESTED DAYPARTS                      
SKIPREC  DS    XL1                 FLAG TO SKIP RECORD OR NOT                   
OPTFLAG  DS    XL1                 FLAG INDICATE Y/N TO OPTION 3                
*                                                                               
ASVSLN   DS    A                   POSITION IN SPOT TABLE                       
ASVQTR   DS    A                   POSITION IN YEAR/QTR TABLE                   
*                                                                               
TEMPWK1  DS    XL8                 WEEK AFTER 1ST DATCON (MMMDD/YY)             
TEMPWK2  DS    XL8                 WEEK AFTER DATVAL (EBCDIC YYMMDD)            
*                                                                               
SVRTCD   DS    CL8                 RATE CODE SAVEAREA                           
SVYSQ    DS    21XL4               YEAR/SPOT LENGTH/QTRS TABLE                  
SVYSQN   DS    XL1                 NUMBER OF ENTRIES IN TABLE                   
SVSLN    DS    21XL2               SPOT LENGTHS         TABLE                   
SVSLNN   DS    XL1                 NUMBER OF ENTRIES IN TABLE                   
SVQTR    DS    21XL2               YEAR/QTRS            TABLE                   
SVQTRN   DS    XL1                 NUMBER OF ENTRIES IN TABLE                   
SVRATEN  DS    XL1                 NUMBER OF SAVED RATES                        
SVDEMPTR DS    A                   A(DEMO VALUES)                               
*                                                                               
STAMP    DS    CL8                 WORKAREA PROGRAM STAMP                       
*                                                                               
DPMENU   DS    CL4                 DAYPART MENU CODE                            
*                                                                               
RATESW   DS    XL1                 X'80' FIRST BOOK'S RATES PRINTED             
*                                                                               
SAVELN   EQU   *-SYSSPARE          SAVE AREA LENGTH                             
MYBASE   DS    A                                                                
ASTACK   DS    A                                                                
ATXTWRK  DS    A                                                                
ADPTBL   DS    A                                                                
RELO     DS    A                                                                
ASVCLST  DS    A                                                                
*                                                                               
RNGLST   DS    CL256               LIST OF BOOKS WITHIN RANGE                   
RNGLST2  DS    CL256                  9 BYTES X 56 BOOKS MAX                    
*                                        1 BYTE  BOOKVAL BITS                   
*                                        2 BYTES RINVKBK                        
*                                        1 BYTE  BOOK TYPE                      
*                                        1 BYTE  RINVKRSR                       
*                                        4 BYTES DISK ADDRESS                   
RNGLSTND DS    CL1                 END OF TABLE                                 
DPTBL    DS    XL(24*DPTBLL)       DAYPART TABLE                                
*                                                                               
*&&DO                                                                           
SVRATE   DS    51XL(RATENTL)       SAVED RATES FOR REPORT                       
*&&                                                                             
         DS    CL(L'SYSSPARE-(*-SYSSPARE)) SPARE                                
*                                                                               
         ORG   BUFF+7000                                                        
TXTWRK   DS    XL(15*61)           TEXT WORKAREA                                
*                                                                               
         DS    0D                                                               
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS-DPTBL'                    
***********************************************************************         
*                                                                     *         
*        DSECT FOR TABLE OF DAYPART CODES AND NAMES                   *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
DPTBLD   DSECT                                                                  
DPTBCODE DS    CL(L'RRDPCODE)      DAYPART CODE                                 
DPTBSNAM DS    CL(L'RRDPSNAM)      DAYPART SHORT NAME                           
DPTBLNAM DS    CL(L'RRDPLNAM)      DAYPART LONG NAME                            
DPTBLL   EQU   *-DPTBLD            LENGTH OF TABLE ENTRY                        
*                                                                               
         TITLE 'T8190E --- RERES0E --- GENERAL AVAILS-INCLUDES'                 
***********************************************************************         
*                                                                     *         
*        OTHER INCLUDED BOOKS                                         *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
RESETRECD      DSECT                                                            
*REGENSET                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENSET                                                       
         PRINT ON                                                               
RERDPRECD      DSECT                                                            
*REGENRDP                                                                       
         PRINT OFF                                                              
       ++INCLUDE REGENRDP                                                       
         PRINT ON                                                               
*REDDEQUS                                                                       
         PRINT OFF                                                              
       ++INCLUDE REDDEQUS                                                       
         PRINT ON                                                               
*GERFPIOD                                                                       
         PRINT OFF                                                              
       ++INCLUDE GERFPIOD                                                       
         PRINT ON                                                               
*FATIOB                                                                         
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
*DDGLOBEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
*DDGLVXCTLD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
*RERMPPROF                                                                      
         PRINT OFF                                                              
       ++INCLUDE RERMPPROF                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'160RERES0E   04/14/16'                                      
         END                                                                    
