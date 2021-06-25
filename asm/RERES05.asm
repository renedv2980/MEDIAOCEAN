*          DATA SET RERES05    AT LEVEL 183 AS OF 04/24/09                      
*PHASE T81905C                                                                  
*INCLUDE UPOUT                                                                  
*INCLUDE KHDUMMY                                                                
         TITLE 'T81905 --- RERES05 --- INVENTORY MASTER'                        
*                                                                               
********************************************************************            
*                                                                  *            
*     RERES05 (T81905) --- INVENTORY MASTER                        *            
*                                                                  *            
*------------------------------------------------------------------*            
* UPDATE HISTORY:                                                  *            
*                                                                  *            
* 8/4/89 (SNS) ALLOW OPTION TO SUPRESS MULTIPLE DAYPARTS           *            
*                                                                  *            
* AUG09/89 (SNS) ALLOW OPTION TO TRANSFER INFORMATION              *            
*                                                                  *            
* FEB26/90 (MRR) --- 1) IF REQUEST CAN'T RUN NOW, SOON IS AS GOOD  *            
*                       AS OVERNIGHT                               *            
*                    2) SKIP SINGLE DAYPART/SSU FOR DDS TERMINALS  *            
*                    3) MOVE A NUMBER OF ROUTINES TO NMOD          *            
*                                                                  *            
* AUG02/90 (MRR) --- 1) USE ALTERNATE DAYPART NAME TABLE IFF       *            
*                       REP = 'NB' (NBC)                           *            
*                    2) CHANGE LABELS FOR INV X'92' KEY AS PER     *            
*                       REGENINV DSECT CHANGES                     *            
*                                                                  *            
* SEP25/90 (MRR) --- >INSTALL OPTION FOR 1 DECIMAL PRINTING        *            
*                    >CHANGE HEADLINE TO STANDARD                  *            
*                    >COL WIDTH 5>7 AND #DEMS 19>17, MENU 24>17    *            
*                                                                  *            
* APR01/91 (MRR) --- >MULTSW PRINTING OF A 'N' LEFT 1 NUMBER SHOWNG*            
*                    >HOME SHARE WAS NOT SUPPRESSED                *            
*                                                                  *            
* JAN06/92 (MRR) --- >'FIX' CONTINUED MESSAGE                      *            
*                                                                  *            
* JUL01/92 (BU ) --- >RANGE TEST:  IF TABLE FULL, DON'T ABORT.     *            
*                     WILL DISPLAY 30 ITEMS, REST ARE SKIPPED.     *            
*                                                                  *            
* AUG28/92 (BU ) --- >FIX STATION MARKET NAME DISPLAY PROBLEM      *            
*                                                                  *            
* MAY03/94 (BU ) --- >FIX STACKED FORMAT SHARE CALCULATION.        *            
*                                                                  *            
* JUL12/01 (BU ) --- >CHANGES TO 'DUMMY' WORKSPACE USE.            *            
*                                                                  *            
* APR24/09 (SMY) --- >SUPPORT NEW INVENTORY KEY                    *            
*                                                                  *            
*                                                                  *            
*                                                                  *            
********************************************************************            
*                                                                               
         SPACE                                                                  
T81905   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1905**,RA,RR=R2                                              
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
         LH    RF,=Y(BUFF-SYSD)                                                 
         LA    RF,SYSD(RF)                                                      
         ST    RF,SBUFF                                                         
*                                                                               
         CLC   AGENCY(2),=C'NB'                                                 
         BNE   MAIN10                                                           
         L     RF,=A(DPTBL2)                                                    
         AR    RF,R2               RELO STILL IN R2                             
         ST    RF,ADPTBL                                                        
         B     MAIN20                                                           
MAIN10   EQU   *                                                                
         L     RF,=A(DPTBL1)                                                    
         AR    RF,R2               RELO STILL IN R2                             
         ST    RF,ADPTBL                                                        
MAIN20   EQU   *                                                                
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VKEY                                                             
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PREP                                                             
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*        VALIDATE KEY                                                           
*                                                                               
VKEY     MVI   PRINTOPT,X'00'      CLEAR OUT PRINTOPT                           
         LA    RF,SAVELN                                                        
         LA    RE,SYSSPARE         INIT SAVEAREA                                
         XCEF                                                                   
         MVC   STAMP,=CL8'T81905'  STAMP SAVEAREA                               
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *           SOURCE               *           
*                                  *                                *           
*                                  **********************************           
         LA    R2,MASSRCEH         VALIDATE SOURCE                              
         GOTO1 VVALSRC                                                          
         SPACE 1                                                                
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
         BL    ERREND                                                           
         SPACE 1                                                                
         CLI   1(R4),0             1 FIELD MAY BE RANGE                         
         BE    BK10                OR JUST 1 BOOK                               
         SPACE 1                                                                
         OI    PRINTOPT,X'10'      INDICATE RANGE OF BOOKS                      
         ZIC   R5,0(R4)            LENGTH OF 1ST HALF OF FIELD                  
         LA    R5,8(R2,R5)                                                      
         CLI   0(R5),C'-'                                                       
         BNE   ERREND                                                           
         MVI   0(R5),C','          DUMMY FOR BOOKVAL (RESTORED BELOW)           
         SPACE 1                                                                
BK10     MVI   MAX,10              DUMMY SO I CAN DO ERROR MSG                  
         GOTO1 VVALBOOK                                                         
         MVC   NUMBOOK,ACTUAL      SAVE NUMBER OF BOOKS                         
         CLI   ACTUAL,6            REALLY ONLY 6 BOOKS ALLOWED                  
         BNH   BK20                                                             
         MVC   CONHEAD(L'MANYBKS),MANYBKS    TOO MANY BOOKS                     
         B     MYEND                                                            
         SPACE 1                                                                
BK20     TM    PRINTOPT,X'10'      RANGE OF BOOKS CAN'T HAVE                    
         BZ    BK50                                                             
         MVI   0(R5),C'-'          (RESTORE HYPHEN)                             
         TM    BOOK+0,X'FF'-X'41'  SPECIAL BOOK FILTER                          
         BNZ   BK30                                                             
         TM    BOOK+4,X'FF'-X'41'                                               
         BZ    BK40                                                             
BK30     MVC   CONHEAD(L'BADBK1),BADBK1                                         
         B     MYEND                                                            
         SPACE 1                                                                
BK40     CLC   BOOK+1(2),BOOK+5    START CANNOT BE GREATER THAN END             
         BNH   BK50                                                             
         MVC   CONHEAD(L'BADBK3),BADBK3                                         
         B     MYEND                                                            
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
BK90     MVC   CONHEAD(L'BADBK2),BADBK2                                         
         B     MYEND                                                            
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *           STATION              *           
*                                  *                                *           
*                                  **********************************           
STA00    LA    R2,MASSTNH          VALIDATE STATION                             
         MVI   STATSW,C'I'         STATION SWITCH - ALLOW INVALID STA           
         GOTO1 VVALSTA                                                          
         CLI   STATSW,C'I'         WAS STATION INVALID                          
         BE    STA02                                                            
*                                                                               
         MVC   STATSV(5),ACTSTAT                                                
         GOTO1 VVALMKT             GET MARKET NAME                              
* DON'T USE DEMO MARKET NAME                                                    
*******  MVC   MKTSV(29),WORK+8                                                 
* GET IT NOW FROM STATION RECORD                                                
STA02    EQU   *                                                                
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
STA05    MVI   KEY+26,C' '                                                      
STA10    GOTO1 HIGH                                                             
         MVI   ERROR,112          STATION RECORD NOT ON FILE                    
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   DEMO00                                                           
         MVC   MKTSV(20),2(R6)    MARKET NAME                                   
         EJECT                                                                  
*                                  **********************************           
*                                  *                                *           
*                                  *     DEMOS (& DEMO MENU)        *           
*                                  *                                *           
*                                  **********************************           
DEMO00   LA    R2,MASDEMH          VALIDATE DEMOS                               
         MVI   ERROR,2             INVALID INPUT FIELD                          
         CLC   8(3,R2),=C'ALL'     DEFAULT MENU                                 
         BE    DEM10                                                            
         CLC   8(2,R2),=C'M='      OR SPECIFIC MENU                             
         BNE   DEM50                                                            
         CLI   5(R2),4             MENU IS 2 A/N CHARACTERS                     
         BE    DEM10                                                            
         MVC   CONHEAD(L'BADMENU),BADMENU                                       
         B     MYEND                                                            
         SPACE 1                                                                
DEM10    XC    KEY,KEY             VALIDATE DEMO MENU                           
         MVI   KEY,X'23'                                                        
         MVC   KEY+23(2),AGENCY    USE MAIN REP (NOT PARENT REP)                
         MVC   KEY+25(2),=C'ZZ'    DEFAULT MENU                                 
         CLC   8(3,R2),=C'ALL'                                                  
         BE    *+10                                                             
         MVC   KEY+25(2),10(R2)    OR CHOSEN MENU                               
         GOTO1 READ                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   ERREND                                                           
         GOTO1 GETREC                                                           
         LA    R6,IO                                                            
         USING RDEMREC,R6                                                       
         MVC   NUMDEMS,RDEMNUM     SAVE NUMBER OF DEMOS                         
         DROP  R6                                                               
         CLI   NUMDEMS,17                                                       
         BNH   DEM20                                                            
         MVI   NUMDEMS,17                                                       
DEM20    EQU   *                                                                
         MVI   ELCODE,X'02'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RDEMDEL,R6                                                       
         MVC   DEMLST,RDEMDEM      SAVE DEMOS                                   
         B     DPT                                                              
         DROP  R6                                                               
DEM50    MVI   MAX,19              DUMMY SO I CAN DO ERROR MSG                  
         MVI   NFLDS,2                                                          
         GOTO1 VVALDEM                                                          
         CLI   ACTUAL,17           REALLY ALLOW 17 DEMOS HERE                   
         BNH   DEM60                                                            
         MVC   CONHEAD(L'MANYDEM),MANYDEM                                       
         B     MYEND                                                            
         SPACE 1                                                                
DEM60    MVC   NUMDEMS,ACTUAL      SAVE NUMBER OF DEMOS                         
         MVC   DEMLST,DEMOS                                                     
         EJECT                                                                  
*                                  **********************************           
*                                  *                                *           
*                                  *          DAYPART(S)            *           
*                                  *                                *           
*                                  **********************************           
DPT      LA    R2,MASDPTH          VALIDATE DAYPART                             
         XC    DPLIST,DPLIST                                                    
         CLI   5(R2),0             CAN BE BLANK IF INV # SELECTED               
         BE    DPT5                                                             
         CLC   8(3,R2),=C'ALL'                                                  
         BNE   *+14                                                             
DPT5     MVC   DPLIST(L'ALLDPT),ALLDPT                                          
         B     DAY                                                              
         CLI   5(R2),1                                                          
         BNE   *+8                                                              
         OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
         SPACE 1                                                                
         MVI   ERROR,2             INVALID INPUT FIELD                          
         LA    R5,8(R2)                                                         
         ZIC   R6,5(R2)                                                         
         STC   R6,NOREQDPT        # OF REQUESTED DAYPARTS                       
DPT10    L     R4,ADPTBL           MAKE SURE INPUT IS IN TABLE                  
DPT20    CLI   0(R4),X'FF'                                                      
         BE    ERREND                                                           
         CLC   0(1,R5),0(R4)                                                    
         BE    DPT30                                                            
         LA    R4,L'DPTBL1(R4)                                                  
         B     DPT20                                                            
DPT30    LA    R5,1(R5)                                                         
         BCT   R6,DPT10                                                         
         ZIC   R6,5(R2)                                                         
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     DAY                                                              
         MVC   DPLIST(0),8(R2)                                                  
         EJECT                                                                  
*                                  **********************************           
*                                  *                                *           
*                                  *               DAY              *           
*                                  *                                *           
*                                  **********************************           
DAY      LA    R2,MASDAYH          DAY (OPTIONAL)                               
         MVI   DAYOPT,X'FF'        DAYOPT COMPARED TO RINVKDAY                  
         CLI   5(R2),0                                                          
         BE    FTR                                                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    FTR                                                              
*                                                                               
***   VALIDATE DAY - RETURNS DAY NUMBER IN ACTUAL                               
*                                                                               
         GOTO1 ANY                                                              
         LA    R3,MDAYTBL                                                       
DAY10    CLC   0(3,R3),8(R2)                                                    
         BNE   DAY20                                                            
         MVC   ACTUAL(1),3(R3)                                                  
         B     DAY90                                                            
DAY20    LA    R3,4(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   DAY10                                                            
         MVC   CONHEAD(L'INVDAY2),INVDAY2                                       
         B     MYEND                                                            
         SPACE 1                                                                
MDAYTBL  DC    C'M-F0MON1TUE2WED3THU4FRI5SAT6SUN7M-S8VAR9'                      
         DC    X'FF'                                                            
INVDAY2  DC    C'* ERROR * INVALID DAY'                                         
DAY90    DS    0H                                                               
*                                                                               
         OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
         MVC   DAYOPT,ACTUAL                                                    
*                                                                               
*                                  **********************************           
*                                  *                                *           
*                                  *             FILTER             *           
*                                  *                                *           
*                                  **********************************           
FTR      LA    R2,MASFTRH          FILTER (OPTIONAL)                            
         CLI   5(R2),0                                                          
         BE    INV                                                              
         OI    PRINTOPT,X'08'      INDICATE FILTERS SELECTED                    
         MVC   NUMFILT,5(R2)       SAVE NUMBER OF FILTERS                       
         CLI   5(R2),6             CAN HAVE UP TO 6                             
         BNH   INV                                                              
         MVC   CONHEAD(L'MANYFLT),MANYFLT                                       
         B     MYEND                                                            
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *        INVENTORY NUMBER        *           
*                                  *                                *           
*                                  **********************************           
INV      LA    R2,MASINVH          INVENTORY NUMBER (OPTIONAL)                  
         CLI   5(R2),0                                                          
         BNE   INV30                                                            
         CLI   MASDPTH+5,0         WAS DAYPART FIELD BLANK                      
         BNE   OP1                                                              
         LA    R2,MASDPTH                                                       
         MVI   ERROR,1             MISSING INPUT                                
         B     ERREND                                                           
         SPACE 1                                                                
INV30    MVI   WORK+2,X'F0'        VALINV DOESN'T DO THIS FOR US                
         GOTO1 VVALINV                                                          
         MVC   INVNO(3),WORK       SAVE INVENTORY NUMBER                        
         OI    PRINTOPT,X'20'      1 DPT/DAY/INV                                
         EJECT                                                                  
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
         BNE   ERREND                                                           
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
         MVC   CONHEAD(L'BADSHARE),BADSHARE                                     
         B     MYEND                                                            
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *        PRINT TEXT OPTION       *           
*                                  *                                *           
*                                  **********************************           
OP2      LA    R2,MASOP2H          TEXT (OPTIONAL)                              
         MVI   ERROR,2             INVALID INPUT                                
         CLI   5(R2),0             DEFAULT YES                                  
         BE    OPT3                                                             
         CLI   8(R2),C'Y'                                                       
         BE    OPT3                                                             
         SPACE 1                                                                
OP220    CLI   8(R2),C'N'                                                       
         BNE   ERREND                                                           
         OI    PRINTOPT,X'80'      SUPPRESS TEXT                                
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
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         MVI   OPTFLAG,1                                                        
         SPACE                                                                  
*                                  **********************************           
*                                  *     TRANSFER INFORMATION       *           
*                                  **********************************           
OPT4     LA    R2,MASOP4H                                                       
         CLI   5(R2),0                                                          
         BE    OPT5                                                             
         CLI   8(R2),C'N'                                                       
         BE    OPT5                                                             
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         SPACE                                                                  
*                                  **********************************           
*                                  *     ROUNDED DEMOS?             *           
*                                  **********************************           
OPT5     LA    R2,MASOP5H                                                       
         CLI   5(R2),0                                                          
         BE    ESDT10                                                           
         CLI   8(R2),C'N'                                                       
         BE    ESDT10                                                           
         CLI   8(R2),C'Y'                                                       
         BNE   ERREND                                                           
         SPACE                                                                  
*                                  **********************************           
*                                  *                                *           
*                                  *     EFFECTIVE START DATE       *           
*                                  *                                *           
*                                  **********************************           
ESDT10   LA    R2,MASESDTH          EFFECTIVE START DATE (OPTIONAL)             
         CLI   5(R2),0                                                          
         BE    EEDT10                                                           
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,ESDT)   ESDT=2 BYTE COMPRESSED           
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *       EFFECTIVE END DATE       *           
*                                  *                                *           
*                                  **********************************           
EEDT10   LA    R2,MASEEDTH          EFFECTIVE END DATE (OPTIONAL)               
         MVC   EEDT,=X'FFFF'                                                    
         CLI   5(R2),0                                                          
         BE    ADT10                                                            
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(2,EEDT)   EEDT=2 BYTE COMPRESSED           
*                                                                               
         CLC   ESDT,EEDT           START CANNOT BE GREATER THAN END             
         BH    ERREND                                                           
         SPACE 1                                                                
*                                  **********************************           
*                                  *                                *           
*                                  *      ACTIVITY START DATE       *           
*                                  *                                *           
*                                  **********************************           
ADT10    LA    R2,MASADTH          ACTIVITY START DATE (OPTIONAL)               
         CLI   5(R2),0                                                          
         BE    VKEYX                                                            
         GOTO1 VVALDATE                                                         
         GOTO1 DATCON,DMCB,(0,WORK),(3,ADT)   ADT=YMD BINARY                    
         SPACE 1                                                                
VKEYX    B     XIT                                                              
         EJECT                                                                  
*              PRINT DATA TYPE REPORT                                           
*              ----------------------                                           
         SPACE 2                                                                
PREP     L     R1,=A(HEDSPECS)                                                  
         A     R1,RELO                                                          
         ST    R1,SPECS                                                         
         LA    R1,HOOK                                                          
         ST    R1,HEADHOOK                                                      
         L     R1,=V(UPOUT)                                                     
         A     R1,RELO                                                          
         ST    R1,AUPOUT                                                        
         SPACE 1                                                                
         CLC   STAMP,=CL8'T81905'  STOP IF STORAGE NOT STAMPED                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   MAXPLINS,X'2F'      47 PRINT LINES FIT ON A PAGE                 
         SPACE 1                                                                
         L     R1,SBUFF                                                         
         ZIC   RE,MAXPLINS                                                      
         MH    RE,=H'132'                                                       
         LA    R1,0(RE,R1)                                                      
         ST    R1,XBUFF            SAVE ADDRESS OF END OF 1 FULL PAGE           
         SPACE 1                                                                
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
         BAS   RE,GETEL                                                         
         USING RREPELEM,R6                                                      
         MVC   REPPAR,RREPPAR      SAVE DEMOS                                   
         DROP  R6                                                               
*                                                                               
         CLI   NUMDEMS,13          CAN ALTERNATE FORMAT BE EMPLOYED             
         BH    *+8                                                              
         MVI   PRNTDISP+1,30       SHIFT 6 COLUMNS (6*5=30)                     
*                                                                               
         GOTO1 =A(DHED),DMCB,(RC),RR=RELO   CALC DEMO HEADS ONCE                
*                                                                               
         L     R2,SBUFF                                                         
         XCEF  (R2),2600           CLEAR BUFFER AREA                            
*                                                                               
         MVI   RCSUBPRG,0                                                       
         CLI   OFFLINE,C'Y'        OFFLINE REPORT?                              
         BNE   ONLIN020            NO  - ONLINE                                 
         L     R2,VADUMMY          OFFLINE DUMMY ASSIGNMENT                     
         B     SETSTACK                                                         
ONLIN020 EQU   *                                                                
         L     R2,=V(DUMMY)        ONLINE DUMMY ASSIGNMENT                      
         A     R2,RELO                                                          
SETSTACK EQU   *                                                                
         ST    R2,ASTACK                                                        
         GOTO1 DATCON,DMCB,(3,BTODAY),(2,XTODAY)                                
         SPACE 4                                                                
*              DEAL WITH MARKET AND STATION FACTS                               
*              ----------------------------------                               
         SPACE 2                                                                
         TM    PRINTOPT,X'80'      SUPPRESS TEXT                                
         BO    MAS10                                                            
         MVI   OPTION,C'M'                                                      
         BAS   RE,MAS300                                                        
         SPACE 2                                                                
         MVI   OPTION,C'S'                                                      
         BAS   RE,MAS300                                                        
         EJECT                                                                  
*              CONTROL THE IO ROUTINES                                          
*              -----------------------                                          
         SPACE 2                                                                
MAS10    LA    R2,DPLIST                                                        
         LA    R3,NDPT                                                          
         SPACE 1                                                                
MAS20    CLI   0(R2),0             CONTROL FOR EACH DP                          
         BE    XIT                                                              
         MVC   SVDPT,0(R2)                                                      
         BAS   RE,NEWDPT                                                        
         BAS   RE,MAS30                                                         
         LA    R2,1(R2)                                                         
         BCT   R3,MAS20                                                         
         B     XIT                                                              
         SPACE 1                                                                
MAS30    NTR1                      BUILD A STACK OF D/A                         
         L     R5,ASTACK                                                        
         SR    R6,R6                                                            
         LA    R4,KEY                                                           
         USING RIDPKEY,R4                                                       
         XC    KEY,KEY                                                          
*SMY*    MVI   RIDPKTYP,X'92'                                                   
         MVI   RIDPKTYP,RIDPKTYQ                                                
         MVC   RIDPKREP,REPPAR     USE PARENT REP                               
         MVC   RIDPKSTA,ACTSTAT                                                 
         MVC   RIDPKDPT,0(R2)                                                   
         GOTO1 HIGH                                                             
         B     MAS50                                                            
         DROP  R4                                                               
         SPACE 1                                                                
MAS40    GOTO1 SEQ                                                              
         SPACE 1                                                                
MAS50    CLC   KEYSAVE(11),KEY     STATION D/P C/B                              
         BNE   MAS60                                                            
         MVC   0(4,R5),KEY+28                                                   
         LA    R5,4(R5)                                                         
         LA    R6,1(R6)                                                         
         B     MAS40                                                            
         SPACE 1                                                                
MAS60    LTR   R6,R6                                                            
         BZ    XIT                                                              
         L     R5,ASTACK                                                        
         B     MAS80                                                            
         SPACE 1                                                                
MAS70    LM    R5,R6,SAVESTAK                                                   
         LA    R5,4(R5)                                                         
         BCT   R6,MAS80                                                         
         B     XIT                                                              
         EJECT                                                                  
MAS80    MVC   KEY+28(4),0(R5)                                                  
         STM   R5,R6,SAVESTAK                                                   
         OI    DMINBTS,X'08'       SET PASS DELETED RECORDS                     
         GOTO1 GETREC                                                           
         NI    DMINBTS,X'F7'       UNSET PASS DELETED RECORDS                   
         MVC   KEY(27),IO                                                       
         TM    IO+29,X'80'         TEST RECORD IS DELETED                       
         BO    MAS70                                                            
         USING RINVKEY,R4                                                       
         OC    INVNO(3),INVNO      INVENTORY FILTER                             
         BZ    MAS90                                                            
         CLC   RINVKINV,INVNO                                                   
         BNE   MAS70                                                            
MAS90    CLI   DAYOPT,X'FF'        DAY FILTER                                   
         BE    MAS100                                                           
         CLC   DAYOPT,RINVKINV+1                                                
         BNE   MAS70                                                            
         DROP  R4                                                               
         SPACE 2                                                                
MAS100   MVI   ELCODE,X'01'                                                     
         LA    R6,IO                                                            
         BAS   RE,GETEL                                                         
         USING RINVPEL,R6                                                       
*                                                                               
         CLI   OPTFLAG,0          OPTION-SUPRESS PRNT IN MULT DPTS              
         BE    MAS102                                                           
         ZIC   R1,NOREQDPT        # OF REQ. DAYPARTS                            
         CH    R1,=H'1'           NO CHANCE OF DUPLICATE IF ONLY 1 DPT          
         BE    MAS102                                                           
*                                                                               
         BAS   RE,TSTDPT                                                        
         CLI   SKIPREC,0                                                        
         BNE   MAS70               DON'T PRINT IT IN THIS DAYPART               
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
         B     MAS70               NOTHING MATCHES-DON'T WANT IT                
         DROP  R6                                                               
         SPACE 2                                                                
MAS125   OC    ADT,ADT             WAS ACTIVITY DATE SPECIFIED                  
         BZ    MAS130                                                           
         LA    R6,IO                                                            
         MVI   ELCODE,X'EF'                                                     
         BAS   RE,GETEL            FIND ACTIVITY ELEMENT                        
         BNE   MAS70                                                            
         USING RINVAEL,R6                                                       
         CLC   RINVALST,ADT        AND SELECT CHANGES SINCE DATE                
         BL    MAS70                                                            
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
MAS130   LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         USING RINVPEL,R6                                                       
         CLC   ESDT(4),=X'0000FFFF' FILTER ON EFFECTIVE DATE(S)                 
         BE    MAS136                                                           
         CLC   RINVPEFF+0(2),EEDT  IGNORE ANY                                   
         BH    MAS70               STARTING AFTER SELECTED END DATE             
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    MAS140                                                           
         CLC   RINVPEFF+2(2),ESDT  IGNORE ANY                                   
         BL    MAS70               ENDING BEFORE SELECTED START DATE            
         B     MAS140                                                           
*                                                                               
MAS136   OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    MAS140                                                           
         CLC   RINVPEFF+2(2),XTODAY SEE IF IT ENDS BEFORE TODAY                 
         BL    MAS70                                                            
         SPACE 1                                                                
MAS140   GOTO1 =A(HDR),DMCB,(RC),RR=RELO     PRINT DETAILS                      
         TM    PRINTOPT,X'10'                RANGE OF BOOKS                     
         BO    MAS150                                                           
         BAS   RE,MAS170           GO AND LOOK FOR BOOKS                        
         B     *+8                                                              
MAS150   BAS   RE,RANGE            GO LOOK FOR RANGE OF BOOKS                   
         TM    PRINTOPT,X'80'      SUPPRESS TEXT                                
         BO    MAS160                                                           
         MVI   OPTION,C'I'         INDICATE INVENTORY TEXT                      
         BAS   RE,MAS300                                                        
         SPACE 2                                                                
MAS160   EQU   *                   IF ALTERNATE FORMAT (19+ DEM0S),             
         CLI   PRNTDISP+1,0                                                     
         BNE   MAS70                                                            
         MVI   CONTSW,C'N'                                                      
         MVC   P,SPACES            THEN PRINT                                   
         GOTO1 SPOOL,PARAS,(R8)    2ND BLANK LINE AFTER INV. CHUNK              
         B     MAS70                                                            
         EJECT                                                                  
* TSTDPT -- CHECKS IF PROGRAM HAS BEEN PRINTED ALREADY                          
*           PROGRAMS PRINT IN REQUEST ORDER                                     
         SPACE                                                                  
TSTDPT   NTR1                                                                   
         MVI   SKIPREC,0          0=USE RECORD                                  
         LA    R1,DPLIST          REQUEST LIST                                  
TSTD20   CLC   0(1,R1),0(R2)      CURRENT DAYPART?                              
         BE    XIT                USE THIS RECORD                               
*                                                                               
         LA    R3,6               # OF DPTS IN RECORD                           
         LA    RE,RINVDP          DAYPARTS CODED FOR PROGRAM                    
TSTD50   CLI   0(RE),C' '         ANY MORE DPTS?                                
         BE    TSTD80             NO                                            
*                                                                               
         CLC   0(1,R1),0(RE)                                                    
         BNE   TSTD70                                                           
         MVI   SKIPREC,1          SKIP RECORD - ALREADY PRINTED                 
         B     XIT                                                              
*                                                                               
TSTD70   LA    RE,1(RE)           CHECK NEXT DPT IN PROGRAM                     
         BCT   R3,TSTD50                                                        
*                                                                               
TSTD80   LA    R1,1(R1)           CHECK NEXT DPT IN REQUEST                     
         B     TSTD20                                                           
         EJECT                                                                  
*              CONTROL READING OF BOOKS - RANGE                                 
*              ---------------------------------                                
         SPACE 2                                                                
* 1. PRINT HEADER                                                               
* 2. CREATE LIST (RNGLST) OF BOOKS(2), SOURCE(1), AND DISK ADDRESS(4)           
*         OF ALL INVENTORY THAT FITS RANGE (MAX 30 BOOKS)                       
* 3. SORT LIST IN BOOK, SOURCE ORDER (FILE IS IN SOURCE, BOOK ORDER)            
* 4. THEN GET BOOKS IN NON-RANGE CODE USING RNGLIST DISK ADDRESSES              
         SPACE 2                                                                
RANGE    NTR1                                                                   
         USING RINVKEY,R4                                                       
         MVI   HDRSW,C'Y'          PUT HDR LINE TO BUFF 1ST-TIME-THRU           
         SPACE 1                                                                
         LA    R4,KEY                                                           
         MVC   KEYSAVE,KEY                                                      
         SPACE 1                                                                
         MVC   KEY,KEYSAVE                                                      
         XC    SVLST,SVLST                                                      
         LA    R5,SVLST                                                         
         LA    RE,SVCLST                                                        
         SPACE 1                                                                
RNG3     CLC   SVSOURCE(1),0(RE)                                                
         BNE   RNG5                                                             
         MVC   0(1,R5),2(RE)                                                    
         LA    R5,1(R5)                                                         
RNG5     LA    RE,L'SVCLST(RE)                                                  
         LA    RE,L'SVCLST(RE)     AND GO PAST 'NO CPM' BOOK                    
         CLI   0(RE),X'FF'         END OF SOURCE LIST                           
         BNE   RNG3                                                             
         MVI   0(R5),X'FF'         MARK END OF SAVE SOURCE LIST                 
         SPACE 1                                                                
         LA    R5,SVLST                                                         
         SPACE 1                                                                
         XC    RNGLST,RNGLST         RNGLST=SOURCE(1), BOOK(2), D/A(4)          
         MVI   RNGLSTND,X'FF'      SET DELIMITER FOR EOT                        
         LA    R3,RNGLST                                                        
         SR    R2,R2                                                            
         SPACE 1                                                                
         MVC   RINVKRSR,0(R5)                                                   
         MVC   RINVKBK,BOOKS+1                                                  
         GOTO1 HIGH                                                             
*SMY*    CLC   KEY(24),KEYSAVE                                                  
         CLC   KEY(RINVKRSR-RINVKEY),KEYSAVE                                    
         BNE   RNGX                                                             
         B     RNG20                                                            
         SPACE                                                                  
RNG10    GOTO1 SEQ                                                              
RNG20    CLC   RINVKRSR,0(R5)                                                   
         BE    RNG30                                                            
         LA    R5,1(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BNE   RNG20                                                            
         B     RNG40                                                            
         SPACE 1                                                                
RNG30    CLC   BOOKS+1(2),RINVKBK          ************                         
         BH    RNG10                       *  WITHIN  *                         
         CLC   BOOKS+5(2),RINVKBK          *  RANGE?  *                         
         BL    RNG10                       ************                         
         SPACE 1                                                                
         MVC   0(2,R3),RINVKBK                                                  
         MVC   2(1,R3),RINVKRSR                                                 
         MVC   3(4,R3),KEY+28      DISK ADDRESS                                 
         LA    R3,7(R3)                                                         
         LA    R2,1(R2)                                                         
         CH    R2,=H'30'           TABLE FULL?                                  
         BE    RNG40               YES - DON'T ALLOW MORE INPUT                 
         B     RNG10                                                            
         SPACE 1                                                                
RNG40    STC   R2,BYTE                                                          
         CH    R2,=H'1'                                                         
         BE    RNG50               SKIP XSORT FOR 1 BOOK                        
         CH    R2,=H'30'                                                        
         BNH   *+6                                                              
*                                                                               
*  NOTE:  THIS DUMP CAN NO LONGER HAPPEN....                                    
*                                                                               
         DC    H'0'                TOO MANY BOOKS IN RANGE                      
         GOTO1 XSORT,DMCB,RNGLST,(R2),7,3,0                                     
         SPACE 1                                                                
RNG50    LA    R5,RNGLST                                                        
         ZIC   RE,BYTE                                                          
         MH    RE,=H'7'                                                         
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
         LA    R2,BOOKS                                                         
         ZIC   R3,NUMBOOK                                                       
         LA    R4,KEY                                                           
         MVC   KEYSAVE,KEY                                                      
*                                                                               
MAS180   MVC   KEY,KEYSAVE                                                      
         MVC   RINVKRSR(3),0(R2)                                                
         SPACE 1                                                                
         LA    RE,SVCLST           CONVERSION TABLE                             
         SPACE 1                                                                
MAS185   CLC   3(1,RE),0(R2)       FROM BOOKVAL FORMAT TO KSRC FORMAT           
         BE    MAS190                                                           
         LA    RE,L'SVCLST(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   MAS185                                                           
         DC    H'0'                                                             
MAS190   MVC   RINVKRSR,2(RE)                                                   
         SPACE 1                                                                
         GOTO1 HIGH                                                             
*                                                                               
MAS192   DS    0H                                                               
*SMY*    CLC   KEY(24),KEYSAVE     ANY LUCK                                     
         CLC   KEY(RINVKRSR-RINVKEY),KEYSAVE     ANY LUCK                       
         BNE   MAS199                                                           
*                                                                               
*SMY*    CLC   KEY+24(3),KEYSAVE+24     SAME SOURCE AND BOOK                    
         CLC   KEY+RINVKRSR-RINVKEY(6),KEYSAVE+RINVKRSR-RINVKEY                 
         BE    MAS200                                                           
*                                                                               
MAS199   LA    R2,4(R2)            R2 POINTS TO NEXT BOOK                       
         BCT   R3,MAS180                                                        
         B     MAS240              ALL THROUGH                                  
         EJECT                                                                  
*     NOW GETREC FOR BOTH RANGE/NON-RANGE BOOKS AND THEN PRINT                  
*     --------------------------------------------------------                  
         SPACE 2                                                                
MAS200   CLI   HDRSW,C'Y'                                                       
         BNE   MAS201                                                           
***           PUT HDR LINE TO BUFF 1ST-TIME-THRU                                
         BAS   RE,BUFFADD                                                       
         BAS   RE,BUFFFULL                                                      
         MVI   HDRSW,0                                                          
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BNE   MAS201                                                           
***    BLANK LINE BTWN HDR & 1ST BK                                             
         BAS   RE,BUFFADD                                                       
         BAS   RE,BUFFFULL                                                      
*                                                                               
MAS201   TM    PRINTOPT,X'10'      RANGE OF BOOKS                               
         BNO   MAS202                                                           
         CLI   0(R5),X'FF'         END OF LIST                                  
         BE    MAS240                                                           
*                          MAKE IT LOOK LIKE WE READ FILE TO GET RECORD         
         MVC   KEY(RINVKRSR-RINVKEY),KEYSAVE                                    
         MVC   KEY+RINVKRSR-RINVKEY(1),2(R5)                                    
         MVC   KEY+RINVKBK-RINVKEY(2),0(R5)                                     
         MVC   KEY+28(4),3(R5)     BUT REALLY USE SAVED DISK ADDRESS            
*                                                                               
MAS202   GOTO1 GETREC                                                           
         BAS   RE,DATA             FORMAT DEMOS                                 
*                                                                               
MAS206   GOTO1 =A(BOOKSQ),DMCB,(RC),SVCLST,RR=RELO  FORMAT BOOK NAME            
         BAS   RE,BUFFADD         POINT TO NEXT LINE                            
         BAS   RE,BUFFFULL                                                      
         XC    BLOCK1,BLOCK1       USE BLOCK1 TO SAVE SHR/LVL VALUES            
         TM    PRINTOPT,X'40'      SHRS/LVLS ARE SUPPRESSED OPTIONALLY          
         BZ    MAS210                                                           
         CLI   MULTSW,C'Y'         OR ON COMBOS                                 
         BE    MAS210                                                           
         CLC   SAVECODE,=C'PA'     IF PROGRAM CODE IS PA, PRINT SHR/LVL         
         BE    MAS208                                                           
         TM    PRINTOPT,X'02'      BUT SUPPRESS IF UPGRADES                     
         BO    MAS210                                                           
         CLC   SAVECODE,=C'ES'     OR IF PROGRAM CODE IS ES                     
         BE    MAS210                                                           
         CLC   SAVECODE,=C'PE'     OR PE                                        
         BE    MAS210                                                           
         CLC   SAVECODE,=C'PJ'     OR PJ                                        
         BE    MAS210                                                           
MAS208   BAS   RE,SHRLVL           FORMAT (& PRINT) SHARES & LEVELS             
         SPACE 1                                                                
MAS210   MVI   OPTION,C'A'         PRINT ANY AUTO FOOTNOTES                     
         BAS   RE,RAT                                                           
*                                                                               
MAS230   TM    PRINTOPT,X'02'      ANY UPGRADE                                  
         BZ    *+8                                                              
         BAS   RE,UPPRNT           YES, PRINT UPGRADE EXPRESSION                
         MVI   SPACING,1                                                        
         BAS   RE,BUFFADD         SPACE AFTER EVERY GROUP                       
         BAS   RE,BUFFFULL                                                      
         TM    PRINTOPT,X'10'      RANGE OF BOOKS                               
         BNO   MAS199              NEXT BOOK-NON RANGE                          
         LA    R5,7(R5)                                                         
         B     MAS200              NEXT BOOK-RANGE                              
*                                                                               
MAS240   CLI   HDRSW,C'Y'          HAS ANY DATA BEEN PRINTED                    
         BNE   MAS244                                                           
         BAS   RE,BUFFADD          NO, SO POINT PAST HEADER LINE                
         BAS   RE,BUFFFULL                                                      
         CLI   PRNTDISP+1,0        AND IF THIS IS 19+ FORMAT                    
         BNE   MAS244                                                           
         BAS   RE,BUFFADD          THEN LEAVE SPACING LINE                      
         BAS   RE,BUFFFULL                                                      
         SPACE 1                                                                
MAS244   L     RE,SBUFF                                                         
         LA    RF,5                COULD BE 5 HEADLINES                         
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
         BAS   RE,BUFFFULL                                                      
         SPACE 1                                                                
         BAS   RE,PRNTALL                                                       
         B     XIT                                                              
         EJECT                                                                  
*     PRINT ALL BOOKS THAT BELONG TO A PIECE OF INVENTORY TOGETHER              
*     ------------------------------------------------------------              
*                  (TEXT CAN BE ON A DIFFERENT PAGE)                            
         SPACE 2                                                                
PRNTALL  NTR1                                                                   
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
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
* ROUTINE CHECKS TO SEE THAT PRINT BUFFER HASN'T GOTTEN TOO BIG                 
         SPACE 2                                                                
BUFFFULL ST    RE,FULL                                                          
         L     RE,ABUFF                                                         
         C     RE,XBUFF                                                         
         BL    BFXIT                                                            
         OI    PRNTOPT2,X'80'      BUFFER BIGGER THAN 1 PAGE (P1)               
         BAS   RE,PRNTALL          TOO BIG, SO PRINT IT                         
         OI    PRNTOPT2,X'40'      BUFFER BIGGER THAN 1 PAGE (P2)               
         L     RE,SBUFF                                                         
         ST    RE,ABUFF            AND START AT BEGINNING AGAIN                 
BFXIT    L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
       ++INCLUDE RESVCTAB                                                       
         EJECT                                                                  
*              HANDLE DAYPART BREAK                                             
*              --------------------                                             
         SPACE 2                                                                
NEWDPT   NTR1                                                                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 4                                                                
*              CONTROL HANDLING OF RATIONALE                                    
*              -----------------------------                                    
         SPACE 2                                                                
MAS300   NTR1                                                                   
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         MVC   KEY,KEYSAVE                                                      
         CLI   OPTION,C'I'                                                      
         BE    MAS310                                                           
         XC    KEY,KEY                                                          
*SMY*    MVI   RINVKTYP,X'12'                                                   
         MVI   RINVKTYP,RINVKTYQ                                                
         MVC   RINVKREP,REPPAR     USE PARENT REP                               
         MVC   RINVKSTA(5),ACTSTAT                                              
         MVC   RINVKRSR,OPTION                                                  
         B     *+8                                                              
MAS310   MVI   RINVKRSR,X'FF'      FOR OPTION I                                 
         XC    RINVKTXT,RINVKTXT                                                
         GOTO1 HIGH                                                             
         B     MAS330                                                           
         SPACE 2                                                                
MAS320   GOTO1 SEQ                                                              
         SPACE 2                                                                
MAS330   DS    0H                                                               
*SMY*    CLC   KEY(25),KEYSAVE                                                  
         CLC   KEY(RINVKBK-RINVKEY),KEYSAVE                                     
         BNE   XIT                                                              
         BAS   RE,RAT              RATIONALE                                    
         B     MAS320                                                           
*                                                                               
         EJECT                                                                  
*              ROUTINES FOR RATIONALE RECORDS                                   
*              ------------------------------                                   
         SPACE 2                                                                
RAT      NTR1                                                                   
         GOTO1 GETREC                                                           
         CLI   MASOP4,C'Y'        TRANSFER INFO?                                
         BNE   RAT30              NO                                            
         LA    R2,SVCLST                                                        
         GOTO1 =A(HISTDATA),DMCB,(RC),(R2),RR=RELO                              
         BAS   RE,BUFFFULL                                                      
*                                                                               
RAT30    CLI   OPTION,C'A'   AUTO FOOTNOTES DON'T NEED FILTER CHECK             
         BNE   RAT40                                                            
         L     R3,ABUFF            PUT AUTO FOOTNOTES TO BUFF WITH THE          
         B     RAT110              REST OF THE BOOK STUFF                       
         SPACE 1                                                                
RAT40    L     R3,SBUFF       OTHER TEXT PRINTS IN A CHUNK OF ITS OWN           
         MVC   0(132,R3),SPACES                                                 
         SR    R2,R2               LINE COUNTER                                 
         SPACE 1                                                                
         LA    R6,IO                                                            
         MVI   ELCODE,X'02'        IS A FILTER APPLICABLE                       
         BAS   RE,GETEL                                                         
         BNE   RAT110                                                           
         USING RINVFEL,R6                                                       
         SPACE 1                                                                
         CLC   REPPAR,AGENCY       IF NOT LOCAL REP                             
         BNE   RAT45                                                            
         CLI   RINVFLOC,C'Y'       AND TEXT IS LOCAL ONLY                       
         BE    XIT                 THEN DON'T WANT IT FOR MAIN REP              
         SPACE 1                                                                
RAT45    CLI   RINVFSRC,0          SOURCE FILTER                                
         BE    RAT50                                                            
         CLC   SVSOURCE,RINVFSRC                                                
         BNE   XIT                                                              
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
         BL    XIT                                                              
         CLC   RINVFBK,5(R1)       AND END RANGE                                
         BH    XIT                                                              
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
         LA    RE,SVCLST           MATCH SOURCE                                 
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
         B     XIT                                                              
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
         B     XIT                                                              
         SPACE 1                                                                
RAT80    TM    PRINTOPT,X'04'      ANY FILTERS TO PRINT                         
         BZ    RAT110                                                           
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
         LA    R1,SVCLST           CONVERSION TABLE                             
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
         SPACE 1                                                                
RAT110   LA    R6,IO                                                            
         MVI   ELCODE,X'01'                                                     
         CLI   OPTION,C'A'                                                      
         BE    RAT145                                                           
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         MVC   5(4,R3),=C'TEXT'                                                 
         CLI   OPTION,C'M'                                                      
         BNE   *+10                                                             
         MVC   5(4,R3),=C'MKT.'                                                 
         CLI   OPTION,C'S'                                                      
         BNE   *+10                                                             
         MVC   5(4,R3),=C'STN.'                                                 
         EDIT  (2,RINVKTXT),(5,10(R3)),ALIGN=LEFT                               
RAT120   LA    R5,11(R3)                                                        
         SPACE 1                                                                
RAT130   CLC   0(3,R5),SPACES                                                   
         BE    RAT140                                                           
         LA    R5,1(R5)                                                         
         B     RAT130                                                           
         SPACE 1                                                                
RAT140   LA    R5,1(R5)            LEAVE SPACE                                  
         DROP  R4                                                               
         USING RINVTEL,R6                                                       
RAT145   BAS   RE,GETEL                                                         
         B     RAT160                                                           
         SPACE 1                                                                
RAT150   BAS   RE,NEXTEL                                                        
         SPACE 1                                                                
RAT160   BNE   RAT170                                                           
         LA    RE,RINVTEXT                                                      
         ZIC   R1,RINVTLEN                                                      
         SH    R1,=H'7'                                                         
         CLI   OPTION,C'A'                                                      
         BNE   RAT166                                                           
         LA    R5,28(R3)                                                        
         SH    R1,=H'6'                                                         
         LA    RE,6(RE)            REMOVE MMMYY FROM FOOTNOTE                   
RAT166   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),0(RE)                                                    
         LA    R3,132(R3)                                                       
         MVC   28(104,R3),SPACES   DON'T CREAM HEADLINE STUFF THERE             
         LA    R5,132(R5)                                                       
         LA    R2,1(R2)                                                         
         B     RAT150                                                           
         SPACE 2                                                                
RAT170   CLI   OPTION,C'A'                                                      
         BNE   RAT173                                                           
         ST    R3,ABUFF                                                         
         BAS   RE,BUFFFULL                                                      
         B     XIT                                                              
         SPACE 1                                                                
RAT173   LTR   R2,R2               ANYTHING TO PRINT                            
         BNZ   RAT176                                                           
         CLC   P(40),SPACES        HD2-HD5 TO BE PRINTED                        
         BNE   RAT190                                                           
         B     XIT                                                              
RAT176   STC   R2,ALLOWLIN                                                      
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
         BE    XIT                                                              
RAT190   MVI   SPACING,1                                                        
         GOTO1 SPOOL,PARAS,(R8)    SPACE AFTER RATIONALE                        
         B     XIT                                                              
         SPACE 3                                                                
*              ROUTINES FOR DATA RECORDS                                        
*              -------------------------                                        
         SPACE 2                                                                
DATA     NTR1                                                                   
         LA    R6,IO               CHECK FOR CODE                               
         MVI   MULTSW,C'N'                                                      
         MVI   ELCODE,X'CD'                                                     
         BAS   RE,GETEL                                                         
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
         BAS   RE,GETEL                                                         
         BNE   *+8                                                              
         OI    PRINTOPT,X'02'      INDICATE UPGRADE EXISTS                      
         LA    R5,ACTDEMOS         POINT AT OUTPUT FOR DEMO VALUES              
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO                                      
         B     XIT                                                              
         EJECT                                                                  
*              SHARES AND LEVELS -  FORMATTING AND PRINTING                     
*              --------------------------------------------                     
         SPACE 2                                                                
SHRLVL   NTR1                                                                   
         MVI   LINSW,C'L'          *** LEVELS ***                               
         MVC   SVDEMLST,DEMLST     SAVE DEMLST                                  
         BAS   RE,EFFDEMS          DEVELOPE DEMO TYPES                          
         BAS   RE,BUFFADD         LEVELS PRINT AFTER SHARES                     
         LA    R5,BLOCK1+100                                                    
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO    FORMAT THE DEMOS                  
         MVC   DEMLST,SVDEMLST                RESTORE DEMLST                    
         L     RE,ABUFF                                                         
         LA    RF,6(RE)                                                         
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    RF,35(RE)                                                        
         MVC   0(5,RF),=C'H/P/T'                                                
         SH    RE,=H'132'          BACK UP A LINE TO PRINT SHARES               
         ST    RE,ABUFF                                                         
*                                                                               
         MVI   LINSW,C'S'          *** SHARES ***                               
         CLC   SAVECODE,=C'PA'                                                  
         BNE   SL45                                                             
         SPACE 1                                                                
SL40     BAS   RE,RECALC                                                        
         OI    PRINTOPT,X'01'                  INDICATE RECALC DEMOS            
         LA    R5,BLOCK1                       POINT TO SHARES                  
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO     FORMAT DEMOS                     
         NI    PRINTOPT,X'FE'                                                   
         B     SL50                                                             
         SPACE 1                                                                
SL45     MVC   SVDEMLST,DEMLST                 SAVE DEMLST                      
         BAS   RE,EFFDEMS                      DEVELOPE DEMO TYPES              
         LA    R5,BLOCK1                                                        
         GOTO1 =A(DAT50),DMCB,(RC),RR=RELO     FORMAT THE DEMOS                 
         MVC   DEMLST,SVDEMLST                 RESTORE DEMLST                   
SL50     BAS   RE,PERCENT                      PERCENT SIGNS FOR SHARES         
         L     RE,ABUFF                                                         
         LA    RF,6(RE)                                                         
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    RF,35(RE)                                                        
         MVC   0(5,RF),=C'SHARE'                                                
         LA    RE,132(RE)          SHARE LINE                                   
         LA    RE,132(RE)          LEVELS LINE                                  
         ST    RE,ABUFF                                                         
         BAS   RE,BUFFFULL                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*  RECALCULATE SHARES FOR PA                                                    
*                                                                               
*                    (RATING X 10000)                                           
*                    ---------------   = SHARE   (ROUND IN DAT50)               
*                         LEVEL                                                 
         SPACE 2                                                                
RECALC   NTR1                                                                   
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
         B     XIT                                                              
         EJECT                                                                  
*              PERCENT SIGN TACKED ONTO SHARES                                  
*              -------------------------------                                  
         SPACE 2                                                                
PERCENT  NTR1                                                                   
         L     R3,ABUFF                                                         
         LA    R3,12(R3)           FORMATTED LINE                               
         AH    R3,PRNTDISP         ALTERNATE FORMAT                             
         LA    R2,DEMLST                                                        
*                                                                               
PC100    EQU   *                   NON-ZERO VALUES ONLY                         
         CLI   3(R3),C' '                                                       
         BE    PC140                                                            
         MVI   5(R3),C'%'          INSERT PERCENT SIGN                          
PC140    LA    R2,3(R2)                                                         
         LA    R3,7(R3)                                                         
         CLI   0(R2),X'FF'                                                      
         BNE   PC100                                                            
         B     XIT                                                              
         EJECT                                                                  
*              WORK OUT EFFECTIVE DEMO TYPES FOR DIFFERENT LINES                
*              -------------------------------------------------                
         SPACE 2                                                                
EFFDEMS  NTR1                                                                   
         CLI   LINSW,C'I'                                                       
         BE    XIT                                                              
         LA    R3,DEMLST                                                        
         ZIC   R0,NUMDEMS                                                       
         SPACE 1                                                                
EFF2     MVC   WORK(1),1(R3)       COLUMN TYPE                                  
         MVC   WORK+1(1),LINSW     LINE TYPE                                    
         LA    R1,EFFTABLE                                                      
         SPACE 1                                                                
EFF4     CLI   0(R1),X'FF'                                                      
         BE    EFF6                                                             
         CLC   WORK(2),0(R1)                                                    
         BE    EFF6                                                             
         LA    R1,3(R1)                                                         
         B     EFF4                                                             
         SPACE 1                                                                
EFF6     MVC   1(1,R3),2(R1)       SUBSTITUTE TYPE                              
         LA    R3,3(R3)                                                         
         BCT   R0,EFF2                                                          
         B     XIT                                                              
         SPACE 3                                                                
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
*              PRINT UPGRADE COMMENTS                                           
*              ----------------------                                           
         SPACE 2                                                                
UPPRNT   NTR1                                                                   
         LA    R6,IO                                                            
         MVI   ELCODE,X'05'        ANY UPGRADE ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         CLC   SAVECODE,=C'PJ'     ONLY SHOW UPGRADE IF                         
         BNE   XIT                 CODE IS PJ                                   
         L     R3,ABUFF                                                         
         LA    R2,28(R3)                                                        
         GOTO1 AUPOUT,DMCB,(R6),(R2)    EDIT UPGRADE EXPRESSION                 
         LA    R3,132(R3)                                                       
         ST    R3,ABUFF                                                         
         BAS   RE,BUFFFULL                                                      
         B     XIT                                                              
         EJECT                                                                  
*              HOOK ROUTINE FOR HEADLINE DETAILS                                
*              ---------------------------------                                
         SPACE 2                                                                
HOOK     NTR1                                                                   
         MVC   H3+10(6),MASSTN     STATION CALL LETTERS                         
         MVC   H3+17(29),MKTSV     MARKET NAME                                  
         MVC   H3+108(3),MASSRCE   SOURCE                                       
         SPACE 1                                                                
         L     R3,ADPTBL            LOOK UP DAYPART                             
HK10     CLC   SVDPT,0(R3)                                                      
         BE    HK20                                                             
         LA    R3,L'DPTBL1(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   HK10                                                             
         SPACE 1                                                                
HK20     MVC   H3+65(20),1(R3)     DAYPART NAME                                 
         SPACE 1                                                                
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    HK70                                                             
*                                                                               
         MVC   H5,SVH7             MOVE IN SAVED DEMO HEADLINES                 
         MVC   H6,SVH8                                                          
         MVC   H7,SVH9                                                          
*                                                                               
         MVC   H5(13),=C'INV  DAY TIME'                                         
         MVC   H5+30(7),=C'DAYPART'                                             
         MVC   H6(17),=C'PROGRAM/ADJACENCY'                                     
         MVC   H7(9),=C'EFFECTIVE'                                              
         MVC   H7+31(10),=C'BOOK    CD'                                         
         SPACE 1                                                                
         CLI   CONTSW,C'Y'                                                      
         BNE   HOOKX                                                            
         MVC   H8(16),CONTINUE     NNNN (CONTINUED)                             
         B     HOOKX                                                            
         SPACE 1                                                                
HK70     MVC   H7,SVH7             MOVE IN SAVED DEMO HEADLINES                 
         MVC   H8,SVH8                                                          
         MVC   H9,SVH9                                                          
*                                                                               
         MVC   H5+000(13),=C'INV  DAY TIME'                                     
         MVC   H5+022(17),=C'PROGRAM/ADJACENCY'                                 
         MVC   H5+107(9),=C'EFFECTIVE'                                          
         MVC   H5+121(7),=C'DAYPART'                                            
         MVC   H7+001(10),=C'BOOK    CD'                                        
         SPACE 1                                                                
         CLI   CONTSW,C'Y'                                                      
         BNE   HOOKX                                                            
         MVC   H10(16),CONTINUE    NNNN (CONTINUED)                             
*                                                                               
HOOKX    B     XIT                                                              
         EJECT                                                                  
ALLDPT   DC    C'MDERATLWKFNPVSJO' DAYPARTS VALID FOR 'ALL'                     
         SPACE 2                                                                
         EJECT                                                                  
* COMMON ROUTINES                                                               
*                                                                               
XIT      XIT1                                                                   
         SPACE 2                                                                
MYEND    MVI   ERROR,X'FE'         USING MY OWN ERROR MSG                       
ERREND   GOTO1 VERRXIT                                                          
         SPACE 2                                                                
SPERR    GOTO1 VCURSERR,PARAS,0                                                 
         B     XIT                                                              
         SPACE 2                                                                
         GETEL (R6),34,ELCODE                                                   
         SPACE 2                                                                
BUMP     ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         BR    RE                                                               
*                                                                               
BUFFADD  L     R1,ABUFF                                                         
         LA    R1,132(R1)                                                       
         ST    R1,ABUFF                                                         
         BR    RE                                                               
*                                                                               
         SPACE 2                                                                
RELO     DS    A                                                                
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
PATCH    DC    XL64'00'                                                         
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         SPACE 2                                                                
*  MY OWN ERROR MESSAGES                                                        
*                                                                               
MANYBKS  DC    C'* ERROR * TOO MANY BOOKS - LIMIT IS 6'                         
MANYDEM  DC    C'* ERROR * TOO MANY DEMOS - LIMIT IS 17'                        
MANYFLT  DC    C'* ERROR * TOO MANY FILTERS - LIMIT IS 6'                       
BADBK1   DC    C'* ERROR * BOOK CAN NOT HAVE PREFIX'                            
BADBK2   DC    C'* ERROR * BOOKS MUST BE THE SAME SOURCE'                       
BADBK3   DC    C'* ERROR * END BOOK LESS THAN START BOOK'                       
BADMENU  DC    C'* ERROR * MENU IS 2 A/N CHARACTERS'                            
BADSHARE DC   C'* ERROR * SHR-H/P/T MUST FILTER ON DYPT OR DAY OR INV#'         
         SPACE 3                                                                
* REPORT HEADLINE SPECS                                                         
*                                                                               
HEDSPECS DS    0H                                                               
         SPROG 0                                                                
         PSPEC H1,001,AGYNAME                                                   
         PSPEC H1,055,C'INVENTORY MASTER LISTING'                               
         PSPEC H1,099,REPORT                                                    
         PSPEC H1,115,PAGE                                                      
         PSPEC H2,001,REQUESTOR                                                 
         PSPEC H2,055,24C'-'                                                    
         PSPEC H2,099,RUN                                                       
         PSPEC H3,001,C'STATION -'                                              
         PSPEC H3,056,C'DAYPART -'                                              
         PSPEC H3,099,C'SERVICE -'                                              
         SPACE 1                                                                
         DC    X'00'                                                            
         SPACE 3                                                                
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
         DS    0H                                                               
DPTBL1   DS    0CL21               DEFAULT DAYPART TABLE                        
         DC    CL21'MMORNING'                                                   
         DC    CL21'DDAYTIME'                                                   
         DC    CL21'EEARLY FRINGE'                                              
         DC    CL21'REARLY NEWS'                                                
         DC    CL21'APRIME ACCESS'                                              
         DC    CL21'TLATE NEWS'                                                 
         DC    CL21'LLATE FRINGE'                                               
         DC    CL21'WWEEKEND'                                                   
         DC    CL21'KKIDS'                                                      
         DC    CL21'FFRINGE'                                                    
         DC    CL21'NNEWS'                                                      
         DC    CL21'PPRIME'                                                     
         DC    CL21'VMOVIES'                                                    
         DC    CL21'SSPECIAL'                                                   
         DC    CL21'JSPORTS'                                                    
         DC    CL21'OSOAPS'                                                     
         DC    CL21'UCOMPETITIVE'                                               
         DC    CL21'XLOCAL'                                                     
         DC    CL21'YOTHER'                                                     
         DC    X'FF'                                                            
         DC    CL20'GENERAL'                                                    
         SPACE 1                                                                
NDPT     EQU   (*-DPTBL1)/L'DPTBL1                                              
         SPACE 3                                                                
DPTBL2   DS    0CL21               NBC DAYPART TABLE                            
         DC    CL21'MMORNING'                                                   
         DC    CL21'DDAYTIME'                                                   
         DC    CL21'EEARLY FRINGE'                                              
         DC    CL21'REARLY NEWS'                                                
         DC    CL21'APRIME ACCESS'                                              
         DC    CL21'TLATE NEWS'                                                 
         DC    CL21'LLATE FRINGE'                                               
         DC    CL21'WWEEKEND'                                                   
         DC    CL21'KKIDS'                                                      
         DC    CL21'FFRINGE'                                                    
         DC    CL21'NNEWS'                                                      
         DC    CL21'PPRIME'                                                     
         DC    CL21'VMOVIES'                                                    
         DC    CL21'SSPECIAL'                                                   
         DC    CL21'JSPORTS'                                                    
         DC    CL21'OOLYMPICS'     <------------------                          
         DC    CL21'UCOMPETITIVE'                                               
         DC    CL21'XLOCAL'                                                     
         DC    CL21'YOTHER'                                                     
         DC    X'FF'                                                            
         DC    CL20'GENERAL'                                                    
         EJECT                                                                  
         SPACE 3                                                                
*           ROUTINE TO PRINT '03' ELEMENT OUT                                   
*         ------------------------------------                                  
         SPACE                                                                  
HISTDATA NMOD1 0,*HISTDT*                                                       
         L     RC,0(R1)                                                         
         L     R2,4(R1)           CONVERSION TABLE                              
         LA    R6,IO                                                            
         MVI   ELCODE,X'03'                                                     
         BAS   RE,GETEL                                                         
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
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*        PROCESSING HEADER RECORDS                                              
*        -------------------------                                              
*        R4, ON ENTRY, POINTS TO INVENTORY KEY                                  
*        R6, ON ENTRY, POINTS TO INVENTORY 01 EL                                
*                                                                               
HDR      NMOD1 0,***HDR**                                                       
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         USING RINVKEY,R4                                                       
         USING RINVPEL,R6                                                       
         NI    PRNTOPT2,X'3F'      RESET BUFFER MORE THAN 1 PAGE BIT            
         MVC   WORK,SPACES                                                      
         GOTO1 UNDAY,PARAS,RINVPDAY,WORK      DAY                               
         OC    WORK,SPACES                                                      
         GOTO1 UNTIME,PARAS,RINVPTIM,WORK+20  TIME                              
         GOTO1 SQUASHER,PARAS,WORK,40                                           
         MVC   P+5(22),WORK                   DAY/TIME                          
*                                                                               
         ZIC   R5,RINVPLEN                                                      
         SH    R5,=H'41'                                                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+22(0),RINVPROG               PROGRAM                           
         CLI   PRNTDISP+1,0        FOR ALTERNATE FORMAT                         
         BNE   HDR12                                                            
         MVC   P4,SPACES                                                        
         MVC   P4(27),P+22         PUT A SPACE BTWN EACH OF                     
         MVC   P4+28(27),P+49      THE 3 LINES, AND THEN                        
         MVC   P4+56(27),P+76       SQUASH THEM                                 
         LA    R5,1(R5)                                                         
         GOTO1 SQUASHER,PARAS,P4,(R5)                                           
         ZIC   R5,DMCB+4                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   P+22(0),P4                                                       
*                                                                               
HDR12    MVC   WORK,SPACES                    EFFECTIVE DATES                   
         CLC   RINVPEFF(2),RINVPEFF+2                                           
         BNE   HDR13                                                            
         MVC   WORK(4),=C'ONLY'                                                 
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,WORK+5)                             
         B     HDR16                                                            
*                                                                               
HDR13    DS    0H                                                               
         MVC   WORK(4),=C'FROM'                                                 
         GOTO1 DATCON,PARAS,(2,RINVPEFF),(8,WORK+5)                             
         OC    RINVPEFF+2(2),RINVPEFF+2                                         
         BZ    HDR16                                                            
         MVC   WORK(8),WORK+5                                                   
         MVC   WORK+8(56),SPACES                                                
         MVI   WORK+8,C'-'                                                      
         GOTO1 DATCON,PARAS,(2,RINVPEFF+2),(8,WORK+9)                           
         SPACE 1                                                                
HDR16    MVC   P+107(17),WORK      EFFECTIVE DATE(S)                            
*                                                                               
         MVI   P+124,C'-'                                                       
         MVC   P+125(6),RINVDP     -DAYPART CODE(S)-                            
         LA    R4,P+130                                                         
HDR17    CLI   0(R4),C' '                                                       
         BNE   *+10                                                             
         BCTR  R4,0                                                             
         B     HDR17                                                            
         MVI   1(R4),C'-'                                                       
         SPACE 1                                                                
         LA    R4,KEY              INVENTORY NUMBER                             
         USING RINVKEY,R4                                                       
         CLI   RINVKINV+3,0        IF NEW INV RECORD                            
         BE    *+14                                                             
         MVC   P(4),RINVKINV          INV NUMBER ALREADY CHARACTER              
         B     HDR19                                                            
*                                                                               
         EDIT  (1,RINVKINV),(2,P),FILL=0                                        
         MVC   P+2(2),RINVKINV+1                                                
         CLI   P+3,C'0'                                                         
         BNE   *+8                                                              
         MVI   P+3,C' '                                                         
*                                                                               
HDR19    DS    0H                                                               
*                                                                               
         MVC   CONTINUE(4),P                                                    
         MVC   CONTINUE+5(11),=C'(CONTINUED)'                                   
*                                                                               
         L     RE,SBUFF                                                         
         ST    RE,ABUFF                                                         
*                                                                               
         CLI   PRNTDISP+1,0        ALTERNATE FORMATE                            
         BNE   HDR20                                                            
         MVC   0(132,RE),P         SAVE P IN BUFF                               
         B     XIT                                                              
         SPACE 1                                                                
HDR20    MVC   0(22,RE),P+0        INV & DAY/TIME                               
         MVC   31(8,RE),P+124      DAYPART(S)                                   
*                                                                               
         LA    RE,132(RE)          PROGRAM#'S 1, 2 & 3                          
         LA    R4,P+22             INTO NEXT 3 LINES                            
         LA    R5,3                                                             
HDR30    CLC   0(27,R4),SPACES                                                  
         BE    HDR40                                                            
         MVC   0(27,RE),0(R4)                                                   
         LA    RE,132(RE)                                                       
         LA    R4,27(R4)                                                        
         BCT   R5,HDR30                                                         
HDR40    MVC   0(17,RE),P+107      EFFECTIVE DATE(S) INTO NEXT LINE...          
*                                                                               
         XIT1                                                                   
         DROP  R4,R6                                                            
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ONLY BUILD DEMO HEADLINES ONCE                                   
*              ------------------------------                                   
         SPACE 2                                                                
DHED     NMOD1 0,**DHED**                                                       
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         XC    SVH7,SVH7                                                        
         XC    SVH8,SVH8                                                        
         XC    SVH9,SVH9                                                        
         LA    R3,SVH7+12          POINT TO WHERE 1ST DEMO SHOULD PRINT         
         AH    R3,PRNTDISP         ALERNATE FORMAT                              
         LA    R5,SVH8+12                                                       
         AH    R5,PRNTDISP         ALERNATE FORMAT                              
         LA    R2,DEMLST                                                        
         ZIC   R6,NUMDEMS                                                       
         LA    R4,DBLOCKA1                                                      
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'INV'                                                   
         MVC   DBCOMFCS,ACOMFACS                                                
         MVI   DBSELMED,C'T'                                                    
DHED40   CLI   1(R2),C'T'          FUDGE FOR DEMOCON                            
         BNE   *+8                                                              
         MVI   1(R2),C'I'                                                       
         GOTO1 DEMOCON,PARAS,(0,(R2)),(4,WORK),(0,DBLOCKD)                      
         CLI   1(R2),C'I'          RESET FOR NEXT TIME                          
         BNE   *+8                                                              
         MVI   1(R2),C'T'                                                       
         DROP  R4                                                               
*                                                                               
         MVC   0(4,R3),WORK        FORMAT - .WM.1849                            
         MVC   0(4,R5),WORK+4                                                   
         LA    R1,TYPTAB                                                        
DHED50   CLC   0(1,R1),1(R2)                                                    
         BE    DHED60                                                           
         LA    R1,5(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BNE   DHED50                                                           
DHED60   MVC   SVH9-SVH8(4,R5),1(R1)                                            
         CLC   1(2,R2),=C'D702'    PUT=HUT FOR MET                              
         BNE   *+8                                                              
         MVI   132(R5),C'H'                                                     
         CLC   1(2,R2),=X'D703'    OR METB                                      
         BNE   *+8                                                              
         MVI   132(R5),C'H'                                                     
         LA    R2,3(R2)                                                         
         LA    R3,7(R3)                                                         
         LA    R5,7(R5)                                                         
         BCT   R6,DHED40                                                        
         XIT1                                                                   
         SPACE 4                                                                
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
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              FORMAT BOOK SEQUENCE                                             
*              --------------------                                             
         SPACE 1                                                                
BOOKSQ   NMOD1 0,*BOOKSQ*                                                       
*                                                                               
         L     RC,0(R1)                                                         
         L     R1,4(R1)            PASSED CONVERSION TABLE                      
*                                                                               
         L     R3,ABUFF                                                         
         LA    R3,1(R3)                                                         
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    R3,29(R3)                                                        
         LA    R4,KEY                                                           
         USING RINVKEY,R4                                                       
         SPACE 1                                                                
BKSQ10   CLC   RINVKRSR(1),2(R1)                                                
         BE    BKSQ20                                                           
         LA    R1,L'SVCLST(R1)                                                  
         CLI   0(R1),X'FF'                                                      
         BNE   BKSQ10                                                           
         DC    H'0'                                                             
BKSQ20   MVC   0(1,R3),1(R1)       PRECEDE WITH P,T,S OR E                      
         LA    R3,1(R3)                                                         
         SPACE 1                                                                
BKSQ70   ZIC   R1,RINVKBK+1                                                     
         BCTR  R1,0                                                             
         MH    R1,=H'3'                                                         
         LA    R1,MONTHS(R1)                                                    
         MVC   0(3,R3),0(R1)                                                    
         CLI   RINVKBK+1,0             UNSPECIFIED MONTH                        
         BNE   BKSQ80                                                           
         BCTR  R3,0                                                             
         MVC   0(3,R3),=C'EST'     E BECOMES EST                                
         SPACE 1                                                                
BKSQ80   MVI   3(R3),C'/'                                                       
         EDIT  (1,RINVKBK),(2,4(R3)),WRK=DMCB                                   
*                                                                               
         L     R5,ABUFF                                                         
         LA    R5,9(R5)                                                         
         CLI   PRNTDISP+1,0        ALTERNATE FORMAT                             
         BE    *+8                                                              
         LA    R5,29(R5)                                                        
         MVC   0(2,R5),SAVECODE    PRINT SAVECODE (FROM 'DATA' RTN)             
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
*              ROUTINES FOR HANDLING THE DEMO FORMATTING                        
*              -----------------------------------------                        
*      ON ENTRY, R5 POINTS TO OUTPUT FOR DEMO VALUES                            
         SPACE 2                                                                
DAT50    NMOD1 0,**DAT50*                                                       
*                                                                               
         L     RC,0(R1)                                                         
*                                                                               
         LA    R2,DEMLST           DEMO LIST                                    
         TM    PRINTOPT,X'01'      RECALULATED DEMOS                            
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
         MVC   DBXTID(4),=C'SPOT'                                               
         CLI   MASOP5,C'Y'                                                      
         BE    DAT51A                                                           
         MVI   DBXTTRP,X'01'                                                    
         MVI   DBXTTSP,X'01'                                                    
         MVI   DBXTTIP,X'02'                                                    
         B     DAT51B                                                           
DAT51A   EQU   *                                                                
         MVI   DBXTTRP,X'00'                                                    
         MVI   DBXTTSP,X'00'                                                    
         MVI   DBXTTIP,X'03'                                                    
DAT51B   EQU   *                                                                
         DROP  R1                                                               
*                                                                               
         GOTO1 DEMOUT,DMCB,(C'L',(R2)),DBLOCKD,(R5)                             
         CLI   DBERROR,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  R4                                                               
         SPACE 1                                                                
DAT55    L     R3,ABUFF                                                         
         LA    R3,11(R3)           POINT TO PRINT LINE                          
         AH    R3,PRNTDISP         ALTERNATE FORMAT                             
         ZIC   R4,NUMDEMS                                                       
DAT60    L     R1,0(R5)                                                         
         TM    PRINTOPT,X'01'      RECALULATED DEMOS                            
         BNO   DAT65               ALWAYS NEED TO DROP DECIMAL                  
         SR    R0,R0               THEN DROP DECIMAL                            
         LA    R1,5(R1)                                                         
         D     R0,=F'10'                                                        
         ST    R1,0(R5)            STORE ROUNDED VALUE                          
DAT65    EQU   *                                                                
         LTR   R1,R1               IF NO VALUE                                  
         BNZ   DAT70                                                            
         MVI   4(R3),C'.'          SHOW .                                       
         B     DAT80                                                            
DAT70    EQU   *                                                                
         CLI   MASOP5,C'Y'                                                      
         BE    DAT75                                                            
         EDIT  (R1),(6,0(R3)),1                                                 
         B     DAT80                                                            
DAT75    EQU   *                                                                
         EDIT  (R1),(5,0(R3))                                                   
DAT80    EQU   *                                                                
         CLC   1(2,R2),=X'D701'    IF HUT                                       
         BE    DAT90                                                            
         CLC   1(2,R2),=X'E201'    OR SHARE                                     
         BNE   DAT100                                                           
DAT90    CLI   MULTSW,C'Y'         AND IT'S A COMBO                             
         BNE   DAT100                                                           
         MVC   0(6,R3),=C'  N   '  THEN DON'T SHOW VALUE                        
         SPACE 1                                                                
DAT100   LA    R2,3(R2)            NEXT DEMO                                    
         LA    R3,7(R3)            NEXT DEMO PRINTING AREA                      
         LA    R5,4(R5)            NEXT DEMO VALUE                              
         BCT   R4,DAT60            LOOP FOR MORE DEMOS                          
*                                                                               
         XIT1                                                                   
         SPACE 3                                                                
         LTORG                                                                  
         EJECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
       ++INCLUDE RERESWRK                                                       
         EJECT                                                                  
* DSECT TO COVER SCREEN                                                         
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE RERESF5D                                                       
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
         EJECT                                                                  
* SAVE AREA VALUES                                                              
*                                                                               
SYSD     DSECT                                                                  
         ORG   SYSSPARE            LOCAL WORKING STORAGE                        
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
*                                  X'40'  PRINT SHARES AND H/P/T                
*                                  X'20'  1 DPT/DAY/INV                         
*                                  X'10'  RANGE OF BOOKS                        
*                                  X'08'  FILTERS SPECIFIED                     
*                                  X'04'  TEXT FILTERS TO PRINT                 
*                                  X'02'  UPGRADE EXISTS                        
*                                  X'01'  RECALCUTE DEMOS                       
*                                                                               
PRNTOPT2 DS    XL1                 X'80'  BUFFER MORE THAN 1 PAGE (P1)          
*                                  X'40'  BUFFER MORE THAN 1 PAGE (P2)          
DAYOPT   DS    CL1                                                              
*                                                                               
ESDT     DS    XL2                 EFFECTIVE START DATE (COMPRESSED)            
EEDT     DS    XL2                 EFFECTIVE END DATE (COMPRESSED)              
*                                                                               
ADT      DS    CL3                 ACTIVITY DATE (YMD BINARY)                   
         DS    CL1                                                              
INVNO    DS    CL4                 INVENTORY FILTER (1ST 3-BYTES USED)          
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
ABUFF    DS    A                   POINTER TO NEXT LINE IN BUFF                 
XBUFF    DS    A                   END OF 1 FULL PAGE IN BUFF                   
AUPOUT   DS    A                   REUPOUT                                      
SVLST    DS    CL10                VALID ARB, NSI OR SRC SOURCE CODES           
RNGLST   DS    CL210               LIST OF BOOKS WITHIN RANGE                   
*                                     7 BYTES X 30 BOOKS MAX                    
*                                        2 BYTES RINVKBK                        
*                                        1 BYTE RINVKSRC                        
*                                        4 BYTES DISK ADDRESS                   
RNGLSTND DS    CL1                 END OF TABLE                                 
INVSRC   DS    XL1                 1ST BYTE FROM BOOKVAL                        
ACTDEMOS DS    CL96                24 DEMO VALUES (4 BYTES EACH)                
MAXPLINS DS    XL1                 MAX NO. PRINT LINES (NOT HEADS)              
NOREQDPT DS    XL1                 # OF REQUESTED DAYPARTS                      
SKIPREC  DS    XL1                 FLAG TO SKIP RECORD OR NOT                   
OPTFLAG  DS    XL1                 FLAG INDICATE Y/N TO OPTION 3                
SBUFF    DS    A                   A(START OF PRINT BUFFER)                     
STAMP    DS    CL8                 STORAGE STAMP                                
SAVELN   EQU   *-SYSSPARE          SAVE AREA LENGTH                             
MYBASE   DS    A                                                                
ASTACK   DS    A                                                                
ADPTBL   DS    A                   A(THE DAYPART TABLE)                         
         DS    CL(L'SYSSPARE-(*-SYSSPARE)) SPARE                                
*                                                                               
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'183RERES05   04/24/09'                                      
         END                                                                    
