*          DATA SET ACREPCT02  AT LEVEL 055 AS OF 05/23/16                      
*PHASE ACCT02A,+0                                                               
         TITLE 'ACCT - OM TIME SHEETS'                                          
ACCT02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACCT**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACCTD,RC                                                         
         LA    R8,PROGPROF                                                      
         USING PROFD,R8                                                         
         EJECT                                                                  
*--------------------------------------------                                   
*        R U N F I R S T                                                        
*--------------------------------------------                                   
*                                                                               
RUNF00   CLI   MODE,RUNFRST                                                     
         BNE   REQF00                                                           
         LA    RE,RELOTAB                                                       
         LA    R1,ATYPES           RELOCATE A-TYPES                             
RNF01    L     RF,0(RE)                                                         
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RNF01                                                            
*                                                                               
         SR    R0,R0               GET MAIN STORAGE                             
         LA    R4,MAINTAB                                                       
RNF03    CLI   0(R4),X'FF'         END OF TABLE                                 
         BE    RNF05                                                            
         A     R0,0(R4)            ADD THE LENGTH OF EACH TABLE                 
         LA    R4,L'MAINTAB(R4)                                                 
         B     RNF03                                                            
*                                                                               
RNF05    ST    R0,MAINLEN          SAVE LENGTH OF TABLE                         
         GETMAIN  R,LV=(0)                                                      
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         ST    R1,MAINBGN          START OF AREA                                
         LA    R4,MAINTAB                                                       
*                                                                               
RNF07    L     R3,4(R4)                                                         
         ST    R1,0(R3)            A(START OF THIS TABLE)                       
         L     R0,0(R4)            LENGTH OF THIS TABLE                         
         AR    R1,R0               R1 TO NEXT AREA                              
         LA    R4,L'MAINTAB(R4)                                                 
         CLI   0(R4),X'FF'                                                      
         BNE   RNF07                                                            
*                                                                               
         L     RF,MAINBGN          START OF AREA                                
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         STCM  RF,15,MCUSRDMP                                                   
         A     RF,MAINLEN          LENGTH OF AREA                               
         STCM  RF,15,MCUSRDMP+4                                                 
         DROP  RE                                                               
         BAS   RE,BLDTAB           BUILD TABLE OF SJ ACCOUNTS                   
*        BAS   RE,LINEUP           FORM LINEUP                                  
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------                                    
*        R E Q F I R S T                                                        
*-------------------------------------------                                    
*                                                                               
*                                                                               
REQF00   CLI   MODE,REQFRST                                                     
         BNE   LDG00                                                            
         BAS   RE,BLDCAL            BUILD PERIOD BASED ON CALENDAR              
         MVC   WORK,SPACES                                                      
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)                                  
         MVC   OLDATE,=3X'FF'                                                   
         ZIC   R3,PROFDAYS                                                      
         LTR   R3,R3                                                            
         BZ    RQF04                                                            
         LNR   R3,R3               FORCES IT NEGATIVE                           
         GOTO1 ADDAY,DMCB,WORK,WORK+6,(R3)                                      
         GOTO1 DATCON,DMCB,WORK+6,(1,OLDATE)                                    
*                                                                               
RQF04    MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   MAXROW,DETMAX       DEFAULT IS 36 LINES PER PAGE                 
         CLI   PROFROWS,0          PROFILE CAN OVERRIDE                         
         BE    *+12                                                             
         IC    R3,PROFROWS                                                      
         STC   R3,MAXROW                                                        
*                                                                               
*                                        INIT FOR SORT **                       
         LA    R1,SRTKLEN                SORT KEY LENGTH                        
         CVD   R1,DUB                    CONVERT KEY LEN TO CHARS               
         OI    DUB+7,X'0F'                                                      
         UNPK  SORTCARD+15(3),DUB+6(2)                                          
         LA    R1,SRTLEN                 SORT RECORD LENGTH                     
         CVD   R1,DUB                    CONVERT REC LEN TO CHARS               
         OI    DUB+7,X'0F'                                                      
         UNPK  RECCARD+22(3),DUB+6(2)                                           
         GOTO1 ADSORTER,DMCB,SORTCARD,RECCARD,0                                 
         B     XIT                                                              
         EJECT                                                                  
*                                                                               
*-----------------------------------------------------                          
*              F I R S T  F O R  L E D G E R                                    
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LDG00    CLI   MODE,LEDGFRST                                                    
         BNE   LVA00                                                            
         L     RF,ADLDGHIR                                                      
         SR    R2,R2                       NUMBER OF ACTUAL LEVELS              
         LA    R2,1(R2)                    AT LEAST ONE LEVEL                   
         USING ACLELD,RF                                                        
         MVC   LLEVA(LLEVELLN),ACLVALS     LEVEL LENGTHS/NAMES                  
         LA    R3,LENLEVLS                 INDIVIDUAL LENGTHS OF LEVELS         
         SR    R4,R4                                                            
         LA    R1,LLEVA                    COMBINED LEVEL LENGTHS               
         LA    R0,LLEVLNUM                 MAXIMUM NUMBER OF LEVELS             
LDG02    DS    0H                                                               
         ZIC   R5,0(R1)                    PREVIOUS COMBINED LENGTH             
         SR    R5,R4                       MINUS NEW COMBINED LENGTH            
         BP    *+6                         EQUALS INDIVIDUAL LEVEL LEN          
         DC    H'0'                                                             
         STC   R5,0(R3)                    SAVE INDIVD LENGTH OF LEVEL          
         CLI   0(R1),MAXLEN                LAST LEV HAS MAXLEN FOR ACCT         
         BE    LDG04                                                            
         LA    R2,1(R2)                    ADD TO LEVEL COUNT                   
         ZIC   R4,0(R1)                    COMBINED LENGTH IN R4                
         LA    R1,LLEVALN(R1)              BUMP TO NEXT COMBINED LENGTH         
         LA    R3,L'LENLEVA(R3)            NEXT INDIVDUAL LEN SAVE AREA         
         BCT   R0,LDG02                                                         
         DC    H'0'                                                             
LDG04    STC   R2,NUMLEVLS                 ACTUAL NUMBER OF LEVELS              
         B     XIT                                                              
         DROP  RF                                                               
         EJECT                                                                  
*-----------------------------------------------------                          
*              L E V A F I R S T                                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LVA00    CLI   MODE,LEVAFRST                                                    
         BNE   LVB00                                                            
*                                  1R HIGHER LEVEL CODE,NAME                    
*                                                                               
         GOTO1 HINAME,DMCB,0,ADLVANAM,ADHEIRA                                   
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              L E V B F I R S T                                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LVB00    CLI   MODE,LEVBFRST                                                    
         BNE   LVC00                                                            
         MVC   LOCATION,SPACES     CLEAR THE LOCATION                           
*                                  1R HIGHER LEVEL CODE,NAME,ADDRESS            
*                                                                               
         GOTO1 HINAME,DMCB,ADLVBADD,ADLVBNAM,ADHEIRB                            
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              L E V C F I R S T                                                
*-----------------------------------------------------                          
*                                                                               
*                                                                               
LVC00    CLI   MODE,LEVCFRST                                                    
         BNE   ACF00                                                            
*                                                                               
         USING ACTRECD,R7                                                       
         L     R7,ADHEIRC          ADDR OF RECORD                               
*                                  1R HIGHER LEVEL CODE,NAME,ADDRESS            
*                                                                               
         GOTO1 HINAME,DMCB,ADLVCADD,ADLVCNAM,ADHEIRC                            
*                                                                               
         LA    R6,SRTWRK           SORT WORK AREA                               
         USING SRTD,R6             CLEAR IT                                     
         MVC   SRTKEY(SRTKLEN),SPACES                                           
         ZIC   R1,LLEVC            LENGTH OF LEVEL A+B+C                        
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EXMVC R1,SRTHI,ACTKACT    ALL 1R HIGHER LEVELS TO SORT KEY             
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              F I R S T  F O R  A C C O U N T                                  
*-----------------------------------------------------                          
*                                                                               
*                                                                               
ACF00    CLI   MODE,PROCACC                                                     
         BNE   PTRN00                                                           
         MVI   FCRDTRNS,C'N'                                                    
         MVI   FCRDTIME,C'N'                                                    
         CLI   PROFLOK,C'N'        SUPPRESS LOCKED ACCOUNTS?                    
         BE    ACF02                                                            
         L     R2,ADACCSTA                                                      
         USING RSTELD,R2                                                        
         TM    RSTSTAT,RSTSACIL    SUPPRESS LOCKED ACCOUNT                      
         BO    XIT                                                              
ACF02    CLI   PROFSAL,C'Y'        SALARY REQUIRED FOR TS TO PRINT              
         BNE   ACF06                                                            
         L     R4,ADACC                                                         
         MVI   ELCODE,MSAELQ       SALARY ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   XIT                 IGNORE IF NO SALARIES                        
ACF06    DS    0H                                                               
         L     R7,ADACC                                                         
         USING ACTRECD,R7                                                       
         LA    R6,SRTWRK           SORT WORK AREA                               
         USING SRTD,R6                                                          
         XC    SRTNUMB,SRTNUMB                                                  
         LA    RE,SRTCLTS                                                       
         L     RF,=A(SRTCLN)                                                    
         XCEFL                                                                  
         MVC   SRTEMP(SRTEMLN),SPACES                                           
*                                                                               
*                                  CHECK IF IT'S AN OVERHEAD ACCOUNT            
         ZIC   R1,LLEVA            LENGTH OF LEVEL A                            
         BCTR  R1,0                                                             
         EXCLC R1,ACTKACT,NINES    CORPORATE LEVEL OVERHEAD                     
         BE    XIT                                                              
         LA    R2,ACTKACT                                                       
         ZIC   R0,LLEVA                                                         
         AR    R2,R0                                                            
         ZIC   R1,LENLEVB          LENGTH OF LEVEL B                            
         BCTR  R1,0                                                             
         EXCLC R1,0(R2),NINES      OFFICE LEVEL OVERHEAD                        
         BE    XIT                                                              
         LA    R2,ACTKACT                                                       
         ZIC   R0,LLEVC            LENGTH OF LEVEL A+B+C                        
         AR    R2,R0                                                            
         CLC   0(3,R2),NINES       DEPT LEVEL OVERHEAD                          
         BE    XIT                                                              
*                                  NOT AN OVERHEAD ACCOUNT                      
*                                  (R2 STILL POINTS AT LEVEL 4)                 
         ZIC   R1,LENLEVD          LENGTH OF EMPLOYEE CODE                      
         SH    R1,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
         EXMVC R1,SRTEMP,0(R2)     EMPLOYEE CODE TO SORT KEY                    
         MVC   SRTEMPL,SRTEMP      PRIMARY AND SECONDARY                        
         CLI   QOPT1,C'N'          EMPLOYEE NUMBER ORDER                        
         BE    *+10                YES -                                        
         MVC   SRTEMP,SPACES       NO  - ALPHA ORDER BY EMPL NAME               
         LA    R1,SRTCLTS          1ST TABLE ENTRY                              
         ST    R1,ALTAB            SAVE ADDR OF ENTRY                           
*                                                                               
         L     R2,ADACCNAM                                                      
         CLI   0(R2),NAMELQ        NAME ELEMENT                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 BLDTNAM,DMCB,ADACCNAM                                            
         MVC   SRTNAM,WORK         NAME PASSED BACK IN WORK                     
         MVC   SRTLOC,LOCATION     SAVE THE LOCATION                            
         L     R2,ADACCADD         WAS AN ADDRESS ELEMENT PASSED?               
         USING ADRELD,R2                                                        
         CLI   ADREL,ADRELQ        MAKE SURE IT'S A ADDR EL                     
         BNE   ACF10                                                            
         CLC   ADRADD1,SPACES                                                   
         BE    ACF10                                                            
         MVC   SRTLOC,ADRADD1      LOCATION OVERIDE?                            
*                                                                               
ACF10    MVI   FCRDTRNS,C'Y'       SET TO READ TRANSACTIONS                     
         MVI   FCRDTIME,C'Y'       AND TIME                                     
         B     XIT                                                              
         DROP  R2,R6,R7                                                         
         EJECT                                                                  
*-----------------------------------------------------                          
*              R E A D  T R A N S A C T I O N S                                 
*-----------------------------------------------------                          
*                                                                               
*                                                                               
PTRN00   CLI   MODE,PROCTRNS                                                    
         BNE   PTIM00                                                           
         L     R4,ADTRANS                                                       
         USING TRNELD,R4                                                        
         CLI   TRNEL,TRNELQ        MAKE SURE IT'S A TRANSACTION                 
         BNE   XIT                                                              
         CLC   TRNDATE,OLDATE                                                   
         BL    XIT                 IGNORE OLD ONES                              
         MVI   ELCODE,PCIELQ       PROJECT CONTROL ELEMENT                      
         BAS   RE,NEXTEL                                                        
         BNE   XIT                                                              
         LA    R6,SRTWRK           SORT WORK AREA                               
         USING SRTD,R6                                                          
*                                                                               
         USING PCIELD,R4                                                        
         CLC   PCICLI+1(L'SJ),SJ                                                
         BNE   XIT                 SKIP IF NOT SJ LEDGER                        
         CLC   PCICLI+1+L'SJ(L'PCICLI-1-L'SJ),SPACES                            
         BNH   XIT                 SKIP IF NOT VALID DATA                       
         MVC   WRKEY,SPACES                                                     
         MVC   WRKEY,PCICLI        CLIENT PRODUCT                               
         CLI   PCILN,PCILN2Q                                                    
         BL    *+10                NO JOB ON THIS ACCOUNT                       
         MVC   WRKEY,PCIPRJT       CLIENT PRODUCT JOB                           
         BAS   RE,INDXUP           LOOKUP JOB INDEX NUMBER                      
         OC    INDEX,INDEX                                                      
         BZ    XIT                 IF IT NO LONGER EXISTS SKIP IT               
         BAS   RE,ADDIT            ADD INDEX TO SORT RECORD                     
         B     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
*-----------------------------------------------------                          
*              R E A D  T I M E   R E C O R D S                                 
*-----------------------------------------------------                          
*                                                                               
*                                                                               
PTIM00   CLI   MODE,PROCTIME                                                    
         BNE   ACL00                                                            
         L     R4,ADTRANS                                                       
         USING TIMELD,R4                                                        
         CLI   TIMEL,TIMELQ        MAKE SURE IT'S A TIME ELEMENT                
         BNE   XIT                                                              
         CLI   TIMETYP,TIMEINP     MAKE SURE IT'S OF TYPE INPUT DETAIL          
         BNE   XIT                                                              
*                                                                               
         L     R7,AMONACC                                                       
         USING ACMD,R7                                                          
         L     R7,ACMALTN          GET ADDRESS OF TIME RECORD                   
         USING TIMRECD,R7                                                       
         CLC   TIMKPEDT,OLDATE                                                  
         BL    XIT                 IGNORE OLD ONES                              
*                                                                               
         CLC   TIMACC(2),=C'SJ'                                                 
         BNE   XIT                 SKIP NON CLIENT                              
         MVC   WRKEY,SPACES                                                     
         MVC   WRKEY(1),RCCOMPFL                                                
         MVC   WRKEY+1(L'TIMACC),TIMACC   CLIENT PRODUCT                        
         BAS   RE,INDXUP           LOOKUP JOB INDEX NUMBER                      
         OC    INDEX,INDEX                                                      
         BZ    XIT                 IF IT NO LONGER EXISTS SKIP IT               
         BAS   RE,ADDIT            ADD INDEX TO SORT RECORD                     
         B     XIT                                                              
         DROP  R4,R7                                                            
         EJECT                                                                  
*-----------------------------------------------------                          
*              L A S T  F O R  A C C O U N T                                    
*-----------------------------------------------------                          
*                                                                               
*                                                                               
ACL00    CLI   MODE,ACCLAST                                                     
         BNE   RQL00                                                            
         LA    R6,SRTWRK           SORT WORK AREA                               
         USING SRTD,R6                                                          
         CLC   SRTEMPL,SPACES                                                   
         BE    XIT                                                              
         GOTO1 ADSORTER,DMCB,=C'PUT',(R6)      PUT TO SORT                      
         B     XIT                                                              
         EJECT                                                                  
*-----------------------------------------------------                          
*              L A S T  F O R  R E Q L A S T                                    
*-----------------------------------------------------                          
*                                                                               
*                                                                               
RQL00    CLI   MODE,REQLAST                                                     
         BNE   RUL00                                                            
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         MVC   OFDPSB(LEVSAV),SPACES  CLEAR CODE/NAME SAVE AREA                 
RQL02    DS    0H                                                               
         GOTO1 ADSORTER,DMCB,=C'GET'                                            
         L     R2,4(R1)                                                         
         LTR   R2,R2                                                            
         BZ    XIT                                                              
         LA    R6,SRTWRK                                                        
         LA    R7,SRTLEN           R6 ADDR OF RECEIVING (R7) LENGTH             
         LR    R3,R7               R2 ADDR SENDING (R3) LENGTH                  
         MVCL  R6,R2                                                            
         LA    R3,P                                                             
         USING PRNTD,R3                                                         
         L     R5,AECDLST            STACK LIST                                 
         USING BIND,R5                                                          
         XC    BININ,BININ           CLEAR EMPLOYEE STACK TABLE                 
         BAS   RE,STAXUP             STACK CLIENTS NUMERIC OR ALPHA             
         L     R2,BININ              USE R0 AS COUNTER                          
         LTR   R2,R2                                                            
         BP    RQL04                 TABLE EMPTY NEXT EMPLOYEE                  
         MVC   WRKAREA,SPACES                                                   
         LA    R4,WRKAREA                                                       
         LA    R2,1                                                             
         B     RQL06                                                            
RQL04    L     R4,BINTABLE           ADDRESS OF 1ST TABLE ENTRY                 
         USING ECLICDE,R4                                                       
RQL06    BAS   RE,HEADUP             HEADUP A NEW PAGE                          
         MVI   GETLNE,DETLNE         1ST CLIENT LINE                            
         BAS   RE,SKIPLNE                                                       
RQL10    CLC   LINE,MAXROW           END OF THE PAGE                            
         BH    RQL06                                                            
         MVC   P,SPACES                                                         
         MVC   PRNTDES,ECLINAME      SJ CLIENT NAME                             
         MVC   PRNTCOD,ECLIACT2      ACCOUNT CODE                               
         GOTO1 ACREPORT                                                         
         LA    R4,ECLILEN(R4)        NEXT CLIENT                                
         BCT   R2,RQL10                                                         
         B     RQL02                 NEXT EMPLOYEE                              
         DROP  R3,R4,R5,R6                                                      
         EJECT                                                                  
*------------------------------------------------                               
*              L A S T  F O R  R U N                                            
*------------------------------------------------                               
*                                                                               
*                                                                               
RUL00    CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         LM    R0,R1,MAINLEN                                                    
         FREEMAIN R,LV=(0),A=(1)                                                
         L     RE,ADMASTC                                                       
         USING MASTD,RE                                                         
         XC    MCUSRDMP,MCUSRDMP    CLEAR EXTRA DUMP AREA                       
         DROP  RE                                                               
XIT      XMOD1 1                                                                
         EJECT                                                                  
*************************************************************                   
*              BUILD CALENDAR DATE TABLE                                        
*************************************************************                   
*                                                                               
BLDCAL   NTR1                                                                   
         CLI   MODE,REQFRST                                                     
         BNE   BLDC00                                                           
         MVI   CALENDAR,C'N'                                                    
         MVC   PERIODCP,SPACES                                                  
         MVC   WKOFFICE,SPACES     OFFICE TO SPACES                             
         MVC   STDATE,QSTART                                                    
         MVC   EDDATE,QEND                                                      
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         CLC   QEND,SPACES         IF START AND END SPECIFIED USE IT            
         BNE   BLDC12                                                           
         B     *+12                                                             
BLDC00   CLI   CALENDAR,C'Y'       IF NO CORP CALENDAR                          
         BNE   BLDC10              DONT BOTHER LOOKING FOR OFFICE LEVEL         
         L     R7,ACREC            LOOK FOR CALENDAR PASSIVE POINTER            
         USING CASRECD,R7                                                       
         XC    CASPAS,CASPAS                                                    
         MVI   CASPTYP,CASPTYPQ    X'3E0C'                                      
         MVI   CASPSUB,CASPSUBQ                                                 
         MVC   CASPCPY,RCCOMPFL                                                 
         MVC   CASPEDTE,START                                                   
         MVC   CASPOFC,WKOFFICE                                                 
         MVC   SAVEKEY,CASPAS                                                   
         MVC   COMMAND,DMRDHI                                                   
BLDC02   DS    0H                                                               
         GOTO1 DATAMGR,DMCB,COMMAND,ACCDIR,ACREC,ACREC                          
         CLC   CASPAS(CASPEDTE-CASRECD),SAVEKEY                                 
         BNE   BLDC10                                                           
         CLC   CASPSDTE,START      SEE IF START FALLS WITHIN REC DATES          
         BH    BLDC10                                                           
         CLC   CASPEDTE,START                                                   
         BL    BLDC10                                                           
*                                                                               
BLDC03   CLC   CASPOFC,WKOFFICE                                                 
         BE    BLDC04                                                           
         MVC   COMMAND,DMRSEQ                                                   
         B     BLDC02                                                           
*                                                                               
BLDC04   MVC   DA,CASPDA           DISK ADDRESS                                 
         GOTO1 DATAMGR,DMCB,DMGET,ACCMST,DA,ACREC,DMWORK                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,CASRFST          FIRST ELEMENT                                
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         USING TMPELD,R4                                                        
BLDC06   CLI   TMPEL,0                                                          
         BE    BLDC10                                                           
         CLI   TMPEL,TMPELQ        TIME SHEET PERIOD ELEMENT?                   
         BE    BLDC08                                                           
BLDC06A  SR    R1,R1                                                            
         IC    R1,TMPLN                                                         
         AR    R4,R1                                                            
         B     BLDC06                                                           
*                                                                               
BLDC08   CLC   TMPSTART,START      SEE IF START FALLS WITHIN  DATES             
         BH    BLDC06A                                                          
         CLC   TMPEND,START                                                     
         BL    BLDC06A                                                          
         GOTO1 DATCON,DMCB,(1,TMPSTART),(0,STDATE)                              
         GOTO1 DATCON,DMCB,(1,TMPEND),(0,EDDATE)                                
         MVI   CALENDAR,C'Y'                                                    
         B     BLDC12                                                           
*                                                                               
*                                  ****NO CALENDAR FOUND******                  
BLDC10   CLI   MODE,REQFRST        IF CORP LEVEL JUST USE QSTART QEND           
         BE    BLDC12                                                           
         MVC   PERIOD,PERIODCP                                                  
         B     BLDC20                                                           
*                                                                               
BLDC12   DS    0H                                                               
         MVC   PERIOD,SPACES                                                    
         CLC   EDDATE,SPACES                                                    
         BNE   BLDC14                                                           
         LA    R3,7                DEFAULT END DATE IS 7 DAYS                   
         GOTO1 ADDAY,DMCB,STDATE,EDDATE,(R3)                                    
BLDC14   GOTO1 DATCON,DMCB,(X'20',STDATE),(10,PERIOD),(0,EDDATE)                
         CLI   MODE,REQFRST                                                     
         BNE   BLDC20                                                           
         MVC   PERIODCP,PERIOD     SAVE CORP CAL PERIOD                         
*                                                                               
BLDC20   B     XIT                                                              
         DROP  R4,R7                                                            
         EJECT                                                                  
*************************************************************                   
*              ROUTINE TO HEADUP NEW PAGE                                       
*************************************************************                   
*                                                                               
HEADUP   NTR1                                                                   
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         GOTO1 PRINT,DMCB,P,=C'BC01' SKIP TO CHANNEL 1                          
         MVI   LINE,1                SET LINE NUMBER                            
         MVI   GETLNE,OFFLNE         GET TO OFFICE LINE                         
         BAS   RE,SKIPLNE                                                       
         MVC   P,SPACES                                                         
         LA    R3,P                                                             
         USING HEADD,R3                                                         
         BAS   RE,HIGHUP             MAKE SURE CORRECT NAMES                    
         MVC   HEADOFF,OFFCODE                                                  
         MVC   HEADDESC,OFFNAME      NAME OF OFFICE                             
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   GETLNE,DPTLNE         GET TO DEPT LINE                           
         BAS   RE,SKIPLNE                                                       
         MVC   P,SPACES                                                         
         MVC   HEADDPT,DEPCODE                                                  
         MVC   HEADDESC,DEPNAME      NAME OF DEPT                               
         MVC   HEADPER,PERIOD        PERIOD                                     
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   GETLNE,SUBLNE         GET TO DEPT LINE                           
         BAS   RE,SKIPLNE                                                       
         MVC   P,SPACES                                                         
         MVC   HEADSUB,SUBCODE                                                  
         MVC   HEADDESC,SUBNAME      NAME OF SUB DEPT                           
         GOTO1 ACREPORT                                                         
*                                                                               
         MVI   GETLNE,EMPLNE         GET TO EMPLOYEE LINE                       
         BAS   RE,SKIPLNE                                                       
         MVC   P,SPACES                                                         
         MVC   HEADEMP,SRTEMPL       EMPLOYEE CODE                              
         MVC   HEADDESC,SRTNAM       EMPLOYEE NAME                              
         MVC   HEADLOC,SRTLOC        EMPLOYEE LOCATION                          
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
* **********************************************************                    
*        CHECK FOR HIGHER LEVEL CHANGES                                         
* **********************************************************                    
*                                                                               
HIGHUP   NTR1                                                                   
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         CLC   OFDPSB,SRTHI                   ANY LEVEL CHANGES                 
         BE    XIT                            NO                                
*                                                                               
         MVC   WRKEY,SPACES                   GET NEW SUB DPT NAME              
         MVC   WRKEY(L'SRTHI),SRTHI                                             
         BAS   RE,HINAM                       GO LOOK IT UP                     
         MVC   SUBNAME,WORK                   NAME OF SUB                       
         LA    R2,SRTHI                                                         
         ZIC   R0,LLEVB                       LEN OF A + B                      
         AR    R2,R0                                                            
         ZIC   R1,LENLEVC                     LENGTH OF LEVEL C                 
         BCTR  R1,0                                                             
         EXMVC R1,SUBCODE,0(R2)                                                 
*                                                                               
*                                             OFFICE DEPT CHANGE?               
         ZIC   R1,LLEVB                       LEN OF A + B                      
         BCTR  R1,0                                                             
         EXCLC R1,SRTHI,OFDPSB                                                  
         BE    HIUP04                                                           
         MVC   WRKEY,SPACES                                                     
         EXMVC R1,WRKEY,SRTHI                                                   
         BAS   RE,HINAM                       GO LOOK IT UP                     
         MVC   DEPNAME,WORK                   NAME OF DPT                       
         LA    R2,SRTHI                                                         
         ZIC   R0,LLEVA                       LEN OF A                          
         AR    R2,R0                                                            
         ZIC   R1,LENLEVB                     LENGTH OF LEVEL B                 
         BCTR  R1,0                                                             
         EXMVC R1,DEPCODE,0(R2)                                                 
*                                                                               
         ZIC   R1,LLEVA                       LEN OF A                          
         BCTR  R1,0                                                             
         EXCLC R1,SRTHI,OFDPSB                                                  
         BE    HIUP04                                                           
         MVC   WRKEY,SPACES                                                     
         EXMVC R1,WRKEY,SRTHI                                                   
         BAS   RE,HINAM                       GO LOOK IT UP                     
         MVC   OFFNAME,WORK                   NAME OF OFFICE                    
         ZIC   R1,LLEVA                       LEN OF A                          
         BCTR  R1,0                                                             
         EXMVC R1,OFFCODE,SRTHI                                                 
         MVC   WKOFFICE,OFFCODE                                                 
         BAS   RE,BLDCAL                      GET CHANGE OF PERIOD              
*                                                                               
HIUP04   MVC   OFDPSB,SRTHI                   SAVE CODES                        
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* **********************************************************                    
*        LOOKUP HIGHER LEVEL NAMES                                              
* **********************************************************                    
*                                                                               
HINAM    NTR1                                                                   
         L     R5,AEMPCDE          ADDR OF TABLE                                
         USING BIND,R5                                                          
         MVC   WORK,SPACES                                                      
         L     R0,BININ            NUMBER OF TABLE ENTRIES                      
         L     RF,BINTABLE         1ST TABLE ENTRY                              
         USING EMPD,RF                                                          
HINAM02  CLC   WRKEY(L'EMPACT),EMPACT                                           
         BE    HINAM04                                                          
         LA    RF,EMPLEN(RF)                                                    
         BCT   R0,HINAM02                                                       
         MVC   WORK(20),=C'TEST NAME'                                           
         B     XIT                                                              
*                                                                               
HINAM04  MVC   WORK,EMPNAM         PASS BACK NAME                               
         B     XIT                                                              
         DROP  R5,RF                                                            
         EJECT                                                                  
*************************************************************                   
*              ROUTINE TO PRINT LINEUP                                          
*************************************************************                   
*&&DO                                                                           
LINEUP   NTR1                                                                   
         MVC   P,SPACES                                                         
         LA    R3,P                                                             
         USING PRNTD,R3                                                         
         ZIC   R2,PRFLINUP           NUMBER OF LINEUP PAGES                     
         LTR   R2,R2                                                            
         BP    LINUP00                                                          
         LA    R2,5                  DEFAULT IS FIVE LINEUP PAGES               
LINUP00  DS    0H                                                               
         GOTO1 PRINT,DMCB,P,=C'BC01' SKIP TO CHANNEL 1                          
         MVI   LINE,1                SET LINE NUMBER                            
         MVI   GETLNE,XXXLNE         GET TO XXX LINE                            
         BAS   RE,SKIPLNE                                                       
         MVI   PRNTDES,XXX                                                      
         MVC   PRNTDES+1(L'PRNTDES-1),PRNTDES                                   
         MVI   PRNTCOD,XXX                                                      
         MVC   PRNTCOD+1(L'PRNTCOD-1),PRNTCOD                                   
         MVC   PSECOND,P                                                        
         GOTO1 ACREPORT                                                         
         BCT   R2,LINUP00                                                       
         B     XIT                                                              
         DROP  R3                                                               
*&&                                                                             
         EJECT                                                                  
*************************************************************                   
*              ROUTINE TO SKIP TO NEW LINE                                      
*************************************************************                   
*                                                                               
SKIPLNE  NTR1                                                                   
         ZIC   R1,GETLNE           LINE NUMBER I WANT TO SKIP TO                
         ZIC   R2,LINE             CURRENT LINE                                 
         SR    R1,R2                                                            
         BZ    XIT                 ALREADY AT THE LINE I WANT                   
         BP    *+6                                                              
         DC    H'0'                LOST TRACK                                   
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SKIP+1(3),DUB+6(2)                                               
         MVC   SKIP(2),=C'BL'                                                   
         AR    R2,R1               ADD SKIP COUNT  TO LINE                      
         STC   R2,LINE                                                          
         GOTO1 PRINT,DMCB,P,SKIP   AND SKIP                                     
         B     XIT                                                              
         EJECT                                                                  
*******************************************************                         
*        ROUTINE TO ADD 1R HIGHER LEVEL NAMES TO TABLE                          
*        PARM1     R2 - CONTAINS ADDRESS OF ADDR ELEMENT                        
*        PARM2     R3 - CONTAINS ADDRESS OF NAME ELEMENT                        
*        PARM3     R4 - CONTAINS ADDRESS OF THE RECORD                          
*******************************************************                         
*                                                                               
HINAME   NTR1                                                                   
         LM    R2,R4,0(R1)                                                      
         USING ACTRECD,R4                                                       
         USING EMPD,R6                                                          
         LA    R6,WRKAREA                                                       
         MVC   WRKAREA,SPACES               CLEAR THE KEY                       
         MVC   EMPACT,ACTKACT               SAVE OFFICE LEVEL CODE              
         GOTO1 BLDTNAM,DMCB,(R3)            GET THE NAME INTO WORK              
         MVC   EMPNAM,WORK                                                      
         GOTO1 BINADD,DMCB,(R6),AEMPCDE                                         
*                                                                               
         LTR   R2,R2               WAS AN ADDRESS ELEMENT PASSED?               
         BNP   XIT                                                              
         USING ADRELD,R2                                                        
         CLI   ADREL,ADRELQ        MAKE SURE IT'S A ADDR EL                     
         BNE   XIT                                                              
         CLC   ADRADD1,SPACES                                                   
         BE    XIT                                                              
         MVC   LOCATION,ADRADD1    SAVE THE LOCATION                            
         B     XIT                                                              
         DROP  R2,R4,R6                                                         
         EJECT                                                                  
* **********************************************************                    
*        BUILD SJ INDEXED TABLES                                                
* **********************************************************                    
*                                                                               
BLDTAB   NTR1                                                                   
*                                                                               
         OC    PROFFILT,PROFFILT   IS THERE A FORCE FILTER USED                 
         BZ    BLDTB04                                                          
         OC    PRFFTVAL,PRFFTVAL   IS THERE A FORCE FILTER VALUE                
         BZ    BLDTB04                                                          
         MVI   FILTVAL,SPACE                                                    
         XC    FILTDISP,FILTDISP                                                
         LA    R2,FILTAB           TABLE TO FIGURE FORCE FILTER                 
         USING FRCFLTD,R2                                                       
BLDTB00  CLI   FRCFLT,EOT          END OF TABLE                                 
         BE    BLDTB04                                                          
         CLC   PROFFILT,FRCFLT     FILTER1 OR FILTER2 OR.......                 
         BE    BLDTB02                                                          
         LA    R2,FRCLEN(R2)                                                    
         B     BLDTB00                                                          
*                                                                               
BLDTB02  MVC   FILTVAL,PRFFTVAL    SAVE FILTER VALUE                            
         MVC   FILTDISP,FRCDISP    AND DISPLACEMENT                             
*                                                                               
BLDTB04  L     R5,AFORLST          ADDR OF FORCED SJ TABLE                      
         USING BIND,R5                                                          
         L     R1,BINTABLE         1ST TABLE ENTRY                              
         XC    BININ,BININ                                                      
         ST    R1,ALTAB                                                         
         L     R5,ACODLST          ADDR OF INDEXED SJ TABLE                     
         USING BIND,R5                                                          
         XC    BININ,BININ                                                      
         SR    R3,R3                                                            
         L     R6,BINTABLE         1ST TABLE ENTRY                              
         USING CLICDE,R6                                                        
         L     R7,ACREC            RECORD READING AREA                          
         USING ACTRECD,R7                                                       
*                                                                               
         MVC   ACTKEY,SPACES                CLEAR THE KEY                       
         MVC   ACTKCPY,RCCOMPFL             COMPANY                             
         MVC   ACTKUNT(L'SJ),SJ             UNIT/LEDGER TO READ                 
         MVI   ACTKACT,X'41'                FORCE PAST LEDGER RECORD            
         MVC   CUL,ACTKCULA                                                     
         B     *+8                                                              
BLDTB06  MVI   ACTKCULA+L'ACTKCULA,X'FF'                                        
         BAS   RE,HIGH                                                          
         CLC   CUL,ACTKEY                                                       
         BNE   BLDTB10                                                          
*                                  ALL AFTER ACCOUNT MUST BE SPACES             
         CLC   ACTKCULA+L'ACTKCULA(L'ACTKEY-L'ACTKCULA),SPACES                  
         BNE   BLDTB06             FORCE NEXT ACCOUNT                           
         ST    R6,ATABSAV          SAVE ADDR OF CURRENT TABLE ENTRY             
         MVC   CLICDE(CLILEN),SPACES                                            
         XC    CLIINDX,CLIINDX                                                  
         MVC   CLIACT,ACTKACT      ACCOUNT                                      
         GOTO1 BLDTNAM,DMCB,0      GET THE NAME INTO WORK                       
         MVC   CLINAME,WORK                                                     
         DS    0H                                                               
         L     R4,ACREC                                                         
         MVI   ELCODE,RSTELQ       STATUS ELEMENT                               
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    RSTSTAT1-RSTELD(R4),RSTSACIC+RSTSACIL  CLOSED OR LOCKED?         
         BZ    BLDTB07                                NO                        
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 DATCON,DMCB,(4,RCDATE),(0,WORK)      OLDER THAN A YEAR           
         GOTO1 ADDAY,DMCB,(C'Y',WORK),WORK+10,F'-1'                             
         XC    WORK(5),WORK                                                     
         GOTO1 DATCON,DMCB,(0,WORK+10),(1,WORK)                                 
         CLC   RSTTDATE-RSTELD(L'RSTTDATE,R4),WORK                              
         BL    BLDTB06                                YES - SKIP IT             
*                                                                               
BLDTB07  CLI   FILTVAL,SPACE       DO WE HAVE A FORCE FILTER VALUE?             
         BE    BLDTB08             NO- SKIP STATUS LOOKUP                       
         LR    R1,R4               ADDR OF STATUS ELEMENT INTO R1               
         AH    R1,FILTDISP         DISP TO FILTER                               
         CLC   0(L'FILTVAL,R1),FILTVAL                                          
         BNE   BLDTB08             PRINT ON EVERY TIMESHEET?                    
         BAS   RE,FORCEIT                                                       
BLDTB08  A     R3,=F'1'                                                         
         ST    R3,BININ                                                         
         ST    R3,CLIINDX          INDEX NUMBER                                 
         C     R3,BINMAX                                                        
         BL    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         LA    R6,CLILEN(R6)                                                    
         B     BLDTB06                                                          
*                                                                               
BLDTB10  L     R3,BININ                                                         
         LTR   R3,R3                                                            
         BP    *+6                                                              
         DC    H'0'                TABLE IS EMPTY, SOMETHINGS WRONG             
         B     XIT                                                              
         DROP  R2,R5,R6,R7                                                      
         EJECT                                                                  
* **********************************************************                    
*        LOOKUP CLIENT INDEX NUMBER                                             
* **********************************************************                    
*                                                                               
INDXUP   NTR1                                                                   
         XC    INDEX,INDEX                                                      
         L     R5,ACODLST          ADDR OF INDEXED SJ TABLE                     
         USING BIND,R5                                                          
         L     R0,BININ            NUMBER OF TABLE ENTRIES                      
         L     RF,BINTABLE         1ST TABLE ENTRY                              
         USING CLICDE,RF                                                        
INDEX02  CLC   WRKEY+L'CUL(L'WRKEY-L'CUL),CLIACT                                
         BE    INDEX04                                                          
         LA    RF,CLILEN(RF)                                                    
         BCT   R0,INDEX02                                                       
         B     XIT                 SKIP IT IF NOT FOUND                         
*                                                                               
INDEX04  MVC   INDEX,CLIINDX       PASS BACK INDEX                              
         B     XIT                                                              
         DROP  R5,RF                                                            
         EJECT                                                                  
* **********************************************************                    
*        ADD INDEX TO SORT RECORD                                               
* **********************************************************                    
*                                                                               
ADDIT    NTR1                                                                   
         OC    INDEX,INDEX                                                      
         BZ    XIT                 NOTHING TO ADD                               
         LA    R6,SRTWRK           SORT WORK AREA                               
         USING SRTD,R6                                                          
         LH    R0,SRTNUMB          CHECK IF WE ALREADY HAVE THIS CLIENT         
         LTR   R0,R0                                                            
         BNP   ADD04                                                            
         LA    R1,SRTCLTS                                                       
         CLC   0(L'SRTCLTS,R1),INDEX                                            
         BE    XIT                                                              
         LA    R1,L'SRTCLTS(R1)                                                 
         BCT   R0,*-14                                                          
ADD04    LH    R1,SRTNUMB          ADD ONE TO INDEX COUNTER                     
         AH    R1,=H'1'                                                         
         LA    R2,CLIMAX                                                        
         CR    R1,R2               COMPARE TO MAX SORT REC WILL HOLD            
         BL    *+6                                                              
         DC    H'0'                TOO MANY CLIENTS TO FIT IN SORT REC          
         STH   R1,SRTNUMB          SAVE INDEX COUNTER                           
         L     R1,ALTAB            ADDR OF SRTCLTS SLOT                         
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         MVC   0(L'SRTCLTS,R1),INDEX       INDEX NUMBER                         
         LA    R1,L'SRTCLTS(R1)                                                 
         ST    R1,ALTAB            SAVE ADDR OF NEXT ENTRY                      
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
* **********************************************************                    
*        STACK THIS EMPLOYEE'S CLIENTS UP                                       
* **********************************************************                    
*                                                                               
STAXUP   NTR1                                                                   
         LA    R6,SRTWRK                                                        
         USING SRTD,R6                                                          
         LA    R7,WRKAREA                                                       
         MVC   WRKAREA,SPACES                                                   
         MVI   FORCDONE,C'N'         SWITCH FOR FORCE TABLE                     
         USING ECLICDE,R7                                                       
*                                                                               
         LH    R2,SRTNUMB            NUMBER OF CLIENTS                          
         LTR   R2,R2                                                            
         BZ    STAX04                CHECK FORCE TABLE                          
         BP    *+6                                                              
         DC    H'0'                                                             
         LA    R4,SRTCLTS            1ST CLIENT                                 
*                                                                               
STAX02   MVC   INDEX,0(R4)           CLIENT INDEX NUMBER INTO HALF              
         MVC   ECLICDE(ECLILEN),SPACES                                          
         BAS   RE,JOBLOOK            LOOKUP JOB NAME AND NUMBER                 
         L     R5,ALTAB                                                         
         USING CLICDE,R5                                                        
         MVC   ECLINAME,CLINAME      SJ CLIENT NAME                             
         MVC   ECLIACT2,CLIACT       ACCOUNT CODE                               
         CLI   PROFAORD,C'Y'         ALPHA NAME ORDER?                          
         BE    *+10                                                             
         MVC   ECLIACT,CLIACT        ACCOUNT CODE                               
         GOTO1 BINADD,DMCB,(R7),AECDLST                                         
         LA    R4,L'SRTCLTS(R4)      NEXT CLIENT INDEX                          
         BCT   R2,STAX02                                                        
*                                                                               
STAX04   CLI   FORCDONE,C'Y'                                                    
         MVI   FORCDONE,C'Y'                                                    
         BE    XIT                                                              
         L     R5,AFORLST          ADDR OF FORCE TABLE                          
         USING BIND,R5                                                          
         L     R2,BININ            USE R0 AS COUNTER                            
         LTR   R2,R2                                                            
         BNP   XIT                                                              
         L     R4,BINTABLE         ADDRESS OF 1ST TABLE ENTRY                   
         B     STAX02                                                           
         DROP  R5,R6,R7                                                         
         EJECT                                                                  
* **********************************************************                    
*        LOOKUP CLIENT BY INDEX NUMBER                                          
* **********************************************************                    
*                                                                               
JOBLOOK  NTR1                                                                   
         L     R5,ACODLST          ADDR OF INDEXED SJ TABLE                     
         USING BIND,R5                                                          
         L     RF,BINTABLE         1ST TABLE ENTRY                              
         USING CLICDE,RF                                                        
         L     R1,INDEX            CLIENT INDEX NUMBER                          
         S     R1,=F'1'            LESS ONE                                     
         BNM   *+6                                                              
         DC    H'0'                                                             
         C     R1,BININ            CANT BE MORE THAN # IN THE TABLE             
         BNH   *+6                                                              
         DC    H'0'                                                             
         ST    R1,FULL                                                          
         LA    R3,CLILEN                                                        
         M     R2,FULL             TIMES LENGTH OF RECORD                       
         AR    RF,R3                                                            
         ST    RF,ALTAB                                                         
*                                                                               
         B     XIT                                                              
         DROP  R5,RF                                                            
         EJECT                                                                  
*******************************************                                     
*         GETTING THE NAME INTO WORK                                            
*******************************************                                     
*                                                                               
BLDTNAM  NTR1                                                                   
         L     R2,0(R1)            DID WE PASS ADDR OF ELEMENT                  
         LTR   R2,R2                                                            
         BNZ   BLDTNM3                                                          
         L     R2,ACREC                                                         
         AH    R2,DATADISP                                                      
BLDTNM1  CLI   0(R2),0                                                          
         BE    BLDTNM9                                                          
         CLI   0(R2),NAMELQ        NAME ELEMENT                                 
         BE    BLDTNM3                                                          
         ZIC   R0,1(R2)                                                         
         AR    R2,R0                                                            
         LTR   R0,R0                                                            
         BP    BLDTNM1                                                          
         DC    H'0'                                                             
         USING NAMELD,R2                                                        
BLDTNM3  MVC   WORK,SPACES                                                      
         IC    R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         BM    BLDTNM9                                                          
         EXMVC R1,WORK,NAMEREC                                                  
BLDTNM9  B     XIT                                                              
         DROP  R2                                                               
         EJECT                                                                  
****************************************************************                
*        BUILD INDEX TABLE OF JOBS FOR ALL TIME SHEETS                          
****************************************************************                
FORCEIT  NTR1                                                                   
         L     R5,AFORLST          ADDR OF FORCE TABLE                          
         USING BIND,R5                                                          
         L     R4,ALTAB            ADDRESS OF NEXT TABLE ENTRY                  
         USING FORCD,R4                                                         
         L     R3,BININ            USE R3 AS COUNTER                            
         L     R6,ATABSAV          ADDRESS OF THE SJ INDEX ENTRY                
         USING CLICDE,R6                                                        
         MVC   FORINDX,CLIINDX     INDEX NUMBER OF SJ JOB TO FORCE              
         LA    R4,FORLEN(R4)       SET FOR NEXT ENTRY IN TABLE                  
         ST    R4,ALTAB            SAVE THE ADDR OF THE NEXT ENTRY              
         A     R3,=F'1'            UPDATE COUNTER                               
         C     R3,BINMAX           COMPARE TO MAX ENTRIES                       
         BL    *+6                 GO READ NEXT RECORD                          
         DC    H'0'                TABLE IS FULL                                
         ST    R3,BININ            UPDATE NUMBER IN TABLE (BININ)               
         B     XIT                                                              
         DROP  R4,R5,R6                                                         
         EJECT                                                                  
****************************************************************                
*              ADD ITEM TO BINSRCH TABLE                                        
*              P1                  A(ITEM TO BE ADDED)                          
*              P2                  A(TABLE)                                     
****************************************************************                
*                                                                               
         USING BIND,R5                                                          
BINADD   NTR1                                                                   
         L     R5,4(R1)                                                         
         MVC   DMCB+8(16),BININ    NUMBER LENGTH,KEY,MAX                        
         L     R6,BINTABLE         A(TABLE)                                     
         L     R4,0(R1)            A(ITEM)                                      
         GOTO1 BINSRCH,DMCB,(X'01',(R4)),(R6)                                   
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         B     XIT                                                              
         EJECT                                                                  
*******************************************                                     
*              DATAMGR INTERFACE                                                
*******************************************                                     
*                                                                               
*                                                                               
HIGH     MVC   COMMAND,=C'DMRDHI'                                               
         MVC   SAVEKEY,0(R7)                                                    
         B     GTREC                                                            
*                                                                               
SEQ      MVC   COMMAND,=C'DMRSEQ'                                               
*                                                                               
GTREC    NTR1                                                                   
         L     R7,ACREC                                                         
         GOTO1 DATAMGR,DMCB,COMMAND,=C'ACCOUNT',(R7),(R7)                       
         B     XIT                                                              
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
******************************                                                  
*        CONSTANTS                                                              
******************************                                                  
*                                                                               
RELOTAB  DS    0A                                                               
         DC    A(RECORD)                                                        
         DC    A(CODLIST)                                                       
         DC    A(ECDLIST)                                                       
         DC    A(FORLIST)                                                       
         DC    A(EMPCODE)                                                       
         DC    X'FF'                                                            
*                                                                               
SJ       DC    CL2'SJ'                                                          
NINES    DC    CL12'99999999999'                                                
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
SORTCARD DC    CL80'SORT FIELDS=(1,000,A),FORMAT=BI,WORK=1'                     
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=(000,,,)'                              
*                                                                               
CODLIST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(CLILEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(CLIKLN)         KEY LENGTH                                   
         DC    AL4(CODNUM)         MAX IN TABLE                                 
CODTAB   DS    AL4(0)              ADDRESS OF THE TABLE                         
*                                                                               
*                                                                               
ECDLIST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(ECLILEN)        RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(ECLIKLN)        KEY LENGTH                                   
         DC    AL4(ECLIMAX)        MAX IN TABLE                                 
ECODTAB  DS    AL4(0)              ADDRESS OF THE TABLE                         
*                                                                               
FORLIST  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(FORLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(FORKLN)         KEY LENGTH                                   
         DC    AL4(FORNUM)         MAX IN TABLE                                 
FORTAB   DS    AL4(0)              ADDRESS OF THE TABLE                         
*                                                                               
EMPCODE  DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(EMPLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(EMPKLN)         KEY LENGTH                                   
         DC    AL4(EMPNUM)         MAX IN TABLE                                 
EMPTAB   DS    AL4(0)              ADDRESS OF THE TABLE                         
         EJECT                                                                  
MAINTAB  DS    0D                                                               
         DC    AL4((((CODNUM*CLILEN)+7)/8)*8),A(CODTAB)                         
         DC    AL4((((ECLIMAX*ECLILEN)+7)/8)*8),A(ECODTAB)                      
         DC    AL4((((FORNUM*FORLEN)+7)/8)*8),A(FORTAB)                         
         DC    AL4((((EMPNUM*EMPLEN)+7)/8)*8),A(EMPTAB)                         
         DC    X'FF'                                                            
*                                                                               
FILTAB   DS    0D                                                               
         DC    AL1(1),AL1(0),Y(RSTFILT1-RSTELD)                                 
         DC    AL1(2),AL1(0),Y(RSTFILT2-RSTELD)                                 
         DC    AL1(3),AL1(0),Y(RSTFILT3-RSTELD)                                 
         DC    AL1(4),AL1(0),Y(RSTFILT4-RSTELD)                                 
         DC    AL1(5),AL1(0),Y(RSTFILT5-RSTELD)                                 
         DC    AL1(EOT)                                                         
*                                                                               
RECORD   DS    0D                                                               
         DS    CL42                                                             
         DS    CL2000                                                           
*                                                                               
         DS    0D                                                               
SRTWRK   DS    CL(SRTLEN)                                                       
*                                                                               
CODNUM   EQU   120000              MAX CLIENTS IN SJ TABLE                      
CLIMAX   EQU   110                 MAX CLIENTS PER EMPLOYEE                     
* DCUR * INCREASED TO 110 FROM 100 ON 11/27/01                                  
ECLIMAX  EQU   CLIMAX+FORNUM       MAX CLIENTS PER EMPLOYEE                     
FORNUM   EQU   40                  MAX FORCED CLIENTS                           
EMPNUM   EQU   4000                MAX 1R HIGHER LEVELS                         
*                                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
************************************************                                
*         D S E C T S                                                           
************************************************                                
*                                                                               
*                                                                               
*              DSECT FOR WORKING STORAGE                                        
ACCTD    DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
ACREC    DS    A                                                                
ACODLST  DS    A                                                                
AECDLST  DS    A                                                                
AFORLST  DS    A                                                                
AEMPCDE  DS    A                                                                
*                                                                               
ATABSAV  DS    A                                                                
ALTAB    DS    A                                                                
*                                                                               
MAINLEN  DS    F                                                                
MAINBGN  DS    F                                                                
*                                                                               
COMMAND  DS    CL6                                                              
DA       DS    XL4                                                              
WKOFFICE DS    CL2                 SAVE OFFICE CODE                             
CALENDAR DS    CL1                 CALENDAR SWITCH                              
ELCODE   DS    CL1                                                              
PERIOD   DS    CL19                                                             
PERIODCP DS    CL19                CORP PERIOD                                  
STDATE   DS    CL6                 START DATE                                   
EDDATE   DS    CL6                 END DATE                                     
START    DS    XL3                 QSTART YMD                                   
END      DS    XL3                 QEND YMD                                     
MAXROW   DS    CL1                                                              
GETLNE   DS    CL1                                                              
PGEMAX   DS    CL1                                                              
SKIP     DS    CL4                                                              
OLDATE   DS    CL3                                                              
CUL      DS    CL3                                                              
WRKEY    DS    CL15                                                             
SAVEKEY  DS    CL42                                                             
WRKAREA  DS    CL100                                                            
INDEX    DS    AL4                                                              
FILTDISP DS    H                     DISP TO FILTER IN STATUS ELEMENT           
FILTVAL  DS    CL(L'RSTFILT1)        FILTER VALUE                               
FORCDONE DS    CL1                   FORCE TABLE SWITCH                         
LOCATION DS    CL(L'ADRADD1)         FLOOR NUMBER WHERE EMPLOYEE WORKS          
OFDPSB   DS    CL(L'SRTHI)                                                      
OFFCODE  DS    CL3                   SAVE OFFICE CODE                           
DEPCODE  DS    CL3                   SAVE DEPT CODE                             
SUBCODE  DS    CL3                   SAVE SUB CODE                              
OFFNAME  DS    CL(L'EMPNAM)                                                     
DEPNAME  DS    CL(L'EMPNAM)                                                     
SUBNAME  DS    CL(L'EMPNAM)                                                     
LEVSAV   EQU   *-OFDPSB              LENGTH OF SAVED LEVELS                     
*                                                                               
LLEVELS  EQU   *                                                                
LLEVA    DS    CL(L'ACLVLEN)         1R LEV A LENGTH                            
LLEVANAM DS    CL(L'ACLVDESC)        1R LEV A NAME                              
LLEVALN  EQU   *-LLEVELS                                                        
LLEVB    DS    CL(L'ACLVLEN)         1R LEV B LENGTH (A+B)                      
LLEVBNAM DS    CL(L'ACLVDESC)        1R LEV B NAME                              
LLEVC    DS    CL(L'ACLVLEN)         1R LEV C LENGTH (A+B+C)                    
LLEVCNAM DS    CL(L'ACLVDESC)        1R LEV C NAME                              
LLEVD    DS    CL(L'ACLVLEN)         1R LEV D LENGTH (A+B+C+D)                  
LLEVDNAM DS    CL(L'ACLVDESC)        1R LEV D NAME                              
LLEVELLN EQU   *-LLEVELS                                                        
LLEVLNUM EQU   LLEVELLN/LLEVALN                                                 
*                                                                               
NUMLEVLS DS    XL1        NUMBER OF LEVELS IN 1R                                
LENLEVLS EQU   *                                                                
LENLEVA  DS    XL1        REAL LENGTH OF LEVEL A                                
LENLEVB  DS    XL1        REAL LENGTH OF LEVEL B                                
LENLEVC  DS    XL1        REAL LENGTH OF LEVEL C                                
LENLEVD  DS    XL1        REAL LENGTH OF LEVEL D                                
LENLEVLN EQU   *-LENLEVLS                                                       
LENLVNUM EQU   LENLEVLN/L'LENLEVA                                               
         EJECT                                                                  
*              DSECT FOR PROFILES                                               
*                                                                               
PROFD    DSECT                                                                  
PROFROWS DS    CL1             CLIENT ROWS PRE PAGE                             
PROFDAYS DS    CL1             DAYS FOR PREVIOUS CODES 0-90                     
PROFSAL  DS    CL1             NO SALARY REQUIRED Y,N                           
PROFFILT DS    CL1             FILTER TO FORCE JOBS ON TIMESHEET                
PRFFTVAL DS    CL1             VALUE OF FORCE FILTER                            
PROFAORD DS    CL1             CLIENTS IN NAME ALPHA ORDER                      
PROFLOK  DS    CL1             SUPPRESS LOCKED 1R ACCOUNTS Y,N                  
PRFLINUP DS    CL1             NUMBER OF LINEUP PAGES 2-5                       
*                                                                               
*                                                                               
*              DSECT FOR BINSRCH PARAMETERS                                     
*                                                                               
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
*                                                                               
*                                                                               
*              DSECT FOR INDEXED CODE LIST TABLE                                
*                                                                               
CLICDE   DSECT                                                                  
CLIINDX  DS    F                                                                
CLIACT   DS    CL12                                                             
CLIKLN   EQU   *-CLICDE                                                         
CLINAME  DS    CL(L'PRNTDES)                                                    
CLILEN   EQU   *-CLICDE                                                         
*                                                                               
*                                                                               
*              DSECT FOR EMPLOYEE'S INDEXED CODE LIST TABLE                     
*                                                                               
ECLICDE  DSECT                                                                  
ECLIACT  DS    CL12                                                             
ECLINAME DS    CL(L'PRNTDES)                                                    
ECLIACT2 DS    CL12                                                             
ECLIKLN  EQU   *-ECLICDE                                                        
ECLILEN  EQU   *-ECLICDE                                                        
*                                                                               
*              DSECT FOR FORCED ON EVERY TIMESHEET TABLE                        
*                                                                               
FORCD    DSECT                                                                  
FORINDX  DS    AL4                                                              
FORKLN   EQU   *-FORCD                                                          
FORLEN   EQU   *-FORCD                                                          
         EJECT                                                                  
*              DSECT FOR 1R LEVEL NAMES                                         
*                                                                               
EMPD     DSECT                                                                  
EMPACT   DS    CL12                EMPLOYEE HIGHER LEVEL ACCOUNT                
EMPKLN   EQU   *-EMPD                                                           
EMPNAM   DS    CL36                HIGHER LEVEL ACCOUNT NAME                    
EMPLEN   EQU   *-EMPD                                                           
*                                                                               
*                                                                               
*              DSECT FOR SORTED FILE                                            
*                                                                               
SRTD     DSECT                                                                  
SRTKEY   DS    0C                                                               
SRTHI    DS    CL12                                                             
SRTEMP   DS    CL7                 EMPLOY CODE (SPACES IF ALPHABETICAL)         
SRTNAM   DS    CL36                                                             
SRTEMPL  DS    CL7                                                              
SRTEMLN  EQU   *-SRTEMP            LENGTH OF SORT EMPLOYEE INFO                 
*                                                                               
SRTKLEN  EQU   *-SRTD              LENGTH OF SORT RECORD KEY                    
SRTLOC   DS    CL(L'ADRADD1)       FLOOR NUMBER WHERE EMPLOYEE WORKS            
         DS    0H                                                               
SRTNUMB  DS    H                   NUMBER OF CLIENT INDEX ENTRIES               
SRTCLTS  DS    (CLIMAX)AL4         CLIENT INDEX ENTRIES                         
SRTCLN   EQU   *-SRTCLTS           LENGTH OF CLIENT INDEX                       
SRTLEN   EQU   *-SRTD              LENGTH OF SORT RECORD                        
*                                                                               
*                                                                               
*              DSECT FOR HEADLINES                                              
*                                                                               
HEADD    DSECT                                                                  
         DS    CL10                SPACES                                       
HEADOFF  DS    CL3                 1R OFFICE                                    
         ORG   HEADOFF                                                          
HEADDPT  DS    CL3                 1R DEPT                                      
         ORG   HEADOFF                                                          
HEADSUB  DS    CL3                 1R SUB DEPT                                  
         ORG   HEADOFF                                                          
HEADEMP  DS    CL(L'SRTEMPL)       EMPLOYEE NUMBER                              
HEADDESC DS    CL(L'EMPNAM)        DECSRIPTION                                  
         DS    CL7                 SPACES                                       
HEADPER  DS    CL18                PERIOD                                       
         ORG   HEADPER                                                          
HEADLOC  DS    CL20                LOCATION                                     
HEADLN   EQU   *-HEADD                                                          
         EJECT                                                                  
*              DSECT FOR PRINTLINE                                              
*                                                                               
PRNTD    DSECT                                                                  
         DS    CL1                 SPACES                                       
PRNTDES  DS    CL22                JOB DESCRIPTION                              
         DS    CL2                 SPACES                                       
PRNTCOD  DS    CL12                ACCOUNT CODE                                 
PRNTLNQ  EQU   *-PRNTD                                                          
*                                                                               
*                                                                               
*              DSECT FOR FORCE FILTERING                                        
*                                                                               
FRCFLTD  DSECT                                                                  
FRCFLT   DS    AL1                 FILTER TO USE                                
         DS    AL1                 SPARE                                        
FRCDISP  DS    Y                   DISP TO FILTER VALUE                         
FRCLEN   EQU   *-FRCFLTD                                                        
         EJECT                                                                  
**************************************                                          
*        EQUATES                                                                
**************************************                                          
*                                                                               
*                                                                               
MAXLEN   EQU   12                  MAX LENGTH OF AN ACCOUNT                     
OFFLNE   EQU   1                   LINE NUMBER TO PRINT OFFICE                  
DPTLNE   EQU   2                   LINE NUMBER FOR DEPT                         
PERLNE   EQU   2                   LINE NUMBER FOR PERIOD                       
SUBLNE   EQU   3                   LINE NUMBER FOR SUB                          
EMPLNE   EQU   4                   LINE NUMBER FOR EMPLOYEE                     
LOCLNE   EQU   4                   LINE NUMBER FOR LOCATION                     
XXXLNE   EQU   7                   LINE NUMBER FOR DETAIL LINEUP                
DETLNE   EQU   7                   LINE NUMBER FOR DETAIL                       
DETMAX   EQU   36                  LAST DETAIL LINE                             
DAYSBAK  EQU   90                  DEFAULT DAYS BACK                            
EOT      EQU   X'FF'               END OF TABLE                                 
SPACE    EQU   X'40'               SINGLE SPACE                                 
XXX      EQU   C'X'                X'S FOR LINEUP                               
*                                                                               
*ACGENFILE                                                                      
*ACGENMODES                                                                     
*ACREPWORKD                                                                     
*ACMASTD                                                                        
*DDREPMASTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACMASTD                                                        
       ++INCLUDE DDREPMASTD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'055ACREPCT02 05/23/16'                                      
         END                                                                    
