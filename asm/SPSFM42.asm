*          DATA SET SPSFM42    AT LEVEL 025 AS OF 08/02/02                      
*PHASE T21742A                                                                  
T21742   TITLE 'SPSFM42 - PROFIT WITHIN LIST OVERLAY'                           
***********************************************************************         
*============================= UPDATE LOG ============================*         
*   Date   Lvl User   Description                                     *         
* -------- --- ----   ------------------------------------------------*         
* Mar15/02 024 MCHO - Changed screens from sp* to sc*                 *         
*                                                                     *         
* Oct25/99 023 GLEE - Clear replace text field                        *         
*                                                                     *         
* Sep15/99 022 GLEE - Fix RUP# routine to handle very large numbers   *         
*                                                                     *         
* Sep07/99 021 GLEE - Disable PF3 key/Restore function                *         
*                                                                     *         
* Aug19/99 020 GLEE - Disallow product code to end in "#"--it's       *         
*                      reserved for trade products                    *         
*                                                                     *         
* Nov09/98 019 GLEE - Handle case where LMK# routine returns nulls    *         
*                                                                     *         
* Aug06/98 018 GLEE - Because MOBILE now passes SPOTPROF to GETBROAD, *         
*                      clear SPOTPROF+8 when getting actual b'cst mths*         
*                                                                     *         
* May06/98 017 GLEE - Fix bug that reads EST headers in GBI# routine  *         
*                                                                     *         
* Apr03/98 016 GLEE - Fix bug in GMG# routine                         *         
*                                                                     *         
* Oct29/97 015 GLEE - Check for "redo lock" flag in PWDTISLD          *         
*                                                                     *         
* Sep15/97 014 GLEE - Alter meaning of ATF1PGLS flag                  *         
*                                                                     *         
* Jul24/97 013 GLEE - Check for potentially deferrable spots          *         
*                                                                     *         
* Jul11/97 012 GLEE - For BLIST, list only those markets having       *         
*                      buy activity for the month specified           *         
*                                                                     *         
* Jul11/97 011 GLEE - Ignore buy records for spill markets            *         
*                                                                     *         
* Jul09/97 010 GLEE - Being pendantic about reading buys              *         
*                                                                     *         
* Jul09/97 009 GLEE - Set up customized GETEL routine in SUBR03       *         
*                                                                     *         
* Jul09/97 008 GLEE - List using PW records as well                   *         
*                   - Don't list market w/o buys and locked dollars   *         
*                                                                     *         
* Jul09/97 007 GLEE - Remove trap in SDC# for BRDMTHTB & BRDWKTB2     *         
*                   - Fixed flaw in logic for out-of-week rotators    *         
*                                                                     *         
* Jun06/97 006 GLEE - Fix flaw with getting market w/ buys logic      *         
*                                                                     *         
* Mar27/97 005 GLEE - Fix "month w/in estimate period" validation bug *         
*                                                                     *         
* Jul17/95 004 GLEE - Read BUY records to check for deletion.         *         
*                                                                     *         
* May26/95 003 GLEE - Format locked-PW% field w/ right info.          *         
*                                                                     *         
* May26/95 002 GLEE - Remove test which checks Locked CLT$=0 when     *         
*                      Locked WIM$<>0 in FMTLKPW routine.             *         
*                   - Suppress PW functions for WILA.                 *         
*                   - Change GTMKTGOL to look for x'21' element.      *         
*                   - Display c'N' (not c'+') if no goals at all.     *         
*                                                                     *         
* May12/95 001 GLEE - New program for Profit Within listing           *         
*                                                                     *         
***********************************************************************         
         EJECT                                                                  
T21742   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T21742*,R7,RR=RE                                              
                                                                                
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         ST    RE,RELO                                                          
         ST    RB,BASE1                                                         
         ST    R7,BASE2                                                         
         BAS   RE,MYINIT           INITIALIZE VALUES FIRST                      
                                                                                
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VK                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
                                                                                
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*========================== INITIALIZE VALUES ========================*         
MYINIT   NTR1                                                                   
                                                                                
         GOTO1 DATCON,DMCB,(X'03',BTODAY),(X'02',CTODAY),0                      
         MVC   RELOCKSW,DCRELOCK   DATE-FLAG TO FLAG NEED TO RE-LOCK            
                                                                                
         DS    0H                  SET ADDRESSES OF TABLES & ROUTINES           
         LA    R0,ATABSQ           R0=# OF ADDRESSES TO SET                     
         LH    RE,=Y(TTABDSP-T21742)                                            
         A     RE,BASE1            RE-->DISPL OF TABLES                         
         LA    RF,ATABS            RF-->PLACE TO STORE ADDRESSES                
MINIT10A SR    R1,R1                                                            
         ICM   R1,3,0(RE)          PICK DISPLACEMENT UP                         
         A     R1,BASE1             ADD BASE ONTO IT                            
         ST    R1,0(RF)             AND STORE THE ADDRESS                       
         LA    RE,L'TTABDSP(RE)                                                 
         LA    RF,L'ATABS(RF)                                                   
         BCT   R0,MINIT10A                                                      
                                                                                
                                                                                
         DS    0H                  SET LABELS IN SPOT STORAGE AREA              
         LA    R0,LBLTABQ          R0=# OF LABELS TO SET                        
         L     RF,ALABLTB                                                       
         LA    R1,ATIATBS                                                       
MINIT20  SR    RE,RE                                                            
         ICM   RE,3,0(RF)                                                       
         A     RE,ATIA                                                          
         MVC   0(8,RE),4(RF)       SET LABEL IN TIA TABLE                       
         SR    RE,RE                                                            
         ICM   RE,3,2(RF)                                                       
         A     RE,ATIA                                                          
         ST    RE,0(R1)            STORE ADDRESS OF TIA TABLE                   
         LA    R1,4(R1)            BUMP POINTERS                                
         LA    RF,L'LBLTAB(RF)                                                  
         BCT   R0,MINIT20                                                       
                                                                                
                                                                                
         DS    0H                  SET ADDRESSES OF CORE-RES PHASES             
         LA    R0,COREQLSQ         R0=# OF CORE-RES PHASES TO GET               
         L     R3,ACORQLST         R3-->LIST OF CORE-RES PHASES                 
         LA    R4,ACORES           R4-->START OF PLACE TO PUT THEM              
         ICM   R2,14,=X'D9000A'                                                 
MINIT30  IC    R2,0(R3)                                                         
         GOTO1 CALLOV,DMCB,0,(R2),0                                             
         CLI   4(R1),XFF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   0(4,R4),0(R1)                                                    
         LA    R3,1(R3)                                                         
         LA    R4,4(R4)                                                         
         BCT   R0,MINIT30                                                       
                                                                                
                                                                                
         DS    0H                  SET MOBILE ADCON LIST                        
         MVC   AADDAY,ADDAY                                                     
         MVC   ADATCON,DATCON                                                   
         MVC   AGETDAY,GETDAY                                                   
                                                                                
                                                                                
         DS    0H                                                               
         MVI   USEIO,C'N'          FREE UP LASTSELK FOR MY USE                  
         MVC   AIO,AIO1            SET AIO TO IO AREA 1                         
         OI    CONSERVH+6,X80+X01                                               
         OI    GENSTAT1,NOSETEFH   PASS KEYS W/ PFKEYS                          
         OI    GENSTAT4,NODELLST   DO NOT ALLOW DELETES FROM LIST               
                                                                                
                                                                                
         DS    0H                  SET (ROW#-1) OF CURSOR POSITION              
         L     R1,SYSPARMS                                                      
         L     RF,0(R1)            RF-->TIO                                     
         USING TIOBD,RF                                                         
         LH    R1,TIOBCURS                                                      
         DROP  RF                                                               
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         STC   R1,CURSROW          CURSROW = ROW# - 1 OR CURSOR POS             
                                                                                
                                                                                
         CLC   TWAAGY,=C'WI'       SUPPRESS PW FUNCTIONS IF AGENCY              
         BC    0,PWNAVE             IS WILA (WHEN ASKED FOR)                    
                                                                                
                                                                                
         DS    0H                  BUYER VS. BILLER LIST SCREEN                 
         MVI   GOSUBN,SMB#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
         NI    CHNGFLG1,XFF-CF1ACTEQ                                            
         CLC   SVACTEQU,ACTEQU     IF ACTION EQUATE CHANGED,                    
         BE    MINIT40                                                          
         OI    CHNGFLG1,CF1ACTEQ    FLAG FOR CHANGE,                            
         MVC   SVACTEQU,ACTEQU      SAVE ACTEQU,                                
         MVI   GOSUBN,SCR#          AND SET SCREEN LABELS, ETC.                 
         GOTO1 AGOSUB                                                           
                                                                                
                                                                                
MINIT40  DS    0H                                                               
         B     XIT                                                              
                                                                                
                                                                                
*--------------------------- MYINIT ERRORS ---------------------------*         
PWNAVE   DS    0H                                                               
         LA    R2,CONRECH                                                       
         MVI   MYERRCD,PWNAVQ                                                   
         B     MYERROR                                                          
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST OVERLAY (VALKEY)'                  
***********************************************************************         
*======================== VALIDATE KEY ROUTINE =======================*         
VK       DS    0H                                                               
         NI    CHNGFLG1,XFF-CF1KEY                                              
         XC    MYTEXT(MYTEXTL),MYTEXT                                           
                                                                                
*                                                                               
*--------------------------- VALIDATE MEDIA --------------------------*         
*                                                                               
         LA    R2,PWLMEDH          MEDIA                                        
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGE?                   
         GOTO1 VALIMED                                                          
         OI    4(R2),X20                                                        
                                                                                
         L     R6,AIO                                                           
         USING AGYHDRD,R6                                                       
         MVC   MYAGYID,AGYID       HOLD ONTO AGENCY ID                          
         DROP  R6                                                               
*                                                                               
*-------------------------- VALIDATE CLIENT --------------------------*         
*                                                                               
         LA    R2,PWLCLTH          CLIENT                                       
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGE?                   
         GOTO1 VALICLT                                                          
         OI    4(R2),X20                                                        
                                                                                
         DS    0H                  SET SPOT PROFILE                             
         MVC   WORK(12),=CL12'S000'                                             
         MVC   WORK+4(3),MYAGYID                                                
         MVC   WORK+7(3),QCLT                                                   
         L     R6,AIO                                                           
         USING CLTHDRD,R6                                                       
         CLI   COFFICE,X'41'                                                    
         BL    *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         DROP  R6                                                               
         GOTO1 GETPROF,DMCB,WORK,SPOTPROF,DATAMGR                               
*                                                                               
*-------------------------- VALIDATE PRODUCT -------------------------*         
*                                                                               
         LA    R2,PWLPRDH          PRODUCT                                      
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGE?                   
                                                                                
         MVI   MYERRCD,MFLDQ       A PRODUCT IS REQUIRED                        
         CLI   5(R2),0                                                          
         BE    MYERROR                                                          
                                                                                
         MVI   MYERRCD,NPOLQ       MAKE SURE POL WAS NOT REQUESTED              
         CLC   8(3,R2),=C'POL'                                                  
         BE    MYERROR                                                          
                                                                                
         DS    0H                  DISALLOW TRADE PRODUCTS                      
         ZIC   R1,5(R2)                                                         
         LA    R1,(8-1)(R1,R2)      R1-->LAST CHAR OF INPUT                     
         CLI   0(R1),C'#'                                                       
         BNE   VKPRD049                                                         
         MVI   MYERRCD,IPRD2Q       SET ERROR CODE                              
         MVC   MYTEXT+1(6),=C'for PW'  AND EXTRA TEXT                           
         MVI   MYTEXT+0,6                                                       
         B     MYERROR                                                          
VKPRD049 EQU   *                                                                
                                                                                
         GOTO1 VALIPRD                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
*------------------------- VALIDATE ESTIMATE -------------------------*         
*                                                                               
         LA    R2,PWLESTH          ESTIMATE                                     
         BAS   RE,KYCHNGED         DID THIS KEY FIELD CHANGE?                   
         XC    PWLESDT,PWLESDT     CLEAR FIELD THAT DISPLAYS                    
         OI    PWLESDTH+6,X80       THE ESTIMATE STRT/END DATES                 
                                                                                
         GOTO1 VALIEST                                                          
         OI    4(R2),X'20'                                                      
                                                                                
         L     R6,AIO                                                           
         USING ESTHDRD,R6                                                       
         SR    R1,R1                                                            
         ICM   R1,7,EPWPCT         CHECK IF PW% EXISTS                          
         BZ    MPCTE                ERROR IF NONE                               
         N     R1,=X'007FFFFF'     IN CASE PW% = 0%                             
         ST    R1,OPWPCT           HOLD ONTO PW%                                
         EDIT  (R1),(7,PWLPRCT),2,ZERO=NOBLANK,FLOAT=-                          
         OI    PWLPRCTH+6,X80       AND DISPLAY IT                              
                                                                                
         MVC   ESDATE(L'ESTART+L'EEND),ESTART    EST START/END DATES            
         DS    0H                  GET ESTIMATE DATES IN BINARY                 
         GOTO1 DATCON,DMCB,(0,ESDATE),(3,ESDATEB),0                             
         GOTO1 (RF),(R1),(0,EEDATE),(3,EEDATEB),0                               
         DS    0H                  DISPLAY EST DATES ON SCREEN                  
         GOTO1 (RF),(R1),(X'10',ESTART),(5,PWLESDT),0                           
                                                                                
         CLI   EOWSDAY,0           OUT-OF-WEEK START DAY                        
         BE    *+10                                                             
         MVC   SPOTPROF+8(1),EOWSDAY                                            
         DROP  R6                                                               
*                                                                               
*-------------------------- VALIDATE MARKET --------------------------*         
*                                                                               
         NI    CHNGFLG1,XFF-CF1MKT                                              
         XC    SVINPMKT,SVINPMKT                                                
         LA    R2,PWLMKTH          MARKET                                       
                                                                                
         BAS   RE,FLDCHNG          DID THIS FIELD CHANGE?                       
         BNE   *+8                  NOPE, DON'T TURN FLAG ON                    
         OI    CHNGFLG1,CF1MKT                                                  
                                                                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VK50                                                             
         GOTO1 VALIMKT                                                          
         MVC   SVINPMKT,BMKT        YES, USE AS START-AT FIELD                  
                                                                                
VK50     DS    0H                                                               
         OI    4(R2),X20                                                        
         B     VK60                                                             
                                                                                
                                                                                
VK60     DS    0H                                                               
         MVI   MYERRCD,0                                                        
         B     VKOPT                                                            
         EJECT                                                                  
*                                                                               
*---------------------- VALIDATE OPTIONS FIELD ----------------------*          
*                                                                               
VKOPT    DS    0H                                                               
         LA    R2,PWLOPTNH                                                      
                                                                                
         NI    CHNGFLG1,XFF-CF1OPT                                              
         BAS   RE,FLDCHNG          DID THIS FIELD CHANGE?                       
         BNE   *+8                                                              
         OI    CHNGFLG1,CF1OPT      YES, FLAG THAT IT DID                       
                                                                                
         BAS   RE,VALOPTS                                                       
         BNE   MYERROR                                                          
                                                                                
         CLI   WHOLIST,C'B'        FOR ACTION=BLIST                             
         BNE   *+12                                                             
         CLI   BILLYRMT+1,0         A MONTH INPUT IN OPT FIELD REQUIRED         
         BE    MTRQE                                                            
                                                                                
         OI    4(R2),X20           FIELD HAS BEEN VALIDATED                     
         B     VKFLT                                                            
*                                                                               
*----------------------- VALIDATE FILTER FIELD ----------------------*          
*                                                                               
VKFLT    DS    0H                                                               
         LA    R2,PWLFLTRH                                                      
                                                                                
         NI    CHNGFLG1,XFF-CF1FLT                                              
         BAS   RE,FLDCHNG          DID THIS FIELD CHANGE?                       
         BNE   *+8                                                              
         OI    CHNGFLG1,CF1FLT      YES, FLAG THAT IT DID                       
                                                                                
         BAS   RE,VALFLTS                                                       
         BNE   MYERROR                                                          
         OI    4(R2),X20           FIELD HAS BEEN VALIDATED                     
         B     VKBUYR                                                           
*                                                                               
*--------------------- VALIDATE BUYER'S INITIALS --------------------*          
*                                                                               
VKBUYR   DS    0H                                                               
         MVC   BUYRID,SPACES                                                    
         LA    R2,PWLBUYRH                                                      
                                                                                
         NI    CHNGFLG1,XFF-CF1BYR                                              
         BAS   RE,FLDCHNG          DID THIS FIELD CHANGE?                       
         BNE   *+8                                                              
         OI    CHNGFLG1,CF1BYR      YES, FLAG THAT IT DID                       
                                                                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VKBUYRX              NOPE, EXIT VALIDATE BUYER                   
         MVI   MYERRCD,IFLDQ       ASSUME ERROR                                 
         CLI   5(R2),2             NEED AT LEAST 2-CHAR ID                      
         BL    MYERROR                                                          
         MVI   GOSUBN,CAN#         HAS TO BE ALPHANUMERIC                       
         GOTO1 AGOSUB                                                           
         BNE   MYERROR                                                          
         CLI   8(R2),C'0'          1ST CHAR MUST BE ALPHABETIC                  
         BNL   MYERROR                                                          
                                                                                
         DS    0H                  SAVE THE BUYER'S INITIALS                    
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,BUYRID,8(R2)                                                  
*                                                                               
VKBUYRX  DS    0H                                                               
         OI    4(R2),X20           FIELD IS VALIDATED                           
         B     VK100                                                            
         EJECT                                                                  
*                                                                               
*----------------------- VALKEY PRE-EXIT TASKS ----------------------*          
*                                                                               
VK100    DS    0H                                                               
         TM    CHNGFLG1,CF1KEY     WAS KEY CHANGED?                             
         BZ    VK110                NO, DON'T RE-INITIALIZED                    
         NI    MISCFLG1,XFF-MF1BLTAB                                            
                                                                                
VK110    DS    0H                                                               
         TM    CHNGFLG1,CF1AKOFM   ACTEQU,KEY,OPT,MKT,FLTR, CHNGE?              
         BZ    VK150                NO, DON'T RE-INITIALIZE                     
         NI    MISCFLG1,XFF-MF1LSTDN                                            
         NI    SELFLAG1,XFF-SF1MNSEL                                            
         NI    SELFLAG1,XFF-SF1TSLOK                                            
         NI    SELFLAG1,XFF-SF1SELCT                                            
         MVI   LKALLMDE,C'N'                                                    
         MVI   LKALLDNE,C'N'                                                    
         MVI   GOSUBN,ISL#         CLEAR OUT SEL FIELDS                         
         GOTO1 AGOSUB                                                           
*                                                                               
VK150    DS    0H                                                               
         MVI   GOSUBN,TSL#         TEST SELECT FIELDS                           
         GOTO1 AGOSUB                                                           
         ICM   R2,15,FULL           A(FLD W/ ERROR) RETURNED IN FULL            
         BNZ   MYERROR                                                          
                                                                                
         MVI   GOSUBN,FSL#         FILL SEL FIELDS (IF NEEDED)                  
         GOTO1 AGOSUB                                                           
                                                                                
         MVI   GOSUBN,VSL#         VALIDATE SEL FIELDS                          
         GOTO1 AGOSUB                                                           
         ICM   R2,15,FULL           A(FLD W/ ERROR) RETURNED IN FULL            
         BNZ   MYERROR                                                          
                                                                                
         MVI   GOSUBN,IPF#         GO HANDLE PFKEYS                             
         GOTO1 (RF)                                                             
                                                                                
         MVI   GOSUBN,PWS#         SET UP PW STUFF                              
         GOTO1 (RF)                                                             
                                                                                
         B     XIT                                                              
         EJECT                                                                  
*------------------------- TEST CHANGE OF KEY ------------------------*         
*                                                                               
KYCHNGED DS    0H                                                               
         TM    4(R2),X80                                                        
         BZ    *+12                                                             
         NI    4(R2),XFF-X80                                                    
         B     KYCH10                                                           
                                                                                
         TM    4(R2),X20                                                        
         BOR   RE                                                               
                                                                                
KYCH10   OI    CHNGFLG1,CF1KEY                                                  
         BR    RE                                                               
                                                                                
                                                                                
*------------------------ TEST CHANGE OF FIELD -----------------------*         
*                                                                               
FLDCHNG  DS    0H                                                               
         TM    4(R2),X80           SEE IF FIELD INPUT THIS TIME                 
         BZ    *+12                                                             
         NI    4(R2),XFF-X80                                                    
         B     FCHNGYES             YES, FIELD WAS CHANGED                      
                                                                                
         TM    4(R2),X20           SEE IF FIELD VALIDATED PREVIOUSLY            
         BO    FCHNGNO              YES, FIELD WASN'T CHANGED                   
                                                                                
FCHNGYES DS    0H                  PASS BACK "FIELD WAS CHANGED"                
         CR    RE,RE                                                            
         BR    RE                                                               
                                                                                
FCHNGNO  DS    0H                  PASS BACK "FIELD WAS NOT CHANGED"            
         LTR   RE,RE                                                            
         BR    RE                                                               
         EJECT                                                                  
*---------------------- VALIDATE OPTIONS FIELD ----------------------*          
                                                                                
* At entry, R2-->options field.                                                 
                                                                                
         DS    0H                                                               
VALOPTS  NTR1                                                                   
                                                                                
         MVI   NOPTN,0             ASSUME NOTHING IN OPTIONS FIELD              
         MVI   GOSUBN,IOV#         INITIALIZE OPTION VALUES                     
         GOTO1 AGOSUB                                                           
         XC    BILLYRMT,BILLYRMT                                                
*                                                                               
         CLI   5(R2),0             ANY OPTIONS INPUTTED?                        
         BE    VALOPTXY             NO                                          
*                                                                               
         MVI   MYERRCD,IFLDQ       ASSUME ERROR                                 
         MVI   BYTE,0                                                           
         GOTO1 SCANNER,DMCB,(R2),(X'80',AIO3),0                                 
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BZ    VALOPTXN            ERROR                                        
         STC   R0,NOPTN                                                         
                                                                                
         L     R2,AIO3             R2-->SCANNER BLOCK                           
         MVI   COUNTER,0                                                        
*                                                                               
** VALIDATE OPTION KEYWORD **                                                   
*                                                                               
VOPT10   DS    0H                  RUN THRU OPTION KEYWORDS TABLE               
         L     R3,AOPTABS                                                       
         USING OPTDSECT,R3                                                      
                                                                                
VOPT12   DS    0H                                                               
         MVI   MYERRCD,INVOQ       ASSUME INVALID KEYWORD ERROR                 
         MVC   BYTE,4(R2)           HOLD ONTO DISPLACEMENT                      
                                                                                
         CLI   WHOLIST,C'B'        IF BILLER LIST SCREEN,                       
         BNE   VOPT12A                                                          
         CLI   COUNTER,0            AND THIS IS 1ST OPTION INPUTTED,            
         BNE   VOPT12A                                                          
         XC    WORK,WORK            THEN IT SHOULD BE A MONTH                   
                                                                                
         MVI   MYERRCD,IMTHQ       MONTH EXPRESSION EXPECTED,                   
         CLI   1(R2),0              SHOULD ONLY BE A SINGLE FIELD               
         BNE   VALOPTXN                                                         
         TM    2(R2),X40            OF VALID ALPHABETICS                        
         BZ    VALOPTXN                                                         
         ZIC   R1,0(R2)                                                         
         STC   R1,WORK+5                                                        
         BCTR  R1,0                                                             
         EXMVC R1,WORK+8,12(R2)                                                 
         LA    R1,8+1(R1)                                                       
         STC   R1,WORK                                                          
                                                                                
         DS    0H                  VALIDATE MONTH EXPRESSION                    
         MVC   DATE3,ESDATEB        USING EST STRT DATE AS TODAY'S DATE         
         MVI   GOSUBN,VMTH#                                                     
         GOTO1 AGOSUB                                                           
         MVI   BYTE,0              FUDGE DSPL INTO OPTN FIELD                   
         BNE   VALOPTXN            INVALID MONTH EXPRESSION ERROR               
                                                                                
         MVI   MYERRCD,MNIEQ       ASSUME MNTH NOT IN EST PERIOD                
         DS    0H                  GET CALENDAR MNTH OF EST STRT DATE           
         GOTO1 DATCON,DMCB,(3,ESDATEB),(0,DATE6),0                              
         GOTO1 AGETBROD,DMCB,(1,DATE6),STARTEND,AGETDAY,AADDAY                  
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(3,WORK),0                            
                                                                                
         DS    0H                  GET CALENDAR MNTH OF EST END DATE            
         ZICM  R0,SPOTPROF+8,(1)    GET OUT-OF-WEEK START DAY                   
         BNZ   *+8                                                              
         LA    R0,1                  IF NONE, THEN IT'S MONDAY                  
         GOTO1 GETDAY,DMCB,EEDATE,DUB                                           
         ZIC   R1,DMCB+0            GET DAY OF EST END DATE                     
         CR    R0,R1                                                            
         BH    *+8                                                              
         AH    R0,=H'7'             ENSURE R0 WILL NOT BE POSITIVE              
         SR    R0,R1                1 <= R0 <= 7                                
         SH    R0,=H'7'             R0=DSPLCMNT TO BDCST WEEK START             
         GOTO1 ADDAY,DMCB,(C'D',EEDATE),DATE6,(R0)                              
*&&DO                                                                           
         GOTO1 DATCON,DMCB,(3,EEDATEB),(0,DATE6),0                              
*&&                                                                             
         GOTO1 AGETBROD,DMCB,(1,DATE6),STARTEND,AGETDAY,AADDAY                  
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(3,WORK+3),0                          
                                                                                
         CLC   DATE3(2),WORK       TEST W/ YEAR AND MONTH                       
         BNL   *+20                                                             
         MVC   DATE3(1),WORK+3     USE YEAR FROM ESTIMATE END DATE              
         CLC   DATE3(2),WORK        AND TEST AGAIN                              
         BL    VALOPTXN                                                         
                                                                                
         CLC   DATE3(2),WORK+3     TEST W/ YEAR AND MONTH OF EST. END           
         BH    VALOPTXN                                                         
                                                                                
         MVC   BILLYRMT,DATE3      SAVE (IMPLICIT YR &) MONTH INPUTTED          
         MVI   BILLYRMT-1,0        DSPL INTO OPTIONS FIELD                      
         B     VOPT62                                                           
         EJECT                                                                  
VOPT12A  DS    0H                                                               
         CLI   0(R3),EOT           END OF TABLE?                                
         BE    VALOPTXN             YES, CAN'T MATCH INPUTTED OPTION            
                                                                                
         DS    0H                  MATCH KEYWORD                                
         LA    R4,OPTNAME          R4-->KEYWORD TABLE                           
VOPT14   CLI   0(R4),EOT           ARE WE AT THE END OF KEYWORD TABLE?          
         BE    VOPTBUMP             YES, BUMP TO NEXT OPTION ENTRY              
                                                                                
         ZIC   R1,0(R4)            R1=L(KEYWORD)                                
         CLM   R1,1,0(R2)          MATCH L(KEYWORD) TO L(INPUT)                 
         BNE   VOPT14A                                                          
         BCTR  R1,0                                                             
         EXCLC R1,1(R4),12(R2)     MATCH KEYWORD TO INPUT                       
         BE    VOPT16                                                           
         LA    R1,1(R1)                                                         
VOPT14A  LA    R4,1(R4,R1)         BUMP TO NEXT KEYWORD IN TABLE                
         B     VOPT14                                                           
                                                                                
VOPT16   DS    0H                  OPTION ENTRY FOUND                           
         MVI   MYERRCD,DUPOQ       ASSUME DUPLICATED OPTION KEYWORD             
         MVC   BYTE,4(R2)           HOLD ONTO DISPLACEMENT                      
         MVC   HALF(1),OPTUSED                                                  
         NC    HALF(1),OPTBIT      WAS IT SPECIFIED ALREADY?                    
         BNZ   VALOPTXN             YES, ERROR                                  
                                                                                
         DS    0H                  CHECK FOR OPTIONS CONFLICT                   
         MVI   MYERRCD,IOCBQ       ASSUME INVALID OPTION COMBO                  
         MVC   BYTE,4(R2)           HOLD ONTO DISPLACEMENT                      
         MVC   HALF(1),OPTUSED                                                  
         NC    HALF(1),OPTEXCL     ANY OPTION EXCLUDING ANOTHER?                
         BNZ   VALOPTXN             YES, ERROR                                  
                                                                                
         B     VOPT20                                                           
*                                                                               
** VALIDATE OPTION VALUE **                                                     
*                                                                               
VOPT20   DS    0H                                                               
         TM    OPTFLAG,OPFOTHAB    OPTION VALUES FOUND IN ANOTHER TBLE          
         BO    VOPT30                                                           
         TM    OPTFLAG,OPFVRTN     OPTION VALUES VALIDATED IN ROUTINE           
         BO    VOPT40                                                           
         TM    OPTFLAG,OPFKYWRD    OPTION SPECIFIABLE BY KEYWORD ONLY           
         BO    VOPT50                                                           
         DC    H'0'                                                             
                                                                                
VOPT30   DS    0H                  VALIDATE OPTION VALUE VIA TABLE              
         ZICM  R4,OPTDSPVT,(3)                                                  
         A     R4,BASE1                                                         
         MVI   MYERRCD,IOPDQ       ASSUME INVALID DATA VALUE                    
         MVC   BYTE,8(R2)           AND HOLD ONTO DISPLACEMENT                  
                                                                                
VOPT32   CLI   0(R4),EOT           AT END OF VALUES TABLE?                      
         BE    VALOPTXN             YEP, VALUE INPUTTED IS INVALID              
         ZIC   R1,0(R4)                                                         
         CLM   R1,1,1(R2)          MATCH L(VALUE) INPUTTED                      
         BNE   VOPT32A                                                          
         BCTR  R1,0                                                             
         EXCLC R1,1(RF),22(R2)                                                  
         BE    VOPT36                                                           
         LA    R1,1(R1)                                                         
VOPT32A  LA    R4,1(R4,R1)         BUMP TO NEXT OPTION VALUE                    
         B     VOPT32                                                           
                                                                                
VOPT36   ZIC   R1,OPTOLEN                                                       
         BCTR  R1,0                                                             
         ZICM  RE,OPTODSPL                                                      
         A     RE,ASYSD                                                         
         EXMVC R1,0(RE),1(R4)                                                   
         B     VOPT60                                                           
*                                                                               
VOPT40   DS    0H                  VALIDATE OPTION VIA ROUTINE                  
         MVC   GOSUBN,OPTRTNUM                                                  
         GOTO1 AGOSUB                                                           
         BE    VOPT60                                                           
         MVC   BYTE,8(R2)           AND HOLD ONTO DISPLACEMENT                  
         B     VALOPTXN                                                         
*                                                                               
VOPT50   DS    0H                  OPTION SPECIFIABLE BY KEYWORD ONLY           
*        B     VOPT60                                                           
*                                                                               
** OPTION ENTRY IS VALID **                                                     
*                                                                               
VOPT60   DS    0H                                                               
         OC    OPTUSED,OPTBIT      FLAG THAT OPTION REQUESTED                   
                                                                                
VOPT62   DS    0H                                                               
         ZIC   R1,COUNTER          ANY MORE SCANNER ENTRIES?                    
         LA    R1,1(R1)                                                         
         CLM   R1,1,NOPTN                                                       
         BNL   VALOPTXY             NOPE, FINISHED W/ OPTIONS                   
         STC   R1,COUNTER                                                       
         LA    R2,32(R2)            YES, BUMP TO NEXT SCANNER ENTRY             
         B     VOPT10                                                           
*                                                                               
VOPTBUMP ZIC   R0,OPTLEN                                                        
         AR    R3,R0                                                            
         B     VOPT12                                                           
         DROP  R3                                                               
                                                                                
                                                                                
VALOPTXY DS    0H                                                               
         B     YES                                                              
*                                                                               
VALOPTXN DS    0H                  ERROR - SET CURSOR ONTO SUB-FIELD            
         L     R1,SYSPARMS                                                      
         L     RF,0(R1)            RF = A(TIOB)                                 
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC+TIOBALRM                                       
         LA    R0,PWLOPTNH-T217FFD                                              
         STH   R0,TIOBCURD                                                      
         MVC   TIOBCURI,BYTE                                                    
         DROP  RF                                                               
         MVI   PFKEY,0             IGNORE ANY PFKEY HIT                         
         B     NO                                                               
         EJECT                                                                  
*----------------------- VALIDATE FILTER FIELD ----------------------*          
                                                                                
* At entry, R2-->filter field.                                                  
                                                                                
         DS    0H                                                               
VALFLTS  NTR1                                                                   
                                                                                
         MVI   GOSUBN,IFV#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VALFLTXY             NO, EXIT W/ CC EQUAL                        
                                                                                
         GOTO1 SCANNER,DMCB,(R2),(X'80',AIO3),0                                 
         L     R2,AIO3             R2-->SCANNER BLOCK                           
                                                                                
         MVI   MYERRCD,IFLDQ       ASSUME ERROR                                 
         SR    R0,R0                                                            
         ICM   R0,1,DMCB+4                                                      
         BNZ   *+12                 NO ERROR                                    
         MVI   4(R2),0              ERROR, FUDGE DISPL OF ERROR                 
         B     VALFLTXN                                                         
                                                                                
         STC   R0,COUNTER          COUNTER = # OF FILTER ENTRIES                
         XC    APVNTRY,APVNTRY     NO PREVIOUS ENTRY YET                        
*                                                                               
VFLT10   DS    0H                                                               
         L     R3,AFLTABS                                                       
         USING FLTDSECT,R3                                                      
VFLT12   CLI   0(R3),EOT           ANY MORE FILTER ENTRIES?                     
         BE    VFLT20               NOPE                                        
                                                                                
         DS    0H                  CHECK AGAINST KEYWORDS                       
         ZICM  R4,FLTKYTB,(3)                                                   
         LA    R4,FLTDSECT(R4)     R4-->BEGINNING OF KEYWORD TABLE              
VFLT14   CLI   0(R4),EOT           ARE WE @ END OF KEYWORD TABLE YET?           
         BE    VFLT18               YEP, INPUT IS NOT THIS FILTER ENTRY         
         SR    R1,R1                                                            
         CLC   0(1,R2),0(R4)       INPUT LEN > MAX L(KEYWORD)?                  
         BH    VFLT16               YEP, TRY NEXT KEYWORD                       
         ICM   R1,1,0(R2)           NOPE, MATCH INPUT TO KEYWORD                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         EXCLC R1,12(R2),1(R4)                                                  
         BE    VFLT30              MATCHED!  FOUND FILTER ENTRY                 
VFLT16   IC    R1,0(R4)            NO MATCH,                                    
         LA    R4,1(R4,R1)          TRY NEXT KEYWORD                            
         B     VFLT14                                                           
                                                                                
VFLT18   DS    0H                  BUMP TO NEXT FILTER ENTRY                    
         ZIC   R0,FLTLEN                                                        
         AR    R3,R0                                                            
         B     VFLT12                                                           
*                                                                               
VFLT20   DS    0H                  NO MATCH AGAINST FILTER TABLE                
         MVI   MYERRCD,IFLDQ                                                    
         CLI   1(R2),0             IS INPUT DIVIDED FIELD?                      
         BNE   VALFLTXN             YES, KEYWORD INPUTTED IS BAD                
         ICM   R3,15,APVNTRY       CAN WE USE THE PREVIOUS ENTRY?               
         BZ    VALFLTXN             NO, EXIT W/ ERROR                           
                                                                                
         MVC   1(1,R2),0(R2)        YES, ASSUME MORE DATA FOR PREV              
         MVC   3(1,R2),2(R2)         FILTER ENTRY.  FUDGE INPUT TO BE           
         MVC   8(4,R2),4(R2)         IN 2ND HALF OF FIELD                       
         MVC   22(10,R2),12(R2)                                                 
*                                                                               
VFLT30   DS    0H                  VALIDATE FILTER DATA INPUTTED                
         ST    R3,APVNTRY          STORE ADDRESS FOR NEXT INPUT                 
         MVI   MYERRCD,IFLTQ       ASSUME INVALID FILTER                        
         CLI   1(R2),0             MUST HAVE DATA FOR FILTER                    
         BE    VALFLTXN                                                         
                                                                                
         DS    0H                  CHECK # OF FILTER DATA SO FAR                
         MVI   MYERRCD,TMFDQ       ASSUME TOO MANY FILTER DATA                  
         ZICM  R4,FLTODSPL,(3)                                                  
         A     R4,ASYSD            R4-->STORED FILTER VALUES                    
         CLC   0(1,R4),FLTMAXN     CHECK AGAINST MAX POSSIBLE                   
         BNL   VALFLTXN             NO MORE ROOM TO FIT CURRENT DATA            
                                                                                
         DS    0H                  VALIDATE DATA                                
         TM    FLTFLAG,FLFVLTAB     USE TABLE?                                  
         BO    VFLT40                                                           
         TM    FLTFLAG,FLFVLRTN     USE ROUTINE?                                
         BO    VFLT50                                                           
         DC    H'0'                                                             
*                                                                               
VFLT40   DS    0H                  USING LOOKUP TABLE                           
         ZICM  R4,FLTTBDSP,(3)                                                  
         A     R4,BASE1            R4-->TABLE OF VALID VALUES                   
         MVI   MYERRCD,IFLTQ       ASSUME INVALID FILTER EXPRESSION             
VFLT42   CLI   0(R4),EOT           IF WE REACH THE END-OF-TABLE,                
         BE    VALFLTXN             IT'S INVALID                                
         CLC   1(1,R2),0(R4)       CHK LNGTHS BEFORE COMPARING VALUES           
         BH    VFLT44                                                           
         ZIC   R1,1(R2)                                                         
         BCTR  R1,0                                                             
         EXCLC R1,22(R2),1(R4)                                                  
         BE    VFLT46              MATCH!  FILTER DATA IS VALID                 
VFLT44   ZIC   R1,0(R4)            NO MATCH, BUMP TO NEXT VAL IN TBL            
         LA    R4,1(R4,R1)                                                      
         B     VFLT42                                                           
                                                                                
VFLT46   DS    0H                  STORE FILTER DATA INTO SYSD                  
         ZICM  R4,FLTODSPL,(3)                                                  
         A     R4,ASYSD            R4-->OUTPUT AREA                             
         ZIC   RE,FLTOLEN          RE=L(OUTPUT ENTRIES)                         
         LR    R1,RE               HOLD ONTO IT                                 
         ZIC   RF,0(R4)            RF=# OF OUTPUT ENTRIES SO FAR                
         LA    R0,1(RF)                                                         
         STC   R0,0(R4)             AND UPDATE IT                               
         MR    RE,RE                                                            
         LA    RF,1(RF,R4)         R4-->SLOT FOR NEXT ENTRY                     
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),22(R2)                                                  
                                                                                
         OC    FLTUSED,FLTUBIT     IF OKAY, TURN ON FILTER-USED FLAGS           
         OC    FLTUSED2,FLTUBIT2                                                
         B     VFLT60                                                           
*                                                                               
VFLT50   DS    0H                  USING ROUTINE                                
         MVC   GOSUBN,FLTRTNUM                                                  
         GOTO1 AGOSUB                                                           
         BNE   VALFLTXN                                                         
                                                                                
         OC    FLTUSED,FLTUBIT     IF OKAY, TURN ON FILTER-USED FLAGS           
         OC    FLTUSED2,FLTUBIT2                                                
         B     VFLT60                                                           
         DROP  R3                                                               
*                                                                               
VFLT60   DS    0H                  GET NEXT INPUT IN SCANNER BLOCK              
         ZIC   R1,COUNTER          UPDATE LOOP COUNTER                          
         SH    R1,=H'1'                                                         
         BZ    VALFLTXY             ALL DONE, EXIT CLEANLY                      
         STC   R1,COUNTER                                                       
         LA    R2,32(R2)           R2-->NEXT SCANNER BLOCK ENTRY                
         B     VFLT10                                                           
                                                                                
                                                                                
VALFLTXY DS    0H                                                               
         B     YES                                                              
*                                                                               
VALFLTXN DS    0H                  ERROR - SET CURSOR ONTO SUB-FIELD            
         L     R1,SYSPARMS                                                      
         L     RF,0(R1)            RF = A(TIOB)                                 
         USING TIOBD,RF                                                         
         OI    TIOBINDS,TIOBSETC+TIOBALRM                                       
         LA    R0,PWLFLTRH-T217FFD                                              
         STH   R0,TIOBCURD                                                      
         MVC   TIOBCURI,4(R2)                                                   
         DROP  RF                                                               
         MVI   PFKEY,0             IGNORE ANY PFKEY HIT                         
         B     NO                                                               
         EJECT                                                                  
*------------------------- MY VALKEY ERRORS --------------------------*         
MPCTE    DS    0H                                                               
         MVI   MYERRCD,MPCTQ                                                    
         B     MYERROR                                                          
                                                                                
MTRQE    DS    0H                                                               
         MVI   MYERRCD,MTRQQ                                                    
         B     MYERROR                                                          
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST OVERLAY (LISTRECS)'                
***********************************************************************         
*============================ LIST RECORD ============================*         
LR       DS    0H                                                               
                                                                                
         MVI   NLISTS,NLSTLINQ     SET GENCON'S LIMIT OF LIST LINES             
         XC    LISTINFO(NLSTLINQ),LISTINFO                                      
         XC    LISTINF1(NLSTLINQ),LISTINF1                                      
         XC    LISTINF2(NLSTLINQ),LISTINF2                                      
         NI    MISCFLG1,XFF-MF1LSTDN                                            
         NI    SELFLAG1,XFF-SF1MNSEL                                            
         NI    SELFLAG1,XFF-SF1TSLOK                                            
         NI    SELFLAG1,XFF-SF1SELCT                                            
                                                                                
         OC    ACCUTABN,ACCUTABN   ANY MARKETS TO LIST?                         
         BZ    LRX                  NOPE                                        
         CLI   WHOLIST,C'M'        DETERMINE THE TYPE OF LIST                   
         BE    LR04                                                             
         CLI   WHOLIST,C'B'                                                     
         BE    LR200                                                            
         DC    H'0'                                                             
         EJECT                                                                  
*--------------------- LIST LOGIC FOR ACTION=LIST --------------------*         
*                                                                               
** TOTALS LINE **                                                               
*                                                                               
* This is my only chance to display total amounts on the list screen.           
*                                                                               
LR04     DS    0H                                                               
         MVC   PWLTOTL,SPACES                                                   
         OI    PWLTOTLH+6,X80      TRANSMIT TOTALS LINE                         
                                                                                
         LA    R4,PWLTOTL                GET R4-->DISPLAY LINE                  
         USING LISTD,R4                                                         
         XC    DUB,DUB                                                          
         MVI   DUB+1,9                   SET L(OUTPUT FIELDS)                   
         MVI   DUB+3,9                                                          
         MVI   DUB+5,(LSTCLOCK-4)-LISTD  SET DISPL OF OUTPUT FIELDS             
         MVI   DUB+7,(LSTWLOCK-2)-LISTD                                         
         MVC   TEMPAJB,CLTLKTOT          SET AMOUNTS                            
         MVC   TEMPACB,WIMLKTOT                                                 
         MVC   TEMPTAX,TAXLKTOT                                                 
         MVI   GOSUBN,FCW#                                                      
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,FPL#                                                      
         GOTO1 (RF)                                                             
                                                                                
         MVI   LSTGLS,C'N'         ASSUME NO GOALS WHATSOEVER                   
         TM    MISCFLG1,MF1NOGOL                                                
         BO    LR05                                                             
         MVI   LSTGLS,C'+'          FLAG IT IF GOALS < LOCKED CLT$              
         CLC   GOALTOTL,CLTLKTOT                                                
         BL    *+8                                                              
         MVI   LSTGLS,C' '         COMPARE TOTAL LOCKED CLT$ TO GOALS           
         DROP  R4                                                               
*                                                                               
** MESSAGE IN SCREEN HEADER **                                                  
*                                                                               
* Determine whether to use GENCON'S msgs or my msgs.                            
*                                                                               
LR05     DS    0H                                                               
         NI    GENSTAT2,XFF-USMYOK-USGETTXT                                     
                                                                                
         TM    OPTUSED,OPUSBBPM        WAS ANY SELECT-ALL REQUESTED?            
         BZ    LR07                     NO                                      
         OC    LKSELCDE,LKSELCDE       IS THERE A SEL CODE FILLER?              
         BZ    LR07                     NO                                      
         CLI   LKALLDNE,C'Y'           ARE WE ALL DONE W/ ALL-LOCKING?          
         BE    LR07                     YES, DSPLY REG GENCON MSG               
                                                                                
         MVC   MSGNUM2,=AL2(SI#SLMKT)  INFO MESSAGE DEPENDS ON                  
         TM    OPTUSED,OPUSLALL         TYPE OF ALL-LOCK                        
         BO    LR05A                                                            
         MVC   MSGNUM2,=AL2(SI#BLMKT)                                           
         TM    OPTUSED,OPUBLALL         TYPE OF ALL-LOCK                        
         BO    LR05A                                                            
         MVC   MSGNUM2,=AL2(SI#BUMKT)                                           
         TM    OPTUSED,OPUBUALL                                                 
         BO    LR05A                                                            
         MVC   MSGNUM2,=AL2(SI#PLMKT)                                           
         TM    OPTUSED,OPUPLALL                                                 
         BO    LR05A                                                            
         MVC   MSGNUM2,=AL2(SI#MTMKT)                                           
         TM    OPTUSED,OPUMTALL                                                 
         BO    LR05A                                                            
         DC    H'0'                                                             
LR05A    MVI   GOSUBN,BGI#          YES, BUILD GTXT BLOCK FOR INFO MSG          
         GOTO1 AGOSUB                                                           
*                                                                               
** LIST MARKETS **                                                              
*                                                                               
LR07     DS    0H                                                               
         XC    FRSTSELK,FRSTSELK                                                
         MVC   TEMPBMKT,LASTSELK+(PWKMKT-PWFKEY)  PICK UP FROM LAST             
         OC    LASTSELK,LASTSELK                   LEFT OFF,                    
         BNZ   LR10                                                             
         MVC   TEMPBMKT,SVINPMKT   OR START LISTING FROM MRKT INPUTTED,         
         OC    SVINPMKT,SVINPMKT                                                
         BNZ   LR10                                                             
         L     RF,AACCUTAB                                                      
         MVC   TEMPBMKT,0(RF)      OR START LISTING W/ 1ST MKT                  
         B     LR10                                                             
*                                                                               
LR10     DS    0H                                                               
         MVI   GOSUBN,LMK#         FIND ENTRY FOR MKT IN ACCUTAB                
         GOTO1 AGOSUB                                                           
         ICM   R3,15,FULL          R3-->ENTRY (ON RDHIGH) FOR MARKET            
         BZ    LRX                                                              
         USING ACCUTABD,R3                                                      
                                                                                
         OC    ATMRKT,ATMRKT                                                    
         BZ    LR20                                                             
         MVC   TEMPBMKT,ATMRKT                                                  
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB                                                           
         MVC   FRSTSELK,KEY         REMEMBER MY FIRST LIST KEY                  
*                                                                               
*** START OF LISTING LOOP ***                                                   
*                                                                               
LR20     DS    0H                                                               
         OC    ATMRKT,ATMRKT       PROCESSED ALL MKTS IN ACCUTAB?               
         BZ    LRX                  YEP, EXIT LIST ROUTINE                      
                                                                                
         TM    ATFLAG1,ATF1PFLT    DID THIS ENTRY PASS FILTER?                  
         BZ    LR150                NOPE, BUMP TO NEXT ENTRY                    
*                                                                               
**** PASSED FILTER TEST--PUT STUFF ON LISTLINE AND LISTINFOS ****               
*                                                                               
LR100    DS    0H                                                               
         ZIC   RE,LISTNUM          PUT ENTRY FOR LISTINFO                       
         LR    RF,RE                                                            
         LA    RE,LISTINFO(RE)                                                  
         MVC   0(L'LISTINFO,RE),ATPWFLAG                                        
         LR    RE,RF                                                            
         LA    RF,LISTINF1(RF)                                                  
         MVC   0(L'LISTINF1,RF),ATFLAG1                                         
         LA    RE,LISTINF2(RE)                                                  
         MVC   0(L'LISTINF2,RE),ATFLAG2                                         
                                                                                
         LA    R4,LISTAR           USE R4 FOR LIST LINE                         
         USING LISTD,R4                                                         
         MVC   LISTAR,SPACES       LIST LINE: BLANK IT OUT                      
                                                                                
         XC    WORK,WORK           GET EBCDIC MKT#                              
         MVC   WORK(2),ATMRKT                                                   
         GOTO1 MSUNPK,DMCB,WORK,FULL,DUB                                        
         XC    WORK,WORK           GET MARKET NAME                              
         MVI   WORK,12              BUILD DUMMY TWA HEADER                      
         MVI   WORK+4,X'88'                                                     
         MVI   WORK+5,4                                                         
         MVC   WORK+8(4),FULL                                                   
         LA    R2,WORK              MAKE R2-->TWA HEADER                        
         GOTO1 VALIMKT                                                          
                                                                                
         MVC   LSTMKT,FULL         LIST LINE: MKT#                              
         MVC   LSTMKNAM,MKTNM      LIST LINE: MKT NAME                          
*                                                                               
         MVI   LSTBLK,C'U'         LIST LINE : BUYLK                            
         TM    ATPWFLAG,PWGNBILQ                                                
         BZ    *+8                                                              
         MVI   LSTBLK,C'L'                                                      
                                                                                
         MVI   LSTPLK,C'U'         LIST LINE : PWLK                             
         TM    ATPWFLAG,PWGNPLKQ                                                
         BZ    *+8                                                              
         MVI   LSTPLK,C'L'                                                      
                                                                                
         MVI   LSTBYS,C'Y'         LIST LINE : BYS                              
         TM    ATFLAG1,ATF1FBYS                                                 
         BO    *+8                                                              
         MVI   LSTBYS,C'N'                                                      
                                                                                
         MVI   LSTGLS,C'N'         LIST LINE : GLS                              
         TM    ATFLAG1,ATF1FGLS+ATF1PGLS                                        
         BZ    LR110                                                            
         MVI   LSTGLS,C'+'                                                      
         TM    ATFLAG1,ATF1LCGG                                                 
         BO    LR110                                                            
         MVI   LSTGLS,C'Y'                                                      
         TM    ATFLAG1,ATF1FGLS                                                 
         BO    LR110                                                            
         MVI   LSTGLS,C'M'                                                      
         TM    ATFLAG1,ATF1PGLS                                                 
         BO    LR110                                                            
         DC    H'0'                                                             
*                                                                               
LR110    DS    0H                  LOCK ACTIVITY DATES                          
         LA    R0,DTDSPTBQ                                                      
         L     R2,ADTDSPTB                                                      
         MVC   DTIELEM,ATDTIELM                                                 
         LA    R1,DMCB             BUILD PARM BLOCK FOR DATCON MYSELF           
LR110A   LH    RE,0(R2)                                                         
         LA    RE,DTIELEM(RE)                                                   
         ST    RE,0(R1)            P1 : DATE SOURCE                             
         MVI   0(R1),X'02'         P1 : INPUT TYPE                              
         LH    RE,2(R2)                                                         
         LA    RE,LISTD(RE)                                                     
         ST    RE,4(R1)            P2 : DATE DESTINATION                        
         MVI   4(R1),X'07'         P2 : OUTPUT TYPE                             
         XC    8(4,R1),8(R1)       P3 : LEAVE BLANK                             
         GOTO1 DATCON,(R1)         LIST LINE : DATES                            
         LA    R2,(2*L'DATDSPTB)(R2)                                            
         BCT   R0,LR110A                                                        
*                                                                               
         XC    DUB,DUB                                                          
         MVI   DUB+1,L'LSTCLOCK        SET L(OUTPUT FIELDS)                     
         MVI   DUB+3,L'LSTWLOCK                                                 
         MVI   DUB+5,(LSTCLOCK-LISTD)  SET DISPL OF OUTPUT FIELDS               
         MVI   DUB+7,(LSTWLOCK-LISTD)                                           
         MVC   TEMPAJB,ATCLOCK         SET AMOUNTS                              
         MVC   TEMPACB,ATWLOCK                                                  
         MVC   TEMPTAX,ATTAXLK                                                  
         MVI   GOSUBN,FCW#         LIST LINE : CLLOCK$ AND WIMLOCK$             
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,FPL#         LIST LINE : LOCKED PW%                       
         GOTO1 (RF)                                                             
         B     LR129                                                            
                                                                                
LR129    DS    0H                                                               
         B     LR140                                                            
*                                                                               
LR140    DS    0H                  PASS LIST LINE TO LISTMON                    
         MVC   TEMPBMKT,ATMRKT                                                  
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB                                                           
         MVC   LASTSELK,KEY         REMEMBER MY LAST LIST KEY                   
                                                                                
         GOTO1 LISTMON                                                          
         DROP  R4                                                               
*                                                                               
*** BUMP TO NEXT MARKET IN ACCUTAB ***                                          
*                                                                               
LR150    DS    0H                                                               
         LA    R3,ACCUTABQ(R3)                                                  
         B     LR20                                                             
         DROP  R3                                                               
         EJECT                                                                  
*-------------------- LIST LOGIC FOR ACTION=BLIST --------------------*         
*                                                                               
** TOTALS LINE **                                                               
*                                                                               
* This is my only chance to display total amounts on the list screen.           
*                                                                               
LR200    DS    0H                                                               
         MVC   PWLTOTL,SPACES                                                   
         OI    PWLTOTLH+6,X80      TRANSMIT TOTALS LINE                         
                                                                                
         LA    R4,PWLTOTL          GET R4-->DISPLAY LINE                        
         USING BLISTD,R4                                                        
         TM    MISCFLG1,MF1DRCR                                                 
         BZ    LR205                                                            
         L     R0,DRCRTOTL         GET TOTAL DRCR                               
         MVI   GOSUBN,RUP#          AND ROUND IT UP                             
         GOTO1 AGOSUB                                                           
         L     R1,FULL                                                          
         EDIT  (R1),(7,BLSTDRCR),FLOAT=-,ZERO=NOBLANK                           
         DROP  R4                                                               
*                                                                               
** MESSAGE IN SCREEN HEADER **                                                  
*                                                                               
* Determine whether to use GENCON'S msgs or my msgs.                            
*                                                                               
LR205    DS    0H                                                               
         NI    GENSTAT2,XFF-USMYOK-USGETTXT                                     
                                                                                
         TM    OPTUSED,OPUBIALL        WAS ANY SELECT-ALL REQUESTED?            
         BZ    LR210                    NO                                      
         OC    LKSELCDE,LKSELCDE       IS THERE A SEL CODE FILLER?              
         BZ    LR210                    NO                                      
         CLI   LKALLDNE,C'Y'           ARE WE ALL DONE W/ ALL-LOCKING?          
         BE    LR210                    YES, DSPLY REG GENCON MSG               
                                                                                
         MVC   MSGNUM2,=AL2(SI#BIMKT)  INFO MESSAGE DEPENDS ON                  
         TM    OPTUSED,OPUBIALL         TYPE OF SELECT-ALL                      
         BO    LR205A                                                           
         DC    H'0'                                                             
LR205A   MVI   GOSUBN,BGI#          YES, BUILD GTXT BLOCK FOR INFO MSG          
         GOTO1 AGOSUB                                                           
*                                                                               
** LIST MARKETS **                                                              
*                                                                               
LR210    DS    0H                                                               
         XC    FRSTSELK,FRSTSELK                                                
         MVC   TEMPBMKT,LASTSELK+(PWKMKT-PWFKEY)  PICK UP FROM LAST             
         OC    LASTSELK,LASTSELK                   LEFT OFF,                    
         BNZ   LR220                                                            
         MVC   TEMPBMKT,SVINPMKT   OR START LISTING FROM MRKT INPUTTED,         
         OC    SVINPMKT,SVINPMKT                                                
         BNZ   LR220                                                            
         L     RF,AACCUTAB                                                      
         MVC   TEMPBMKT,0(RF)      OR START LISTING W/ 1ST MKT                  
         B     LR220                                                            
*                                                                               
LR220    DS    0H                                                               
         MVI   GOSUBN,LMK#         FIND ENTRY FOR MKT IN ACCUTAB                
         GOTO1 AGOSUB                                                           
         ICM   R3,15,FULL          R3-->ENTRY (ON RDHIGH) FOR MARKET            
         BZ    LRX                                                              
         USING ACCUTABD,R3                                                      
                                                                                
         OC    ATMRKT,ATMRKT                                                    
         BZ    LR230                                                            
         MVC   TEMPBMKT,ATMRKT                                                  
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB                                                           
         MVC   FRSTSELK,KEY         REMEMBER MY FIRST LIST KEY                  
*                                                                               
*** START OF LISTING LOOP ***                                                   
*                                                                               
LR230    DS    0H                                                               
         OC    ATMRKT,ATMRKT       PROCESSED ALL MKTS IN ACCUTAB?               
         BZ    LRX                  YEP, EXIT LIST ROUTINE                      
                                                                                
         TM    ATFLAG1,ATF1PFLT    DID THIS ENTRY PASS FILTER?                  
         BZ    LR290                NOPE, BUMP TO NEXT ENTRY                    
         TM    ATFLAG1,ATF1FBYS    DID MARKET HAVE BUYS IN MONTH?               
         BZ    LR290                NOPE, BUMP TO NEXT ENTRY                    
*                                                                               
** PASSED FILTER TEST--PUT STUFF ON LISTLINE AND LISTINFOS **                   
*                                                                               
         ZIC   RE,LISTNUM          PUT ENTRY FOR LISTINFO                       
         LR    RF,RE                                                            
         LA    RE,LISTINFO(RE)                                                  
         MVC   0(L'LISTINFO,RE),ATPWFLAG                                        
         LR    RE,RF                                                            
         LA    RF,LISTINF1(RF)                                                  
         MVC   0(L'LISTINF1,RF),ATFLAG1                                         
         LA    RE,LISTINF2(RE)                                                  
         MVC   0(L'LISTINF2,RE),ATFLAG2                                         
                                                                                
         LA    R4,LISTAR                                                        
         USING BLISTD,R4                                                        
         MVC   LISTAR,SPACES                                                    
*                                                                               
         XC    WORK,WORK           GET EBCDIC MKT#                              
         MVC   WORK(2),ATMRKT                                                   
         GOTO1 MSUNPK,DMCB,WORK,FULL,DUB                                        
         XC    WORK,WORK           GET MARKET NAME                              
         MVI   WORK,12              BUILD DUMMY TWA HEADER                      
         MVI   WORK+4,X'88'                                                     
         MVI   WORK+5,4                                                         
         MVC   WORK+8(4),FULL                                                   
         LA    R2,WORK              MAKE R2-->TWA HEADER                        
         GOTO1 VALIMKT                                                          
                                                                                
         MVC   BLSTMKT,FULL        LIST LINE: MKT#                              
         MVC   BLSTMKNA,MKTNM      LIST LINE: MKT NAME                          
*                                                                               
         MVI   BLSTPLK,C'U'                                                     
         TM    ATPWFLAG,PWGNPLKQ                                                
         BZ    *+8                                                              
         MVI   BLSTPLK,C'L'                                                     
                                                                                
         LA    R6,ATDTIELM                                                      
         USING PWDTIEL,R6                                                       
         GOTO1 DATCON,DMCB,(X'02',PWDTIPLD),(X'07',BLSTPLK+1),0                 
         DROP  R6                                                               
*                                                                               
         TM    ATFLAG1,ATF1ESTB                                                 
         BZ    *+8                                                              
         MVI   BLSTBILL+1,C'*'     LIST LINE: ESTIM BILLED                      
         TM    ATFLAG1,ATF1FNLB                                                 
         BZ    *+8                                                              
         MVI   BLSTBILL+2,C'*'     LIST LINE: FINAL BILLED                      
*                                                                               
         ICM   R0,15,ATDRCR                                                     
         BZ    LR241                                                            
         MVI   BLSTDRCR+L'BLSTDRCR-1,C'0'                                       
         CLM   R0,15,=X'80000000'                                               
         BE    LR241                                                            
         MVI   GOSUBN,RUP#                                                      
         GOTO1 AGOSUB                                                           
         L     R1,FULL                                                          
         EDIT  (R1),(7,BLSTDRCR),FLOAT=-                                        
*                                                                               
LR241    DS    0H                                                               
         B     LR250                                                            
*                                                                               
LR250    DS    0H                  PAST LIST LINE TO LISTMON                    
         MVC   TEMPBMKT,ATMRKT                                                  
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB               REMEMBER MY LAST LIST KEY                   
         MVC   LASTSELK,KEY                                                     
                                                                                
         GOTO1 LISTMON                                                          
         DROP  R4                                                               
*                                                                               
*** BUMP TO NEXT MARKET IN ACCUTAB ***                                          
*                                                                               
LR290    DS    0H                                                               
         LA    R3,ACCUTABQ(R3)                                                  
         B     LR230                                                            
         DROP  R3                                                               
*                                                                               
** EXIT LISTRECS **                                                             
*                                                                               
LRX      DS    0H                                                               
         OI    MISCFLG1,MF1LSTDN   LIST IS DONE                                 
         XC    LASTSELK,LASTSELK   WANT TO START LISTING FROM THE TOP           
         LA    R0,PWLSELH                                                       
         ST    R0,ACURFORC                                                      
                                                                                
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST'                                   
***********************************************************************         
*==================== SUB-ROUTINE POOL INTERFACE =====================*         
                                                                                
GOSUB    NTR1  BASE=BASE1,LABEL=N                                               
         L     R7,BASE2                                                         
                                                                                
         L     RE,4(RD)            PUT EYECATCHER IN MYSELF                     
         MVC   0(3,RE),=C'+GO'                                                  
         MVC   3(1,RE),GOSUBN       AND FUDGE RTN# IN THERE                     
         SR    RE,RE               CLEAR RE JUST TO BE SAFE                     
                                                                                
         MVC   ASUBRTN,ASUBR01                                                  
         CLI   GOSUBN,R01#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR02                                                  
         CLI   GOSUBN,R02#                                                      
         BNH   GOSUBGO                                                          
         MVC   ASUBRTN,ASUBR03                                                  
         CLI   GOSUBN,R03#                                                      
         BNH   GOSUBGO                                                          
         DC    H'0'                                                             
*                                                                               
GOSUBGO  GOTO1 ASUBRTN,DMCB,(GOSUBN,(RC))                                       
         DS    0H                                                               
         XIT1                                                                   
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (MISC STUFF)'                      
***********************************************************************         
*======================== MISCELLANEOUS STUFF ========================*         
                                                                                
YES      SR    RC,RC                                                            
NO       LTR   RC,RC                                                            
XIT      XIT1                                                                   
                                                                                
                                                                                
         EJECT                                                                  
*----------------------- MY ROUTINES FOR ERRORS ----------------------*         
                                                                                
MYERROR  XC    CONHEAD,CONHEAD                                                  
         MVI   MSGSYS,0                                                         
         ZIC   RF,MYERRCD                                                       
         LTR   RF,RF                                                            
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         B     MYERR0(RF)                                                       
                                                                                
MFLDQ    EQU   ((MYERR1-MYERR0)/4)+1                                            
IFLDQ    EQU   ((MYERR2-MYERR0)/4)+1                                            
INVOQ    EQU   ((MYERR3-MYERR0)/4)+1                                            
DUPOQ    EQU   ((MYERR4-MYERR0)/4)+1                                            
IOPDQ    EQU   ((MYERR5-MYERR0)/4)+1                                            
IOCBQ    EQU   ((MYERR6-MYERR0)/4)+1                                            
MPCTQ    EQU   ((MYERR7-MYERR0)/4)+1                                            
EMKTQ    EQU   ((MYERR8-MYERR0)/4)+1                                            
ISELQ    EQU   ((MYERR9-MYERR0)/4)+1                                            
IPFKQ    EQU   ((MYERR10-MYERR0)/4)+1                                           
IDTEQ    EQU   ((MYERR11-MYERR0)/4)+1                                           
IFTOQ    EQU   ((MYERR12-MYERR0)/4)+1                                           
IFLTQ    EQU   ((MYERR13-MYERR0)/4)+1                                           
TMFDQ    EQU   ((MYERR14-MYERR0)/4)+1                                           
NPOLQ    EQU   ((MYERR15-MYERR0)/4)+1                                           
BPNLQ    EQU   ((MYERR16-MYERR0)/4)+1                                           
MBUYRQ   EQU   ((MYERR17-MYERR0)/4)+1                                           
IMTHQ    EQU   ((MYERR18-MYERR0)/4)+1                                           
MNIEQ    EQU   ((MYERR19-MYERR0)/4)+1                                           
MTRQQ    EQU   ((MYERR20-MYERR0)/4)+1                                           
NRMKQ    EQU   ((MYERR21-MYERR0)/4)+1                                           
NBKPQ    EQU   ((MYERR22-MYERR0)/4)+1                                           
PWNAVQ   EQU   ((MYERR23-MYERR0)/4)+1                                           
IPRD2Q   EQU   ((MYERR24-MYERR0)/4)+1                                           
                                                                                
MYERR0   DS    0H                                                               
MYERR1   B     MFLD                MISSING INPUT FIELD                          
MYERR2   B     IFLD                INVALID INPUT FIELD                          
MYERR3   B     INVO                INVALID OPTION KEYWORD                       
MYERR4   B     DUPO                DUPLICATE OPTION KEYWORD                     
MYERR5   B     IOPD                INVALID OPTION DATA VALUE                    
MYERR6   B     IOCB                INVALID OPTION COMBINATION                   
MYERR7   B     MPCT                MISSING PW%                                  
MYERR8   B     EMKT                INVALID MARKET                               
MYERR9   B     ISEL                INVALID SELECT CODE                          
MYERR10  B     IPFK                INVALID PFKEY PRESSED                        
MYERR11  B     IDTE                INVALID DATE FORMAT                          
MYERR12  B     IFTO                INVALID FILTER OPTIONS                       
MYERR13  B     IFLT                INVALID FILTER                               
MYERR14  B     TMFD                TOO MANY FILTER DATA FOR KEYWORD             
MYERR15  B     NPOL                PRODUCT CAN'T BE POL                         
MYERR16  B     BPNL                BUY AND/OR PW NOT LOCKED YET                 
MYERR17  B     MBUYR               MISSING BUYER                                
MYERR18  B     IMTH                INVALID MONTH EXPRESSION                     
MYERR19  B     MNIE                MONTH(S) NOT IN ESTIMATE                     
MYERR20  B     MTRQ                MONTH REQUIRED                               
MYERR21  B     NRMK                NO RECORD ON FILE FOR MARKET                 
MYERR22  B     NBKP                NOTHING TO RESTORE                           
MYERR23  B     PWNAV               PW FUNCTIONS NOT AVAIL TO BUY SVCE           
MYERR24  B     IPRD2               INVALID PRODUCT CODE &T                      
*                                                                               
MFLD     MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
IFLD     MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
INVO     MVC   MSGNUM2,=H'206'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
DUPO     MVC   MSGNUM2,=H'208'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
IOPD     MVC   MSGNUM2,=H'209'                                                  
         MVI   MSGSYS,GTGENSYS                                                  
         B     ERRGTXT                                                          
*                                                                               
IOCB     MVC   MSGNUM2,=AL2(SE#IOCMB)                                           
         B     ERRGTXT                                                          
*                                                                               
MPCT     MVC   MSGNUM2,=AL2(SE#MSSPW)                                           
         B     ERRGTXT                                                          
*                                                                               
EMKT     MVC   MSGNUM2,=AL2(SE#INVMK)                                           
         B     ERRGTXT                                                          
*                                                                               
ISEL     MVC   MSGNUM2,=AL2(SE#INSEL)                                           
         B     ERRGTXT                                                          
*                                                                               
IPFK     MVC   MSGNUM2,=AL2(SE#IPFKY)                                           
         B     ERRGTXT                                                          
*                                                                               
IDTE     MVC   MSGNUM2,=AL2(SE#INDTE)                                           
         B     ERRGTXT                                                          
*                                                                               
IFTO     MVC   MSGNUM2,=AL2(SE#IFLTO)                                           
         B     ERRGTXT                                                          
*                                                                               
IFLT     MVC   MSGNUM2,=AL2(SE#INFLT)                                           
         B     ERRGTXT                                                          
*                                                                               
TMFD     MVC   MSGNUM2,=AL2(SE#TMFTD)                                           
         B     ERRGTXT                                                          
*                                                                               
NPOL     MVC   MSGNUM2,=AL2(SE#NOPOL)                                           
         B     ERRGTXT                                                          
*                                                                               
BPNL     MVC   MSGNUM2,=AL2(SE#BPNLK)                                           
         B     ERRGTXT                                                          
*                                                                               
MBUYR    MVC   MSGNUM2,=AL2(SE#MBUYR)                                           
         LA    R0,PWLBUYRH                                                      
         ST    R0,ACURFORC                                                      
         B     ERRGTXT                                                          
*                                                                               
IMTH     MVC   MSGNUM2,=AL2(SE#IMNTH)                                           
         B     ERRGTXT                                                          
*                                                                               
MNIE     MVC   MSGNUM2,=AL2(SE#MNIEP)                                           
         B     ERRGTXT                                                          
*                                                                               
MTRQ     MVC   MSGNUM2,=AL2(SE#MTRQR)                                           
         B     ERRGTXT                                                          
*                                                                               
NRMK     MVC   MSGNUM2,=AL2(SE#NRCMK)                                           
         B     ERRGTXT                                                          
*                                                                               
NBKP     MVC   MSGNUM2,=AL2(SE#NBKUP)                                           
         B     ERRGTXT                                                          
*                                                                               
PWNAV    MVC   MSGNUM2,=AL2(SE#PWNAV)                                           
         B     ERRGTXT                                                          
*                                                                               
IPRD2    DS    0H                  INVALID PRODUCT CODE &T                      
         MVC   MSGNUM2,=AL2(SE#INVP2)                                           
         B     ERRGTXT                                                          
*                                                                               
ERRGTXT  DS    0H                  TELL GENCON TO GOTO GETTXT                   
         OI    GENSTAT2,USGETTXT                                                
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMTYP,GTMERR        ERROR TYPE MSG                              
         MVC   GTMSGNO,MSGNUM2      ERROR #                                     
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         CLI   MSGSYS,0             OVERRIDE CONNECTED SYSTEM?                  
         BE    *+10                                                             
         MVC   GTMSYS,MSGSYS                                                    
         CLI   MYTEXT,0            ANY REPLACE TEXT?                            
         BE    *+18                                                             
         MVC   GTLTXT,MYTEXT        YES, PUT LENGTH IN                          
         LA    RF,MYTEXT+1                                                      
         STCM  RF,7,GTATXT           AS WELL AS THE ADDR OF THE TEXT            
         DROP  R1                                                               
         MVI   ERROR,0                                                          
         B     ERREXIT                                                          
*                                                                               
ERREXIT  DS    0H                                                               
         OI    6(R2),X'81'         FIELD IN ERROR IS MODIFIED FOR NEXT          
         GOTO1 ERREX                                                            
         B     XIT                                                              
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (LTORG && CONST)'                  
***********************************************************************         
*======================== LTORG AND CONSTANTS ========================*         
         LTORG                                                                  
         SPACE 2                                                                
DCRELOCK DC    BL2'0000000110100001'   X'01A1' CMPRSSD DATE FLG==>RELCK         
                                                                                
                                                                                
***********************************************************************         
                                                                                
                                                                                
         DROP  R7,R8,R9,RA,RB,RC                                                
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (SUBR01)'                          
***********************************************************************         
*======================== SUBROUTINE POOL ONE ========================*         
SUBR01Q  EQU   ((((*-T21742)/4096)+1)*4096)                                     
                                                                                
         ORG   T21742+SUBR01Q                                                   
SUBR01   NMOD1 0,**4201**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE                      
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R01_00(R1)                                                       
                                                                                
IOV#     EQU   (R01_01-R01_00)/4+1                                              
VLA#     EQU   (R01_02-R01_00)/4+1                                              
IFV#     EQU   (R01_03-R01_00)/4+1                                              
VFD#     EQU   (R01_04-R01_00)/4+1                                              
VFL#     EQU   (R01_05-R01_00)/4+1                                              
ISL#     EQU   (R01_06-R01_00)/4+1                                              
TSL#     EQU   (R01_07-R01_00)/4+1                                              
FSL#     EQU   (R01_08-R01_00)/4+1                                              
VSL#     EQU   (R01_09-R01_00)/4+1                                              
IPF#     EQU   (R01_10-R01_00)/4+1                                              
PWS#     EQU   (R01_11-R01_00)/4+1                                              
SDC#     EQU   (R01_12-R01_00)/4+1                                              
PAT#     EQU   (R01_13-R01_00)/4+1                                              
                                                                                
R01_00   DS    0H                                                               
R01_01   B     INITOPTV                                                         
R01_02   B     VLKALL                                                           
R01_03   B     INITFLTV                                                         
R01_04   B     VLFTDATE                                                         
R01_05   B     VLFTLDAT                                                         
R01_06   B     INITSEL                                                          
R01_07   B     TESTSEL                                                          
R01_08   B     FILLSEL                                                          
R01_09   B     VALSEL                                                           
R01_10   B     IPFKEY                                                           
R01_11   B     PWSETUP                                                          
R01_12   B     SDCMTHBT                                                         
R01_13   B     PROCACTB                                                         
R01#     EQU   (*-R01_00)/4                                                     
DIE_01   DC    H'0'                                                             
                                                                                
YES_01   SR    RC,RC                                                            
NO_01    LTR   RC,RC                                                            
XIT_01   XIT1                                                                   
         TITLE 'SPSFM42 - PW LIST (SUBR01--IOV#)'                               
*------------------------- INIT OPTION VALUES ------------------------*         
                                                                                
* Initialize option values.                                                     
                                                                                
INITOPTV DS    0H                                                               
         MVI   OPTUSED,0           CLEAR OPTIONS-USED FLAG                      
                                                                                
         L     R2,AOPTABS                                                       
         USING OPTDSECT,R2                                                      
*                                                                               
IOV10    DS    0H                  RUN THROUGH ALL FILTER ENTRIES               
         CLI   0(R2),EOT                                                        
         BE    IOVX                ALL DONE!                                    
                                                                                
         ZICM  R3,OPTODSPL,(3)                                                  
         BZ    IOV40                                                            
         A     R3,ASYSD            R3-->OPTION OUTPUT VALUE                     
         ZICM  R1,OPTOLEN,(1)                                                   
         BZ    IOV40                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8               NONE SO FAR                                 
         B     *+10                                                             
         XC    0(0,R3),0(R3)                                                    
         B     IOV40                                                            
*                                                                               
IOV40    DS    0H                  BUMP TO NEXT OPTION ENTRY                    
         ZIC   R1,OPTLEN                                                        
         AR    R2,R1                                                            
         B     IOV10                                                            
         DROP  R2                                                               
*                                                                               
IOVX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR01--VLA#)'                               
*---------------------- VALIDATE LOCK-ALL OPTION ---------------------*         
                                                                                
* Validates the data part of an option entry of the form:                       
*  (Type of Select) = ALL                                                       
* At entry,                                                                     
*   R2-->SCANNER block entry                                                    
*   R3-->option table                                                           
* NOTE: COUNTER is used by caller, DON'T use it here!                           
                                                                                
VLKALL   DS    0H                                                               
         USING OPTDSECT,R3                                                      
         MVI   MYERRCD,IOPDQ       ASSUME INVALID OPTION DATA                   
                                                                                
         CLI   22(R2),C'*'                                                      
         BNE   VLA10                                                            
         TM    OPTFLAG,OPFNULFY    CAN OPTION BE NULLIFIED?                     
         BO    VLAXY                YES, LEAVE OUTPUT FIELD ALONE               
         B     VLAXN                NO, EXIT W/ ERROR                           
*                                                                               
VLA10    DS    0H                                                               
         CLI   1(R2),3             DATA VALUE MUST BE 'ALL'                     
         BNE   VLA10A                                                           
         CLC   22(3,R2),=C'ALL'                                                 
         BNE   VLAXN                                                            
         B     VLA12                                                            
                                                                                
VLA10A   DS    0H                                                               
         TM    OPTBIT,OPUBIALL     FOR BLIST B= OPTION,                         
         BZ    VLAXN                                                            
         CLI   1(R2),4              DATA VALUE CAN BE 'ALL0'                    
         BNE   VLAXN                                                            
         CLC   22(4,R2),=C'ALL0'                                                
         BNE   VLAXN                                                            
*                                                                               
VLA12    DS    0H                  SET OUTPUT FIELD                             
         LA    RE,LKSELCDE-1                                                    
         MVC   0(1,RE),8(R2)       SET DSPL IN OPTNS FLD OF DATA                
                                                                                
         MVC   LKSELCDE,=X'E2D300' STATION LOCKIN SEL CODE                      
         TM    OPTBIT,OPUSLALL                                                  
         BO    VLAXY                                                            
         MVC   LKSELCDE,=X'C2D300' BUY LOCK       SEL CODE                      
         TM    OPTBIT,OPUBLALL                                                  
         BO    VLAXY                                                            
         MVC   LKSELCDE,=X'C2E400' BUY UNLOCK     SEL CODE                      
         TM    OPTBIT,OPUBUALL                                                  
         BO    VLAXY                                                            
         MVC   LKSELCDE,=X'D7D300' PW  LOCK       SEL CODE                      
         TM    OPTBIT,OPUPLALL                                                  
         BO    VLAXY                                                            
         MVC   LKSELCDE,=X'D40000' PW/MAINT       SEL CODE                      
         TM    OPTBIT,OPUMTALL                                                  
         BO    VLAXY                                                            
         MVC   LKSELCDE,=X'C20000' PW/BILL        SEL CODE                      
         CLI   1(R2),4              DATA VALUE CAN BE 'ALL0'                    
         BNE   *+10                                                             
         MVC   LKSELCDE,=X'C2F000' PW/BILL,DC=0   SEL CODE                      
         TM    OPTBIT,OPUBIALL                                                  
         BO    VLAXY                                                            
         DC    H'0'                                                             
                                                                                
VLAXN    DS    0H                                                               
         B     NO_01                                                            
*                                                                               
VLAXY    DS    0H                                                               
         B     YES_01                                                           
         DROP  R3                                                               
         TITLE 'SPSFM42 - PW LIST (SUBR01--IFV#)'                               
*------------------------- INIT FILTER VALUES ------------------------*         
                                                                                
* Initialize filter values                                                      
                                                                                
INITFLTV DS    0H                                                               
         MVI   FLTUSED,0           CLEAR FILTERS-USED FLAGS                     
         MVI   FLTUSED2,0                                                       
                                                                                
         L     R2,AFLTABS                                                       
         USING FLTDSECT,R2                                                      
*                                                                               
IFV10    DS    0H                  RUN THROUGH ALL FILTER ENTRIES               
         CLI   0(R2),EOT                                                        
         BE    IFVX                ALL DONE!                                    
                                                                                
         ZICM  R3,FLTODSPL,(3)                                                  
         A     R3,ASYSD            R3--># OF FILTER OUTPUT VALUES               
         MVI   0(R3),0              NONE SO FAR                                 
                                                                                
         LA    R3,1(R3)            R3-->PLACES TO STORE VALUES                  
         MVC   COUNTER,FLTMAXN     MAX # OF THEM                                
*                                                                               
IFV20    DS    0H                                                               
         ZICM  R4,FLTTSTBL,(3)                                                  
         LA    R4,FLTDSECT(R4)     R4-->FILTER TEST TABLES                      
         USING FTSTDSCT,R4                                                      
                                                                                
IFV22    DS    0H                                                               
         CLI   0(R4),EOT           ANY MORE FILTER TESTS?                       
         BE    IFV30                NOPE                                        
                                                                                
         ZIC   R1,FTSLCMP          R1=L(COMPARE FOR FILTERING)                  
         BCTR  R1,0                                                             
         ZICM  RE,FTSMDSPL,(3)                                                  
         AR    RE,R3               RE-->WHERE MODEL VALUE STARTS                
                                                                                
         DS    0H                  ASSUME EXACT MATCH OR LOW VALUE              
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,RE),0(RE)                                                    
         CLI   FTSTYPE,FTSTYXCT    COMPARING FOR EXACT MATCH?                   
         BE    IFV25                YEP                                         
         CLI   FTSTYPE,FTSTYLOW    COMPARING FOR LOW LIMIT?                     
         BE    IFV25                YEP                                         
         CLI   FTSTYPE,FTSTYNCE    INIT TO NULLS FOR THIS TEST TOO!             
         BE    IFV25                YEP                                         
                                                                                
         DS    0H                  ASSUME HIGH VALUE                            
         MVI   0(RE),XFF                                                        
         LTR   R1,R1               CAN WE DO EXECUTED MOVE?                     
         BZ    IFV25                NO!                                         
         BCTR  R1,0                                                             
         EXMVC R1,1(RE),0(RE)                                                   
         CLI   FTSTYPE,FTSTYHI     COMPARING FOR HIGH LIMIT?                    
         BE    IFV25                YEP                                         
         DC    H'0'                                                             
                                                                                
IFV25    DS    0H                  BUMP TO NEXT TEST RULE                       
         LA    R4,FTSTDSCQ(R4)                                                  
         B     IFV22                                                            
         DROP  R4                                                               
*                                                                               
IFV30    DS    0H                  BUMP TO NEXT OUTPUT ENTRY                    
         ZIC   R1,COUNTER                                                       
         SH    R1,=H'1'                                                         
         BZ    IFV40                                                            
         STC   R1,COUNTER                                                       
         ZIC   R1,FLTOLEN                                                       
         AR    R3,R1               R3-->NEXT OUTPUT ENTRY                       
         B     IFV20                                                            
*                                                                               
IFV40    DS    0H                  BUMP TO NEXT FILTER ENTRY                    
         ZIC   R1,FLTLEN                                                        
         AR    R2,R1                                                            
         B     IFV10                                                            
         DROP  R2                                                               
*                                                                               
IFVX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR01--VFD#)'                               
*------------------ VALIDATE FILTER DATE EXPRESSION ------------------*         
                                                                                
* At entry, R2-->Input in SCANNER BLOCK                                         
*           R3-->Filter entry                                                   
* At exit, CC set to equal if valid input, else CC set to not equal             
* COUNTER is used by caller, do not use it here!                                
                                                                                
VLFTDATE DS    0H                                                               
         USING FLTDSECT,R3                                                      
                                                                                
         ZICM  R4,FLTODSPL,(3)                                                  
         A     R4,ASYSD            R4-->OUTPUT AREA                             
         ZIC   RE,FLTOLEN          RE=L(OUTPUT ENTRIES)                         
         ZIC   RF,0(R4)            RF=# OF ENTRIES SO FAR                       
         MR    RE,RE                                                            
         LA    R4,1(RF,R4)         R4-->SLOT TO PUT NEXT ENTRY                  
         USING FTVDATD,R4                                                       
         XC    FTVDLDAT,FTVDLDAT   INITIALIZE LOW  DATE VALUE                   
         MVC   FTVDHDAT,=X'FFFF'        "     HIGH  "     "                     
                                                                                
         ZIC   R1,1(R2)            R1=L(INPUT)                                  
         BCTR  R1,0                                                             
         LA    RE,22(R1,R2)        RE-->LAST CHAR OF INPUT                      
         LR    R0,RE               HOLD ONTO IT                                 
                                                                                
         LTR   R1,R1               THIS IS A VERY IMPORTANT CHECK               
         BNP   VFD10                                                            
         BCTR  R1,0                                                             
         CLI   0(RE),C'+'                                                       
         BE    VFD10                                                            
         CLI   0(RE),C'-'                                                       
         BE    VFD10                                                            
         LA    R1,1(R1)            INCREMENT L(INPUT) BACK UP BY ONE            
                                                                                
VFD10    DS    0H                                                               
         XC    WORK,WORK                                                        
         EXMVC R1,WORK,22(R2)                                                   
         LA    R1,1(R1)            RESTORE ORIGINAL LENGTH OF DATE              
         STC   R1,BYTE                                                          
         XC    PERVALB,PERVALB                                                  
                                                                                
         GOTO1 PERVAL,DMCB,(BYTE,WORK),(X'60',PERVALB)                          
         CLI   4(R1),0                                                          
         BE    VFD15                                                            
         CLI   4(R1),X'04'                                                      
         BE    VFD15                                                            
         MVI   MYERRCD,IDTEQ       INVALID DATE                                 
         B     VFDXN                                                            
                                                                                
VFD15    DS    0H                                                               
         LA    RF,PERVALB                                                       
         USING PERVALD,RF                                                       
         MVC   HALF,PVALCSTA                                                    
         DROP  RF                                                               
         B     VFD20                                                            
*                                                                               
VFD20    DS    0H                                                               
         LR    RE,R0               R0 WAS HOLDING A(LAST CHAR)                  
         CLI   0(RE),C'+'          WANT DATES AFTER INPUT DATE?                 
         BNE   VFD22                                                            
         MVC   FTVDLDAT,HALF        YES, FILL IN LOW DATE ONLY                  
         B     VFD30                                                            
VFD22    CLI   0(RE),C'-'          WANT DATES BEFORE INPUT DATE?                
         BNE   VFD24                                                            
         MVC   FTVDHDAT,HALF        YES, FILL IN HIGH DATE ONLY                 
         B     VFD30                                                            
VFD24    MVC   FTVDLDAT,HALF       FILTER EXACTLY ON INPUT DATE,                
         MVC   FTVDHDAT,HALF        FILL IN LOW & HIGH DATES                    
         B     VFD30                                                            
*                                                                               
VFD30    DS    0H                  UPDATE # OF FILTER VALUES                    
         ZICM  R4,FLTODSPL,(3)                                                  
         A     R4,ASYSD                                                         
         ZIC   R1,0(R4)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,0(R4)                                                         
                                                                                
         B     VFDXY                                                            
         DROP  R3,R4                                                            
                                                                                
VFDXY    DS    0H                                                               
         B     YES_01                                                           
*                                                                               
VFDXN    DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM42 - PW LIST (SUBR01--VFL#)'                               
*---------------- VALIDATE FILTER LOCK/DATE EXPRESSION ---------------*         
                                                                                
* At entry, R2-->Input in SCANNER BLOCK                                         
*           R3-->Filter entry                                                   
* At exit, CC set to equal if valid input, else CC set to not equal             
* COUNTER is used by caller, do not use it here!                                
                                                                                
VLFTLDAT DS    0H                                                               
         USING FLTDSECT,R3                                                      
                                                                                
         ZICM  R4,FLTODSPL,(3)                                                  
         A     R4,ASYSD            R4-->OUTPUT AREA                             
         ZIC   RE,FLTOLEN          RE=L(OUTPUT ENTRIES)                         
         ZIC   RF,0(R4)            RF=# OF ENTRIES SO FAR                       
         MR    RE,RE                                                            
         LA    R4,1(RF,R4)         R4-->SLOT TO PUT NEXT ENTRY                  
         USING FTVLDTD,R4                                                       
         XC    FTVLLDAT,FTVLLDAT   INITIALIZE LOW  DATE VALUE                   
         MVC   FTVLHDAT,=X'FFFF'        "     HIGH  "     "                     
                                                                                
         MVI   FTVLLOCK,C'L'       ASSUME LOCK REQUESTED                        
         CLI   22(R2),C'L'                                                      
         BE    VFL20                                                            
         MVI   FTVLLOCK,C'U'       ASSUME UNLOCK REQUESTED                      
         CLI   22(R2),C'U'                                                      
         BE    VFL20                                                            
         MVI   FTVLLOCK,0          IGNORE LOCK STATUS                           
         B     VFL20                                                            
*                                                                               
VFL20    DS    0H                                                               
         CLI   1(R2),1             DID USER PUT IN A DATE?                      
         BH    VFL20A               YES, IT SEEMS LIKE IT                       
         CLI   FTVLLOCK,0           NO, THEN FTVLLOCK SHOULD                    
         BNE   VFL40                 HAVE A C'U' OR C'L'                        
         MVI   MYERRCD,IFLTQ                                                    
         B     VFLXN                                                            
                                                                                
VFL20A   DS    0H                                                               
         ZIC   R1,1(R2)            R1=L(INPUT)                                  
         LA    RF,22(R2)                                                        
         CLI   FTVLLOCK,0          TO CHECK IF 1ST CHAR IS 'L' OR 'U'           
         BE    VFL22                IT ISN'T                                    
         BCTR  R1,0                                                             
         LA    RF,23(R2)            IT IS, ADJUST FOR IT                        
                                                                                
VFL22    DS    0H                                                               
         BCTR  R1,0                                                             
         LA    RE,0(R1,RF)         RE-->LAST CHAR OF INPUT                      
         LR    R0,RE               HOLD ONTO IT                                 
                                                                                
         LTR   R1,R1               THIS IS A VERY IMPORTANT CHECK               
         BNP   VFL24                                                            
         BCTR  R1,0                                                             
         CLI   0(RE),C'+'                                                       
         BE    VFL24                                                            
         CLI   0(RE),C'-'                                                       
         BE    VFL24                                                            
         LA    R1,1(R1)            INCREMENT L(INPUT) BACK UP BY ONE            
                                                                                
VFL24    DS    0H                                                               
         XC    WORK,WORK                                                        
         EXMVC R1,WORK,0(RF)                                                    
         LA    R1,1(R1)            RESTORE ORIGINAL LENGTH OF DATE              
         STC   R1,BYTE                                                          
         XC    PERVALB,PERVALB                                                  
                                                                                
         GOTO1 PERVAL,DMCB,(BYTE,WORK),(X'60',PERVALB)                          
         CLI   4(R1),0                                                          
         BE    VFL26                                                            
         CLI   4(R1),X'04'                                                      
         BE    VFL26                                                            
         MVI   MYERRCD,IDTEQ       INVALID DATE                                 
         B     VFLXN                                                            
                                                                                
VFL26    DS    0H                                                               
         LA    RF,PERVALB                                                       
         USING PERVALD,RF                                                       
         MVC   HALF,PVALCSTA                                                    
         DROP  RF                                                               
         B     VFL30                                                            
*                                                                               
VFL30    DS    0H                                                               
         LR    RE,R0               R0 WAS HOLDING A(LAST CHAR)                  
         CLI   0(RE),C'+'          WANT DATES AFTER INPUT DATE?                 
         BNE   VFL32                                                            
         MVC   FTVLLDAT,HALF        YES, FILL IN LOW DATE ONLY                  
         B     VFL40                                                            
VFL32    CLI   0(RE),C'-'          WANT DATES BEFORE INPUT DATE?                
         BNE   VFL34                                                            
         MVC   FTVLHDAT,HALF        YES, FILL IN HIGH DATE ONLY                 
         B     VFL40                                                            
VFL34    MVC   FTVLLDAT,HALF       FILTER EXACTLY ON INPUT DATE,                
         MVC   FTVLHDAT,HALF        FILL IN LOW & HIGH DATES                    
         B     VFL40                                                            
         DROP  R4                                                               
                                                                                
*                                                                               
VFL40    DS    0H                  UPDATE # OF FILTER VALUES                    
         ZICM  R4,FLTODSPL,(3)                                                  
         A     R4,ASYSD                                                         
         ZIC   R1,0(R4)                                                         
         LA    R1,1(R1)                                                         
         STC   R1,0(R4)                                                         
                                                                                
         B     VFLXY                                                            
         DROP  R3                                                               
                                                                                
                                                                                
VFLXY    DS    0H                                                               
         B     YES_01                                                           
*                                                                               
VFLXN    DS    0H                                                               
         B     NO_01                                                            
         TITLE 'SPSFM42 - PW LIST (SUBR01--ISL#)'                               
*------------------------- TEST SELECT FIELD -------------------------*         
                                                                                
* Initializes SEL fields.  This routine should only get called when             
*  there is a change of action, key, option, filter, market, or buyer.          
                                                                                
INITSEL  DS    0H                                                               
                                                                                
         LA    R0,NLSTLINQ          CLEAR ALL SELECT FIELDS                     
         LA    R2,PWLSELH                                                       
                                                                                
ISL10    DS    0H                                                               
         MVI   5(R2),0                                                          
         XC    8(L'PWLSEL,R2),8(R2)                                             
         OI    6(R2),X80                                                        
         BCT   R0,ISLBUMP                                                       
         B     ISLX                                                             
                                                                                
ISLBUMP  DS    0H                  MAKE SURE ONLY R1 & R2 ARE CHANGED           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED LISTLINE.                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->NEXT SELECT FIELD                       
         B     ISL10                                                            
*                                                                               
ISLX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR01--TSL#)'                               
*------------------------- TEST SELECT FIELD -------------------------*         
                                                                                
* Validate select code.                                                         
* At exit, FULL = 0 if select fields were ok, else                              
*          FULL = A(field w/ error).                                            
                                                                                
TESTSEL  DS    0H                                                               
         SR    R2,R2               CLEAR R2==>"GOOD" RETURN CODE                
                                                                                
         TM    SELFLAG1,SF1TSLOK   IF SEL CODES OKAY,                           
         BO    TSLX                 NO NEED TO CHECK THEM AGAIN                 
                                                                                
         CLI   LISTNUM,0           ANYTHING ON LIST SCREEN YET?                 
         BE    TSL40                NOPE                                        
*                                                                               
** CHECK LISTED LINES **                                                        
*                                                                               
         LA    R2,PWLSELH          R2-->FIRST SELECT FIELD.                     
         MVC   NTIMES,LISTNUM      CHECK SEL FLD OF LISTED LINES FIRST          
         MVI   COUNTER,0                                                        
                                                                                
                                                                                
TSL10    DS    0H                  START OF LOOP                                
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
*                                                                               
         CLI   5(R2),0             ANY INPUT IN SELECT FIELD?                   
         BE    TSL15                NOPE                                        
                                                                                
         MVI   MYERRCD,ISELQ       ASSUME INVALID SEL CODE ERROR                
         SR    R0,R0                                                            
         L     RE,APFTABS          MATCH INPUT SEL CODE TO PFTABLES             
         USING PFTABD,RE                                                        
TSL12    CLI   0(RE),XFF                                                        
         BE    TSLXNO              THIS SEL FIELD IS NOT OK                     
         MVC   WORK(L'PWLSEL),8(R2)                                             
         OC    WORK(L'PWLSEL),SPACES                                            
         CLC   PFTSEL,WORK         NEED EXACT MATCH                             
         BE    TSL14                                                            
         IC    R0,PFTLEN           BUMP TO NEXT ENTRY PFTABLE ENTRY             
         AR    RE,R0                                                            
         B     TSL12                                                            
                                                                                
TSL14    DS    0H                                                               
         CLI   WHOLIST,C'B'        FOR BLIST,                                   
         BNE   TSL14A                                                           
         CLI   PFTAID,4             DISALLOW SEL CODE OF 'B0' (PF4)             
         BNE   TSL14A                                                           
         MVI   MYERRCD,ISELQ                                                    
         B     TSLXNO                                                           
TSL14A   DS    0H                                                               
         OI    SELFLAG1,SF1MNSEL   FLAG FOR MANUAL SELECTION                    
         B     TSL15                                                            
         DROP  RE                                                               
*                                                                               
TSL15    DS    0H                  CHECK PFKEYS                                 
         CLI   PFKEY,0             ANY PFKEY HIT?                               
         BE    TSL20                                                            
         LH    R1,2(R2)            FIND THE (ROW#-1) OF THIS LINE               
         SR    R0,R0                                                            
         D     R0,=F'80'                                                        
         CLM   R1,1,CURSROW         AND COMPARE TO THAT OF CURSOR'S             
         BNE   TSL20                                                            
                                                                                
         DS    0H                  CURSOR ON THIS LINE WHEN PFKEY HIT           
         MVI   MYERRCD,IPFKQ       ASSUME INVALID PFKEY ERROR                   
         L     RE,APFTABS          MATCH INPUT SEL CODE TO PFTABLES             
         USING PFTABD,RE                                                        
TSL17    CLI   0(RE),XFF                                                        
         BE    TSLXNO              INVALID PFKEY HIT                            
         CLC   PFTAID,PFKEY        FIND MATCHING PF ENTRY IN TABLES             
         BE    TSL18                                                            
         IC    R0,PFTLEN           BUMP TO NEXT ENTRY PFTABLE ENTRY             
         AR    RE,R0                                                            
         B     TSL17                                                            
                                                                                
TSL18    DS    0H                                                               
         CLI   WHOLIST,C'B'        FOR BLIST,                                   
         BNE   TSL18A                                                           
         CLI   PFTAID,4             DISALLOW PF4 (SEL CODE = C'B0')             
         BNE   TSL18A                                                           
         MVI   MYERRCD,IPFKQ                                                    
         B     TSLXNO                                                           
TSL18A   DS    0H                                                               
         OI    SELFLAG1,SF1MNSEL   FLAG FOR MANUAL SELECTION                    
         B     TSL20                                                            
         DROP  RE                                                               
*                                                                               
TSL20    DS    0H                  SEL CODE OKAY, CHECK NEXT ONE                
         BAS   RE,TSLBUMP          BUMP R2 TO NEXT SEL FIELD                    
         CLC   COUNTER,NTIMES      LOOPED THRU ALL LINES W/ DATA YET?           
         BL    TSL10                NO, GO BACK & CHECK THIS LIST LINE          
         B     TSL40                YEP, GO CHECK LINES W/O DATA                
*                                                                               
** CHECK LINES W/O DATA **                                                      
*                                                                               
TSL40    DS    0H                                                               
         MVI   MYERRCD,IFLDQ       ASSUME INVALID INPUT ERROR                   
         LA    RF,PWLPFLNH         RF-->LAST LINE ON SCREEN                     
TSL40A   CR    R2,RF               IF REACHED, THEN EXIT OKAY-LY                
         BNL   TSLXOK                                                           
                                                                                
         CLI   5(R2),0             IF THERE IS INPUT HERE,                      
         BNE   TSLXNO               THERE SHOULDN'T BE                          
                                                                                
         CLI   PFKEY,0             PFKEY SHOULDN'T HAVE BEEN HIT                
         BE    TSL40B               HERE EITHER                                 
         LH    R1,2(R2)                                                         
         SR    R0,R0                                                            
         D     R0,=F'80'           R1 = (ROW NO.) - 1                           
         CLM   R1,1,CURSROW        CURSROW=CRSR'S (ROW#-1) WHEN PFK HIT         
         BE    TSLXNO                                                           
                                                                                
TSL40B   DS    0H                                                               
         BAS   RE,TSLBUMP                                                       
         B     TSL40A                                                           
                                                                                
                                                                                
TSLXNO   DS    0H                  AN INVALID SELECT CODE FOUND                 
         B     TSLX                  R2-->FIELD W/ ERROR                        
*                                                                               
TSLXOK   DS    0H                  NO INVALID SELECT CODE FOUND                 
         OI    SELFLAG1,SF1TSLOK    SET FLAG FOR NO INVALID SEL CODE            
         SR    R2,R2                CLEAR R2 FOR RETURN CODE                    
         B     TSLX                                                             
*                                                                               
TSLX     DS    0H                                                               
         ST    R2,FULL             RETURN "ERROR CODE" IN FULL                  
         B     XIT_01                                                           
                                                                                
                                                                                
TSLBUMP  DS    0H                  MAKE SURE ONLY R1 & R2 ARE CHANGED           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED LISTLINE.                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->NEXT SELECT FIELD                       
         BR    RE                                                               
         TITLE 'SPSFM42 - PW LIST (SUBR01--FSL#)'                               
*------------------------- FILL SELECT FIELDS ------------------------*         
                                                                                
* Routine is useful when all-locking is requested as an option.  It             
*  fills in the select code of a particular locking into the SEL                
*  fields.  This is to help automate the process somewhat.                      
                                                                                
FILLSEL  DS    0H                                                               
         TM    CHNGFLG1,CF1ALL_T   ACTEQU KEY,OPT,FLTR,MKT,BYR CHNGE?           
         BNZ   FSLX                 YES, EXIT ROUTINE NOW                       
                                                                                
         MVI   BYTE,OPUSBBPM       SEE IF ANY SELECT-ALL REQUESTED              
         CLI   WHOLIST,C'M'         MAINT & BILL SCREEN USE DIFF FLAGS          
         BE    *+8                                                              
         MVI   BYTE,OPUBIALL                                                    
         NC    BYTE,OPTUSED                                                     
         BZ    FSLX                 NO, EXIT NOW                                
                                                                                
         OC    LKSELCDE,LKSELCDE   IS THERE A SELECT CODE FILLER?               
         BZ    FSLX                 NO, ALL-LOCK PROBABLY NULLIFIED             
         CLI   LISTNUM,0           ANY THING LISTED YET?                        
         BE    FSLX                 NO, NO USE FILLING IN SEL FIELDS            
         TM    SELFLAG1,SF1MNSEL   ANY MANUAL SELECTIONS?                       
         BO    FSLX                 YES, DO MANUAL SELECTION FIRST              
         CLI   LKALLMDE,C'Y'       IF WE'RE IN ALL-LOCK MODE ALREADY,           
         BE    FSLX                 DON'T REFILL SEL FIELDS                     
                                                                                
         DS    0H                  WE CAN FILL IN SEL CODE                      
         LA    R3,LKSELCDE+L'LKSELCDE-1                                         
         CLI   0(R3),0                                                          
         BNE   *+8                                                              
         BCT   R3,*-8                                                           
         LA    R1,LKSELCDE-1       ADJUSTING FOR OFF-BY-ONE ERROR               
         SR    R3,R1               R3=L(SELECT CODE)                            
         MVC   NTIMES,LISTNUM      NTIMES = LOOP COUNTER                        
         LA    R2,PWLSELH                                                       
*                                                                               
FSL20    DS    0H                                                               
         STC   R3,5(R2)            SET LENGTH INTO FIELD                        
         OI    6(R2),X80                                                        
         MVC   8(L'PWLSEL,R2),LKSELCDE                                          
                                                                                
         ZIC   R1,NTIMES                                                        
         SH    R1,=H'1'                                                         
         BZ    FSL30                                                            
         STC   R1,NTIMES                                                        
         BAS   RE,FSLBUMP                                                       
         B     FSL20                                                            
*                                                                               
FSL30    DS    0H                                                               
         MVI   LKALLMDE,C'Y'       SIGNIFY ALL-LOCKING MODE                     
*                                                                               
FSLX     DS    0H                                                               
         B     XIT_01                                                           
                                                                                
                                                                                
FSLBUMP  DS    0H                  MAKE SURE ONLY R1 & R2 ARE CHANGED           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED LISTLINE.                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->NEXT SELECT FIELD                       
         BR    RE                                                               
         TITLE 'SPSFM42 - PW LIST (SUBR01--VSL#)'                               
*----------------------- VALIDATE SELECT FIELD -----------------------*         
                                                                                
* Validates select code w/ respect to other input.                              
* At exit, FULL = 0 if select fields were ok, else                              
*          FULL = A(field) to put cursor upon error exit.                       
* NOTE: At the time being, routine is suppose to exit w/ FULL=0                 
*        for ACTION=BLIST.                                                      
                                                                                
VALSEL   DS    0H                                                               
         SR    R2,R2               CLEAR R2==>"GOOD" RETURN CODE                
                                                                                
         CLI   LISTNUM,0           ANYTHING ON LIST SCREEN YET?                 
         BE    VSLX                 NOPE                                        
*                                                                               
** CHECK LISTED LINES **                                                        
*                                                                               
         LA    R2,PWLSELH          R2-->FIRST SELECT FIELD.                     
         MVC   NTIMES,LISTNUM      CHECK SEL FLD OF LISTED LINES                
         MVI   COUNTER,0                                                        
*                                                                               
VSL10    DS    0H                  START OF LOOP                                
         ZIC   R1,COUNTER                                                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
                                                                                
         CLI   PFKEY,0             ANY PF KEY HIT?                              
         BNE   VSL20                YES, THIS FIELD NEED TO BE CHECKED          
         OC    8(3,R2),8(R2)       ANY INPUT IN THIS FIELD?                     
         BZ    VSL100               NO, DON'T NEED TO CHECK IT                  
*                                                                               
VSL20    DS    0H                                                               
         CLI   WHOLIST,C'M'        STATION-LOCKIN IS FOR                        
         BNE   VSL30                MAINT (BUYER'S) LIST ONLY                   
         CLC   8(3,R2),=X'E2D300'  CHECK IF STATION-LOCKIN REQUESTED            
         BE    VSL25                                                            
         CLI   PFKEY,5              BY "SL " OR PF5 HIT                         
         BNE   VSL30                                                            
                                                                                
         LH    R1,2(R2)            WAS CRSR W/IN THIS FLD WHEN PF5 HIT?         
         SR    R0,R0                CALCULATE (ROW#-1) OF THIS LINE             
         D     R0,=F'80'             R1 = ROW# - 1                              
         CLM   R1,1,CURSROW         AND COMPARE IT TO THAT OF CURSOR            
         BE    VSL25               YES!  PF5 HIT ON THIS FIELD                  
         B     VSL30                                                            
                                                                                
VSL25    DS    0H                  REQUIREMENTS FOR STATION-LOCKIN              
         CLC   BUYRID,SPACES       USER MUST INPUT BUYER'S INITIALS!            
         BNE   VSL26                                                            
         MVI   MYERRCD,MBUYRQ                                                   
         B     VSLXNO                                                           
                                                                                
VSL26    DS    0H                  BUY & PW MUST BE LOCKED!                     
         ZIC   RE,COUNTER                                                       
         BCTR  RE,0                                                             
         LA    RE,LISTINFO(RE)                                                  
         TM    0(RE),PWGNBILQ+PWGNPLKQ                                          
         BO    VSL27                                                            
         MVI   MYERRCD,BPNLQ                                                    
         B     VSLXNO                                                           
                                                                                
VSL27    DS    0H                                                               
         B     VSL30                                                            
*                                                                               
VSL30    DS    0H                  CHECK IF RESTORE VALID                       
         CLI   WHOLIST,C'M'        RESTORE IS FOR                               
         BNE   VSL40                MAINT (BUYER'S) LIST ONLY                   
         CLC   8(3,R2),=X'D90000'  CHECK IF RESTORE REQUESTED                   
         BE    VSL35                                                            
         CLI   PFKEY,3              BY "R  " OR PF3 HIT                         
         BNE   VSL40                                                            
                                                                                
         LH    R1,2(R2)            WAS CRSR W/IN THIS FLD WHEN PF3 HIT?         
         SR    R0,R0                CALCULATE (ROW#-1) OF THIS LINE             
         D     R0,=F'80'             R1 = ROW# - 1                              
         CLM   R1,1,CURSROW         AND COMPARE IT TO THAT OF CURSOR            
         BE    VSL35               YES!  PF3 HIT ON THIS FIELD                  
         B     VSL40                                                            
                                                                                
VSL35    DS    0H                  REQUIREMENTS FOR RESTORE                     
         ZIC   RE,COUNTER                                                       
         BCTR  RE,0                                                             
         LA    RE,LISTINF1(RE)                                                  
         TM    0(RE),ATF1NOPW      MUST HAVE PW RECORD FOR MARKET               
         BZ    VSL36                                                            
         MVI   MYERRCD,NRMKQ                                                    
         B     VSLXNO                                                           
                                                                                
VSL36    DS    0H                  MUST HAVE BACKUP ELEMENTS                    
         ZIC   RE,COUNTER                                                       
         BCTR  RE,0                                                             
         LA    RE,LISTINF2(RE)                                                  
         TM    0(RE),ATF2BKUP                                                   
         BO    VSL37                                                            
         MVI   MYERRCD,NBKPQ                                                    
         B     VSLXNO                                                           
                                                                                
VSL37    DS    0H                  (NEXT REQUIREMENT FOR RESTORE)               
         B     VSL40                                                            
*                                                                               
VSL40    DS    0H                  FUDGE FPKEY FOR BLIST B=ALL0                 
         CLI   WHOLIST,C'B'        THIS IS FOR                                  
         BNE   VSL50                BILLER'S LIST ONLY                          
         TM    OPTUSED,OPUBIALL     AND THIS OPTION ONLY                        
         BZ    VSL50                                                            
         CLC   8(3,R2),=X'C2F000'  B=ALL0 OPTION HAS THIS SEL CODE              
         BNE   VSL50                                                            
         MVI   PFKEY,4             FUDGE PF4                                    
*                                                                               
VSL50    DS    0H                  (NEXT THING TO CHECK)                        
                                                                                
         OI    SELFLAG1,SF1SELCT   FLAG THAT A VALID SELECTION MADE             
         B     VSL100                                                           
*                                                                               
VSL100   DS    0H                  THIS SEL FIELD OK, CHECK NEXT ONE            
         CLC   COUNTER,NTIMES      LOOPED THRU ALL LINES W/ DATA YET?           
         BE    VSLXOK               YEP, CHECK LINES W/O DATA                   
         BAS   RE,VSLBUMP           NO, BUMP R2 TO NEXT SEL FIELD               
         B     VSL10                 AND CHECK IT                               
                                                                                
                                                                                
VSLXNO   DS    0H                  SEL FLD INPT CNFLICTNG W/ OTHR INPT          
         B     VSLX                  R2-->FIELD W/ ERROR                        
*                                                                               
VSLXOK   DS    0H                  SEL FLD COMPATIBLE W/ OTHER INPUT            
         SR    R2,R2                CLEAR R2 FOR RETURN CODE                    
         B     VSLX                                                             
*                                                                               
VSLX     DS    0H                                                               
         ST    R2,FULL             RETURN "ERROR CODE" IN FULL                  
         B     XIT_01                                                           
                                                                                
                                                                                
VSLBUMP  DS    0H                  MAKE SURE ONLY R1 & R2 ARE CHANGED           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->PROTECTED LISTLINE.                     
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               R2-->NEXT SELECT FIELD                       
         BR    RE                                                               
         TITLE 'SPSFM42 - PW LIST (SUBR01--IPF#)'                               
*------------------------- INITIALIZE PFKEYS -------------------------*         
                                                                                
IPFKEY   DS    0H                                                               
                                                                                
         MVI   CALLPFK,0                                                        
         LA    R0,PWLSELH                                                       
         ST    R0,ACURFORC                                                      
         XC    BINYRMT,BINYRMT     NULLS IN THIS SHARED (SYSSPARE) FLD          
         CLI   WHOLIST,C'B'         UNLESS IT'S ACTION=BLIST                    
         BNE   *+10                                                             
         MVC   BINYRMT,BILLYRMT     PASS IN A MONTH W/IN ESTIMATE               
         GOTO1 INITPFKY,DMCB,APFTABS                                            
                                                                                
         TM    TRNSTAT,RETURNED                                                 
         BZ    IPFX                                                             
                                                                                
         TM    SELFLAG1,SF1MNSEL   IF USER MANUALLY ENTERRED SEL CODE,          
         BZ    *+10                                                             
         MVC   LASTSELK,FRSTSELK    DISPLAY SAME PAGE OF MARKETS                
                                                                                
         DS    0H                                                               
         MVI   BYTE,OPUSBBPM       MAINT & BILL HAVE DIFF FLAGS                 
         CLI   WHOLIST,C'M'                                                     
         BE    *+8                                                              
         MVI   BYTE,OPUBIALL                                                    
         NC    BYTE,OPTUSED        WAS ANY SELECT-ALL REQUESTED?                
         BZ    IPFX                 NO                                          
                                                                                
         OC    LKSELCDE,LKSELCDE   IS THERE A SEL CODE FILLER?                  
         BZ    IPFX                 NO                                          
         TM    SELFLAG1,SF1MNSEL   DID USER MANUALLY ENTER SEL CODE?            
         BO    IPFX                 YES, EXIT NOW TO PROMPT USER AGAIN          
                                                                                
         CLI   LKALLMDE,C'Y'       WERE WE IN ALL-LOCKING MODE?                 
         BNE   *+8                                                              
         MVI   LKALLMDE,C'N'        YES, TURN IT OFF SINCE WE CAME BACK         
         TM    MISCFLG1,MF1LSTDN                                                
         BZ    IPFX                                                             
                                                                                
         DS    0H                  ALL-LOCKING DONE FOR EACH MARKET             
         LA    RE,LKSELCDE-1                                                    
         ZIC   R1,0(RE)            R1=DSPL INTO OPTIONS FIELD OF OPTN           
         LA    R2,PWLOPTN(R1)                                                   
         MVI   0(R2),C'*'          NULLIFY ALL-LOCKING FOR NEXT TIME            
         OI    PWLOPTNH+6,X80                                                   
         MVI   LKALLDNE,C'Y'                                                    
         B     IPFX                                                             
*                                                                               
IPFX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR01--PWS#)'                               
*-------------------------- GENERAL PW SETUP -------------------------*         
                                                                                
PWSETUP  DS    0H                                                               
         NI    CHNGFLG1,XFF-CF1ACCTB   ASSME '*ACTB42*' STILL IN TMPSTR         
                                                                                
         TM    TRNSTAT,RETURNED    WAS RETPROG CALLED?                          
         BO    PWS10                YES, JUST RESTORE TABLES                    
                                                                                
         MVI   BYTE,CF1AKO         CHECK ACTEQU, KEY, OR OPTN (BLIST)           
         CLI   WHOLIST,C'B'                                                     
         BE    *+8                                                              
         MVI   BYTE,CF1AK          CHECK ACTEQU OR KEY (LIST)                   
         NC    BYTE,CHNGFLG1       IF THEY CHANGED                              
         BNZ   PWS20                                                            
                                                                                
         TM    MISCFLG1,MF1BLTAB   WERE TABLES BUILT FOR THIS KEY?              
         BZ    PWS20                NOPE, BUILD THEM NOW                        
         B     PWS10               ELSE, GO RESTORE TABLES                      
*                                                                               
** RESTORE TIA TABLES **                                                        
*                                                                               
PWS10    DS    0H                  RESTORE TIA TABLES                           
         MVI   GOSUBN,RTI#                                                      
         GOTO1 AGOSUB                                                           
                                                                                
         L     RF,ATIA                                                          
         LA    RF,(ACTBLABL-SPOTAREA)(RF)  SEE IF WE HAVE THE                   
         CLC   =C'*ACTB42*',0(RF)           RIGHT ACCUTAB                       
         BE    PWS30                       IF YES, RE-PROCESS ACCUTAB,          
         MVC   0(8,RF),=C'*ACTB42*'         RIGHT ACCUTAB                       
         OI    CHNGFLG1,CF1ACCTB                                                
         B     PWS20                        ELSE, REBUILD TABLES                
*                                                                               
** BUILD TABLES **                                                              
*                                                                               
PWS20    DS    0H                   BUILD EVERYTHING FROM SCRATCH               
         XC    ACCUTABN,ACCUTABN     NO ENTRIES IN ACCUTAB YET                  
         L     RE,AACCUTAB                                                      
         LH    RF,=Y(ACCUTABX-ACCUTAB)                                          
         XCEF                                                                   
                                                                                
         MVI   GOSUBN,GBT#         GET WEEKS OF ESTIMATE                        
         GOTO1 AGOSUB                                                           
         MVI   GOSUBN,GMB#         GET MARKETS W/ BUYS IN THEM                  
         GOTO1 (RF)                                                             
         MVI   GOSUBN,GMG#         GET MARKETS W/ GOALS IN THEM                 
         GOTO1 (RF)                                                             
         MVI   GOSUBN,GMP#         GET MARKETS HAVING PW RECORDS                
         GOTO1 (RF)                                                             
         MVI   GOSUBN,GSB#         LOOK FOR ESTIM BILLED                        
         GOTO1 (RF)                                                             
         MVI   GOSUBN,GBI#         LOOK FOR ESTIM/FINAL BILLED                  
         GOTO1 (RF)                                                             
                                                                                
         OI    MISCFLG1,MF1BLTAB   ACCUTAB BUILT                                
         B     PWS30               GO PROCESS ACCUTAB                           
*                                                                               
** PROCESS ACCUTAB **                                                           
*                                                                               
PWS30    DS    0H                  PROCESS ACCUTAB                              
         TM    CHNGFLG1,CF1AKOFM   ACTEQU,KEY,OPT,FLTR,MKT CHNGE?               
         BZ    *+10                                                             
         XC    LASTSELK,LASTSELK    YES, START LISTING FROM THE TOP             
         TM    CHNGFLG1,CF1BYR     BUYER CHANGED?                               
         BZ    *+10                                                             
         MVC   LASTSELK,FRSTSELK    YES, LIST SAME PAGE                         
                                                                                
         TM    CHNGFLG1,CF1ALL_B   ACTEQU,KEY,OPT,FLTR,MKT,ACCTB CHNGE?         
         BNZ   PWS35                YES, NEED TO PROCESS ACCUTAB                
         TM    SELFLAG1,SF1SELCT   ANY SELECTION MADE?                          
         BO    PWS35                YES, PW RECDS MAY HAVE CHANGED              
         B     PWS40                                                            
                                                                                
PWS35    DS    0H                                                               
         MVI   GOSUBN,PAT#          YES, PROCESS ACCUTAB                        
         GOTO1 AGOSUB                                                           
         B     PWS40                                                            
*                                                                               
** SAVE TIA TABLES **                                                           
*                                                                               
PWS40    DS    0H                                                               
         MVI   GOSUBN,STI#         SAVE TIA TABLES                              
         GOTO1 AGOSUB                                                           
         B     PWSX                                                             
                                                                                
                                                                                
PWSX     DS    0H                                                               
         B     XIT_01                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR01--SDC#)'                               
*------------------------ SET DR/CR MONTH BITS -----------------------*         
                                                                                
* Sets the month bits for which there are Adj DR/CR dollar amounts into         
*  DCMTHBT, and for which there are no Adj DR/CR into NDCMTHBT.                 
* At entry,                                                                     
*   APWREC = address of mkt-level PW record.                                    
* At exit, DCMTHBT and NDCMTHBT are set.                                        
                                                                                
SDCMTHBT DS    0H                                                               
         XC    WORK,WORK           USED AS MNTH TBLE (DR/CR & NO DR/CR)         
         LA    R2,BRDMTHTB                                                      
         LA    R3,BRDWKTB2                                                      
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
         MVI   ELCODE,PWDOLCDQ                                                  
                                                                                
SDC10    DS    0H                  START OF LOOP                                
         CLI   0(R2),XFF           DID ALL MONTHS IN ESTIM YET?                 
         BE    SDC40                YEP, EXIT LOOP                              
                                                                                
SDC15    DS    0H                  FIND STRT-END DATES OF 1ST WK                
         CLI   0(R3),XFF            SHOULD NOT REACH END OF BRDWKTB2            
         BE    SDC40                 BUT W/ OOWR's, IT CAN                      
*&&DO                                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
*&&                                                                             
         CLC   0(2,R2),0(R3)       (BCST MNTH STRT) VS (BCST WEEK STRT)         
         BNH   SDC20                                                            
         LA    R3,4(R3)             IF HIGH, KEEP BUMPING WEEK UNTIL            
         B     SDC15                 BCST MNTH STRT <= BCST WEEK STRT           
                                                                                
SDC20    DS    0H                  R3-->STRT-END DATES OF 1ST WEEK              
         L     R6,APWREC                                                        
         LA    R4,WORK+12          ASSUME NO DR/CR FOR THIS MONTH               
                                                                                
         BAS   RE,GETEL                                                         
         B     SDC20B                                                           
SDC20A   BAS   RE,NEXTEL                                                        
SDC20B   BNE   SDC30               NO ADJ DR/CR FOR THIS MONTH                  
                                                                                
         USING PWDOLEL,R6                                                       
         CLC   PWDOLWK,0(R3)       ELEM DATE < BCST WEEK STRT DATE              
         BL    SDC20A               GET NEXT PWDOLEL                            
         CLC   PWDOLWK,2(R3)       ELEM DATE > BCST WEEK END  DATE              
         BH    SDC30                NO ADJ DR/CR                                
         OC    PWDOLBIL,PWDOLBIL   ELEM IS 1ST WK OF MNTH, ANY DR/CR?           
         BZ    SDC30                NOPE, NO ADJ DR/CR                          
         DROP  R6                                                               
                                                                                
         LA    R4,WORK             POINT TO DR/CR SCTION OF MTH # TBLE          
         B     SDC30                                                            
*                                                                               
SDC30    DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,2(R2)),(3,DATE6),0                                
                                                                                
         ZIC   R1,DATE6+1          DATE6(3)=BINARY BDCST MNTH END DATE          
         AR    R4,R1               STICK MNTH # INTO APPROPRIATE                
         STC   R1,0(R4)             SECTION OF MNTH # TABLE                     
*                                                                               
         DS    0H                  DO NEXT MONTH IN ESTIMATE PERIOD             
         LA    R2,4(R2)                                                         
         B     SDC10                                                            
*                                                                               
SDC40    DS    0H                                                               
         MVI   GOSUBN,CMB#                                                      
         GOTO1 AGOSUB                                                           
         MVC   DCMTHBT,HALF        MNTHS W/ DR/CR                               
                                                                                
         MVC   WORK+1(12),WORK+13                                               
         GOTO1 (RF)                                                             
         MVC   NDCMTHBT,HALF       MNTHS W/O DR/CR                              
                                                                                
         B     XIT_01                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR01--PAT#)'                               
*--------------------------- PROCESS ACCUTAB -------------------------*         
                                                                                
* This routine processes ACCUTAB entries.  It does three things with            
*  each entry:                                                                  
*   - gets the PW record for the market in the entry,                           
*   - moves relevent data into entry from the record, and                       
*   - indicates in the entry whether it passes filter or not                    
*   - sums up the dollars amounts for those mkts passing filter.                
* And for the buyer's (MAINT) list screen, it exclusively                       
*   - indicates in the entry whether locked CLT$ > goals or not.                
                                                                                
PROCACTB DS    0H                                                               
         L     R2,AACCUTAB                                                      
         USING ACCUTABD,R2                                                      
                                                                                
         DS    0H                  CLEAR TOTAL FIELDS                           
         XC    TOTLVALS(TOTLVALQ),TOTLVALS                                      
         NI    MISCFLG1,XFF-MF1DRCR  ASSUME NO DRCR AMTS (BLIST)                
*                                                                               
PAT10    DS    0H                                                               
         OC    ATMRKT,ATMRKT       ANY MORE ENTRIES IN ACCUTAB?                 
         BZ    PATX                 NOPE, ALL DONE!                             
                                                                                
         MVC   TEMPBMKT,ATMRKT                                                  
         MVI   GOSUBN,GPR#                                                      
         GOTO1 AGOSUB              GET PW RECORD FOR MARKET                     
                                                                                
         DS    0H                  BRANCH TO PROCESS FOR DIFF LISTS             
         CLI   WHOLIST,C'M'                                                     
         BE    PAT20                                                            
         CLI   WHOLIST,C'B'                                                     
         BE    PAT100                                                           
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
** BUYER'S (MAINT) LIST **                                                      
*                                                                               
PAT20    DS    0H                  GET STUFF WE NEED FROM PW RECORD             
         L     R3,APWREC                                                        
         USING PWRECD,R3                                                        
                                                                                
         NI    ATFLAG1,XFF-ATF1NOPW                                             
         OC    DMDSKADD,DMDSKADD   ANY PW RECORD FOR MARKET?                    
         BNZ   PAT22                                                            
         OI    ATFLAG1,ATF1NOPW     NOPE, FLAG IT                               
                                                                                
PAT22    DS    0H                  SEE IF ANY BACKUP ELEMENTS                   
         NI    ATFLAG2,XFF-ATF2BKUP                                             
         LA    R6,(PWEL-PWRECD)(R3)                                             
PAT22A   CLI   0(R6),0             AT EORECORD?                                 
         BE    PAT22X               YES, NO BACKUP ELEMENTS                     
         CLI   0(R6),X'F1'         ACTIVITY ELEMENT?                            
         BE    PAT22B               YES, IGNORE IT                              
         TM    0(R6),X80           IS THIS A BACKUP ELEMENT?                    
         BZ    PAT22B                                                           
         OI    ATFLAG2,ATF2BKUP     YES, FLAG IT                                
         B     PAT22X                AND EXIT LOOP NOW                          
PAT22B   SR    R0,R0                NOPE, BUMP TO NEXT ELEMENT                  
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     PAT22A                                                           
PAT22X   DS    0H                                                               
                                                                                
         MVC   ATPWFLAG,PWGNFLG      PUT PWGNFLG INTO ACCUTAB                   
                                                                                
         NI    ATFLAG1,XFF-ATF1PGLS  SET GOALS-IN-PW-RECORD FLAG                
         OC    PWGNGOAL,PWGNGOAL                                                
         BZ    PAT23GX                                                          
         TM    PWGNFLG,PWGNUGOL       WAS GOAL AMOUNT INPUTTED BY USER?         
         BZ    PAT23GX                 NOPE, IGNORE GOAL IN PW RECD             
         OI    ATFLAG1,ATF1PGLS                                                 
PAT23GX  EQU   *                                                                
                                                                                
         LR    R6,R3                 GET PWDTIEL                                
         MVI   ELCODE,PWDTICDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
         BAS   RE,GETEL                                                         
         BNE   PAT23JX                                                          
         MVC   ATDTIELM,0(R6)                                                   
         LA    RF,ATDTIELM                                                      
         USING PWDTIEL,RF                                                       
         CLC   PWDTISLD,RELOCKSW      IF FLAGGED FOR STATION RELOCK,            
         BNE   PAT23JX                                                          
         XC    PWDTISLD,PWDTISLD       CLEAR STA-LOCK DATE                      
         DROP  RF                                                               
PAT23JX  EQU   *                                                                
                                                                                
         SR    R4,R4                 GET WIMLOCK$ TOTAL,                        
         SR    R5,R5                  CLTLOCK$ TOTAL,                           
         SR    R7,R7                  AND TAXLOCK$ TOTAL                        
         LR    R6,R3                                                            
         MVI   ELCODE,PWDOLCDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
                                                                                
         BAS   RE,GETEL                                                         
         B     PAT24B                                                           
PAT24A   BAS   RE,NEXTEL                                                        
PAT24B   BNE   PAT24D                                                           
         USING PWDOLEL,R6                                                       
         ICM   R0,15,PWDOLWG                                                    
         AR    R4,R0                                                            
         ICM   R0,15,PWDOLTAX                                                   
         AR    R7,R0                                                            
                                                                                
         DS    0H                  CLTLOCK$: CHECK FOR OVERRIDES                
         GOTO1 HELLO,DMCB,(C'G',SYSFIL),('PWCLLCDQ',(R3)),(2,PWDOLWK),0         
         CLI   12(R1),X'06'                                                     
         BE    PAT24C               NO OVERRIDES, PROCESS NORMAL                
         CLI   12(R1),0                                                         
         BE    PAT24A               ELSE, SKIP OVERRIDES                        
         B     DIE_01                                                           
PAT24C   ICM   R0,15,PWDOLCG                                                    
         AR    R5,R0                                                            
                                                                                
         B     PAT24A                                                           
         DROP  R6                                                               
PAT24D   STCM  R4,15,ATWLOCK                                                    
         STCM  R5,15,ATCLOCK                                                    
         STCM  R7,15,ATTAXLK                                                    
                                                                                
         SR    R4,R4                 GET CLTLOCK$ OVERRIDES (IF ANY)            
         LR    R6,R3                                                            
         MVI   ELCODE,PWCLLCDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
                                                                                
         BAS   RE,GETEL                                                         
         B     PAT25B                                                           
PAT25A   BAS   RE,NEXTEL                                                        
PAT25B   BNE   PAT25C                                                           
         USING PWCLLEL,R6                                                       
         ICM   R0,15,PWCLLAMT                                                   
         AR    R4,R0                                                            
         B     PAT25A                                                           
         DROP  R6                                                               
PAT25C   ICM   R0,15,ATCLOCK                                                    
         AR    R0,R4                                                            
         STCM  R0,15,ATCLOCK                                                    
*                                                                               
         NI    ATFLAG1,XFF-ATF1LCGG  COMPARE LOCKED CLT$ VS CLT GOALS           
         CLC   PWGNGOAL,ATCLOCK      IF LOCKED CLT$ > GOALS                     
         BNL   *+8                                                              
         OI    ATFLAG1,ATF1LCGG       INDICATE IT IN ENTRY                      
*                                                                               
         DS    0H                  RUN THROUGH FILTER TEST                      
         MVC   DTIELEM,ATDTIELM                                                 
                                                                                
         DS    0H                  BUYS STATUS                                  
         MVI   ANYBUYS,C'Y'                                                     
         TM    ATFLAG1,ATF1FBYS                                                 
         BO    *+8                                                              
         MVI   ANYBUYS,C'N'                                                     
                                                                                
         DS    0H                  GOALS STATUS                                 
         MVI   ANYGOAL,C'+'                                                     
         TM    ATFLAG1,ATF1LCGG                                                 
         BO    PAT31                                                            
         MVI   ANYGOAL,C'Y'                                                     
         TM    ATFLAG1,ATF1FGLS                                                 
         BO    PAT31                                                            
         MVI   ANYGOAL,C'M'                                                     
         TM    ATFLAG1,ATF1PGLS                                                 
         BO    PAT31                                                            
         MVI   ANYGOAL,C'N'                                                     
         BO    PAT31                                                            
                                                                                
PAT31    DS    0H                  BUY LOCK STATUS                              
         MVI   BUYLOCK,C'U'                                                     
         TM    ATPWFLAG,PWGNBILQ                                                
         BZ    PAT32                                                            
         MVI   BUYLOCK,C'L'                                                     
                                                                                
PAT32    DS    0H                  PW  LOCK STATUS                              
         MVI   PWLOCK,C'U'                                                      
         TM    ATPWFLAG,PWGNPLKQ                                                
         BZ    PAT35                                                            
         MVI   PWLOCK,C'L'                                                      
                                                                                
PAT35    DS    0H                                                               
         MVI   GOSUBN,SDC#         SET DR/CR MONTH BITS                         
         GOTO1 AGOSUB                                                           
                                                                                
         NI    ATFLAG1,XFF-ATF1PFLT                                             
         CLC   ATMRKT,SVINPMKT     MUST BE AT LEAST START-AT MKT                
         BL    PAT40                                                            
         MVI   GOSUBN,FTE#                                                      
         GOTO1 AGOSUB                                                           
         BNE   PAT40                                                            
                                                                                
         TM    ATFLAG1,ATF1FGLS+ATF1PGLS  LIST MARKETS WITH GOALS               
         BNZ   PAT036X                                                          
         TM    ATFLAG1,ATF1FBYS           LIST MARKETS WITH BUYS                
         BNZ   PAT036X                                                          
         OC    ATWLOCK,ATWLOCK            LIST MARKETS WITH LOCKED $$           
         BNZ   PAT036X                                                          
         B     PAT40                      ELSE, DON'T LIST MARKET               
PAT036X  EQU   *                                                                
                                                                                
         DS    0H                  MARKET PASSES FILTER                         
         OI    ATFLAG1,ATF1PFLT     INDICATE IT IN ENTRY                        
         ICM   R0,15,ATWLOCK       UPDATE TOTAL WIM LOCK$                       
         A     R0,WIMLKTOT                                                      
         ST    R0,WIMLKTOT                                                      
         ICM   R0,15,ATCLOCK       UPDATE TOTAL CLT LOCK$                       
         A     R0,CLTLKTOT                                                      
         ST    R0,CLTLKTOT                                                      
         ICM   R0,15,ATTAXLK       UPDATE TOTAL LOCKED TAX$                     
         A     R0,TAXLKTOT                                                      
         ST    R0,TAXLKTOT                                                      
         ICM   R0,15,PWGNGOAL      UPDATE TOTAL GOALS                           
         A     R0,GOALTOTL                                                      
         ST    R0,GOALTOTL                                                      
         B     PAT40                                                            
*                                                                               
PAT40    DS    0H                                                               
         DROP  R3                                                               
*                                                                               
PAT50    DS    0H                                                               
         LA    R2,ACCUTABQ(R2)     BUMP TO NEXT ACCUTAB ENTRY                   
         B     PAT10                                                            
         EJECT                                                                  
*                                                                               
** BILLER'S LIST SCREEN (BLIST) **                                              
*                                                                               
PAT100   DS    0H                                                               
         L     R3,APWREC                                                        
         USING PWRECD,R3                                                        
*                                                                               
*** GET RELEVENT DATA ***                                                       
*                                                                               
         MVC   ATPWFLAG,PWGNFLG    PUT PWGNFLG INTO ACCUTAB                     
                                                                                
         LR    R6,R3                 GET PWDTIEL                                
         MVI   ELCODE,PWDTICDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
         BAS   RE,GETEL                                                         
         BNE   PAT105X                                                          
         MVC   ATDTIELM,0(R6)      PUT PWDTIEL INTO ACCUTAB                     
         LA    RF,ATDTIELM                                                      
         USING PWDTIEL,RF                                                       
         CLC   PWDTISLD,RELOCKSW   IF FLAGGED FOR STATION RELOCK,               
         BNE   PAT105X                                                          
         XC    PWDTISLD,PWDTISLD    CLEAR STA-LOCK DATE                         
         DROP  RF                                                               
PAT105X  EQU   *                                                                
                                                                                
         DS    0H                  GET ADJ DR/CR AMOUNT                         
         LA    R4,BRDMTHTB         DETERMINE STRT/END FOR BDCST MNTH            
PAT110A  CLI   0(R4),XFF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(2,2(R4)),(0,DATE6),0                                
         GOTO1 AGETBROD,DMCB,(1,DATE6),STARTEND,AGETDAY,AADDAY                  
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(3,DATE3),0                           
         CLC   BILLYRMT,DATE3                                                   
         BE    PAT120                                                           
         LA    R4,4(R4)                                                         
         B     PAT110A                                                          
                                                                                
PAT120   DS    0H                  FIND PWDOLCD ELEM W/ DRCR FOR MNTH           
         XC    ATDRCR,ATDRCR                                                    
         MVI   ELCODE,PWDOLCDQ                                                  
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
         LR    R6,R3                                                            
         BAS   RE,GETEL                                                         
         B     PAT120B                                                          
PAT120A  BAS   RE,NEXTEL                                                        
PAT120B  BNE   PAT120X                                                          
         USING PWDOLCD,R6                                                       
         CLC   PWDOLWK,0(R4)                                                    
         BL    PAT120A                                                          
PAT120C  MVC   ATDRCR,PWDOLBIL                                                  
PAT120X  DS    0H                                                               
         DROP  R6                                                               
                                                                                
         DS    0H                  DETERMINE IF ANY DR/CR AMOUNT                
         MVI   ANYDRCR,C'N'                                                     
         OC    ATDRCR,ATDRCR                                                    
         BZ    *+8                                                              
         MVI   ANYDRCR,C'Y'                                                     
                                                                                
         DS    0H                  GET BILL TYPE                                
         MVI   BILLTYPE,2           ASSUME FINAL BILLING                        
         TM    ATFLAG1,ATF1FNLB                                                 
         BO    PAT130X                                                          
         MVI   BILLTYPE,1           ASSUME ESTIM BILLING                        
         TM    ATFLAG1,ATF1ESTB                                                 
         BO    PAT130X                                                          
         MVI   BILLTYPE,XFF         NO BILLING DONE YET (FOR THIS MNTH)         
PAT130X  DS    0H                                                               
                                                                                
         B     PAT150                                                           
*                                                                               
*** RUN THROUGH FILTER TEST ***                                                 
*                                                                               
PAT150   DS    0H                                                               
         MVC   DTIELEM,ATDTIELM                                                 
                                                                                
         MVI   PWLOCK,C'U'         PW LOCK STATUS                               
         TM    ATPWFLAG,PWGNPLKQ                                                
         BZ    *+8                                                              
         MVI   PWLOCK,C'L'                                                      
                                                                                
         NI    ATFLAG1,XFF-ATF1PFLT                                             
         CLC   ATMRKT,SVINPMKT     MUST BE AT LEAST START-AT MKT                
         BL    PAT160                                                           
         OC    DMDSKADD,DMDSKADD    AND MUST HAVE PW RECD ON FILE               
         BZ    PAT160                                                           
         MVI   GOSUBN,FTE#                                                      
         GOTO1 AGOSUB                                                           
         BNE   PAT160                                                           
                                                                                
         DS    0H                  MARKET PASSES FILTER                         
         OI    ATFLAG1,ATF1PFLT     INDICATE IT IN ENTRY                        
         ICM   R0,15,ATDRCR         AND UPDATE TOTAL DRCR AMOUNT                
         BZ    *+8                                                              
         OI    MISCFLG1,MF1DRCR    A DRCR AMOUNT IN MKT PASSING FLTR            
         CLM   R0,15,=X'80000000'                                               
         BNE   *+6                                                              
         SR    R0,R0                                                            
         A     R0,DRCRTOTL                                                      
         ST    R0,DRCRTOTL                                                      
         B     PAT160                                                           
*                                                                               
PAT160   DS    0H                  PROCESS NEXT ACCUTAB ENTRY                   
         LA    R2,ACCUTABQ(R2)                                                  
         B     PAT10                                                            
                                                                                
                                                                                
PATX     DS    0H                                                               
         B     XIT_01                                                           
         DROP  R2                                                               
                                                                                
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (SUBR01--LTORG && CONSTANT+        
                S)'                                                             
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
X6FFS    DC    6X'FF'                                                           
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (SUBR01--MISC STUFF)'              
*--------------------- SUBR01 MISCELLANEOUS STUFF --------------------*         
                                                                                
         GETEL R6,DATADISP,ELCODE                                               
                                                                                
*                                                                               
SUBR01L  EQU   *-SUBR01                                                         
         DS    0CL(X'1000'-SUBR01L+1)                                           
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (SUBR02)'                          
***********************************************************************         
*======================== SUBROUTINE POOL TWO ========================*         
SUBR02Q  EQU   ((((*-T21742)/4096)+1)*4096)                                     
                                                                                
         ORG   T21742+SUBR02Q                                                   
SUBR02   NMOD1 0,**4202**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE                      
         SH    R1,=Y(R01#)          SUBTRACT FOR SUB-RTN # 2                    
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R02_00(R1)                                                       
                                                                                
PWPW#    EQU   ((R02_01-R02_00)/4+1)+R01#                                       
RUP#     EQU   ((R02_02-R02_00)/4+1)+R01#                                       
CAN#     EQU   ((R02_03-R02_00)/4+1)+R01#                                       
FCW#     EQU   ((R02_04-R02_00)/4+1)+R01#                                       
FPL#     EQU   ((R02_05-R02_00)/4+1)+R01#                                       
BGI#     EQU   ((R02_06-R02_00)/4+1)+R01#                                       
STI#     EQU   ((R02_07-R02_00)/4+1)+R01#                                       
RTI#     EQU   ((R02_08-R02_00)/4+1)+R01#                                       
VDC#     EQU   ((R02_09-R02_00)/4+1)+R01#                                       
CMB#     EQU   ((R02_10-R02_00)/4+1)+R01#                                       
VBI#     EQU   ((R02_11-R02_00)/4+1)+R01#                                       
FTE#     EQU   ((R02_12-R02_00)/4+1)+R01#                                       
LMK#     EQU   ((R02_13-R02_00)/4+1)+R01#                                       
AMK#     EQU   ((R02_14-R02_00)/4+1)+R01#                                       
SCR#     EQU   ((R02_15-R02_00)/4+1)+R01#                                       
SMB#     EQU   ((R02_16-R02_00)/4+1)+R01#                                       
VMTH#    EQU   ((R02_17-R02_00)/4+1)+R01#                                       
                                                                                
R02_00   DS    0H                                                               
R02_01   B     PWCPW                                                            
R02_02   B     ROUNDUP                                                          
R02_03   B     CHKALFNM                                                         
R02_04   B     FCWLKDOL                                                         
R02_05   B     FMTLKPW                                                          
R02_06   B     BGTXTINF                                                         
R02_07   B     SAVETIA                                                          
R02_08   B     RSTRTIA                                                          
R02_09   B     VDCMNTH                                                          
R02_10   B     CMTHBIT                                                          
R02_11   B     VBILTYP                                                          
R02_12   B     FLTRTST                                                          
R02_13   B     LOCMKT                                                           
R02_14   B     ADDMKT                                                           
R02_15   B     SETSCRN                                                          
R02_16   B     STMNTBIL                                                         
R02_17   B     VMNTH                                                            
R02#     EQU   ((*-R02_00)/4)+R01#                                              
DIE_02   DC    H'0'                                                             
                                                                                
YES_02   SR    RC,RC                                                            
NO_02    LTR   RC,RC                                                            
XIT_02   XIT1                                                                   
         TITLE 'SPSFM42 - PW LIST (SUBR02--PWPW#)'                              
*--------------------------- CALCULATE PW% ---------------------------*         
                                                                                
* At entry, TEMPAJB, TEMPACB, TEMPACG, TEMPTAX, & TEMPPW are set.               
* At exit, TEMPPW has the calculated PW%.                                       
                                                                                
PWCPW    DS    0H                                                               
         MVC   TEMPPW,OPWPCT       SET DEFAULT VALUE                            
         OC    TEMPAJB,TEMPAJB     IF NO ADJUSTED BUYS,                         
         BZ    PWCPWX               THEN EXIT                                   
                                                                                
         L     R2,AIO3             USE 3RD I/O FOR PWBLOCK                      
         USING PWBLKD,R2                                                        
         XC    0(PWBLKL,R2),0(R2)                                               
         MVI   PWACT,PWGETPW                                                    
         MVC   PWACTBUY,TEMPACB                                                 
         MVC   PWADJBUY,TEMPAJB                                                 
         MVC   PWTAX,TEMPTAX                                                    
                                                                                
         GOTO1 APWCALC,DMCB,(R2)                                                
         CLI   PWERR,0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEMPPW,PWVAL                                                     
         DROP  R2                                                               
*                                                                               
PWCPWX   DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--RUP#)'                               
*------------------------------ ROUNDING -----------------------------*         
                                                                                
*  Rounds off to the nearest 100s                                               
*   At entry, R0 contains the number to be rounded                              
*   At exit,  FULL = the result                                                 
                                                                                
ROUNDUP  DS    0H                                                               
         SRDA  R0,31               DOUBLE AND STORE IN R0R1                     
         D     R0,=F'100'          N/50                                         
         LTR   R1,R1                                                            
         BM    *+8                                                              
         AHI   R1,1                (N/50)+1                                     
         SRA   R1,1                (N/100)+0.5                                  
         ST    R1,FULL                                                          
         B     XIT_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--CAN#)'                               
*-------------------------- CHECK ALPHANUMERIC -----------------------*         
                                                                                
* Checks TWA fields for valid alphanumeric input.                               
* At entry: R2-->TWA field                                                      
* At exit : CC=EQ if input is valid alphanumeric                                
*           CC=NEQ if it isn't                                                  
                                                                                
CHKALFNM DS    0H                                                               
         ZIC   R0,5(R2)            R0=L(INPUT)                                  
         LA    R3,8(R2)            R3-->INPUT DATA                              
CAN20    LA    RF,ALPHANUM         RF-->TABLE OF VALID ALPHANUMERICS            
CAN20A   CLI   0(RF),C'\'          IF AT END OF THIS TABLE,                     
         BE    CANXN                THEN INPUT NOT ALPHANUMERIC                 
         CLC   0(1,R3),0(RF)       IF MATCH FOUND                               
         BE    CAN20B               VALIDATE NEXT CHAR OF INPUT                 
         LA    RF,1(RF)             ELSE, KEEP LOOKING FOR MATCH                
         B     CAN20A                                                           
CAN20B   LA    R3,1(R3)            R3-->NEXT CHAR OF INPUT                      
         BCT   R0,CAN20                                                         
         B     CANXY                                                            
*                                                                               
CANXY    B     YES_02                                                           
CANXN    B     NO_02                                                            
         TITLE 'SPSFM42 - PW LIST (SUBR02--FCW#)'                               
*---------------------- FORMAT CLT & WIM DOLLARS ---------------------*         
                                                                                
* Formats the locked CLT and WIM $'s onto list line.                            
* At entry,                                                                     
*   R4-->list line.                                                             
*   TEMPAJB & TEMPACB   = the locked CLT and WIM $'s respectively.              
*   DUB+0(2) & DUB+2(2) = lengths of output fields for CLT and WIM $'s.         
*   DUB+4(2) & DUB+6(2) = displacements into list line of output flds.          
                                                                                
FCWLKDOL DS    0H                                                               
         MVI   GOSUBN,RUP#         ROUND UP ROUTINE                             
         L     RF,AGOSUB                                                        
                                                                                
         DS    0H                  LOCKED CLT DOLLARS                           
         L     R0,TEMPAJB                                                       
         GOTO1 AGOSUB                                                           
         L     R1,FULL             R1=ROUNDED DOLLAR AMOUNT                     
         MVC   HALF,DUB            HALF=OUTPUT LENGTH                           
         LH    R2,DUB+4                                                         
         AR    R2,R4               R2-->OUTPUT AREA                             
         BAS   RE,MYEDIT                                                        
                                                                                
         DS    0H                  LOCKED WIM DOLLARS                           
         L     R0,TEMPACB                                                       
         GOTO1 AGOSUB                                                           
         L     R1,FULL             R1=ROUNDED DOLLAR AMOUNT                     
         MVC   HALF,DUB+2          HALF=OUTPUT LENGTH                           
         LH    R2,DUB+6                                                         
         AR    R2,R4               R2-->OUTPUT AREA                             
         BAS   RE,MYEDIT                                                        
                                                                                
         B     XIT_02                                                           
                                                                                
                                                                                
* This routine simulates the following EDIT macro:                              
*--->    EDIT  (R1),(HALF,(R2)),ZERO=NOBLANK                                    
* Be careful not to clobber RE!                                                 
                                                                                
MYEDIT   DS    0H                                                               
         CVD   R1,MYDUB                                                         
*^^NOP   MVC   WORK(17),=X'4040404040402020202020202020202020'                  
*^^NOP   MVI   WORK+15,X'21'                                                    
         MVC   WORK(17),=X'4040404040402020202020202020202120'                  
         ED    WORK(17),MYDUB+2                                                 
         LH    R1,HALF                                                          
         BCTR  R1,0                                                             
         LA    RF,WORK+17                                                       
         SH    RF,HALF                                                          
         EXMVC R1,0(R2),0(RF)                                                   
         BR    RE                                                               
                                                                                
         TITLE 'SPSFM42 - PW LIST (SUBR02--FPL#)'                               
*---------------------- FORMAT PW% ONTO LISTLINE ---------------------*         
                                                                                
* Calculates and puts PW% onto list line.                                       
* At entry,                                                                     
*   R4-->list line.                                                             
*   TEMPACB, TEMPAJB, & TEMPTAX are set.                                        
                                                                                
FMTLKPW  DS    0H                                                               
         USING LISTD,R4                                                         
                                                                                
         MVC   LSTPW,SPACES                                                     
                                                                                
         MVC   LSTPW,=C'**N/A*'    ASSUME PW% NOT APPLICABLE                    
         OC    TEMPACB,TEMPACB     IS LOCKED ACTUAL COST ZERO?                  
         BZ    FPL30                YES, GOOD ASSUMPTION                        
                                                                                
         DS    0H                  LOCKED ACTUAL COST NON-ZERO                  
         MVC   LSTPW,=C'*RNGE*'    ASSUME LOCKED PW% OUT-OF-RANGE               
         OC    TEMPAJB,TEMPAJB     IS LOCKED ADJUSTED COST ZERO?                
         BZ    FPL30                YES, GOOD ASSUMPTION                        
                                                                                
         MVI   GOSUBN,PWPW#         NO, CALCULATE THE LOCKED PW%                
         GOTO1 AGOSUB                                                           
         L     R1,TEMPPW                                                        
         C     R1,=A(MINPWQ)                                                    
         BL    FPL30                                                            
         CH    R1,=Y(MAXPWQ)                                                    
         BH    FPL30                                                            
                                                                                
         MVC   LSTPW,SPACES                                                     
         MVI   LSTPW,C'0'          ASSUME LOCKED PW% IS ZERO                    
         LTR   R1,R1                                                            
         BZ    FPL30                                                            
         EDIT  (R1),(6,LSTPW),2,FLOAT=-                                         
         B     FPL30                                                            
*                                                                               
FPL30    DS    0H                                                               
*                                                                               
FPLX     DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--BGI#)'                               
*-------------------- BUILD GETTEXT INF MSG BLOCK --------------------*         
                                                                                
* At entry, MSGNUM2 = msg # of info message to get.                             
                                                                                
BGTXTINF DS    0H                                                               
         OI    GENSTAT2,USMYOK+USGETTXT                                         
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVI   GTMAXL,L'CONHEAD     MAX L(OUTPUT AREA)                          
         MVI   GTMSYS,2             SPOT SYSTEM                                 
         MVI   GTMTYP,GTMINF        INFO MESSAGE                                
         MVC   GTMSGNO,MSGNUM2      MSG #                                       
         LA    RF,CONHEADH                                                      
         STCM  RF,7,GTAOUT          A(OUTPUT AREA)                              
         DROP  R1                                                               
                                                                                
         B     XIT_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--STI# && RTI#)'                       
*--------------------------- SAVE TIA TABLES -------------------------*         
                                                                                
* save TIA tables into TEMPSTR                                                  
                                                                                
SAVETIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ                                                     
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         GOTO1 DATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,ATIA,0,0                     
         B     XIT_02                                                           
         SPACE 2                                                                
*-------------------------- RESTORE TIA TABLES -----------------------*         
                                                                                
* restore TIA tables from TEMPSTR                                               
                                                                                
RSTRTIA  DS    0H                                                               
         MVI   DMCB+8,PAGEQ        3RD PARAMETER                                
         MVI   DMCB+9,0                                                         
         MVC   DMCB+10(2),TWATRM                                                
         MVC   DMCB+20(2),=C'L='   6TH PARAMETER                                
         MVC   DMCB+22(2),=H'18432'                                             
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'TEMPSTR',,ATIA,0                      
         B     XIT_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--VDC#)'                               
*-------------------- VALIDATE DR/CR MONTH FILTERS -------------------*         
                                                                                
* Validates inputs in filter field of the forms:                                
*       DC=mar-may,jul   and   NDC=mar-may,jul                                  
* At entry, R2-->Input in SCANNER BLOCK                                         
*           R3-->Filter entry                                                   
* At exit, CC set to equal if valid input, else CC set to not equal             
* COUNTER is used by caller, do not use it here!                                
                                                                                
VDCMNTH  DS    0H                                                               
         MVC   BYTE,1(R2)          BYTE = L(INPUT) TO VALIDATE                  
         MVI   MYERRCD,IMTHQ       ASSUME INVALID MONTH EXPRESSION              
         XC    PERVALB,PERVALB                                                  
         LA    R4,PERVALB                                                       
         USING PERVALD,R4                                                       
         MVC   PVALBSTA,ESDATEB    FUDGE EST STRT AS TODAY'S DATE               
                                                                                
         GOTO1 PERVAL,DMCB,(BYTE,22(R2)),(X'80',(R4))                           
         CLI   4(R1),0                                                          
         BNE   VDCXN                                                            
                                                                                
PVALSEDY EQU   PVALASD+PVALASY+PVALAED+PVALAEY                                  
         TM    PVALASSM,PVALSEDY   ONLY MONTH INPUT ALLOWED                     
         BNO   VDCXN                (PERVAL NEEDS TO ASSUME THE REST)           
                                                                                
         MVI   MYERRCD,MNIEQ       CHECK INPUT MNTHS W/IN EST PERIOD            
         CLC   PVALBSTA(2),EEDATEB IF (INPUT START) > (EST END)                 
         BH    VDCXN                NOT GOOD                                    
         CLC   PVALBEND(2),ESDATEB IF (INPUT END) < (EST START)                 
         BL    VDCXN                NOT GOOD                                    
         B     VDC20                                                            
*                                                                               
** INPUT IS VALID **                                                            
*                                                                               
* Put the month #'s of the months requested in filter field into their          
*  respective slots in WORK+1(12).                                              
VDC20    DS    0H                  INPUT IS VALID                               
         XC    WORK,WORK           WORK+1(12) WILL CONTAIN MNTH #'S             
         ZIC   R1,PVALBSTA+1       R1 = MONTH NUMBER                            
VDC22    LA    R2,WORK(R1)         R2-->MONTH'S SLOT IN WORK                    
         STC   R1,0(R2)             STICK MNTH# IN ITS SLOT                     
         CLM   R1,1,PVALBEND+1     ARE WE DONE?                                 
         BE    VDC30                                                            
         CH    R1,=H'12'                                                        
         BL    *+6                                                              
         SR    R1,R1                                                            
         LA    R1,1(R1)             NOPE, BUMP TO NEXT MONTH #                  
         B     VDC22                                                            
         DROP  R4                                                               
*                                                                               
VDC30    DS    0H                                                               
         MVI   GOSUBN,CMB#         CONVERT MONTH #'S INTO BITS                  
         GOTO1 AGOSUB              HALF = THE CONVERTTED BIT PATTERN            
                                                                                
         USING FLTDSECT,R3                                                      
         ZICM  R4,FLTODSPL,(3)                                                  
         A     R4,ASYSD            R4-->OUTPUT AREA                             
         ZIC   RF,0(R4)            RF=# OF ENTRIES SO FAR                       
         LA    R1,1(RF)             INCREMENT IT                                
         STC   R1,0(R4)             AND UPDATE IT                               
         ZIC   RE,FLTOLEN          RE=L(OUTPUT ENTRIES)                         
         LR    R1,RE                SAVE IN R1 FOR EXMVC                        
         MR    RE,RE                                                            
         LA    R4,1(RF,R4)         R4-->SLOT TO PUT NEXT ENTRY                  
         BCTR  R1,0                                                             
         EXMVC R1,0(R4),HALF                                                    
         DROP  R3                                                               
                                                                                
                                                                                
VDCXY    DS    0H                                                               
         B     YES_02                                                           
*                                                                               
VDCXN    DS    0H                                                               
         MVC   4(1,R2),8(R2)       FUDGE IT TO HAVE DSPL TO 2ND FIELD           
         B     NO_02                                                            
         TITLE 'SPSFM42 - PW LIST (SUBR02--CMB#)'                               
*----------------------- CONVERT MONTHS TO BITS ----------------------*         
                                                                                
* Translates a table of month #'s to bits.  The bit pattern is out-             
*  putted into a 2-byte area.  Each of the 12 left-most bit positions           
*  in this 2-byte area represents a month, with the left-most bit               
*  representing January, and the 4th bit (from the left) of the 2nd             
*  byte representing December.  Only the month numbers appearing in             
*  the (input) table will have their bits turned on.  The 4 right-              
*  most bits in the 2-byte area are not used.                                   
* At entry,                                                                     
*   WORK+1(12) = table of month numbers                                         
* At exit,                                                                      
*   HALF       = bit-wise representation of the table of month #'s              
                                                                                
CMTHBIT  DS    0H                                                               
         XC    HALF,HALF                                                        
         SR    RF,RF               DO THE BIT SHIFTING IN RF                    
         SR    R1,R1                                                            
                                                                                
CMB10    DS    0H                                                               
         LA    R1,1(R1)            INCREMENT TO NEXT MONTH NUMBER               
         SLL   RF,1                SHIFT TO NEXT MONTH SLOT                     
         LA    R2,WORK(R1)         R2-->CORRESPONDING MONTH SLOT                
         CLM   R1,1,0(R2)          IF THE SLOT CONTAINS ITS MONTH #             
         BNE   *+8                                                              
         LA    RF,1(RF)             TURN ON BIT FOR IT                          
         CH    R1,=H'12'           12 MONTHS IN A YEAR                          
         BL    CMB10                                                            
                                                                                
         SLL   RF,4                THE 4 RIGHT-MOST BITS ARE NOT USED           
         STH   RF,HALF                                                          
                                                                                
         B     XIT_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--VBI#)'                               
*---------------- VALIDATE BILL TYPE FILTER EXPRESSION ---------------*         
                                                                                
* Validates inputs in filter field of the form:                                 
*       BLLD=n,    BLLD=*,   or   BLLD=**                                       
* At entry, R2-->Input in SCANNER BLOCK                                         
*           R3-->Filter entry                                                   
* At exit, CC set to equal if valid input, else CC set to not equal             
* COUNTER is used by caller, do not use it here!                                
                                                                                
VBILTYP  DS    0H                                                               
         MVI   MYERRCD,IOPDQ       ASSUME INVALID OPTION DATA VALUE             
         CLI   1(R2),2             MAX LENGTH OF INPUT IS 2                     
         BH    VBIXN                                                            
         BE    VBI15                                                            
                                                                                
         DS    0H                  L(INPUT) = 1 (COULDN'T HAVE BEEN 0)          
         MVI   BYTE,XFF            VALID INPUTS AT THIS LENGTH ARE:             
         CLI   22(R2),C'N'          N                                           
         BE    VBI20                                                            
         MVI   BYTE,1                                                           
         CLI   22(R2),C'*'          *                                           
         BE    VBI20                                                            
         B     VBIXN                                                            
                                                                                
VBI15    DS    0H                  L(INPUT) = 2                                 
         MVI   BYTE,2                                                           
         CLC   22(2,R2),=C'**'     ONLY VALID INPUT IS  **                      
         BE    VBI20                                                            
         B     VBIXN                                                            
*                                                                               
VBI20    DS    0H                  MOVE STUFF TO OUTPUT AREA                    
         USING FLTDSECT,R3                                                      
         ZICM  R4,FLTODSPL,(3)                                                  
         A     R4,ASYSD            R4-->OUTPUT AREA                             
         ZIC   RF,0(R4)            RF=# OF ENTRIES SO FAR                       
         LA    R1,1(RF)             INCREMENT IT                                
         STC   R1,0(R4)             AND UPDATE IT                               
         ZIC   RE,FLTOLEN          RE=L(OUTPUT ENTRIES)                         
         LR    R1,RE                SAVE IN R1 FOR EXMVC                        
         MR    RE,RE                                                            
         LA    R4,1(RF,R4)         R4-->SLOT TO PUT NEXT ENTRY                  
         BCTR  R1,0                                                             
         EXMVC R1,0(R4),BYTE                                                    
         DROP  R3                                                               
                                                                                
                                                                                
VBIXY    DS    0H                                                               
         B     YES_02                                                           
*                                                                               
VBIXN    DS    0H                                                               
         MVC   4(1,R2),8(R2)       FUDGE IT TO HAVE DSPL TO 2ND FIELD           
         B     NO_02                                                            
         TITLE 'SPSFM42 - PW LIST (SUBR02--FTE#)'                               
*---------------------------- FILTER TEST ----------------------------*         
                                                                                
FLTRTST  DS    0H                                                               
         L     R2,AFLTABS                                                       
         USING FLTDSECT,R2                                                      
*                                                                               
FTE10    DS    0H                  RUN THROUGH ALL FILTER ENTRIES               
         CLI   0(R2),EOT                                                        
         BE    FTEXY               ALL DONE!                                    
                                                                                
         ZICM  R3,FLTODSPL,(3)                                                  
         A     R3,ASYSD            R3--># OF FILTER OUTPUT VALUES               
         CLI   0(R3),0             ANY TO TEST?                                 
         BE    FTE50                NO, BUMP TO NEXT FILTER ENTRY               
                                                                                
         MVC   NTIMES,0(R3)         YES, HOLD # OF FILTER VALUES                
         MVI   COUNTER,0                                                        
         LA    R3,1(R3)            R3-->PLACES HOLDING VALUES                   
*                                                                               
FTE20    DS    0H                                                               
         ZIC   R1,COUNTER          UPDATE NTH LOOP-AROUND                       
         LA    R1,1(R1)                                                         
         STC   R1,COUNTER                                                       
                                                                                
         ZICM  R4,FLTTSTBL,(3)                                                  
         LA    R4,FLTDSECT(R4)     R4-->FILTER TEST TABLES                      
         USING FTSTDSCT,R4                                                      
                                                                                
FTE22    DS    0H                                                               
         CLI   0(R4),EOT           ANY MORE FILTER TESTS?                       
         BE    FTE50                NO, WE PASSED, DO NXT FILTER ENTRY          
                                                                                
         ZIC   R1,FTSLCMP          R1=L(COMPARE FOR FILTERING)                  
         BCTR  R1,0                                                             
         ZICM  RE,FTSMDSPL,(3)                                                  
         AR    RE,R3               RE-->WHERE MODEL VALUE STARTS                
         ZICM  RF,FTSSADDR,(3)                                                  
         A     RF,ASYSD                                                         
         ZICM  R0,FTSSDSPL,(3)                                                  
         AR    RF,R0               RF-->WHERE SPECIMEN STARTS                   
                                                                                
         DS    0H                  THIS IS WHERE WE DO THE COMPARINGS           
         CLI   FTSTYPE,FTSTYXCT    COMPARING FOR EXACT MATCH?                   
         BE    FTE25                YEP                                         
         CLI   FTSTYPE,FTSTYLOW    COMPARING FOR LOW LIMIT?                     
         BE    FTE26                YEP                                         
         CLI   FTSTYPE,FTSTYHI     COMPARING FOR HIGH LIMIT?                    
         BE    FTE27                YEP                                         
         CLI   FTSTYPE,FTSTYNCE    DO LOGCL AND & COMPARE FOR EQUAL?            
         BE    FTE28                YEP                                         
         DC    H'0'                                                             
                                                                                
FTE25    DS    0H                  COMPARE FOR EXACT MATCH                      
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)       AUTOMATICALLY PASS IF NO MODEL               
         BZ    FTE30                                                            
         EX    R1,*+8              MODEL PRESENT                                
         B     *+10                                                             
         CLC   0(0,RE),0(RF)                                                    
         BNE   FTE40               FAILED TEST, TRY NEXT MODEL                  
         B     FTE30               PASSED TEST, DO NEXT FILTER TEST             
                                                                                
FTE26    DS    0H                  COMPARE AGAINST LOW  LIMIT                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)                                                    
         BH    FTE40               FAILED TEST, TRY NEXT MODEL                  
         B     FTE30               PASSED TEST, DO NEXT FILTER TEST             
                                                                                
FTE27    DS    0H                  COMPARE AGAINST HIGH LIMIT                   
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),0(RF)                                                    
         BL    FTE40               FAILED TEST, TRY NEXT MODEL                  
         B     FTE30               PASSED TEST, DO NEXT FILTER TEST             
                                                                                
FTE28    DS    0H                  DO LOGICAL AND & CMPARE FOR EQUAL            
         EXMVC R1,WORK,0(RF)        MOVE SPECIMEN INTO WORK AREA                
         EX    R1,*+8                                                           
         B     *+10                                                             
         NC    WORK(0),0(RE)                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RE),WORK                                                     
         BNE   FTE40               FAILED TEST, TRY NEXT MODEL                  
         B     FTE30               PASSED TEST, DO NEXT FILTER TEST             
*                                                                               
FTE30    DS    0H                  BUMP TO NEXT TEST RULE                       
         LA    R4,FTSTDSCQ(R4)                                                  
         B     FTE22                                                            
         DROP  R4                                                               
*                                                                               
FTE40    DS    0H                  FAILED A FLTR TEST, USE NXT SPECIMEN         
         CLC   COUNTER,NTIMES      ANY MORE NEXT SPECIMEN                       
         BE    FTEXN                NO, FAILED FILTER TEST                      
                                                                                
         ZIC   R1,FLTOLEN          BUMP TO NEXT SPECIMEN                        
         AR    R3,R1                                                            
         B     FTE20                AND RUN THIS THRU FILTER TESTS              
*                                                                               
FTE50    DS    0H                  PASSED FILTER ENTRY--BUMP TO NXT ONE         
         ZIC   R1,FLTLEN                                                        
         AR    R2,R1                                                            
         B     FTE10                                                            
         DROP  R2                                                               
                                                                                
                                                                                
FTEXN    DS    0H                  DID NOT PASS FILTER                          
         B     NO_02                                                            
*                                                                               
FTEXY    DS    0H                  PASSED FILTER                                
         B     YES_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--LMK#)'                               
*----------------------------- LOCATE MARKET -------------------------*         
                                                                                
* Locates the ACCUTAB entry for a market by doing a read high                   
*  via BINSRCH.                                                                 
* At entry,                                                                     
*   TEMPBMKT = market to find                                                   
* At exit,                                                                      
*   If entry for market found,                                                  
*     FULL   = address of entry, and CC equal.                                  
*   If entry for market not found,                                              
*     FULL   = address of entry for read-high mkt, and CC not equal.            
                                                                                
LOCMKT   DS    0H                                                               
                                                                                
         XC    FULL,FULL           CLEAR OUTPUT AREA                            
         LA    RE,WORK                                                          
         USING ACCUTABD,RE                                                      
         XC    0(ACCUTABQ,RE),0(RE)                                             
         MVC   ATMRKT,TEMPBMKT                                                  
         DROP  RE                                                               
                                                                                
         LH    R0,ACCUTABN                                                      
         GOTO1 BINSRCH,DMCB,(X'02',WORK),AACCUTAB,(R0),ACCUTABQ,       +        
               (0,L'ATMRKT),ACCUTBMX                                            
                                                                                
         MVC   FULL+1(3),1(R1)     FULL=A(ENTRY FOR MARKET)                     
         CLI   0(R1),X01                                                        
         BE    LMKXN                                                            
         L     RE,FULL                                                          
         USING ACCUTABD,RE                                                      
         CLC   ATMRKT,TEMPBMKT                                                  
         BNE   LMKXN                                                            
         DROP  RE                                                               
         B     LMKXY                                                            
                                                                                
                                                                                
LMKXY    DS    0H                                                               
         B     YES_02                                                           
*                                                                               
LMKXN    DS    0H                                                               
         B     NO_02                                                            
         TITLE 'SPSFM42 - PW LIST (SUBR02--AMK#)'                               
*------------------------------ ADD MARKET ---------------------------*         
                                                                                
* Adds an ACCUTAB entry for a market via BINSRCH.                               
* At entry,                                                                     
*   TEMPBMKT = market to add.                                                   
* At exit,                                                                      
*   FULL     = address of entry for market.                                     
*   ACCUTABN = new # of entries in ACCUTAB.                                     
                                                                                
ADDMKT   DS    0H                                                               
                                                                                
         XC    FULL,FULL           CLEAR OUTPUT AREA                            
         LA    RE,WORK                                                          
         USING ACCUTABD,RE                                                      
         XC    0(ACCUTABQ,RE),0(RE)                                             
         MVC   ATMRKT,TEMPBMKT                                                  
         DROP  RE                                                               
                                                                                
         LH    R0,ACCUTABN                                                      
         GOTO1 BINSRCH,DMCB,(X'01',WORK),AACCUTAB,(R0),ACCUTABQ,       +        
               (0,L'ATMRKT),ACCUTBMX                                            
         OC    0(4,R1),0(R1)       IS TABLE FULL?                               
         BNZ   *+6                                                              
         DC    H'0'                 YEP, DIE                                    
                                                                                
         L     R0,8(R1)            R0=# OF RECORDS SO FAR                       
         STH   R0,ACCUTABN                                                      
         MVC   FULL+1(3),1(R1)     FULL=A(ENTRY FOR MARKET)                     
         B     XIT_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--SCR#)'                               
*------------------------------ SET SCREEN ---------------------------*         
                                                                                
* Set screen labels depending on whether this is a Biller's screen,             
*  or a Buyer's screen.                                                         
                                                                                
SETSCRN  DS    0H                                                               
         XC    PWLCHDR,PWLCHDR                                                  
         XC    PWLTOTL,PWLTOTL                                                  
         XC    PWLPFLN,PWLPFLN                                                  
         MVCDD PWLCHDR(3),SP#SEL                                                
         OI    PWLCHDRH+6,X80      TRANSMIT COLUMNS HEADER LINE                 
         OI    PWLTOTLH+6,X80      TRANSMIT TOTALS LINE                         
         OI    PWLPFLNH+6,X80      TRANSMIT PFLINE                              
                                                                                
         CLI   WHOLIST,C'B'        BLIST                                        
         BE    SCRB10                                                           
         CLI   WHOLIST,C'M'        BUYER LIST                                   
         BE    SCRM10                                                           
         DC    H'0'                                                             
                                                                                
SCRB10   DS    0H                                                               
         LA    R2,PWLCHDR+5                                                     
         USING BLISTD,R2                                                        
         PRINT GEN                                                              
         MVCDD BLSTMKT,SP#MKNUM    MKT#                                         
         MVI   BLSTMKT+L'BLSTMKT,C'/'                                           
         MVCDD BLSTMKNA,SP#MKNAM   MARKET NAME                                  
         MVCDD BLSTPLK,SP#PWPLK    PWLK                                         
         MVCDD BLSTBILL,SP#BLLD    BLLD                                         
         MVCDD BLSTDRCR+1,SP#PWADC ADJUSTED DR/CR                               
         PRINT NOGEN                                                            
         DROP  R2                                                               
                                                                                
         MVCDD PWLPFLN,SP#PWLPB                                                 
         B     SCRX                                                             
*                                                                               
SCRM10   DS    0H                                                               
         LA    R2,PWLCHDR+5                                                     
         USING LISTD,R2                                                         
         PRINT GEN                                                              
         MVCDD LSTMKT,SP#MKNUM     MKT#                                         
         MVI   LSTMKT+L'LSTMKT,C'/'                                             
         MVCDD LSTMKNAM,SP#MKNAM   MARKET NAME                                  
         MVI   LSTBYS,C'B'                                                      
         MVI   LSTGLS,C'G'                                                      
         MVCDD LSTCLK,SP#PWXCL     CL$LK                                        
         MVCDD LSTSLK,SP#PWSLK     STALK                                        
         MVCDD LSTBLK,SP#PWBLK     BUYLK                                        
         MVCDD LSTPLK,SP#PWPLK     PWLK                                         
         MVCDD LSTCLOCK,SP#PWCLD   CLLOCK                                       
         MVCDD LSTWLOCK,SP#PWWLD   WIMLOCK                                      
         MVCDD LSTPW,SP#PWLPW      LKPW%                                        
         PRINT NOGEN                                                            
         DROP  R2                                                               
                                                                                
         MVCDD PWLPFLN,SP#PWLPM                                                 
         B     SCRX                                                             
                                                                                
                                                                                
SCRX     DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--SMB#)'                               
*---------------------------- SET MAINT/BILL -------------------------*         
                                                                                
* Sets fields accordingly for maint LIST or biller LIST screen.                 
                                                                                
STMNTBIL DS    0H                                                               
         MVC   PVACTEQU,ACTEQU                                                  
                                                                                
         CLI   ACTEQU,ACTLIST                                                   
         BE    SMBM10                                                           
         CLI   ACTEQU,21                                                        
         BE    SMBB10                                                           
         DC    H'0'                                                             
*                                                                               
SMBM10   DS    0H                  LIST SCREEN FOR MAINT (BUYER)                
         MVI   WHOLIST,C'M'                                                     
         MVC   APFTABS,AMPFTAB                                                  
         MVC   AFLTABS,AMFLTAB                                                  
         MVC   AOPTABS,AMOPTAB                                                  
         B     SMBX                                                             
*                                                                               
SMBB10   DS    0H                  LIST SCREEN FOR BILLER                       
         MVI   WHOLIST,C'B'                                                     
         MVC   APFTABS,ABPFTAB                                                  
         MVC   AFLTABS,ABFLTAB                                                  
         MVC   AOPTABS,ABOPTAB                                                  
         B     SMBX                                                             
                                                                                
                                                                                
SMBX     DS    0H                                                               
         B     XIT_02                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR02--VMTH#)'                              
*--------------------------- VALIDATE MONTH --------------------------*         
                                                                                
* Validates a month inputted.                                                   
* At entry,                                                                     
*   WORK  = dummy TWA header and field containing month inputted                
*   DATE3 = binary date to pass as today's date.                                
* At exit,                                                                      
*   DATE6 = EBCDIC date of 1st day in month inputted.                           
*   DATE3 = binary date of 1st day in month inputted.                           
*   DATE2 = cmprss date of 1st day in month inputted.                           
                                                                                
VMNTH    DS    0H                                                               
         MVC   BYTE,WORK+5                                                      
         MVI   MYERRCD,IMTHQ       ASSUME INVALID MONTH EXPRESSION              
         XC    PERVALB,PERVALB                                                  
         LA    R4,PERVALB                                                       
         USING PERVALD,R4                                                       
         MVC   PVALBSTA,DATE3      PUT TODAY'S DATE IN                          
                                                                                
         GOTO1 PERVAL,DMCB,(BYTE,WORK+8),(X'80',(R4))                           
         CLI   4(R1),0                                                          
         BNE   VMTHXN                                                           
                                                                                
PVALSED2 EQU   PVALASD+PVALASY+PVALAED+PVALAEY                                  
         TM    PVALASSM,PVALSED2                                                
         BNO   VMTHXN                                                           
                                                                                
         MVC   DATE6,PVALESTA                                                   
         MVC   DATE3,PVALBSTA                                                   
         MVC   DATE2,PVALCSTA                                                   
         DROP  R4                                                               
         B     VMTHXY                                                           
                                                                                
                                                                                
VMTHXN   DS    0H                                                               
         B     NO_02                                                            
*                                                                               
VMTHXY   DS    0H                                                               
         B     YES_02                                                           
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (SUBR02--LTORG && CONSTANT+        
                S)'                                                             
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
ALPHANUM DC    C'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\'                         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (SUBR02--MISC STUFF)'              
*--------------------- SUBR02 MISCELLANEOUS STUFF --------------------*         
                                                                                
*                                                                               
SUBR02L  EQU   *-SUBR02                                                         
         DS    0CL(X'1000'-SUBR02L+1)                                           
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (SUBR03)'                          
***********************************************************************         
*======================= SUBROUTINE POOL THREE =======================*         
SUBR03Q  EQU   ((((*-T21742)/4096)+1)*4096)                                     
                                                                                
         ORG   T21742+SUBR03Q                                                   
SUBR03   NMOD1 0,**4203**                                                       
         SR    RC,RC                                                            
         ICM   RC,7,1(R1)                                                       
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
                                                                                
         L     R1,0(R1)                                                         
         SRL   R1,24               SHIFT TO LOW-ORDER BYTE                      
         SH    R1,=Y(R02#)          SUBTRACT FOR SUB-RTN # 3                    
         BCTR  R1,0                 SUBTRACT ONE,                               
         SLL   R1,2                 AND MULTIPLY BY FOUR                        
         B     R03_00(R1)                                                       
                                                                                
GMB#     EQU   ((R03_01-R03_00)/4+1)+R02#                                       
GMG#     EQU   ((R03_02-R03_00)/4+1)+R02#                                       
GSB#     EQU   ((R03_03-R03_00)/4+1)+R02#                                       
GBI#     EQU   ((R03_04-R03_00)/4+1)+R02#                                       
GBT#     EQU   ((R03_05-R03_00)/4+1)+R02#                                       
BPK#     EQU   ((R03_06-R03_00)/4+1)+R02#                                       
BBK#     EQU   ((R03_07-R03_00)/4+1)+R02#                                       
BGK#     EQU   ((R03_08-R03_00)/4+1)+R02#                                       
BSK#     EQU   ((R03_09-R03_00)/4+1)+R02#                                       
BBLK#    EQU   ((R03_10-R03_00)/4+1)+R02#                                       
GPR#     EQU   ((R03_11-R03_00)/4+1)+R02#                                       
GMP#     EQU   ((R03_12-R03_00)/4+1)+R02#                                       
                                                                                
R03_00   DS    0H                                                               
R03_01   B     GTMKTBUY                                                         
R03_02   B     GTMKTGOL                                                         
R03_03   B     GTSTABUK                                                         
R03_04   B     GTBILL                                                           
R03_05   B     GETBMTAB                                                         
R03_06   B     BPWKEY                                                           
R03_07   B     BBUYKEY                                                          
R03_08   B     BGOALKEY                                                         
R03_09   B     BSTABKEY                                                         
R03_10   B     BBILLKEY                                                         
R03_11   B     GETPWREC                                                         
R03_12   B     GTMKTPW                                                          
R03#     EQU   ((*-R03_00)/4)+R02#                                              
DIE_03   DC    H'0'                                                             
                                                                                
YES_03   SR    RC,RC                                                            
NO_03    LTR   RC,RC                                                            
XIT_03   XIT1                                                                   
         TITLE 'SPSFM42 - PW LIST (SUBR03--GMB#)'                               
*------------------------ GET MARKETS WITH BUYS ----------------------*         
                                                                                
* Reads the file to get markets under the specified M/C/P/E that have           
*  buys in them, and sorts them into ACCUTAB.                                   
                                                                                
GTMKTBUY DS    0H                                                               
         XC    TEMPBMKT,TEMPBMKT                                                
         MVI   GOSUBN,BBK#                                                      
         GOTO1 AGOSUB              BUILD KEY OF BUY RECORD                      
                                                                                
GMB10    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     GMB15                                                            
                                                                                
GMB12    DS    0H                                                               
         GOTO1 SEQ                                                              
         B     GMB15                                                            
                                                                                
*                                                                               
GMB15    DS    0H                                                               
         CLC   KEY(BKYPRDL),KEYSAVE   SAME AM/CLT/PRD?                          
         BNE   GMBX                                                             
                                                                                
         LA    R6,KEY                                                           
         USING BUYRECD,R6                                                       
                                                                                
         CLC   BUYKEST,BEST        DID WE MATCH ON ESTIMATES?                   
         BL    GMB20                NOPE, TOO LOW                               
         BE    GMB30                YEP                                         
         BH    GMB40                NOPE, TOO HIGH                              
         DC    H'0'                SOMETHING'S GOTTA BE WRONG!                  
*                                                                               
GMB20    DS    0H                  BUYKEST < BEST                               
         MVC   BUYKEST,BEST        JUMP TO ESTIMATE ASKED FOR                   
         XC    BUYKBUY,BUYKBUY      AND ERASE BUY DETAILS                       
         B     GMB10                                                            
*                                                                               
GMB30    DS    0H                  BUYKEST = BEST                               
         IC    R0,DMINBTS                                                       
         OI    DMINBTS,X08         PASS BACK DELETED AS WELL                    
         GOTO1 GETREC              CHECK IF BUY DELETED                         
         STC   R0,DMINBTS                                                       
         L     RF,AIO                                                           
         TM    (BUYRCNTL-BUYRECD)(RF),X80                                       
*&&DO                                                                           
         BO    GMB37                                                            
*&&                                                                             
         BO    GMB12                IF YES, READ SEQ FOR NEXT BUY LINE          
         CLC   BUYMSTA(2),BUYMSTA-BUYRECD(RF)  IF SPILL MARKET,                 
         BNE   GMB12                            SKIP THIS BUY RECORD            
                                                                                
         DS    0H                  ANY BUY ELEMENT W/ CORRECT ALLOCATN?         
         L     R3,AIO                                                           
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         MVC   DATADISP,=Y(PWEL-PWRECD)                                         
         BAS   RE,GETEL3                                                        
         B     *+8                                                              
GMB32C   BAS   RE,NEXTEL3                                                       
         BNE   GMB12                                                            
         USING REGELEM,R3                                                       
         CLC   RPPRD,BPRD                                                       
         BNE   GMB32C                                                           
                                                                                
         CLI   WHOLIST,C'B'        IF BLIST SCREEN,                             
         BNE   GMB32G                                                           
         TM    RSTATUS,X'40'        MAKE SURE SPOT IS NOT MINUSED               
         BNZ   GMB32C                                                           
         BAS   RE,GMBGTRDT          (GET "REPRESENTATIVE" DATE)                 
         CLC   RUNDATE,BILLYMSE+0    MAKE SURE SPOT IS IN                       
         BL    GMB32C                                                           
         CLC   RUNDATE,BILLYMSE+2     THE MONTH THAT'S SPECIFIED                
         BH    GMB32C                                                           
GMB32G   EQU   *                                                                
         DROP  R3                                                               
*                                                                               
         MVC   TEMPBMKT,BUYMSTA    LOCATE MARKET IN ACCUTAB                     
         MVI   GOSUBN,LMK#                                                      
         GOTO1 AGOSUB                                                           
         BE    GMB35                                                            
                                                                                
         DS    0H                  NEED TO ADD MARKET INTO ACCUTAB              
         MVI   GOSUBN,AMK#                                                      
         GOTO1 AGOSUB                                                           
         B     GMB35                                                            
                                                                                
GMB35    DS    0H                  FULL=A(ACCUTAB ENTRY FOR MARKET)             
         L     R2,FULL                                                          
         USING ACCUTABD,R2                                                      
         OI    ATFLAG1,ATF1FBYS    FLAG THAT MKT HAS BUYS                       
         DROP  R2                                                               
                                                                                
GMB37    DS    0H                  FIX KEY TO GET NEXT MARKET                   
         MVC   BUYMSTA+2(3),=3X'FF'                                             
         MVI   BUYKEST,XFF                                                      
         MVC   BUYKBUY,=3X'FF'                                                  
         B     GMB10                                                            
*                                                                               
GMB40    DS    0H                  BUYKEST > BEST                               
         MVI   BUYKEST,XFF         JUMP TO NEXT STATION                         
         MVC   BUYKBUY,=3X'FF'                                                  
         B     GMB10                                                            
         DROP  R6                                                               
                                                                                
                                                                                
GMBX     DS    0H                                                               
         B     XIT_03                                                           
                                                                                
                                                                                
* Routine to set "representative" date for a spot                               
* At entry,                                                                     
*   R3-->buy element for spot                                                   
*   AIO = A(buy record)                                                         
* At exit,                                                                      
*   RUNDATE = compressed "representative" date for spot                         
                                                                                
GMBGTRDT NTR1                                                                   
         USING REGELEM,R3                                                       
*                                                                               
         L     R6,AIO                                                           
         USING BUYRECD,R6                                                       
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   RUNDATE,RDATE       INITIALIZE TO BUY DATE                       
*                                                                               
         OC    RPAY,RPAY           IF SPOT IS PAID,                             
         BNZ   GMBGTRDX             USE BUY DATE                                
*                                                                               
         ZIC   R1,RLEN                                                          
         AR    R1,R3                                                            
         CLI   0(R1),X'10'                  ANY AFFIDAVIT ELEMENT?              
         BNE   *+14                                                             
         MVC   RUNDATE,(ADATE-AFFELEM)(R1)   SET TO AFFID DATE IF EXIST         
         B     GMBGTRDX                                                         
*                                                                               
         DS    0H                  SET TO LAST DAY OF ROTATION                  
         SR    R0,R0                                                            
         ZIC   RE,BDSEDAY                                                       
         SRDL  RE,4                                                             
         SRL   RF,28                                                            
         CR    RE,RF                                                            
         BNH   *+12                                                             
         LA    RF,7(RF)                                                         
         SR    RF,RE                                                            
         LR    R0,RF                                                            
                                                                                
         GOTO1 DATCON,DMCB,(2,RUNDATE),(0,STARTEND)                             
         GOTO1 ADDAY,DMCB,STARTEND,STARTEND+6,(R0)                              
         GOTO1 DATCON,DMCB,(0,STARTEND+6),(2,RUNDATE)                           
         B     GMBGTRDX                                                         
                                                                                
*                                                                               
GMBGTRDX DS    0H                                                               
         B     XIT_03                                                           
         DROP  R3,R6                                                            
         TITLE 'SPSFM42 - PW LIST (SUBR03--GMG#)'                               
*----------------------- GET MARKETS WITH GOALS ----------------------*         
                                                                                
* Reads the file to get markets under the specified AM/C/P/E that have          
*  goals in them, and sorts them into ACCUTAB.                                  
                                                                                
GTMKTGOL DS    0H                                                               
         OI    MISCFLG1,MF1NOGOL   ASSUME NO GOALS AT ALL                       
         XC    TEMPBMKT,TEMPBMKT                                                
         MVI   GOSUBN,BGK#                                                      
         GOTO1 AGOSUB              BUILD KEY OF GOAL RECORD                     
                                                                                
GMG10    DS    0H                                                               
         GOTO1 HIGH                                                             
         B     GMG10X                                                           
GMG10C   DS    0H                                                               
         GOTO1 SEQ                                                              
GMG10X   EQU   *                                                                
*                                                                               
         CLC   KEY(GKYPRDL),KEYSAVE   SAME AM/CLT/PRD?                          
         BNE   GMBX                    NO, ALL DONE!                            
                                                                                
         LA    R6,KEY                                                           
         USING GOALRECD,R6                                                      
                                                                                
         CLC   GKEYEST,BEST        DID WE MATCH ON ESTIMATES?                   
         BL    GMG20                NOPE, TOO LOW                               
         BE    GMG30                YEP                                         
         BH    GMG40                NOPE, TOO HIGH                              
         DC    H'0'                SOMETHING'S GOTTA BE WRONG!                  
*                                                                               
GMG20    DS    0H                  GKEYEST < BEST                               
         MVC   GKEYEST,BEST        JUMP TO ESTIMATE ASKED FOR                   
         XC    GKEYDPT(5),GKEYDPT   AND ERASE REST OF KEYS                      
         B     GMG10                                                            
*                                                                               
GMG30    DS    0H                  GKEYEST = BEST                               
         GOTO1 GETREC              NEED TO LOOK FOR X'21' ELEMENT TO            
         L     RF,AIO               SEE IF GOALS REALLY EXIST                   
         LA    RF,GDELEM-GOALRECD(RF)                                           
         SR    R0,R0                                                            
                                                                                
GMG32    DS    0H                                                               
         CLI   0(RF),0             AT END OF RECORD?                            
         BE    GMG10C               YEP, NOT A "REAL" GOAL, RD NXT REC          
         CLI   0(RF),X'21'         IS THERE A X'21' ELEM?                       
         BE    GMG34                YEP, GOALS EXIST FOR THIS MARKET            
         IC    R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GMG32                                                            
                                                                                
GMG34    DS    0H                                                               
         MVC   TEMPBMKT,GKEYMKT    LOCATE MARKET IN ACCUTAB                     
         MVI   GOSUBN,LMK#                                                      
         GOTO1 AGOSUB                                                           
         BE    GMG35                                                            
                                                                                
         DS    0H                  NEED TO ADD MARKET INTO ACCUTAB              
         MVI   GOSUBN,AMK#                                                      
         GOTO1 AGOSUB                                                           
         B     GMG35                                                            
                                                                                
GMG35    DS    0H                                                               
         L     R2,FULL             R2-->ENTRY FOR MARKET IN ACCUTAB             
         USING ACCUTABD,R2                                                      
         OI    ATFLAG1,ATF1FGLS    INDICATE GOALS FOR MARKET                    
         DROP  R2                                                               
         NI    MISCFLG1,XFF-MF1NOGOL   AT LEAST ONE GOAL IN ALL MKTS            
                                                                                
         MVC   GKEYEST(6),=6X'FF'  FIX KEY TO GET NEXT MARKET                   
         B     GMG10                                                            
*                                                                               
GMG40    DS    0H                  GKEYEST > BEST                               
         MVC   GKEYEST(6),=6X'FF'  JUMP TO NEXT MARKET                          
         B     GMG10                                                            
         DROP  R6                                                               
                                                                                
                                                                                
GMGX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR03--GSB#)'                               
*------------------------ GET STATION BUCKET -------------------------*         
                                                                                
* Reads STATION BILLING BUCKET records for every market in ACCUTAB              
*  to determine billing status (estimate or final).                             
                                                                                
GTSTABUK DS    0H                                                               
         XC    TEMPBMKT,TEMPBMKT                                                
         MVI   GOSUBN,BSK#                                                      
         GOTO1 AGOSUB              BUILD KEY STATION BILLING RECD               
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(SKYESTL),KEYSAVE  ANYTHING FOR AM/CLT/PRD/EST?               
         BNE   GSBX                   NOPE, EXIT NOW                            
                                                                                
         MVC   TEMPBMKT,KEY+(STABKMKT-STABUCKD)                                 
         MVI   GOSUBN,LMK#         LOCATE FILE'S 1ST MKT IN ACCUTAB             
         GOTO1 AGOSUB              ON RETURN, FULL=A(READ-HIGH ENTRY)           
                                                                                
         ICM   R2,15,FULL                                                       
         BZ    GSBX                                                             
         USING ACCUTABD,R2                                                      
*                                                                               
GSB10    DS    0H                                                               
         OC    ATMRKT,ATMRKT       ANY MORE ENTRIES IN ACCUTAB?                 
         BZ    GSBX                                                             
                                                                                
         MVC   TEMPBMKT,ATMRKT                                                  
         MVI   GOSUBN,BSK#                                                      
         GOTO1 AGOSUB                 BUILD KEY STATION BILLING RECD            
                                                                                
         DS    0H                      AND READ IT                              
         GOTO1 HIGH                                                             
         CLC   KEY(SKYESTL),KEYSAVE   SAME AM/CLT/PRD/EST?                      
         BNE   GSBX                    NOPE, MIGHT AS WELL EXIT NOW!            
         CLC   KEY(SKYMKTL),KEYSAVE   SAME AM/CLT/PRD/EST/MKT?                  
         BNE   GSB20                                                            
                                                                                
         DS    0H                      YES, GO GET RECORD                       
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         LA    R6,(STABELEM-STABUCK)(R6)                                        
         USING STABELEM,R6                                                      
*                                                                               
         SR    R0,R0                                                            
GSB15    DS    0H                                                               
         CLI   0(R6),0             AT EORECORD?                                 
         BE    GSB20                YEP, MKT IS NOT BILLED--GET NXT MKT         
         CLC   STABPER,BILLYRMT    SAME BILLING PERIOD?                         
         BNE   GSB15A                                                           
         OI    ATFLAG1,ATF1ESTB     YES, IT'S AT LEAST ESTIM BILLED             
         B     GSB20                                                            
                                                                                
GSB15A   DS    0H                                                               
         IC    R0,1(R6)             NOPE, TRY NEXT ELEMENT                      
         AR    R6,R0                                                            
         B     GSB15                                                            
         DROP  R6                                                               
*                                                                               
GSB20    DS    0H                  BUMP TO NEXT MARKET IN ACCUTAB               
         LA    R2,ACCUTABQ(R2)                                                  
         B     GSB10                                                            
         DROP  R2                                                               
                                                                                
                                                                                
GSBX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR03--GBI#)'                               
*-------------------------- GET BILL RECORD --------------------------*         
                                                                                
* Reads all the BILL records for the same AM/CLT/PRD/EST/YR/MNTH and            
*  determines if any markets in ACCUTAB are billed (estim & final).             
                                                                                
GTBILL   DS    0H                                                               
         MVI   GOSUBN,BBLK#        GO BUILD KEY OF BILL RECORD                  
         GOTO1 AGOSUB                                                           
         LA    R6,KEY                                                           
         USING BILLRECD,R6                                                      
         OC    BKEYYSRV(2),BKEYYSRV  IF IT LOOKS LIKE KEY OF EST HDR,           
         BZ    GBIX                   NO Y/M SPECIFIED, SO EXIT NOW             
         DROP  R6                                                               
                                                                                
         DS    0H                  READ BILL RECORDS                            
         GOTO1 HIGH                                                             
         B     GBI10                                                            
GBI05    GOTO1 SEQ                                                              
         B     GBI10                                                            
*                                                                               
GBI10    DS    0H                                                               
         LA    R1,BIKYYML-1        SAME AM/CLT/PRD/EST/YR/MNTH?                 
         EX    R1,CLCKYKSV          (CLC   KEY(BIKYYML),KEYSAVE)                
         BNE   GBIX                 NO, EXIT NOW                                
                                                                                
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING BILLRECD,R6                                                      
                                                                                
         DS    0H                  CHECK ON MARKET FIRST                        
         CLC   BLMKT,SPACES                                                     
         BE    GBI05                                                            
         PACK  DUB,BLMKT                                                        
         CVB   R0,DUB                                                           
         STCM  R0,3,TEMPBMKT                                                    
         MVI   GOSUBN,LMK#                                                      
         GOTO1 AGOSUB              LOCATE MKT ENTRY IN ACCUTAB                  
         BNE   GBI05                NOT FOUND, READ NEXT BILL RECORD            
                                                                                
         DS    0H                  FOUND ENTRY FOR MKT IN ACCUTAB               
         L     R2,FULL                                                          
         USING ACCUTABD,R2                                                      
         OI    ATFLAG1,ATF1ESTB    MKT IS ESTIM BILLED?                         
                                                                                
         TM    BILSTAT2,BSTCLRDQ   IS IT ALSO FINAL BILLED?                     
         BZ    *+8                                                              
         OI    ATFLAG1,ATF1FNLB     YES!                                        
         B     GBI05                                                            
         DROP  R2,R6                                                            
                                                                                
                                                                                
GBIX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR03--GBT#)'                               
*------------------------ GET BROADCAST MONTHS -----------------------*         
                                                                                
GETBMTAB DS    0H                                                               
*                                                                               
** BROADCAST WEEKS **                                                           
*                                                                               
         XC    BRDWKTB2,BRDWKTB2     GENERATE WEEKS OF SCHEDULE                 
         MVI   BYTE,BRDWKQ-1       BYTE = MAX # OF WEEKS TO GENERATE            
         GOTO1 AMOBILE,DMCB,(BYTE,ESDATE),(4,BRDWKTB2),MOBINPAD,       +        
               SPOTPROF                                                         
         MVC   NUMWEEKS,DMCB       SAVE # OF DATE-PAIRS GENERATED               
*                                                                               
** BROADCAST MONTHS **                                                          
*                                                                               
         MVI   BYTE,BRDMTHQ-1      BYTE = MAX # OF MNTHS TO GENERATE            
         MVC   ESTOOWSD,SPOTPROF+8     SAVE PROFILE SETTING                     
         MVI   SPOTPROF+8,0            CLEAR IT TO GET B'CST MONTHS             
         GOTO1 AMOBILE,DMCB,(BYTE,ESDATE),(0,BRDMTHTB),MOBINPAD,       +        
               SPOTPROF                                                         
         MVC   SPOTPROF+8(1),ESTOOWSD  RESTORE PROFILE SETTINGS                 
                                                                                
*                                                                               
** BROADCAST MONTH FOR MONTH SPECIFIED (BLIST) **                               
*                                                                               
         DS    0H                                                               
         CLI   WHOLIST,C'B'                                                     
         BNE   GBT039                                                           
                                                                                
*                                                                               
         DS    0H                                                               
         MVC   DATE3(2),BILLYRMT                                                
         MVI   DATE3+2,15          FUDGE IN MIDDLE-OF-THE-MONTH DATE            
                                                                                
         DS    0H                  CONVERT TO TYPE-0 DATE FORMAT                
         GOTO1 DATCON,DMCB,(3,DATE3),(0,DATE6),0                                
                                                                                
         DS    0H                  GET START/END OF BRDCST MONTH                
         GOTO1 AGETBROD,DMCB,(1,DATE6),STARTEND,AGETDAY,AADDAY                  
         CLC   STARTEND+0(6),ESDATE    IF BDCST MTH STRT < ESTM STRT            
         BNL   *+10                                                             
         MVC   STARTEND+0(6),ESDATE     FORCE BDCST MTH STRT = EST STRT         
         CLC   STARTEND+6(6),EEDATE    IF BDCST MTH END > ESTM END              
         BNH   *+10                                                             
         MVC   STARTEND+6(6),EEDATE     FORCE BDCST MTH END = EST END           
                                                                                
         DS    0H                  GET BROADCAST WEEKS IN BRDCST MONTH          
         GOTO1 AMOBILE,DMCB,(5,STARTEND),(4,WORK),MOBINPAD,SPOTPROF             
                                                                                
         DS    0H                  GET START/END BROADCAST WEEK DATES           
         LA    RF,WORK                                                          
         MVC   BILLYMSE+0(2),0(RF)   START DATE OF FIRST WEEK                   
         CLI   4(RF),XFF                                                        
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     *-12                                                             
         MVC   BILLYMSE+2(2),2(RF)   END DATE OF LAST WEEK                      
GBT039   EQU   *                                                                
                                                                                
*                                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR03--BPK#, BBK#, BGK#, BSK#, && B+        
               BLK#)'                                                           
*---------------------------- BUILD PW KEY ---------------------------*         
                                                                                
* At entry, TEMPBMKT contains market we want PW key for                         
                                                                                
BPWKEY   DS    0H                                                               
         XC    KEY,KEY                                                          
PWK      USING PWRECD,KEY                                                       
         MVC   PWK.PWKTYP,=X'0D7A'                                              
         MVC   PWK.PWKAGMD,BAGYMD                                               
         MVC   PWK.PWKCLT,BCLT                                                  
         MVC   PWK.PWKPRD,BPRD                                                  
         MVC   PWK.PWKEST,BEST                                                  
         MVC   PWK.PWKMKT,TEMPBMKT                                              
         DROP  PWK                                                              
         B     XIT_03                                                           
                                                                                
                                                                                
*---------------------- BUILD KEY OF BUY RECORD ----------------------*         
                                                                                
* At entry,                                                                     
*    TEMPBMKT = market to start reading off from                                
                                                                                
BBUYKEY  DS    0H                                                               
         XC    KEY,KEY             SET UP BUY RECORD KEY                        
         LA    RE,KEY                                                           
         USING BUYRECD,RE                                                       
         MVC   BUYKAM,BAGYMD                                                    
         MVC   BUYKCLT,BCLT                                                     
         MVC   BUYKPRD,BPRD                                                     
         MVC   BUYMSTA(L'BMKT),TEMPBMKT                                         
         MVC   BUYKEST,BEST                                                     
         DROP  RE                                                               
                                                                                
         B     XIT_03                                                           
                                                                                
                                                                                
*---------------------- BUILD KEY OF GOAL RECORD ---------------------*         
                                                                                
* At entry,                                                                     
*    TEMPBMKT = market to build GOAL key for                                    
                                                                                
BGOALKEY DS    0H                                                               
         XC    KEY,KEY             CLEAR OUT KEY FIELD                          
GLK      USING GOALRECD,KEY                                                     
         MVI   GLK.GKEYTYPE,X02                                                 
         MVC   GLK.GKEYAM,BAGYMD                                                
         MVC   GLK.GKEYCLT,BCLT                                                 
         MVC   GLK.GKEYPRD,BPRD                                                 
         MVC   GLK.GKEYMKT,TEMPBMKT                                             
         MVC   GLK.GKEYEST,BEST                                                 
         DROP  GLK                                                              
                                                                                
         B     XIT_03                                                           
         EJECT                                                                  
*---------------- BUILD KEY OF STATION BILLING RECORD ----------------*         
                                                                                
* At entry,                                                                     
*    TEMPBMKT = market to build STATION BILLING key for.                        
                                                                                
BSTABKEY DS    0H                                                               
         XC    KEY,KEY             CLEAR OUT KEY FIELD                          
SBK      USING STABUCKD,KEY                                                     
         MVC   SBK.STABKCOD,=X'0E01'                                            
         MVC   SBK.STABKAM,BAGYMD                                               
         MVC   SBK.STABKCLT,BCLT                                                
         MVC   SBK.STABKPRD,BPRD                                                
         MVC   SBK.STABKEST,BEST                                                
         MVC   SBK.STABKMKT,TEMPBMKT                                            
         DROP  SBK                                                              
                                                                                
         B     XIT_03                                                           
                                                                                
                                                                                
*-------------------- BUILD KEY OF BILLING RECORD --------------------*         
                                                                                
* At entry,                                                                     
*    BILLYRMT = year & month of service to build BILLING key for.               
                                                                                
BBILLKEY DS    0H                                                               
         XC    KEY,KEY             CLEAR OUT KEY FIELD                          
BIK      USING BILLRECD,KEY                                                     
         MVI   BIK.BKEYTYPE,0                                                   
         MVC   BIK.BKEYAM,BAGYMD                                                
         MVC   BIK.BKEYCLT,BCLT                                                 
         MVC   BIK.BKEYPRD,QPRD                                                 
         MVC   BIK.BKEYEST,BEST                                                 
         MVC   BIK.BKEYYSRV,BILLYRMT                                            
         MVC   BIK.BKEYMSRV,BILLYRMT+1                                          
         DROP  BIK                                                              
                                                                                
         B     XIT_03                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR03--GPR#)'                               
*---------------------------- GET PW RECORD --------------------------*         
                                                                                
* At entry, TEMPBMKT contains market we want PW record for                      
                                                                                
GETPWREC DS    0H                                                               
         MVI   GOSUBN,BPK#         SET UP PROFIT WITHIN KEY                     
         GOTO1 AGOSUB                                                           
                                                                                
         GOTO1 HIGH                                                             
         CLC   KEY(L'PWFKEY),KEYSAVE                                            
         BE    GPR10                                                            
                                                                                
         DS    0H                  PW RECORD DOESN'T EXIST, MAKE ONE            
         MVC   KEY(L'PWFKEY),KEYSAVE    RESTORE KEY                             
         L     R6,APWREC                                                        
         USING PWRECD,R6                                                        
         LR    RE,R6                    CLEAR PW RECORD AREA                    
         LA    RF,L'PWREC                                                       
         XCEF                                                                   
         DS    0H                       KEY                                     
         MVC   PWFKEY,KEY                                                       
         MVC   PWAGYA,TWAAGY                                                    
         DS    0H                       PWGNEL ELEMENT                          
         MVI   PWGNEL,PWGNELQ                                                   
         MVI   PWGNLEN,PWGNLENQ                                                 
         OI    PWGNFLG,PWGNPLKQ                                                 
         DS    0H                       PWDTIEL ELEMENT                         
         LA    RF,PWGNEL+PWGNLENQ                                               
         USING PWDTIEL,RF                                                       
         MVI   PWDTICD,PWDTICDQ                                                 
         MVI   PWDTILEN,PWDTILNQ                                                
         MVC   PWDTIPLD,CTODAY                                                  
         DROP  RF                                                               
         DS    0H                       EORECORD MARKER                         
         MVI   PWEL+PWGNLENQ+PWDTILNQ,0                                         
         LA    RE,(PWEL-PWFKEY)+PWGNLENQ+PWDTILNQ+1                             
         STCM  RE,3,PWLEN               L(RECORD)                               
         XC    DMDSKADD,DMDSKADD        NO RECORD, CLEAR D/A                    
         B     GPRX                                                             
*                                                                               
GPR10    DS    0H                  PW RECORD DOES EXIST                         
         L     R0,AIO              SAVE ORIGINAL I/O AREA                       
         MVC   AIO,APWREC                                                       
         GOTO1 GETREC              GET PW RECORD INTO PW RECORD AREA            
         ST    R0,AIO              RSTR ORIGINAL I/O AREA                       
         B     GPRX                                                             
*                                                                               
GPRX     DS    0H                                                               
         B     XIT_03                                                           
         DROP  R6                                                               
         TITLE 'SPSFM42 - PW LIST (SUBR03--GMP#)'                               
*------------------------- GET MARKETS WITH PW -----------------------*         
                                                                                
* Reads the file to get markets under the specified M/C/P/E for which           
*  there are PW records and sorts them into ACCUTAB.                            
                                                                                
GTMKTPW  DS    0H                                                               
         XC    TEMPBMKT,TEMPBMKT                                                
         MVI   GOSUBN,BPK#                                                      
         GOTO1 AGOSUB              BUILD KEY OF BUY RECORD                      
                                                                                
GMP10    DS    0H                                                               
         GOTO1 HIGH                                                             
                                                                                
*                                                                               
GMP15    DS    0H                                                               
         CLC   KEY(PKYESTL),KEYSAVE   SAME AM/CLT/PRD/EST?                      
         BNE   GMPX                                                             
                                                                                
         LA    R6,KEY                                                           
         USING PWFKEY,R6                                                        
                                                                                
         MVC   TEMPBMKT,PWKMKT     LOCATE MARKET IN ACCUTAB                     
         MVI   GOSUBN,LMK#                                                      
         GOTO1 AGOSUB                                                           
         BE    GMP35                                                            
                                                                                
         DS    0H                  NEED TO ADD MARKET INTO ACCUTAB              
         MVI   GOSUBN,AMK#                                                      
         GOTO1 AGOSUB                                                           
         B     GMP35                                                            
                                                                                
GMP35    DS    0H                  FULL=A(ACCUTAB ENTRY FOR MARKET)             
                                                                                
*                                                                               
GMP50    DS    0H                  FIX KEY TO GET NEXT MARKET                   
         MVC   PWKSTA(4),=4X'FF'                                                
         B     GMP10                                                            
         DROP  R6                                                               
                                                                                
                                                                                
GMPX     DS    0H                                                               
         B     XIT_03                                                           
         TITLE 'SPSFM42 - PW LIST (SUBR03--???#)'                               
*--------------------------- ????????????? ---------------------------*         
                                                                                
         TITLE 'SPSFM42 - PW LIST (SUBR03--LTORG && CONSTANTS)'                 
*------------------------- LTORG & CONSTANTS -------------------------*         
         LTORG                                                                  
                                                                                
                                                                                
         TITLE 'SPSFM42 - PW LIST (SUBR03--MISC STUFF)'                         
*--------------------- SUBR03 MISCELLANEOUS STUFF --------------------*         
                                                                                
CLCKYKSV CLC   KEY(0),KEYSAVE           FOR EX INSTRUCTION ONLY                 
*                                                                               
GETEL3   DS    0H                  "GETEL3  R3,DATADISP,ELCDLO,ELCDHI"          
         PRINT OFF                                                              
         AH    R3,DATADISP                                                      
         PRINT ON                                                               
FIRSTEL3 DS    0H                                                               
         PRINT OFF                                                              
         CLI   0(R3),0                                                          
         BNE   *+10                                                             
         CLI   0(R3),1                                                          
         BR    RE                  RETURN CC NOT EQUAL                          
         CLI   ELCDLO,0                                                         
         BER   RE                  RETURN CC EQUAL                              
         CLI   ELCDHI,0                                                         
         BER   RE                  RETURN CC EQUAL                              
         CLC   ELCDLO,ELCDHI                                                    
         BHR   RE                  RETURN CC NOT EQUAL                          
         B     NEXTEL3C                                                         
         PRINT ON                                                               
NEXTEL3  DS    0H                                                               
         PRINT OFF                                                              
         CLI   0(R3),0                                                          
         BE    NEXTEL3X                                                         
         ZIC   R0,1(R3)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R3,R0                                                            
NEXTEL3C CLI   0(R3),0                                                          
         BE    NEXTEL3X                                                         
         CLC   ELCDLO,0(R3)                                                     
         BH    NEXTEL3                                                          
         CLC   ELCDHI,0(R3)                                                     
         BL    NEXTEL3                                                          
         CR    RB,RB                                                            
         B     *+6                                                              
NEXTEL3X LTR   RB,RB                                                            
         BR    RE                                                               
         PRINT ON                                                               
                                                                                
*                                                                               
SUBR03L  EQU   *-SUBR03                                                         
         DS    0CL(X'1000'-SUBR03L+1)                                           
***********************************************************************         
         DROP  R8,R9,RA,RB,RC                                                   
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (TABLES)'                          
***********************************************************************         
*======================== MISCELLANEOUS TABLES =======================*         
                                                                                
TTABDSP  DS    0H                  TABLE OF TABLE DISPLACEMENTS                 
         DC    Y(DATDSPTB-T21742)                                               
         DC    Y(LBLTAB-T21742)                                                 
         DC    Y(COREQLST-T21742)                                               
         DC    Y(MPFTABLE-T21742)                                               
         DC    Y(BPFTABLE-T21742)                                               
         DC    Y(MFLTABLE-T21742)                                               
         DC    Y(BFLTABLE-T21742)                                               
         DC    Y(MOPTABLE-T21742)                                               
         DC    Y(BOPTABLE-T21742)                                               
         DC    Y(GOSUB-T21742)                                                  
         DC    Y(SUBR01-T21742)                                                 
         DC    Y(SUBR02-T21742)                                                 
         DC    Y(SUBR03-T21742)                                                 
TTABDSPQ EQU   (*-TTABDSP)/(L'TTABDSP)                                          
                                                                                
                                                                                
DATDSPTB DS    0H                  TABLE OF DATE DISPLACEMENTS                  
         DC    Y(PWDTICLD-PWDTIEL),Y(LSTCLK-LISTD)                              
         DC    Y(PWDTISLD-PWDTIEL),Y(LSTSLK-LISTD)                              
         DC    Y(PWDTIBLD-PWDTIEL),Y(LSTBLK+1-LISTD)                            
         DC    Y(PWDTIPLD-PWDTIEL),Y(LSTPLK+1-LISTD)                            
DTDSPTBQ EQU   (*-DATDSPTB)/(2*L'DATDSPTB)                                      
                                                                                
                                                                                
LBLTAB   DS    0CL(2+2+8)          TABLE OF LABELS FOR SPOT STRG AREA           
         DC    AL2(ACTBLABL-SPOTAREA,ACCUTAB-SPOTAREA),CL8'*ACTB42*'            
         DC    AL2(ESTBLABL-SPOTAREA,ESTAB-SPOTAREA),CL8'**ESTAB*'              
         DC    AL2(PWRCLABL-SPOTAREA,PWREC-SPOTAREA),CL8'*PWRECD*'              
         DC    AL2(SPBKLABL-SPOTAREA,SBLOCK-SPOTAREA),CL8'*SPTBLK*'             
LBLTABQ  EQU   (*-LBLTAB)/(L'LBLTAB)                                            
                                                                                
                                                                                
COREQLST DS    0AL1                LIST OF CORE-RESIDENT PHASE TO GET           
         DC    AL1(QPWCALC)                                                     
         DC    AL1(QSPOTIO)         THE ORDER OF THIS LIST SHOULD               
         DC    AL1(QSPOTBUY)        BE CHANGED WITH CARE                        
         DC    AL1(QSPOTGL)                                                     
         DC    AL1(QMOBILE)                                                     
         DC    AL1(QGETBROD)                                                    
COREQLSQ EQU   *-COREQLST                                                       
                                                                                
                                                                                
YESNO    DS    0C                                                               
         DC    AL1(3),C'YES'                                                    
         DC    AL1(2),C'NO'                                                     
         DC    AL1(EOT)                                                         
                                                                                
                                                                                
YESNOMAN DS    0C                                                               
         DC    AL1(3),C'YES'                                                    
         DC    AL1(2),C'NO'                                                     
         DC    AL1(6),C'MANUAL'                                                 
         DC    AL1(1),C'+'                                                      
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*============================ PFKEY TABLES ===========================*         
                                                                                
*-------------------- PFKEY TABLES FOR MAINT LIST --------------------*         
                                                                                
MPFTABLE DS    0C                  ***** REFER TO PFTABD & KEYD ******          
*                                                                               
PFT02    DS    0C                  PW/Maint                                     
         DC    AL1(PFT02X-PFT02,02)                                             
         DC    AL1(PFTCPROG)                                                    
         DC    AL1((PFT02X-PFT02K)/KEYLNQ)                                      
         DC    AL1(PFTCLRKY)                                                    
PFT02SEL DC    CL3'M  '             SELECT CODE                                 
         DC    CL8'PW      '        RECORD                                      
         DC    CL8'MAINTAIN'        ACTION                                      
PFT02K   DC    AL1(KEYTYTWA,L'PWLMED-1),AL2(PWLMED-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLCLT-1),AL2(PWLCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLPRD-1),AL2(PWLPRD-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLEST-1),AL2(PWLEST-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LSTMKT-1),AL2(LSTMKT-LISTD)                       
PFT02X   EQU   *                                                                
*&&DO                                                                           
*                                                                               
PFT03    DS    0C                  Restore                                      
         DC    AL1(PFT03X-PFT03,03)                                             
         DC    AL1(PFTCPROG)                                                    
         DC    AL1((PFT03X-PFT03K)/KEYLNQ)                                      
         DC    AL1(PFTCLRKY)                                                    
PFT03SEL DC    CL3'R  '             SELECT CODE                                 
         DC    CL8'PW      '        RECORD                                      
         DC    CL8'MAINTAIN'        ACTION                                      
PFT03K   DC    AL1(KEYTYTWA,L'PWLMED-1),AL2(PWLMED-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLCLT-1),AL2(PWLCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLPRD-1),AL2(PWLPRD-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLEST-1),AL2(PWLEST-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LSTMKT-1),AL2(LSTMKT-LISTD)                       
PFT03X   EQU   *                                                                
*&&                                                                             
*                                                                               
PFT05    DS    0C                  Station Lock                                 
         DC    AL1(PFT05X-PFT05,05)                                             
         DC    AL1(PFTCPROG)                                                    
         DC    AL1((PFT05X-PFT05K)/KEYLNQ)                                      
         DC    AL1(PFTCLRKY)                                                    
PFT05SEL DC    CL3'SL '             SELECT CODE                                 
         DC    CL8'PW      '        RECORD                                      
         DC    CL8'MAINTAIN'        ACTION                                      
PFT05K   DC    AL1(KEYTYTWA,L'PWLMED-1),AL2(PWLMED-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLCLT-1),AL2(PWLCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLPRD-1),AL2(PWLPRD-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLEST-1),AL2(PWLEST-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LSTMKT-1),AL2(LSTMKT-LISTD)                       
PFT05X   EQU   *                                                                
*                                                                               
PFT06    DS    0C                  Buy Lock                                     
         DC    AL1(PFT06X-PFT06,06)                                             
         DC    AL1(PFTCPROG)                                                    
         DC    AL1((PFT06X-PFT06K)/KEYLNQ)                                      
         DC    AL1(PFTCLRKY)                                                    
PFT06SEL DC    CL3'BL '             SELECT CODE                                 
         DC    CL8'PW      '        RECORD                                      
         DC    CL8'MAINTAIN'        ACTION                                      
PFT06K   DC    AL1(KEYTYTWA,L'PWLMED-1),AL2(PWLMED-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLCLT-1),AL2(PWLCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLPRD-1),AL2(PWLPRD-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLEST-1),AL2(PWLEST-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LSTMKT-1),AL2(LSTMKT-LISTD)                       
PFT06X   EQU   *                                                                
*                                                                               
PFT07    DS    0C                  Buy Unlock                                   
         DC    AL1(PFT07X-PFT07,07)                                             
         DC    AL1(PFTCPROG)                                                    
         DC    AL1((PFT07X-PFT07K)/KEYLNQ)                                      
         DC    AL1(PFTCLRKY)                                                    
PFT07SEL DC    CL3'BU '             SELECT CODE                                 
         DC    CL8'PW      '        RECORD                                      
         DC    CL8'MAINTAIN'        ACTION                                      
PFT07K   DC    AL1(KEYTYTWA,L'PWLMED-1),AL2(PWLMED-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLCLT-1),AL2(PWLCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLPRD-1),AL2(PWLPRD-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLEST-1),AL2(PWLEST-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LSTMKT-1),AL2(LSTMKT-LISTD)                       
PFT07X   EQU   *                                                                
*                                                                               
PFT08    DS    0C                  PW  Lock                                     
         DC    AL1(PFT08X-PFT08,08)                                             
         DC    AL1(PFTCPROG)                                                    
         DC    AL1((PFT08X-PFT08K)/KEYLNQ)                                      
         DC    AL1(PFTCLRKY)                                                    
PFT08SEL DC    CL3'PL '             SELECT CODE                                 
         DC    CL8'PW      '        RECORD                                      
         DC    CL8'MAINTAIN'        ACTION                                      
PFT08K   DC    AL1(KEYTYTWA,L'PWLMED-1),AL2(PWLMED-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLCLT-1),AL2(PWLCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLPRD-1),AL2(PWLPRD-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLEST-1),AL2(PWLEST-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LSTMKT-1),AL2(LSTMKT-LISTD)                       
PFT08X   EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*--------------------- PFKEY TABLES FOR BILL LIST --------------------*         
                                                                                
BPFTABLE DS    0C                  ***** REFER TO PFTABD & KEYD ******          
*                                                                               
BPFT03   DS    0C                  PW/Bill                                      
         DC    AL1(BPFT03X-BPFT03,03)                                           
         DC    AL1(PFTCPROG)                                                    
         DC    AL1((BPFT03X-BPFT03K)/KEYLNQ)                                    
         DC    AL1(PFTCLRKY)                                                    
BPFT03SL DC    CL3'B  '             SELECT CODE                                 
         DC    CL8'PW      '        RECORD                                      
         DC    CL8'BILL    '        ACTION                                      
BPFT03K  DC    AL1(KEYTYTWA,L'PWLMED-1),AL2(PWLMED-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLCLT-1),AL2(PWLCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLPRD-1),AL2(PWLPRD-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLEST-1),AL2(PWLEST-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LSTMKT-1),AL2(LSTMKT-LISTD)                       
BPFT03X  EQU   *                                                                
*                                                                               
BPFT04   DS    0C                  PW/Bill & force CLT$ Adj DR/CR=0             
         DC    AL1(BPFT04X-BPFT04,04)                                           
         DC    AL1(PFTCPROG)                                                    
         DC    AL1((BPFT04X-BPFT04K)/KEYLNQ)                                    
         DC    AL1(PFTCLRKY)                                                    
BPFT04SL DC    CL3'B0 '             SELECT CODE                                 
         DC    CL8'PW      '        RECORD                                      
         DC    CL8'BILL    '        ACTION                                      
BPFT04K  DC    AL1(KEYTYTWA,L'PWLMED-1),AL2(PWLMED-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLCLT-1),AL2(PWLCLT-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLPRD-1),AL2(PWLPRD-T217FFD)                     
         DC    AL1(KEYTYTWA,L'PWLEST-1),AL2(PWLEST-T217FFD)                     
         DC    AL1(KEYTYCUR,L'LSTMKT-1),AL2(LSTMKT-LISTD)                       
BPFT04X  EQU   *                                                                
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
*=========================== OPTIONS TABLES ==========================*         
                                                                                
*---------------- OPTION TABLES FOR MAINT LIST SCREEN ----------------*         
                                                                                
MOPTABLE DS    0C                  SEE OPTDSECT                                 
*                                                                               
OPT1     DS    0C                  SL=ALL                                       
         DC    AL1(OPSLALL#),AL1(OPT1X-OPT1)                                    
         DC    AL1(OPUSLALL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(OPUSBBPM-OPUSLALL)                                           
         DC    AL1(OPFVRTN+OPFNULFY)                                            
         DC    AL1(VLA#,0)                                                      
         DC    AL1(L'LKSELCDE),AL2(LKSELCDE-SYSD)                               
OPT1NAM1 DC    AL1(OPT1NAMX-OPT1NAM1-1),C'SL'                                   
OPT1NAMX DC    AL1(EOT)                                                         
OPT1X    EQU   *                                                                
*                                                                               
OPT2     DS    0C                  BL=ALL                                       
         DC    AL1(OPBLALL#),AL1(OPT2X-OPT2)                                    
         DC    AL1(OPUBLALL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(OPUSBBPM-OPUBLALL)                                           
         DC    AL1(OPFVRTN+OPFNULFY)                                            
         DC    AL1(VLA#,0)                                                      
         DC    AL1(L'LKSELCDE),AL2(LKSELCDE-SYSD)                               
OPT2NAM1 DC    AL1(OPT2NAMX-OPT2NAM1-1),C'BL'                                   
OPT2NAMX DC    AL1(EOT)                                                         
OPT2X    EQU   *                                                                
*                                                                               
OPT3     DS    0C                  BU=ALL                                       
         DC    AL1(OPBUALL#),AL1(OPT3X-OPT3)                                    
         DC    AL1(OPUBUALL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(OPUSBBPM-OPUBUALL)                                           
         DC    AL1(OPFVRTN+OPFNULFY)                                            
         DC    AL1(VLA#,0)                                                      
         DC    AL1(L'LKSELCDE),AL2(LKSELCDE-SYSD)                               
OPT3NAM1 DC    AL1(OPT3NAMX-OPT3NAM1-1),C'BU'                                   
OPT3NAMX DC    AL1(EOT)                                                         
OPT3X    EQU   *                                                                
*                                                                               
OPT4     DS    0C                  PL=ALL                                       
         DC    AL1(OPPLALL#),AL1(OPT4X-OPT4)                                    
         DC    AL1(OPUPLALL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(OPUSBBPM-OPUPLALL)                                           
         DC    AL1(OPFVRTN+OPFNULFY)                                            
         DC    AL1(VLA#,0)                                                      
         DC    AL1(L'LKSELCDE),AL2(LKSELCDE-SYSD)                               
OPT4NAM1 DC    AL1(OPT4NAMX-OPT4NAM1-1),C'PL'                                   
OPT4NAMX DC    AL1(EOT)                                                         
OPT4X    EQU   *                                                                
*                                                                               
OPT5     DS    0C                  M=ALL                                        
         DC    AL1(OPMTALL#),AL1(OPT5X-OPT5)                                    
         DC    AL1(OPUMTALL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(OPUSBBPM-OPUMTALL)                                           
         DC    AL1(OPFVRTN+OPFNULFY)                                            
         DC    AL1(VLA#,0)                                                      
         DC    AL1(L'LKSELCDE),AL2(LKSELCDE-SYSD)                               
OPT5NAM1 DC    AL1(OPT5NAMX-OPT5NAM1-1),C'M'                                    
OPT5NAMX DC    AL1(EOT)                                                         
OPT5X    EQU   *                                                                
*                                                                               
OPTX     DC    AL1(EOT)                                                         
                                                                                
*---------------- OPTION TABLES FOR BILLER LIST SCREEN ---------------*         
                                                                                
BOPTABLE DS    0C                  SEE OPTDSECT                                 
*                                                                               
BOPT1    DS    0C                  B=ALL                                        
         DC    AL1(OPBIALL#),AL1(BOPT1X-BOPT1)                                  
         DC    AL1(OPUBIALL)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(OPFVRTN+OPFNULFY)                                            
         DC    AL1(VLA#,0)                                                      
         DC    AL1(L'LKSELCDE),AL2(LKSELCDE-SYSD)                               
BOPT1NA1 DC    AL1(BOPT1NAX-BOPT1NA1-1),C'B'                                    
BOPT1NAX DC    AL1(EOT)                                                         
BOPT1X   EQU   *                                                                
*                                                                               
BOPTX    DC    AL1(EOT)                                                         
         EJECT                                                                  
*======================= FILTER KEYWORDS TABLE =======================*         
                                                                                
*--------------- FILTER TABLES FOR MAINT LIST SCREEN -----------------*         
                                                                                
MFLTABLE DS    0C                  SEE FLTDSECT                                 
*                                                                               
FLT1     DS    0C                  GLS=                                         
         DC    AL1(1),AL1(FLT1X-FLT1)                                           
         DC    AL1(FLTUGLS,0,FLFVLTAB)                                          
         DC    AL2(YESNOMAN-T21742)                                             
         DC    AL1(FLTGLSMX)                                                    
         DC    AL2(FLTGLSNM-SYSD),AL1(L'FLTGLSTB)                               
         DC    AL2(FLT1NAM1-FLT1,FLT1FTS1-FLT1)                                 
FLT1NAM1 DC    AL1(FLT1NAMX-FLT1NAM1-1),C'GLS'                                  
FLT1NAMX DC    AL1(EOT)                                                         
FLT1FTS1 DC    AL1(FTSTYXCT),AL1(L'ANYGOAL)                                     
         DC    AL2(ANYGOAL-SYSD,0)                                              
         DC    AL2(0)                                                           
FLT1FTSX DC    AL1(EOT)                                                         
FLT1X    EQU   *                                                                
*                                                                               
FLT2     DS    0C                  CLK=                                         
         DC    AL1(2),AL1(FLT2X-FLT2)                                           
         DC    AL1(FLTUCLK,0,FLFVLRTN)                                          
         DC    AL1(VFD#,0)                                                      
         DC    AL1(FLTCLKMX)                                                    
         DC    AL2(FLTCLKNM-SYSD),AL1(L'FLTCLKTB)                               
         DC    AL2(FLT2NAM1-FLT2,FLT2FTS1-FLT2)                                 
FLT2NAM1 DC    AL1(FLT2NAM2-FLT2NAM1-1),C'CLK'                                  
FLT2NAM2 DC    AL1(FLT2NAMX-FLT2NAM2-1),C'CL$LK'                                
FLT2NAMX DC    AL1(EOT)                                                         
FLT2FTS1 DC    AL1(FTSTYLOW),AL1(L'FTVDLDAT)                                    
         DC    AL2(DTIELEM-SYSD,PWDTICLD-PWDTIEL)                               
         DC    AL2(FTVDLDAT-FTVDATD)                                            
FLT2FTS2 DC    AL1(FTSTYHI),AL1(L'FTVDHDAT)                                     
         DC    AL2(DTIELEM-SYSD,PWDTICLD-PWDTIEL)                               
         DC    AL2(FTVDHDAT-FTVDATD)                                            
FLT2FTSX DC    AL1(EOT)                                                         
FLT2X    EQU   *                                                                
*                                                                               
FLT3     DS    0C                  SLK=                                         
         DC    AL1(3),AL1(FLT3X-FLT3)                                           
         DC    AL1(FLTUSLK,0,FLFVLRTN)                                          
         DC    AL1(VFD#,0)                                                      
         DC    AL1(FLTSLKMX)                                                    
         DC    AL2(FLTSLKNM-SYSD),AL1(L'FLTSLKTB)                               
         DC    AL2(FLT3NAM1-FLT3,FLT3FTS1-FLT3)                                 
FLT3NAM1 DC    AL1(FLT3NAM2-FLT3NAM1-1),C'SLK'                                  
FLT3NAM2 DC    AL1(FLT3NAMX-FLT3NAM2-1),C'STALK'                                
FLT3NAMX DC    AL1(EOT)                                                         
FLT3FTS1 DC    AL1(FTSTYLOW),AL1(L'FTVDLDAT)                                    
         DC    AL2(DTIELEM-SYSD,PWDTISLD-PWDTIEL)                               
         DC    AL2(FTVDLDAT-FTVDATD)                                            
FLT3FTS2 DC    AL1(FTSTYHI),AL1(L'FTVDHDAT)                                     
         DC    AL2(DTIELEM-SYSD,PWDTISLD-PWDTIEL)                               
         DC    AL2(FTVDHDAT-FTVDATD)                                            
FLT3FTSX DC    AL1(EOT)                                                         
FLT3X    EQU   *                                                                
*                                                                               
FLT4     DS    0C                  BLK=                                         
         DC    AL1(4),AL1(FLT4X-FLT4)                                           
         DC    AL1(FLTUBLK,0,FLFVLRTN)                                          
         DC    AL1(VFL#,0)                                                      
         DC    AL1(FLTBLKMX)                                                    
         DC    AL2(FLTBLKNM-SYSD),AL1(L'FLTBLKTB)                               
         DC    AL2(FLT4NAM1-FLT4,FLT4FTS1-FLT4)                                 
FLT4NAM1 DC    AL1(FLT4NAM2-FLT4NAM1-1),C'BLK'                                  
FLT4NAM2 DC    AL1(FLT4NAMX-FLT4NAM2-1),C'BUYLK'                                
FLT4NAMX DC    AL1(EOT)                                                         
FLT4FTS1 DC    AL1(FTSTYXCT),AL1(L'FTVLLOCK)                                    
         DC    AL2(BUYLOCK-SYSD,0)                                              
         DC    AL2(FTVLLOCK-FTVLDTD)                                            
FLT4FTS2 DC    AL1(FTSTYLOW),AL1(L'FTVLLDAT)                                    
         DC    AL2(DTIELEM-SYSD,PWDTIBLD-PWDTIEL)                               
         DC    AL2(FTVLLDAT-FTVLDTD)                                            
FLT4FTS3 DC    AL1(FTSTYHI),AL1(L'FTVLHDAT)                                     
         DC    AL2(DTIELEM-SYSD,PWDTIBLD-PWDTIEL)                               
         DC    AL2(FTVLHDAT-FTVLDTD)                                            
FLT4FTSX DC    AL1(EOT)                                                         
FLT4X    EQU   *                                                                
*                                                                               
FLT5     DS    0C                  PLK=                                         
         DC    AL1(5),AL1(FLT5X-FLT5)                                           
         DC    AL1(FLTUPLK,0,FLFVLRTN)                                          
         DC    AL1(VFL#,0)                                                      
         DC    AL1(FLTPLKMX)                                                    
         DC    AL2(FLTPLKNM-SYSD),AL1(L'FLTPLKTB)                               
         DC    AL2(FLT5NAM1-FLT5,FLT5FTS1-FLT5)                                 
FLT5NAM1 DC    AL1(FLT5NAM2-FLT5NAM1-1),C'PLK'                                  
FLT5NAM2 DC    AL1(FLT5NAMX-FLT5NAM2-1),C'PWLK'                                 
FLT5NAMX DC    AL1(EOT)                                                         
FLT5FTS1 DC    AL1(FTSTYXCT),AL1(L'FTVLLOCK)                                    
         DC    AL2(PWLOCK-SYSD,0)                                               
         DC    AL2(FTVLLOCK-FTVLDTD)                                            
FLT5FTS2 DC    AL1(FTSTYLOW),AL1(L'FTVLLDAT)                                    
         DC    AL2(DTIELEM-SYSD,PWDTIPLD-PWDTIEL)                               
         DC    AL2(FTVLLDAT-FTVLDTD)                                            
FLT5FTS3 DC    AL1(FTSTYHI),AL1(L'FTVLHDAT)                                     
         DC    AL2(DTIELEM-SYSD,PWDTIPLD-PWDTIEL)                               
         DC    AL2(FTVLHDAT-FTVLDTD)                                            
FLT5FTSX DC    AL1(EOT)                                                         
FLT5X    EQU   *                                                                
*                                                                               
FLT6     DS    0C                  DC=                                          
         DC    AL1(6),AL1(FLT6X-FLT6)                                           
         DC    AL1(FLTUMDC,0,FLFVLRTN)                                          
         DC    AL1(VDC#,0)                                                      
         DC    AL1(FLTMDCMX)                                                    
         DC    AL2(FLTMDCNM-SYSD),AL1(L'FLTMDCTB)                               
         DC    AL2(FLT6NAM1-FLT6,FLT6FTS1-FLT6)                                 
FLT6NAM1 DC    AL1(FLT6NAMX-FLT6NAM1-1),C'DC'                                   
FLT6NAMX DC    AL1(EOT)                                                         
FLT6FTS1 DC    AL1(FTSTYNCE),AL1(L'DCMTHBT)                                     
         DC    AL2(DCMTHBT-SYSD,0)                                              
         DC    AL2(0)                                                           
FLT6FTSX DC    AL1(EOT)                                                         
FLT6X    EQU   *                                                                
*                                                                               
FLT7     DS    0C                  NDC=                                         
         DC    AL1(7),AL1(FLT7X-FLT7)                                           
         DC    AL1(FLTUNDC,0,FLFVLRTN)                                          
         DC    AL1(VDC#,0)                                                      
         DC    AL1(FLTNDCMX)                                                    
         DC    AL2(FLTNDCNM-SYSD),AL1(L'FLTNDCTB)                               
         DC    AL2(FLT7NAM1-FLT7,FLT7FTS1-FLT7)                                 
FLT7NAM1 DC    AL1(FLT7NAMX-FLT7NAM1-1),C'NDC'                                  
FLT7NAMX DC    AL1(EOT)                                                         
FLT7FTS1 DC    AL1(FTSTYNCE),AL1(L'NDCMTHBT)                                    
         DC    AL2(NDCMTHBT-SYSD,0)                                             
         DC    AL2(0)                                                           
FLT7FTSX DC    AL1(EOT)                                                         
FLT7X    EQU   *                                                                
*                                                                               
FLT8     DS    0C                  BUYS=                                        
         DC    AL1(8),AL1(FLT8X-FLT8)                                           
         DC    AL1(FLTUBYS,0,FLFVLTAB)                                          
         DC    AL2(YESNO-T21742)                                                
         DC    AL1(FLTBYSMX)                                                    
         DC    AL2(FLTBYSNM-SYSD),AL1(L'FLTBYSTB)                               
         DC    AL2(FLT8NAM1-FLT8,FLT8FTS1-FLT8)                                 
FLT8NAM1 DC    AL1(FLT8NAMX-FLT8NAM1-1),C'BUYS'                                 
FLT8NAMX DC    AL1(EOT)                                                         
FLT8FTS1 DC    AL1(FTSTYXCT),AL1(L'ANYBUYS)                                     
         DC    AL2(ANYBUYS-SYSD,0)                                              
         DC    AL2(0)                                                           
FLT8FTSX DC    AL1(EOT)                                                         
FLT8X    EQU   *                                                                
*                                                                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
*--------------- FILTER TABLES FOR BILLER LIST SCREEN ----------------*         
                                                                                
BFLTABLE DS    0C                  SEE FLTDSECT                                 
*                                                                               
BFLT1    DS    0C                  PLK=                                         
         DC    AL1(1),AL1(BFLT1X-BFLT1)                                         
         DC    AL1(FLTUPLK,0,FLFVLRTN)                                          
         DC    AL1(VFL#,0)                                                      
         DC    AL1(FLTPLKMX)                                                    
         DC    AL2(FLTPLKNM-SYSD),AL1(L'FLTPLKTB)                               
         DC    AL2(BFLT1NA1-BFLT1,BFLT1FT1-BFLT1)                               
BFLT1NA1 DC    AL1(BFLT1NA2-BFLT1NA1-1),C'PLK'                                  
BFLT1NA2 DC    AL1(BFLT1NAX-BFLT1NA2-1),C'PWLK'                                 
BFLT1NAX DC    AL1(EOT)                                                         
BFLT1FT1 DC    AL1(FTSTYXCT),AL1(L'FTVLLOCK)                                    
         DC    AL2(PWLOCK-SYSD,0)                                               
         DC    AL2(FTVLLOCK-FTVLDTD)                                            
BFLT1FT2 DC    AL1(FTSTYLOW),AL1(L'FTVLLDAT)                                    
         DC    AL2(DTIELEM-SYSD,PWDTIPLD-PWDTIEL)                               
         DC    AL2(FTVLLDAT-FTVLDTD)                                            
BFLT1FT3 DC    AL1(FTSTYHI),AL1(L'FTVLHDAT)                                     
         DC    AL2(DTIELEM-SYSD,PWDTIPLD-PWDTIEL)                               
         DC    AL2(FTVLHDAT-FTVLDTD)                                            
BFLT1FTX DC    AL1(EOT)                                                         
BFLT1X   EQU   *                                                                
*                                                                               
BFLT2    DS    0C                  BLLD=                                        
         DC    AL1(2),AL1(BFLT2X-BFLT2)                                         
         DC    AL1(0,FLTU2BIL,FLFVLRTN)                                         
         DC    AL1(VBI#,0)                                                      
         DC    AL1(FLTBILMX)                                                    
         DC    AL2(FLTBILNM-SYSD),AL1(L'FLTBILTB)                               
         DC    AL2(BFLT2NA1-BFLT2,BFLT2FT1-BFLT2)                               
BFLT2NA1 DC    AL1(BFLT2NAX-BFLT2NA1-1),C'BLLD'                                 
BFLT2NAX DC    AL1(EOT)                                                         
BFLT2FT1 DC    AL1(FTSTYXCT),AL1(L'BILLTYPE)                                    
         DC    AL2(BILLTYPE-SYSD,0)                                             
         DC    AL2(0)                                                           
BFLT2FTX DC    AL1(EOT)                                                         
BFLT2X   EQU   *                                                                
*                                                                               
BFLT3    DS    0C                  DRCR=                                        
         DC    AL1(3),AL1(BFLT3X-BFLT3)                                         
         DC    AL1(0,FLTU2DC,FLFVLTAB)                                          
         DC    AL2(YESNO-T21742)                                                
         DC    AL1(FLTDCMX)                                                     
         DC    AL2(FLTDCNM-SYSD),AL1(L'FLTDCTB)                                 
         DC    AL2(BFLT3NA1-BFLT3,BFLT3FT1-BFLT3)                               
BFLT3NA1 DC    AL1(BFLT3NAX-BFLT3NA1-1),C'DRCR'                                 
BFLT3NAX DC    AL1(EOT)                                                         
BFLT3FT1 DC    AL1(FTSTYXCT),AL1(L'ANYDRCR)                                     
         DC    AL2(ANYDRCR-SYSD,0)                                              
         DC    AL2(0)                                                           
BFLT3FTX DC    AL1(EOT)                                                         
BFLT3X   EQU   *                                                                
*                                                                               
         DC    AL1(EOT)                                                         
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST'                                   
***********************************************************************         
*========================== T21742's EQUATES =========================*         
XFF      EQU   X'FF'                                                            
X80      EQU   X'80'                                                            
X40      EQU   X'40'                                                            
X20      EQU   X'20'                                                            
X10      EQU   X'10'                                                            
X08      EQU   X'08'                                                            
X04      EQU   X'04'                                                            
X02      EQU   X'02'                                                            
X01      EQU   X'01'                                                            
                                                                                
*                                 ******** PERMISSABLE OPTIONS ********         
OPSLALL# EQU   1                   SL=ALL  (MAINT LIST SCREEN)                  
OPBLALL# EQU   2                   BL=ALL  (MAINT LIST SCREEN)                  
OPBUALL# EQU   3                   BU=ALL  (MAINT LIST SCREEN)                  
OPPLALL# EQU   4                   PL=ALL  (MAINT LIST SCREEN)                  
OPMTALL# EQU   5                   M=ALL   (MAINT LIST SCREEN)                  
OPBIALL# EQU   6                   B=ALL   (BILLER LIST SCREEN)                 
OPMONTH# EQU   7                   MONTH   (BILLER LIST SCREEN)                 
                                                                                
*                                 ************ MISCELLANEOUS **********         
EOT      EQU   X'00'                                                            
PAGEQ    EQU   1                                                                
MINPWQ   EQU   -10000              MIN PW% = -100.00%                           
MAXPWQ   EQU   8499                MAX PW% = +84.99%                            
PKYESTL  EQU   PWKMKT-PWFKEY                L(PW KEY) UP TILL MARKET            
GKYPRDL  EQU   GKEYPRD+L'GKEYPRD-GOALREC    L(GOAL KEY) INCLUDE PRD             
GKYESTL  EQU   GKEYEST+L'GKEYEST-GOALREC    L(GOAL KEY) INCLUDE EST             
BKYPRDL  EQU   BUYKPRD-BUYKEY+L'BUYKPRD     L(BUY KEY) INCLUDING PRD            
BKEYMKTL EQU   (BUYMSTA+2)-BUYKEY           L(BUY KEY) INCLUDING MKT            
SKYESTL  EQU   STABKEST+L'STABKEST-STABUCKD L(SB KEY) INCLUDING EST             
SKYMKTL  EQU   STABKMKT+L'STABKMKT-STABUCKD L(SB KEY) INCLUDING MKT             
BIKYYML  EQU   BKEYMSRV+L'BKEYMSRV-BILLRECD L(BI KEY) INCLUDING Y/M             
NLSTLINQ EQU   ((PWLTOTLH-PWLSELH)/(PWLDATA+L'PWLDATA-PWLSELH))                 
PWBLKL   EQU   PWBLKX-PWBLKD                                                    
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (SPSFMWORKD)'                      
***********************************************************************         
*============================= SPSFMWORKD ============================*         
       ++INCLUDE SPSFMWORKD                                                     
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (TWA DSECTS)'                      
***********************************************************************         
*================================ TWA ================================*         
                                                                                
*---------------------------- BASE SCREEN ----------------------------*         
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
*--------------------------- PW MAINT SCREEN -------------------------*         
         ORG   CONTAGH                                                          
         PRINT OFF                                                              
       ++INCLUDE SCSFM9AD                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*---------------------------- PW BILL SCREEN -------------------------*         
         ORG   CONTAGH                                                          
         PRINT OFF                                                              
       ++INCLUDE SCSFM98D                                                       
         PRINT ON                                                               
         SPACE 2                                                                
*---------------------------- PW LOCK SCREEN -------------------------*         
         ORG   CONTAGH                                                          
         PRINT OFF                                                              
       ++INCLUDE SCSFM97D                                                       
         PRINT ON                                                               
         EJECT                                                                  
*--------------------------- PW LIST SCREEN --------------------------*         
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM99D                                                       
                                                                                
         DS    0CL((L'PWLOPTN-L'PWMOPTN)+1)                                     
         DS    0CL((L'PWMOPTN-L'PWLOPTN)+1)                                     
         EJECT                                                                  
*---------------------------- STORAGE AREA ---------------------------*         
                                                                                
SVACTEQU DS   XL(L'ACTEQU)         ACTEQU OF PREV TRANSACTION                   
*                                                                               
MY99TWAL EQU   *-CONHEADH                                                       
         DS    0CL(3520-MY99TWAL)  CHECK AGAINST GENCON'S TWA LIMIT             
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST OVERLAY'                           
***********************************************************************         
*============================ OTHER DSECTS ===========================*         
                                                                                
* DDGENTWA                                                                      
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* FAGETTXTD                                                                     
* FATIOB                                                                        
* DDCOMFACS                                                                     
* DDPERVALD                                                                     
* DDCOREQUS                                                                     
* SPDDEQUS                                                                      
* SPMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDGENTWA                                                       
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
       ++INCLUDE FAGETTXTD                                                      
       ++INCLUDE FATIOB                                                         
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE DDCOREQUS                                                      
       ++INCLUDE SPDDEQUS                                                       
       ++INCLUDE SPMSGEQUS                                                      
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST (SPGEN DSECTS)'                    
***********************************************************************         
*============================ SPGEN DSECTS ===========================*         
                                                                                
*----------------------------- SPGENWIPW -----------------------------*         
       ++INCLUDE SPGENWIPW         WESTERN INTL. PROFIT WITHIN REC              
         EJECT                                                                  
*------------------------------ SPGENAGY -----------------------------*         
AGYHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENAGY                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ SPGENCLT -----------------------------*         
CLTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ SPGENEST -----------------------------*         
ESTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ SPGENBUY -----------------------------*         
BUYRECD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ SPGENMKT -----------------------------*         
MKTRECD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENMKT                                                       
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ SPGENGOAL ----------------------------*         
GOALRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENGOAL                                                      
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ SPGENSTAB ----------------------------*         
         PRINT OFF                                                              
       ++INCLUDE SPGENSTAB                                                      
         PRINT ON                                                               
                                                                                
                                                                                
*------------------------------ SPGENBILL ----------------------------*         
BILLRECD DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENBILL                                                      
         PRINT ON                                                               
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST OVERLAY'                           
***********************************************************************         
*============================== PW BLOCK =============================*         
                                                                                
       ++INCLUDE SPPWBLOCK                                                      
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST OVERLAY (SYSD)'                    
***********************************************************************         
*========================== SAVED WORK AREA ==========================*         
                                                                                
SYSD     DSECT                                                                  
         ORG   SYSSPARE                                                         
* The following area is shared by the PW phases under SPSFM.  The               
*  purpose is to enable communication among themselves when needed.             
                                                                                
PVACTEQU DS    XL(L'ACTEQU)        ACTEQU OF LAST TRANSACTION                   
CTODAY   DS    XL2                 TODAY'S DATE COMPRESSED                      
CALLPFK  DS    XL(L'PFKEY)                                                      
BUYRID   DS    CL3                 BUYER'S INITIALS                             
BINYRMT  DS    XL2                 A BINARY YEAR/MONTH FIELD                    
SVOPTFLD DS    CL(L'PWLOPTN)       SAVE TEXT IN OPTIONS FIELD                   
                                                                                
* The following fields are owned by this program only                           
                                                                                
*                                 ************* ADDRESSES *************         
RELO     DS    F                   RELOCATABLE FACTOR                           
BASE1    DS    A                   A(T21742)                                    
BASE2    DS    A                   A(T21742+X'1000')                            
                                                                                
ATABS    DS    0A                  A(TABLES IN T21742 CSECT)                    
ADTDSPTB DS    A                    A(TABLE OF DATE DISPLACEMENTS)              
ALABLTB  DS    A                    A(TBL OF LABELS FOR SPOT STRG AREA)         
ACORQLST DS    A                    A(LIST OF CORE-RES PHASES TO GET)           
AMPFTAB  DS    A                    A(PFTABLES FOR MAINT LIST)                  
ABPFTAB  DS    A                    A(PFTABLES FOR BILLER LIST)                 
AMFLTAB  DS    A                    A(FLTABLES FOR MAINT LIST)                  
ABFLTAB  DS    A                    A(FLTABLES FOR BILLER LIST)                 
AMOPTAB  DS    A                    A(OPTABLES FOR MAINT LIST)                  
ABOPTAB  DS    A                    A(OPTABLES FOR BILLER LIST)                 
AGOSUB   DS    A                    A(SUBROUTINE INTERFACE)                     
ASUBR01  DS    A                    A(1ST SUBROUTINE POOL)                      
ASUBR02  DS    A                    A(2ND SUBROUTINE POOL)                      
ASUBR03  DS    A                    A(3RD SUBROUTINE POOL)                      
ATABSQ   EQU   (*-ATABS)/(L'ATABS)                                              
         DS    0CL((ATABSQ-TTABDSPQ)+1)                                         
         DS    0CL((TTABDSPQ-ATABSQ)+1)                                         
                                                                                
ATIATBS  DS    0A                  A(TABLES IN TIA)                             
AACCUTAB DS    A                    A(ACCUTAB)                                  
AESTAB   DS    A                    A(ESTAB)                                    
APWREC   DS    A                    A(PWREC)                                    
ASBLOCK  DS    A                    A(SPOT BLOCK)                               
ATIATBSQ EQU   (*-ATIATBS)/(L'ATIATBS)                                          
         DS    0CL((ATIATBSQ-LBLTABQ)+1)                                        
         DS    0CL((LBLTABQ-ATIATBSQ)+1)                                        
                                                                                
ACORES   DS    0A                  A(CORE RES PHASES)                           
APWCALC  DS    A                    A(PWCALC)                                   
ASPOTIO  DS    A                    A(SPOTIO)                                   
ASPOTBUY DS    A                    A(SPOTBUY)                                  
ASPOTGL  DS    A                    A(SPOTGL)                                   
AMOBILE  DS    A                    A(MOBILE)                                   
AGETBROD DS    A                    A(GETBROAD)                                 
ACORESQ  EQU   (*-ACORES)/(L'ACORES)                                            
         DS    0CL((ACORESQ-COREQLSQ)+1)                                        
         DS    0CL((COREQLSQ-ACORESQ)+1)                                        
                                                                                
         ORG   AGETBROD                                                         
MOBINPAD DS    0F                  MOBILE ADCON LIST                            
         DS    A                    A(GETBROAD)                                 
AADDAY   DS    A                    A(ADDAY)                                    
AGETDAY  DS    A                    A(GETDAY)                                   
ADATCON  DS    A                    A(DATCON)                                   
                                                                                
APVNTRY  DS    A                   A(PREVIOUS TABLE ENTRY)                      
ASUBRTN  DS    A                   A(SUBROUTINE POOL) TO USE                    
APFTABS  DS    A                   A(PFTABLES) TO USE                           
AFLTABS  DS    A                   A(FLTABLES) TO USE                           
AOPTABS  DS    A                   A(OPTABLES) TO USE                           
                                                                                
*                                 ************* CONSTANTS *************         
RELOCKSW DS    XL2                 SWITCH TO DO A RE-LOCK (SFM3E)               
                                                                                
*                                 **************** MISC ***************         
MYDUB    DS    D                                                                
OPWPCT   DS    F                   THE PW% FROM ESTIMATE RECORD                 
HOLDRE   DS    F                                                                
                                                                                
TOTLVALS DS    0F                                                               
WIMLKTOT DS    F                                                                
CLTLKTOT DS    F                                                                
TAXLKTOT DS    F                                                                
GOALTOTL DS    F                                                                
DRCRTOTL DS    F                                                                
TOTLVALQ EQU   *-TOTLVALS                                                       
                                                                                
TEMPACB  DS    F                   TEMPORARY ACTUAL   BUY $                     
TEMPAJB  DS    F                       "     ADJUSTED BUY $                     
TEMPTAX  DS    F                       "     TAX$                               
TEMPPW   DS    F                       "     PW%                                
ACCUTABN DS    H                   # OF ENTRIES IN ACCUTAB                      
WHOLIST  DS    CL1                 M(AINT) OR B(ILL) LIST SCREEN                
MYAGYID  DS    CL(L'AGYID)         AGENCY ID FROM AGYHDR                        
ESDATE   DS    CL(L'ESTART)        EBCDIC START DATE OF ESTIMATE                
EEDATE   DS    CL(L'EEND)            "    END    "   "     "                    
ESDATEB  DS    XL3                 BINARY START DATE OF ESTIMATE                
EEDATEB  DS    XL3                   "    END    "   "     "                    
TEMPKEY  DS    XL(L'KEY)           MY TEMPORARY STORAGE FOR KEY                 
TEMPBMKT DS    XL(L'BMKT)          "      "        "     "  BMKT                
SVINPMKT DS    XL(L'BMKT)          SAVE INPUT MARKET                            
MYERRCD  DS    XL1                 MY ERROR CODE                                
STARTEND DS    CL12                TEMP STRGE FOR STRT-END DATE                 
DATE6    DS    CL6                 TEMP EBCDIC DATE STORAGE                     
DATE3    DS    XL3                 TEMP BINARY DATE STORAGE                     
DATE2    DS    XL2                 TEMP COMPRESSED DATE STORAGE                 
SDATE2   DS    XL2                 TEMP COMPRESSED START DATE STORAGE           
EDATE2   DS    XL2                  "       "      END    "      "              
RUNDATE  DS    XL2                                                              
MSGNUM2  DS    XL2                 2-BYTE MESSAGE NO.                           
MSGSYS   DS    XL1                 SYSTEM UNDER WHICH THE MESSAGE LIVES         
NUMWEEKS DS    XL1                 # OF WEEKS IN SCHEDULE                       
COUNTER  DS    XL1                                                              
NTIMES   DS    XL1                 NUMBER OF TIMES TO LOOP                      
GOSUBN   DS    XL1                 ROUTINE # FOR SUB-RTN INTERFACE              
ANYGOAL  DS    CL1                 (Y)ES/(N)O/(M)ANUAL                          
ANYBUYS  DS    CL1                 (Y)ES/(N)O                                   
BUYLOCK  DS    CL1                 BUY STATUS--(L)OCK/(U)NLOCK                  
PWLOCK   DS    CL1                 PW  STATUS--(L)OCK/(U)NLOCK                  
DCMTHBT  DS    XL2                 MTHS (BITWISE) W/ DR/CR                      
NDCMTHBT DS    XL2                 MTHS (BITWISE) W/O DR/CR                     
BILLTYPE DS    XL1                 FF=NONE, 01=ESTIM, 02=FINAL BILLED           
ANYDRCR  DS    CL1                 Y=DR/CR EXISTS, N=DOESN'T EXIST              
LKALLMDE DS    CL1                                                              
LKALLDNE DS    CL1                                                              
CURSROW  DS    XL1                 (ROW#-1) OF CURSOR POSITION                  
NOPTN    DS    XL1                 # OF OPTIONS INPUTTED                        
ELCDLO   DS    XL1                 LOW  ELCODE                                  
ELCDHI   DS    XL1                 HIGH ELCODE                                  
ESTOOWSD DS    XL(L'EOWSDAY)       OUT-OF-WEEK ROTATOR START DAY                
         EJECT                                                                  
*                                 *************** FLAGS ***************         
CHNGFLG1 DS    XL1                 CHANGED (FROM PREV TRNSCTN) FLAG #1          
CF1ACTEQ EQU   X80                  ACTION EQUATE CHANGED                       
CF1KEY   EQU   X40                  A KEY FIELD CHANGED                         
CF1OPT   EQU   X20                  AN OPTION CHANGED                           
CF1FLT   EQU   X10                  A FILTER CHANGED                            
CF1MKT   EQU   X08                  MARKET CHANGED                              
CF1BYR   EQU   X04                  BUYER (SUPERVISOR) CHANGED                  
CF1ACCTB EQU   X02                  NON-SPSFM42 ACCUTAB IN TEMPSTR              
CF1AK    EQU   CF1ACTEQ+CF1KEY                                                  
CF1AKO   EQU   CF1ACTEQ+CF1KEY+CF1OPT                                           
CF1AKOFM EQU   CF1ACTEQ+CF1KEY+CF1OPT+CF1FLT+CF1MKT                             
CF1KOFM  EQU   CF1KEY+CF1OPT+CF1FLT+CF1MKT                                      
CF1KOFMB EQU   CF1KEY+CF1OPT+CF1FLT+CF1MKT+CF1BYR                               
CF1ALL   EQU   CF1ACTEQ+CF1KEY+CF1OPT+CF1FLT+CF1MKT+CF1BYR+CF1ACCTB             
CF1ALL_B EQU   CF1ALL-CF1BYR                                                    
CF1ALL_T EQU   CF1ALL-CF1ACCTB                                                  
                                                                                
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAG #1                        
MF1LSTDN EQU   X80                  FINISH LISTING                              
MF1ERROR EQU   X40                  PREV TRANSACTION RESULTED IN ERROR          
MF1DRCR  EQU   X20                  AT LEAST 1 DR/CR NOT EQ TO NULLS            
MF1BLTAB EQU   X10                  TABLES BUILT IN PREV TRANSACTION            
MF1NOGOL EQU   X08                  NOT ONE *@)$^(!& IN ALL MKTS                
                                                                                
SELFLAG1 DS    XL1                 SELECT FLAG                                  
SF1TSLOK EQU   X80                  TESTSEL RETURNED W/ OK                      
SF1MNSEL EQU   X40                  USER DID A MANUAL SELECTION                 
SF1SELCT EQU   X20                  A VALID SELECTION HAS BEEN MADE             
                                                                                
FLTUSED  DS    XL1                 FILTERS-USED FLAG                            
FLTUGLS  EQU   X80                  FILTER ON GOALS                             
FLTUCLK  EQU   X40                  FILTER ON CLIENT$ LOCK                      
FLTUSLK  EQU   X20                  FILTER ON STATION LOCK                      
FLTUBLK  EQU   X10                  FILTER ON BUY     LOCK                      
FLTUPLK  EQU   X08                  FILTER ON PW      LOCK                      
FLTUMDC  EQU   X04                  FILTER ON MONTHS W/ DR/CR                   
FLTUNDC  EQU   X02                  FILTER ON MONTHS W/O DR/CR                  
FLTUBYS  EQU   X01                  FILTER ON BUYS                              
                                                                                
FLTUSED2 DS    XL1                 FILTERS-USED FLAG #2                         
FLTU2BIL EQU   X80                  FILTER ON BILLED TYPE                       
FLTU2DC  EQU   X40                  FILTER ON DRCR                              
                                                                                
OPTUSED  DS    XL1                 OPTIONS USED FLAG (OPTDSECT--OPTBIT)         
OPUSLALL EQU   X80                  SL=ALL STATION LOCK ALL MARKETS             
OPUBLALL EQU   X40                  BL=ALL BUY LOCK ALL MARKETS                 
OPUBUALL EQU   X20                  BU=ALL BUY UNLOCK ALL MARKETS               
OPUPLALL EQU   X10                  PL=ALL BUY UNLOCK ALL MARKETS               
OPUMTALL EQU   X08                  M=ALL  GOTO MAINT SCRN FOR ALL MKTS         
OPUBIALL EQU   X04                  B=ALL  GOTO BILL SCRN FOR ALL MKTS          
OPUMONTH EQU   X02                  MONTH (BILLER LIST SCREEN)                  
OPUSBBPL EQU   OPUSLALL+OPUBLALL+OPUBUALL+OPUPLALL                              
OPUSBBPM EQU   OPUSLALL+OPUBLALL+OPUBUALL+OPUPLALL+OPUMTALL                     
         EJECT                                                                  
*                                 *********** OPTION VALUES ***********         
OPTVALS  DS    0C                                                               
                                                                                
         DS    XL1                  DSPL IN OPTNS FLD OF BELOW DATA VAL         
LKSELCDE DS    CL3                 SEL CODE FOR ALL-LOCKING                     
         DS    XL1                  DSPL IN OPTNS FLD OF BELOW DATA VAL         
BILLYRMT DS    XL2                 BNRY YR/MTH TO LIST (BILLER LIST)            
BILLYMSE DS    2XL2                CMPRESSED STRT/END OF BLIST'S MONTH          
                                                                                
OPTVALSQ EQU   *-OPTVALS                                                        
         EJECT                                                                  
*                                 *********** FILTER VALUES ***********         
FLTVALS  DS    0C                                                               
                                                                                
FLTGLS   DS    0C                  FILTER VALUES FOR GOALS                      
FLTGLSMX EQU   2                    MAX # OF VALUES ALLOWED                     
FLTGLSNM DS    XL1                  # OF VALUES SO FAR                          
FLTGLSTB DS    (FLTGLSMX)XL1        TABLE TO HOLD VALUES                        
FLTGLSQ  EQU   *-FLTGLS                                                         
                                                                                
FLTCLK   DS    0C                  FILTER VALUES FOR CLIENT$ LOCK               
FLTCLKMX EQU   2                     MAX # OF VALUES ALLOWED                    
FLTCLKNM DS    XL1                   # OF VALUES SO FAR                         
FLTCLKTB DS    (FLTCLKMX)XL(FTVDATQ) TABLE TO HOLD VALUES                       
FLTCLKQ  EQU   *-FLTCLK                                                         
                                                                                
FLTSLK   DS    0C                  FILTER VALUES FOR STATION LOCK               
FLTSLKMX EQU   2                     MAX # OF VALUES ALLOWED                    
FLTSLKNM DS    XL1                   # OF VALUES SO FAR                         
FLTSLKTB DS    (FLTSLKMX)XL(FTVDATQ) TABLE TO HOLD VALUES                       
FLTSLKQ  EQU   *-FLTSLK                                                         
                                                                                
FLTBLK   DS    0C                  FILTER VALUES FOR BUY     LOCK               
FLTBLKMX EQU   2                     MAX # OF VALUES ALLOWED                    
FLTBLKNM DS    XL1                   # OF VALUES SO FAR                         
FLTBLKTB DS    (FLTBLKMX)XL(FTVLDTQ) TABLE TO HOLD VALUES                       
FLTBLKQ  EQU   *-FLTBLK                                                         
                                                                                
FLTPLK   DS    0C                  FILTER VALUES FOR PW      LOCK               
FLTPLKMX EQU   2                     MAX # OF VALUES ALLOWED                    
FLTPLKNM DS    XL1                   # OF VALUES SO FAR                         
FLTPLKTB DS    (FLTPLKMX)XL(FTVLDTQ) TABLE TO HOLD VALUES                       
FLTPLKQ  EQU   *-FLTPLK                                                         
                                                                                
FLTMDC   DS    0C                  FILTER VALUES FOR MNTHS W/ DR/CR             
FLTMDCMX EQU   2                     MAX # OF VALUES ALLOWED                    
FLTMDCNM DS    XL1                   # OF VALUES SO FAR                         
FLTMDCTB DS    (FLTMDCMX)XL2         TABLE TO HOLD VALUES                       
FLTMDCQ  EQU   *-FLTMDC                                                         
                                                                                
FLTNDC   DS    0C                  FILTER VALUES FOR MNTHS W/O DR/CR            
FLTNDCMX EQU   2                     MAX # OF VALUES ALLOWED                    
FLTNDCNM DS    XL1                   # OF VALUES SO FAR                         
FLTNDCTB DS    (FLTNDCMX)XL2         TABLE TO HOLD VALUES                       
FLTNDCQ  EQU   *-FLTNDC                                                         
                                                                                
FLTBYS   DS    0C                  FILTER VALUES FOR BUYS                       
FLTBYSMX EQU   1                    MAX # OF VALUES ALLOWED                     
FLTBYSNM DS    XL1                  # OF VALUES SO FAR                          
FLTBYSTB DS    (FLTGLSMX)XL1        TABLE TO HOLD VALUES                        
FLTBYSQ  EQU   *-FLTBYS                                                         
                                                                                
FLTBIL   DS    0C                  FILTER VALUES FOR BILLED TYPE                
FLTBILMX EQU   2                    MAX # OF VALUES ALLOWED                     
FLTBILNM DS    XL1                  # OF VALUES SO FAR                          
FLTBILTB DS    (FLTBILMX)XL1        TABLE TO HOLD VALUES                        
FLTBILQ  EQU   *-FLTBIL                                                         
                                                                                
FLTDC    DS    0C                  FILTER VALUES FOR ANY DR/CR AMOUNT           
FLTDCMX  EQU   1                    MAX # OF VALUES ALLOWED                     
FLTDCNM  DS    XL1                  # OF VALUES SO FAR                          
FLTDCTB  DS    (FLTDCMX)XL1         TABLE TO HOLD VALUES                        
FLTDCQ   EQU   *-FLTDC                                                          
                                                                                
FLTVALSQ EQU   *-FLTVALS                                                        
         EJECT                                                                  
*                                 *************** TABLES **************         
LISTINFO DS    (NLSTLINQ)XL1       INFO ABOUT EACH ENTRY ON LIST                
LISTINF1 DS    (NLSTLINQ)XL1       INFO ABOUT EACH ENTRY (ATFLAG1)              
LISTINF2 DS    (NLSTLINQ)XL1       INFO ABOUT EACH ENTRY (ATFLAG2)              
                                                                                
FRSTSELK DS    XL(L'LASTSELK)      KEY OF 1ST MARKET LISTED ON SCREEN           
                                                                                
BRDWKQ   EQU   14+1                MAX 14 BRDCST WKS (+1 FOR CUSHION)           
BRDWKTB2 DS    XL(BRDWKQ*4+1)                                                   
                                                                                
BRDMTHQ  EQU   5+1                 MAX 5 BRDCST MTHS (+1 FOR CUSHION)           
BRDMTHTB DS    XL(BRDMTHQ*4+1)                                                  
                                                                                
DTIELEM  DS    XL(PWDTILNQ)         A COPY OF PWDTIEL FROM PW RECORD            
                                                                                
PERVALB  DS    XL(L'PVALOUTB)       PERVAL BLOCK                                
                                                                                
MYTEXT   DS    0X                  MISCELLANEOUS TEXT FIELD                     
         DS    XL1                  L'TEXT                                      
         DS    CL20                 THE TEXT ITSELF                             
MYTEXTX  EQU   *                                                                
MYTEXTL  EQU   MYTEXTX-MYTEXT                                                   
         SPACE 2                                                                
MYSSPREL EQU   *-SYSSPARE                                                       
         DS    0CL(1024-MYSSPREL)  CHECK AGAINST AVAIL SYSSPARE AMT             
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST OVERLAY (MISC DSECTS)'             
***********************************************************************         
*========================== LIST LINE DSECT ==========================*         
                                                                                
*------------------------ BUYER (MAINT) SCREEN -----------------------*         
LISTD    DSECT                                                                  
LSTMKT   DS    CL4                                                              
         DS    CL1                                                              
LSTMKNAM DS    CL15                                                             
         DS    CL2                                                              
LSTBYS   DS    CL1                                                              
LSTGLS   DS    CL1                                                              
         DS    CL1                                                              
LSTCLK   DS    CL5                                                              
         DS    CL1                                                              
LSTSLK   DS    CL5                                                              
         DS    CL1                                                              
LSTBLK   DS    CL6                                                              
         DS    CL1                                                              
LSTPLK   DS    CL6                                                              
         DS    CL1                                                              
LSTCLOCK DS    CL7                                                              
         DS    CL1                                                              
LSTWLOCK DS    CL7                                                              
         DS    CL1                                                              
LSTPW    DS    CL6                                                              
LISTQ    EQU   *-LISTD                                                          
         DS    0CL(L'PWLDATA-LISTQ+1)                                           
                                                                                
*--------------------------- BILLER SCREEN ---------------------------*         
BLISTD   DSECT                                                                  
BLSTMKT  DS    CL4                                                              
         DS    CL1                                                              
BLSTMKNA DS    CL15                                                             
         DS    CL2                                                              
BLSTPLK  DS    CL6                                                              
         DS    CL1                                                              
BLSTBILL DS    CL4                                                              
         DS    CL1                                                              
BLSTDRCR DS    CL7                                                              
BLISTQ   EQU   *-BLISTD                                                         
         DS    0CL(L'PWLDATA-BLISTQ+1)                                          
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*====================== ACCUMULATOR TABLE DSECT ======================*         
                                                                                
ACCUTABD DSECT                                                                  
ATMRKT   DS    XL2                 MARKET                                       
                                                                                
ATFLAG1  DS    XL1                 FLAGS                                        
ATF1PFLT EQU   X80                  ENTRY PASSED FILTER (BLIST & LIST)          
ATF1FGLS EQU   X40                  GOAL RECORDS EXIST ON FILE                  
ATF1PGLS EQU   X20                  GOALS EXIST ON PW RECD (USER INPTD)         
ATF1FBYS EQU   X10                  BUY RECORDS EXIST ON FILE                   
ATF1LCGG EQU   X08                  LOCKED CLT$ > GOALS                         
ATF1ESTB EQU   X04                  MKT IS ESTIM BILLED (BLIST)                 
ATF1FNLB EQU   X02                  MKT IS FINAL BILLED (BLIST)                 
ATF1NOPW EQU   X01                  NO PW RECORD FOR MARKET                     
                                                                                
ATFLAG2  DS    XL1                 MORE FLAGS                                   
ATF2BKUP EQU   X80                  PW RECORD HAS BACKUP ELEMENTS               
                                                                                
         DS    XL1                 (NOT USED YET)                               
                                                                                
ATPWFLAG DS    XL(L'PWGNFLG)       PWGNFLG FROM PW RECORD                       
                                                                                
ATDTIELM DS    XL(PWDTILNQ)        PWDTIELEM FROM PW RECORD                     
                                                                                
ATDRCR   DS    XL4                 ADJ DR/CR FOR MNTH SPECIFIED (BLIST)         
         ORG   ATDRCR                                                           
ATCLOCK  DS    XL4                 CLIENT LOCKED$                               
ATWLOCK  DS    XL4                 WIM    LOCKED$                               
ATTAXLK  DS    XL4                 TAX$ LOCKED                                  
                                                                                
ACCUTABQ EQU   *-ACCUTABD                                                       
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== OPTIONS TABLE DSECT  =======================*         
                                                                                
OPTDSECT DSECT                                                                  
OPTNUMB  DS    XL1                 INTERNAL OPTION NUMBER (SEE EQUATES)         
OPTLEN   DS    XL1                 LENGTH OF OPTION TABLE                       
OPTBIT   DS    XL1                 OPTION BIT (SEE OPTUSED IN SYSD)             
OPTREQR  DS    XL1                 OTHER REQUIRED OPTIONS                       
OPTEXCL  DS    XL1                 OTHER OPTIONS NOT ALLOWED                    
OPTFLAG  DS    XL1                 FLAGS ABOUT OPTION ENTRY                     
OPFOTHAB EQU   X80                  VALUES FOUND IN ANOTHER TABLE               
OPFVRTN  EQU   X40                  VALIDATION VIA ROUTINE                      
OPFKYWRD EQU   X20                  OPTION SPECIFIED BY KEYWORD ONLY            
OPFNULFY EQU   X10                  OPTION CAN BE NULLIFIED (W/ '*')            
OPTRTNUM DS    XL1                 ROUTINE # (OPFVRTN)                          
         ORG   OPTRTNUM                                                         
OPTDSPVT DS    XL2                 DISPL OF VALIDATION TABLE (OPFOTHAB)         
OPTOLEN  DS    XL1                 OUTPUT LENGTH                                
OPTODSPL DS    XL2                 DISPL FROM SYSD OF OUTPUT FIELD              
OPTNAME  DS    0C                  AL1(L'NAME),C'NAME' ... AL1(EOT)             
***********************************************************************         
         EJECT                                                                  
***********************************************************************         
*======================== FILTER TABLES DSECT ========================*         
                                                                                
FLTDSECT DSECT                                                                  
FLTNUM   DS    XL1                 INTERNAL FILTER NUMBER                       
FLTLEN   DS    XL1                 LENGTH OF FILTER KEYWORD ENTRY               
FLTUBIT  DS    XL1                 FILTER-USED BIT                              
FLTUBIT2 DS    XL1                 FILTER-USED BIT #2                           
FLTFLAG  DS    XL1                 FLAGS ABOUT FILTER ENTRY                     
FLFVLRTN EQU   X80                  VALIDATION IS BY ROUTINE                    
FLFVLTAB EQU   X40                  VALID VALUES IN A TABLE                     
FLTRTNUM DS    XL1                 VALIDATION ROUTINE # (FLFVLRTN)              
         ORG   FLTRTNUM                                                         
FLTTBDSP DS    AL2                 VLDTN TABLE DSPL FROM T21742 CSECT           
FLTMAXN  DS    XL1                 MAX # OF FILTER DATA ALLOWED                 
FLTODSPL DS    XL2                 DISPL FROM SYSD OF OUTPUT FIELD              
FLTOLEN  DS    XL1                 OUTPUT ENTRY LENGTH                          
FLTKYTB  DS    AL2                 DSPL OF KEYWORD TABLE                        
FLTTSTBL DS    AL2                 DSPL OF FILTER TESTS TABLE                   
FLTDSCTQ EQU   *-FLTDSECT          L(FIXED PART OF TABLE)                       
FLTKYWRD DS    0C                  FILTER KEYWORDS                              
FLTFLTST DS    0C                  FILTER TESTS (SEE FTSTDSCT)                  
                                                                                
                                                                                
FTSTDSCT DSECT                                                                  
FTSTYPE  DS    XL1                 TYPE OF TEST TO PERFORM                      
FTSTYXCT EQU   1                    TEST ON EXACT MATCH                         
FTSTYLOW EQU   2                    TEST AGAINST LOW VALUE                      
FTSTYHI  EQU   3                    TEST AGAINST HI VALUE                       
FTSTYNCE EQU   4                    DO LOGCL AND & CMPRE FOR EQUAL              
FTSLCMP  DS    XL1                 LENGTH OF COMPARE                            
FTSSADDR DS    AL2                 A(FIELD/AREA) CONTAINING SPECIMEN            
FTSSDSPL DS    AL2                 DSPL INTO FIELD OF SPECIMEN                  
FTSMDSPL DS    AL2                 DSPL OF MODEL                                
FTSTDSCQ EQU   *-FTSTDSCT                                                       
                                                                                
                                                                                
FTVDATD  DSECT                                                                  
FTVDLDAT DS    XL2                 LOW  DATE                                    
FTVDHDAT DS    XL2                 HIGH DATE                                    
FTVDATQ  EQU   *-FTVDATD                                                        
                                                                                
                                                                                
FTVLDTD  DSECT                                                                  
FTVLLOCK DS    XL1                 (L)OCK / (U)NLOCK                            
FTVLLDAT DS    XL2                 LOW  DATE                                    
FTVLHDAT DS    XL2                 HIGH DATE                                    
FTVLDTQ  EQU   *-FTVLDTD                                                        
                                                                                
***********************************************************************         
         TITLE 'SPSFM42 - PROFIT WITHIN LIST OVERLAY (SPOT STRG AREA)'          
***********************************************************************         
*========================= SPOT STORAGE AREA =========================*         
                                                                                
SPOTAREA DSECT                                                                  
ACCUTBMX EQU   200                                                              
ACTBLABL DS    D                   *ACCUTB*                                     
ACCUTAB  DS    (ACCUTBMX+1)XL(ACCUTABQ)                                         
ACCUTABX EQU   *                                                                
                                                                                
ESTBLABL DS    D                   **ESTAB*                                     
ESTAB    DS    XL256                                                            
ESTABX   EQU   *                                                                
                                                                                
PWRCLABL DS    D                   *PWRECD*                                     
PWREC    DS    XL2000                                                           
PWRECX   EQU   *                                                                
                                                                                
SPBKLABL DS    D                   *SPTBLK*                                     
         PRINT OFF                                                              
       ++INCLUDE SPOTBLOCK                                                      
         PRINT ON                                                               
SPBKLEN  EQU   SBLOCKX-SBLOCK                                                   
                                                                                
                                                                                
MYTIALEN EQU   SBLOCKX-SPOTAREA                                                 
         DS    0CL((X'4800'-MYTIALEN)+1)                                        
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPSFM42   08/02/02'                                      
         END                                                                    
