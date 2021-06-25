*          DATA SET ACFIL44    AT LEVEL 067 AS OF 02/16/21                      
*PHASE T62344A                                                                  
                                                                                
         TITLE 'APPROVER RECORD (EBUYER)'                                       
*                                                                               
*YNGX 002 07JUN05 <LO01-4238> APPROVERS TO BE HELD AT PRO/JOB LEVELS            
*YNGX 003 06JUL05 <LO01-4326> ADD JOB POINTERS                                  
*YNGX 003 06JUL05 <LO01-4479> ALLOW OFFICES OF APPROVER RECORD,                 
*                 CREATE DEPARTMENT PASSIVE POINTERS                            
*YNGX 003 23AUG05 <LO01-4628> ADD 1N NON CLIENT TIME LIST AND POINTERS          
*NSHE 003 07OCT05 <LO01-4783> ADD FINANCE APPROVER AND CHANGE                   
*                 IN RECORD LAYOUT                                              
*NSHE 004 11NOV05 <LO01-4923> ADD BACK UP APPROVER TO RECORD                    
*YNGX 005 15DEC05 <LO01-4829> CHANGE TO UK STRUCTURE OF PIDRECD                 
*NSHE 006 19DEC05 <LO01-4923> FIX BUGS WITH THIS CHANGE AT LEVEL 4              
*SMAN 007 19MAY06 <1037015> BUG FIX IN NTROUT ROUTINE                           
*NSHE 008 16JUN06 UKCR00007368 BUG FIX TO PASSIVE DELETE                        
*SMAN 009 19JUN06 <LO01-5373> ADD F DCCCOUNTS, FILTERING + BUG FIX              
*TKLU 010 13SEP06 <DU01-5770> NEW JOB APPROVER OBJECT                           
*NSHE 011 18NOV06 <BR100089L> FIX ADD VALIDATION PROBLEM                        
*SMAN 012 27FEB07 <BR10213D> BUG FIX TO NUMBER FORMAT                           
*SMAN 013 21DEC06 <LO01-5946> NTRDO MACRO, ESTIMATE APPROVER,                   
*                 INVOICE APPROVER AND SUPPLIER (SV/SX) ACCOUNTS PAGE           
*TKLU     03JAN07 BUG FIX - SET COMPLETE STATUS AREA FOR PASSIVES               
*SMAN 014 07MAR07 <BR11371L> BUG FIX - DEAL WITH ABSENT SX LEDGER               
*SMAN 015 13MAR07 CHANGE HOW SV AND SV LEDGER NAMES ARE OBTAINED                
*SMAN 016 16MAR07 <BR11497L> BUG FIX TO LVL 015                                 
*TKLU 017 02APR07 <UKCR00012100> DELETE PASSIVES ON RLDEL                       
*YNGX 018 18JUN07 <LO01-6397> ENABLE OFFICE LEVEL APPROVAL                      
*                 <LO01-6368> ENABLE APPROVERS FOR TIMESHEETS                   
*                             EXPENSE CLAIMS TO BE SET SEPARATELY               
*                             AND MERGE UK AND US VERSIONS                      
*NSHE 019 13SEP07 <BR13820L> BUG FIX - GET FINAL LEDGER LENGTH                  
*TKLU 020 21NOV07 <LO01-7011> BACK UP APPROVER AS LIST FILTER                   
*         03DEC07 <UKCR00015262> SET 'SJ EXISTS' FLAG FOR ESTIM/APPR            
*TKLU 021 15APR08 <UR12257D> PURCHASE APPROVER CAN HAVE LIDEL LISTS             
*YNGX 022 17APR08 <BR17428L> BUG FIX TO LVL 015                                 
*YNGX 023 01NOV07 <LO01-6727> NEW DOWNLOAD ACTION                               
*NSHE     06AUG08 <LO01-7743> ALLOW SEPARATE CLIENT APPROVERS IN BRNOCN         
*JFOS 024 18SEP08 <BR20178L> TEST PP DELETE STS WHEN RESTORING APPROVER         
*SMAN 025 29SEP08 <OT49886L/BR13033D> BUILD LIDELDS CORRECTLY                   
*NSHE 026 03OCT08 <LO01-8219> ALLOW INTERNAL ESTIMATE APPROVERS                 
*MPEN     16JAN09 <BR22456L> STOP USER ADDING TERMINATED APPROVERS              
*SMAN 030 10FEB09 <BR22995L> DELETE APPROVER PASSIVES CORRECTLY                 
*YNGX 031 06MAY09 <LO01-8887> ADD ORDER TYPE TO OFFICE/VALUE LIST               
*YNGX 032 09JUN09 <BR10023X> BUG FIX IN DOFLT ROUNTINE                          
*MPEN 033 12JUL09 <LO01-9013> CHANGES TO EXPENDITURE TYPE CODE                  
*YNGX 034 05JUN09 <LO01-8981> REDESIGN OFFICE/VALUE PAGE                        
*YNGX 035 21SEP09 <LO01-8981> REMOVE A CODE                                     
*YNGX 036 23SEP09 <BR27526L> FIX BUG DOWNLOADING REPORT                         
*YNGX 037 23SEP09 <BR27640L> ADD MISSING COLUMNS AND CHANGE HEADINGS            
*YNGX 038 25SEP09 <BR27330L> BUG FIX CHECKING DUPLICATE PASSIVE ON P4           
*         25SEP09 <BR27683L> CHANGE MAX ENTRIES ON EACH PAGE TO 3000            
*MPEN 039 07OCT09 <BR27944L> ALLOW DUPLICATE 1R ACCOUNT                         
*JFOS 040 10NOV09 <BR28683L> REFRESH MEDIA/OFF FIELDS WHEN CPJ CHANGED          
*JFOS 042 24NOV09 <DU01-9514>ALLOW SAME TYPE/DIFF APPS+AMTS ON APPROVER         
*JFOS 043 12FEB10 <BR30976L> PADDLE UPDATES PRIME PTR STS ON ADD                
*JFOS 044 16FEB10 <LO01-9813> EXTRA APPS IN APPL COL (PAGE 4)                   
*MPEN 045 26OCT09 <LO01-9355> NEW FILTERING FOR PID                             
*MPEN     22FEB10 <LO01-9681> ALLOW USE TO REMOVE OFFICE IF CLIENT              
*MPEN/RGUP 045 20APR10 <UKCR00027708> BRING OVER US CODE AND BUG FIXES          
*YNGX 046 17MAY10 <BR15882D>  BUG FIX FOR DUPLICATE ENTRY ON PAGE 4             
*MPEN 047 20MAY10 <PR000109> ADD INTERNAL ESTIMATE APPROVER                     
*         20MAY10 <PR000349> ADD BACKUP APPROVER PAGE                           
*JFOS 049 25JAN11 <BR17244D> REINSTATE TLKSSEQ:EXISTG APP RECS NEED IT          
*                 PLUS US 1-CHAR OFFICE BUG FIX                                 
*JFOS 050 03MAY11 <BR41433L> FIX DATCON O/P TYPE FOR TERM DATE COMPARE          
*YNGX 051 31JAN11 <PR001426> ADD TLAPVAL TO TLKSRT AND REMOVE TLKSSEQ           
*MPEN 052 06JAN11 <PR001315> ALLOW 1R ON PAGE 4                                 
*CPAT 062 17NOV17 <SPEC3100> FIX SOON REQUEST FOR ORDER TYPE                    
*MPEN     24OCT18 <DSRD-20447> NEW TIMEOFF APPROVER SETTING                     
*RGUP     05MAR19 <DSRD-21513> APPROVER REPORT NOT SHOWING TIME OFF APP         
*ABID 065 11OCT19 <DSRD-23318> TIME OFF- SET BACK UP APPROVER DIFFERENT         
*                              THAN TIMESHEET BACK UP APPROVER                  
*ABID 066 05DEC19 <DSRD-23318> DSRD-23587  MF - AMEND APPROVER REC FOR          
*                              ORDERS/INVOICES TO ACCEPT OLIST/OFFICE           
*VGUP 066 11NOV20 <SPEC-34212> ADDED A VALIDATION ROUTINE FOR DOWNLOAD          
*                                                                               
FIL44    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL44**,RA,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
         LA    R1,OVROUT1                                                       
         LA    R0,OVROUT1N                                                      
         XR    RE,RE                                                            
         LR    R2,RF                                                            
         L     RF,=A(OVROU1)                                                    
         A     RF,BORELO                                                        
STRT01   STCM  RE,1,0(R1)                                                       
         STCM  RF,7,1(R1)                                                       
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,STRT01                                                        
*                                                                               
         L     RE,ATWA                                                          
         MVC   SESNL,TWASESNL-TWAD(RE)                                          
         L     R6,ATWA                                                          
         AH    R6,=Y(TWUSER-TWAD)                                               
         USING SAVED,R6                                                         
*                                                                               
         LR    RF,R2                                                            
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
         SPACE 1                                                                
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
         L     R1,CALLR1           RETURN PARAMETERS TO CALLER                  
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
EXIT     XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITSHRT MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     EXITL               EXIT WITH FIELD TOO SHORT SET                
EXITLONG MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               EXIT WITH FIELD TOO LONG SET                 
EXITIETC MVC   FVMSGNO,=AL2(AE$INETY)                                           
         B     EXITL               EXIT WITH INVALID EXP TYPE SET               
EXITIOFF MVC   FVMSGNO,=AL2(AE$EXPNV) EXPENDITURE TYPE NOT VALID FOR            
         B     EXITL               EXIT WITH INV EXP TYPE FOR OFF SET           
*                                                                               
EXITIACT MVC   FVMSGNO,=AL2(AE$IACTS)   INVALID ACTION FOR THIS SCREEN          
         LH    RF,GSDSPACT              SET CURSOR TO ACTION FIELD              
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
         B     EXITL                                                            
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
INIT     GOTO1 VDICTAT,BOPARM,C'LL  ',DCLSTL,DSLSTL  LOWER CASE                 
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLSTU,DSLSTU  UPPER CASE                 
*&&US*&& MVI   WRNIND,0                                                         
*                                                                               
T        USING CPYRECD,IOKEY                                                    
         MVC   T.CPYKEY,BCSPACES   GET SJ CLI/PRO/JOB LENGTH                    
         MVC   T.CPYKCPY,CUABIN                                                 
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO1                                          
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                MISSING COMPANY RECORD                       
                                                                                
         L     R4,AIO1             LOCATE COMPANY ELEMENT                       
         LA    R4,CPYRFST-CPYRECD(R4)                                           
         USING CPXELD,R4                                                        
         XR    R0,R0                                                            
INIT02   CLI   CPXEL,0             TEST EOR                                     
         BE    INIT04                                                           
         CLI   CPXEL,CPXELQ        TEST COMPANY EXTRA ELEMENT                   
         BE    *+14                                                             
         IC    R0,CPXLN                                                         
         AR    R4,R0                                                            
         B     INIT02                                                           
         MVC   SVCPXST6,CPXSTAT6                                                
         DROP  R4                                                               
*                                                                               
         USING LDGTABD,R4                                                       
T        USING LDGRECD,IOKEY                                                    
INIT04   MVC   T.LDGKEY,BCSPACES   GET SJ CLI/PRO/JOB LENGTH                    
         MVC   T.LDGKCPY,CUABIN                                                 
         MVC   T.LDGKUNT(L'PRDUL),PRDUL                                         
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                NO SJ LEDGER?                                
         ICM   R4,15,ACALDG                                                     
         MVC   CLILEN(L'CLILEN+L'PROLEN+L'JOBLEN),LDGTLVA                       
*                                                                               
         MVC   T.LDGKEY,BCSPACES   GET 1R LENGTHS                               
         MVC   T.LDGKCPY,CUABIN                                                 
         MVC   T.LDGKUNT(L'ONERUL),ONERUL                                       
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                NO 1R LEDGER?                                
         ICM   R4,15,ACALDG                                                     
         MVC   LEN1RA(LEN1RLNQ),LDGTLVA                                         
         DROP  T,R4                                                             
*                                                                               
         CLI   LEN1RA,L'ACTKACT    ONLY SINGLE LEVEL OF 1R!!                    
         BNE   *+12                                                             
         MVI   LEN1RLOW,L'ACTKACT                                               
         B     INIT08                                                           
*                                                                               
         LA    RF,LEN1RD                                                        
         CLI   0(RF),0                                                          
         BNE   *+12                                                             
         SHI   RF,1                                                             
         B     *-12                                                             
         SHI   RF,1                                                             
         MVC   LEN1RLOW,0(RF)      LENGTH OF LOWEST LEVEL FOR 1R                
*                                                                               
INIT08   CLI   CSACT,A#DLOAD       DOWNLOAD?                                    
         BNE   *+8                                                              
         MVI   WHENOK,WHENOV+WHENSOON          NOTIFY VALID INPUTS              
*                                                                               
         NI    GCINDS3,FF-GCIRCHG                                               
         OI    GCINDS3,GCIPNORM               OVERRIDE SHOW/HIDE                
         LH    RF,GSDSPPAG                                                      
         A     RF,ATWA                                                          
         NI    FVATRB-FVIHDR(RF),FF-FVAPROT   AND UNPROTECT                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDING           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
*                                                                               
ITER02   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITERH                                                            
*                                                                               
TABLEOO  DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(ODLOAD),AL1(0,0,0),AL4(DLOAD)                                
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SCREEN OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(SCREEN)                                                        *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCREEN   LM    R0,R3,SVPARMS                                                    
         LA    RF,TABLSCR                                                       
         B     ITER                                                             
*                                                                               
TABLSCR  DC    AL1(SKSET),AL1(0,0,0),AL4(SCRKSET)                               
         DC    AL1(SSET),AL1(0,0,0),AL4(SCRMSET)                                
         DC    AL1(SLSET),AL1(0,0,0),AL4(SCRLSET)                               
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* SET KEY SCREEN                                                      *         
***********************************************************************         
         SPACE 1                                                                
SCRKSET  DS    0H                                                               
         MVI   GSSKCODE,0                                                       
*                                                                               
         CLI   CSACT,A#DLOAD       DOWNLOAD?                                    
         BNE   EXITOK                                                           
*                                                                               
         TM    BCCPYST1,CPYSOROE   ARE WE ON OFFICE BASED AGENCY                
         BO    EXITOK                                                           
         MVI   GSSKCODE,C'1'       CHANGE KEY SCREEN CODE                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET MAINTENANCE DATA SCREEN                                         *         
***********************************************************************         
         SPACE 1                                                                
SCRMSET  DS    0H                                                               
         MVI   GSSMCODE,C'A'       ** TESTING ONLY **                           
         MVI   GSSMCODE,0          CHANGE TO '0' BEFORE MAKING IT LIVE          
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BO    EXITOK              YES - OK                                     
         MVI   GSSMCODE,C'1'       CHANGE MAINTENANCE PAGE CODE                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET LIST SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLSET  DS    0H                                                               
*        MVI   GSSLCODE,C'A'       ** TESTING ONLY **                           
         MVI   GSSLCODE,0          CHANGE TO '0' BEFORE MAKING IT LIVE          
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BO    EXITOK              YES - OK                                     
         CLI   CSACT,A#LST                                                      
         BNE   EXITOK                                                           
         MVI   GSSLCODE,C'1'       CHANGE LIST PAGE CODE                        
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* KEY OBJECT                                                          *         
* ----------                                                          *         
* SVPARMS1 HOLDS EQUATED OBJECT                                       *         
* SVPARMS2 HOLDS EQUATED VERB                                         *         
* SVPARMS3 A(KEY)                                                     *         
* SVPARMS4 HOLDS SUB-ACTION                                           *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R1,R2,SVPARMS2                                                   
         USING APPRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ    APPROVER RECORD TYPE                         
         MVI   APPKSUB,APPKSUBQ    AND SUBTYPE                                  
         MVC   APPKCPY,CUABIN      CONNECTED USER                               
*                                                                               
         XC    SVFLTS(SVFLTLQ),SVFLTS FOR DOWNLOADING                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    APPKEY,APPKEY                                                    
         MVI   APPKTYP,APPKTYPQ    APPROVER RECORD TYPE                         
         MVI   APPKSUB,APPKSUBQ    AND SUBTYPE                                  
         MVC   APPKCPY,CUABIN                                                   
*                                                                               
         XC    SVFLTS(SVFLTLQ),SVFLTS                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** LAST TIME FOR KEY OBJECT ***                  
*                                 ------------------------                      
KLTABL   DC    AL1(KFVAL),AL1(0,0,0),AL4(KLKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  XC    CDOPTION,CDOPTION                                                
         GOTO1 AVALDOPT,0                                                       
         BL    EXITL                                                            
         CLC   CDOPTION,SDOPTION   TEST DELETE OPTION CHANGED                   
         BE    EXITOK                                                           
         MVI   LSSCIND1,LSSCIFLT   REFRESH LIST                                 
         MVC   SDOPTION(SDOPTSL),CDOPTION                                       
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* RECORD OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R0,R3,SVPARMS                                                    
         USING APPRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT                                        *         
***********************************************************************         
         SPACE 1                                                                
RECFRST  L     R1,SVPARMS4                                                      
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RFTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD                                  *         
***********************************************************************         
         SPACE 1                                                                
RFADD    CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
         CLI   GSSMPAGE,1          PAGE 1?                                      
         BNE   EXITIACT            NO - INVALID ACTION FOR THIS SCREEN          
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    MVC   FVADDR,APIDFLD                                                   
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE                                                                  
RECLAST  L     R1,SVPARMS4                                                      
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE                                                                  
RLADD    DS    0H                                                               
         GOTO1 AADDPAS,BOPARM,APPRECD                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE                                                                  
T        USING APPRECD,R4                                                       
RLDEL    DS    0H                                                               
         GOTO1 ADELPAS,BOPARM,APPRECD                                           
         MVC   IOKEY(L'APPKEY),APPKEY                                           
*                                                                               
RLDEL10  LA    R4,IOKEY            READ NEXT APPROVER SUB-RECORD                
         LLC   RF,T.APPKSEQ        THEY ARE ALWAYS IN SEQUENCE                  
         AHI   RF,1                                                             
         STC   RF,T.APPKSEQ                                                     
*                                                                               
         L     R1,=AL4(XORDUP+XOACCDIR+XIO2)                                    
         GOTO1 AIO                                                              
         BNE   EXITOK              DELETE OR NO FOUND - END                     
*                                                                               
         OI    T.APPKSTAT,APPSDELT DELETE APPROVER DIR                          
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         OI    T.APPRSTAT,APPSDELT DELETE MASTER RECORD                         
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
*                                                                               
         GOTO1 ADELPAS,BOPARM,AIO2                                              
         B     RLDEL10                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE                               *         
***********************************************************************         
         SPACE                                                                  
T        USING APPRECD,R4                                                       
RLRES    DS    0H                                                               
         GOTO1 AADDPAS,BOPARM,APPRECD                                           
*                                                                               
         MVC   IOKEY(L'APPKEY),APPKEY                                           
RLRES10  LA    R4,IOKEY            READ NEXT APPROVER SUB-RECORD                
         LLC   RF,T.APPKSEQ        THEY ARE ALWAYS IN SEQUENCE                  
         AHI   RF,1                                                             
         STC   RF,T.APPKSEQ                                                     
*                                                                               
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)                                   
         GOTO1 AIO                                                              
         TM    IOERR,IOERRS-(IOEDEL+IOERNF)                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOERNF        RECORD NOT FOUND - END                       
         BO    EXITOK                                                           
*                                                                               
         NI    T.APPKSTAT,FF-APPSDELT     RESTORE IT                            
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         NI    T.APPRSTAT,FF-APPSDELT                                           
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
*                                                                               
         GOTO1 AADDPAS,BOPARM,AIO2                                              
         B     RLRES10                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE                                                                  
T        USING APPRECD,R4                                                       
RLWRT    DS    0H                                                               
*                                                                               
         GOTO1 AADDPAS,BOPARM,APPRECD                                           
         MVC   IOKEY(L'APPKEY),APPKEY                                           
*                                                                               
RLWRT04  LA    R4,IOKEY            READ NEXT APPROVER SUB-RECORD                
         LLC   RF,T.APPKSEQ        THEY ARE ALWAYS IN SEQUENCE                  
         AHI   RF,1                                                             
         STC   RF,T.APPKSEQ                                                     
*                                                                               
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK              DELETE OR NO FOUND - END                     
*                                                                               
         GOTO1 AADDPAS,BOPARM,AIO2                                              
         B     RLWRT04                                                          
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                               
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION         *         
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                  *         
* P3 BYTES 1-3 HOLDS EQUATED ACTION VERB                              *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
*                                                                     *         
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING APPRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS3         GET GLOBAL VERB                              
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA04   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(,RF)                                                   
         B     DATA04                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING APPRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
*        BR    RF                                                               
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                         *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(F#APP#APPIDL),AL4(PIL)  PERSON ID IN LIST                    
         DC    AL2(F#APP#APFNML),AL4(PFNL) PERSON FIRST NAME LIST               
         DC    AL2(F#APP#APLNML),AL4(PLNL) PERSON LAST NAME LIST                
         DC    AL2(F#APP#APPNMD),AL4(APND) APPROVER NAME (DOWNLOAD)             
         DC    AL2(F#APP#APPID),AL4(PID)   PERSON ID                            
         DC    AL2(F#APP#APFNM),AL4(PFN)   PERSON FIRST NAME                    
         DC    AL2(F#APP#APLNM),AL4(PLN)   PERSON LAST NAME                     
         DC    AL2(F#APP#JOBAP),AL4(JAP)   JOB APPROVER                         
         DC    AL2(F#APP#JOBAD),AL4(JAP)   JOB APPROVER (DOWNLOAD)              
         DC    AL2(F#APP#FINAP),AL4(FAP)   FINANCE APPROVER                     
         DC    AL2(F#APP#FINAD),AL4(FAP)   FINANCE APPROVER (DOWNLOAD)          
         DC    AL2(F#APP#ESTAP),AL4(EST)   ESTIMATE APPROVER                    
         DC    AL2(F#APP#ESTAD),AL4(EST)   ESTIMATE APPROVER (DOWNLOAD)         
         DC    AL2(F#APP#IEST),AL4(IEST)   INTERNAL ESTIMATE APPROVER           
         DC    AL2(F#APP#IESTD),AL4(IEST)  INTERNAL ESTIMATE (DOWNLOAD)         
         DC    AL2(F#APP#FAPL),AL4(FAPL)   APPROVAL LIMIT FILTER                
         DC    AL2(F#APP#FAPLD),AL4(FAPL)  APPROVAL LIMIT (DOWNLOAD)            
         DC    AL2(F#APP#FCPJ),AL4(FCPJ)   CLI/PRO/JOB CODE FILTER              
         DC    AL2(F#APP#FETC),AL4(FETC)   EXPENDITURE CODE FILTER              
         DC    AL2(F#APP#FODC),AL4(FODC)   OFF/DEPT CODE FILTER                 
         DC    AL2(F#APP#FDPTC),AL4(FODC)  DEPARTMENT CODE FILTER               
         DC    AL2(F#APP#FNCC),AL4(FNCC)   NON-CLIENT CODE FILTER               
         DC    AL2(F#APP#F1RC),AL4(F1RC)   COSTING 1R CODE FILTER               
         DC    AL2(F#APP#FPSC),AL4(FPSC)   PROD/SUPP CODE FILTER                
         DC    AL2(F#APP#FMED),AL4(FMED)   MEDIA CODE FILTER                    
         DC    AL2(F#APP#FMEDD),AL4(FMED)  MEDIA CODE FILTER (DOWNLOAD)         
         DC    AL2(F#APP#APCPJ),AL4(CPJ)   CLI/PRO/JOB CODE LIST                
         DC    AL2(F#APP#APCMED),AL4(CMED) CLI/PRO/JOB MEDIA LIST               
         DC    AL2(F#APP#APCOFF),AL4(COFF) CLI/PRO/JOB OFFICE LIST              
         DC    AL2(F#APP#APCPJN),AL4(CPJN) CLI/PRO/JOB NAME LIST                
         DC    AL2(F#APP#APNCC),AL4(NCC)   NON-CLIENT TIME CODE LIST            
         DC    AL2(F#APP#APNCN),AL4(NCN)   NON-CLIENT TIME NAME LIST            
         DC    AL2(F#APP#AP1RC),AL4(C1R)   COSTING 1R CODE LIST                 
         DC    AL2(F#APP#AP1RN),AL4(N1R)   COSTING 1R NAME LIST                 
         DC    AL2(F#APP#BUFNM),AL4(BFN)   BACK UP APP FIRST NAME               
         DC    AL2(F#APP#BULNM),AL4(BLN)   BACK UP APP LAST NAME                
         DC    AL2(F#APP#BAKPID),AL4(BAP)  BACK UP APPROVER PID                 
         DC    AL2(F#APP#BUAPDL),AL4(BAPL) BACK UP APPR PID (DOWNLOAD)          
         DC    AL2(F#APP#BUAPD),AL4(BAP)   BACK UP APP PID (DOWN FILT)          
         DC    AL2(F#APP#APPBAK),AL4(BAPP) BACKUP APPROVER APPL                 
         DC    AL2(F#APP#BAKAPD),AL4(BAPP) BACKUP APPROVER APPL (DOWN)          
         DC    AL2(F#APP#BUANMD),AL4(BAND) BACK UP APPR NAME (DOWNLOAD)         
         DC    AL2(F#APP#APPLI),AL4(APP)   APPLICATION 1R APPROVAL              
         DC    AL2(F#APP#APSJA),AL4(SJA)   APPLICATION SJ APPROVAL              
         DC    AL2(F#APP#TIME),AL4(ONA)    APPLICATION 1N APPROVAL              
         DC    AL2(F#APP#APOTY),AL4(OTY)   APPROVER ORDER TYPE LIST             
         DC    AL2(F#APP#APAPL),AL4(APL)   APPROVER APPLICATION LIST            
         DC    AL2(F#APP#APPSC),AL4(PSC)   PROD/SUPPLIER CODE LIST              
         DC    AL2(F#APP#APODC),AL4(ODC)   OFF/DEPT CODE LIST                   
         DC    AL2(F#APP#APODN),AL4(ODN)   OFF/DEPT NAME (DOWNLOAD)             
         DC    AL2(F#APP#APMED),AL4(MED)   MEDIA CODE LIST                      
         DC    AL2(F#APP#APETC),AL4(ETC)   ETYPE CODE LIST                      
         DC    AL2(F#APP#APETN),AL4(ETN)   ETYPE NAME (DOWNLOAD)                
         DC    AL2(F#APP#APPSN),AL4(PSN)   PROD/SUPPLIER (DOWNLOAD)             
         DC    AL2(F#APP#APVAL),AL4(APV)   APPROVAL VALUE LIST                  
         DC    AL2(F#APP#APSEL),AL4(SEL)   SELF APPROVAL VALUE                  
         DC    AL2(F#APP#ESTFD),AL4(ESTD)  ESTIMATE APPROVER (DOWNLOAD)         
         DC    AL2(F#APP#EXPFD),AL4(FIND)  EXPENSE APPROVER (DOWNLOAD)          
         DC    AL2(F#APP#JOBFD),AL4(JOBD)  JOB APPROVER (DOWNLOAD)              
         DC    AL2(F#APP#IESTFD),AL4(IESD) INTERNAL ESTIMATE APPR (DL)          
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
         SPACE 2                                                                
***********************************************************************         
* MACRO BRANCH TO DATA OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
         MACRO                                                                  
&NTRDO   NTRDO                                                                  
         DC    CL8'&NTRDO'                                                      
         DS    0H                                                               
         USING *,RF                                                             
&NTRDO   NTR1                                                                   
         DROP  RF                                                               
         LR    R7,RF                                                            
         USING &NTRDO,R7                                                        
         LA    RF,&NTRDO.TBL       TABLE OF KNOWN VERBS                         
         B     ITER                                                             
         MEND                                                                   
         SPACE 1                                                                
FIL44    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   CLI   CSACT,A#DLOAD       DOWNLOAD?                                    
         BNE   EXITOK                                                           
         XC    SVPIDBIN,SVPIDBIN                                                
         MVC   SVPIDD(SVPIDLQ),BCSPACES                                         
         XC    SVBUPIDB,SVBUPIDB                                                
         MVC   SVBUPIDD(SVBUPILQ),BCSPACES                                      
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   DS    0H                                                               
         NI    APPINDS,FF-(APPIVCPJ+APPICCPJ)                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3                                                      
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
*&&US*&& DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
*&&US                                                                           
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
DLDDIS   DS    0H                                                               
         TM    WRNIND,WRNTRMD                                                   
         BZ    EXITOK                                                           
         NI    WRNIND,X'FF'-WRNTRMD                                             
         MVC   FVMSGNO,=AL2(AE$DTTRM) DATE IS AFTER TERMINATION DATE            
         OI    GCINDS3,GCIDSMSG    SET OWN DISPLAY MESSAGE                      
         MVI   FVOSYS,QSACC                                                     
         B     EXITOK                                                           
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
DLDVAL   DS    0H                                                               
         CLM   R2,B'0111',ATLST+1  ARE WE PROCESSING MAINTENANCE LIST?          
         BNE   EXITOK              NO - EXIT                                    
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMSQ                                                      
*&&UK*&& BNH   DLDV04                                                           
*&&US*&& BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     DLDV02                                                           
*&&US                                                                           
         CLI   GSSMPAGE,BAKLISTQ   ARE WE ON BACKUP APPROVER PAGE               
         BNE   DLDV04                                                           
         CHI   RF,10                                                            
         BL    DLDV04                                                           
         MVC   FVMSGNO,=AL2(AE$BAMAX) ONLY 10 BACKUP APPROVERS ALLOWED.         
*&&                                                                             
DLDV02   LH    R0,LSCURLIN                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR     SET CURSOR TO CURRENT LIST LINE               
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
* CHECK DEFAULT DOESN'T ALREADY EXIST ON ANOTHER APPROVER                       
*                                                                               
DLDV04   TM    LSLNIND1,LSLNIDEL           LINE DELETE REQUESTED?               
         BO    EXITOK                      YES - OK                             
         TM    LSLNIND1,LSLNIINP+LSLNIRED  LINE HAS CHANGED                     
         BZ    EXITOK                      NO - FINE                            
         XC    MYWORK,MYWORK                                                    
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE?           
         BE    DLDV10                                                           
         CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R LIST PAGE?              
         BE    DLDV20                                                           
         CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT LIST PAGE?              
         BE    DLDV30                                                           
         CLI   GSSMPAGE,APPLISTQ   ARE WE ON APPLICATION LIST PAGE?             
         BE    DLDV40                                                           
         B     EXITOK              NO - NOTHING TO DO                           
*                                                                               
DLDV10   DS    0H                                                               
         OC    TLKASJAC,BCSPACES                                                
         OC    TLKASJME,BCSPACES                                                
         OC    TLKASJOF,BCSPACES                                                
                                                                                
         CLC   TLKASJAC,BCSPACES                                                
         BH    DLDV14                                                           
         CLC   TLKASJME,BCSPACES                                                
         BH    DLDV14                                                           
         CLC   TLKASJOF,BCSPACES                                                
         BH    DLDV14                                                           
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
T        USING JOBPASD,IOKEY                                                    
DLDV14   XC    T.JOBPAS,T.JOBPAS                                                
         MVI   T.JOBPTYP,JOBPTYPQ                                               
         MVI   T.JOBPSUB,JOBPSUBQ                                               
         MVC   T.JOBPCPY,CUABIN    CONNECTED ID                                 
         MVI   T.JOBPAPPL,0                                                     
         MVI   T.JOBPVIEW,JOBPVOFF VIEW TYPE - OFF/CLI/PRO/JOB/MEDIA            
         MVC   T.JOBPCOFF,TLKASJOF CLIENT OFFICE                                
         MVC   T.JOBPCPJ,TLKASJAC  CLIENT PROD JOB                              
         MVC   T.JOBPCMED,TLKASJME CLIENT MEDIA CODE                            
         MVC   SVIOKEY,IOKEY                                                    
*                                                                               
         TM    TLASTAT,TLASTIM     TIME APPLICATION ?                           
         BZ    *+12                                                             
         MVI   T.JOBPAPPL,JOBPATIM                                              
         BAS   RE,CDEFSJ                                                        
         TM    TLASTAT,TLASEXP     EXPENSE APPLICATION ?                        
         BZ    *+12                                                             
         MVI   T.JOBPAPPL,JOBPAEXP                                              
         BAS   RE,CDEFSJ                                                        
         TM    TLASTAT,TLASESTD    ESTIMATE APPLICATION - DEFAULT               
         BZ    *+12                                                             
         MVI   T.JOBPAPPL,JOBPAEST                                              
         BAS   RE,CDEFSJ                                                        
         TM    TLASTAT,TLASJOBD    JOB APPLICATION - DEFAULT                    
         BZ    *+12                                                             
         MVI   T.JOBPAPPL,JOBPAJOB                                              
         BAS   RE,CDEFSJ                                                        
         TM    TLASTA2,TLASESID    INTERNAL ESTIMATE - DEFAULT                  
         BZ    *+12                                                             
         MVI   T.JOBPAPPL,JOBPAESI                                              
         BAS   RE,CDEFSJ                                                        
         TM    TLASTA2,TLAEL1B     EXPENSES LEVEL 1 BILLABLE                    
         BZ    *+12                                                             
         MVI   T.JOBPAPPL,JOBPAX1B                                              
         BAS   RE,CDEFSJ                                                        
         TM    TLASTA2,TLAEL1N     EXPENSES LEVEL 1 NON-BILLABLE                
         BZ    *+12                                                             
         MVI   T.JOBPAPPL,JOBPAX1N                                              
         BAS   RE,CDEFSJ                                                        
         TM    TLASTA2,TLAEL2B     EXPENSES LEVEL 2 BILLABLE                    
         BZ    *+12                                                             
         MVI   T.JOBPAPPL,JOBPAX2B                                              
         BAS   RE,CDEFSJ                                                        
         TM    TLASTA2,TLAEL2N     EXPENSES LEVEL 2 NON-BILLABLE                
         BZ    *+12                                                             
         MVI   T.JOBPAPPL,JOBPAX2N                                              
         BAS   RE,CDEFSJ                                                        
         B     EXITOK                                                           
         DROP  T                                                                
*                                                                               
T        USING DPAPASD,IOKEY                                                    
DLDV20   XC    T.DPAPAS,T.DPAPAS                                                
         MVI   T.DPAPTYP,DPAPTYPQ                                               
         MVI   T.DPAPSUB,DPAPSUBQ                                               
         MVC   T.DPAPCPY,CUABIN    CONNECTED ID                                 
         MVI   T.DPAPAPPL,0                                                     
         ZAP   T.DPAPXVAL,BCPZERO                                               
         MVC   T.DPAP1RAC,TLKA1RAC 1R ACCOUNT CODE                              
         MVC   SVIOKEY,IOKEY                                                    
*                                                                               
         TM    TLAPDTY,LIDAPDTI    TIME APPLICATION ?                           
         BZ    *+12                                                             
         MVI   T.DPAPAPPL,DPAPATIM                                              
         BAS   RE,CDEF1R                                                        
         TM    TLAPDTY,LIDAPDEX    EXPENSE APPLICATION ?                        
         BZ    *+12                                                             
         MVI   T.DPAPAPPL,DPAPAEXP                                              
         BAS   RE,CDEF1R                                                        
         TM    TLAPDTY,LIDAPDED    DEFAULT FINANCE?                             
         BZ    *+12                                                             
         MVI   T.DPAPAPPL,DPAPAEXF                                              
         BAS   RE,CDEF1R                                                        
         TM    TLAPDTY,LIDAPDE1    EXPENSE CLAIMS LVL 1 APPR NON BILL           
         BZ    *+12                                                             
         MVI   T.DPAPAPPL,DPAPAEX1                                              
         BAS   RE,CDEF1R                                                        
         TM    TLAPDTY,LIDAPDE2    EXPENSE CLAIMS LVL 2 APPR NON BILL           
         BZ    *+12                                                             
         MVI   T.DPAPAPPL,DPAPAEX2                                              
         BAS   RE,CDEF1R                                                        
         TM    TLAPDTY,LIDAPDB1    EXPENSE CLAIMS LVL 1 APPR BILLABLE           
         BZ    *+12                                                             
         MVI   T.DPAPAPPL,DPAPAEB1                                              
         BAS   RE,CDEF1R                                                        
         TM    TLAPDTY,LIDAPDB2    EXPENSE CLAIMS LVL 2 APPR BILLABLE           
         BZ    *+12                                                             
         MVI   T.DPAPAPPL,DPAPAEB2                                              
         BAS   RE,CDEF1R                                                        
         TM    TLAPDT2,TLAPDTO     TIMEOFF                                      
         BZ    *+12                                                             
         MVI   T.DPAPAPPL,DPAPATIO                                              
         BAS   RE,CDEF1R                                                        
         B     EXITOK                                                           
         DROP  T                                                                
*                                                                               
T        USING NCTPASD,IOKEY                                                    
DLDV30   TM    TLASTAT,LIDATIME    CHECK IT'S TIME                              
         BZ    EXITOK                                                           
         XC    T.NCTPAS,T.NCTPAS                                                
         MVI   T.NCTPTYP,NCTPTYPQ                                               
         MVI   T.NCTPSUB,NCTPSUBQ                                               
         MVC   T.NCTPCPY,CUABIN    CONNECTED ID                                 
         MVI   T.NCTPAPPL,NCTPATIM                                              
         MVC   T.NCTPNCC,TLKA1NAC                                               
         LHI   R1,XOACCDIR+XOHIGH+XIO2                                          
         B     *+8                                                              
DLDV34   LHI   R1,XOACCDIR+XOSEQ+XIO2                                           
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(NCTPPIDB-NCTPASD),IOKEY                                 
         BNE   EXITOK                                                           
         CLC   SVPIDBIN,T.NCTPPIDB                                              
         BE    DLDV34                                                           
         MVC   MYWORK(L'NCTPPIDB),T.NCTPPIDB                                    
         B     DLDVERR             DEFAULT EXISTS FOR ANOTHER APPROVER          
         DROP  T                                                                
*                                                                               
DLDV40   DS    0H                                                               
         OC    TLKAPOFF,BCSPACES                                                
         OC    TLKAPDPT,BCSPACES                                                
         OC    TLKAPETY,BCSPACES                                                
         OC    TLKAPMED,BCSPACES                                                
         OC    TLKAPACA,BCSPACES                                                
         OC    TLAPVAL,TLAPVAL                                                  
         BNZ   *+10                                                             
         ZAP   TLAPVAL,BCPZERO                                                  
         OC    TLAPSEL,TLAPSEL                                                  
         BNZ   *+10                                                             
         ZAP   TLAPSEL,BCPZERO                                                  
*                                                                               
T        USING APPPASD,IOKEY                                                    
         XC    T.APPPAS,T.APPPAS                                                
         MVI   T.APPPTYP,APPPTYPQ                                               
         MVI   T.APPPSUB,APPPSUBQ                                               
         MVC   T.APPPCPY,CUABIN   CONNECTED ID                                  
         MVI   T.APPPCAT,0                                                      
         MVC   T.APPPSCAT,TLKAPOTY                                              
         MVC   T.APPPVAL,TLAPVAL                                                
         MVC   T.APPPOFFC,TLKAPOFF                                              
         MVC   T.APPPDEPT,TLKAPDPT                                              
         MVC   T.APPPETYP,TLKAPETY                                              
         MVC   T.APPPMED,TLKAPMED                                               
         MVC   T.APPPSEL,TLAPSEL                                                
         MVC   T.APPPACLA,TLKAPACA                                              
         MVC   SVIOKEY,IOKEY                                                    
*                                                                               
         TM    TLAPTYP,LIDAPORD                                                 
         BZ    *+12                                                             
         MVI   T.APPPCAT,APPPORD   ORDER APPROVER                               
         BAS   RE,CDEFAP                                                        
         TM    TLAPTYP,LIDAPOFD                                                 
         BZ    DLDV42                                                           
         MVI   T.APPPCAT,APPPORDF  ORDERS FINANCE APPROVER                      
         ZAP   T.APPPVAL,BCPZERO                                                
         BAS   RE,CDEFAP                                                        
DLDV42   TM    TLAPTYP,LIDAPIND                                                 
         BZ    *+12                                                             
         MVI   T.APPPCAT,APPPINV   INVOICE APPROVER                             
         BAS   RE,CDEFAP                                                        
*                                                                               
         TM    TLAPTYP,LIDAPESD                                                 
         BZ    *+12                                                             
         MVI   T.APPPCAT,APPPEST   ESTIMATES APPROVER                           
         BAS   RE,CDEFAP                                                        
         TM    TLAPTY2,LIDAPOAD                                                 
         BZ    *+12                                                             
         MVI   T.APPPCAT,APPPAEO   APPROVED EST ORDERS APPROVER                 
         BAS   RE,CDEFAP                                                        
         TM    TLAPTY2,LIDAPOUD                                                 
         BZ    *+12                                                             
         MVI   T.APPPCAT,APPPUEO   UNAPPROVED EST ORDERS APPROVER               
         BAS   RE,CDEFAP                                                        
         TM    TLAPTY3,LIDAINED                                                 
         BZ    *+12                                                             
         MVI   T.APPPCAT,APPPIEA   INTERNAL ESTIMATE APPROVER                   
         BAS   RE,CDEFAP                                                        
         TM    TLAPTY3,LIDAEXCD                                                 
         BZ    *+12                                                             
         MVI   T.APPPCAT,APPPEXPS  EXPENSES APPROVER                            
         BAS   RE,CDEFAP                                                        
*                                                                               
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
*                                                                               
* CHECK DUPLICATED SJ ACCOUNT  EXIST ON ANOTHER APPROVER                        
*                                                                               
T        USING JOBPASD,IOKEY                                                    
CDEFSJ   ST    RE,SAVERE                                                        
         LHI   R1,XOACCDIR+XOHIGH+XIO2                                          
         B     *+8                                                              
CDEFSJ04 LHI   R1,XOACCDIR+XOSEQ+XIO2                                           
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(JOBPPIDB-JOBPASD),IOKEY                                 
         BNE   CDEFSJOK                                                         
         CLC   T.JOBPPIDB,SVPIDBIN                                              
         BE    CDEFSJ04                                                         
         CLI   T.JOBPAPPL,JOBPAEST CHECK DEFAULT ESTIMATE                       
         BE    CDEFSJ08                                                         
         CLI   T.JOBPAPPL,JOBPAJOB CHECK DEFAULT JOB                            
         BE    CDEFSJ08                                                         
         CLI   T.JOBPAPPL,JOBPAESI CHECK DEFAULT INTERNAL ESTIMATE              
         BE    CDEFSJ08                                                         
         B     *+12                                                             
CDEFSJ08 TM    T.JOBPSTAT,JOBPDFLT ONLY ONE DEFAULT APPROVER                    
         BZ    CDEFSJ04                                                         
         MVC   MYWORK(L'JOBPPIDB),T.JOBPPIDB                                    
         B     DLDVERR             DEFAULT EXISTS FOR ANOTHER APPROVER          
         DROP  T                                                                
*                                                                               
CDEFSJOK MVC   IOKEY,SVIOKEY       RESTORE IOKEY                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
* CHECK DUPLICATED 1R ACCOUNT  EXIST ON ANOTHER APPROVER                        
*                                                                               
T        USING DPAPASD,IOKEY                                                    
CDEF1R   ST    RE,SAVERE                                                        
         LHI   R1,XOACCDIR+XOHIGH+XIO2                                          
         B     *+8                                                              
CDEF1R04 LHI   R1,XOACCDIR+XOSEQ+XIO2                                           
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(DPAPPIDB-DPAPASD),IOKEY                                 
         BNE   CDEF1ROK                                                         
         CLC   T.DPAPPIDB,SVPIDBIN                                              
         BE    CDEF1R04                                                         
         CLI   T.DPAPAPPL,DPAPAEXF DEFAULT FINANCE?                             
         BNE   *+12                                                             
         TM    T.DPAPSTAT,DPAPDFLT DEFAULT FINANCE EXIST?                       
         BZ    CDEF1R04            NO - GET NEXT                                
         MVC   MYWORK(L'DPAPPIDB),T.DPAPPIDB                                    
         B     DLDVERR             DEFAULT EXISTS FOR ANOTHER APPROVER          
         DROP  T                                                                
*                                                                               
CDEF1ROK MVC   IOKEY,SVIOKEY       RESTORE IOKEY                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
         SPACE 2                                                                
*                                                                               
* CHECK DUPLICATED APPLICATION EXIST ON ANOTHER APPROVER                        
*                                                                               
T        USING APPPASD,IOKEY                                                    
CDEFAP   ST    RE,SAVERE                                                        
         LHI   R1,XOACCDIR+XOHIGH+XIO2                                          
         B     *+8                                                              
CDEFAP04 LHI   R1,XOACCDIR+XOSEQ+XIO2                                           
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(APPPPIDB-APPPASD),IOKEY                                 
         BNE   CDEFAPOK                                                         
         CLC   T.APPPPIDB,SVPIDBIN                                              
         BE    CDEFAP04                                                         
         TM    T.APPPSTAT,APPPDFLT                                              
         BZ    CDEFAP04                                                         
         MVC   MYWORK(L'APPPPIDB),T.APPPPIDB                                    
         B     DLDVERR             DEFAULT EXISTS FOR ANOTHER APPROVER          
                                                                                
         DROP  T                                                                
*                                                                               
CDEFAPOK MVC   IOKEY,SVIOKEY       RESTORE IOKEY                                
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
DLDVERR  GOTOX ('GETPID',AGROUTS),MYWORK                                        
         MVC   FVXTRA(L'SVPID),BCWORK                                           
         MVC   FVMSGNO,=AL2(AE$DFEAP)                                           
         LH    RF,LSCURLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO 1ST INPUT FIELD                
         B     EXITL               DEFAULT EXISTS FOR ANOTHER APPROVER          
         DROP  R2                                                               
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DOWNLOAD OBJECT - LEVEL 1 (NEED 2 LEVELS FOR NTRDO)                 *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
DLOAD    DS    0H                                                               
         L     RF,=AL4(DOWN)       SHORTCUT TO LEVEL 2                          
         A     RF,BORELO                                                        
         LM    R0,R3,SVPARMS                                                    
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 2                                                                
FIL44N   CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR PERSON ID CODE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PID      NTRDO                                                                  
*                                                                               
PIDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPID)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISPID)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETPID)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPID)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAAPID)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPID)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPID)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHPID)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT PERSON ID CODE AFTER SELECT FROM LIST                     *         
***********************************************************************         
         SPACE 1                                                                
DSETPID  DS    0H                                                               
         B     FLTXX               RETURN 'NOT VALID' TO FORCE UNPROT           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PERSON ID CODE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISPID   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'SVPID),SVPID                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PERSON ID CODE                                           *         
***********************************************************************         
         SPACE 1                                                                
VAAPID   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    VFLTPID             YES - USE VALIDATE FILTER FIELD              
*                                                                               
         MVC   APIDFLD,FVADDR      SAVE A(FIELD)                                
         CLC   FVIFLD(L'SVPID),BCSPACES                                         
         BH    *+14                                                             
         MVC   FVIFLD(L'SVPID),SVPID                                            
         MVI   FVILEN,8                                                         
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         MVC   SVPID,FVIFLD                                                     
*                                                                               
         GOTOX ('VALPID',AGROUTS),FVIFLD                                        
         BNE   VPIDERR                                                          
*&&UK                                                                           
         GOTO1 ACHKLPID,BOPARM,BCWORK CHECK PID IS VALID FOR LOGON              
         BL    VPIDERR                                                          
*&&                                                                             
         MVC   APPKPIDB,BCWORK                                                  
         MVC   SVPIDBIN,BCWORK                                                  
         MVC   SVPIDFNM,BCWORK+2                                                
         MVC   SVPIDLNM,BCWORK+22                                               
         MVC   SVPIDMNM,BCWORK+42                                               
*&&US                                                                           
         L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    VPID04                                                           
         GOTO1 ACHKPID,BOPARM,SVPIDBIN                                          
         BE    VPID04                                                           
         MVC   FVMSGNO,=AL2(AE$WAOOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         CLI   CSACT,A#DIS                                                      
         BE    EXITL                                                            
         MVC   FVMSGNO,=AL2(AE$CMCOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         B     EXITL                                                            
*&&                                                                             
VPID04   L     R4,AIO3             AIO3 USED BY VALPID                          
         LA    R4,SAPEDATA-SAPEREC(R4) CHECK WHETHER PERSON HAS BEEN            
         XR    R0,R0                   TERMINATED                               
         USING SAPERD,R4                                                        
VPID06   CLI   SAPEREL,0               FIND PERSONNEL LIST ELEMENT              
         BE    VPID10                                                           
         CLI   SAPEREL,SAPERELQ                                                 
         BE    VPID08                                                           
         IC    R0,SAPERLN                                                       
         AR    R4,R0                                                            
         B     VPID06                                                           
*                                                                               
VPID08   OC    SAPERDTE,SAPERDTE       IS THERE A TERMINATION DATE?             
         BZ    VPID10                                                           
         GOTO1 VDATCON,BODMCB,(2,SAPERDTE),(1,TEMPDATE)                         
         CLC   TEMPDATE,ASPDAT         COMPARE TERM DATE WITH CURRENT           
*&&UK*&& BNH   VPIDERR2                                                         
*&&US                                                                           
         BH    VPID10                                                           
         OI    WRNIND,WRNTRMD          SET FLAG TO DISPLAY WARNING              
*&&                                                                             
         DROP  R4                                                               
*                                                                               
VPID10   MVC   FLTIFLD(L'SAPALPID),FVIFLD                                       
         B     EXITOK                                                           
*                                                                               
VPIDERR  MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
VPIDERR2 MVC   FVMSGNO,=AL2(AE$DTTRM) DATE IS AFTER TERMINATION DATE            
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PERSON ID FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTPID  MVC   FVIFLD(L'SAPALPID),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PERSON ID FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
VFLTPID  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTOX ('VALPID',AGROUTS),FVIFLD                                        
         BNE   VFLTP03                                                          
*&&US*&& BE    VFLTP04                                                          
*&&UK                                                                           
         GOTO1 ACHKLPID,BOPARM,BCWORK                                           
         BNL   VFLTP04                                                          
*&&                                                                             
VFLTP03  MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
VFLTP04  L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    VFLTP08                                                          
         GOTO1 ACHKPID,BOPARM,BCWORK                                            
         BE    VFLTP08                                                          
*&&US*&& MVC   FVMSGNO,=AL2(AE$WAOOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         B     EXITL                                                            
*                                                                               
VFLTP08  CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BNE   *+10                                                             
         MVC   APPKPIDB,BCWORK     YES - ONLY SHOW THIS PERSON.                 
*                                                                               
         MVC   FLTIFLD(L'SAPALPID),FVIFLD                                       
         MVC   SVPIDFL(L'SAPALPID),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PERSON ID                                          *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTPID  B     FLTXE                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A PERSON ID                                               *         
***********************************************************************         
         SPACE 1                                                                
SRCHPID  L     R0,FVADDR                                                        
         S     R0,ATWA                                                          
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VSRCHCAL,BOPARM,('STMPSTRQ',(R0)),ATWA,ACOM,0,          X        
               (1,=CL8'PERSON'),0                                               
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A PERSON FIRST NAME FIELD                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PFN      NTRDO                                                                  
*                                                                               
PFNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPFN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PERSON FIRST NAME FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DISPFN   MVC   FVIFLD(L'SVPIDFNM),SVPIDFNM                                      
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A PERSON LAST NAME FIELD                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PLN      NTRDO                                                                  
*                                                                               
PLNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPLN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PERSON LAST NAME FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISPLN   MVC   FVIFLD(L'SVPIDLNM),SVPIDLNM                                      
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FINANCE APPROVER FIELD                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FAP      NTRDO                                                                  
*                                                                               
FAPTBL   DC    AL1(DSET),AL1(0,0,0),AL4(DSETFAP)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISFAP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFAP)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFAP)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFAP)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFAP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFAP  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FINANCE APPROVER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRSTA,GSRECSTA                                                 
DISFAP   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.APPRSTAT,APPSFINA                                              
         BZ    DISFAP02                                                         
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
*                                                                               
DISFAP02 TM    T.APPRSTAT,APPSFIND                                              
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@DEF),LC@DEF                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FINANCE APPROVER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRSTA,GSRECSTA                                                 
VALFAP   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    VFLTFAP             YES - USE VALIDATE FILTER FIELD              
         NI    T.APPRSTAT,FF-(APPSFINA+APPSFIND)                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   VALFAP02                                                         
         OI    T.APPRSTAT,APPSFINA                                              
         B     VALFAPX                                                          
*                                                                               
VALFAP02 CLC   FVIFLD(1),LC@DEF    DEFAULT FINANCE APPROVER?                    
         BNE   EXITNV                                                           
         OI    T.APPRSTAT,APPSFIND                                              
T        USING DPAPASD,IOKEY                                                    
         XC    T.DPAPAS,T.DPAPAS                                                
         MVI   T.DPAPTYP,DPAPTYPQ                                               
         MVI   T.DPAPSUB,DPAPSUBQ                                               
         MVC   T.DPAPCPY,CUABIN   CONNECTED ID                                  
         MVI   T.DPAPAPPL,DPAPAEXF                                              
         MVI   T.DPAP1RAC,FF                                                    
         MVC   T.DPAP1RAC+1(L'DPAP1RAC-1),T.DPAP1RAC                            
         ZAP   T.DPAPXVAL,BCPZERO                                               
         MVC   SVIOKEY,IOKEY                                                    
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         B     *+8                                                              
VALFAP04 LHI   R1,XOSEQ+XOACCDIR+XIO2                                           
         GOTO1 AIO                                                              
         CLC   SVIOKEY(DPAPXVAL-DPAPASD),IOKEY                                  
         BNE   VALFAPX                                                          
         CLC   T.DPAPPIDB,SVPIDBIN                                              
         BE    VALFAP04                                                         
         TM    T.DPAPSTAT,DPAPDFLT                                              
         BZ    VALFAP04                                                         
         MVC   FVMSGNO,=AL2(AE$DFEAP)                                           
         B     EXITL               DEFAULT EXISTS FOR ANOTHER APPROVER          
*                                                                               
VALFAPX  B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FINANCE APPROVER FILTER FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
DFLTFAP  MVC   FVIFLD(L'LC@DEF),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FINANCE APPROVER FILTER FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
VFLTFAP  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVI   SVFFINAP,NO                                                      
         OI    FVIFLD,X'40'                                                     
         MVC   FLTIFLD(L'BC@NO),BC@NO                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVFFINAP,YES                                                     
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVI   SVFFINAP,DEF                                                     
         MVC   FLTIFLD(L'LC@DEF),LC@DEF                                         
         CLC   FVIFLD(1),LC@DEF                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'LC@DEF),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON FINANCE APPROVER                         *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFAP  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A JOB APPROVER FIELD                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
JAP      NTRDO                                                                  
*                                                                               
JAPTBL   DC    AL1(DSET),AL1(0,0,0),AL4(DSETJAP)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISJAP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALJAP)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTJAP)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTJAP)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTJAP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETJAP  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A JOB APPROVER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRSTA,GSRECSTA                                                 
DISJAP   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.APPRSTA2,APPSRJAQ                                              
         BZ    DISJAP02                                                         
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
*                                                                               
DISJAP02 TM    T.APPRSTA2,APPSDJAQ                                              
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@DEF),LC@DEF                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A JOB APPROVER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRSTA,GSRECSTA                                                 
VALJAP   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    VFLTJAP             YES - USE VALIDATE FILTER FIELD              
         NI    T.APPRSTA2,FF-(APPSRJAQ+APPSDJAQ)                                
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   VALJAP04                                                         
         OI    T.APPRSTA2,APPSRJAQ                                              
         B     VALJAPX                                                          
VALJAP04 CLC   FVIFLD(1),LC@DEF    DEFAULT JOB APPROVER?                        
         BNE   EXITNV                                                           
         OI    T.APPRSTA2,APPSDJAQ                                              
T        USING JOBPASD,IOKEY                                                    
         XC    T.JOBPAS,T.JOBPAS                                                
         MVI   T.JOBPTYP,JOBPTYPQ                                               
         MVI   T.JOBPSUB,JOBPSUBQ                                               
         MVI   T.JOBPAPPL,JOBPAJOB                                              
         MVI   T.JOBPMED,FF                                                     
         MVC   T.JOBPOFFC,XFFS                                                  
         MVI   T.JOBPJOB,FF                                                     
         MVC   T.JOBPJOB+1(L'JOBPJOB-1),T.JOBPJOB                               
         MVC   T.JOBPCPY,CUABIN   CONNECTED ID                                  
         LHI   R1,XOACCDIR+XOHIGH+XIO2                                          
         B     *+8                                                              
VALJAP08 LHI   R1,XOACCDIR+XOSEQ+XIO2                                           
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(JOBPMED-JOBPASD),IOKEY                                  
         BNE   VALJAPX                                                          
         CLC   T.JOBPPIDB,SVPIDBIN                                              
         BE    VALJAP08                                                         
         TM    T.JOBPSTAT+1,APPSDJAQ                                            
         BZ    VALJAP08                                                         
         MVC   FVMSGNO,=AL2(AE$DFEAP)                                           
         B     EXITL               DEFAULT EXISTS FOR ANOTHER APPROVER          
         DROP  T                                                                
                                                                                
VALJAPX  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A JOB APPROVER FIELD FILTER                                 *         
***********************************************************************         
         SPACE 1                                                                
DFLTJAP  MVC   FVIFLD(L'LC@DEF),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A JOB APPROVER FIELD FILTER                                *         
***********************************************************************         
         SPACE 1                                                                
VFLTJAP  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVI   SVFJOBAP,NO                                                      
         OI    FVIFLD,X'40'                                                     
         MVC   FLTIFLD(L'BC@NO),BC@NO                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVFJOBAP,YES                                                     
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVI   SVFJOBAP,DEF                                                     
         MVC   FLTIFLD(L'LC@DEF),LC@DEF                                         
         CLC   FVIFLD(1),LC@DEF                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'LC@DEF),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON A JOB APPROVER FIELD                     *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTJAP  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN ESTIMATE APPROVER FIELD               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
EST      NTRDO                                                                  
*                                                                               
ESTTBL   DC    AL1(DSET),AL1(0,0,0),AL4(DSETEAP)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISEAP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEAP)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTEAP)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTEAP)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTEAP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETEAP  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN ESTIMATE APPROVER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRSTA,GSRECSTA                                                 
DISEAP   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO     NO IS DEFAULT                          
         TM    T.APPRSTA2,APPSESTQ                                              
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ESTIMATE APPROVER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRSTA,GSRECSTA                                                 
VALEAP   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    VFLTEAP             YES - USE VALIDATE FILTER FIELD              
         NI    T.APPRSTA2,FF-(APPSESTQ)                                         
         CLI   FVILEN,0                  IS A Y/N FIELD                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.APPRSTA2,APPSESTQ                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN ESTIMATE APPROVER FIELD FILTER                           *         
***********************************************************************         
         SPACE 1                                                                
DFLTEAP  MVC   FVIFLD(L'LC@DEF),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ESTIMATE APPROVER FIELD FILTER                          *         
***********************************************************************         
         SPACE 1                                                                
VFLTEAP  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVI   SVFESTAP,NO                                                      
         OI    FVIFLD,X'40'                                                     
         MVC   FLTIFLD(L'BC@NO),BC@NO                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVFESTAP,YES                                                     
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON AN ESTIMATE APPROVER FIELD               *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTEAP  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN INTERNAL ESTIMATE APPROVER FIELD      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
IEST     NTRDO                                                                  
*                                                                               
IESTTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETIEAP)                               
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISIEAP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIEAP)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTIEAP)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTIEAP)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTIEAP)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETIEAP DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN INTERNAL ESTIMATE APPROVER FIELD                         *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRSTA,GSRECSTA                                                 
DISIEAP  CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO     NO IS DEFAULT                          
         TM    T.APPRSTA2,APPSESIQ                                              
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN INTERNAL ESTIMATE APPROVER FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRSTA,GSRECSTA                                                 
VALIEAP  CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    VFLTIEAP            YES - USE VALIDATE FILTER FIELD              
         NI    T.APPRSTA2,FF-(APPSESIQ)                                         
         CLI   FVILEN,0                  IS A Y/N FIELD                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.APPRSTA2,APPSESIQ                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN INTERNAL ESTIMATE APPROVER FIELD FILTER                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTIEAP MVC   FVIFLD(L'LC@DEF),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN INTERNAL ESTIMATE APPROVER FIELD FILTER                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTIEAP CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVI   SVFIESTA,NO                                                      
         OI    FVIFLD,X'40'                                                     
         MVC   FLTIFLD(L'BC@NO),BC@NO                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVFIESTA,YES                                                     
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON AN INTERNAL ESTIMATE APPROVER FIELD      *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTIEAP B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR BACK-UP PERSON ID APPLICATION                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
BAPP     NTRDO                                                                  
*                                                                               
BAPPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISBAPP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAABAPP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 1R APPLICATION INDICATOR FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISBAPP  LA    RF,TLBAPP                                                        
         LA    RE,TLBAPP2                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DBAPP02                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDBUAPP                                                      
         LA    RE,TLDBUAP2                                                      
         OC    TLDBUPID,TLDBUPID                                                
         BZ    EXITOK              DON'T SHOW IT IF NO PID                      
*                                                                               
DBAPP02  MVC   FVIFLD(3),NOS       SET FIELD TO NNN                             
         OC    0(L'TLBAPP+L'TLBAPP2,RF),0(RF)       ANY DATA ?                  
         BZ    EXITOK              NO  - NOTHING TO DO                          
*                                                                               
         TM    0(RF),TLBTIME       TIMESHEET ?                                  
         BZ    DBAPP04                                                          
         MVC   FVIFLD(1),BC@YES                                                 
*                                                                               
DBAPP04  TM    0(RF),TLBEXP        EXPENSE APPROVER?                            
         BZ    DBAPP06                                                          
         MVC   FVIFLD+1(1),BC@YES                                               
*                                                                               
DBAPP06  TM    0(RE),TLBTOFF       TIME-OFF APPROVER ?                          
         BZ    DBAPPX                                                           
         MVC   FVIFLD+2(1),BC@YES                                               
*                                                                               
DBAPPX   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 1R APPLICATION INDICATOR FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VAABAPP  MVI   TLBAPP,0                                                         
         NI    TLBAPP2,X'FF'-LIDLTOFF                                           
         CLI   CSACT,A#DLOAD                                                    
         BE    EXITOK                                                           
         CLI   FVILEN,0            ANYTHING ENTERED?                            
         BE    EXITOK                                                           
*                                                                               
         LA    RE,FVIFLD                                                        
         CLC   0(1,RE),BC@YES      TIMESHEETS?                                  
         BNE   *+8                                                              
         OI    TLBAPP,TLBTIME                                                   
*                                                                               
         CLC   1(1,RE),BC@YES      EXPENSES?                                    
         BNE   *+8                                                              
         OI    TLBAPP,TLBEXP                                                    
*                                                                               
         CLC   2(1,RE),BC@YES      TIME-OFF ?                                   
         BNE   VAABAPPX                                                         
         OI    TLBAPP2,LIDLTOFF                                                 
*                                                                               
VAABAPPX B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR BACK-UP PERSON ID CODE                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
BAP      NTRDO                                                                  
*                                                                               
BAPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBAP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAABAP)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHPID)                               
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETBAP)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTBAP)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTBAP)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTBAP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETBAP  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BACK-UP PERSON ID CODE                                      *         
***********************************************************************         
         SPACE 1                                                                
DISBAP   MVC   FVIFLD(L'TLKBCPID),TLKBCPID                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BACK-UP PERSON ID CODE                                     *         
***********************************************************************         
         SPACE 1                                                                
VAABAP   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    VFLTBAP             YES - USE VALIDATE FILTER FIELD              
         CLI   FVILEN,0                                                         
         BNE   VBAP02                                                           
         XC    TLKBCPID,TLKBCPID                                                
         XC    TLKBBPID,TLKBBPID                                                
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
VBAP02   GOTOX ('VALPID',AGROUTS),FVIFLD                                        
         BNE   VBAP03                                                           
*&&US*&& B     VBAP04                                                           
*&&UK                                                                           
         GOTO1 ACHKLPID,BOPARM,BCWORK                                           
         BNL   VBAP04                                                           
*&&                                                                             
VBAP03   MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
VBAP04   DS    0H                                                               
*&&US                                                                           
         L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    VBAP05                                                           
         GOTO1 ACHKPID,BOPARM,BCWORK                                            
         BE    VBAP05                                                           
         MVC   FVMSGNO,=AL2(AE$CMCOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         B     EXITL                                                            
*&&                                                                             
VBAP05   CLC   SVPIDBIN,BCWORK                                                  
         BNE   VBAP06                                                           
         MVC   FVMSGNO,=AL2(AE$APBAP) CAN'T BE SAME AS APPROVER                 
         B     EXITL                                                            
*                                                                               
VBAP06   MVC   TLKBBPID,BCWORK                                                  
         MVC   TLKBCPID,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BACK UP APPROVER FIELD FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFLTBAP  GOTOX ('GETPID',AGROUTS),FLTIFLD                                       
         MVC   FVIFLD(L'SAPALPID),BCWORK                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A BACK UP APPROVER FILTER FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
VFLTBAP  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTOX ('VALPID',AGROUTS),FVIFLD                                        
         BNE   VFLTB03                                                          
*&&US*&& BE    VFLTB04                                                          
*&&UK                                                                           
         GOTO1 ACHKLPID,BOPARM,BCWORK                                           
         BNL   VFLTB04                                                          
*&&                                                                             
VFLTB03  MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
VFLTB04  MVC   FLTIFLD(2),BCWORK                                                
         MVC   SVFBAP(2),BCWORK                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR BACK UP APPROVER                                   *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTBAP  B     FLTXE                                                            
         SPACE 2                                                                
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A BACK-UP PERSON FIRST NAME FIELD        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
BFN      NTRDO                                                                  
*                                                                               
BFNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBFN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BACK-UP FIRST NAME FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DISBFN   GOTOX ('VALPID',AGROUTS),TLKBCPID                                      
         BE    DBFN02                                                           
         DC    H'0'                                                             
*                                                                               
DBFN02   MVC   FVIFLD(L'SVPIDFNM),BCWORK+2                                      
DBFNX    B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A BACK-UP PERSON LAST NAME FIELD         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
BLN      NTRDO                                                                  
*                                                                               
BLNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBLN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BACK UP PERSON LAST NAME FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DISBLN   GOTOX ('VALPID',AGROUTS),TLKBCPID                                      
         BE    DBLN02                                                           
         DC    H'0'                                                             
*                                                                               
DBLN02   MVC   FVIFLD(L'SVPIDLNM),BCWORK+22                                     
DBLNX    B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR PID (PERSONAL ID) ON LIST SCREEN                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
PIL      NTRDO                                                                  
*                                                                               
PILTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPIL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PID FIELD                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISPIL   L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLKPID),TLKPID                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FIRST NAME IN LIST                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PFNL     NTRDO                                                                  
*                                                                               
PFNLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIRST NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISFNL   GOTOX ('VALPID',AGROUTS),TLKPID                                        
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'SANAME),BCWORK+2                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LAST NAME IN LIST                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PLNL     NTRDO                                                                  
*                                                                               
PLNLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNL)                                 
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY LAST NAME FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISLNL   GOTOX ('VALPID',AGROUTS),TLKPID                                        
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'SANAME),BCWORK+22                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PERSON NAME ON DOWNLOAD REPORT                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APND     NTRDO                                                                  
*                                                                               
APNDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPND)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PERSON NAME ON DOWNLOAD REPORT                              *         
***********************************************************************         
         SPACE 1                                                                
DISAPND  L     R2,ATLST                                                         
         CLC   TLKPID,SVPID        HAS PID BEEN CHANGED?                        
         BE    DAPND10             NO - OK                                      
*                                                                               
         MVC   BCWORK,BCSPACES                                                  
         GOTOX ('VALPID',AGROUTS),TLKPID                                        
         MVC   SVPID,TLKPID        SAVE PID INFO FOR DOWNLOAD                   
         MVC   SVPIDFNM,BCWORK+2                                                
         MVC   SVPIDLNM,BCWORK+22                                               
*                                                                               
DAPND10  MVC   FVIFLD(L'SVPIDFNM),SVPIDFNM                                      
         LA    RF,FVIFLD                                                        
         CLC   SVPIDFNM,BCSPACES   ANY FIRST NAME?                              
         BE    DAPND14                                                          
         LA    RF,L'SVPIDFNM(,RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(,RF)                                                        
*                                                                               
DAPND14  MVC   0(L'SVPIDLNM,RF),SVPIDLNM                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BACK-UP PERSON ID CODE ON DOWNLOAD REPORT           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BAPL     NTRDO                                                                  
*                                                                               
BAPLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISBAPL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BACK-UP PERSON ID CODE FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISBAPL  L     R2,ATLST                                                         
         MVC   BCWORK,BCSPACES                                                  
         OC    TLDBUPID,TLDBUPID   ANY PID?                                     
         BZ    DISBAPL4                                                         
         OC    SVBUPID,SVBUPID     FIRST TIME?                                  
         BZ    DISBAPL2                                                         
         CLC   TLDBUPID,SVBUPIDB   HAS PID BEEN CHANGED?                        
         BNE   DISBAPL2            YES - GET PID INFO                           
         MVC   FVIFLD(L'SVBUPID),SVBUPID                                        
         B     EXITOK                                                           
*                                                                               
DISBAPL2 GOTOX ('GETPID',AGROUTS),TLDBUPID                                      
         BNE   DISBAPL4                                                         
         MVC   FVIFLD(L'SVBUPID),BCWORK                                         
         MVC   SVBUPID,BCWORK                                                   
*                                                                               
         MVC   BCWORK,BCSPACES                                                  
         GOTOX ('VALPID',AGROUTS),SVBUPID                                       
         MVC   SVBUPIDB,TLDBUPID   SAVE BUCK-UP PID INFO FOR DOWNLOAD           
*                                                                               
DISBAPL4 MVC   SVBUPIDF,BCWORK+2                                                
         MVC   SVBUPIDL,BCWORK+22                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BACK-UP PERSON NAME ON DOWNLOAD REPORT              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BAND     NTRDO                                                                  
*                                                                               
BANDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISBAND)                                
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY BACK-UP BACK-UP PERSON NAME ON DOWNLOAD REPORT              *         
***********************************************************************         
         SPACE 1                                                                
DISBAND  MVC   FVIFLD(L'SVBUPIDF),SVBUPIDF                                      
         LA    RF,FVIFLD                                                        
         CLC   SVBUPIDF,BCSPACES   ANY FIRST NAME?                              
         BE    DBAND10                                                          
         LA    RF,L'SVBUPIDF(,RF)                                               
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
         LA    RF,2(,RF)                                                        
*                                                                               
DBAND10  MVC   0(L'SVBUPIDL,RF),SVBUPIDL                                        
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON APPROVER LIMIT FILED        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FAPL     NTRDO                                                                  
*                                                                               
FAPLTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFAPL)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFAPL)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFAPL)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFAPL)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFAPL)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFAPL DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPROVAL LIMIT FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTFAPL OC    FLTIFLD,FLTIFLD                                                  
         BZ    EXITOK                                                           
         CURED (P6,FLTIFLD),(13,FVIFLD),0,ALIGN=LEFT,COMMAS=YES,       C        
               DMCB=BODMCB                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPROVAL LIMIT FILTER FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VFLTFAPL XC    FLTIFLD,FLTIFLD                                                  
         XC    SVFAPV,SVFAPV                                                    
         CLI   FVILEN,0                                                         
         BE    VFFAPLX                                                          
         GOTO1 AVALLMT,BOPARM,(FVILEN,FVIFLD)                                   
         BNE   EXITL                                                            
*                                                                               
         ZAP   FLTIFLD(L'MYPL6),MYPL6                                           
         ZAP   SVFAPV,MYPL6                                                     
*                                                                               
VFFAPLX  TM    FVIIND,FVITHIS                                                   
         BNO   EXITOK                                                           
         OI    LSSCIND1,LSSCIFLT   REFRESH LIST                                 
         MVI   FLTTYPE,DOFIFAPV                                                 
         GOTO1 AVALFLT,BOPARM,(FLTTYPE,SVFAPV)                                  
         BNE   EXITL               APPROVER NOT FOUND                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR APPROVAL LIMIT                                     *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFAPL B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON FINANCE APPROVER FIELD      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FFAP     NTRDO                                                                  
*                                                                               
FFAPTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFCPJ)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFCPJ)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFCPJ)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFCPJ)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFCPJ)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFFAP DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FINANCE APPROVER FILTER FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
DFLTFFAP MVC   FVIFLD(L'LC@DEF),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FINANCE APPROVER FILTER FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
VFLTFFAP CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVI   SVFFINAP,NO                                                      
         OI    FVIFLD,X'40'                                                     
         MVC   FLTIFLD(L'BC@NO),BC@NO                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVFFINAP,YES                                                     
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVI   SVFFINAP,DEF                                                     
         MVC   FLTIFLD(L'LC@DEF),LC@DEF                                         
         CLC   FVIFLD(1),LC@DEF                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'LC@DEF),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON FINANCE APPROVER                         *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFFAP B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON CLI/PRO/JOB FIELD           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FCPJ     NTRDO                                                                  
*                                                                               
FCPJTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFCPJ)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFCPJ)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFCPJ)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFCPJ)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFCPJ)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFCPJ)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFCPJ DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON CLI/PRO/JOB FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTFCPJ MVC   FVIFLD(L'SVFCPJ),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON CLI/PRO/JOB FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VFLTFCPJ CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'TLKASJAC                                                
         BH    EXITLONG            FIELD TOO LONG                               
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ FOR CLI/PRO/JOB ACCOUNT RECORD          
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'PRDUL),PRDUL                                         
         MVC   T.ACTKACT,FVIFLD                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACC) INVALID ACCOUNT                           
         B     EXITL                                                            
*                                                                               
         MVC   SVFCPJ,FVIFLD                                                    
         MVC   SVFCPJXL,FVXLEN                                                  
         MVC   FLTIFLD(L'SVFCPJ),SVFCPJ                                         
         MVI   FLTTYPE,DOFIFCPJ                                                 
         GOTO1 AVALFLT,BOPARM,(FLTTYPE,SVFCPJ)                                  
         BNE   EXITL               APPROVER NOT FOUND                           
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO/JOB CODE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
SRCHFCPJ DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRDUL,ACOM,    >        
               (X'13',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON CLI/PRO/JOB                              *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFCPJ B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON EXPENDITURE FIELD           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FETC     NTRDO                                                                  
*                                                                               
FETCTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFETC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFETC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFETC)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFETC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFETC)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFETC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFETC DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON EXPENDITURE FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTFETC MVC   FVIFLD(L'SVFETC),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON EXPENDITURE FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VFLTFETC CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'TLKAPETY                                                
         BH    EXITLONG            FIELD TOO LONG                               
T        USING ETYRECD,IOKEY                                                    
         XC    T.ETYKEY,T.ETYKEY   READ FOR EXPENDITURE TYPE REC                
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CUABIN                                                 
         MVC   T.ETYKCODE,FVIFLD                                                
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEY(ETYKCODE-ETYRECD),IOKEYSAV                                 
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INETY)                                           
         B     EXITL               INVALID EXPENDITURE CODE                     
*                                                                               
         MVC   SVFETC,FVIFLD                                                    
         MVC   FLTIFLD(L'SVFETC),SVFETC                                         
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON EXPENDITURE TYPE NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHFETC DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,EXPTYP,ACOM,0           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON EXPENDITURE                              *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFETC B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON OFF/DEPT FIELD              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FODC     NTRDO                                                                  
*                                                                               
FODCTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFODC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFODC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFODC)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFODC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFODC)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFODC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFODC DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON OFF/DEPT FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DFLTFODC MVC   FVIFLD(L'SVFODC),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON OFF/DEPT FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VFLTFODC CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BO    VFODC02                                                          
         CLI   FVILEN,L'TLKAPDPT                                                
         BH    EXITLONG                                                         
         B     VFODC20                                                          
*                                                                               
VFODC02  CLC   FVILEN,LEN1RA       OFFICE LEVEL ONLY ?                          
         BL    EXITSHRT            FIELD IS TOO SHORT - ERROR                   
         BH    VFODC04             HIGH - OFFICE + DEPT LEVEL                   
*                                  YES - OFFICE LEVEL ONLY                      
         CLI   FVILEN,2            2 BYTES OFFICE CODE INPUT ?                  
         BE    VFODC06             YES, CHECK FOR OFFICE LIST INPUT             
         B     VFODC20             NO, MUST BE 1 BYTE, CONTINUE                 
*                                                                               
VFODC04  CLC   FVILEN,LEN1RB       OFF+DEPT LENGTH ?                            
         BE    VFODC20             YES, CONTINUE                                
         BH    EXITLONG            HIGHT , ERROR - FIELD TOO LONG               
*                                  LOW THAN OFFICE + DEPT LENGTH                
         CLI   FVILEN,2            OLIST INPUT FOR 1 CHAR OFFICE ?              
         BNE   EXITNV              NO, CONTINUE                                 
*                                  YES                                          
T        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
VFODC06  MVC   T.OFFKEY,BCSPACES   INIT TO SPACES                               
         MVI   T.OFFKTYP,OFFKTYPQ  OFFICE RECORD TYPE                           
         MVC   T.OFFKCPY,CUABIN    COMPANY CODE                                 
         MVC   T.OFFKOFF(2),FVIFLD OFFICE CODE                                  
         MVC   SVIOKEY,IOKEY       SAVE KEY DETAILS                             
         LHI   R1,XOHI+XOACCDIR+XIO2                                            
         GOTO1 AIO                                                              
         BE    VFODC08                                                          
         DC    H'0'                                                             
*                                                                               
VFODC08  CLC   T.OFFKEY,SVIOKEY    RECORD FOUND FOR THE OFFICE CODE ?           
         BNE   EXITNV              NO, ERROR                                    
*                                  YES, CONTINUE                                
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         JO    VFODC10             YES, CONTINUE                                
*                                  N0,                                          
         TM    T.OFFKSTAT,OFFSLIST FOR 1 CHAR. OFFICE,OLIST INPUT?              
         BZ    EXITNV              NO, PROCESS ERROR                            
*                                  YES                                          
VFODC10  MVC   SVFODC,BCSPACES     INIT OFFICE/DEPT CODE                        
         MVC   SVFOFF(L'SVFOFF),FVIFLD  SAVE OFFICE CODE                        
         MVC   FLTIFLD(L'SVFODC),BCSPACES INIT OFFICE/DEPT FILTER CODE          
         MVC   FLTIFLD(L'SVFOFF),FVIFLD    SAVE FILTER VALUE                    
*                                                                               
         MVI   FLTTYPE,DOFIFODC                                                 
         GOTO1 AVALFLT,BOPARM,(FLTTYPE,SVFODC)                                  
         BNE   EXITL               APPROVER NOT FOUND                           
*                                                                               
         B     EXITOK              EXIT                                         
*                                                                               
T        USING ACTRECD,IOKEY                                                    
VFODC20  MVC   T.ACTKEY,BCSPACES   READ FOR OFF/DEPT ACCOUNT RECORD             
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'OFFUL),OFFUL                                         
         LLC   RF,FVXLEN                                                        
         MVC   T.ACTKACT(0),FVIFLD                                              
         EX    RF,*-6                                                           
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     EXITL               INVALID ACCOUNT                              
*                                                                               
         MVC   FLTIFLD(L'SVFODC),FVIFLD                                         
         MVC   SVFODC,BCSPACES                                                  
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BO    VFODC30                                                          
         MVC   SVFDPT,T.ACTKACT                                                 
*                                                                               
         MVI   FLTTYPE,DOFIFODC                                                 
         GOTO1 AVALFLT,BOPARM,(FLTTYPE,SVFODC)                                  
         BNE   EXITL               APPROVER NOT FOUND                           
*                                                                               
         B     EXITOK                                                           
                                                                                
VFODC30  LLC   RF,LEN1RA           RF = OFFICE LENGTH                           
         LR    R1,RF               SAVE OFFICE LENGTH                           
         SHI   RF,1                                                             
         MVC   SVFOFF(0),T.ACTKACT                                              
         EX    RF,*-6                                                           
         LLC   RF,LEN1RB                                                        
         SR    RF,R1               RF = DEPARTMENT LENGTH                       
         SHI   RF,1                                                             
         LA    RE,T.ACTKACT(R1)                                                 
         MVC   SVFDPT(0),0(RE)                                                  
*                                                                               
         MVI   FLTTYPE,DOFIFODC                                                 
         GOTO1 AVALFLT,BOPARM,(FLTTYPE,SVFODC)                                  
         BNE   EXITL               APPROVER NOT FOUND                           
*                                                                               
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A OFF/DEPT CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHFODC DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,OFFUL,ACOM,    X        
               (X'14',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON OFF/DEPT                                 *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFODC B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON NON-CLIENT CODE FIELD       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FNCC     NTRDO                                                                  
*                                                                               
FNCCTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFNCC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFNCC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFNCC)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFNCC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFNCC)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFNCC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFNCC DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON NON-CLIENT CODE FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTFNCC MVC   FVIFLD(L'SVFNCC),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON NON-CLIENT CODE FIELD                         *         
***********************************************************************         
         SPACE 1                                                                
VFLTFNCC CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'TLKA1NAC                                                
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ FOR NON-CLIENT ACCOUNT RECORD           
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'NCTUL),NCTUL                                         
         MVC   T.ACTKACT,FVIFLD                                                 
         DROP  T                                                                
         GOTO1 AGETACT,0           READ ACCOUNT/TEST SECURITY                   
         BNE   EXITL                                                            
                                                                                
         TM    ACINDS1,ACIACTLO    TEST LOW LEVEL ACCOUNT                       
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NLOWA)                                           
         B     EXITL               NOT A LOWER LEVEL ACCOUNT                    
*                                                                               
         MVC   SVFNCC,FVIFLD                                                    
         MVC   FLTIFLD(L'SVFNCC),SVFNCC                                         
         MVI   FLTTYPE,DOFIFNCC                                                 
         GOTO1 AVALFLT,BOPARM,(FLTTYPE,SVFNCC)                                  
         BNE   EXITL               APPROVER NOT FOUND                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A NON-CLIENT CODE FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
SRCHFNCC DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,NCTUL,ACOM,    X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON NON-CLIENT CODE                          *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFNCC B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON COSTING 1R CODE FIELD       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
F1RC     NTRDO                                                                  
*                                                                               
F1RCTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETF1RC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTF1RC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTF1RC)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTF1RC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTF1RC)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHF1RC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETF1RC DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON COSTING 1R CODE FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTF1RC MVC   FVIFLD(L'SVF1RC),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON COSTING 1R CODE FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
VFLTF1RC CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ FOR COSTING 1R ACCOUNT RECORD           
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'ONERUL),ONERUL                                       
         MVC   T.ACTKACT,FVIFLD                                                 
         DROP  T                                                                
         GOTO1 AGETACT,0           READ ACCOUNT/TEST SECURITY                   
         BNE   EXITL                                                            
*                                                                               
         MVC   SVF1RC,FVIFLD                                                    
         MVC   FLTIFLD(L'SVF1RC),SVF1RC                                         
         MVI   FLTTYPE,DOFIF1RC                                                 
         GOTO1 AVALFLT,BOPARM,(FLTTYPE,SVF1RC)                                  
         BNE   EXITL               APPROVER NOT FOUND                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A COSTING 1R CODE FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
SRCHF1RC DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,ONERUL,ACOM,   X        
               (X'14',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON COSTING 1R CODE                          *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTF1RC B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON PROD/SUPP CODE FIELD         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FPSC     NTRDO                                                                  
*                                                                               
FPSCTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFPSC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFPSC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFPSC)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFPSC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFPSC)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFPSC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFPSC DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON PROD/SUPP CODE/1R FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
DFLTFPSC MVC   FVIFLD(L'SVFPSC),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON PROD/SUPP CODE/1R FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
VFLTFPSC CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'SVUL                                                    
         BNH   EXITSHRT                                                         
         CLI   FVILEN,L'ACTKULA                                                 
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
         CLC   PRDUL,FVIFLD       MUST BE SJ/SV/SX                              
         BE    VFLTFP02                                                         
         CLC   SVUL,FVIFLD                                                      
         BE    VFLTFP02                                                         
         CLC   SXUL,FVIFLD                                                      
         BE    VFLTFP02                                                         
         CLC   ONERUL,FVIFLD                                                    
         BE    VFLTFP02                                                         
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         MVC   FVXTRA(2),FVIFLD    INVALID LEDGER                               
         B     EXITL                                                            
*                                                                               
T        USING ACTRECD,IOKEY                                                    
VFLTFP02 MVC   T.ACTKEY,BCSPACES   READ FOR PROD/SUPP ACCOUNT RECORD            
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKULA,FVIFLD                                                 
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
         MVC   SVFPSC,FVIFLD+1                                                  
         MVC   FLTIFLD(L'ACTKULA),FVIFLD                                        
         MVI   FLTTYPE,DOFIFPSC                                                 
         GOTO1 AVALFLT,BOPARM,(FLTTYPE,SVFPSC)                                  
         BNE   EXITL               APPROVER NOT FOUND                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A PROD/SUPP CODE/1R FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
SRCHFPSC L     RF,FVADDR           A(INPUT FIELD)                               
         CLC   PRDUL,8(RF)         SEARCH ON SJ/SV/SX ONLY                      
         BE    SRCHF02                                                          
         CLC   SVUL,8(RF)                                                       
         BE    SRCHF02                                                          
         CLC   ONERUL,8(RF)                                                     
         BE    SRCHF02                                                          
         CLC   SXUL,8(RF)                                                       
         BNE   EXITOK                                                           
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
SRCHF02  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,ACOM,        X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON PROD/SUPP CODE                           *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFPSC B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON MEDIA CODE FIELD             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FMED     NTRDO                                                                  
*                                                                               
FMEDTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFMED)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFMED)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFMED)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFMED)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFMED)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFMED)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFMED DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON MEDIA CODE FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTFMED MVC   FVIFLD(L'SVFMED),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON MEDIA CODE FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VFLTFMED CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'SVFMED                                                  
         BH    EXITLONG           FIELD TOO LONG                                
*                                                                               
T        USING PMDRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES                                                   
         MVI   T.PMDKTYP,PMDKTYPQ  MEDIA RECORD                                 
         MVC   T.PMDKCPY,CUABIN    COMPANY                                      
         MVC   T.PMDKMED,FVIFLD    MEDIA CODE                                   
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MDRNF)                                           
         B     EXITL               MEDIA DOES NOT EXIST                         
         MVC   SVFMED,FVIFLD                                                    
         MVC   FLTIFLD(L'SVFMED),FVIFLD                                         
         MVI   FLTTYPE,DOFIFMED                                                 
         GOTO1 AVALFLT,BOPARM,(FLTTYPE,SVFMED)                                  
         BNE   EXITL               APPROVER NOT FOUND                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A MEDIA CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
SRCHFMED DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MEDCODE,ACOM            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON MEDIA CODE                               *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFMED B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR 1R APPLICATION INDICATOR                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APP      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
APPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPP)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(DHDAPP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 1R APPLICATION INDICATOR FIELD                             *          
***********************************************************************         
         SPACE 1                                                                
DISAPP   LA    RF,TLAPDTY                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DISAPP0                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDAPDTY                                                      
         CLC   TLDA1RAC,BCSPACES                                                
         BNH   EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
*                                                                               
DISAPP0  MVC   FVIFLD(4),NOS       SET FIELD TO NNNN                            
         TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BZ    *+10                                                             
         MVC   FVIFLD+4(3),NOS                                                  
         CLI   0(RF),0             CHECK WHETHER ANYTHING SET                   
         BNE   *+12                                                             
         CLI   TLDAPDT2,0                                                       
         BE    EXITOK              YES - NOTHING TO DO                          
*                                                                               
         TM    0(RF),TLAPTIM       TIMESHEET ?                                  
         BZ    *+10                                                             
         MVC   FVIFLD(1),BC@YES                                                 
*                                                                               
         TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BO    DISAPP2                                                          
         TM    0(RF),TLAPDEX       EXPENSE APPROVER?                            
         BZ    *+10                                                             
         MVC   FVIFLD+1(1),BC@YES                                               
         LA    R4,FVIFLD+2         NEXT COLUMN                                  
         B     DISAPP4                                                          
*                                                                               
DISAPP2  TM    0(RF),TLAPDB1       EXPENSE CLAIMS - LVL 1 APPR BILLABLE         
         BZ    *+10                                                             
         MVC   FVIFLD+1(1),BC@YES                                               
         TM    0(RF),TLAPDE1       EXPENSE CLAIMS - LVL 1 APPR NON BILL         
         BZ    *+10                                                             
         MVC   FVIFLD+2(1),BC@YES                                               
         TM    0(RF),TLAPDB2       EXPENSE CLAIMS - LVL 2 APPR BILLABLE         
         BZ    *+10                                                             
         MVC   FVIFLD+3(1),BC@YES                                               
         TM    0(RF),TLAPDE2       EXPENSE CLAIMS - LVL 2 APPR NON BILL         
         BZ    *+10                                                             
         MVC   FVIFLD+4(1),BC@YES                                               
         LA    R4,FVIFLD+5                                                      
*                                                                               
DISAPP4  TM    0(RF),TLAPDEF       FINANCE ?                                    
         BZ    *+10                                                             
         MVC   0(1,R4),BC@YES                                                   
         TM    0(RF),TLAPDED       DEFAULT FINANCE?                             
         BZ    *+10                                                             
         MVC   0(1,R4),BC@DEF                                                   
         AHI   R4,1                                                             
*                                                                               
         LA    RF,TLAPDT2                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DISAPP6                                                          
         LA    RF,TLDAPDT2                                                      
*                                                                               
DISAPP6  TM    0(RF),TLAPDTO       TIMEOFF?                                     
         BZ    *+10                                                             
         MVC   0(1,R4),BC@YES                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 1R APPLICATION INDICATOR FIELD                            *          
***********************************************************************         
         SPACE 1                                                                
VALAPP   MVI   TLAPDTY,0                                                        
         MVI   TLAPDT2,0                                                        
         CLI   FVILEN,0                                                         
         BE    VALAPPX                                                          
         CLC   FVIFLD(5),BCSPACES                                               
         BE    EXITNV                                                           
         TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BO    VAPP02                                                           
         CLI   FVILEN,4                                                         
         BNH   VAPP02                                                           
         MVI   FVERRNDX,3          SET CURSOR TO COLUMN 4                       
         CLI   FVIFLD+3,C' '                                                    
         BH    EXITNV                                                           
         MVI   FVERRNDX,4          SET CURSOR TO COLUMN 5                       
         CLI   FVIFLD+4,C' '                                                    
         BH    EXITNV                                                           
         MVI   FVERRNDX,5          SET CURSOR TO COLUMN 6                       
         B     EXITNV                                                           
                                                                                
VAPP02   CLC   FVIFLD(1),BC@YES                                                 
         BNE   *+12                                                             
         OI    TLAPDTY,TLAPTIM     SET TIMESHEET APPLICATION                    
         B     VAPP04                                                           
         CLI   FVIFLD,C' '                                                      
         BE    VAPP04                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
*                                                                               
VAPP04   TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BO    VAPP06                                                           
         CLC   FVIFLD+1(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLAPDTY,TLAPDEX     SET EXPENSE CLAIMS APPLICATION               
         B     VAPP04A                                                          
         CLI   FVIFLD+1,C' '                                                    
         BE    VAPP04A                                                          
         CLC   FVIFLD+1(1),BC@NO                                                
         BE    VAPP04A                                                          
         MVI   FVERRNDX,1          SET CURSOR TO COLUMN 2                       
         B     EXITNV                                                           
VAPP04A  LA    R4,FVIFLD+2         NEXT COLUMN                                  
         MVI   MYBYTE,2            CURSOR TO NEXT COLUMN                        
         B     VAPP14                                                           
*                                                                               
VAPP06   CLC   FVIFLD+1(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLAPDTY,TLAPDB1     SET EXP CLAIMS - LVL 1 APPR BILLABLE         
         B     VAPP08                                                           
         CLI   FVIFLD+1,C' '                                                    
         BE    VAPP08                                                           
         CLC   FVIFLD+1(1),BC@NO                                                
         BE    VAPP08                                                           
         MVI   FVERRNDX,1          SET CURSOR TO COLUMN 2                       
         B     EXITNV                                                           
*                                                                               
VAPP08   CLC   FVIFLD+2(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLAPDTY,TLAPDE1     SET EXP CLAIMS - LVL 1 APPR NON BILL         
         B     VAPP10                                                           
         CLI   FVIFLD+2,C' '                                                    
         BE    VAPP10                                                           
         CLC   FVIFLD+2(1),BC@NO                                                
         BE    VAPP10                                                           
         MVI   FVERRNDX,2          SET CURSOR TO COLUMN 3                       
         B     EXITNV                                                           
*                                                                               
VAPP10   CLC   FVIFLD+3(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLAPDTY,TLAPDB2     SET EXP CLAIMS - LVL 2 APPR BILLABLE         
         B     VAPP12                                                           
         CLI   FVIFLD+3,C' '                                                    
         BE    VAPP12                                                           
         CLC   FVIFLD+3(1),BC@NO                                                
         BE    VAPP12                                                           
         MVI   FVERRNDX,3          SET CURSOR TO COLUMN 4                       
         B     EXITNV                                                           
*                                                                               
VAPP12   CLC   FVIFLD+4(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLAPDTY,TLAPDE2     SET EXP CLAIMS - LVL 2 APPR NON BILL         
         B     VAPP12A                                                          
         CLI   FVIFLD+4,C' '                                                    
         BE    VAPP12A                                                          
         CLC   FVIFLD+4(1),BC@NO                                                
         BE    VAPP12A                                                          
         MVI   FVERRNDX,4          SET CURSOR TO COLUMN 4                       
         B     EXITNV                                                           
VAPP12A  LA    R4,FVIFLD+5         NEXT COLUMN                                  
         MVI   MYBYTE,5            CURSOR TO NEXT COLUMN                        
*                                                                               
VAPP14   CLC   0(1,R4),BC@YES                                                   
         BNE   *+12                                                             
         OI    TLAPDTY,TLAPDEF     SET FINANCE APPROVER                         
         B     VAPP16                                                           
         CLC   0(1,R4),BC@DEF                                                   
         BNE   *+12                                                             
         OI    TLAPDTY,TLAPDED     SET DEFAULT FINANCE APPROVER                 
         B     VAPP16                                                           
         CLI   0(R4),C' '                                                       
         BE    VAPP16                                                           
         CLC   0(1,R4),BC@NO                                                    
         BE    VAPP16                                                           
         MVC   FVERRNDX,MYBYTE                                                  
         B     EXITNV                                                           
*                                                                               
VAPP16   CLC   1(1,R4),BC@YES                                                   
         BNE   *+12                                                             
         OI    TLAPDT2,TLAPDTO     SET TIMEOFF                                  
         B     VALAPPX                                                          
         CLI   1(R4),C' '                                                       
         BE    VALAPPX                                                          
         CLC   1(1,R4),BC@NO                                                    
         BE    VALAPPX                                                          
         MVC   FVERRNDX,MYBYTE                                                  
         B     EXITNV                                                           
*                                                                               
VALAPPX  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* MODIFY COLUMN HEADINGS FOR 1R APPLICATION FIELD                     *         
***********************************************************************         
DHDAPP   TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BZ    EXITOK                                                           
         L     RE,SVPARMS5         HEADLINE 1                                   
         L     RF,SVPARMS6         HEADLINE 2                                   
         MVC   0(L'LC@2L1RH,RE),LC@2L1RH                                        
         MVC   0(L'LC@2L1RH,RF),HEADERU                                         
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR APPLICATION CLIENT/PRO/JOB APPROVAL                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SJA      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
SJATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSJA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSJA)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(DHDSJA)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPLICATION SJ APPROVAL INDICATOR FIELD                    *          
***********************************************************************         
         SPACE 1                                                                
DISSJA   LA    RF,TLASTAT                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DISSJA2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDASJA                                                       
         CLC   TLDASJAC,BCSPACES                                                
         BH    DISSJA2             DON'T SHOW IT IF NO DATA                     
         CLC   TLDASJOF,BCSPACES                                                
         BH    DISSJA2                                                          
         CLI   TLDASJME,C' '                                                    
         BNH   EXITOK                                                           
*                                                                               
DISSJA2  MVC   FVIFLD(5),NOS       SET FIELD TO NNNNN                           
         TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BZ    *+10                                                             
         MVC   FVIFLD+5(3),NOS                                                  
         CLC   0(2,RF),=X'0000'                                                 
         BE    EXITOK              YES - NOTHING TO DO                          
                                                                                
         TM    0(RF),TLASTIM       TIMESHEET APPROVER                           
         BZ    *+10                                                             
         MVC   FVIFLD(1),BC@YES                                                 
*                                                                               
         TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BO    DISSJA4                                                          
         TM    0(RF),TLASEXP       EXPENSE APPROVER?                            
         BZ    *+10                                                             
         MVC   FVIFLD+1(1),BC@YES                                               
         LA    R4,FVIFLD+2         NEXT COLUMN                                  
         B     DISSJA6                                                          
*                                                                               
DISSJA4  TM    1(RF),TLAEL1B       EXPENSES LEVEL 1 BILLABLE                    
         BZ    *+10                                                             
         MVC   FVIFLD+1(1),BC@YES                                               
         TM    1(RF),TLAEL1N       EXPENSES LEVEL 1 NON-BILLABLE                
         BZ    *+10                                                             
         MVC   FVIFLD+2(1),BC@YES                                               
         TM    1(RF),TLAEL2B       EXPENSES LEVEL 2 BILLABLE                    
         BZ    *+10                                                             
         MVC   FVIFLD+3(1),BC@YES                                               
         TM    1(RF),TLAEL2N       EXPENSES LEVEL 2 NON-BILLABLE                
         BZ    *+10                                                             
         MVC   FVIFLD+4(1),BC@YES                                               
         LA    R4,FVIFLD+5                                                      
*                                                                               
DISSJA6  TM    0(RF),TLASESTD      DEFAULT ESTIMATE APPROVER?                   
         BZ    *+10                                                             
         MVC   0(1,R4),BC@DEF                                                   
         TM    0(RF),TLASEST       ESTIMATE APPROVER?                           
         BZ    *+10                                                             
         MVC   0(1,R4),BC@YES                                                   
         TM    0(RF),TLASJOBD      DEFAULT JOB APPROVER?                        
         BZ    *+10                                                             
         MVC   1(1,R4),BC@DEF                                                   
         TM    0(RF),TLASJOB       JOB APPROVER ?                               
         BZ    *+10                                                             
         MVC   1(1,R4),BC@YES                                                   
         TM    1(RF),TLASESID      DEFAULT INTERNAL ESTIMATE APPROVER?          
         BZ    *+10                                                             
         MVC   2(1,R4),BC@DEF                                                   
         TM    1(RF),TLASESI       INTERNAL ESTIMATE APPROVER?                  
         BZ    EXITOK                                                           
         MVC   2(1,R4),BC@YES                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPLICATION SJ APPROVAL FIELD                             *          
***********************************************************************         
         SPACE 1                                                                
VALSJA   MVI   TLASTAT,0                                                        
         MVI   TLASTA2,0                                                        
         CLI   FVILEN,0                                                         
         BE    VSJA12                                                           
         CLC   FVIFLD(8),BCSPACES                                               
         BE    EXITNV                                                           
         TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BO    VSJA02                                                           
         CLI   FVILEN,5                                                         
         BNH   VSJA02                                                           
         MVI   FVERRNDX,5          SET CURSOR TO COLUMN 6                       
         CLI   FVIFLD+5,C' '                                                    
         BH    EXITNV                                                           
         MVI   FVERRNDX,6          SET CURSOR TO COLUMN 7                       
         CLI   FVIFLD+6,C' '                                                    
         BH    EXITNV                                                           
         MVI   FVERRNDX,7          SET CURSOR TO COLUMN 8                       
         B     EXITNV                                                           
*                                                                               
VSJA02   CLC   FVIFLD(1),BC@YES                                                 
         BNE   *+12                                                             
         OI    TLASTAT,TLASTIM                                                  
         B     VSJA04                                                           
         CLI   FVIFLD,C' '                                                      
         BE    VSJA04                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
*                                                                               
VSJA04   TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BO    VSJA06                                                           
         CLC   FVIFLD+1(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLASTAT,TLASEXP                                                  
         B     VSJA04A                                                          
         CLI   FVIFLD+1,C' '                                                    
         BE    VSJA04A                                                          
         CLC   FVIFLD+1(1),BC@NO                                                
         BE    VSJA04A                                                          
         MVI   FVERRNDX,1          SET CURSOR TO COLUMN 2                       
         B     EXITNV                                                           
VSJA04A  LA    R4,FVIFLD+2         NEXT COLUMN                                  
         MVI   MYBYTE,2            CURSOR TO NEXT COLUMN                        
         B     VSJA14                                                           
*                                                                               
VSJA06   CLC   FVIFLD+1(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLASTA2,TLAEL1B                                                  
         B     VSJA08                                                           
         CLI   FVIFLD+1,C' '                                                    
         BE    VSJA08                                                           
         CLC   FVIFLD+1(1),BC@NO                                                
         BE    VSJA08                                                           
         MVI   FVERRNDX,1          SET CURSOR TO COLUMN 2                       
         B     EXITNV                                                           
*                                                                               
VSJA08   CLC   FVIFLD+2(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLASTA2,TLAEL1N                                                  
         B     VSJA10                                                           
         CLI   FVIFLD+2,C' '                                                    
         BE    VSJA10                                                           
         CLC   FVIFLD+2(1),BC@NO                                                
         BE    VSJA10                                                           
         MVI   FVERRNDX,2          SET CURSOR TO COLUMN 3                       
         B     EXITNV                                                           
*                                                                               
VSJA10   CLC   FVIFLD+3(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLASTA2,TLAEL2B                                                  
         B     VSJA12                                                           
         CLI   FVIFLD+3,C' '                                                    
         BE    VSJA12                                                           
         CLC   FVIFLD+3(1),BC@NO                                                
         BE    VSJA12                                                           
         MVI   FVERRNDX,3          SET CURSOR TO COLUMN 4                       
         B     EXITNV                                                           
*                                                                               
VSJA12   CLC   FVIFLD+4(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLASTA2,TLAEL2N                                                  
         B     VSJA12A                                                          
         CLI   FVIFLD+4,C' '                                                    
         BE    VSJA12A                                                          
         CLC   FVIFLD+4(1),BC@NO                                                
         BE    VSJA12A                                                          
         MVI   FVERRNDX,4          SET CURSOR TO COLUMN 5                       
         B     EXITNV                                                           
VSJA12A  LA    R4,FVIFLD+5         NEXT COLUMN                                  
         MVI   MYBYTE,5            CURSOR TO NEXT COLUMN                        
*                                                                               
VSJA14   CLC   0(1,R4),BC@YES                                                   
         BNE   *+12                                                             
         OI    TLASTAT,TLASEST                                                  
         B     VSJA16                                                           
         CLC   0(1,R4),BC@DEF                                                   
         BNE   *+12                                                             
         OI    TLASTAT,TLASESTD                                                 
         B     VSJA16                                                           
         CLI   0(R4),C' '                                                       
         BE    VSJA16                                                           
         CLC   0(1,R4),BC@NO                                                    
         BE    VSJA16                                                           
         MVC   FVERRNDX,MYBYTE                                                  
         B     EXITNV                                                           
*                                                                               
VSJA16   CLC   1(1,R4),BC@YES                                                   
         BNE   *+12                                                             
         OI    TLASTAT,TLASJOB                                                  
         B     VSJA18                                                           
         CLC   1(1,R4),BC@DEF                                                   
         BNE   *+12                                                             
         OI    TLASTAT,TLASJOBD                                                 
         B     VSJA18                                                           
         CLI   1(R4),C' '                                                       
         BE    VSJA18                                                           
         CLC   1(1,R4),BC@NO                                                    
         BE    VSJA18                                                           
         LLC   RF,MYBYTE                                                        
         AHI   RF,1                                                             
         STC   RF,FVERRNDX                                                      
         B     EXITNV                                                           
*                                                                               
VSJA18   CLC   2(1,R4),BC@YES                                                   
         BNE   *+12                                                             
         OI    TLASTA2,TLASESI                                                  
         B     VALSJAX                                                          
         CLC   2(1,R4),BC@DEF                                                   
         BNE   *+12                                                             
         OI    TLASTA2,TLASESID                                                 
         B     VALSJAX                                                          
         CLI   2(R4),C' '                                                       
         BE    VALSJAX                                                          
         CLC   2(1,R4),BC@NO                                                    
         BE    VALSJAX                                                          
         LLC   RF,MYBYTE                                                        
         AHI   RF,2                                                             
         STC   RF,FVERRNDX                                                      
         B     EXITNV                                                           
*                                                                               
VALSJAX  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* MODIFY COLUMN HEADINGS FOR SJ APPLICATION FIELD                     *         
***********************************************************************         
DHDSJA   TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BZ    EXITOK                                                           
         L     RE,SVPARMS5         HEADLINE 1                                   
         L     RF,SVPARMS6         HEADLINE 2                                   
         MVC   0(L'LC@2LSJH,RE),LC@2LSJH                                        
         MVC   0(L'LC@2LSJH,RF),HEADERU                                         
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR APPLICATION 1N APPROVAL                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ONA      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
ONATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DIS1NA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAL1NA)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPLICATION 1N APPROVAL INDICATOR FIELD                    *          
***********************************************************************         
         SPACE 1                                                                
DIS1NA   LA    RF,TLASTAT                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDA1NA                                                       
         CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    0(RF),TLASTIM       TIME APPROVER                                
         BNO   *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPLICATION 1N APPROVAL FIELD                             *          
***********************************************************************         
         SPACE 1                                                                
VAL1NA   MVI   TLASTAT,0                                                        
         MVI   TLASTA2,0                                                        
         CLI   FVILEN,0                                                         
         BE    VAL1NAX                                                          
         CLC   FVIFLD(3),BCSPACES                                               
         BE    EXITNV                                                           
*                                                                               
V1NA02   CLC   FVIFLD(1),BC@YES                                                 
         BNE   *+12                                                             
         OI    TLASTAT,TLASTIM                                                  
         B     VAL1NAX                                                          
         CLC   FVIFLD(1),BCSPACES                                               
         BE    VAL1NAX                                                          
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
*                                                                               
VAL1NAX  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CLI/PRO/JOB CODE LIST                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CPJ      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
CPJTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPJ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPJC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCPJ)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CLI/PRO/JOB CODE LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISCPJ   LA    RF,TLKASJAC                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDASJAC                                                      
         MVC   FVIFLD(L'TLKASJAC),0(RF)                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CLI/PRO/JOB CODE LIST FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VALCPJC  MVC   TLRLEN,=AL2(TLSJLNQ)                                             
*&&US*&& NI    ERRIND,X'FF'-ERCOFIN                                             
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   VCPJC02                                                          
         MVC   TLKASJAC,BCSPACES                                                
         B     EXITOK                                                           
*                                                                               
T        USING APPRSTA,GSRECSTA                                                 
VCPJC02  TM    T.APPRSTA2,APPSDJAQ                                              
         BNZ   VCPJNOI                                                          
         DROP  T                                                                
*                                                                               
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         OI    APPINDS,APPICPJV                                                 
         MVC   FVIFLD+LIDASJOF-LIDASTAT(L'LIDASJOF),TLKASJOF                    
         CLC   TLKASJAC,BCSPACES    ANY CLIENT PRODUCT JOB BEFORE?              
         BNH   VCPJC0A                                                          
         LLC   RE,FVILEN                                                        
         CLM   RE,1,CLILEN         IS ENTERED LENGTH CLIENT?                    
         BE    VCPJC0A             THEN MAKE SURE WE GET CLIENT OFFICE          
         SHI   RE,1                                                             
         CLC   FVIFLD(0),TLKASJAC                                               
         EX    RE,*-6                                                           
         BE    VCPJC0B                                                          
*                                                                               
VCPJC0A  OI    APPINDS,APPICCPJ    CLI/PRO/JOB HAS BEEN CHANGED                 
*                                                                               
VCPJC0B  GOTO1 AVALCPJ,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VCPJC04                                                          
         CLC   FVILEN,CLILEN                                                    
*&&UK*&& BNE   *+14                                                             
*&&US*&& BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCLI)                                           
         B     EXITL               INVALID CLIENT CODE                          
         CLC   FVILEN,PROLEN                                                    
*&&UK*&& BNE   *+14                                                             
*&&US*&& BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INPRO)                                           
         B     EXITL               INVALID PRODUCT CODE                         
         MVC   FVMSGNO,=AL2(AE$INJOB)                                           
         B     EXITL               INVALID JOB CODE                             
*                                                                               
VCPJC04  DS    0H                  VALCPJ HAS BEEN CALLED                       
*&&US                                                                           
         TM    ERRIND,ERCOFIN                                                   
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CMCOL) CAN'T MAKE CHANGES OFF LIMITED            
         B     EXITL                                                            
*&&                                                                             
         OI    APPINDS,APPIVCPJ    VALCPJ HAS BEEN CALLED                       
         MVC   TLKASJAC,CPJCODE    SORT THE CLI/PRO/JOB CODES                   
         MVC   TLASJNM,CPJNAME     SAVE CLI/PRO/JOB NAME                        
*&&US*&& MVC   TLKASJOF,BCSPACES   CLEAR OUT OFFICE                             
*&&US*&& CLC   FVILEN,CLILEN       DON'T SAVE IF ONLY ENTERED CLIENT            
*&&US*&& BE    *+10                                                             
         MVC   TLKASJOF,CPJOFF     SAVE OFFICE CODE                             
         CLI   TLKASJME,C' '                                                    
         BH    *+10                                                             
         MVC   TLKASJME,CPJMED     SAVE MEDIA CODE                              
         B     EXITOK                                                           
*                                                                               
VCPJNOI  DS    0H                                                               
         MVC   FVMSGNO,=AL2(AE$CSODA) CAN'T SET CLI/PRO/JOB ON DEF APR.         
         MVI   FVPARMS+0,L'LC@JOBS+1                                            
         MVC   FVPARMS+1(L'LC@JOBS),LC@JOBS                                     
         MVI   FVPARMS+1+L'LC@JOBS,0                                            
         B     EXITL                                                            
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO/JOB CODE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
SRCHCPJ  DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRDUL,ACOM,    >        
               (X'13',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CLI/PRO/JOB MEDIA LIST                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CMED     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
CMEDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCMED)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCMED)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCMED)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CLI/PRO/JOB MEDIA LIST FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
DISCMED  LA    RF,TLKASJME                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDASJME                                                      
         MVC   FVIFLD(L'TLKASJME),0(RF)                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CLI/PRO/JOB MEDIA LIST FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VALCMED  MVC   TLRLEN,=AL2(TLSJLNQ)                                             
         CLI   FVILEN,L'TLKASJME                                                
         BH    EXITLONG            FIELD TOO LONG                               
         LA    RE,TLKASJAC         POINT TO C/P/J FIELD                         
         LLC   RF,PROLEN           GET CLI+PRO LENGTH                           
         AR    RE,RF               BUMP TO JOB                                  
         CLI   0(RE),C' '          TEST JOB INPUT                               
         BH    VCMED01             YES - TAKE MEDIA CODE FROM JOB               
         CLI   FVILEN,0                                                         
         BNE   VCMED04                                                          
         MVI   TLKASJME,C' '       NO MEDIA CODE INPUT                          
         B     EXITOK                                                           
VCMED01  TM    APPINDS,APPIVCPJ    TEST VALCPJ ALREADY CALLED                   
         BO    VCMED02                                                          
         OI    APPINDS,APPICPJV                                                 
         GOTO1 AVALCPJ,BOPARM,(L'TLKASJAC,TLKASJAC)                             
         OI    APPINDS,APPIVCPJ                                                 
*                                                                               
VCMED02  MVC   FVIFLD(L'CPJMED),CPJMED  TAKE MEDIA CODE FROM JOB                
         MVC   TLKASJME,FVIFLD                                                  
         OI    FVOIND,FVOXMT       TRANSMIT FIELD                               
         B     EXITOK                                                           
*                                                                               
VCMED04  MVC   TLKASJME,FVIFLD                                                  
         CLI   TLKASJME,C' '                                                    
         BNH   EXITOK                                                           
         GOTO1 ACHKFLD,BOPARM,('CKDATAQ',CKTAB1Q)   (0-9, A-Z)?                 
         BNE   EXITNV              INVALID CHARS IN THE FIELD                   
T        USING PMDRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES                                                   
         MVI   T.PMDKTYP,PMDKTYPQ  MEDIA RECORD                                 
         MVC   T.PMDKCPY,CUABIN    COMPANY                                      
         MVC   T.PMDKMED,TLKASJME  MEDIA CODE                                   
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$MDRNF)                                           
         B     EXITL               MEDIA DOES NOT EXIST                         
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO/JOB CODE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
SRCHCMED DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MEDCODE,ACOM            
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CLI/PRO/JOB OFFICE LIST                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
COFF     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
COFFTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOFF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOFF)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCOFF)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CLI/PRO/JOB CODE LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISCOFF  LA    RF,TLKASJOF                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDASJOF                                                      
         MVC   FVIFLD(L'TLKASJOF),0(RF)                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CLI/PRO/JOB CODE LIST FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VALCOFF  MVC   TLRLEN,=AL2(TLSJLNQ)                                             
         CLI   FVILEN,L'TLKASJOF                                                
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
         CLI   FVILEN,0                                                         
         BH    VCOF00                                                           
*&&US                                                                           
         TM    FVIIND,FVITHIS      ENTERED ANYTHING THIS TIME?                  
         BO    VCOF0A                                                           
         LA    RF,TLKASJAC                                                      
         LLC   R0,CLILEN                                                        
         AR    RF,R0                                                            
         CLI   0(RF),C' '          ANY PRODUCT CODE?                            
         BH    EXITOK              THEN CAN'T REMOVE OFFICE                     
         MVC   TLKASJOF,BCSPACES                                                
         B     EXITOK                                                           
*&&                                                                             
*                                                                               
VCOF0A   TM    FVIIND,FVITHIS      ENTERED ANYTHING THIS TIME?                  
         BZ    EXITOK                                                           
         LA    RF,TLKASJAC                                                      
         LLC   R0,CLILEN                                                        
         AR    RF,R0                                                            
         CLI   0(RF),C' '          ANY PRODUCT CODE?                            
         BH    VCOF00              THEN CAN'T REMOVE OFFICE                     
*                                                                               
         MVC   TLKASJOF,BCSPACES                                                
         B     EXITOK                                                           
                                                                                
VCOF00   TM    APPINDS,APPIVCPJ    TEST VALCPJ CALLED                           
         BO    VCOF02              YES - GET OFFICE CODE FROM JOB               
         OI    APPINDS,APPICPJV                                                 
         GOTO1 AVALCPJ,BOPARM,(L'TLKASJAC,TLKASJAC)                             
         OI    APPINDS,APPIVCPJ                                                 
*                                                                               
VCOF02   TM    APPINDS,APPICLIO    TEST CLIENT CODE ONLY INPUT                  
         BZ    VCOF06              NO - TAKE OFFICE FROM PROD/JOB               
         TM    FVIIND,FVITHIS      ENTERED ANYTHING THIS TIME?                  
         BZ    EXITOK                                                           
         CLC   FVIFLD,BCSPACES     ANY OFFICE ENTERED                           
         BH    VCOF03                                                           
         LA    RF,TLKASJAC                                                      
         LLC   R0,CLILEN                                                        
         AR    RF,R0                                                            
         CLI   0(RF),C' '          ANY PRODUCT CODE?                            
         BH    VODC04              THEN CAN'T REMOVE OFFICE                     
         MVC   TLKASJOF,BCSPACES                                                
         B     EXITOK                                                           
*                                                                               
VCOF03   CLC   CPJCLOF,FVIFLD      TEST CLIENT'S OWN OFFICE                     
         BE    VCOF08              OK                                           
*                                                                               
         LA    RF,CPJCOFF          ELSE ALLOW ANY OFFICE BELONGING...           
VCOF04   CLC   0(L'TRNOFFC,RF),BCSPACES ...TO A PRODUCT OF THIS CLIENT          
         BNH   EXITL               EOT - INVALID OFFICE                         
         CLC   0(L'TRNOFFC,RF),FVIFLD                                           
         BE    VCOF08                                                           
         AHI   RF,L'TRNOFFC                                                     
         B     VCOF04                                                           
VCOF06   CLC   CPJOFF,BCSPACES                                                  
         BNH   VCOF10                                                           
         MVC   FVIFLD(L'CPJOFF),CPJOFF                                          
VCOF08   MVC   TLKASJOF,FVIFLD                                                  
*&&US                                                                           
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE                                  
         BNE   EXITL               INVALID OFFICE                               
*&&                                                                             
         OI    FVOIND,FVOXMT       TRANSMIT FIELD                               
         B     EXITOK                                                           
*                                                                               
VCOF10   GOTO1 ATSTOFF,FVIFLD      TEST OFFICE                                  
         BNE   EXITL               INVALID OFFICE                               
         MVC   TLKASJOF,FVIFLD                                                  
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO/JOB CODE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
SRCHCOFF DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRDUL,ACOM,    C        
               (X'13',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A CLI/PRO/JOB CODE NAME FIELD            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CPJN     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
CPJNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPJN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN CLI/PRO/JOB CODE NAME FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISCPJN  CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLASJNM),TLASJNM                                        
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLDASJAC,BCSPACES                                                
         BNH   EXITOK                                                           
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED USER                               
         MVC   T.ACTKUNT(L'PRDUL),PRDUL                                         
         MVC   T.ACTKACT,TLDASJAC                                               
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR NON-CLIENT CODE LIST                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NCC      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
NCCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNCC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNCC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHNC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NON-CLIENT CODE LIST FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DISNCC   LA    RF,TLKA1NAC                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDA1NAC                                                      
         MVC   FVIFLD(L'TLKA1NAC),0(RF)                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE NON-CLIENT CODE LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VALNCC   MVC   TLRLEN,=AL2(TL1NLNQ)                                             
         CLI   FVILEN,0                                                         
         BNE   VNCC04                                                           
         XC    TLKA1NAC,TLKA1NAC                                                
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
VNCC04   CLI   FVILEN,L'TLKA1NAC                                                
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
         GOTO1 AVALNCL,BOPARM,(FVILEN,FVIFLD)                                   
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
         MVC   TLKA1NAC,NCLCODE    SORT THE NON CLIENT CODES                    
         MVC   TLA1NNM,NCLNAME     SAVE NON CLIENT NAME                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A NON-CLIENT CODE FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
SRCHNC   DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,NCTUL,ACOM,    X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A NON-CLIENT NAME FIELD                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NCN      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
NCNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNCN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A NON-CLIENT NAME FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISNCN   CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLA1NNM),TLA1NNM                                        
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLDA1NAC,BCSPACES                                                
         BNH   EXITOK                                                           
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED USER                               
         MVC   T.ACTKUNT(L'NCTUL),NCTUL                                         
         MVC   T.ACTKACT,TLDA1NAC                                               
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR COSTING 1R CODE LIST                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
C1R      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
C1RTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DIS1RC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAL1RC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCH1R)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COSTING 1R CODE LIST FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DIS1RC   LA    RF,TLKA1RAC                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDA1RAC                                                      
         MVC   FVIFLD(L'TLKA1RAC),0(RF)                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COSTING 1R CODE LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VAL1RC   MVC   TLRLEN,=AL2(TL1RLNQ)                                             
         CLI   FVILEN,0                                                         
         BNE   V1RC04                                                           
         XC    TLKA1RAC,TLKA1RAC                                                
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
V1RC04   CLI   TLAPDTY,0           ANY APPLICATION SELECTED ?                   
         BNE   V1RC10                                                           
         CLI   TLAPDT2,0                                                        
         BNE   V1RC10                                                           
         LH    RF,LSCURLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO 1ST INPUT FIELD                
         B     EXITNO              MISSING INPUT FIELD                          
*                                                                               
V1RC10   CLI   FVILEN,L'TLKA1NAC                                                
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
         GOTO1 AVALC1R,BOPARM,(FVILEN,FVIFLD)                                   
         BE    V1RC12                                                           
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
*&&US                                                                           
         BL    EXITL                                                            
         MVC   FVMSGNO,=AL2(AE$CMCOL) CAN'T MAKE CHANGES OFF LIMITED            
*&&                                                                             
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
V1RC12   MVC   TLKA1RAC(L'C1RCODE),C1RCODE SORT THE COSTING 1R CODES            
         MVC   TLA1RNM,C1RNAME     SAVE COSTING 1R NAME                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A COSTING 1R CODE FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
SRCH1R   DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,ONERUL,ACOM,   X        
               (X'14',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A COSTING 1R NAME FIELD                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
N1R      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
N1RTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DIS1RN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A COSTING 1R NAME FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DIS1RN   CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLA1RNM),TLA1RNM                                        
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLDA1RAC,BCSPACES                                                
         BNH   EXITOK                                                           
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED USER                               
         MVC   T.ACTKUNT(L'ONERUL),ONERUL                                       
         MVC   T.ACTKACT,TLDA1RAC                                               
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR APPROVAL ORDER TYPE                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OTY      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
OTYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOTY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOTY)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPROVAL ORDER TYPE                                        *          
***********************************************************************         
         SPACE 1                                                                
DISOTY   MVC   MYBYTES,TLKAPOTY                                                 
         CLI   CSACT,A#DLOAD       DOWNLOAD ??                                  
         JNE   DOTY00                                                           
         L     R2,ATLST                                                         
         MVC   MYBYTES,TLDAPOTY                                                 
         J     DOTY02                                                           
*                                                                               
DOTY00   OC    TLAPTYP,TLAPTYP     ANY APPROVAL BITS SET?                       
         JNZ   DOTY02                                                           
         OC    TLAPTY2,TLAPTY2                                                  
         JNZ   DOTY02                                                           
         OC    TLAPTY3,TLAPTY3                                                  
         BZ    EXITOK                                                           
*                                                                               
DOTY02   OC    MYBYTES,MYBYTES                                                  
         BZ    DISOTYX             SET TO DEFAULT IF NO ORDER TYPE              
         LA    R4,APTYTAB                                                       
         USING APTYTABD,R4                                                      
DOTY04   CLI   APTYOTY,0                                                        
         BE    DISOTYX             TYPE NOT IN TABLE                            
         CLC   APTYOTY,MYBYTES                                                  
         BE    *+12                                                             
DOTY08   LA    R4,APTYTLNQ(R4)                                                  
         B     DOTY04                                                           
         CLI   APTYCTRY,CTRYALL    TEST ALL COUNTRIES                           
         BE    *+14                                                             
         CLC   APTYCTRY,CUCTRY     ELSE MATCH ON COUNTRY                        
         BNE   DOTY08                                                           
         LH    RF,APTYDISP         DISPLACEMENT TO TYPE NAME                    
         LA    RF,DSLSTU(RF)                                                    
         MVC   FVIFLD(OTYPFLQ),0(RF) TYPE NAME                                  
         B     EXITOK                                                           
         DROP  R4                                                               
                                                                                
DISOTYX  DS    0H                  SET TO DEFAULT IF NOT FOUND                  
         CLI   CSACT,A#DLOAD                                                    
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@FLDEF),UC@FLDEF                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPROVAL ORDER TYPE                                        *         
***********************************************************************         
         SPACE 1                                                                
VALOTY   MVC   TLRLEN,=AL2(TLAPLNQ)                                             
         CLI   FVILEN,0            NO INPUT NOT VALID                           
         BE    EXITNO                                                           
*                                                                               
VOTY04   LLC   RF,FVXLEN                                                        
         LA    R4,APTYTAB                                                       
         USING APTYTABD,R4                                                      
VOTY08   CLI   APTYOTY,0                                                        
         BE    EXITNV              UNKNOWN TYPE - INVALID                       
         CLI   APTYCTRY,CTRYALL    TEST ALL COUNTRIES                           
         BE    *+14                                                             
         CLC   APTYCTRY,CUCTRY     ELSE MATCH ON COUNTRY                        
         BNE   VOTY12                                                           
         LH    R1,APTYDISP         DISPLACEMENT TO TYPE NAME                    
         LA    R1,DSLSTU(R1)                                                    
         EX    RF,*+8                                                           
         BE    VOTY16                                                           
         CLC   FVIFLD(0),0(R1)                                                  
                                                                                
VOTY12   LA    R4,APTYTLNQ(R4)                                                  
         B     VOTY08                                                           
*                                                                               
VOTY16   MVC   FVIFLD(OTYPFLQ),0(R1) SHOW FULL TYPE NAME                        
         MVC   TLKAPOTY,APTYOTY      AND SET KEY SUBCATEGORY                    
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR APPROVER APPLICATION                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APL      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
APLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPROVAL APPLICATION FIELD                                 *          
***********************************************************************         
         SPACE 1                                                                
DISAPL   CLI   CSACT,A#DLOAD                                                    
         BNE   DAPL04                                                           
         L     R2,ATLST                                                         
         LA    RF,TLDAPTYP                                                      
         OC    TLDAPVAL,TLDAPVAL                                                
         BZ    EXITOK              DON'T SHOW IT IF NO APPROVAL VALUE           
*                                                                               
DAPL04   MVC   FVIFLD(8),NOS       SET FIELD TO NNNNNN                          
         LA    RE,LIDATAB                                                       
         LA    R0,LIDATNQ                                                       
*                                                                               
DAPL06   LH    RF,4(RE)                                                         
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+8                                                              
         LH    RF,6(RE)                                                         
         AR    RF,R2               RF=A(TLAPTYP/2) OR A(TLDAPTYP/2)             
         SR    R1,R1                                                            
         IC    R1,1(RE)            R1=BIT TO TEST                               
         TM    0(RF),0                                                          
         EX    R1,*-4                                                           
         BZ    DAPL08              NOT SET - NEXT                               
         IC    R1,0(RE)                                                         
         LA    R1,FVIFLD(R1)       DISPLACE TO INPUT FIELD POSITION             
         MVC   0(1,R1),BC@YES      INIT TO YES                                  
         CLI   2(RE),YES           TEST TABLE ENTRY                             
         BE    *+10                                                             
         MVC   0(1,R1),BC@DEF      ELSE DEFAULT                                 
DAPL08   AHI   RE,LIDATLNQ                                                      
         BCT   R0,DAPL06                                                        
         B     EXITOK                                                           
                                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPROVAL APPLICATION FIELD                                *          
***********************************************************************         
         SPACE 1                                                                
VALAPL   MVI   TLAPTYP,0                                                        
         MVI   TLAPTY2,0                                                        
         MVI   TLAPTY3,0                                                        
         MVI   MYBYTE,NO                                                        
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
         CLC   FVIFLD(8),BCSPACES                                               
         BE    EXITNV                                                           
*                                  SET APPROVAL STATUS BITS                     
         LHI   R0,LIDATNQ/2        TREAT TABLE ENTRIES AS PAIRS:YES+DEF         
VALA02   LHI   R1,LIDATNQ/2                                                     
         SR    R1,R0                                                            
         LA    RF,FVIFLD(R1)       RF=A(INPUT CHAR TO VALIDATE)                 
         MHI   R1,LIDATLNQ*2                                                    
         LA    RE,LIDATAB(R1)      RE=A(TABLE ENTRY)                            
*                                                                               
         SR    R1,R1                                                            
         IC    R1,1(RE)            R1=BIT VALUE TO SET IF YES                   
         CLC   0(1,RF),BC@YES      TEST INPUT IS YES                            
         BE    VALA04                                                           
         IC    R1,LIDATLNQ+1(RE)   R1=BIT VALUE TO SET IF DEF                   
         CLC   0(1,RF),BC@DEF      TEST INPUT IS DEFAULT                        
         BE    VALA04                                                           
         CLC   0(1,RF),BC@NO       TEST INPUT IS NO                             
         BE    VALA06              **TEST                                       
         BNE   EXITNV              ELSE INVALID                                 
         B     VALA06                                                           
*                                                                               
VALA04   LH    RF,4(RE)                                                         
         AR    RF,R2               RF=A(TLAPTYP/2) - NOT TLDAPTYP/2             
         OI    0(RF),0                                                          
         EX    R1,*-4              SET THE BIT                                  
         MVI   MYBYTE,YES                                                       
*                                                                               
VALA06   BCT   R0,VALA02                                                        
         CLI   MYBYTE,YES          DO WE HAVE ANY APPLICATIONS SET?             
         BE    *+8                                                              
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
                                                                                
VALAPLX  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR APPROVAL PRODUCTION/SUPPLIER LIST                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PSC      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
PSCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPSC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPSC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHPSC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPROVAL PRODUCTION/SUPPLIER CODE LIST FIELD                *         
***********************************************************************         
         SPACE 1                                                                
DISPSC   LA    RF,TLKAPACA                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDAPACA                                                      
         CLC   0(L'TLKAPACA,RF),BCSPACES                                        
         BNH   EXITOK                                                           
         MVI   FVIFLD,C'S'                                                      
         CLI   0(RF),C'R'                                                       
         BNE   *+8                                                              
         MVI   FVIFLD,C'1'                                                      
         MVC   FVIFLD+1(L'TLKAPACA),0(RF)                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPROVAL PROD/SUPPLIER/1R CODE LIST FIELD                  *         
***********************************************************************         
         SPACE 1                                                                
VALPSC   DS    0H                                                               
*&&US*&& NI    ERRIND,X'FF'-ERCOFIN                                             
         CLI   FVILEN,0                                                         
         BNE   VPSC04                                                           
         MVC   TLKAPACA,BCSPACES                                                
         B     EXITOK                                                           
*                                                                               
VPSC04   CLC   ONERUL,FVIFLD      IF 1R ALLOW 1R ONLY ENTRY                     
         BE    *+12                                                             
         CLI   FVILEN,2                                                         
         BNH   EXITSHRT                                                         
         CLI   FVILEN,L'ACTKULA                                                 
         BH    EXITLONG           FIELD TOO LONG                                
*                                                                               
*&&UK*&& MVC   TLKAPMED,BCSPACES                                                
         CLC   PRDUL,FVIFLD       MUST BE SJ/SV/SX/1R                           
         BNE   VPSC08                                                           
         MVC   FVIFLD+2+LIDAPOFF-LIDAPTYP(L'LIDAPOFF),TLKAPOFF                  
         CLC   TLKAPACA,BCSPACES    ANY CLIENT PRODUCT JOB BEFORE?              
         BNH   VPSC0A                                                           
         LLC   RE,FVILEN                                                        
         SHI   RE,2                                                             
         CLM   RE,1,CLILEN         IS ENTERED LENGTH CLIENT?                    
         BE    VPSC0A              THEN MAKE SURE WE GET CLIENT OFFICE          
         CLC   FVIFLD+1(0),TLKAPACA                                             
         EX    RE,*-6                                                           
         BE    VPSC0B                                                           
*                                                                               
VPSC0A   OI    APPINDS,APPICCPJ    CLI/PRO/JOB HAS BEEN CHANGED                 
*                                                                               
VPSC0B   LLC   RF,FVILEN                                                        
         SHI   RF,2                                                             
         OI    APPINDS,APPIPSCV                                                 
         GOTO1 AVALCPJ,BOPARM,((RF),FVIFLD+2)                                   
         BE    VPSC0C                                                           
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
VPSC0C   DS    0H                  VALCPJ HAS BEEN CALLED                       
*&&US                                                                           
         TM    ERRIND,ERCOFIN                                                   
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CMCOL) CAN'T MAKE CHANGES OFF LIMITED            
         B     EXITL                                                            
*&&                                                                             
         OI    APPINDS,APPIVCPJ                                                 
         MVC   TLKAPACA,FVIFLD+1                                                
*        CLC   TLKAPOFF,BCSPACES                                                
*        BH    *+10                                                             
*&&US*&& MVC   TLKAPOFF,BCSPACES   CLEAR OUT OFFICE                             
*&&US*&& LLC   RE,FVILEN                                                        
*&&US*&& SHI   RE,2                                                             
*&&US*&& CLM   RE,1,CLILEN         DON'T SAVE IF ONLY ENTERED CLIENT            
*&&US*&& BE    *+10                                                             
         MVC   TLKAPOFF,CPJOFF     SAVE OFFICE CODE                             
         CLI   TLKAPMED,C' '                                                    
         BH    *+10                                                             
         MVC   TLKAPMED,CPJMED     SAVE MEDIA CODE                              
         B     EXITOK                                                           
*                                                                               
VPSC08   CLC   SVUL,FVIFLD                                                      
         BE    VPSC10                                                           
         CLC   SXUL,FVIFLD                                                      
         BE    VPSC10                                                           
         CLC   ONERUL,FVIFLD                                                    
         BE    VPSC10                                                           
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         MVC   FVXTRA(2),FVIFLD    INVALID LEDGER                               
         B     EXITL                                                            
*                                                                               
VPSC10   MVC   TLKAPACA,FVIFLD+1                                                
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ACTKULA,FVIFLD                                                 
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A APPROVAL PRODUCTION/SUPPLIER CODE FIELD                 *         
***********************************************************************         
         SPACE 1                                                                
SRCHPSC  L     RF,FVADDR           A(INPUT FIELD)                               
         CLC   PRDUL,8(RF)         SEARCH ON SJ/SV/SX ONLY                      
         BE    SRCHPS02                                                         
         CLC   SVUL,8(RF)                                                       
         BE    SRCHPS02                                                         
         CLC   ONERUL,8(RF)                                                     
         BE    SRCHPS02                                                         
         CLC   SXUL,8(RF)                                                       
         BNE   EXITOK                                                           
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
SRCHPS02 GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,ACOM,        X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A PRODUCTION/SUPPLIER NAME FIELD         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PSN      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
PSNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPSN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PRODUCTION/SUPPLIER NAME FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DISPSN   CLI   CSACT,A#DLOAD                                                    
         BNE   EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLDAPACA,BCSPACES                                                
         BNH   EXITOK                                                           
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED USER                               
         MVI   T.ACTKUNT,C'S'                                                   
         CLI   TLDAPACA,C'R'                                                    
         BNE   *+8                                                              
         MVI   T.ACTKUNT,C'1'                                                   
         MVC   T.ACTKLDG(L'TLDAPACA),TLDAPACA                                   
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR APPROVAL OFF/DEPT CODE LIST                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ODC      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
ODCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISODC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALODC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHOD)                                
         DC    AL1(DHED),AL1(0,0,0),AL4(DHDODC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPROVAL OFF/DEPT CODE LIST FIELD                           *         
***********************************************************************         
*                                                                               
DISODC   LLC   R1,LEN1RA           LENGTH OF OFFICE CODE                        
         LA    RF,FVIFLD           ADDRESS SCREEN FIELD                         
         CLI   CSACT,A#DLOAD       DOWNLOAD REQUEST ?                           
         BE    DODC20              YES, CONTINUE                                
*                                                                               
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BZ    DODC10              NO - NO OFFICE CODE                          
*                                                                               
         MVC   FVIFLD(L'TLKAPOFF),TLKAPOFF YES, SAVE OFF/OLIST CODE             
         CHI   R1,1                IS LENGTH GREATER THAN 1 BYTE ?              
         BH    DODC06              YES, CONTINUE                                
*                                  NO,                                          
         CLI   TLKAPOFF+1,X'40'    OFFICE LIST PRESENT ?                        
         BH    EXITOK              YES, EXIT                                    
*                                  NO, MUST BE 1 CHAR OFFICE                    
DODC06   LA    RF,0(R1,RF)         POINT TO END OF THE OFFICE CODE              
*                                                                               
DODC10   MVC   0(L'TLKAPDPT,RF),TLKAPDPT   SAVE DEPT CODE DETAILS               
         B     EXITOK                                                           
*                              ##  PROCESS DOWNLOAD ##                          
DODC20   L     R2,ATLST                                                         
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BZ    DODC30              NO - NO OFFICE CODE..                        
*                                                                               
         MVC   FVIFLD(L'TLDAPOFF),TLDAPOFF YES, SAVE OFFICE DETAILS             
         CHI   R1,1                IS LENGTH GREATER THAN 1 BYTE ?              
         BH    DODC24              YES, CONTINUE                                
*                                  NO,                                          
         CLI   TLDAPOFF+1,X'40'    OFFICE LIST PRESENT ?                        
         BH    EXITOK              YES, EXIT                                    
*                                  NO, MUST BE 1 CHAR OFFICE                    
DODC24   LA    RF,0(R1,RF)         POINT TO END OF THE OFFICE CODE              
*                                                                               
DODC30   MVC   0(L'TLDAPDPT,RF),TLDAPDPT  SAVE DEPRTMENT DETAILS                
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE APPROVAL OFF/DEPT CODE LIST FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
VALODC   DS    0H                                                               
         OC    TLKAPOTY,TLKAPOTY                                                
         BZ    EXITOK                                                           
         CLI   TLKAPACA,C'R'       SKIP IF 1R ACCOUNT                           
         BE    EXITOK                                                           
*                                                                               
         CLI   TLKAPACA,C'J'       TEST SJ ACCOUNT                              
         BNE   VODC08                                                           
         TM    APPINDS,APPIVCPJ    VALCPJ HAS BEEN CALLED?                      
         BO    VODC00              YES - GET/VALIDATE OFFICE CODE               
         MVC   FVIFLD+LIDAPOFF-LIDAPTYP(L'LIDAPOFF),TLKAPOFF                    
         OI    APPINDS,APPIPSCV                                                 
         GOTO1 AVALCPJ,BOPARM,(L'ACTKACT,TLKAPACA+1)                            
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    APPINDS,APPIVCPJ                                                 
*                                                                               
VODC00   TM    APPINDS,APPICLIO    TEST CLIENT CODE ONLY INPUT                  
         BZ    VODC04              NO - TAKE OFFICE FROM PROD/JOB               
         CLI   FVILEN,0            ANYTHING INPUT THIS TIME                     
         BH    VODC01                                                           
*&&US                                                                           
         TM    FVIIND,FVITHIS      ENTERED ANYTHING THIS TIME?                  
         BNZ   VODC0A                                                           
         LA    RF,TLKASJAC                                                      
         LLC   R0,CLILEN                                                        
         AR    RF,R0                                                            
         CLI   0(RF),C' '          ANY PRODUCT CODE?                            
         BH    EXITOK              THEN CAN'T REMOVE OFFICE                     
         MVC   TLKAPOFF,BCSPACES   DEFAULT OFFICE TO SPACES                     
         B     EXITOK                                                           
*&&                                                                             
VODC0A   TM    FVIIND,FVITHIS      ENTERED ANYTHING THIS TIME?                  
         BZ    EXITOK                                                           
         LA    RF,TLKASJAC                                                      
         LLC   R0,CLILEN                                                        
         AR    RF,R0                                                            
         CLI   0(RF),C' '          ANY PRODUCT CODE?                            
         BH    VODC04              THEN CAN'T REMOVE OFFICE                     
         MVC   TLKAPOFF,BCSPACES                                                
         B     EXITOK                                                           
*                                                                               
VODC01   CLC   CPJCLOF,FVIFLD      TEST CLIENT'S OWN OFFICE                     
         BE    VODC06              OK                                           
*                                                                               
         LA    RF,CPJCOFF          ELSE ALLOW ANY OFFICE BELONGING...           
VODC02   CLC   0(L'TRNOFFC,RF),BCSPACES ...TO A PRODUCT OF THIS CLIENT          
         BNH   EXITL               EOT                                          
         CLC   0(L'TRNOFFC,RF),FVIFLD                                           
         BE    VODC06                                                           
         AHI   RF,L'TRNOFFC                                                     
         B     VODC02                                                           
VODC04   MVC   FVIFLD(L'CPJOFF),CPJOFF                                          
VODC06   MVC   TLKAPOFF,FVIFLD                                                  
         OI    FVOIND,FVOXMT       TRANSMIT FIELD                               
*&&US                                                                           
         GOTO1 ATSTOFF,FVIFLD                                                   
         BNE   EXITL                                                            
*&&                                                                             
         B     EXITOK                                                           
*                                                                               
VODC08   CLI   FVILEN,0            NO INPUT IS OK                               
         BH    VODC09                                                           
         MVC   TLKAPOFF,BCSPACES                                                
         B     EXITOK                                                           
*                                                                               
VODC09   TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BO    VODC10              YES - CHECK OFFICE CODE                      
         CLI   FVILEN,L'TLKAPDPT                                                
         BH    EXITLONG                                                         
         B     VODC40                                                           
*                                                                               
VODC10   CLC   FVILEN,LEN1RA                                                    
         BL    EXITSHRT            FIELD TO SHORT                               
         BH    VODC12              OK - OFFICE + DEPT LEVEL                     
*                                  EQUAL - OFFICE LEVEL ONLY                    
         CLI   FVILEN,2            2 BYTES OFFICE CODE INPUT ?                  
         BE    VODC14              YES, CHECK FOR OFFICE LIST INPUT             
         B     VODC30              NO, MUST BE 1 BYTE, CONTINUE                 
*                                                                               
VODC12   CLC   FVILEN,LEN1RB       OFF+DEPT LENGTH ?                            
         BH    EXITLONG            FIELD TOO LONG                               
         BE    VODC30              YES, CONTINUE                                
*                                  LOWER THAN OFFICE + DEPT LENGTH              
         CLI   FVILEN,2            OLIST INPUT FOR 1 CHAR OFFICE ?              
         BNE   EXITNV              NO, INVALID INPUT FIELD                      
*                                  YES                                          
T        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
VODC14   MVC   T.OFFKEY,BCSPACES   INIT TO SPACES                               
         MVI   T.OFFKTYP,OFFKTYPQ  OFFICE RECORD TYPE                           
         MVC   T.OFFKCPY,CUABIN    COMPANY CODE                                 
         MVC   T.OFFKOFF(2),FVIFLD OFFICE CODE                                  
         MVC   SVIOKEY,IOKEY       SAVE KEY DETAILS                             
         LHI   R1,XOHI+XOACCDIR+XIO2                                            
         GOTO1 AIO                                                              
         JE    VODC16                                                           
         DC    H'0'                                                             
*                                                                               
VODC16   CLC   T.OFFKEY,SVIOKEY    RECORD FOUND FOR THE OFFICE ?                
         BNE   EXITNV              NO, PROCESS ERROR                            
*                                                                               
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         JO    VODC20              YES, SAVE OFFICE CODE DETAILS                
*                                  NO, MUST BE 1 CHAR OFFICES AGENCY            
         TM    T.OFFKSTAT,OFFSLIST OLIST INPUT ?                                
         BZ    EXITNV              NO PROCESS ERROR                             
*                                                                               
VODC20   MVC   TLKAPOFF,FVIFLD     SAVE OFFICE CODE                             
         B     EXITOK              EXIT                                         
         DROP  T                                                                
*                                                                               
VODC30   DS    0H                                                               
*&&US                                                                           
         GOTO1 ATSTOFF,FVIFLD                                                   
         BNE   EXITL                                                            
*&&                                                                             
*                                                                               
T        USING ACTRECD,IOKEY                                                    
VODC40   MVC   T.ACTKEY,BCSPACES   READ 1R ACCOUNT RECORD                       
         MVC   T.ACTKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ACTKUNT(L'OFFUL),OFFUL                                         
         LLC   RF,FVXLEN                                                        
         MVC   T.ACTKACT(0),FVIFLD                                              
         EX    RF,*-6                                                           
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BO    *+14                                                             
         MVC   TLKAPDPT,T.ACTKACT                                               
         B     EXITOK                                                           
*                                                                               
         LLC   RF,LEN1RA           RF = OFFICE LENGTH                           
         LR    R1,RF               SAVE OFFICE LENGTH                           
         SHI   RF,1                                                             
         MVC   TLKAPOFF,BCSPACES                                                
         MVC   TLKAPOFF(0),T.ACTKACT                                            
         EX    RF,*-6                                                           
         LLC   RF,LEN1RB                                                        
         SR    RF,R1               RF = DEPARTMENT LENGTH                       
         SHI   RF,1                                                             
         LA    RE,T.ACTKACT(R1)                                                 
         MVC   TLKAPDPT(0),0(RE)                                                
         EX    RF,*-6                                                           
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A APPROVAL OFF/DEPT CODE FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
SRCHOD   DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,OFFUL,ACOM,    X        
               (X'14',0)                                                        
         B     EXITOK                                                           
***********************************************************************         
* MODIFY COLUMN HEADINGS FOR OFF/DEPT FIELD                           *         
***********************************************************************         
DHDODC   TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BO    EXITOK                                                           
         L     RE,SVPARMS5         HEADLINE 1                                   
         L     RF,SVPARMS6         HEADLINE 2                                   
         MVC   0(L'LC@DPT,RE),LC@DPT                                            
         LA    R1,L'LC@DPT                                                      
         LA    R4,LC@DPT                                                        
         CLI   0(R4),C' '                                                       
         BNH   *+12                                                             
         LA    R4,1(,R4)                                                        
         BCT   R1,*-12                                                          
*                                                                               
         SHI   R1,1                                                             
         MVC   0(0,RF),HEADERU     UNDERLINE HEADING                            
         EX    R1,*-6                                                           
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFF/DEPT NAME FIELD                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ODN      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
ODNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISODN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFF/DEPT NAME FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISODN   CLI   CSACT,A#DLOAD                                                    
         BNE   EXITOK                                                           
*                                                                               
         CLI   TLDAPACA,C'R'       SKIP IF 1R                                   
         BE    EXITOK                                                           
*                                                                               
         L     R2,ATLST                                                         
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BZ    DODN10              NO OFFICE CODE , CONTINUE                    
*                                                                               
         CLC   TLDAPOFF,BCSPACES   OFFICE CODE PRESENT ?                        
         BNH   EXITOK              NO, EXIT                                     
*                                                                               
         CLI   TLDAPOFF+1,X'40'    2 BYTE OFFICE CODE OR OLIST PRESENT?         
         BNH   DODN10              NO, CONTINUE                                 
*                                  YES                                          
T        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
         MVC   T.OFFKEY,BCSPACES   INIT TO SPACES                               
         MVI   T.OFFKTYP,OFFKTYPQ  OFFICE RECORD TYPE                           
         MVC   T.OFFKCPY,CUABIN    COMPANY CODE                                 
         MVC   T.OFFKOFF(2),TLDAPOFF OFFICE CODE                                
         MVC   SVIOKEY,IOKEY       SAVE KEY DETAILS                             
         LHI   R1,XOHI+XOACCDIR+XIO2                                            
         GOTO1 AIO                                                              
         BE    DODN04                                                           
         DC    H'0'                                                             
*                                                                               
DODN04   CLC   T.OFFKEY,SVIOKEY    RECORD FOUND FOR THE OFFICE ?                
         BNE   EXITOK              NO, CONTINUE                                 
*                                                                               
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BO    DODN30              YES, CONTINUE                                
*                                                                               
         TM    T.OFFKSTAT,OFFSLIST OFFICE LIST?                                 
         BO    DODN30              YES, PROCESS NAME DETAILS                    
         B     EXITOK              NO, EXIT                                     
*                                                                               
T        USING ACTRECD,IOKEY                                                    
DODN10   MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED USER                               
         MVC   T.ACTKUNT(L'OFFUL),OFFUL                                         
         LA    RF,T.ACTKACT                                                     
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BZ    DODN20                                                           
         MVC   T.ACTKACT(L'TLDAPOFF),TLDAPOFF                                   
         LLC   R1,LEN1RA                                                        
         LA    RF,0(R1,RF)                                                      
DODN20   MVC   0(L'TLDAPDPT,RF),TLDAPDPT                                        
         CLC   T.ACTKACT,BCSPACES                                               
         BNH   EXITOK              EXIT IF NO ACCOUNT CODE                      
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
DODN30   L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* DATA OBJECT FOR APPROVER MEDIA CODE LIST                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
MED      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
MEDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMED)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMED)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHMED)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPROVER MEDIA CODE LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
DISMED   LA    RF,TLKAPMED                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDAPMED                                                      
         MVC   FVIFLD(L'TLKAPMED),0(RF)                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPROVER MEDIA CODE LIST                                   *         
***********************************************************************         
         SPACE 1                                                                
VALMED   OC    TLKAPOTY,TLKAPOTY                                                
         BZ    EXITOK                                                           
         CLI   FVILEN,L'TLKAPMED                                                
         BH    EXITLONG            FIELD TOO LONG                               
         CLI   TLDAPACA,C'R'       SKIP IF 1R                                   
         BE    EXITOK                                                           
         CLI   TLKAPACA,C'J'       TEST SJ ACCOUNT                              
         BNE   VMED04              NO THEN ANY MEDIA OK                         
*                                                                               
VMED00   LA    RE,TLKAPACA+1                                                    
         LLC   RF,PROLEN                                                        
         AR    RE,RF                                                            
         CLI   0(RE),C' '          TEST JOB SPECIFIED                           
         BNH   VMED04              NO - VALIDATE USER INPUT                     
         TM    APPINDS,APPIVCPJ    TEST VALCPJ ALREADY CALLED                   
         BO    VMED02              YES - GET MEDIA CODE FROM JOB                
         OI    APPINDS,APPIPSCV                                                 
         GOTO1 AVALCPJ,BOPARM,(L'ACTKACT,TLKAPACA+1)                            
         OI    APPINDS,APPIVCPJ                                                 
*                                                                               
VMED02   MVC   FVIFLD(L'CPJMED),CPJMED                                          
         MVC   TLKAPMED,FVIFLD                                                  
         OI    FVOIND,FVOXMT       TRANSMIT FIELD                               
         B     EXITOK                                                           
*                                                                               
VMED04   CLI   FVIFLD,C' '         BLANK IS OK                                  
         BNH   VMED06                                                           
         GOTO1 ACHKFLD,BOPARM,('CKDATAQ',CKTAB1Q)   (0-9, A-Z)?                 
         BNE   EXITNV              INVALID CHARS IN THE FIELD                   
         MVC   TLKAPMED,FVIFLD                                                  
T        USING PMDRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES                                                   
         MVI   T.PMDKTYP,PMDKTYPQ  MEDIA RECORD                                 
         MVC   T.PMDKCPY,CUABIN    COMPANY                                      
         MVC   T.PMDKMED,TLKAPMED  MEDIA CODE                                   
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$MDRNF)                                           
         B     EXITL               MEDIA DOES NOT EXIST                         
*                                                                               
VMED06   MVC   TLKAPMED,BCSPACES   CLEAR OUT MEDIA                              
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A APPROVER MEDIA CODE LIST                                *         
***********************************************************************         
         SPACE 1                                                                
SRCHMED  DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MEDCODE,ACOM            
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR APPROVAL EXPENDITURE TYPE CODE LIST                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ETC      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
ETCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISETC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALETC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHETC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPROVAL EXPENDITURE TYPE CODE FIELD                       *          
***********************************************************************         
         SPACE 1                                                                
DISETC   LA    RF,TLKAPETY                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDAPETY                                                      
         MVC   FVIFLD(L'TLKAPETY),0(RF)                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPROVAL EXPENDITURE TYPE CODE FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
VALETC   OC    TLKAPOTY,TLKAPOTY                                                
         BZ    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   TLKAPETY,BCSPACES                                                
         B     EXITOK                                                           
         CLI   FVILEN,L'TLKAPETY                                                
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
         MVC   TLKAPETY,FVIFLD                                                  
T        USING ETYRECD,IOKEY                                                    
         XC    T.ETYKEY,T.ETYKEY   READ EXPENDITURE TYPE RECORD                 
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ETYKCODE,TLKAPETY                                              
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEY(ETYKOFFC-ETYRECD),IOKEYSAV                                 
         BNE   EXITIETC            INVALID EXPENDITURE CODE                     
         CLC   T.ETYKOFFC,BCSPACES NO OFFICE CODE THEN FINE                     
         BNH   EXITOK                                                           
         MVC   MYHALF,T.ETYKOFFC   SAVED OFFICE CODE                            
         DROP  T                                                                
*                                                                               
         CLI   CUACCS,0            GLOBAL LOGON                                 
         BE    VETC14              THEN FINE TO USE IT                          
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   VETC04                                                           
         CLI   CUACCS,C'$'         LIMIT LIST LOGON?                            
         BNE   VETC10                                                           
         CLI   MYHALF,C'$'         OFFICE LIST EXPENDITURE TYPE                 
         BNE   VETC10                                                           
         CLC   MYHALF,CUACCS       CHECK WHETHER OFFICE LIST MATCHES            
         BE    EXITOK                                                           
         B     EXITIOFF            EXPENDITURE TYPE NOT VALID ON THIS           
*                                                                               
X        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
VETC04   XC    IOKEY,IOKEY                                                      
         MVI   X.OFFKTYP,OFFKTYPQ                                               
         MVC   X.OFFKCPY,CUABIN                                                 
         MVC   X.OFFKOFF,CUACCS+2                                               
         LHI   R1,XOHI+XOACCDIR+XIO2                                            
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    X.OFFKSTAT,OFFSLIST  OFFICE LIST?                                
         BZ    VETC10               NO THEN VALIDATE OFFICE                     
         CLC   MYHALF,CUACCS+2      CHECK WHETHER MATCH ON                      
         BE    EXITOK               OFFICE LIST                                 
         DROP  X                                                                
*                                                                               
VETC10   GOTO1 ATSTOFF,MYHALF                                                   
         BNE   EXITIETC             EXPENDITURE TYPE NOT VALID                  
VETC14   CLC   TLKAPOFF,BCSPACES                                                
         BNH   EXITOK                                                           
         CLC   TLKAPOFF,MYHALF                                                  
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$MIXOF)                                           
         B     EXITL               MIXED OFFICE CODE                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON APPROVAL EXPENDITURE TYPE NAME                            *         
***********************************************************************         
         SPACE 1                                                                
SRCHETC  DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,EXPTYP,ACOM,0           
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN EXPENDITURE TYPE NAME FIELD           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ETN      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
ETNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISETN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN EXPENDITURE TYPE NAME FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISETN   CLI   CSACT,A#DLOAD                                                    
         BNE   EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLDAPETY,BCSPACES                                                
         BNH   EXITOK                                                           
T        USING ETYRECD,IOKEY                                                    
         XC    T.ETYKEY,T.ETYKEY   READ EXPENDITURE TYPE RECORD                 
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ETYKCODE,TLDAPETY                                              
         DROP  T                                                                
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         CLC   IOKEY(ETYKOFFC-ETYRECD),IOKEYSAV                                 
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR APPROVAL VALUE LIST                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APV      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
APVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPV)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPROVAL VALUE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DISAPV   LA    RF,TLAPVAL                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDAPVAL                                                      
         OC    0(L'TLAPVAL,RF),0(RF)                                            
         BZ    EXITOK                                                           
         CURED (P6,(RF)),(13,FVIFLD),0,ALIGN=LEFT,COMMAS=YES,          C        
               DMCB=BODMCB                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPROVAL VALUE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
VALAPV   OC    TLKAPOTY,TLKAPOTY                                                
         BZ    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
*                                                                               
         GOTO1 AVALLMT,BOPARM,(FVILEN,FVIFLD)                                   
         BNE   EXITL                                                            
         MVC   TLAPVAL,MYPL6                                                    
         LLC   RF,APRSSEQ                                                       
         AHI   RF,1                                                             
         STC   RF,APRSSEQ                                                       
*        STC   RF,TLKSSEQ                                                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR SELF APPROVAL VALUE LIST                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SEL      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
SELTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSEL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SELF APPROVAL VALUE FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   LA    RF,TLAPSEL                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDAPSEL                                                      
         OC    0(L'TLAPSEL,RF),0(RF)                                            
         BZ    EXITOK                                                           
         CURED (P6,(RF)),(13,FVIFLD),0,ALIGN=LEFT,COMMAS=YES,          C        
               DMCB=BODMCB                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SELF APPROVAL VALUE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   OC    TLKAPOTY,TLKAPOTY                                                
         BZ    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BNE   VSEL04                                                           
         ZAP   TLAPSEL,BCPZERO                                                  
         B     EXITOK                                                           
*                                                                               
VSEL04   GOTO1 AVALLMT,BOPARM,(FVILEN,FVIFLD)                                   
         BNE   EXITL                                                            
         MVC   TLAPSEL,MYPL6                                                    
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR ESTIMATE APPROVER - DOWNLOAD REPORT                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ESTD     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
ESTDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISESTD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ESTIMATE APPROVER - DOWNLOAD REPORT                         *         
***********************************************************************         
         SPACE 1                                                                
DISESTD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO     NO IS DEFAULT                          
         TM    TLDSTA2,APPSESTQ                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FINANCE APPROVER - DOWNLOAD REPORT                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FIND     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
FINDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFIND)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FINANCE APPROVER - DOWNLOAD REPORT                          *         
***********************************************************************         
         SPACE 1                                                                
DISFIND  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSTAT,APPSFINA                                                 
         BZ    *+14                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
                                                                                
         TM    TLDSTAT,APPSFIND                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@DEF),LC@DEF                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR JOB APPROVER - DOWNLOAD REPORT                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
JOBD     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
JOBDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISJOBD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY JOB APPROVER - DOWNLOAD REPORT                              *         
***********************************************************************         
         SPACE 1                                                                
DISJOBD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSTA2,APPSRJAQ                                                 
         BZ    *+14                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
                                                                                
         TM    TLDSTA2,APPSDJAQ                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@DEF),LC@DEF                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR INTERNAL ESTIMATE APPROVER - DOWNLOAD REPORT        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
IESD     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
IESDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISIESD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INTERNAL ESTIMATE APPROVER - DOWNLOAD REPORT                *         
***********************************************************************         
         SPACE 1                                                                
DISIESD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO     NO IS DEFAULT                          
         TM    TLDSTA2,APPSESIQ                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DOWNLOAD OBJECT LEVEL 2                                             *         
***********************************************************************         
         SPACE 1                                                                
THIS     USING APPRECD,R2                                                       
LAST     USING APPRECD,R3                                                       
DOWN     NTRDO                                                                  
*                                                                               
DOWNTBL  DC    AL1(DPQINIT),AL1(0,0,0),AL4(DLPQINI)                             
         DC    AL1(DDLINIT),AL1(0,0,0),AL4(DLINIT)                              
         DC    AL1(DAPPCOL),AL1(0,0,0),AL4(DLSCOL)                              
         DC    AL1(DSCREEN),AL1(0,0,0),AL4(DLSCR)                               
         DC    AL1(DSETCOLS),AL1(0,0,0),AL4(DLSETC)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE PRINT QUEUE                                              *         
***********************************************************************         
         SPACE 1                                                                
DLPQINI  MVC   INSYSID,=C'AC'                                                   
*&&UK*&& MVC   INPRGID,=C'FL'      REPORT PROGRAM ID                            
*&&UK*&& MVC   INJCLID,=C'FL'      REPORT JCL ID                                
*&&US*&& MVC   INPRGID,=C'AF'      REPORT PROGRAM ID                            
*&&US*&& MVC   INJCLID,=C'AF'      REPORT JCL ID                                
         MVI   INPRTY1,0                                                        
         MVI   INPRTY2,0                                                        
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         MVC   INOTYP,=CL6'DOWN'   DOWNLOAD TYPE                                
         MVC   REPSUBID,INUSER     SET REQUESTOR ID                             
         MVC   REPSYSID,INSYSID                                                 
         MVC   REPPRGID,INPRGID                                                 
         MVC   INDEST,REPUSRID     THE CONTROLLER RESET INDEST!!                
         OI    REPIND2,REPILOW                                                  
         OI    REPHEADI,REPHSPAC+REPHCLRA                                       
         OI    REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVC   REPRLH,=Y(48)                                                    
         MVC   REPRDH,=Y(12)                                                    
*                                                                               
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   EXITOK                                                           
         L     R0,=F'9000000'      YES-GET 9M FOR CONTROLLER'S TSAR             
         STCM  R0,15,LTSOBUF       BUFFER (WAS 5M)                              
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,ATSOBUF                                                    
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR DOWNLOAD LIST                                        *         
***********************************************************************         
         SPACE 1                                                                
DLINIT   DS    0H                                                               
         OI    LSSTAT1,LSSTSAR     LIST IS TSAR RECORDS ONLY                    
         NI    APPINDS,FF-APPIDLFC                                              
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* PRINT SCREEN PAGE 1                                                 *         
***********************************************************************         
         SPACE 1                                                                
DLSCR    B     EXITH               LET CONTROLLER PRINT SCREEN PAGE 1           
         SPACE 2                                                                
***********************************************************************         
* SET COLUMNS FOR DOWNLOAD REPORT                                     *         
***********************************************************************         
         SPACE 1                                                                
GSFRP    USING FRPELD,GSFRPEL                                                   
DLSCOL   LA    RE,DLCLMSX                                                       
         STH   RE,LSVARNUM         N'COLUMNS                                    
         LA    R5,LSVARCLM                                                      
         USING DCTABD,R5                                                        
         LA    RF,DLCLMS                                                        
         MVI   MYBYTE,1                                                         
*                                                                               
DLSCOL02 LH    R1,0(RF)                                                         
         CHI   R1,F#APP#APCOFF     TEST CLIENT OFFICE FIELD                     
         BNE   DLSCOL04                                                         
         TM    BCCPYST1,CPYSOROE   ARE WE ON OFFICE BASED AGENCY                
         BZ    DLSCOL06            NO - DON'T SHOW THIS FIELD                   
*                                                                               
DLSCOL04 XC    0(DCTABL,R5),0(R5)  CLEAR COLUMN ENTRY                           
         MVC   DCTFLD#,0(RF)       SET FIELD NUMBER                             
         MVC   DCTCOL#,MYBYTE      SET COLUMN NUMBER                            
         LLC   R1,MYBYTE                                                        
         AHI   R1,1                                                             
         STC   R1,MYBYTE           NEXT COLUMN NUMBER                           
         LA    R5,DCTABL(,R5)                                                   
DLSCOL06 LA    RF,2(,RF)                                                        
         BCT   RE,DLSCOL02         SET NEXT COLUMN                              
*                                                                               
         LLC   R1,MYBYTE                                                        
         SHI   R1,1                                                             
         STH   R1,LSVARNUM         UPDATE N'COLUMNS                             
         XC    0(DCTABL,R5),0(R5)                                               
         MVI   GSFRP.FRPTYPE,FRPTDWN                                            
         XC    LSFIXNUM,LSFIXNUM   NO FIXED COLUMNS                             
         B     EXITOK                                                           
         DROP  R5,GSFRP                                                         
         SPACE 4                                                                
DLCLMS   DC    AL2(F#APP#APPIDL)   PID CODE LIST                                
         DC    AL2(F#APP#APPNMD)   APPROVER NAME LIST                           
         DC    AL2(F#APP#ESTFD)    ESTIMATE APPROVER                            
         DC    AL2(F#APP#EXPFD)    EXPENSE APPROVER                             
         DC    AL2(F#APP#JOBFD)    JOB APPROVER                                 
         DC    AL2(F#APP#IESTFD)   INTERNAL ESTIMATE APPR                       
         DC    AL2(F#APP#BAKAPD)   BACKUP APPROVER APPLICATION                  
         DC    AL2(F#APP#BUAPDL)   BACK UP APPROVER PID LIST                    
         DC    AL2(F#APP#BUANMD)   BACK UP APPROVER NAME LIST                   
         DC    AL2(F#APP#APSJA)    APPLICATION INDICATOR SJ                     
         DC    AL2(F#APP#APCPJ)    CLI/PRO/JOB CODE LIST                        
         DC    AL2(F#APP#APCOFF)   CLI/PRO/JOB OFFICE LIST                      
         DC    AL2(F#APP#APCMED)   CLI/PRO/JOB MEDIA LIST                       
         DC    AL2(F#APP#APCPJN)   CLI/PRO/JOB NAME LIST                        
         DC    AL2(F#APP#TIME)     TIME 1N APPLICATION INDICATOR                
         DC    AL2(F#APP#APNCC)    NON-CLIENT TIME CODE LIST                    
         DC    AL2(F#APP#APNCN)    NON-CLIENT TIME NAME LIST                    
         DC    AL2(F#APP#APPLI)    APPLICATION INDICATOR                        
         DC    AL2(F#APP#AP1RC)    COSTING 1R CODE LIST                         
         DC    AL2(F#APP#AP1RN)    COSTING 1R NAME LIST                         
         DC    AL2(F#APP#APOTY)    APPROVER ORDER TYPE LIST                     
         DC    AL2(F#APP#APAPL)    APPROVER APPLICATION LIST                    
         DC    AL2(F#APP#APPSC)    PROD/SUPPLIER CODE LIST                      
         DC    AL2(F#APP#APPSN)    PROD/SUPPLIER (DOWNLOAD)                     
         DC    AL2(F#APP#APODC)    OFF/DEPT CODE LIST                           
         DC    AL2(F#APP#APODN)    OFF/DEPT NAME (DOWNLOAD)                     
         DC    AL2(F#APP#APMED)    MEDIA CODE LIST                              
         DC    AL2(F#APP#APETC)    ETYPE CODE LIST                              
         DC    AL2(F#APP#APETN)    ETYPE NAME (DOWNLOAD)                        
         DC    AL2(F#APP#APVAL)    APPROVAL VALUE LIST                          
         DC    AL2(F#APP#APSEL)    SELF APPROVAL VALUE                          
DLCLMSX  EQU   (*-DLCLMS)/2                                                     
         SPACE 2                                                                
***********************************************************************         
* SET DOWNLOAD COLUMN HEADINGS                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING FDRELD,R3                                                        
DLSETC   DS    0H                                                               
*&&US*&& OI    DLINDS,DLBALL       BUILD LIST IN ONE GO                         
         CLC   FDRNUM,=AL2(F#APP#APSJA)                                         
         BNE   DLSETC02                                                         
         TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@2LSJH),LC@2LSJH                                      
         B     EXITOK                                                           
*                                                                               
DLSETC02 CLC   FDRNUM,=AL2(F#APP#APPLI)                                         
         BNE   DLSETC04                                                         
         TM    SVCPXST6,CPX2LAEI   2 LEVEL APPROVAL                             
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@2L1RH),LC@2L1RH                                      
         B     EXITOK                                                           
*                                                                               
DLSETC04 CLC   FDRNUM,=AL2(F#APP#APODC)                                         
         BNE   EXITOK                                                           
         TM    BCCPYST1,CPYSOROE   TEST COMPANY FULLY OFFICE BASED?             
         BO    EXITOK                                                           
         MVC   FVIFLD(L'LC@DPT),LC@DPT                                          
         B     EXITOK                                                           
         SPACE 2                                                                
FIL44    CSECT                                                                  
*                                                                               
***********************************************************************         
* OPTIONS OBJECT                                                      *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS DEFAULT HELP NUMBER IN 1ST BYTE. CHANGE IF REQUIRED.       *         
***********************************************************************         
         SPACE 1                                                                
OPT      LM    R0,R3,SVPARMS                                                    
         LA    RF,OPTTABL1                                                      
         B     ITER                                                             
*                                                                               
OPTTABL1 DC    AL1(OHLP),AL1(0,0,0),AL4(OPTHLP)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* OPTION HELP HOOK                                                    *         
***********************************************************************         
         SPACE 1                                                                
OPTHLP   CLI   CSACT,A#LST         LIST USES OPTIONS                            
         BE    EXITOK              HELP=DEFAULT                                 
         B     EXITL               OTHERWISE OPTIONS NOT USED.                  
         EJECT ,                                                                
***********************************************************************         
* NTRSES OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                  *         
***********************************************************************         
         SPACE 1                                                                
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   NTRO02                                                           
         CLI   SREC,R#APPRO        APPROVER RECORD                              
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   NTRO02                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
*                                                                               
NTRO02   CLI   CSREC,R#APPRO                                                    
         BNE   EXITOK                                                           
         CLI   CSACT,A#LST                                                      
         BNE   EXITOK                                                           
         CLI   CSACT,A#DLOAD                                                    
         BE    EXITOK                                                           
         L     R4,ATLST                                                         
         USING TLSTD,R4                                                         
         MVC   SVPID,TLKPID                                                     
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING APPRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING APPRECD,R2                                                       
LAST     USING APPRECD,R3                                                       
         LA    RF,LISTABL                                                       
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMPAGE    CHECK PAGE OK                                
         BE    LITER04             (0 FOR LIST AND 254 FOR DOWNLOAD)            
         LA    RF,OBJTABL(,RF)                                                  
         B     LITER                                                            
*                                                                               
LITER04  ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR)                            
         DC    AL1(LLSTLAST),AL1(0,0,0),AL4(SCRLAST)                            
         DC    AL1(LSCRLAST),AL1(0,0,0),AL4(SCRLAST)                            
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,254),AL4(FLST)                             
         DC    AL1(LGETNEXT),AL1(0,0,254),AL4(NLST)                             
         DC    AL1(LTSARDIR),AL1(0,0,254),AL4(TSARDIR)                          
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
*&&US*&& DC    AL1(LSCRLAST),AL1(0,0,1),AL4(SCRLAST1)                           
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,2),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,2),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,2),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,2),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,2),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,2),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,2),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,2),AL4(UPDLAST1)                           
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,3),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,3),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,3),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,3),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,3),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,3),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,3),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,3),AL4(UPDLAST1)                           
*&&US*&& DC    AL1(LSCRLAST),AL1(0,0,3),AL4(SCRLAST1)                           
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,4),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,4),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,4),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,4),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,4),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,4),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,4),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,4),AL4(UPDLAST1)                           
*&&US*&& DC    AL1(LSCRLAST),AL1(0,0,4),AL4(SCRLAST1)                           
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,5),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,5),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,5),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,5),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,5),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,5),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,5),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,5),AL4(UPDLAST1)                           
*&&US*&& DC    AL1(LSCRLAST),AL1(0,0,5),AL4(SCRLAST1)                           
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INIT LIST                                                           *         
***********************************************************************         
         SPACE 1                                                                
ILST     OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         NI    APPINDS,FF-(APPIDLFC+APPICCPJ)                                   
         MVI   ERRIND,0                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING APPRECD,IOKEY                                                    
FLST     CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BE    FLST04                                                           
         TM    LSSCIND1,LSSCIFLT   TEST FILTERS CHANGED                         
         BO    FLST02              YES, IGNORE ERRIND                           
         TM    ERRIND,ERMAXIO                                                   
         BZ    FLST02                                                           
         MVC   THIS.APPKEY,SKEYLAST                                             
FLST02   NI    ERRIND,FF-ERMAXIO   RESET ERROR INDICATOR                        
         XC    SKEYLAST,SKEYLAST   OR CLEAR SAVED LAST KEY                      
*                                                                               
FLST04   MVC   X.APPKEY,THIS.APPKEY                                             
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    *+12                                                             
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
         TM    APPINDS,APPIDLFC    RESET FLAG ONLY IF IT'S FIRST CALL           
         BO    *+12                                                             
         MVI   DWNINDS,DWNGDATA    GET DATA INTO TSAR                           
         MVI   DWNINDS2,X'00'                                                   
         OI    APPINDS,APPIDLFC    FLST HAS ALREADY BEEN CALLED                 
         B     NLST06                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BNE   NLST02                                                           
         TM    DWNINDS,DWNNOALL    ANY NORE LIST ELEMENTS ?                     
         BNO   NLST12              YES                                          
         TM    DWNINDS2,DWN2OALL                                                
         BNO   NLST12              YES                                          
*                                                                               
NLST02   MVI   DWNINDS,DWNGDATA    GET DATA INTO TSAR                           
         MVI   DWNINDS2,X'00'                                                   
         CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BNE   NLST04                                                           
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
*                                                                               
NLST04   L     R1,=AL4(XOSQD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    NLST06                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
*                                                                               
NLST06   CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BE    NLST08                                                           
         GOTO1 AIOCHK              CHECK FOR MAX IOS                            
         BE    NLST08                                                           
         OI    ERRIND,ERMAXIO                                                   
         MVC   SKEYLAST,IOKEY      WHEN RESUMED, START HERE                     
         B     EXITL                                                            
*                                                                               
NLST08   CLC   X.APPKEY(APPKREM-APPRECD),THIS.APPRECD                           
         BNE   EXITL               CHANGE OF TYPE/SUBTYPE/CPMPANY               
         CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BE    NLST10                                                           
         OC    X.APPKSEQ,X.APPKSEQ IS IT SEQUENTIAL                             
         BNZ   NLST                SKIP SEQUENTIAL FROM LIST                    
         CLI   CRECDEL,0           NO FILTER - DEFAULT IS DELETE=NO             
         BE    NLST10                                                           
         CLI   CRECDEL,YES         DELETE=YES                                   
         BE    NLST12                                                           
         CLI   CRECDEL,ONLY        DELETE=ONLY                                  
         BNE   NLST10                                                           
         TM    IOERR,IOEDEL        TEST IF RECORD IS DELETED                    
         BZ    NLST                NO - GET NEXT                                
         B     NLST12                                                           
*                                                                               
NLST10   TM    IOERR,IOEDEL        IT MUST BE DELETE=NO                         
         BO    NLST                                                             
*                                                                               
NLST12   MVC   THIS.APPKEY(ACCKLEN),IOKEY  WE WANT THIS KEY HERE...             
         CLI   CSACT,A#DLOAD               DOWNLOADING?                         
         BNE   *+12                                                             
         CLI   DWNINDS,DWNGDATA            FIRST TIME READING RECORD?           
         BNE   EXITOK                      NO - SKIP CALLING DOFLT              
         GOTO1 ADOFLT                      FILTER UNWANT RECORDS                
         BE    EXITOK                                                           
         CLI   CSACT,A#DLOAD               DOWNLOADING?                         
         BNE   NLST                                                             
         MVI   DWNINDS,DWNNOALL            YES - READ THE NEXT RECORD?          
         MVI   DWNINDS2,DWN2OALL                                                
         B     NLST                                                             
         DROP  THIS,LAST,X                                                      
         SPACE 2                                                                
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM FILE                               *         
***********************************************************************         
         SPACE 1                                                                
         USING APPRECD,R2                                                       
         USING TLSTD,R3                                                         
TSARDIR  LM    R2,R3,SVPARMS3                                                   
*                                                                               
         MVC   TLKPID,SVPID                                                     
         CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   TLRLEN,=AL2(TLLSLNQ) LENGTH OF TSAR RECORD FOR LIST              
         B     EXITOK                                                           
*                                                                               
         L     R2,AIOREC                                                        
         TM    DWNINDS,DWNGDATA     GET DATA FROM RECORD                        
         BZ    TSDIR10              NO - PUT DATA INTO TSAR                     
         LHI   R1,XOGET+XOACCMST+XIO11                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
*                                                                               
         XC    MNTDISPD(MNTDISPL),MNTDISPD                                      
         GOTO1 AGETNLE,BOPARM,('LIDTAPSJ',MNTCPJ),APPRECD                       
         BE    *+8                                                              
         OI    DWNINDS,DWNNOCPJ                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTAP1N',MNTNCLC),APPRECD                      
         BE    *+8                                                              
         OI    DWNINDS,DWNNONCC                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTAP1R',MNTAP1R),APPRECD                      
         BE    *+8                                                              
         OI    DWNINDS,DWNNOC1R                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTINOR',MNTAPPL),APPRECD                      
         BE    *+8                                                              
         OI    DWNINDS,DWNNOAPL                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTBACK',MNTBACK),APPRECD                      
         BE    *+8                                                              
         OI    DWNINDS2,DWNNOBAK                                                
*                                                                               
TSDIR10  NI    DWNINDS,FF-DWNGDATA                                              
         L     R2,AIOREC            ADDRESS OF APPROVER RECORD                  
         MVC   TLRLEN,=AL2(TLDLLNQ) LENGTH OF TSAR RECORD FOR DOWNLOAD          
         XC    TLDLDAT(TLDLDATL),TLDLDAT                                        
*                                                                               
         MVC   TLDSTAT,APPRSTAT                                                 
         MVC   TLDSTA2,APPRSTA2                                                 
*                                                                               
         TM    DWNINDS,DWNNOCPJ                                                 
         BO    TSDIR14                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTAPSJ',MNTCPJ),APPRECD                       
         BE    *+8                                                              
         OI    DWNINDS,DWNNOCPJ                                                 
         L     RF,8(R1)                                                         
         USING LIDDATA,RF                                                       
         MVC   TLDASJA,LIDASTAT                                                 
         MVC   TLDASJA2,LIDASTA2                                                
         MVC   TLDASJAC,LIDASJAC                                                
         MVC   TLDASJME,LIDASJME                                                
         MVC   TLDASJOF,LIDASJOF                                                
         DROP  RF                                                               
*                                                                               
TSDIR14  TM    DWNINDS,DWNNONCC                                                 
         BO    TSDIR18                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTAP1N',MNTNCLC),APPRECD                      
         BE    *+8                                                              
         OI    DWNINDS,DWNNONCC                                                 
         L     RF,8(R1)                                                         
         USING LIDDATA,RF                                                       
         MVC   TLDA1NA,LIDASTAT                                                 
         MVC   TLDA1NA2,LIDASTA2                                                
         MVC   TLDA1NAC,LIDA1NAC                                                
         DROP  RF                                                               
*                                                                               
TSDIR18  TM    DWNINDS,DWNNOC1R                                                 
         BO    TSDIR22                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTAP1R',MNTAP1R),APPRECD                      
         BE    *+8                                                              
         OI    DWNINDS,DWNNOC1R                                                 
         L     RF,8(R1)                                                         
         USING LIDDATA,RF                                                       
         MVC   TLDAPDTY,LIDAPDTY                                                
         MVC   TLDAPDT2,LIDAPDT2                                                
         MVC   TLDA1RAC,LIDAPACC                                                
         DROP  RF                                                               
*                                                                               
TSDIR22  TM    DWNINDS,DWNNOAPL                                                 
         BO    TSDIR24                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTINOR',MNTAPPL),APPRECD                      
         BE    *+8                                                              
         OI    DWNINDS,DWNNOAPL                                                 
         L     RF,8(R1)                                                         
         USING LIDDATA,RF                                                       
         MVC   TLDAPTYP,LIDAPTYP                                                
         MVC   TLDAPTY2,LIDAPTY2                                                
         MVC   TLDAPTY3,LIDAPTY3                                                
         MVC   TLDAPOTY,LIDAPSCT                                                
         MVC   TLDAPACA,LIDAPAC                                                 
         MVC   TLDAPMED,LIDAPMED                                                
         MVC   TLDAPOFF,LIDAPOFF                                                
         MVC   TLDAPDPT,LIDAPDPT                                                
         MVC   TLDAPETY,LIDAPETY                                                
         MVC   TLDAPVAL,LIDAPVAL                                                
         MVC   TLDAPSEL,LIDAPSEL                                                
         DROP  RF                                                               
*                                                                               
TSDIR24  TM    DWNINDS2,DWNNOBAK                                                
         BO    TSARDIRX                                                         
         GOTO1 AGETLIT,BOPARM,('LIDTBACK',MNTBACK),APPRECD                      
         BE    *+8                                                              
         OI    DWNINDS2,DWNNOBAK                                                
         L     RF,8(R1)                                                         
         USING LIDDATA,RF                                                       
         MVC   TLDBUPID,LIDLPID                                                 
         MVC   TLDBUAPP,LIDLAPPL                                                
         MVC   TLDBUAP2,LIDLAPP2                                                
         DROP  RF                                                               
*                                                                               
TSARDIRX B     EXITOK                                                           
         DROP  R2,R3                                                            
***********************************************************************         
* LAST FOR LIST SCREEN                                                *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  TM    ERRIND,ERMAXIO      MAX IOS SET FROM NLST?                       
         BZ    EXITOK                                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$MAXIO)                                           
         NI    LSLTIND1,FF-LSLTIEOL    NOT DONE YET                             
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1/2/3/4                                 (LINIT) *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         OI    LSSTAT2,LSSADD+LSSNOSEQ                                          
         NI    APPINDS,X'FF'-APPICCPJ                                           
         MVC   LSCOLLIN,=AL2(78)  NUMBER OF COLUMNS PER LIST LINE               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1/2/3/4                              (LLSTFRST) *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LHI   RF,APPRFST-APPRECD                                               
         STH   RF,MNTDISP                                                       
         MVI   READSEQ#,0                                                       
*&&US*&& NI    ERRIND,FF-(EREOFIN+ERCOFIN)  RESET ERROR INDICATOR               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1/2/3/4                                   (LGETFRST) *         
* BUILD LSTBLK FROM APPROVER RECORD LIDELS                            *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
         XC    APRSSEQ,APRSSEQ                                                  
*                                                                               
         AR    R5,R1               A(RECORD)                                    
         CR    R5,R1               MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    R5,APPRFST-APPRECD(R5) IT IS NOW.                                
*                                                                               
         B     NML02                                                            
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1/2/3/4                                               *         
***********************************************************************         
         SPACE 1                                                                
NLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    R5,R1               A(RECORD)                                    
         CR    R5,R1               MAKE SURE MNTDISP INITIALISED                
         BH    NML22                                                            
         LA    R5,APPRFST-APPRECD(R5) IT IS NOW.                                
*                                                                               
         USING LIDELD,R5                                                        
NML02    CLI   LIDEL,0             RECORD END?                                  
         BNE   NML04               YES                                          
         GOTO1 ANXTAPR,BOPARM,AIOREC                                            
         BNE   EXITL               NO MORE RECORD                               
         LHI   RF,APPRFST-APPRECD                                               
         STH   RF,MNTDISP                                                       
         L     R5,AIO5                                                          
         AR    R5,RF               NEW RECORD IN AIO5                           
*                                                                               
NML04    CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML18               YES                                          
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   NML06               NO                                           
         CLI   LIDTYPE,LIDTAPSJ    IS IT CLI/PRO/JOB LIST                       
         BE    NML20                                                            
         B     NML18                                                            
NML06    CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   NML08               NO                                           
         CLI   LIDTYPE,LIDTAP1N    IS IT NON CLIENT ACCOUNT LIST                
         BE    NML20                                                            
         B     NML18                                                            
NML08    CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R ACCOUNT PAGE            
         BNE   NML10               NO                                           
         CLI   LIDTYPE,LIDTAP1R    IS IT COSTING 1R LIST                        
         BE    NML20                                                            
         B     NML18                                                            
NML10    CLI   GSSMPAGE,APPLISTQ   ARE WE ON OFFICE VALUE PAGE                  
         BNE   NML12                                                            
         CLI   LIDTYPE,LIDTINOR    IS IT OFFICE VALUE LIST                      
         BE    NML20                                                            
         B     NML18                                                            
NML12    CLI   GSSMPAGE,BAKLISTQ   ARE WE ON BACKUP APPROVER PAGE               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LIDTYPE,LIDTBACK    IS IT BACKUP APPROVER ELEMENT                
         BE    NML20                                                            
         B     NML18                                                            
*                                                                               
NML18    LLC   RE,LIDLN            GET NEXT ELEMENT                             
         AR    R5,RE                                                            
         B     NML02                                                            
*                                                                               
NML20    LA    R4,LIDDATA          ADDRESS OF DATA START OF ELEMENT             
         LR    RF,R4                                                            
         SR    RF,R5                                                            
         STH   RF,CURDISP          CURRENT DISPLACEMENT INTO ELEMENT            
         LLC   RF,LIDITLN          GET LENGTH OF DATA                           
         STH   RF,DATALEN          SAVE THIS LENGTH                             
         LLC   RF,LIDLN                                                         
         STH   RF,TOTELLN          SAVE TOTAL ELEMENT LENGTH                    
         B     NML24                                                            
*                                                                               
NML22    LH    R4,DATALEN          LENGTH OF DATA                               
         AH    R4,CURDISP          CURRENT DISPLACEMENT TO DATA                 
         STH   R4,CURDISP          SAVE NEXT DISPLACEMENT TO DATA               
         CH    R4,TOTELLN          HAVE WE REACHED END OF ELEMENT               
         BNL   NML18               YES                                          
         AR    R4,R5                                                            
*                                                                               
NML24    CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   NML26               NO                                           
         L     RF,AVALCPJ                                                       
         B     NML40                                                            
NML26    CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   NML28               NO                                           
         L     RF,AVALNCL          SET TO VALIDATE EXPENDITURE TYPE             
         B     NML40                                                            
NML28    CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R ACCOUNT PAGE            
         BNE   NML30                                                            
         L     RF,AVALC1R          SET TO VALIDATE COSTING 1R ACCOUNT           
         B     NML40                                                            
NML30    CLI   GSSMPAGE,APPLISTQ   ARE WE ON OFFICE VALUE PAGE  PAGE            
         BNE   NML32                                                            
         L     RF,AGETAPL          SET TO VALIDATE OFFICE                       
         B     NML40                                                            
NML32    CLI   GSSMPAGE,BAKLISTQ   ARE WE ON BACKUP APPROVER PAGE?              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AGETBAK          SET TO VALIDATE BACKUP APPROVER              
                                                                                
*                                                                               
NML40    GOTO1 (RF),BOPARM,(0,(R4))                                             
         BNE   NML22               INVALID CLI/PRO/JOB CODE                     
*&&US                                                                           
         TM    ERRIND,ERCOFIN      IS OFFICE CODE VALID                         
         BNO   *+8                                                              
         OI    ERRIND,EREOFIN      EXISTING OFFICE IS INVALID                   
*&&                                                                             
                                                                                
NML80    L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
                                                                                
         SR    R5,R1                                                            
         STH   R5,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   TSFL02              NO                                           
         MVC   TLRLEN,=AL2(TLSJLNQ)                                             
         MVC   TLASTAT(L'CPJAPTY),CPJAPTY     APPLICATION                       
         MVC   TLKASJAC,CPJCODE                                                 
         MVC   TLKASJME,CPJMED                                                  
         MVC   TLKASJOF,CPJOFF                                                  
         MVC   TLASJNM,CPJNAME                                                  
         B     EXITOK                                                           
*                                                                               
TSFL02   CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON-CLIENT PAGE                    
         BNE   TSFL08                                                           
         MVC   TLRLEN,=AL2(TL1NLNQ)                                             
         MVC   TLASTAT(L'NCLAPTY),NCLAPTY                                       
         MVC   TLKA1NAC,NCLCODE                                                 
         MVC   TLA1NNM,NCLNAME                                                  
         B     EXITOK                                                           
*                                                                               
TSFL08   CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R LIST PAGE?              
         BNE   TSFL10                                                           
         MVC   TLRLEN,=AL2(TL1RLNQ)                                             
         MVC   TLAPDTY,C1RDTY      APPLICATION                                  
         MVC   TLAPDT2,C1RDT2      APPLICATION #2                               
         MVC   TLKA1RAC,C1RCODE                                                 
         MVC   TLA1RNM,C1RNAME                                                  
         B     EXITOK                                                           
*                                                                               
TSFL10   CLI   GSSMPAGE,APPLISTQ   ARE WE ON OFFICE VALUE PAGE                  
         BNE   TSFL12                                                           
         MVC   TLRLEN,=AL2(TLAPLNQ)                                             
         MVC   TLAPTYP,APRTYP                                                   
         MVC   TLAPTY2,APRTY2                                                   
         MVC   TLAPTY3,APRTY3                                                   
         MVC   TLKAPOTY,APROTY                                                  
         MVC   TLKAPOFF,APROFF                                                  
         MVC   TLKAPDPT,APRDPT                                                  
         MVC   TLKAPETY,APRETY                                                  
         MVC   TLKAPACA,APRACA                                                  
         MVC   TLKAPMED,APRMED                                                  
*        MVC   TLKSSEQ,APRSSEQ                                                  
*        MVC   TLKAPOTY,APROTY                                                  
***      MVC   TLKAPLM,APRAPL                                                   
         MVC   TLAPVAL,APRVAL                                                   
         MVC   TLAPSEL,APRSEL                                                   
         B     EXITOK                                                           
*                                                                               
TSFL12   CLI   GSSMPAGE,BAKLISTQ   ARE WE ON BACKUP APPROVER PAGE               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TLRLEN,=AL2(TLBKLNQ)                                             
         MVC   TLKBCPID,BAKCPID    CHAR PID                                     
         MVC   TLKBBPID,BAKBPID    BINARY PID                                   
         MVC   TLBAPP,APRTYP       APPROVAL STATUS APPLICATION                  
         MVC   TLBAPP2,APRTY2                                                   
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* LAST FOR LIST SCREEN PAGE 1/3/4/5                                   *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
SCRLAST1 DS    0H                  MAX IOS SET FROM NLST?                       
         TM    ERRIND,EREOFIN         WE HAVE AN INVALID OFF ON LIST.           
         BNO   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$WAOOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         OI    GCINDS3,GCIDSMSG    SET OWN DISPLAY MESSAGE                      
         MVI   FVOSYS,QSACC                                                     
         CLI   CSACT,A#DIS                                                      
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CMCOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         B     EXITL                                                            
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE PAGE 1/2/3/4                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING APPRECD,R2                                                       
UPDFRST1 MVI   ANYLINES,NO                                                      
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         L     R2,AIOREC                                                        
         CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R ACCOUNT PAGE            
         BNE   UPDF10                                                           
         MVI   CURITEM,0                                                        
         L     RE,AIO5             CLEAR IO5                                    
         LA    RF,IOAREALN                                                      
         XCEF                                                                   
         B     UPDF10                                                           
*                                                                               
T        USING APPRECD,IOKEY                                                    
UPDF02   CLI   APPKSEQ,0                                                        
         BNE   UPDF04                                                           
         MVC   IOKEY(L'APPKEY),APPKEY                                           
         B     UPDF08                                                           
*                                                                               
UPDF04   CLI   APPRFST,0           EMPTY RECORD                                 
         BNE   UPDF06                                                           
         OI    APPRSTAT,APPSDELT   DELETE MASTER RECORD                         
*                                                                               
         TM    T.APPKSTAT,APPSDELT                                              
         BO    UPDF06              DIR HAS ALREADY BEEN DELETED                 
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)                                   
         GOTO1 AIO                                                              
         TM    IOERR,FF-IOEDEL                                                  
         BZ    *+6                                                              
         DC    H'0'                ERROR READING THE RECORD                     
         OI    T.APPKSTAT,APPSDELT DELETE APPROVER DIR                          
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
*                                                                               
UPDF06   LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                 UPDATE THE RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDF08   SR    RF,RF               READ NEXT XDATA SUB-RECORD                   
         IC    RF,T.APPKSEQ        THEY ARE ALWAYS IN SEQUENCE                  
         AHI   RF,1                                                             
         STC   RF,T.APPKSEQ                                                     
*                                                                               
         L     R1,=AL4(XOHID+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         CLC   T.APPKEY(APPKSEQ-APPRECD),IOKEYSAV                               
         BNE   EXITOK              EXIT - NOT SUB-RECORD                        
         TM    IOERR,FF-IOEDEL                                                  
         BZ    *+6                                                              
         DC    H'0'                ERROR READING THE RECORD                     
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
         L     R2,AIO2                                                          
*                                                                               
UPDF10   GOTO1 ADELPAS,BOPARM,APPRECD  DELETE PASSIVE POINTERS                  
*                                                                               
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   *+12                NO                                           
         LA    RF,=AL1(LIDSJLNQ,LIDTAPSJ)                                       
         B     UPDF14                                                           
         CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   *+12                NO                                           
         LA    RF,=AL1(LID1NLNQ,LIDTAP1N)                                       
         B     UPDF14                                                           
         CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R ACCOUNT PAGE            
         BNE   *+12                NO                                           
         LA    RF,=AL1(LID1RLNQ,LIDTAP1R)                                       
         B     UPDF14                                                           
         CLI   GSSMPAGE,APPLISTQ   ARE WE ON INVOICE/ORDER PAGE                 
         BNE   *+12                                                             
         LA    RF,=AL1(LIDAPLNQ,LIDTINOR)                                       
         B     UPDF14                                                           
         CLI   GSSMPAGE,BAKLISTQ   ARE WE ON BACKUP APPROVER PAGE               
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RF,=AL1(LIDLLN6Q,LIDTBACK)                                       
*                                                                               
UPDF14   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',APPRECD),       X        
               (2,(RF))                                                         
         B     UPDF02                                                           
         DROP  T,R2                                                             
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1/2/3/4                                *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
* AIO5 - A LIST OF 1R ACCOUNTS TO BE UPDATED                          *         
***********************************************************************         
         SPACE 1                                                                
         USING APPRECD,R2                                                       
         USING TLSTD,R3                                                         
         USING ACDTABD,R5                                                       
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
         LM    R2,R3,SVPARMS3                                                   
*                                                                               
         MVI   ADDSEQ#,0                                                        
         MVI   RECFLAG,0                                                        
         MVI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R ACCOUNT PAGE            
         BNE   EXITOK                                                           
*                                                                               
         LLC   RF,CURITEM          CURRENT 1R ITEM                              
         LR    RE,RF                                                            
         AHI   RE,1                                                             
         STC   RE,CURITEM          NEXT ITEM                                    
         MHI   RF,ACDTABL                                                       
         L     R5,AIO5                                                          
         LA    R5,0(RF,R5)         POINT TO THE CURRENT ITEM IN TABLE           
*                                                                               
         MVC   ACDTAPP,TLAPDTY                                                  
         MVC   ACDTCDE,TLKA1RAC                                                 
*                                                                               
         LLC   RF,LEN1RLOW         LENGTH OF LOWEST LEVEL FOR 1R                
         LA    R1,ACDTCDE(RF)                                                   
         CLI   0(R1),C' '                                                       
         BNH   *+8                                                              
         OI    ACDTIND,ACDTLOW     IT'S A LOWEST LEVEL ACCOUNT                  
         B     EXITOK                                                           
         DROP  R2,R3,R5                                                         
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         CLI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         BNE   EXITOK                                                           
*                                                                               
         CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R ACCOUNT PAGE            
         BNE   ULST100                                                          
*        GOTO1 ACHKDAC             CHECK DUPLICATE 1R ACCOUNT                   
*        BE    *+20                                                             
*        MVC   FVXTRA(L'ACDTCDE),MYWORK                                         
*        MVC   FVMSGNO,=AL2(AE$HLEXS)                                           
*        B     ULST1ERR            HIGHER OR LOWER LEVEL ACC. EXISTS            
*                                                                               
T        USING LIDELD,BOELEM                                                    
         USING APPRECD,R2                                                       
ULST100  L     R2,AIOREC                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   T.LIDEL,LIDELQ                                                   
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLI/PRO/JOB PAGE                   
         BNE   ULST102                                                          
         MVI   T.LIDTYPE,LIDTAPSJ                                               
         MVI   T.LIDITLN,LIDSJLNQ                                               
         B     ULST110                                                          
ULST102  CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   ULST104             NO                                           
         MVI   T.LIDTYPE,LIDTAP1N                                               
         MVI   T.LIDITLN,LID1NLNQ                                               
         B     ULST110                                                          
ULST104  CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R LIST PAGE?              
         BNE   ULST106             NO                                           
         MVI   T.LIDTYPE,LIDTAP1R                                               
         MVI   T.LIDITLN,LID1RLNQ                                               
         B     ULST110                                                          
ULST106  CLI   GSSMPAGE,APPLISTQ   ARE WE ON OFFICE VALUE PAGE                  
         BNE   ULST108                                                          
         MVI   T.LIDTYPE,LIDTINOR                                               
         MVI   T.LIDITLN,LIDAPLNQ                                               
         XC    MYWORK,MYWORK       USE TO SAVE THE LAST KEY                     
         B     ULST110                                                          
ULST108  CLI   GSSMPAGE,BAKLISTQ   ARE WE ON BACKUP APPROVER PAGE               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTBACK                                               
         MVI   T.LIDITLN,LIDLLN6Q                                               
         XC    MYWORK,MYWORK       USE TO SAVE THE LAST KEY                     
*                                                                               
ULST110  LA    R4,T.LIDDATA                                                     
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
ULST120  LA    R1,TSANXT           DEAL WITH ALL DELETE REQUEST                 
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    ULST200             END OF FILE                                  
         CLC   TLKSES,SESNL        CHECK NEST LEVEL                             
         BNE   ULST200             DONE ALL FOR THIS LEVEL                      
*                                                                               
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLI/PRO/JOB PAGE                   
         BNE   ULST124                                                          
         USING LIDDATA,R4                                                       
         MVC   LIDASTAT,TLASTAT                                                 
         MVC   LIDASTA2,TLASTA2                                                 
         MVC   LIDASJAC,TLKASJAC                                                
         MVC   LIDASJME,TLKASJME                                                
         MVC   LIDASJOF,TLKASJOF                                                
         B     ULST140                                                          
         DROP  R4                                                               
*                                                                               
ULST124  CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   ULST128                                                          
         USING LIDDATA,R4                                                       
         MVC   LIDASTAT,TLASTAT                                                 
         MVC   LIDASTA2,TLASTA2                                                 
         MVC   LIDA1NAC,TLKA1NAC                                                
         B     ULST140                                                          
         DROP  R4                                                               
*                                                                               
ULST128  CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R LIST PAGE?              
         BNE   ULST130                                                          
         USING LIDDATA,R4                                                       
         MVC   LIDAPDTY,TLAPDTY                                                 
         MVC   LIDAPDT2,TLAPDT2                                                 
         MVC   LIDAPACC,TLKA1RAC                                                
         ZAP   LIDAPEXV,BCPZERO                                                 
         B     ULST140                                                          
         DROP  R4                                                               
*                                                                               
ULST130  CLI   GSSMPAGE,APPLISTQ   ARE WE ON OFFICE VALUE PAGE?                 
         BNE   ULST139                                                          
         USING LIDDATA,R4                                                       
         MVC   LIDAPTYP,TLAPTYP                                                 
         MVC   LIDAPTY2,TLAPTY2                                                 
         MVC   LIDAPTY3,TLAPTY3                                                 
         MVC   LIDAPSCT,TLKAPOTY                                                
         MVC   LIDAPOFF,TLKAPOFF                                                
         MVC   LIDAPDPT,TLKAPDPT                                                
         MVC   LIDAPETY,TLKAPETY                                                
         MVC   LIDAPAC,TLKAPACA                                                 
         MVC   LIDAPMED,TLKAPMED                                                
         MVC   LIDAPVAL,TLAPVAL                                                 
         MVC   LIDAPSEL,TLAPSEL                                                 
***      MVC   MYBYTE,TLKAPLM                                                   
***      NI    MYBYTE,LIDAPALM     ENSURE LOW-ORDER BITS ONLY                   
***      OC    LIDAPTY2,MYBYTE                                                  
         DROP  R4                                                               
*                                                                               
         OC    MYWORK,MYWORK                                                    
         BZ    ULST138                                                          
         CLC   MYWORK(TLKAPVLQ),TLKAPVL                                         
         BNE   ULST138                                                          
*        CLI   LSTAPDEF,YES                                                     
*        BNE   ULST140                                                          
         CLI   LSTAPDEF,YES                                                     
         BE    ULST131                                                          
         TM    TLAPTYP,LIDAPOF DID THE LAST LINE ALSO HAVE A PURCHASING         
         BZ    ULST140         APPROVER?  IF SO DUPLICATE                       
         CLI   LSTAPPOF,YES                                                     
         BNE   ULST140                                                          
*                                                                               
ULST131  MVC   BOWORK1,BCSPACES                                                 
         LA    RF,BOWORK1                                                       
         CLC   TLKAPOFF,BCSPACES                                                
         BNH   ULST132                                                          
         MVC   0(L'TLKAPOFF,RF),TLKAPOFF                                        
         LA    RF,1(,RF)                                                        
         CLI   TLKAPOFF+1,C' '                                                  
         BNH   *+8                                                              
         LA    RF,1(,RF)                                                        
         CLC   TLKAPDPT,BCSPACES                                                
         BNH   *+14                                                             
         MVC   0(L'TLKAPDPT,RF),TLKAPDPT                                        
         LA    RF,L'TLKAPDPT(,RF)                                               
         LA    RF,1(,RF)                                                        
*                                                                               
ULST132  CLC   TLKAPETY,BCSPACES                                                
         BNH   *+14                                                             
         MVC   0(L'TLKAPETY,RF),TLKAPETY                                        
         LA    RF,L'TLKAPETY+1(RF)                                              
*                                                                               
         CLC   TLKAPACA,BCSPACES                                                
         BNH   ULST134                                                          
         MVI   0(RF),C'S'                                                       
         CLI   TLKAPACA,C'R'       IF 1R ACCOUNT                                
         BNE   *+8                                                              
         MVI   0(RF),C'1'                                                       
         MVC   1(L'TLKAPACA,RF),TLKAPACA                                        
         LA    RF,L'TLKAPACA+2(RF)                                              
*                                                                               
ULST134  CLI   TLKAPMED,C' '                                                    
         BNH   *+10                                                             
         MVC   0(L'TLKAPMED,RF),TLKAPMED                                        
*                                                                               
         GOTO1 VSQUASH,BOPARM,BOWORK1,L'BOWORK1                                 
         MVC   FVXTRA,BOWORK1                                                   
         MVC   FVMSGNO,=AL2(AE$DUPEN)                                           
         B     ULST1ERR            DUPLICATE ENTRY                              
*                                                                               
ULST138  MVC   MYWORK(TLKAPVLQ),TLKAPVL                                         
         MVI   LSTAPPOF,NO         SET LAST PURCHASE APPR APPL TO NO            
         TM    TLAPTYP,LIDAPOF                                                  
         BZ    *+8                                                              
         MVI   LSTAPPOF,YES                                                     
         MVI   LSTAPDEF,NO                                                      
         CLI   TLKAPOTY,TLOTDFT                                                 
         BNE   ULST140                                                          
         MVI   LSTAPDEF,YES        DEFAULT ORDER TYPE                           
         B     ULST140                                                          
*                                                                               
ULST139  CLI   GSSMPAGE,BAKLISTQ   ARE WE ON BACKUP APPROVER PAGE?              
         BE    *+6                                                              
         DC    H'0'                UNKNOWN PAGE                                 
         USING LIDDATA,R4                                                       
         MVC   LIDLPID,TLKBBPID                                                 
         MVC   LIDLAPPL,TLBAPP                                                  
         MVC   LIDLAPP2,TLBAPP2                                                 
*                                                                               
ULST140  LLC   RE,T.LIDITLN                                                     
         AR    R4,RE               R4=NEXT DATA BLOCK ON ELEMENT                
         LA    RF,T.LIDELD                                                      
         LR    R1,R4                                                            
         SR    R1,RF               TOTAL DISPLACEMENT OF ELEMENT                
*                                                                               
         LHI   RF,L'BOELEM-1                                                    
         SR    RF,RE                                                            
         CR    R1,RF               ENOUGH SPACE TO STORE THE NEXT ELEM          
         BL    ULST120             YES - OK                                     
*                                                                               
         STC   R1,T.LIDLN                                                       
         GOTO1 AADDAPR,BOPARM,APPRECD                                           
         BNE   ULST1ERR                                                         
         CLI   ADDSEQ#,0                                                        
         BE    *+8                                                              
         L     R2,AIO6             NEW APPROVER RECORD                          
*                                                                               
         XC    BOELEM,BOELEM                                                    
         MVI   T.LIDEL,LIDELQ                                                   
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLI/PRO/JOB PAGE      E            
         BNE   ULST152             NO                                           
         MVI   T.LIDTYPE,LIDTAPSJ                                               
         MVI   T.LIDITLN,LIDSJLNQ                                               
         B     ULST160                                                          
*                                                                               
ULST152  CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   ULST154             NO                                           
         MVI   T.LIDTYPE,LIDTAP1N                                               
         MVI   T.LIDITLN,LID1NLNQ                                               
         B     ULST160                                                          
*                                                                               
ULST154  CLI   GSSMPAGE,C1RLISTQ   ARE WE ON COSTING 1R ACCOUNT PAGE            
         BNE   ULST156             NO                                           
         MVI   T.LIDTYPE,LIDTAP1R                                               
         MVI   T.LIDITLN,LID1RLNQ                                               
         B     ULST160                                                          
*                                                                               
ULST156  CLI   GSSMPAGE,APPLISTQ   ARE WE ON OFFICE VALUE PAGE                  
         BNE   ULST158                                                          
         MVI   T.LIDTYPE,LIDTINOR                                               
         MVI   T.LIDITLN,LIDAPLNQ                                               
         B     ULST160                                                          
*                                                                               
ULST158  CLI   GSSMPAGE,BAKLISTQ   ARE WE ON OFFICE VALUE PAGE                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTBACK                                               
         MVI   T.LIDITLN,LIDLLN6Q                                               
*                                                                               
ULST160  LA    R4,T.LIDDATA                                                     
         B     ULST120                                                          
*                                                                               
ULST200  LA    RF,T.LIDELD                                                      
         SR    R4,RF               LENGTH OF ELEMENT                            
         CHI   R4,LIDLNDQ                                                       
         BNH   ULST204             NO ITEM TO UPDATE                            
*                                                                               
         STC   R4,T.LIDLN                                                       
         GOTO1 AADDAPR,BOPARM,APPRECD                                           
         BNE   ULST1ERR                                                         
ULST204  CLI   ADDSEQ#,0           TEST MAIN RECORD                             
         BE    EXITOK                                                           
         GOTO1 AUPDAPR             UPDATE APPROVER SUB-RECORD                   
         B     EXITOK                                                           
*                                                                               
ULST1ERR OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         B     EXITL                                                            
         DROP  T,R2,R3                                                          
         EJECT ,                                                                
***********************************************************************         
* OVERLAY ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F                                                               
OVROU1   NMOD1 0,**OVR1**,RA,R7                                                 
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         USING WORKD,R9                                                         
         USING GWORKD,R8                                                        
         USING OVERWRKD,RC                                                      
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     DOFLT                                                            
         B     IOCHK                                                            
         B     DELPAS                                                           
         B     ADDPAS                                                           
         B     VALCPJ                                                           
         B     GETCPJ                                                           
         B     VALNCL                                                           
         B     VALC1R                                                           
         B     GETAPL                                                           
         B     GETBAK                                                           
         B     CHKPID                                                           
         B     VALLMT                                                           
         B     GETLIT                                                           
         B     GETNLE                                                           
         B     CHKDAC                                                           
         B     NXTAPR                                                           
         B     ADDAPR                                                           
         B     UPDAPR                                                           
         B     SETFLD                                                           
         B     VALFLT                                                           
*                                                                               
OVROU1H  CLI   *,0                 SET CC HIGH                                  
         B     OVROU1X                                                          
OVROU1L  CLI   *,FF                SET CC LOW                                   
         B     OVROU1X                                                          
OVROU1E  CR    RB,RB               SET CC EQUAL                                 
OVROU1X  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
PRDUL    DC    C'SJ'                                                            
ONERUL   DC    C'1R'                                                            
NCTUL    DC    C'1N'                                                            
OFFUL    DC    C'2D'                                                            
SVUL     DC    C'SV'                                                            
SXUL     DC    C'SX'                                                            
EXPTYP   DC    C'ET'                                                            
MEDCODE  DC    C'MEDIA'                                                         
XFFS     DC    X'FFFFFFFF'                                                      
HEADERU  DC    C'--------'                                                      
NOS      DC    C'NNNNNNNNN'                                                     
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
DEF      EQU   C'D'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
BOTH     EQU   C'B'                                                             
EDITOR   EQU   C'E'                                                             
STMPSTRQ EQU   X'03'                                                            
OTYPFLQ  EQU   5                                                                
SJNAMFLQ EQU   48                                                               
MAXRECSQ EQU   255                                                              
MAXITMSQ EQU   3000                                                             
IOMAXLNQ EQU   1700                                                             
CPJLISTQ EQU   1                                                                
NCCLISTQ EQU   2                                                                
C1RLISTQ EQU   3                                                                
APPLISTQ EQU   4                                                                
BAKLISTQ EQU   5                                                                
*                                                                               
DCLSTU   DS    0D                  UPPER CASE                                   
         DCDDL AC#BOTH,L'UC@BOTH,L                                              
*&&UK*&& DCDDL AC#EDITR,L'UC@EDITR,L                                            
         DCDDL AC#TIME,L'UC@TIME,L                                              
         DCDDL AC#EXP,L'UC@EXP,L                                                
         DCDDL AC#FINCE,L'UC@FINCE,L                                            
         DCDDL AC#DEFFI,L'UC@DEFFI,L                                            
         DCDDL AC#CLI,OTYPFLQ,L                                                 
         DCDDL AC#NCLI,OTYPFLQ,L                                                
         DCDDL AC#EXP,OTYPFLQ,L                                                 
         DCDDL AC#PRO,OTYPFLQ,L                                                 
         DCDDL AC#ARTST,OTYPFLQ,L                                               
         DCDDL AC#INT2,OTYPFLQ,L                                                
         DCDDL AC#FLDEF,OTYPFLQ,L                                               
         DCDDL AC#ORDS,L'UC@ORDS,L                                              
         DCDDL AC#INVS,L'UC@INVS,L                                              
DCLSTUX  DC    AL1(EOT)                                                         
*                                                                               
DCLSTL   DS    0D                  LOWER CASE                                   
         DCDDL AC#JOBS,L'LC@JOBS,L                                              
         DCDDL AC#DEF,L'LC@DEF,L                                                
         DCDDL AC#FL2LA,L'LC@2LSJH,L                                            
         DCDDL AC#FL1R2,L'LC@2L1RH,L                                            
         DCDDL AC#DEPC,L'LC@DEPC,L                                              
         DCDDL AC#DPT,L'LC@DPT,L                                                
DCLSTLX  DC    AL1(EOT)                                                         
                                                                                
         DS    0D                                                               
APTYTAB  DC    AL1(ALIKCLI,CTRYALL),Y(UC@CLI-DSLSTU)                            
APTYTLNQ EQU   *-APTYTAB                                                        
         DC    AL1(ALIKDFT,CTRYALL),Y(UC@FLDEF-DSLSTU)                          
         DC    AL1(ALIKNCLI,CTRYALL),Y(UC@NCLI-DSLSTU)                          
         DC    AL1(ALIKEXP,CTRYALL),Y(UC@EXP2-DSLSTU)                           
         DC    AL1(ALIKPROD,CTRYALL),Y(UC@PRO-DSLSTU)                           
         DC    AL1(ALIKART,CTRYALL),Y(UC@ARTST-DSLSTU)                          
         DC    AL1(ALIKINT,CTRYALL),Y(UC@INT2-DSLSTU)                           
         DC    X'00'                                                            
         DS    0D                                                               
LIDATAB  DC    AL1(0,LIDAPIN,YES,0)  DISP INTO FVIFLD/EQUATE/Y,D/SPARE          
         DC    AL2(TLAPTYP-TLSTD)    DISP TO APLTYPE BYTE TO SET/TEST           
         DC    AL2(TLDAPTYP-TLSTD)   DISP TO D/L APL BYTE TO SET/TEST           
LIDATLNQ EQU   *-LIDATAB                                                        
         DC    AL1(0,LIDAPIND,DEF,0)                                            
         DC    AL2(TLAPTYP-TLSTD),AL2(TLDAPTYP-TLSTD)                           
*                                                                               
         DC    AL1(1,LIDAPOR,YES,0)                                             
         DC    AL2(TLAPTYP-TLSTD),AL2(TLDAPTYP-TLSTD)                           
*                                                                               
         DC    AL1(1,LIDAPORD,DEF,0)                                            
         DC    AL2(TLAPTYP-TLSTD),AL2(TLDAPTYP-TLSTD)                           
*                                                                               
         DC    AL1(2,LIDAPOF,YES,0)                                             
         DC    AL2(TLAPTYP-TLSTD),AL2(TLDAPTYP-TLSTD)                           
*                                                                               
         DC    AL1(2,LIDAPOFD,DEF,0)                                            
         DC    AL2(TLAPTYP-TLSTD),AL2(TLDAPTYP-TLSTD)                           
*                                                                               
         DC    AL1(3,LIDAPES,YES,0)                                             
         DC    AL2(TLAPTYP-TLSTD),AL2(TLDAPTYP-TLSTD)                           
*                                                                               
         DC    AL1(3,LIDAPESD,DEF,0)                                            
         DC    AL2(TLAPTYP-TLSTD),AL2(TLDAPTYP-TLSTD)                           
*                                                                               
         DC    AL1(4,LIDAPOA,YES,0)                                             
         DC    AL2(TLAPTY2-TLSTD),AL2(TLDAPTY2-TLSTD)                           
*                                                                               
         DC    AL1(4,LIDAPOAD,DEF,0)                                            
         DC    AL2(TLAPTY2-TLSTD),AL2(TLDAPTY2-TLSTD)                           
*                                                                               
         DC    AL1(5,LIDAPOU,YES,0)                                             
         DC    AL2(TLAPTY2-TLSTD),AL2(TLDAPTY2-TLSTD)                           
*                                                                               
         DC    AL1(5,LIDAPOUD,DEF,0)                                            
         DC    AL2(TLAPTY2-TLSTD),AL2(TLDAPTY2-TLSTD)                           
*                                                                               
         DC    AL1(6,LIDAINEA,YES,0)                                            
         DC    AL2(TLAPTY3-TLSTD),AL2(TLDAPTY3-TLSTD)                           
*                                                                               
         DC    AL1(6,LIDAINED,DEF,0)                                            
         DC    AL2(TLAPTY3-TLSTD),AL2(TLDAPTY3-TLSTD)                           
*                                                                               
         DC    AL1(7,LIDAEXC,YES,0)                                             
         DC    AL2(TLAPTY3-TLSTD),AL2(TLDAPTY3-TLSTD)                           
*                                                                               
         DC    AL1(7,LIDAEXCD,DEF,0)                                            
         DC    AL2(TLAPTY3-TLSTD),AL2(TLDAPTY3-TLSTD)                           
*                                                                               
LIDATNQ  EQU   (*-LIDATAB)/LIDATLNQ                                             
*                                                                               
         EJECT ,                                                                
*********************************************************************           
* FILTER APPROVER RECORDS                                           *           
* ENTY - IOKEY = APPROVER RECORD KEY                                *           
*********************************************************************           
         SPACE 2                                                                
THIS     USING APPRECD,R2                                                       
X        USING APPRECD,IOKEY                                                    
DOFLT    MVC   SVPIDBIN,X.APPKPIDB                                              
         L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    DOFLT02                                                          
         OI    APPINDS,APPIRSEQ    RESET IO SEQUENCE                            
         GOTO1 ACHKPID,BOPARM,SVPIDBIN                                          
         BNE   DOFLTL                                                           
*                                                                               
DOFLT02  MVC   SVIOKEY,IOKEY                                                    
*&&UK                                                                           
         GOTO1 ACHKLPID,BOPARM,SVPIDBIN      IS PID VALID FOR LOGON?            
         BE    DOFLT03                                                          
         BH    DOFLT0A                                                          
                                                                                
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         MVC   IOKEY,SVIOKEY                                                    
         GOTO1 AIO                                                              
         BE    DOFLTL              BAD PID READ NEXT                            
         DC    H'0'                                                             
*                                                                               
DOFLT03  L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         MVC   IOKEY,SVIOKEY                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*&&                                                                             
DOFLT0A  GOTOX ('GETPID',AGROUTS),SVPIDBIN                                      
         MVC   SVPID,BCWORK                                                     
         NI    APPINDS,FF-APPIRSEQ                                              
         L     R1,=AL4(XORDD+XOACCDIR++XIO11)                                   
         GOTOR AIO                                                              
         BE    *+14                                                             
         TM    IOERR,FF-IOEDEL     IF RESTORING, SHOULD EXIST                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         OC    SVPIDFL,SVPIDFL     ANY PID FILTER ?                             
         BZ    DOFLT10             NO - OK                                      
         CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BNE   DOFLT04                                                          
         CLC   SVPID,SVPIDFL       DO WE WANT THIS PID ?                        
         BNE   DOFLTL              NO - SET CC TO LOW                           
         B     DOFLTE              YES - SET CC TO EQUAL                        
*                                                                               
DOFLT04  CLC   SVPID(L'SAPALPID),SVPIDFL                                        
         BL    DOFLTL                                                           
*                                                                               
DOFLT10  OC    SVSTAFL(SVSTAFLQ),SVSTAFL                                        
         BZ    DOFLT40             OK - NO FILTER ON STATUS                     
                                                                                
DOFLT12  CLI   SVFFINAP,0          IS THERE ANY FINANCE APPROVER FILTER         
         BE    DOFLT18             NO - GO TO NEXT FILTER                       
         CLI   SVFFINAP,YES        FINANCE APPROVER                             
         BNE   DOFLT14             NO                                           
         TM    X.APPKSTAT,APPSFINA                                              
         BNO   DOFLTL                                                           
         B     DOFLT18             GET NEXT ELEMENT                             
*                                                                               
DOFLT14  CLI   SVFFINAP,DEF        DEFAULT FINANCE APPROVER                     
         BNE   DOFLT16             NO - MUST BE NO TO FINANCE APPROVER          
         TM    X.APPKSTAT,APPSFIND IS RECORD SET DEFAULT APPROVER               
         BNO   DOFLTL              NO                                           
         B     DOFLT18             YES - GO TO NEXT FILTER                      
*                                                                               
DOFLT16  TM    X.APPKSTAT,APPSFINA+APPSFIND                                     
         BNZ   DOFLTL                                                           
                                                                                
DOFLT18  CLI   SVFJOBAP,0          IS THERE ANY JOB APPROVER FILTER             
         BE    DOFLT24             NO - GO TO NEXT FILTER                       
         CLI   SVFJOBAP,YES        JOB APPROVER                                 
         BNE   DOFLT20             NO                                           
         TM    X.APPKSTA2,APPSRJAQ                                              
         BNO   DOFLTL                                                           
         B     DOFLT24             GET NEXT ELEMENT                             
*                                                                               
DOFLT20  CLI   SVFJOBAP,DEF        DEFAULT JOB APPROVER                         
         BNE   DOFLT22             NO - MUST BE NO TO JOB APPROVER              
         TM    X.APPKSTA2,APPSDJAQ IS RECORD SET DEFAULT APPROVER               
         BNO   DOFLTL              NO                                           
         B     DOFLT24             YES - GO TO NEXT FILTER                      
*                                                                               
DOFLT22  TM    X.APPKSTA2,APPSRJAQ+APPSDJAQ                                     
         BNZ   DOFLTL                                                           
                                                                                
DOFLT24  CLI   SVFESTAP,0          IS THERE ANY EST. APPROVER FILTER            
         BE    DOFLT28             NO                                           
         CLI   SVFESTAP,YES        ESTIMATE APPROVER                            
         BNE   DOFLT26                                                          
         TM    X.APPKSTA2,APPSESTQ                                              
         BNO   DOFLTL                                                           
         B     DOFLT28                                                          
                                                                                
DOFLT26  TM    X.APPKSTA2,APPSESTQ                                              
         BNZ   DOFLTL                                                           
                                                                                
DOFLT28  CLI   SVFIESTA,0          IS THERE ANY INT EST APPRV FILTER            
         BE    DOFLT40             NO                                           
         CLI   SVFIESTA,YES        INTERNAL ESTIMATE APPROVER                   
         BNE   DOFLT30                                                          
         TM    X.APPKSTA2,APPSESIQ                                              
         BNO   DOFLTL                                                           
         B     DOFLT40                                                          
                                                                                
DOFLT30  TM    X.APPKSTA2,APPSESIQ                                              
         BNZ   DOFLTL                                                           
         DROP  X                                                                
*                                                                               
DOFLT40  OC    SVFELEM(SVFELMLQ),SVFELEM                                        
         BZ    DOFLTE              OK - NO FILTER ON ELEMENTS                   
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO11                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
*                                                                               
         MVC   SVIOKEY,IOKEY       SAVE IOKEY                                   
         MVI   READSEQ#,0                                                       
         XC    DOFINDS,DOFINDS                                                  
         OI    DOFIND2,DOFI2OTH    2ND INDICATOR BITS NOT USED                  
*                                                                               
         OC    SVFBAP,SVFBAP       ANY BACK UP APPROVER FILTER?                 
         BNZ   *+8                                                              
         OI    DOFIND2,DOFI2BAP    DON'T COMPARE BACK UP APPROVER               
         OC    SVFCPJ,SVFCPJ       ANY CLI/PRO/JOB FILTER?                      
         BNZ   *+8                                                              
         OI    DOFIND1,DOFIFCPJ    DON'T COMPARE CLI/PRO/JOB                    
         OC    SVFNCC,SVFNCC       ANY NON-CLIENT FILTER?                       
         BNZ   *+8                                                              
         OI    DOFIND1,DOFIFNCC    DON'T COMPARE NON-CLIENT CODE                
         OC    SVF1RC,SVF1RC       ANY 1R ACCOUNT FILTER?                       
         BNZ   *+8                                                              
         OI    DOFIND1,DOFIF1RC    DON'T COMPARE 1R ACCOUNT CODE                
         OC    SVFODC,SVFODC       ANY OFF/DEPT FILTER?                         
         BNZ   *+8                                                              
         OI    DOFIND1,DOFIFODC    DON'T COMPARE OFF/DEPT CODE                  
         OC    SVFETC,SVFETC       ANY EXPENDITURE FILTER?                      
         BNZ   *+8                                                              
         OI    DOFIND1,DOFIFETC    DON'T COMPARE EXPENDITURE CODE               
         OC    SVFPSC,SVFPSC       ANY PROD/SUPP FILTER?                        
         BNZ   *+8                                                              
         OI    DOFIND1,DOFIFPSC    DON'T COMPARE PROD/SUPP CODE                 
         OC    SVFMED,SVFMED       ANY MEDIA FILTER?                            
         BNZ   *+8                                                              
         OI    DOFIND1,DOFIFMED    DON'T COMPARE SUPPLIER CODE                  
         OC    SVFAPV,SVFAPV       ANY APPROVAL VALUE                           
         BNZ   *+8                                                              
         OI    DOFIND1,DOFIFAPV    DON'T COMPARE APPROVAL VALUE                 
*                                                                               
DOFLT42  L     R4,AIOREC           A(MAIN APPROVER RECORD)                      
         CLI   READSEQ#,0                                                       
         BE    *+8                                                              
         L     R4,AIO5             A(NEXT APPROVER RECORD)                      
         LA    R4,APPRFST-APPRECD(R4)                                           
*                                                                               
         USING LIDELD,R4                                                        
DOFLT44  CLC   DOFINDS,XFFS                                                     
         BE    DOFLTE                                                           
         CLI   LIDEL,0                                                          
         BNE   DOFLT46             END OF RECORD                                
         OI    APPINDS,APPIRSEQ    RESET IO SEQUENCE                            
         GOTO1 ANXTAPR,BOPARM,AIOREC                                            
         BNE   DOFLTL              NO MORE APPROVER RECORD                      
         B     DOFLT42                                                          
*                                                                               
DOFLT46  CLI   LIDEL,LIDELQ                                                     
         BE    DOFLT50                                                          
DOFLT48  LLC   R1,LIDLN                                                         
         AR    R4,R1                                                            
         B     DOFLT44                                                          
*                                                                               
DOFLT50  LLC   R5,LIDITLN          LENGTH OF ITEMS                              
         LLC   R0,LIDLN            LENGTH OF ELEMENT                            
         SHI   R0,LIDDATA-LIDELD                                                
         SRDL  R0,32                                                            
         DR    R0,R5               R1=NUMBER OF ITEMS                           
         LA    R5,LIDDATA                                                       
*                                                                               
DOFLT52  CLI   LIDTYPE,LIDTAPSJ    CLIENT/PROD/JOB?                             
         BNE   DOFLT56                                                          
         TM    DOFIND1,DOFIFCPJ    COMPARE CLI/PRO/JOB?                         
         BO    DOFLT54             NO - CLI/PRO/JOB FOUND                       
         CLC   SVFCPJ,LIDASJAC-LIDDATA(R5)                                      
         BNE   DOFLT54                                                          
         OI    DOFIND1,DOFIFCPJ                                                 
*                                                                               
DOFLT54  TM    DOFIND1,DOFIFMED    COMPARE MEDIA CODE?                          
         BO    DOFLT72             NO - MEDIA FOUND                             
         CLC   SVFMED,LIDASJME-LIDDATA(R5)                                      
         BNE   DOFLT72                                                          
         OI    DOFIND1,DOFIFMED                                                 
         B     DOFLT72                                                          
*                                                                               
DOFLT56  CLI   LIDTYPE,LIDTAP1N    NON CLIENT ACCOUNT CODE?                     
         BNE   DOFLT58                                                          
         TM    DOFIND1,DOFIFNCC    COMPARE NON-CLIENT CODE?                     
         BO    DOFLT72             NO - NON-CLIENT CODE FOUND                   
         CLC   SVFNCC,LIDA1NAC-LIDDATA(R5)                                      
         BNE   DOFLT72                                                          
         OI    DOFIND1,DOFIFNCC                                                 
         B     DOFLT72                                                          
*                                                                               
DOFLT58  CLI   LIDTYPE,LIDTAP1R    1R COSTING ACCOUNTS?                         
         BNE   DOFLT60                                                          
         TM    DOFIND1,DOFIF1RC    COMPARE COSTING 1R CODE?                     
         BO    DOFLT72             NO - COSTING 1R CODE FOUND                   
         CLC   SVF1RC,LIDAPACC-LIDDATA(R5)                                      
         BNE   DOFLT72                                                          
         OI    DOFIND1,DOFIF1RC                                                 
         B     DOFLT72                                                          
*                                                                               
DOFLT60  CLI   LIDTYPE,LIDTINOR    APPROVAL INVOICES/ORDERS                     
         BNE   DOFLT70                                                          
         TM    DOFIND1,DOFIFODC    COMPARE OFF/DEPT CODE?                       
         BO    DOFLT62                                                          
         CLC   SVFODC,LIDAPOFF-LIDDATA(R5)                                      
         BNE   *+8                                                              
         OI    DOFIND1,DOFIFODC                                                 
DOFLT62  TM    DOFIND1,DOFIFETC    COMPARE EXPENDITURE TYPE CODE?               
         BO    DOFLT64                                                          
         CLC   SVFETC,LIDAPETY-LIDDATA(R5)                                      
         BNE   *+8                                                              
         OI    DOFIND1,DOFIFETC                                                 
DOFLT64  TM    DOFIND1,DOFIFPSC    COMPARE PROD/SUPP CODE?                      
         BO    DOFLT66                                                          
         CLC   SVFPSC,LIDAPAC-LIDDATA(R5)                                       
         BNE   *+8                                                              
         OI    DOFIND1,DOFIFPSC                                                 
DOFLT66  TM    DOFIND1,DOFIFMED    COMPARE MEDIA CODE?                          
         BO    DOFLT68                                                          
         CLC   SVFMED,LIDAPMED-LIDDATA(R5)                                      
         BNE   *+8                                                              
         OI    DOFIND1,DOFIFMED                                                 
DOFLT68  TM    DOFIND1,DOFIFAPV    COMPARE APPROVAL VALUE?                      
         BO    DOFLT72                                                          
         CLC   SVFAPV,LIDAPVAL-LIDDATA(R5)                                      
         BNE   DOFLT72                                                          
         OI    DOFIND1,DOFIFAPV                                                 
         B     DOFLT72                                                          
*                                                                               
DOFLT70  CLI   LIDTYPE,LIDTBACK                                                 
         BNE   DOFLT48                                                          
         TM    DOFIND2,DOFI2BAP    COMPARE BACK UP APPROVERS                    
         BO    DOFLT72             NO                                           
         OC    LIDLPID-LIDDATA(L'LIDLPID,R5),LIDLPID-LIDDATA(R5)                
         BZ    DOFLT72                                                          
         CLC   SVFBAP,LIDLPID-LIDDATA(R5)                                       
         BNE   DOFLT72                                                          
         OI    DOFIND2,DOFI2BAP                                                 
         B     DOFLT72                                                          
*                                                                               
DOFLT72  LLC   R0,LIDITLN          LENGTH OF ITEMS                              
         AR    R5,R0                                                            
         BCT   R1,DOFLT52          CHECK NEXT ENTRY                             
         B     DOFLT48             GET NEXT ELEMENT                             
*                                                                               
DOFLTE   MVI   MYBYTE,0            SET CC TO EQUAL                              
         B     DOFLTX                                                           
DOFLTL   MVI   MYBYTE,FF           SET CC TO NOT EQUAL                          
*                                                                               
DOFLTX   TM    APPINDS,APPIRSEQ    RESET IO SEQUENCE?                           
         BZ    DOFLTX2             NO - OK                                      
         MVC   IOKEY,SVIOKEY                                                    
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         GOTOR AIO                                                              
         BE    DOFLTX2                                                          
         TM    IOERR,FF-IOEDEL     IF RESTORING, SHOULD EXIST                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DOFLTX2  CLI   MYBYTE,0                                                         
         BE    OVROU1E                                                          
         B     OVROU1L                                                          
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* IO COUNTING ROUTINE                                                 *         
***********************************************************************         
         SPACE 1                                                                
IOCHK    LH    R0,IOCOUNT                                                       
         AHI   R0,1                                                             
         CLM   R0,2,IOCOUNT        256 IO BOUNDARY?                             
         STH   R0,IOCOUNT                                                       
         BE    OVROU1E                                                          
         GOTO1 VGETFACT,BODMCB,0   GET PHYSICAL IO COUNT                        
         L     R1,0(R1)                                                         
         ICM   R0,3,FATIOCNT-FACTSD(R1)                                         
         AHI   R0,200              WITHIN 200 OF LIMIT?                         
         CLM   R0,3,FATMAXIO-FACTSD(R1)                                         
         BNH   OVROU1E                                                          
*                                                                               
         B     OVROU1L             MAX I/O COUNT REACHED                        
***********************************************************************         
* UPDATE APPROVER PASSIVES                                            *         
*                                                                     *         
* NTRY - P1  = APPROVER RECORD                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING APPRECD,R3                                                       
ADDPAS   L     R3,0(R1)                                                         
*                                                                               
         CLI   APPKSEQ,0                                                        
         BNE   ADDPAS10            NO - IODA HAS ALREADY BEEN SET               
         MVC   IOKEY,0(R3)         GET IODA FOR PADDLE                          
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
ADDPAS10 XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'A',(R3)),(C'A',T.CPTRBLK),IODA,0,ACOM          
         B     OVROU1E                                                          
         DROP  T,R3                                                             
         EJECT ,                                                                
***********************************************************************         
* DELETE APPROVER PASSIVES                                            *         
*                                                                     *         
* NTRY - P1  = PID LIST ELEMENT                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING APPRECD,R3                                                       
DELPAS   L     R3,0(R1)            GET RECORD ADDRESS                           
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
         XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',T.CPTRBLK),0,0,ACOM             
         B     OVROU1E                                                          
         DROP  T,R3                                                             
         EJECT ,                                                                
***********************************************************************         
* CHECK CLI/PRO/JOB CODE IS VALID AND RETURN NAME/OFFICE/MEDIA        *         
*                                                                     *         
* NTRY   P1 BYTE 0   - LENGTH OF ACCOUNT CODE OR 0                    *         
*           BYTE 1-3 - CLI/PRO/JOB CODE OR LIDDATA                    *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
VALCPJ   DS    0H                                                               
*&&US*&& NI    ERRIND,X'FF'-ERCOFIN                                             
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         MVC   MYHALF,BCSPACES                                                  
         LR    RF,R2                                                            
         XR    RE,RE                                                            
         ICM   RE,1,0(R1)          GET LENGTH OF CLI/PRD/JOB                    
         BNZ   VALCPJ04                                                         
         USING LIDDATA,R2                                                       
         MVC   CPJAPTY,LIDASTAT    NO LENGTH IT'S LIDDATA                       
         MVC   CPJCODE,LIDASJAC                                                 
         MVC   CPJOFF,LIDASJOF                                                  
*&&US                                                                           
         GOTO1 ATSTOFF,LIDASJOF                                                 
         BE    *+14                                                             
         OI    ERRIND,ERCOFIN       INVALID OFFICE / NOT IN LIMLIST             
         MVC   FVXTRA,BCSPACES                                                  
*&&                                                                             
*                                                                               
         MVC   CPJMED,LIDASJME                                                  
         MVC   CPJNAME,BCSPACES                                                 
         LHI   RE,L'LIDASJAC                                                    
         B     VALCPJ06                                                         
         DROP  R2                                                               
*                                                                               
VALCPJ04 XC    CPJAPTY,CPJAPTY                                                  
         MVC   CPJVALS(CPJVALSQ),BCSPACES                                       
         NI    APPINDS,255-APPICLIO                                             
         XC    CPJCOFF,CPJCOFF                                                  
         XC    CPJCLOF,CPJCLOF                                                  
         LR    R1,RE                                                            
         SHI   R1,1                                                             
         MVC   CPJCODE(0),0(RF)                                                 
         EX    R1,*-6                                                           
*                                                                               
VALCPJ06 CLC   CPJCODE,BCSPACES                                                 
         BE    VALCPJE                                                          
         LA    RF,CPJCODE(RE)      POINT AT END OF CLI/PROD/JOB                 
         BCTR  RF,0                                                             
VALCPJ08 CLI   0(RF),C' '          IS IT SPACE OR NULLS                         
         BH    VALCPJ12                                                         
         BCTR  RF,0                                                             
         BCT   RE,VALCPJ08                                                      
         DC    H'0'                                                             
*                                                                               
VALCPJ12 STC   RE,MYBYTE           LENGTH OF CLI/PRO/JOB CODE                   
         CLC   MYBYTE,CLILEN                                                    
*&&UK*&& BE    VALCPJ16                                                         
*&&US*&& BNH   VALCPJ16                                                         
         CLC   MYBYTE,PROLEN                                                    
*&&UK*&& BE    VALCPJ16                                                         
*&&UK*&& BL    VALCPJL             INVALID CLI/PRO LENGTH                       
*&&US*&& BNH   VALCPJ16                                                         
         CLC   MYBYTE,JOBLEN                                                    
         BH    VALCPJL             INVALID CLI/PRO/JOB LENGTH                   
         CLI   CPJMED,C' '                                                      
         BH    VALCPJ16            DON'T OVERRIDE MEDIA CODE                    
         LLC   RF,PROLEN                                                        
         LA    R1,CPJCODE(RF)                                                   
         MVC   CPJMED,0(R1)        SAVE MEDIA CODE                              
*                                                                               
VALCPJ16 LA    R4,CPJNAME                                                       
         GOTO1 AGETCPJ,BOPARM,(CLILEN,(R4))                                     
         BNE   VALCPJL             INVALID CLIENT CODE                          
         L     R4,0(R1)            NEW POSITION IN CPJNAME                      
                                                                                
         CLC   MYBYTE,CLILEN       TEST CLIENT CODE ONLY                        
         BH    VALCPJ18                                                         
*&&US                                                                           
         GOTO1 ATSTOFF,MYHALF      TEST OFFICE                                  
         BE    *+14                                                             
         OI    ERRIND,ERCOFIN       INVALID OFFICE / NOT IN LIMLIST             
         MVC   FVXTRA,BCSPACES                                                  
*&&                                                                             
         OI    APPINDS,APPICLIO                                                 
         MVC   CPJCLOF,MYHALF      SAVE CLIENT OFFICE                           
         TM    APPINDS,APPICCPJ    CHANGING CLIENT PRODUCT JOB                  
         BNZ   VALCPJX2                                                         
         B     VALCPJX                                                          
*                                                                               
VALCPJ18 GOTO1 AGETCPJ,BOPARM,(PROLEN,(R4))                                     
         BNE   VALCPJL             INVALID CLIENT CODE                          
         L     R4,0(R1)            NEW POSITION IN CPJNAME                      
         CLC   MYBYTE,PROLEN       TEST PRODUCT CODE ONLY                       
         BNH   VALCPJX2                                                         
         GOTO1 AGETCPJ,BOPARM,(JOBLEN,(R4))                                     
         BNE   VALCPJL             INVALID CLIENT CODE                          
         L     R4,0(R1)            NEW POSITION IN CPJNAME                      
         B     VALCPJX2                                                         
*                                                                               
VALCPJX  TM    APPINDS,APPICPJV    COME FROM CPJ                                
         BZ    VALCPJX1                                                         
         CLC   LIDASJOF-LIDASTAT(L'LIDASJOF,R2),BCSPACES                        
         BNH   VALCPJE             ALREADY HAVE OFF?                            
         MVC   CPJOFF,LIDASJOF-LIDASTAT(R2)                                     
         B     VALCPJE                                                          
*                                                                               
VALCPJX1 TM    APPINDS,APPIPSCV    NO COME FROM PRODUCTION SUPP CODE            
         BZ    VALCPJE                                                          
         CLC   LIDAPOFF-LIDAPTYP(L'LIDAPOFF,R2),BCSPACES                        
         BNH   VALCPJE                                                          
         MVC   CPJOFF,LIDAPOFF-LIDAPTYP(R2)                                     
         B     VALCPJE                                                          
*                                                                               
VALCPJX2 CLC   CPJOFF,BCSPACES                                                  
         BH    VALCPJE                                                          
*&&US                                                                           
         GOTO1 ATSTOFF,MYHALF      TEST OFFICE                                  
         BE    *+14                                                             
         OI    ERRIND,ERCOFIN       INVALID OFFICE / NOT IN LIMLIST             
         MVC   FVXTRA,BCSPACES                                                  
*&&                                                                             
         MVC   CPJOFF,MYHALF       SET DEFAULT OFFICE CODE                      
         B     VALCPJE                                                          
*                                                                               
VALCPJL  NI    APPINDS,X'FF'-(APPIPSCV+APPICPJV)                                
         B     OVROU1L                                                          
VALCPJE  NI    APPINDS,X'FF'-(APPIPSCV+APPICPJV)                                
         B     OVROU1E                                                          
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* GET NAME/OFFICE/MEDIA CODE FOR CLI/PRO/JOB                          *         
*                                                                     *         
* NTRY   P1 BYTE 0   - LENGTH OF CLI/PRO/JOB                          *         
*           BYTE 1-3 - A(CURRENT POSITION IN CPJNAME)                 *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
GETCPJ   ST    R1,SAVER1                                                        
         XR    R4,R4                                                            
         ICM   R4,7,1(R1)                                                       
*                                                                               
         LLC   RF,0(R1)            GET LENGTH OF CLI/PRD/JOB                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'PRDUL),PRDUL                                         
         SHI   RF,1                                                             
         MVC   T.ACTKACT(0),CPJCODE     CLI/PRO/JOB CODE                        
         EX    RF,*-6                                                           
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         DROP  T                                                                
*                                                                               
         L     R3,AIO2                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         XR    R0,R0                                                            
GETCPJ04 CLI   0(R3),0             END OF RECORD                                
         BE    GETCPJX                                                          
         CLI   0(R3),NAMELQ        TEST NAME ELEMENT                            
         BE    GETCPJ12                                                         
         CLI   0(R3),PPRELQ        TEST PRODUCTION PROFILE ELEMENT              
         BE    GETCPJ14                                                         
         CLI   0(R3),LIDELQ        TEST OFFICE LIST LIDEL                       
         BE    GETCPJ16                                                         
GETCPJ08 IC    R0,1(,R3)           BUMP TO NEXT ELEMENT                         
         AR    R3,R0                                                            
         B     GETCPJ04                                                         
*                                                                               
         USING NAMELD,R3                                                        
GETCPJ12 LLC   R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         BM    GETCPJ08            BAD NAME ELEMENT                             
*                                                                               
         LR    RF,R4                                                            
         LA    RE,CPJNAME                                                       
         SR    RF,RE               LENGTH OF CURRENT NAMES                      
         AHI   RF,R1               TOTAL LENGTH                                 
         SHI   RF,SJNAMFLQ+2       ENOUGH SPACE TO STORE NAMES                  
         BNP   *+10                                                             
         SR    R1,RF               NO - TRUNCATE THE NAME                       
         BM    GETCPJ08                                                         
*                                                                               
         CR    R4,RE               TEST FIRST NAME                              
         BE    *+12                YES - DON'T INSERT '/'                       
         MVI   0(R4),C'/'                                                       
         LA    R4,1(,R4)                                                        
*                                                                               
         MVC   0(0,R4),NAMEREC                                                  
         EX    R1,*-6                                                           
         LA    R4,2(R1,R4)                                                      
*        CLC   CPJOFF,BCSPACES                                                  
*        BH    GETCPJX             EXIT IF WE ALREADY HAVE OFFICE               
         B     GETCPJ08                                                         
*                                                                               
         USING PPRELD,R3                                                        
GETCPJ14 CLC   CPJOFF,BCSPACES                                                  
         BH    GETCPJ08            DON'T OVERRIDE OFFICE CODE                   
         CLC   PPRGAOFF,BCSPACES                                                
         BNH   GETCPJ08                                                         
         MVC   MYHALF,PPRGAOFF                                                  
         B     GETCPJ08                                                         
*                                                                               
         USING LIDELD,R3                                                        
GETCPJ16 CLI   LIDTYPE,LIDTPOFC                                                 
         BNE   GETCPJ08                                                         
         LLC   RF,LIDLN                                                         
         AHI   RF,-(LIDLNDQ+1)                                                  
         MVC   CPJCOFF(0),LIDDATA  LIST OF OFFICES IN PRODS ON THIS CLI         
         EX    RF,*-6                                                           
         B     GETCPJ08                                                         
*                                                                               
GETCPJX  L     R1,SAVER1                                                        
         ST    R4,0(R1)            RETURN R4 IN THE FIRST PARAMETER             
         B     OVROU1E                                                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* CHECK NON CLIENT CODE IS VALID AND RETURN NAME                      *         
*                                                                     *         
* NTRY   P1 BYTE 0   - LENGTH OF ACCOUNT CODE OR 0                    *         
*           BYTE 1-3 - NON CLIENT ACCOUNT CODE OR LIDDATA             *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
VALNCL   XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         LR    RF,R2                                                            
         XR    RE,RE                                                            
         ICM   RE,1,0(R1)          GET LENGTH OF NON CLIENT ACCOUNT             
         BNZ   VALNCL04                                                         
         USING LIDDATA,R2                                                       
         MVC   NCLAPTY,LIDASTAT    NO LENGTH IT'S LIDDATA                       
         MVC   NCLCODE,LIDA1NAC                                                 
         B     VALNCL08                                                         
         DROP  R2                                                               
*                                                                               
VALNCL04 XC    NCLAPTY,NCLAPTY                                                  
         MVC   NCLCODE,BCSPACES                                                 
         MVC   NCLCODE(0),0(RF)                                                 
         EX    RE,*-6                                                           
*                                                                               
VALNCL08 MVC   NCLNAME,BCSPACES                                                 
         MVC   T.ACTKEY,BCSPACES   READ NON CLIENT ACCOUNT RECORD               
         MVC   T.ACTKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ACTKUNT(L'BCCPYPRD),=C'1N'                                     
         MVC   T.ACTKACT,NCLCODE                                                
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VALNCLX                                                          
         L     RF,12(R1)                                                        
         USING NAMELD,RF                                                        
                                                                                
         LLC   R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         BM    VALNCLX                                                          
         MVC   NCLNAME(0),NAMEREC                                               
         EX    R1,*-6                                                           
VALNCLX  B     OVROU1E                                                          
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* CHECK FILTER VALUE                                                  *         
*                                                                     *         
* EXIT - CC EQUAL     = CODE IS VALID                                 *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALFLT   DS    0H                                                               
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         MVC   FLTTYPE,0(R1)                                                    
*                                                                               
         CLI   CSACT,A#DLOAD       DOWNLOAD ??                                  
         BNE   OVROU1E                                                          
*                                                                               
X        USING APPRECD,IOKEY                                                    
         XC    X.APPKEY,X.APPKEY                                                
         MVI   X.APPKTYP,APPKTYPQ                                               
         MVI   X.APPKSUB,APPKSUBQ                                               
         MVC   X.APPKCPY,CUABIN                                                 
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         B     VALFLT03                                                         
*                                                                               
VALFLT02 LHI   R1,XOSEQ+XOACCDIR+XIO2                                           
         GOTO1 AIO                                                              
*                                                                               
VALFLT03 CLC   X.APPKCPY,CUABIN                                                 
         BNE   VALFLT99                                                         
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
*                                                                               
         L     R4,AIO2                                                          
         LA    R4,X.APPRFST-X.APPRECD(R4)                                       
*                                                                               
         USING LIDELD,R4                                                        
VALFLT40 CLI   LIDEL,0                                                          
         BE    VALFLT02            NEXT RECORD                                  
*                                                                               
         CLI   LIDEL,LIDELQ        LIST ELEMENT ?                               
         BE    VALFLT46                                                         
*                                                                               
VALFLT45 LLC   R1,LIDLN                                                         
         AR    R4,R1                                                            
         B     VALFLT40            NEXT ELETMENT                                
*                                                                               
VALFLT46 LLC   R5,LIDITLN                                                       
         LLC   R0,LIDLN                                                         
         SHI   R0,LIDDATA-LIDELD                                                
         SRDL  R0,32                                                            
         DR    R0,R5                                                            
         LA    R5,LIDDATA                                                       
*                                                                               
VALFLT47 CLI   LIDTYPE,LIDTAPSJ    CLIENT/PROD/JOB?                             
         BNE   VALFLT49                                                         
         CLI   FLTTYPE,DOFIFCPJ    COMPARE CLI/PRO/JOB?                         
         BNE   VALFLT48            NO - CLI/PRO/JOB FOUND                       
         CLC   0(L'SVFCPJ,R2),LIDASJAC-LIDDATA(R5)                              
         BE    OVROU1E                                                          
         B     VALFLT90                                                         
*                                                                               
VALFLT48 CLI   FLTTYPE,DOFIFMED    COMPARE MEDIA CODE?                          
         BNE   VALFLT45            NO - MEDIA FOUND                             
         CLC   0(L'SVFMED,R2),LIDASJME-LIDDATA(R5)                              
         BE    OVROU1E                                                          
         B     VALFLT90                                                         
*                                                                               
VALFLT49 CLI   LIDTYPE,LIDTAP1N    NON CLIENT ACCOUNT CODE?                     
         BNE   VALFLT50                                                         
         CLI   FLTTYPE,DOFIFNCC    COMPARE NON-CLIENT CODE?                     
         BNE   VALFLT45            NO - NON-CLIENT CODE FOUND                   
         CLC   0(L'SVFNCC,R2),LIDA1NAC-LIDDATA(R5)                              
         BE    OVROU1E                                                          
         B     VALFLT90                                                         
*                                                                               
VALFLT50 CLI   LIDTYPE,LIDTAP1R    1R COSTING ACCOUNTS?                         
         BNE   VALFLT51                                                         
         CLI   FLTTYPE,DOFIF1RC    COMPARE COSTING 1R CODE?                     
         BNE   VALFLT45            NO - COSTING 1R CODE FOUND                   
         CLC   0(L'SVF1RC,R2),LIDAPACC-LIDDATA(R5)                              
         BE    OVROU1E                                                          
         B     VALFLT90                                                         
*                                                                               
VALFLT51 CLI   LIDTYPE,LIDTINOR    APPROVAL INVOICES/ORDERS                     
         BNE   VALFLT45                                                         
         CLI   FLTTYPE,DOFIFODC    COMPARE OFF/DEPT CODE?                       
         BNE   VALFLT52                                                         
         CLC   0(L'SVFODC,R2),LIDAPOFF-LIDDATA(R5)                              
         BE    OVROU1E                                                          
         B     VALFLT90                                                         
*                                                                               
VALFLT52 CLI   FLTTYPE,DOFIFETC    COMPARE EXPENDITURE TYPE CODE?               
         BNE   VALFLT53                                                         
         CLC   0(L'SVFPSC,R2),LIDAPETY-LIDDATA(R5)                              
         BE    OVROU1E                                                          
         B     VALFLT90                                                         
*                                                                               
VALFLT53 CLI   FLTTYPE,DOFIFPSC    COMPARE PROD/SUPP CODE?                      
         BNE   VALFLT54                                                         
         CLC   0(L'SVFPSC,R2),LIDAPAC-LIDDATA(R5)                               
         BE    OVROU1E                                                          
         B     VALFLT90                                                         
*                                                                               
VALFLT54 CLI   FLTTYPE,DOFIFMED    COMPARE MEDIA CODE?                          
         BNE   VALFLT55                                                         
         CLC   0(L'SVFMED,R2),LIDAPMED-LIDDATA(R5)                              
         BE    OVROU1E                                                          
         B     VALFLT90                                                         
*                                                                               
VALFLT55 CLI   FLTTYPE,DOFIFAPV    COMPARE APPROVAL VALUE?                      
         BNE   VALFLT90                                                         
         CLC   0(L'SVFAPV,R2),LIDAPVAL-LIDDATA(R5)                              
         BE    OVROU1E                                                          
*                                                                               
VALFLT90 LLC   R0,LIDITLN          LENGTH OF ITEMS                              
         AR    R5,R0                                                            
         BCT   R1,VALFLT47         CHECK NEXT ENTRY                             
         B     VALFLT45            GET NEXT ELEMENT                             
VALFLT99 MVC   FVMSGNO,=AL2(AE$RECNF)                                           
         B     OVROU1L                                                          
         DROP  R4,X                                                             
         EJECT ,                                                                
***********************************************************************         
* CHECK COSTING 1R CODE IS VALID AND RETURN NAME                      *         
*                                                                     *         
* NTRY   P1 BYTE 0   - LENGTH OF ACCOUNT CODE OR 0                    *         
*           BYTE 1-3 - 1R ACCOUNT CODE OR LIDDATA                     *         
* EXIT - CC EQUAL     = CODE IS VALID                                 *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
VALC1R   DS    0H                                                               
*&&US*&& NI    ERRIND,X'FF'-ERCOFIN                                             
         XR    R2,R2                                                            
         ICM   R2,7,1(R1)                                                       
         LR    RF,R2                                                            
         XR    RE,RE                                                            
         ICM   RE,1,0(R1)          GET LENGTH 1R ACCOUNT                        
         BNZ   VALC1R04                                                         
         USING LIDDATA,R2                                                       
         MVC   C1RDTY,LIDAPDTY     NO LENGTH IT'S LIDDATA                       
         MVC   C1RDT2,LIDAPDT2                                                  
         MVC   C1RCODE,LIDAPACC                                                 
*&&US                                                                           
         MVC   MYHALF,LIDAPACC                                                  
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   *+10                                                             
         MVC   MYHALF+1(1),BCSPACES 1 CHARACTER                                 
         GOTO1 ATSTOFF,MYHALF                                                   
         BE    *+14                                                             
         OI    ERRIND,ERCOFIN       INVALID OFFICE / NOT IN LIMLIST             
         MVC   FVXTRA,BCSPACES                                                  
*&&                                                                             
         B     VALC1R08                                                         
         DROP  R2                                                               
*                                                                               
VALC1R04 MVI   C1RDTY,0                                                         
         MVI   C1RDT2,0                                                         
         MVC   C1RCODE,BCSPACES                                                 
         MVC   C1RCODE(0),0(RF)                                                 
         EX    RE,*-6                                                           
*&&US                                                                           
         MVC   MYHALF,C1RCODE                                                   
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   *+10                                                             
         MVC   MYHALF+1(1),BCSPACES 1 CHARACTER                                 
         GOTO1 ATSTOFF,MYHALF                                                   
         BE    *+14                                                             
         MVC   FVXTRA,BCSPACES                                                  
         B     OVROU1H                                                          
*&&                                                                             
*                                                                               
VALC1R08 MVC   C1RNAME,BCSPACES                                                 
         MVC   T.ACTKEY,BCSPACES   READ NON CLIENT ACCOUNT RECORD               
         MVC   T.ACTKCPY,CUABIN      CONNECTED ID                               
         MVC   T.ACTKUNT(2),ONERUL                                              
         MVC   T.ACTKACT,C1RCODE                                                
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VALC1RX                                                          
         L     RF,12(R1)                                                        
         USING NAMELD,RF                                                        
                                                                                
         LLC   R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         BM    VALC1RX                                                          
         MVC   C1RNAME(0),NAMEREC                                               
         EX    R1,*-6                                                           
VALC1RX  B     OVROU1E                                                          
         EJECT ,                                                                
***********************************************************************         
* GET VALUES FROM APPROVAL INVOICES/ORDERS LIST ELEMENT               *         
*                                                                     *         
* NTRY   P1 BYTE 0   - 0                                              *         
*           BYTE 1-3 - LIDDATA                                        *         
***********************************************************************         
         SPACE 1                                                                
GETAPL   L     R2,0(R1)                                                         
         USING LIDDATA,R2                                                       
         XC    APRTYP,APRTYP                                                    
         XC    APRTY2,APRTY2                                                    
         XC    APRTY3,APRTY3                                                    
         MVC   APRTYP,LIDAPTYP                                                  
         MVC   APRTY2,LIDAPTY2                                                  
         MVC   APRTY3,LIDAPTY3                                                  
         MVC   APROTY,LIDAPSCT                                                  
         LLC   RF,APRSSEQ                                                       
         AHI   RF,1                                                             
         STC   RF,APRSSEQ                                                       
***      MVC   APRAPL,LIDAPTY2                                                  
***      NI    APRAPL,LIDAPALM                                                  
         MVC   APRACA,LIDAPAC                                                   
         MVC   APRMED,LIDAPMED                                                  
         MVC   APROFF,LIDAPOFF                                                  
*&&US                                                                           
         GOTO1 ATSTOFF,LIDAPOFF                                                 
         BE    *+14                                                             
         OI    ERRIND,ERCOFIN       INVALID OFFICE / NOT IN LIMLIST             
         MVC   FVXTRA,BCSPACES                                                  
*&&                                                                             
         MVC   APRDPT,LIDAPDPT                                                  
         MVC   APRETY,LIDAPETY                                                  
         MVC   APRVAL,LIDAPVAL                                                  
         MVC   APRSEL,LIDAPSEL                                                  
         B     OVROU1E                                                          
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* GET BACKUP APPROVER                                                 *         
* NTRY   P1 BYTE 0   - 0                                              *         
*           BYTE 1-3 - LIDDATA                                        *         
***********************************************************************         
         SPACE 1                                                                
GETBAK   L     R2,0(R1)                                                         
*&&US*&& NI    ERRIND,X'FF'-ERCOFIN                                             
         USING LIDDATA,R2                                                       
         XC    APRTYP,APRTYP                                                    
         XC    APRTY2,APRTY2                                                    
         MVC   APRTYP,LIDLAPPL     SAVE OFF APPLICATION BYTES                   
         MVC   APRTY2,LIDLAPP2                                                  
         MVC   BAKBPID,LIDLPID                                                  
         MVC   MYWORK,LIDLPID                                                   
         GOTOX ('GETPID',AGROUTS),MYWORK                                        
         MVC   BAKCPID,BCWORK      GET 8 CHAR PID                               
*&&US                                                                           
         L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    GETBAKE                                                          
         GOTO1 ACHKPID,BOPARM,LIDLPID                                           
         BE    GETBAKE                                                          
         OI    ERRIND,ERCOFIN       INVALID OFFICE / NOT IN LIMLIST             
*&&                                                                             
*                                                                               
GETBAKE  B     OVROU1E                                                          
***********************************************************************         
* CHECK OFFICE CODE FOR THE PID IF AGENCY IS ON LIMIT ACCESSS OFFICE  *         
*                                                                     *         
* NTRY - P1  = PERSON CODE                                            *         
* EXIT - CC EQUAL = OK                                                *         
*      - CC NOT EQUAL = SECURITY LOCK                                 *         
***********************************************************************         
         SPACE 1                                                                
T        USING PIDRECD,IOKEY                                                    
CHKPID   L     R2,0(R1)                                                         
         XC    BCFULL,BCFULL                                                    
         MVC   SVIOKEY,IOKEY       SAVE IOKEY                                   
         XC    T.PIDKEY,T.PIDKEY   BUILD KEY TO READ                            
         MVI   T.PIDKTYP,PIDKTYPQ                                               
         MVI   T.PIDKSUB,PIDKSUBQ                                               
         MVC   T.PIDKCPY,CUABIN                                                 
         MVC   T.PIDKPID,0(R2)                                                  
         MVI   T.PIDKSTYP,PIDKPERQ                                              
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
*&&UK*&& BNE   CHKPIDL                                                          
*&&US*&& BNE   CHKPIDE                                                          
         CLC   T.PIDKEY(PIDKPER-PIDRECD),IOKEYSAV                               
*&&UK*&& BNE   CHKPIDL                                                          
*&&US*&& BNE   CHKPIDE                                                          
         DROP  T                                                                
*                                                                               
         L     R4,AIO2                                                          
         LA    R4,PERRFST-PERRECD(R4)                                           
         USING LOCELD,R4                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
CHKPID04 IC    R0,LOCLN                                                         
         AR    R4,R0                                                            
         CLI   LOCEL,0             END OF RECORD                                
*&&UK*&& BE    CHKPIDL             ACTIVE LOCATION NOT FOUND                    
*&&US*&& BE    CHKPID06                                                         
         CLI   LOCEL,LOCELQ                                                     
         BNE   CHKPID04                                                         
*&&US                                                                           
         ST    R4,BCFULL           SAVE ELEM ADDR AND KEEP READING              
         B     CHKPID04                                                         
*        CLI   LOCSTAT,LOCSTRM     TEST TERMINATED                              
*        BE    CHKPID06            NO - GET NEXT                                
*        CLI   LOCSTAT,LOCSLOA     TEST LOA                                     
*        BE    CHKPID06            NO - GET NEXT                                
*&&                                                                             
         CLI   LOCSTAT,LOCSACT     TEST ACTIVE LOCATION                         
         BNE   CHKPID04            NO - GET NEXT                                
CHKPID06 DS    0H                                                               
*&&US                                                                           
         ICM   R4,15,BCFULL        LOAD MOST CURRENT LOCATION ELEM              
         BZ    CHKPIDL             EXIT IF NO LOC FOUND                         
*&&                                                                             
         GOTO1 ATSTOFF,LOCOFF      TEST OFFICE                                  
         BE    CHKPIDE                                                          
*                                                                               
CHKPIDL  MVC   FVXTRA,BCSPACES                                                  
         XC    FVMSGNO,FVMSGNO                                                  
         MVC   IOKEY,SVIOKEY                                                    
         B     OVROU1L                                                          
*                                                                               
CHKPIDE  MVC   IOKEY,SVIOKEY                                                    
         B     OVROU1E                                                          
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* VALIDATE APPROVER/LIMIT FIELD                                       *         
*                                                                     *         
* NTRY - P1  = FIELD LENGTH, APPROVER/OFFICE LIMIT FIELD              *         
* EXIT - MYPL6 = APPROVER/OFFICE LIMIT IN PACK FORMAT                 *         
*        MYBYTE = APLIMIT LEVEL FOR TSARKEY                           *         
***********************************************************************         
                                                                                
VALLMT   XR    R2,R2                                                            
         MVI   MYBYTE,0            INIT APLIM LVL RETURN                        
         ICM   R2,7,1(R1)                                                       
         LLC   RF,0(R1)                                                         
         GOTO1 VCASHVAL,BOPARM,(C'0',(R2)),(RF),0                               
         CLI   0(R1),X'FF'                                                      
         BE    VLMTNV              INVALID LIMIT                                
         ZAP   MYPL6,6(L'MYPL6,R1)                                              
         CP    MYPL6,BCPZERO                                                    
         BE    VALLMTX             OK - ZERO AMOUNT                             
         BL    VLMTNV              INVALID - NEGATIVE AMOUNT                    
*                                                                               
T        USING ALIRECD,IOKEY                                                    
VALLMT08 XC    T.ALIKEY,T.ALIKEY   READ APPROVAL LIMIT REC(S)                   
         MVI   T.ALIKTYP,ALIKTYPQ                                               
         MVI   T.ALIKSUB,ALIKSUBQ                                               
         MVC   T.ALIKCPY,CUABIN                                                 
*                                                                               
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         B     *+8                                                              
VALLMT20 LHI   R1,XOSEQ+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE   VLMTNV                                                           
         CLC   T.ALIKEY(ALIKCAT-ALIKEY),IOKEYSAV                                
         BNE   VLMTVNAL            YES, NO VALUE FOUND                          
*                                                                               
VALLMT24 L     RF,AIO2                                                          
         LA    RF,ALIRFST-ALIRECD(RF)                                           
         SR    R0,R0                                                            
         USING APLELD,RF                                                        
         MVI   MYBYTE,0            INIT APLIM LVL RETURN                        
VALLMT28 CLI   APLEL,0                                                          
         BE    VALLMT20            EOR - READ NEXT                              
         CLI   APLEL,APLELQ                                                     
         BE    *+14                                                             
VALLMT32 IC    R0,APLLN                                                         
         AR    RF,R0                                                            
         B     VALLMT28                                                         
         IC    R0,MYBYTE                                                        
         AHI   R0,1                                                             
         STC   R0,MYBYTE                                                        
         CP    MYPL6,APLVAL                                                     
         BNE   VALLMT32                                                         
*                                                                               
VALLMTX  B     OVROU1E                                                          
         DROP  T,RF                                                             
*                                                                               
VLMTNV   MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     OVROU1L                                                          
*                                                                               
VLMTVNAL MVC   FVMSGNO,=AL2(AE$VNAPL)  VALUE DOESN'T EXIST IN ANY               
         MVC   FVXTRA(15),0(R2)        APLIM RECORD                             
         B     OVROU1L                                                          
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO GET LIST ITEM                                            *         
*                                                                     *         
* NTRY: P1=LIST TYPE,(CURR DISP INTO RECORD/ELEMENT PAIR)             *         
* EXIT: SET A(LIST ENTRY) IN P3                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING LIDELD,R4                                                        
         USING APPRECD,R2                                                       
GETLIT   L     R2,4(R1)                                                         
         ICM   R5,B'0111',1(R1)                                                 
         LH    R4,0(,R5)                                                        
         AR    R4,R2               R4=A(CURRENT LIST ELEMENT)                   
*                                                                               
         LH    RE,2(,R5)                                                        
         AR    RE,R4               RE=A(CURRENT LIST ENTRY INTO ELEM)           
         ST    RE,8(R1)                                                         
*                                                                               
         LLC   RF,LIDITLN          LENGTH OF LIST ENTRY                         
         AR    RE,RF               POINTS TO NEXT LIST ENTRY                    
         SR    RE,R4               RE=CURR DISP INTO ELEMENT                    
         IC    RF,LIDLN            LENGHT OF ELEMENT                            
         CR    RE,RF               ANY MORE LIST ENTRY IN THIS ELEM?            
         BNL   *+12                NO - GET THE NEXT LIST ELEMENT               
         STH   RE,2(,R5)                                                        
         B     OVROU1E                                                          
*                                                                               
         GOTO1 AGETNLE,(R1)        GET NEXT LIST ELEMENT                        
         BNE   OVROU1L                                                          
         B     OVROU1E                                                          
         DROP  R2,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GET NEXT LIST ELEMENT                                    *         
*                                                                     *         
* NTRY: P1=(LIST TYPE,(CURR DISP INTO RECORD/ELEMENT PAIR)            *         
* EXIT: SET DISPLACEMENT INTO RECORD/ELEMENT IN P1                    *         
***********************************************************************         
         SPACE 1                                                                
         USING LIDELD,R4                                                        
         USING APPRECD,R2                                                       
GETNLE   L     R2,4(R1)                                                         
         MVC   MYBYTE,0(R1)                                                     
         ICM   R5,B'0111',1(R1)                                                 
         LH    R4,0(,R5)                                                        
         LTR   R4,R4               IF NO DISPLACEMENT, SET TO 1ST ELEM          
         BZ    *+10                                                             
         AR    R4,R2               R4=A(CURRENT ELEMENT)                        
         B     GETNL10                                                          
         LA    R4,APPRFST                                                       
         B     *+12                                                             
GETNL10  SR    RF,RF                                                            
         IC    RF,LIDLN                                                         
         AR    R4,RF                                                            
         CLI   LIDEL,0             RECORD END?                                  
         BE    GETNLEL             YES                                          
         CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   GETNL10             NO                                           
         CLC   LIDTYPE,MYBYTE      MATCH LIST TYPE                              
         BNE   GETNL10                                                          
         SR    R4,R2                                                            
         STH   R4,0(,R5)           SET DISPLACEMENT INTO RECORD                 
         LHI   RF,LIDDATA-LIDELD                                                
         STH   RF,2(,R5)           SET DISPLACEMENT INTO ELEMENT                
         B     OVROU1E                                                          
*                                                                               
GETNLEL  XC    0(4,R5),0(R5)       CLEAR DISPLACEMENT TO RECORD/ELEMENT         
         B     OVROU1L             SET CC LOW                                   
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* CHECK FOR DUPLICATE 1R ACCOUNT CODES                                *         
* ON ENTRY: AIO5 - A LIST OF 1R ACCOUNTS TO BE UPDATED                *         
* ON EXIT : MARK ALL 1R ACCOUNTS IN THE LIST IF THEY ARE DUPLICATED   *         
*         : MYWORK - DUPLICATE ACCOUNT                                *         
***********************************************************************         
         SPACE 1                                                                
S        USING ACDTABD,R5                                                       
C        USING ACDTABD,R4                                                       
CHKDAC   MVC   MYWORK,BCSPACES                                                  
         CLI   LEN1RA,L'ACTKACT    SINGLE LEVER 1R ACCOUNT!!!                   
         BE    OVROU1E                                                          
*                                                                               
CHKDAC02 L     R5,AIO5                                                          
         LR    R4,R5                                                            
         LHI   R3,L'ACTKACT                                                     
*                                                                               
CHKDAC10 LA    R4,ACDTABL(,R4)     POINT TO NEXT ITEM                           
         OC    S.ACDTCDE,S.ACDTCDE END OF TABLE ?                               
         BZ    OVROU1E             YES - EXIST                                  
         OC    C.ACDTCDE,C.ACDTCDE END OF CURRENT ITEM ?                        
         BZ    CHKDAC14            YES - CHECK NEXT ITEM                        
         TM    S.ACDTIND,ACDTLOW   LOWEST LEVEL ACCOUNT                         
         BO    CHKDAC14            YES - GET NEXT ONE                           
         CLC   S.ACDTCDE,BCSPACES  ANY ACCOUNT CODE?                            
         BNH   CHKDAC14            NO - GET NEXT ONE                            
         CHI   R3,L'ACTKACT        DO WE HAVE ACCOUNT LENGTH ?                  
         BNE   *+8                 YES - FINE                                   
         BAS   RE,GETACLN          R3=ACCOUNT LEN FOR S.ACDTCDE                 
         EXCLC R3,S.ACDTCDE,C.ACDTCDE                                           
         BE    CHKDAC20                                                         
CHKDAC14 LA    R5,ACDTABL(,R5)     CHECK NEXT ITEM                              
         LR    R4,R5                                                            
         LHI   R3,L'ACTKACT        CALL GETACLN AGAIN                           
         B     CHKDAC10                                                         
                                                                                
CHKDAC20 MVC   MYBYTE,S.ACDTAPP                                                 
         NC    MYBYTE,C.ACDTAPP    DUPLICATE ACCOUNT ?                          
         BZ    CHKDAC10                                                         
         MVC   MYWORK(L'ACDTCDE),S.ACDTCDE                                      
         B     OVROU1L             YES - SHOW ERROR                             
                                                                                
GETACLN  IC    R3,LEN1RA                                                        
         LA    R1,S.ACDTCDE(R3)                                                 
         CLI   0(R1),C' '                                                       
         BE    GETACLNX                                                         
         IC    R3,LEN1RB                                                        
         CLI   LEN1RB,L'ACTKACT                                                 
         BE    GETACLNX                                                         
         LA    R1,S.ACDTCDE(R3)                                                 
         CLI   0(R1),C' '                                                       
         BE    GETACLNX                                                         
         IC    R3,LEN1RC                                                        
         CLI   LEN1RC,L'ACTKACT                                                 
         BE    GETACLNX                                                         
         LA    R1,S.ACDTCDE(R3)                                                 
         CLI   0(R1),C' '                                                       
         BE    GETACLNX                                                         
         IC    R3,LEN1RD                                                        
*                                                                               
GETACLNX SHI   R3,1                LENGTH OF ACCOUNT - 1                        
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
* READ NEXT APPROVER RECORD                                           *         
*                                                                     *         
* NTRY - P1  = A(MAIN APPROVER RECORD)                                *         
* EXIT - IO5      : NEXT APPROVER RECORD                              *         
*      - READSEQ# : NEXT RECORD SEQUENCE NUMBER                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRECD,IOKEY                                                    
NXTAPR   L     RF,0(R1)            A(CURRENCT APPROVER RECORD)                  
         MVC   T.APPKEY,0(RF)                                                   
NXTAPR02 LLC   RF,READSEQ#                                                      
         AHI   RF,1                                                             
         STC   RF,READSEQ#                                                      
         STC   RF,T.APPKSEQ                                                     
*                                                                               
         L     R1,=AL4(XOHID+XOACCMST+XIO5)                                     
         GOTO1 AIO                                                              
         BE    NXTAPR04                                                         
         TM    IOERR,IOEDEL                                                     
         BZ    OVROU1L                                                          
         B     NXTAPR02                                                         
NXTAPR04 CLC   IOKEY(APPKSEQ-APPRECD),IOKEYSAV                                  
         BNE   OVROU1L                                                          
         MVC   READSEQ#,IOKEY+(APPKSEQ-APPRECD)                                 
         B     OVROU1E                                                          
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* ADD APPROVER RECORD                                                 *         
*                                                                     *         
* NTRY - P1     = A(CURRENT APPROVER RECORD)                          *         
*      - BOELEM = CURRENT LIMIT LIST ELEMENT                          *         
* EXIT - ADDSEQ# : NEXT RECORD SEQUENCE NUMBER                        *         
***********************************************************************         
         SPACE 1                                                                
         USING APPRECD,R2                                                       
ADDAPR   L     R2,0(R1)            A(CURRENCT APPROVER RECORD)                  
*                                                                               
ADDAPR04 SR    RF,RF                                                            
         ICM   RF,3,APPRLEN                                                     
         CHI   RF,IOMAXLNQ         GREATER MAX RECORD ALLOWED ?                 
         BNH   ADDAPR16            NO - ADD IT INTO CURRENT RECORD              
*                                                                               
         CLI   ADDSEQ#,0           MAIN APPROVER RECORD ?                       
         BE    ADDAPR08                                                         
         GOTO1 AUPDAPR             NO - UPDATE SAVED APPR REC IN AIO6           
*                                                                               
ADDAPR08 L     R2,AIO6             R2=A(NEW APPROVER SUB-RECORD)                
         L     RF,AIOREC                                                        
         MVC   APPKEY(APPRLNK-APPRECD),0(RF)                                    
         LLC   RF,ADDSEQ#                                                       
         AHI   RF,1                                                             
         STC   RF,ADDSEQ#          NEXT SEQUENCE NUMBER                         
         STC   RF,APPKSEQ                                                       
         CLI   APPKSEQ,MAXRECSQ    TEST MAX APPROVER RECORDS REACH              
         BNL   ADDAPRL             YES - TOO MANY LINES                         
                                                                                
         MVC   IOKEY(L'APPKEY),APPKEY                                           
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO6)                                   
         GOTO1 AIO                                                              
         BE    ADDAPR12                                                         
         TM    IOERR,IOEDEL                                                     
         BNZ   ADDAPR12                                                         
         TM    IOERR,IOERNF                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RECFLAG,RECFADD     ADD NEW APPROVER RECORD                      
         LA    RF,APPRFST                                                       
         MVI   0(RF),0                                                          
         SR    RF,R2                                                            
         AHI   RF,1                                                             
         STCM  RF,3,APPRLEN        LENGTH OF APPROVER RECORD                    
         MVC   APPKSTA,GSRECSTA    COPY STATUS FROM MAIN RECORD                 
         MVI   RECFLAG,1                                                        
         MVC   IOKEY(L'APPKEY),APPKEY                                           
         B     ADDAPR16                                                         
*                                                                               
ADDAPR12 MVI   RECFLAG,RECFPUT     PUT APPROVER RECORD                          
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO6)                                  
         GOTO1 AIO                                                              
         BE    ADDAPR04                                                         
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
ADDAPR16 MVC   APPRSTA,GSRECSTA    COPY STATUS FROM MAIN RECORD                 
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),APPRECD,BOELEM                     
         CLI   12(R1),0                                                         
         BE    OVROU1E                                                          
ADDAPRL  MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     OVROU1L             SOMETHING WRONG!!!                           
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO UPDATE APPROVER RECORD                                   *         
* ETRY - AIO6: A(CURRENT RECORD) TO BE UPDATED                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING APPRECD,R4                                                       
UPDAPR   LA    R4,IOKEY                                                         
         L     RF,AIO6                                                          
         CLI   RECFLAG,0           DO WE WANT TO UPDATE APPROVER RECORD         
         BE    OVROU1E             NO - EXIT                                    
         MVC   T.APPKEY,0(RF)                                                   
         CLI   RECFLAG,RECFPUT     PUT RECORD?                                  
         BNE   UPDAPR10                                                         
*                                                                               
         LHI   R1,XOPUTREC+XOACCMST+XIO6 CHANGE APPROVER RECORD                 
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         NI    T.APPKSTAT,FF-APPSDELT    CHANGE NON DELETED APPR DIR            
         LHI   R1,XOWRITE+XOACCDIR+XIO6                                         
         GOTO1 AIO                                                              
         BE    UPDAPR20                                                         
         DC    H'0'                                                             
*                                                                               
UPDAPR10 LHI   R1,XOADDREC+XOACCMST+XIO6 ADD DIR + FILE RECORDS                 
         GOTO1 AIO                                                              
         BE    UPDAPR20                                                         
         DC    H'0'                ERROR - CAN'T PUT RECORD BACK                
*                                                                               
UPDAPR20 MVI   RECFLAG,0                                                        
         L     RF,AIOREC           RESTORE IOADDR FOR CONTROLLER                
         ST    RF,IOADDR                                                        
         B     OVROU1E                                                          
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SET ALL FIELDS ON THIS LINE TO INPUT THIS TIME           *         
***********************************************************************         
         SPACE 1                                                                
SETFLD   LH    R1,LSCURLIN         DISPLACEMENT TO CURRENT LINE                 
         A     R1,ATWA             SET ALL FIELDS TO INPUT THIS TIME            
         USING FHD,R1              R1=A(SUB-ACTION FIELD)                       
*                                                                               
         LA    RF,FHD              START OF THIS LINE                           
         AH    RF,LSLINLEN         LENGTH OF A LINE                             
         BCTR  RF,0                RF=A(END OF THIS LINE-1)                     
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         AR    R1,RE               R1=A(FIRST DATA FIELD)                       
*                                                                               
SFLD02   IC    RE,FHLN             SET INDEX IN RE                              
         OI    FHII,FHIITH         REVALIDATE EACH FIELD ON THIS LINE           
         BXLE  R1,RE,SFLD02        REPEAT FOR ALL FIELDS ON LINE                
         B     OVROU1E                                                          
         DROP  R1                                                               
         EJECT ,                                                                
                                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT ON                                                               
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* SEACSFILE                                                                     
         PRINT ON                                                               
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
         SPACE 1                                                                
OVROUT1  DS    0A                                                               
ADOFLT   DS    A                                                                
AIOCHK   DS    A                                                                
ADELPAS  DS    A                                                                
AADDPAS  DS    A                                                                
AVALCPJ  DS    A                                                                
AGETCPJ  DS    A                                                                
AVALNCL  DS    A                                                                
AVALC1R  DS    A                                                                
AGETAPL  DS    A                                                                
AGETBAK  DS    A                                                                
ACHKPID  DS    A                                                                
AVALLMT  DS    A                                                                
AGETLIT  DS    A                                                                
AGETNLE  DS    A                                                                
ACHKDAC  DS    A                                                                
ANXTAPR  DS    A                                                                
AADDAPR  DS    A                                                                
AUPDAPR  DS    A                                                                
ASETFLD  DS    A                                                                
AVALFLT  DS    A                                                                
OVROUT1N EQU   (*-OVROUT1)/L'OVROUT1                                            
         SPACE 1                                                                
APIDFLD  DS    A                   A(PERSON ID FIELD)                           
SAVER1   DS    A                   SAVE REGISTER 1                              
SAVERE   DS    A                   SAVE REGISTER 15                             
SVIOKEY  DS    CL(L'IOKEY)                                                      
READSEQ# DS    XL(L'APPKSEQ)                                                    
ADDSEQ#  DS    XL(L'APPKSEQ)                                                    
RECFLAG  DS    XL1                                                              
RECFADD  EQU   1                                                                
RECFPUT  EQU   2                                                                
*                                                                               
MYBYTE   DS    X                                                                
MYBYTE2  DS    X                                                                
MYBYTES  DS    X                                                                
MYHALF   DS    H                                                                
MYPL6    DS    PL6                                                              
MYWORK   DS    XL64                                                             
*                                                                               
*                                                                               
SESNL    DS    XL1                                                              
ANYLINES DS    CL1                                                              
TEMPDATE DS    PL3                 TEMPORARY DATE                               
SVCPXST6 DS    XL(L'CPXSTAT6)      SAVED COMPANY EXTRA STATUS 6                 
*                                                                               
NOEL     DS    XL1                 NO LIDELS FOUND INDICATOR                    
INTYPE   DS    CL1                 NO LIDELS FOUND INDICATOR                    
LSTAPDEF DS    CL1                 LAST APPROVER - DEFAULT ORDER TYPE           
LSTAPPOF DS    CL1                 LAST APPRVR-PURCHASING APPROVER APPL         
*                                                                               
CPJAPTY  DS    XL2                 CLI/PRO/JOB APPROVAL TYPE                    
CPJVALS  DS    0C                                                               
CPJCODE  DS    CL(L'TLKASJAC)      CLI/PRO/JOB CODE                             
CPJOFF   DS    CL(L'TLKASJOF)      PRODUCTION OFFICE CODE                       
CPJCLOF  DS    CL(L'TLKASJOF)      CLIENT OFFICE                                
CPJMED   DS    CL(L'TLKASJME)      PRODUCTION MEDIA CODE                        
CPJNAME  DS    CL(L'TLASJNM)       CLI/PRO/JOB NAME                             
CPJVALSQ EQU   *-CPJVALS                                                        
*                                                                               
NCLAPTY  DS    XL2                 NON-CLIENT APPROVAL TYPE                     
NCLCODE  DS    CL(L'TLKA1NAC)      NON-CLIENT ACCOUNT CODE                      
NCLNAME  DS    CL(L'TLA1NNM)       NON-CLIENT ACCOUNT NAME                      
C1RDTY   DS    XL(L'TLAPDTY)       COSTING 1R APPROVAL TYPE #1                  
C1RDT2   DS    XL(L'TLAPDTY)       COSTING 1R APPROVAL TYPE #2                  
C1RCODE  DS    CL(L'TLKA1RAC)      COSTING 1R ACCOUNT CODE                      
C1RNAME  DS    CL(L'TLA1RNM)       COSTING 1R ACCOUNT NAME                      
BAKCPID  DS    XL(L'SVPID)         BACKUP PID (8 CHAR)                          
BAKBPID  DS    XL2                 BINARY BACKUP PID                            
*                                                                               
APRTYP   DS    CL(L'TLAPTYP)       APPROVAL - STATUS BYTE 1                     
APRTY2   DS    CL(L'TLAPTY2)       APPROVAL - STATUS BYTE 2                     
APRTY3   DS    CL(L'TLAPTY3)       APPROVAL - STATUS BYTE 3                     
APROTY   DS    CL(L'TLKAPOTY)      APPROVAL - SUB CATEGORY                      
APRACA   DS    CL(L'TLKAPACA)      APPROVAL SJ/CREDITOR ACCOUNT                 
APRMED   DS    CL(L'TLKAPMED)      APPROVAL MEDIA                               
APROFF   DS    CL(L'TLKAPOFF)      APPROVAL OFFICE                              
APRDPT   DS    CL(L'TLKAPDPT)      APPROVAL OFFICE/DEPARTMENT                   
APRETY   DS    CL(L'TLKAPETY)      APPROVAL EXPENDITURE TYPE                    
APRSSEQ  DS    XL1                 APPROVAL APLIMIT TYPE/LEVEL                  
*PRAPL   DS    CL(L'TLKAPLM)       APPROVAL APLIMIT TYPE/LEVEL                  
APRVAL   DS    PL6                 APPROVAL VALUE                               
APRSEL   DS    PL6                 SELF APPROVAL VALUE                          
*                                                                               
*                                                                               
DSLSTL   DS    0D       LOWER CASE                                              
LC@JOBS  DS    CL4                 JOBS                                         
LC@DEF   DS    CL10                DEFAULT                                      
LC@2LSJH DS    CL8                 2 LEVEL APPROVAL HEADING FOR SJ              
LC@2L1RH DS    CL7                 2 LEVEL APPROVAL HEADING FOR 1R              
LC@DEPC  DS    CL20                DEPARTMENT CODE FIELD                        
LC@DPT   DS    CL8                 DEPT HEADING                                 
*                                                                               
DSLSTU   DS    0D       UPPER CASE                                              
UC@BOTH  DS    CL4                 BOTH                                         
*&&UK                                                                           
UC@EDITR DS    CL6                 EDITOR                                       
*&&                                                                             
UC@TIME  DS    CL5                 TIME                                         
UC@EXP   DS    CL7                 EXPENSE                                      
UC@FINCE DS    CL7                 FINANCE                                      
UC@DEFFI DS    CL10                DEFFINANCE                                   
UC@CLI   DS    CL(OTYPFLQ)                                                      
UC@NCLI  DS    CL(OTYPFLQ)                                                      
UC@EXP2  DS    CL(OTYPFLQ)                                                      
UC@PRO   DS    CL(OTYPFLQ)                                                      
UC@ARTST DS    CL(OTYPFLQ)                                                      
UC@INT2  DS    CL(OTYPFLQ)                                                      
UC@FLDEF DS    CL(OTYPFLQ)                                                      
UC@ORDS  DS    CL8                                                              
UC@INVS  DS    CL8                                                              
*                                                                               
CPTRWRK  DS    XL128                                                            
OVERWRKX EQU   *-OVERWRKD                                                       
         SPACE 1                                                                
***********************************************************************         
* SAVED DSECT                                                         *         
***********************************************************************         
         SPACE 2                                                                
SAVED    DSECT                                                                  
*                                                                               
APPINDS  DS    XL1                                                              
APPICLIO EQU   X'80'               SJ CLIENT INPUT ONLY (NOT PROD/JOB)          
APPIRSEQ EQU   X'40'               RESTORE IO READ SEQUENCE                     
APPIVCPJ EQU   X'20'               VALCPJ HAS BEEN CALLED                       
APPIDLFC EQU   X'10'               FLST HAS ALREADY BEEN CALLED                 
APPICCPJ EQU   X'04'               CHANGED CLIENT PRODUCT JOB                   
APPICPJV EQU   X'02'               COME FROM VALIDATING CPJ                     
APPIPSCV EQU   X'01'               COME FROM VALIDATING PRODCTN SUPP CD         
*                                                                               
DOFINDS  DS    0XL2                                                             
DOFIND1  DS    XL1                 DOFILT INDICATOR                             
DOFIFCPJ EQU   X'80'               CLI/PRO/JOB CODE FOUND                       
DOFIFNCC EQU   X'40'               NON-CLIENT CODE FOUND                        
DOFIF1RC EQU   X'20'               COSTING 1R CODE FOUND                        
DOFIFODC EQU   X'10'               OFFICE/DEPARTMENT CODE FOUND                 
DOFIFETC EQU   X'08'               EXPENDITURE CODE FOUND                       
DOFIFPSC EQU   X'04'               PRODUCTION/SUPPLIER CODE FOUND               
DOFIFMED EQU   X'02'               MEDIA CODE FOUND                             
DOFIFAPV EQU   X'01'               APPROVER LIMIT FOUND                         
DOFIND2  DS    XL1                 DOFILT INDICATOR                             
DOFI2BAP EQU   X'80'               BACK UP APPROVER FOUND                       
DOFI2OTH EQU   X'7F'                                                            
*                                                                               
DWNINDS  DS    XL1                 DOWNLOAD INDICATOR                           
DWNGDATA EQU   X'80'               GET DATA INTO TSAR                           
DWNNOALL EQU   X'0F'                                                            
DWNNOCPJ EQU   X'08'               NO MORE CLI/PRO/JOB CODE                     
DWNNONCC EQU   X'04'               NO MORE NON-CLIENT CODE                      
DWNNOC1R EQU   X'02'               NO MORE COSTING 1R CODE                      
DWNNOAPL EQU   X'01'               NO MORE APPROVER INVOICE/ORDER               
*                                                                               
DWNINDS2 DS    XL1                 SECOND DOWNLOAD INDICATOR                    
DWNNOBAK EQU   X'80'               NO MORE BACKUP APPROVERS                     
DWN2OALL EQU   DWNNOBAK                                                         
*                                                                               
ERRIND   DS    XL1                 ERROR INDICATOR                              
ERMAXIO  EQU   X'80'               MAX IOS RETURNED                             
EREOFIN  EQU   X'40'               EXISTING OFFICE INVALID                      
ERCOFIN  EQU   X'20'               CURRENT OFFICE INVALID                       
*                                                                               
*&&US                                                                           
WRNIND   DS    XL1                 WARNING INDICATOR                            
WRNTRMD  EQU   X'80'               TERMINATED DATE WARNING                      
*&&                                                                             
CLILEN   DS    XL1                 LENGTH OF CLIENT                             
PROLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT                     
JOBLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT+JOB                 
*                                                                               
FLTTYPE  DS    XL1                                                              
*                                                                               
CURITEM DS     XL1                 CURRENT 1R ITEM IN THE TABLE                 
LEN1RA   DS    XL1                 LENGTH OF LEVEL 1 FOR 1R                     
LEN1RB   DS    XL1                 LENGTH OF LEVEL 2 FOR 1R                     
LEN1RC   DS    XL1                 LENGTH OF LEVEL 3 FOR 1R                     
LEN1RD   DS    XL1                 LENGTH OF LEVEL 4 FOR 1R                     
LEN1RLNQ EQU   *-LEN1RA                                                         
LEN1RLOW DS    XL1                 LENGTH OF LOWEST LEVEL FOR 1R                
*                                                                               
MNTDISPD DS    0H                                                               
MNTDISP  DS    H                   CURR DISP. INTO RECORD/ELEMENT PAIR          
CURDISP  DS    H                                                                
MNTCPJ   DS    H                                                                
CURCPJ   DS    H                                                                
MNTNCLC  DS    H                                                                
CURNCLC  DS    H                                                                
MNTAP1R  DS    H                                                                
CURAP1R  DS    H                                                                
MNTAPPL  DS    H                                                                
CURAPPL  DS    H                                                                
MNTBACK  DS    H                                                                
CURBACK  DS    H                                                                
MNTDISPL EQU   *-MNTDISPD                                                       
DATALEN  DS    H                   LENGTH OF DATA                               
TOTELLN  DS    H                   TOTAL LENGTH OF ELEMENT                      
*                                                                               
SVPIDBIN DS    XL(L'APPKPIDB)      BINARY PID NUMBER                            
SVPIDD   DS    0C                                                               
SVPID    DS    CL(L'SAPALPID)      PID NUMBER                                   
SVPIDFNM DS    CL(L'SANAME)        PERSON ID FIRST NAME                         
SVPIDLNM DS    CL(L'SANAME)        PERSON ID LAST NAME                          
SVPIDMNM DS    CL(L'SANAME)        PERSON ID MIDDLE NAME                        
SVPIDLQ  EQU   *-SVPIDD                                                         
*                                                                               
SVBUPIDB DS    XL(L'APPKPIDB)      SAVED BACK-UP BINARY PID NUMBER              
SVBUPIDD DS    0C                                                               
SVBUPID  DS    CL(L'SAPALPID)      BACK-UP PID NUMBER                           
SVBUPIDF DS    CL(L'SANAME)        BACK-UP PERSON ID FIRST NAME                 
SVBUPIDL DS    CL(L'SANAME)        BACK-UP PERSON ID LAST NAME                  
SVBUPILQ EQU   *-SVBUPIDD                                                       
*                                                                               
SVFLTS   DS    0F                                                               
SVPIDFL  DS    CL(L'SAPALPID)      FILTER ON PID NUMBER                         
SVSTAFL  DS    0F                  FILTER ON STATUS                             
SVFJOBAP DS    CL1                 FILTER ON JOB APPROVER                       
SVFESTAP DS    CL1                 FILTER ON ESTIMATE APPROVER                  
SVFIESTA DS    CL1                 FILTER ON INTERNAL ESTIMATE APPRVER          
SVFFINAP DS    CL1                 FILTER ON FINANCE APPROVER                   
SVSTAFLQ EQU   *-SVSTAFL                                                        
SVFELEM  DS    0F                  FILTER ON ELEMENTS                           
SVFBAP   DS    XL2                 FILTER ON BACK UP PID NUMBER                 
SVFCPJXL DS    XL1                 LENGTH OF INPUT CLI/PRO/JOB - 1              
SVFCPJ   DS    CL(L'TLKASJAC)      FILTER ON CLI/PRO/JOB                        
SVFNCC   DS    CL(L'TLKA1NAC)      FILTER ON NON-CLIENT CODE                    
SVF1RC   DS    CL(L'TLKA1RAC)      FILTER ON COSTING 1R CODE                    
SVFETC   DS    CL(L'TLKAPETY)      FILTER ON EXPENDITURE TYPE CODE              
SVFODC   DS    0CL(L'SVFOFF+L'SVFDPT)                                           
SVFOFF   DS    CL(L'TLKAPOFF)      FILTER ON OFFICE CODE                        
SVFDPT   DS    CL(L'TLKAPDPT)      FILTER ON DEPARTMENT CODE                    
SVFPSC   DS    CL(L'TLKAPACA)      FILTER ON PRODUCTION/SUPPLIER CODE           
SVFMED   DS    CL(L'TLKAPMED)      FILTER ON MEDIA CODE                         
SVFAPV   DS    PL6                 FILTER ON APPROVER LIMIT                     
SVFELMLQ EQU   *-SVFELEM                                                        
SVFLTLQ  EQU   *-SVFLTS                                                         
*                                                                               
IOCOUNT  DS    H                   COUNT IO'S                                   
SKEYLAST DS    XL(L'ACTKEY)        SAVED CONTINUATION KEY FOR FLST              
CPJCOFF  DS    CL200               SAVED SJ CLIENT PROD OFFICES                 
SAVEDX   EQU   *-SAVED                                                          
         EJECT ,                                                                
***********************************************************************         
* DSECT TO COVER LEVEL TABLE                                          *         
***********************************************************************         
         SPACE 2                                                                
ACDTABD  DSECT                                                                  
ACDTIND  DS    AL1                                                              
ACDTLOW  EQU   X'80'               LOWEST LEVEL 1R ACCOUNT                      
ACDTAPP  DS    XL1                 1R APPLICATION                               
ACDTCDE  DS    CL12                1R ACCOUNT                                   
ACDTITL  EQU   *-ACDTAPP           ITEM LENGTH                                  
ACDTABL  EQU   *-ACDTABD                                                        
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER ORDER TYPE TABLE                                     *         
***********************************************************************         
         SPACE 2                                                                
APTYTABD DSECT                                                                  
APTYOTY  DS    XL1                 ORDER TYPE                                   
APTYCTRY DS    XL1                 COUNTRY                                      
APTYDISP DS    AL2                 DISPLACEMENT TO ORDER NAME                   
APTYLNQ  EQU   *-APTYTABD                                                       
         SPACE 2                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKPID   DS    CL(L'SAPALPID)      PERSONAL PID                                 
         ORG   TLKSRT                                                           
TLKASJAC DS    CL(L'LIDASJAC)      SJ ACCOUNT CODE                              
TLKASJOF DS    CL(L'LIDASJOF)      SJ OFFICE CODE                               
TLKASJME DS    CL(L'LIDASJME)      SJ MEDIA CODE                                
         ORG   TLKSRT                                                           
TLKA1NAC DS    CL(L'LIDA1NAC)      NON-CLIENT 1N ACCOUNT CODE                   
         ORG   TLKSRT                                                           
TLKA1RAC DS    CL(L'LIDAPACC)      COSTING 1R ACCOUNT CODE                      
         ORG   TLKSRT                                                           
TLKAPVL  DS    0C                                                               
TLKAPOFF DS    CL(L'LIDAPOFF)      APPROVAL OFFICE                              
TLKAPDPT DS    CL(L'LIDAPDPT)      APPROVAL DEPARTMENT                          
TLKAPETY DS    CL(L'LIDAPETY)      APPROVAL EXPENDITURE TYPE                    
TLKAPACA DS    CL(L'LIDAPAC)       APPROVAL SJ/CREDITOR ACCOUNT                 
TLKAPMED DS    CL(L'LIDAPMED)      APPROVAL MEDIA                               
TLKAPVLQ EQU   *-TLKAPVL                                                        
TLKAPOTY DS    CL(L'LIDAPSCT)      APPROVAL - SUB CATEGORY                      
TLOTDFT  EQU   LIDADFT             DEFAULT                                      
TLOTCLI  EQU   LIDACLI             CLIENT                                       
TLOTNCLI EQU   LIDANCLI            NON CLIENT                                   
TLOTEXP  EQU   LIDAEXP             EXPENSE                                      
TLOTPROD EQU   LIDAPROD            PRODUCTION                                   
TLOTART  EQU   LIDAART             ARTIST                                       
TLOTINT  EQU   LIDAINT             INTERNAL                                     
TLAPVAL  DS    PL6                 APPROVAL VALUE                               
*LKSSEQ  DS    XL1                                                              
         ORG   TLKSRT                                                           
TLKBCPID DS    CL8                 BACKUP PID (8 CHAR)                          
TLKBBPID DS    XL2                 BINARY BACKUP PID                            
         ORG   TLUSER                                                           
TLLSLNQ  EQU   *-TLSTD             TSAR RECORD LENGTH FOR LIST                  
*                                                                               
TLASTAT  DS    XL(L'LIDASTAT)      SJ ACCOUNT TYPE                              
TLASTIM  EQU   LIDATIME            TIMESHEET APPROVER                           
TLASEXP  EQU   LIDAEXPN            EXPENSE CLAIM APPROVER                       
TLASEST  EQU   LIDAESTY            ESTIMATE APPROVER                            
TLASESTD EQU   LIDAESTD            DEFAULT ESTIMATE APPROVER                    
TLASJOB  EQU   LIDAJOBY            JOB APPROVER                                 
TLASJOBD EQU   LIDAJOBD            DEFAULT JOB APPROVER                         
TLASTA2  DS    XL(L'LIDASTA2)      SJ STATUS BYTE 2                             
TLASESI  EQU   LIDAESI             INTERNAL ESTIMATE APPROVER                   
TLASESID EQU   LIDAESID            DEFAULT INTERNAL ESTIMATE APPROVER           
TLAEL1B  EQU   LIDAEL1B            EXPENSES LEVEL 1 BILLABLE                    
TLAEL1N  EQU   LIDAEL1N            EXPENSES LEVEL 1 NON-BILLABLE                
TLAEL2B  EQU   LIDAEL2B            EXPENSES LEVEL 2 BILLABLE                    
TLAEL2N  EQU   LIDAEL2N            EXPENSES LEVEL 2 NON-BILLABLE                
TLASJDAT DS    0C                                                               
TLASJNM  DS    CL(SJNAMFLQ)        SJ ACCOUNT NAME                              
TLSJLNQ  EQU   *-TLSTD                                                          
         ORG   TLASJDAT                                                         
TLA1NNM  DS    CL(L'NAMEREC)       1N ACCOUNT NAME                              
TL1NLNQ  EQU   *-TLSTD                                                          
         ORG   TLUSER                                                           
TLBAPP   DS    XL1                 BACKUP APPROVER APPLICATION                  
TLBTIME  EQU   LIDLTIME            TIMESHEET APPLICATION                        
TLBEXP   EQU   LIDLEXPN            EXPENSES APPLICATION                         
TLBAPP2  DS    XL1                 BACKUP APPROVER APPLICATION 2                
TLBTOFF  EQU   LIDLTOFF            TIME-OFF  APPLICATION                        
TLBKLNQ  EQU   *-TLSTD                                                          
         ORG   TLUSER                                                           
TLAPDTY  DS    XL(L'LIDAPDTY)      DEPT ACCOUNT TYPE (AURA)                     
TLAPTIM  EQU   LIDAPDTI            TIMESHEET APPROVER                           
TLAPDEX  EQU   LIDAPDEX            EXPENSE CLAIMS - LINE MANAGER/LVL 1          
TLAPDEF  EQU   LIDAPDEF            FINANCE APPROVER                             
TLAPDED  EQU   LIDAPDED            DEFAULT FINANCE APPROVER                     
TLAPDE1  EQU   LIDAPDE1            EXPENSE CLAIMS - LVL 1 APPR NON BILL         
TLAPDE2  EQU   LIDAPDE2            EXPENSE CLAIMS - LVL 2 APPR NON BILL         
TLAPDB1  EQU   LIDAPDB1            EXPENSE CLAIMS - LVL 1 APPR BILLABLE         
TLAPDB2  EQU   LIDAPDB2            EXPENSE CLAIMS - LVL 2 APPR BILLABLE         
TLAPDT2  DS    XL(L'LIDAPDT2)      DEPT ACCOUNT TYPE 2 (AURA)                   
TLAPDTO  EQU   LIDAPDTO            TIMEOFF                                      
TLA1RNM  DS    CL(L'NAMEREC)       COSTING 1R NAME                              
TL1RLNQ  EQU   *-TLSTD                                                          
         ORG   TLUSER                                                           
TLAPTYP  DS    CL(L'LIDAPTYP)      APPROVAL - STATUS BYTE 1                     
TLAPTY2  DS    CL(L'LIDAPTY2)      APPROVAL - STATUS BYTE 2                     
TLAPTY3  DS    CL(L'LIDAPTY3)      APPROVAL - STATUS BYTE 3                     
*LAPOR   EQU   LIDAPOR             ORDER - APPROVER BY VALUE                    
*LAPORD  EQU   LIDAPORD            ORDER - APPROVER BY VALUE DEFAULT            
*LAPOF   EQU   LIDAPOF             ORDER - FINANCE APPROVER                     
*LAPOFD  EQU   LIDAPOFD            ORDER - FINANCE APPROVER DEFAULT             
*LAPIN   EQU   LIDAPIN             INVOICES - APPROVER BY VALUE                 
*LAPIND  EQU   LIDAPIND            INVOICES - APPROVER BY VALUE DEFAULT         
TLAPSEL  DS    PL6                 SELF APPROVAL VALUE                          
TLAPLNQ  EQU   *-TLSTD                                                          
         ORG   TLUSER              ** TSAR FOR DOWNLOADING REPORT **            
*                                                                               
TLDLDAT  DS    0C                                                               
TLDASJA  DS    XL(L'LIDASTAT)      SJ APPLICATION                               
TLDASJA2 DS    XL(L'LIDASTA2)      SJ APPLICATION 2                             
TLDASJAC DS    CL(L'LIDASJAC)      SJ ACCOUNT CODE                              
TLDASJME DS    CL(L'LIDASJME)      SJ MEDIA CODE                                
TLDASJOF DS    CL(L'LIDASJOF)      SJ OFFICE CODE                               
TLDA1NA  DS    XL(L'LIDASTAT)      1N APPLICATION                               
TLDA1NA2 DS    XL(L'LIDASTA2)      1N APPLICATION 2                             
TLDA1NAC DS    CL(L'LIDA1NAC)      1N ACCOUNT CODE                              
TLDAPDTY DS    XL(L'LIDAPDTY)      1R APPLICATION #1                            
TLDAPDT2 DS    XL(L'LIDAPDT2)      1R APPLICATION #2                            
TLDA1RAC DS    CL(L'LIDAPACC)      COSTING 1R ACCOUNT CODE                      
*                                                                               
TLDAPTYP DS    CL(L'LIDAPTYP)      APPROVAL - STATUS BYTE 1                     
TLDAPTY2 DS    CL(L'LIDAPTY2)      APPROVAL - STATUS BYTE 2                     
TLDAPTY3 DS    CL(L'LIDAPTY3)      APPROVAL - STATUS BYTE 3                     
TLDAPOTY DS    CL(L'LIDAPSCT)      APPROVAL - SUB CATEGORY                      
TLDAPACA DS    CL(L'LIDAPAC)       APPROVAL SJ/CREDITOR ACCOUNT                 
TLDAPMED DS    CL(L'LIDAPMED)      APPROVAL MEDIA                               
TLDAPOFF DS    CL(L'LIDAPOFF)      APPROVAL OFFICE                              
TLDAPDPT DS    CL(L'LIDAPDPT)      APPROVAL DEPARTMENT                          
TLDAPETY DS    CL(L'LIDAPETY)      APPROVAL EXPENDITURE TYPE                    
TLDAPVAL DS    PL6                 APPROVAL VALUE                               
TLDAPSEL DS    PL6                 SELF APPROVAL VALUE                          
TLDSTAT  DS    XL(L'APPRSTAT)      APPROVAL STATUS                              
TLDSTA2  DS    XL(L'APPRSTA2)      APPROVAL STATUS 2                            
TLDBUPID DS    XL(L'PIDNO)         BACK-UP PERSON ID CODE                       
TLDBUAPP DS    XL(L'LIDASTAT)      BACK UP APPROVER APPLICATION 1               
TLDBUAP2 DS    XL(L'LIDASTAT)      BACK UP APPROVER APPLICATION 2               
TLDLDATL EQU   *-TLDLDAT                                                        
TLDLLNQ  EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067ACFIL44   02/16/21'                                      
         END                                                                    
