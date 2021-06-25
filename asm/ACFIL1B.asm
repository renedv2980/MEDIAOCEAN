*          DATA SET ACFIL1B    AT LEVEL 035 AS OF 03/20/20                      
*&&      SET   NOP=N                                                            
*PHASE T6231BA                                                                  
         TITLE 'NEW FILE COMPANY RECORD'                                        
         SPACE 2                                                                
* YNGX 230 16AUG99 USE LABLES TO REPRESENT FIELD NUMBERS                        
*MPEN 077 20MAY14 <DSRD-2353> OPTION FOR NEW AURA EMAILS                        
*MPEN 079 28MAY15 <DSPCA-1738> ADD AURA EM MODULE CONTROLS DSRD-7402            
*MPEN 084 15OCT15 <DSPCA-2076> PREVENT SETTING BOTH AUTO APPR/INTERNAL          
*MPEN     13JUN16 <DSPCA-2462> <DSPCA-2492> FEDERATED URL                       
*SGAV     07APR17 <SPEC-11986> CPYSAP2J - USE PAY2JOB                           
*MNAS 030 SEP16/17<DSRD-13993> CPYSGPDX - BULK API EXTRACT                      
*JSAY 031 MAY10/18<DSRD-18549> NEW CO RECORD SETTING FOR NET PAID VALUE         
*JSAY 032 MAR28/18<SPEC-18863> ADD G/L START DATE AND UPDATE SETTING            
*VGUP 033 11NOV19 <DSRD-24172> NEW FIELD BULK API PID                           
*MNAS 033 05DEC19 <DSRD-24361> SETTING TO DRIVE ESTIMATE RICH TEXT              
*ABID 034 21MAR20 <DSRD-25114> ORDERS - MF - EU NEW CO REC SETTING              
*                              TO DRIVE SUPPORT FOR ORDERS HTML TEXT            
         SPACE 2                                                                
FIL1B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL1B**,RA,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
         L     R5,ATWA                                                          
         AH    R5,=Y(TWUSER-TWAD)                                               
         USING TWUSER,R5                                                        
*                                                                               
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
         L     R1,CALLR1                                                        
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
EXIT     XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITELTS MVC   FVMSGNO,=AL2(AE$ELNOF)                           *SGAV           
         B     EXITL               EXIT WITH ELEMENT TOO SHORT  *SGAV           
EXITDT   MVC   FVMSGNO,=AL2(AE$INVDT) INVALID DATE                              
         B     EXITL                                                            
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     EXITOK                                                           
*                                                                               
DIE      DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     DS    0H                                                               
         L     R2,=A(DCLIST)          TRANSLATE UPPERCASE WORDS                 
         A     R2,BORELO                                                        
         GOTO1 VDICTAT,BOPARM,C'LU  ',(R2),DSLISTU                              
         CLI   CSACT,A#ADD                                                      
         BE    EXITOK                                                           
         CLI   CSACT,A#CPY                                                      
         BE    EXITOK                                                           
         OI    GSINDSL1,GSIXKEY    DON'T ASK 'ENTER KEY'                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS R1 TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BNE   ITER10           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CHI   R1,DMHED                                                         
         BNE   EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
ITER10   CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE TABLE                                
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH            ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITERH               ITERATE TABLE                                
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO                                                       
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING CPYRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         TABLE OF KNOWN INVOKERS                      
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
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
KFKVAL   MVC   CPYKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVC   CPYKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   CPYKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVI   CPYKCPY,X'41'                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
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
         USING CPYRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
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
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD OR COPY                          *         
***********************************************************************         
         SPACE 1                                                                
RFADD    CLI   CSACT,A#ADD                                                      
         BE    RFADD10                                                          
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CPYELQ',CPYRECD),0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                NO CPYEL                                     
         L     RF,12(,R1)                                                       
         USING CPYELD,RF                                                        
         CLI   CPYLN,CPYLN3Q                                                    
         BL    RFADD10             OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTAT9,FF-CPYSSRNM MUST SET TO NO                              
         DROP  RF                                                               
RFADD10  B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
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
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING CPYRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS+8        GET GLOBAL VERB                              
         LA    RF,DTATABL                                                       
         B     ITER                                                             
*                                                                               
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA04   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING CPYRECD,R2          R2 HOLDS A(RECORD)                           
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
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(CPY#CPYCD),AL4(CPY)    COMPANY CODE                          
         DC    AL2(CPY#CPYNM),AL4(CPN)    COMPANY NAME                          
         DC    AL2(CPY#BNKAC),AL4(LBA)    BANK ACCOUNTS                         
         DC    AL2(CPY#PTCSH),AL4(PUL)    PETTY CASH                            
         DC    AL2(CPY#PROD),AL4(PRDUL)   PRODUCTION                            
         DC    AL2(CPY#RECV),AL4(RCVUL)   RECEIVABLES                           
         DC    AL2(CPY#SUPP),AL4(SUPP)    SUPPLIER                              
         DC    AL2(CPY#XSUPP),AL4(XSUPP)  EXTRA SUPPLIER                        
         DC    AL2(CPY#SUPEX),AL4(SUPPX)  SUPPLIER EXPENSE                      
         DC    AL2(CPY#TAXLD),AL4(TAXLD)  TAX LEDGER                            
         DC    AL2(CPY#ABRV),AL4(ABBR)    ABBREVIATION                          
         DC    AL2(CPY#AGALF),AL4(AGY)    AGENCY ALPHA                          
         DC    AL2(CPY#SFY),AL4(SFY)      ST.OF FIN. YEAR                       
         DC    AL2(CPY#DPTL),AL4(DPTL)    LENGTH OF DEPT CODES                  
         DC    AL2(CPY#PRSTL),AL4(PRLDG)  PRESTO LEDGERS                        
*                                                                               
         DC    AL2(CPY#REPC),AL4(REPC)    REP CODE FOR COMM. ONLY               
         DC    AL2(CPY#PRID),AL4(PRID)    PRINCIPAL ID NUMBER                   
         DC    AL2(CPY#TCMP),AL4(TCMP)    HEX CODE FOR CONVERSION USE           
         DC    AL2(CPY#DISC),AL4(CDIN)    C/D ON EXPNSES TO INCOME AC           
         DC    AL2(CPY#CKDP),AL4(CHKD)    CHCK INV NO/DT/AMT FR DUPES           
         DC    AL2(CPY#VENR),AL4(DVEN)    DEMAND VENDOR FOR BT3/22              
         DC    AL2(CPY#CACA),AL4(CACC)    CNTR AC ON JB BT3 IS CSH AC           
         DC    AL2(CPY#WO14T),AL4(WO14)   DEMAND W/O ACC ON TYPE 14             
         DC    AL2(CPY#NEWGL),AL4(GLSMO)  GL SYSTEM START MONTH                 
         DC    AL2(CPY#IEUP),AL4(GLIUP)   IMMEDIATE UPDATE SETTING              
         DC    AL2(CPY#REQNPF),AL4(RQPF)  REQUISITION # PREFIX                  
         DC    AL2(CPY#REQNSF),AL4(RQSF)  REQUISITION # SUFFIX                  
         DC    AL2(CPY#REQNM),AL4(RQNM)   REQUISITION NAME                      
         DC    AL2(CPY#DCLNO),AL4(DCLN)   DRAFT CLAIM NUMBER                    
         DC    AL2(CPY#LCLNO),AL4(LCLN)   LIVE CLAIM NUMBER                     
         DC    AL2(CPY#STFPS),AL4(STFP)   STAFF PAYABLES ACCOUNTS               
         DC    AL2(CPY#EXTY),AL4(EXTYP)   EXPENDITURE TYPE                      
         DC    AL2(CPY#ESTNO),AL4(ESTN)   ESTIMATE NUMBER                       
         DC    AL2(CPY#IBAC),AL4(IBAC)    INTEROFFICE BALANCING ACCOUNT         
         DC    AL2(CPY#BAPID),AL4(BAPID)  BULK API PID                          
         DC    AL2(CPY#ORDHT),AL4(HTOR)   HTML TEXT FOR ORDERS                  
*****STATUS BYTE 3                                                              
*****STATUS BYTE 4                                                              
         DC    AL2(CPY#PESK),AL4(PPES)    POST % ESTIMATES TO SK                
         DC    AL2(CPY#OFF2),AL4(NOFF)    NEW OFFICE SYSTEM IN USE              
         DC    AL2(CPY#NMIR),AL4(NMIR)    NEW MI RECORDS IN USE                 
*****STATUS BYTE 5                                                              
         DC    AL2(CPY#OFPL),AL4(PLOF)    P&L OFFICES                           
         DC    AL2(CPY#BAPR),AL4(BAPP)    REGULAR BATCH APPROVAL                
         DC    AL2(CPY#NCST),AL4(NCST)    NEW COST EXPENSES IN USE              
         DC    AL2(CPY#PREQ),AL4(PREQ)    PROD REQ FOR EXP ANALYSIS             
         DC    AL2(CPY#VCOP),AL4(VCOP)    COPY PROD VNDR TO EXP VNDR            
         DC    AL2(CPY#EXPO),AL4(AEXPO)   ANALYZE EXP ORDERS AT...              
*****STATUS BYTE 6                                                              
         DC    AL2(CPY#RAPP),AL4(RAPP)    GEN RECORD ACTIVITY PNTRS             
*****STATUS BYTE 7                                                              
*****STATUS BYTE 8                                                              
         DC    AL2(CPY#RLOG),AL4(RLOGO)   PRINT LOGOS ON REPORT TURN.           
*****STATUS BYTE 9                                                              
         DC    AL2(CPY#SEDHO),AL4(EDH)    EDIT HRS DOWNLOAD FOR TEMPO           
         DC    AL2(CPY#LACCP),AL4(PLACC)  LIMITED ACCESS FOR PROD/LIST          
         DC    AL2(CPY#BSOD),AL4(MBSOD)   PST MEDIA BILLNG START OF DAY         
         DC    AL2(F#CPY#UIDTR),AL4(UID)  SET UNIQUE ID ON TRANSACTIONS         
*****STATUS BYTE 10                                                             
         DC    AL2(CPY#OFOLP),AL4(OFO)    OFFICE/OLIST USED IN PRESTO           
         DC    AL2(CPY#TMSFY),AL4(TMSFY)  ALLOW TIME OUTSIDE FISCAL YR          
         DC    AL2(CPY#AUT2C),AL4(AUT2C)  AUTO SETUP 2C ACCT FOR 1099           
         DC    AL2(CPY#FLEXIB),AL4(FLEXB) USING NEW 21 BILLING(FLEXI)           
         DC    AL2(CPY#VBANK),AL4(VBANK)  VENDOR BANKING INFO REQ'D             
         DC    AL2(CPY#TY10XJ),AL4(T10XJ) TY 10 USERS POST XJOB-DR OF           
*****STATUS BYTE 11                                                             
         DC    AL2(CPY#AJLA),AL4(AJLA)     AUTO JOB LEVEL APPROVER              
         DC    AL2(CPY#AJNMJ),AL4(AJNM)    AUTO JOB NUMBERING MCS               
         DC    AL2(CPY#JOPSDT),AL4(JBOSP)  JOB OPENED START PY DT               
         DC    AL2(CPY#CLOB),AL4(CLOB)     CLAIM NUMBERS OFFICE BASED           
         DC    AL2(CPY#ANBE),AL4(ANBE)     APPROVAL NOTICE BY EMAIL             
         DC    AL2(CPY#XC1N),AL4(XC1N)     EXCLUDE CLIENT AND 1N ACCTS          
         DC    AL2(F#CPY#BOMT),AL4(BMT)    BR'OCEAN MOBILE TIME                 
*****STATUS BYTE 12                                                             
         DC    AL2(CPY#COBOT),AL4(COBT)    COMPANY ON BRANDOCEAN TIME.          
         DC    AL2(CPY#RESOF),AL4(RESOF)   CPYSROFF  OFF FOR RESOURCES          
         DC    AL2(CPY#EMOVR),AL4(EMOVR)   CPYSRMEM  EMAIL OVRDUE REM           
         DC    AL2(CPY#TLISTJ),AL4(TEAJ)   TEAM LIST IN JOBS                    
         DC    AL2(CPY#RQBFE),AL4(RBFE)    REQ BALANCED FILE FOR EXPORT         
*****STATUS BYTE 13                                        *SGAV                
         DC    AL2(F#CPY#PAY2J),AL4(PAY2J) USE PAY2JOB     *SGAV                
         DC    AL2(F#CPY#GPDEX),AL4(GPDX)  GREENPLUM DATA EXTRACT               
*****STATUS EXTRA BYTE 1                                                        
         DC    AL2(CPY#INAPP),AL4(INAPP)   INTERNAL APPROVAL                    
         DC    AL2(CPY#JOEM),AL4(JOEM)     RECEIVE JOB EMAIL ALERTS             
         DC    AL2(CPY#OFFMI),AL4(OFMI)    USES OFFICE MI RECORDS               
*****STATUS EXTRA BYTE 2                                                        
         DC    AL2(CPY#INUSE),AL4(INUSD)   INTERNAL USE ONLY                    
         DC    AL2(CPY#SLFAPP),AL4(SFAPE)  SELF APPROVAL IN ESTIMATES           
*****STATUS EXTRA BYTE3                                                         
         DC    AL2(CPY#AJOB),AL4(AJOB)     ACCESS JOBS                          
         DC    AL2(CPY#AMED),AL4(AMED)     ACCESS MEDIA                         
         DC    AL2(CPY#AETYP),AL4(AETYP)   ACCESS EXPENDITURE TYPES             
         DC    AL2(CPY#A1NAC),AL4(A1NAC)   ACCESS 1N ACCOUNTS                   
         DC    AL2(CPY#ASTAF),AL4(ASTAF)   ACCESS 1R ACCOUNTS                   
         DC    AL2(CPY#AWC),AL4(AWC)       ACCESS WORKCODE                      
         DC    AL2(CPY#ASCHM),AL4(ASCHM)   ACCESS SCHEME CODE                   
*****STATUS EXTRA BYTE4                                                         
         DC    AL2(CPY#ASUPP),AL4(ASUPP)   ACCESS SUPPLIER CODE                 
         DC    AL2(CPY#ETIM),AL4(AETIM)    SEND NO TIME EMAILS                  
         DC    AL2(CPY#EORD),AL4(AEORD)    SEND NO ORDER EMAILS                 
         DC    AL2(CPY#EEST),AL4(AEEST)    SEND NO ESTIMATE EMAILS              
         DC    AL2(CPY#EEXP),AL4(AEEXP)    SEND NO EXPENSE EMAILS               
         DC    AL2(CPY#EJOB),AL4(AEJOB)    SEND NO JOB EMAILS                   
         DC    AL2(CPY#EINV),AL4(AEINV)    SEND NO INVOICE EMAILS               
         DC    AL2(CPY#DRFDT),AL4(DRDT)    DISTANCE REFRESH DATE                
*****STATUS EXTRA BYTE8                                                         
         DC    AL2(CPY#RSRM),AL4(RSRM)     RESOURCES IN USE                     
         DC    AL2(F#CPY#AURAE),AL4(AURAE) AURA EMAILS IN USE                   
*****STATUS EXTRA BYTE 9                                                        
         DC    AL2(F#CPY#AURTI),AL4(AURAE) AURA TIME EMAILS                     
         DC    AL2(F#CPY#AURJB),AL4(AUJB)  AURA JOB EMAILS                      
         DC    AL2(F#CPY#AURES),AL4(AUES)  AURA ESTIMATE EMAILS                 
         DC    AL2(F#CPY#AUROR),AL4(AUOR)  AURA ORDER EMAILS                    
         DC    AL2(F#CPY#AUREX),AL4(AUEX)  AURA EXPENSE EMAILS                  
         DC    AL2(F#CPY#AURIV),AL4(AUIV)  AURA INVOICE EMAILS                  
*****STATUS EXTRA BYTE A                                                        
         DC    AL2(F#CPY#FEDAT),AL4(FEDA)  FEDERATED SECURITY                   
*MN DSRD-24361                                                                  
         DC    AL2(F#CPY#ESTRT),AL4(ESRT)  ESTIMATE RICH TEXT SUPPORT           
*MN DSRD-24361                                                                  
*                                                                               
         DC    AL2(EOT)                                                         
*                                                                               
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
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
FIL1B    CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* SUBROUTINE FOR DISPLAYING FILTER PARAMETERS -YES OR NO              *         
***********************************************************************         
         SPACE 1                                                                
DISPFLT  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FLTIFLD,1                                                        
         BNE   *+14                                                             
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         CLI   FLTIFLD,2                                                        
         BNE   *+10                                                             
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SUBROUTINE FOR VALIDATING FILTER PARAMETERS                         *         
***********************************************************************         
         SPACE 1                                                                
VALFLTR  CLC   FVIFLD(1),AC@YES                                                 
         BNE   *+12                                                             
         MVI   FLTIFLD,1                                                        
         B     EXITOK                                                           
         CLC   FVIFLD(1),AC@NO                                                  
         BNE   *+12                                                             
         MVI   FLTIFLD,2                                                        
         B     EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  LM    R1,R2,SVPARMS3                                                   
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   GOTO1 AGETEL,BOPARM,('CPYELQ',CPYRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NO LEDGER ELEMENT                            
         XC    SVCPYEL,SVCPYEL                                                  
         MVC   SVCPYEL,BOELEM      SAVE COMPANY ELEMENT                         
         XC    SVCPXEL,SVCPXEL                                                  
         GOTO1 AGETEL,BOPARM,('CPXELQ',CPYRECD),0                               
         BNE   EXITOK                                                           
         MVC   SVCPXEL,BOELEM      SAVE COMPANY EXTRA ELEMENT                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   XC    SVCPYEL,SVCPYEL                                                  
         GOTO1 AGETEL,BOPARM,('CPYELQ',CPYRECD),0                               
         BE    DFDV02              COMPANY ELEMENT FOUND                        
T        USING CPYELD,BOELEM                                                    
         MVI   T.CPYEL,CPYELQ      IF NO CPYEL, BUILD A NEW ONE                 
         MVI   T.CPYLN,CPYLN4Q                                                  
         MVI   T.CPYGLU,C'G'                                                    
         MVI   T.CPYPANL,C'P'                                                   
         MVI   T.CPYANAL,C'2'                                                   
         OI    T.CPYSTATA,CPYSACCT   FORCE NEW COMPANY ON ACCENT                
         OI    T.CPYSTAT1,CPYSOROE   FORCE NEW COMPANY ON OFFICES               
         OI    T.CPYSTAT4,CPYSOFF2   FORCE NEW COMPANY ON 2 CHAR OFF            
         OI    T.CPYSTAT7,CPYSTMSY   FORCE NEW COMPANY ON TMS                   
         OI    T.CPYSTAT4,CPYSNPRD   FORCE NEW COMPANY ON NEW PRODN             
         MVI   T.CPYSTATB,0                                                     
         MVI   T.CPYSTATC,0                                                     
         MVC   T.CPYPRSTL,BCSPACES                                              
         B     DFDV04                                                           
DFDV02   OI    T.CPYSTAT7,CPYSTMSY   FOR ALL EXISTING COMPANIES ON TMS          
         CLI   T.CPYLN,CPYLN4Q                                                  
         BE    DFDV06                                                           
         MVI   T.CPYLN,CPYLN4Q     MAKE ALL ELEMENTS NEW LENGTH                 
         MVI   T.CPYSTATB,0                                                     
         MVI   T.CPYSTATC,0                                                     
         MVC   T.CPYPRSTL,BCSPACES                                              
DFDV04   GOTO1 AREPEL,BOPARM,('CPYELQ',CPYRECD),0,BOELEM                        
DFDV06   MVC   SVCPYEL,BOELEM                                                   
         GOTO1 AGETEL,BOPARM,('CPXELQ',CPYRECD),0                               
         BE    DFDV08                                                           
T        USING CPXELD,BOELEM                                                    
         XC    BOELEM,BOELEM                                                    
         MVI   T.CPXEL,CPXELQ                                                   
         MVI   T.CPXLN,CPXLNQ                                                   
         GOTO1 AADDEL,BOPARM,CPYRECD                                            
DFDV08   MVC   SVCPXEL,BOELEM                                                   
         B     EXITOK                                                           
         DROP  T                                                                
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
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   DS    0H                                                               
*                                                                               
X        USING CPXELD,SVCPXEL                                                   
         TM    X.CPXSTATA,CPXFEDAT                                              
         BZ    DLDVALX                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FFTELQ',CPYRECD),       +        
               (L'FFTTYPE,=AL1(FFTTFURL))                                       
         CLI   12(R1),0                                                         
         BE    DLDVALX                                                          
         L     RE,AFEDFLD                                                       
         ST    RE,FVADDR                                                        
         MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL                                                            
*                                                                               
DLDVALX  GOTO1 AREPEL,BOPARM,('CPYELQ',CPYRECD),0,SVCPYEL                       
         GOTO1 AREPEL,BOPARM,('CPXELQ',CPYRECD),0,SVCPXEL                       
         B     EXITOK                                                           
         DROP  X                                                                
         SPACE 2                                                                
FIL1BN   CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR COMPANY CODE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CPY      NTRDO                                                                  
*                                                                               
*        DC    AL1(DMHED),AL1(0,0,0),AL4(DMHED1)                                
CPYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPY)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCPY)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETCPY)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCPY)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCPY)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFCPY)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTCPY)                               
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DDFTCPY)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETCPY  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN CPYKCPY FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISCPY   DS    0H                                                               
         CLI   CPYKCPY,C' '                                                     
         BNH   EXITOK                                                           
         GOTO1 VHEXOUT,BOPARM,CPYKCPY,FVIFLD,L'CPYKCPY,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN CPYKCPY FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALCPY   DS    0H                                                               
         CLI   CSACT,A#LST            MAKE LIST ACTION INVALID FOR NOW          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     EXITL                                                            
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VHEXIN,BOPARM,FVIFLD,CPYKCPY,(RF)                                
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVCP) INVALID COMPANY                           
         B     EXITL                                                            
         CLI   CSACT,A#CHA         TEST CHANGING A RECORD                       
         BE    VALCPYX                                                          
         CLI   CPYKCPY,X'41'       PREVENT ANY SILLY VALUES                     
         BL    EXITNV                                                           
         CLI   CPYKCPY,X'FE'       X'41' TO X'FE'                               
         BH    EXITNV                                                           
*                                                                               
VALCPYX  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR COMPANY                                             *         
***********************************************************************         
         SPACE 1                                                                
DDFTCPY  GOTO1 VHEXOUT,BOPARM,CUABIN,FVIFLD,L'CUABIN,0                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN CPYKCPY FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTCPY  CLI   FLTIFLD,C' '                                                     
         BNH   EXITOK                                                           
         GOTO1 VHEXOUT,BOPARM,FLTIFLD,FVIFLD,L'CPYKCPY,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN CPYKCPY FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTCPY  CLI   CSACT,A#LST              LIST SCREEN NOT READY YET!              
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     EXITL                                                            
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VHEXIN,BOPARM,FVIFLD,FLTIFLD,(RF)                                
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCPY) INVALID COMPANY                           
         B     EXITL                                                            
*                                                                               
         MVC   CPYKCPY,FLTIFLD                                                  
         CLI   CPYKCPY,X'41'       PREVENT ANY SILLY VALUES                     
         BL    EXITNV                                                           
         CLI   CPYKCPY,X'FE'       X'41' TO X'FE'                               
         BH    EXITNV                                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON COMPANY CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFCPY   CLC   CPYKCPY,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A COMPANY NAME FIELD                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CPN      NTRDO                                                                  
*                                                                               
CPNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A COMPANY NAME FIELD FROM THE KEY                           *         
***********************************************************************         
         SPACE 1                                                                
DISCPN   LR    R1,R2                                                            
         GOTO1 AGETNAM                                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COMPANY NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
VALCPN   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('NAMELQ',CPYRECD),0               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         XC    BOELEM,BOELEM                                                    
         USING NAMELD,BOELEM                                                    
         MVI   NAMEL,NAMELQ        MOVE IN ELEMENT CODE                         
         XR    RF,RF               CLEAR RF                                     
         IC    RF,FVILEN           PUT LENGTH OF DATA INTO RF                   
         LA    RF,NAMLN1Q(RF)      INCREASE (RF) BY FRONT OF ELEMENT            
         STC   RF,NAMLN            STORING (RF) INTO ELEMENT LENGTH             
*                                                                               
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   NAMEREC(0),FVIFLD                                                
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),CPYRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING THE START OF FINANCIAL YEAR              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SFY      NTRDO                                                                  
*                                                                               
SFYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSFY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSFY)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSFY)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSFY)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSFY)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE START OF FINFANCIAL YEAR                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R3                                                        
DISSFY   LA    R3,SVCPYEL                                                       
         CLI   CPYSFST,0                                                        
         BE    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(7,0),(X'FF',0)                                   
         L     R1,4(R1)                                                         
         L     RE,=A(MONLST)                                                    
         A     RE,BORELO                                                        
         LA    RF,L'MONLST                                                      
DISF02   CLC   CPYSFST,0(RE)                                                    
         BE    DISF04                                                           
         LA    RE,1(RE)                                                         
         LA    R1,3(R1)                                                         
         BCT   RF,DISF02                                                        
         B     *+10                                                             
DISF04   MVC   FVIFLD(3),0(R1)                                                  
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE START OF FINANCIAL YEAR                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R3                                                        
VALSFY   LA    R3,SVCPYEL                                                       
         MVI   CPYSFST,0                                                        
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(7,0),(X'FF',0)                                   
         L     R1,4(R1)                                                         
         L     RE,=A(MONLST)                                                    
         A     RE,BORELO                                                        
         LA    RF,L'MONLST                                                      
         XR    R4,R4                                                            
VALF04   IC    R4,FVXLEN                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(R1)                                                  
         BNE   *+14                                                             
         MVC   CPYSFST,0(RE)                                                    
         B     EXITOK                                                           
         LA    RE,1(RE)                                                         
         LA    R1,3(R1)                                                         
         BCT   RF,VALF04                                                        
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITL                                                            
         DROP  R3                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY START OF FINANCIAL YEAR    FILTER                           *         
***********************************************************************         
         SPACE 1                                                                
DFLTSFY  DS    0H                                                               
         CLI   FLTIFLD,0                                                        
         BE    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(7,0),(X'FF',0) GET A LIST OF THE                 
         L     R1,4(R1)                        MONTHS - JAN TO DEC              
         L     RE,=A(MONLST)                                                    
         A     RE,BORELO                                                        
         LA    RF,L'MONLST                                                      
DFLTSFY2 CLC   FLTIFLD(1),0(RE)                                                 
         BNE   *+14                                                             
         MVC   FVIFLD(3),0(R1)                                                  
         B     EXITOK                                                           
         LA    RE,1(RE)                                                         
         LA    R1,3(R1)                                                         
         BCT   RF,DFLTSFY2                                                      
         DC    H'0'                              DIE IF MONTH NOT FOUND         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE START OF FINANCIAL YEAR    FILTER FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTSFY  CLI   FVIFLD,0                                                         
         BE    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(7,0),(X'FF',0)                                   
         L     R1,4(R1)                                                         
         L     RE,=A(MONLST)           POINT TO LIST OF 1-C                     
         A     RE,BORELO                                                        
         LA    RF,L'MONLST                                                      
         XR    R4,R4                                                            
VFLF04   IC    R4,FVXLEN                                                        
         EX    R4,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),0(R1)                                                  
         BNE   VFLF06                                                           
         MVC   FLTIFLD(L'CPYSFST),0(RE)                                         
         MVC   FVIFLD(3),0(R1)                                                  
         B     EXITOK                                                           
VFLF06   LA    RE,1(RE)                                                         
         LA    R1,3(R1)                                                         
         BCT   RF,VFLF04                                                        
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON START OF FINANCIAL YEAR                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTSFY  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLC   CPYSFST,FLTIFLD                                                  
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR THE BANK ACCOUNT UL                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LBA      NTRDO                                                                  
*                                                                               
LBATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBNK)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBNK)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLBNK)                                
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLBNK)                                
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFBNK)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE BANK ACCOUNT UNIT LEDGER FOR A COMPANY RECORD           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISBNK   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYBANK),CPYBANK                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BANK ACCOUNT UNIT LEDGER                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALBNK   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   CPYBANK(2),FVIFLD                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY BANK ACCOUNT UNIT LEDGER FILTER FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
DFLBNK   DS    0H                                                               
         MVC   FVIFLD(L'CPYBANK),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON BANK ACCOUNT UNIT LEDGER FILTER                         *         
***********************************************************************         
         SPACE 1                                                                
VFLBNK   MVC   FLTIFLD(L'CPYBANK),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON BANK ACCOUNT UNIT LEDGER                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFBNK   GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLC   CPYBANK,FLTIFLD                                                  
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR DISPLAYING THE PETTY CASH UNIT LEDGER               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PUL      NTRDO                                                                  
*                                                                               
PULTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DSPETTY)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VLPETTY)                                
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFPETTY)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFPETTY)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFPETTY)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A UNIT LEDGER FOR PETTY CASH                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DSPETTY  LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYPETY),CPYPETY                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PETTY CASH   UNIT LEDGER                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VLPETTY  LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   CPYPETY(2),FVIFLD                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY PETTY CASH   UNIT LEDGER FILTER FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
DFPETTY  DS    0H                                                               
         MVC   FVIFLD(L'CPYPETY),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON PETTY CASH   UNIT LEDGER FILTER                         *         
***********************************************************************         
         SPACE 1                                                                
VFPETTY  MVC   FLTIFLD(L'CPYPETY),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON PETTY CASH   UNIT LEDGER                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFPETTY GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLC   CPYPETY,FLTIFLD                                                  
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR THE PRODUCTION UL                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PRDUL    NTRDO                                                                  
*                                                                               
PRDULTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPUL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPUL)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLPUL)                                
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLPUL)                                
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFPUL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE PRODUCTION UNIT LEDGER                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISPUL   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYPROD),CPYPROD                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PRODUCTION UL                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALPUL   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   CPYPROD(2),FVIFLD                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY PRODUCTION UL FILTER                                        *         
***********************************************************************         
         SPACE 1                                                                
DFLPUL   DS    0H                                                               
         MVC   FVIFLD(L'CPYPROD),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE PRODUCTION UL FILTER                                   *         
***********************************************************************         
         SPACE 1                                                                
VFLPUL   MVC   FLTIFLD(L'CPYPROD),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON PRODUCTION UL                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFPUL   GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLC   CPYPROD,FLTIFLD                                                  
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR THE RECEIVABLES UL                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RCVUL    NTRDO                                                                  
*                                                                               
RCVULTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRCV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRCV)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTRCV)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTRCV)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTRCV)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE RECEIVABLES UNIT LEDGER                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISRCV   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYRECV),CPYRECV                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECEIVABLES UNIT LEDGER                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALRCV   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   CPYRECV(2),FVIFLD                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY RECEIVABLES UL FILTER                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTRCV  DS    0H                                                               
         MVC   FVIFLD(L'CPYRECV),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON RECEIVABLES UL FILTER                                   *         
***********************************************************************         
         SPACE 1                                                                
VFLTRCV  MVC   FLTIFLD(L'CPYRECV),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON RECEIVALBES UL FILTER                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTRCV  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLC   CPYRECV,FLTIFLD                                                  
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR THE SUPPLIER UL                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SUPP     NTRDO                                                                  
*                                                                               
SUPPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISUPP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSUPP)                                
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLSUPP)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLSUPP)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFSUPP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE SUPPLIER UL                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISUPP   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYSUPP),CPYSUPP                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE SUPPLIER UL                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALSUPP  LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   CPYSUPP(2),FVIFLD                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY THE SUPPLIER UL FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLSUPP  DS    0H                                                               
         MVC   FVIFLD(L'CPYSUPP),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE SUPPLIER UL FILTER                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLSUPP  MVC   FLTIFLD(L'CPYSUPP),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON SUPPLIER UL                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFSUPP  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLC   CPYSUPP,FLTIFLD                                                  
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR THE EXTRA SUPPLIER LEDGER                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
XSUPP    NTRDO                                                                  
*                                                                               
XSUPPTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISXSUP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXSUP)                                
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLXSUP)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLXSUP)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFXSUP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE EXTRA SUPPLIER LEDGER                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISXSUP  LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYXSUPP),CPYXSUPP                                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE EXTRA SUPPLIER LEDGER                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALXSUP  LA    R1,SVCPYEL                                                       
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   CPYXSUPP(1),FVIFLD                                               
         MVC   SVXSUPP,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY EXTRA SUPPLIER LEDGER FILTER                                *         
***********************************************************************         
         SPACE 1                                                                
DFLXSUP  DS    0H                                                               
         MVC   FVIFLD(L'CPYXSUPP),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE EXTRA SUPPLIER LEDGER FILTER                           *         
***********************************************************************         
         SPACE 1                                                                
VFLXSUP  MVC   FLTIFLD(L'CPYXSUPP),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON THE EXTRA SUPPLIER LEDGER                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2                                                       
DOFXSUP  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLC   CPYXSUPP,FLTIFLD                                                 
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         DROP  R1,R2                                                            
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR THE SUPPLIER EXPENSE LEDGER                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SUPPX    NTRDO                                                                  
*                                                                               
SUPPXTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUPX)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSUPX)                                
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLSUPX)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLSUPX)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFSUPX)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE SUPPLIER EXPENSE LEDGER                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISSUPX  LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYSUPX),CPYSUPX                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUPPLIER EXPENSE LEDGER                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALSUPX  LA    R1,SVCPYEL                                                       
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   CPYSUPX(2),FVIFLD                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY SUPPLER EXPENSE LEDGER FILTER                               *         
***********************************************************************         
         SPACE 1                                                                
DFLSUPX  DS    0H                                                               
         MVC   FVIFLD(L'CPYSUPX),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE SUPPLIER EXPENSE LEDGER FILTER                         *         
***********************************************************************         
         SPACE 1                                                                
VFLSUPX  MVC   FLTIFLD(L'CPYSUPX),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON SUPPLIER EXPENSE LEDGER                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFSUPX  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLC   CPYSUPX,FLTIFLD                                                  
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR THE TAX LEDGER                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TAXLD    NTRDO                                                                  
*                                                                               
TAXLDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISTAX)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTAX)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTAX)                                
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTAX)                                
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTAX)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE TAX LEDGER                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISTAX   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYTAX),CPYTAX                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE TAX LEDGER                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALTAX   LA    R1,SVCPYEL                                                       
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   CPYTAX,FVIFLD                                                    
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY THE TAX LEDGER FILTER                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTAX   DS    0H                                                               
         MVC   FVIFLD(L'CPYTAX),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE TAX LEDGER FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTAX   MVC   FLTIFLD(L'CPYTAX),FVIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON TAX LEDGER                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTAX   GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLC   CPYTAX,FLTIFLD                                                   
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         DROP  R1,R2                                                            
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR DISPLAYING THE  AGENCY ALPHA  CODE                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AGY      NTRDO                                                                  
*                                                                               
AGYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(AGYCPN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAGY)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE AGENCY ALPHA CODE                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
AGYCPN   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYALPHA),CPYALPHA                                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AGENCY ALPHA CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALAGY   LA    R1,SVCPYEL                                                       
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CPYALPHA(0),FVIFLD                                               
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING THE  ABBREVIATION                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ABBR     NTRDO                                                                  
*                                                                               
ABBRTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(ABBCPN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALABB)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE ABBREVIATION                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
ABBCPN   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'CPYLOGO),CPYLOGO                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ABBREVIATION                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALABB   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   CPYLOGO(2),FVIFLD                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR C/D ON EXPENSES TO INCOME ACCONT                    *         
* =FILE LABEL:  CD                                                              
* =AFM  LABEL:  POST EXP INV LESS CD OR TO SI                                   
***********************************************************************         
         SPACE 1                                                                
CDIN     NTRDO                                                                  
*                                                                               
CDINTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDISC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDISC)                                
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTDIS)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTDIS)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTDIS)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YES OR NO FOR CASH DISCOUNT                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISDISC  LA    R1,SVCPYEL                                                       
*                                                                               
         TM    CPYSTAT1,CPYSDISC                                                
         BZ    *+14                                                             
         MVC   FVIFLD(2),=C'SI'                                                 
         B     EXITOK                                                           
         MVC   FVIFLD(L'AC@LESS),AC@LESS                                        
         B     EXITOK                                                           
         DROP  R1                                                               
***********************************************************************         
* VALIDATE THE CASH DISCOUNT FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALDISC  LA    R1,SVCPYEL                                                       
*                                                                               
         NI    CPYSTAT1,X'FF'-CPYSDISC                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@LESS                                                
         BE    EXITOK                                                           
         CLC   FVIFLD(2),=C'SI'                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT1,CPYSDISC                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY EXPENSE DISC TO INCOME     FILTER                           *         
***********************************************************************         
         SPACE 1                                                                
DFLTDIS  DS    0H                                                               
         B     DISPFLT                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON EXPENSE DISC TO INCOME  FILTER FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTDIS  B     VALFLTR                                                          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON EXPENSE DISC TO INCOME  FILTER FIELD                *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTDIS  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLI   FLTIFLD,1                                                        
         BNE   DOFTDIS2                                                         
         TM    CPYSTAT1,CPYSDISC                                                
         BO    FLTXE                                                            
         BZ    FLTXL                                                            
DOFTDIS2 TM    CPYSTAT1,CPYSDISC                                                
         BZ    FLTXE                                                            
         BO    FLTXL                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR CHECK INV. NO/DATE/AMT FOR DUPES                    *         
* =FILE LABEL:  CHKDUP                                                *         
* =AFM  LABEL:  CHECK FOR DUP INVOICE ON TY 1,21                      *         
***********************************************************************         
         SPACE 1                                                                
CHKD     NTRDO                                                                  
*                                                                               
CHKDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCKD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCKD)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCKD)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCKD)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCKD)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YES OR NO                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISCKD   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT2,CPYSCKDP                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON DUPLICATE INVOICE NUMBERS                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALCKD   LA    R1,SVCPYEL                                                       
*                                                                               
         NI    CPYSTAT2,X'FF'-CPYSCKDP                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT2,CPYSCKDP                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY ON DUPLICATE INVOICE NOS FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFLTCKD  DS    0H                                                               
         B     DISPFLT                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON DUPLICATE INVOICE NUMBERS FILTER FIELD                  *         
***********************************************************************         
         SPACE 1                                                                
VFLTCKD  B     VALFLTR                                                          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON DUPLICATE INVOICE NUMBERS FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTCKD  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
         CLI   FLTIFLD,1                                                        
         BNE   DOFTCK2                                                          
         TM    CPYSTAT2,CPYSCKDP                                                
         BO    FLTXE                                                            
         BZ    FLTXL                                                            
DOFTCK2  TM    CPYSTAT2,CPYSCKDP                                                
         BZ    FLTXE                                                            
         BO    FLTXL                                                            
         DROP  R2                                                               
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR DEMAND VENDOR FOR BT3 AND BT22                      *         
* =FILE LABEL:  VEND                                                  *         
* =AFM  LABEL:  IF ADJUST TO CASH, VENDOR REQ'D                       *         
***********************************************************************         
         SPACE 1                                                                
DVEN     NTRDO                                                                  
*                                                                               
DVENTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISVEN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVEN)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTVEN)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTVEN)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTVEN)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YES OR NO                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISVEN   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT2,CPYSVENR                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON DEMAND ON VENDOR BT3 AND BT22                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALVEN   LA    R1,SVCPYEL                                                       
*                                                                               
         NI    CPYSTAT2,X'FF'-CPYSVENR                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT2,CPYSVENR                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY DEMAND VENDOR FOR BT3 &BT22 FILTER                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTVEN  DS    0H                                                               
         B     DISPFLT                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON DEMAND VENDOR FOR BT3 & BT22 FILTER FIELD               *         
***********************************************************************         
         SPACE 1                                                                
VFLTVEN  B     VALFLTR                                                          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON DEMAND VENDOR FOR BT3 & BT22 FIELD                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DOFTVEN  DS    0H                                                               
         LA    R1,SVCPYEL                                                       
*                                                                               
         CLI   FLTIFLD,1                                                        
         BNE   DOFTVEN2                                                         
         TM    CPYSTAT2,CPYSVENR                                                
         BO    FLTXE                                                            
         BZ    FLTXL                                                            
DOFTVEN2 TM    CPYSTAT2,CPYSVENR                                                
         BZ    FLTXE                                                            
         BO    FLTXL                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR CONTRA ACCOUNT ON JOB BT3 IS CASH AC                *         
* =FILE LABEL:  SJCNTRA=SC                                            *         
* =AFM  LABEL:  IF ADJUST TO CASH, C/A IS:                            *         
***********************************************************************         
         SPACE 1                                                                
CACC     NTRDO                                                                  
*                                                                               
CACCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCAC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCAC)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY VEND OR SC                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISCAC   LA    R1,SVCPYEL                                                       
*                                                                               
         TM    CPYSTAT2,CPYSCACA                                                
         BZ    *+14                                                             
         MVC   FVIFLD(2),=C'SC'                                                 
         B     EXITOK                                                           
         MVC   FVIFLD(L'AC@VEND),AC@VEND    'VEND' IS THE DEFAULT               
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON CONTRA ACCOUNT ON JOB BT3 IS CASH AC                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALCAC   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT2,FF-CPYSCACA                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS VEND                   
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@VEND   VEND                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(2),=C'SC'                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT2,CPYSCACA                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*                                                                               
***********************************************************************         
* DATA OBJECT FOR DEMAND W/O ACCOUNT TYPE 14                          *         
* =FILE LABEL:  WO                                                    *         
* =AFM  LABEL:  REQ CONTROL POSTINGS ON PROD W/O                      *         
***********************************************************************         
         SPACE 1                                                                
WO14     NTRDO                                                                  
*                                                                               
WO14TBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISWO1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWO1)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTWO1)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTWO1)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTWO1)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YES OR NO                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISWO1   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT3,CPYSWO14                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON DEMEND W/O ON ACCOUNT TYPE 14                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALWO1   LA    R1,SVCPYEL                                                       
*                                                                               
         NI    CPYSTAT3,X'FF'-CPYSWO14                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT3,CPYSWO14                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY DEMAND W/O ACCOUNT ON TYPE 14 FILTER                        *         
***********************************************************************         
         SPACE 1                                                                
DFLTWO1  DS    0H                                                               
         B     DISPFLT                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON DEMAND W/O ACCOUNT ON TYPE 14 FILTER                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTWO1  B     VALFLTR                                                          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON DEMAND W/O ACCOUNT ON TYPE 14                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTWO1  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLI   FLTIFLD,1                                                        
         BNE   DOFTWO12                                                         
         TM    CPYSTAT3,CPYSWO14                                                
         BO    FLTXE                                                            
         BZ    FLTXL                                                            
DOFTWO12 TM    CPYSTAT3,CPYSWO14                                                
         BZ    FLTXE                                                            
         BO    FLTXL                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* STATUS BYTE 4                                                       *         
* DATA OBJECT FOR POST % ESTIMATES TO SK                              *         
* =FILE LABEL:  %EST=SK                                               *         
* =AFM  LABEL:  POST % OF EST COMM TO SI/SK                           *         
***********************************************************************         
         SPACE 1                                                                
PPES     NTRDO                                                                  
*                                                                               
PPESTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPES)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPES)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPES)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPES)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPES)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YES OR NO                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISPES   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(2),=C'SI'                                                 
         TM    CPYSTAT4,CPYSPESK                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(2),=C'SK'                                                 
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON POST % ESTIMATES TO SK                                  *         
* VALID ENTRIES:  SK OR SI(DEFAULT)                                             
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALPES   LA    R1,SVCPYEL                                                       
*                                                                               
         NI    CPYSTAT4,X'FF'-CPYSPESK                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,=C'SI'                                                 
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,=C'SK'                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT4,CPYSPESK                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY POST % ESTIMATES TO SK        FILTER                        *         
***********************************************************************         
         SPACE 1                                                                
DFLTPES  DS    0H                                                               
         B     DISPFLT                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON POST % ESTIMATES TO SK        FILTER                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTPES  B     VALFLTR                                                          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON POST % ESTIMATES TO SK                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTPES  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLI   FLTIFLD,1                                                        
         BNE   DOFTPES2                                                         
         TM    CPYSTAT4,CPYSPESK                                                
         BO    FLTXE                                                            
         BZ    FLTXL                                                            
DOFTPES2 TM    CPYSTAT4,CPYSPESK                                                
         BZ    FLTXE                                                            
         BO    FLTXL                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* NEW OFFICE SYSTEM IN USE                                            *         
* =FILE LABEL:  NEWOFF                                                *         
* =AFM  LABEL:  OFFICE CODE LENGTH                                    *         
***********************************************************************         
         SPACE 1                                                                
NOFF     NTRDO                                                                  
*                                                                               
NOFFTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOF2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOF2)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOF2)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTOF2)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOF2)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY 1 FOR NOT ON NEW OFFICES OR 2 IF ON NEW OFFICES             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISOF2   LA    R1,SVCPYEL                                                       
*                                                                               
         MVI   FVIFLD,C'1'                                                      
         TM    CPYSTAT4,CPYSOFF2                                                
         BZ    EXITOK                                                           
         MVI   FVIFLD,C'2'                                                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON NEW OFFICE SYSTEM IN USE                                *         
* RULES:                                                              *         
* 1 MEANS NOT ON NEW OFFICES -- 2 MEANS ON NEW OFFICES.               *         
* CAN'T CHANGE THIS FIELD IF BATCH RECORDS EXIST.                     *         
* IF ON NEW OFFICES ALSO TURN ON BIT FOR OFFICES.                     *         
* IF ON NEW OFFICES MAKE SURE AT LEAST ONE OFFICE RECORD EXISTS.      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R3                                                        
VALOF2   LA    R3,SVCPYEL                                                       
*                                                                               
         MVC   BCHALF(1),CPYSTAT4  SAVE THE OLD STATUS                          
         NI    BCHALF,CPYSOFF2                                                  
         NI    CPYSTAT4,X'FF'-CPYSOFF2                                          
         CLI   FVILEN,0                                                         
         BE    VOF210                                                           
         CLI   FVIFLD,C'1'                                                      
         BE    VOF210                                                           
         CLI   FVIFLD,C'2'                                                      
         BNE   EXITNV                                                           
         OI    CPYSTAT4,CPYSOFF2    IF ON NEW OFFICES                           
         OI    CPYSTAT1,CPYSOROE    MUST ALSO BE ON OFFICES                     
*                                                                               
VOF210   CLI   CSACT,A#ADD         IF ADDING A NEW COMPANY RECORD               
         BNE   VOF220              THEY MUST BE ON NEW OFFICES                  
         TM    CPYSTAT4,CPYSOFF2                                                
         BO    VOF220                                                           
         B     EXITNV                                                           
*                                                                               
VOF220   MVC   BCHALF+1(1),CPYSTAT4  NOW COMPARE TO MAKE SURE IF THIS           
         NI    BCHALF+1,CPYSOFF2     FIELD HAS CHANGED FROM NO TO YES           
         CLC   BCHALF(1),BCHALF+1    OR YES TO NO                               
         BE    EXITOK                                                           
*                                                                               
         USING TBARECD,R4                                                       
         LA    R4,IOKEY                                                         
         XC    TBAKEY,TBAKEY                                                    
         MVI   TBAKTYP,TBAKTYPQ                                                 
         MVC   TBAKCPY,CPYKCPY                                                  
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEY(TBAKUSER-TBAKEY),IOKEYSAV                                  
         BNE   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CHANA) CANNOT CHANGE STATUS                      
         B     EXITL                                                            
*                                                                               
         DROP  R3,R4                                                            
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY NEW OFFICE SYSTEM IN USE      FILTER                        *         
***********************************************************************         
         SPACE 1                                                                
DFLTOF2  DS    0H                                                               
         B     DISPFLT                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON NEW OFFICE SYSTEM IN USE      FILTER                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTOF2  B     VALFLTR                                                          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON NEW OFFICE SYSTEM IN USE                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTOF2  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLI   FLTIFLD,1                                                        
         BNE   DOFTOF22                                                         
         TM    CPYSTAT4,CPYSOFF2                                                
         BO    FLTXE                                                            
         BZ    FLTXL                                                            
DOFTOF22 TM    CPYSTAT4,CPYSOFF2                                                
         BZ    FLTXE                                                            
         BO    FLTXL                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR NEW MEDIA INTERFACE RECORDS IN USE                  *         
* =FILE LABEL:  MI                                                    *         
* =AFM  LABEL:  USES MEDIA INTERFACE RECORDS                          *         
***********************************************************************         
         SPACE 1                                                                
NMIR     NTRDO                                                                  
*                                                                               
NMIRTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMIR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMIR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY NEW MEDIA INTERFACE RECS IN USE FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISMIR   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT4,CPYSMINT                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE NEW MEDIA INTERFACE RECS IN USE FIELD                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALMIR   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT4,FF-CPYSMINT                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT4,CPYSMINT                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NEW OFFICE MEDIA INTERFACE RECORDS IN USE           *         
* =FILE LABEL:  NONE                                                  *         
* =AFM  LABEL:  USES OFFICE MI RECORDS                                *         
***********************************************************************         
         SPACE 1                                                                
OFMI     NTRDO                                                                  
*                                                                               
OFMITBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOMI)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOMI)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY USES OFFICE MI RECORDS FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISOMI   LA    R1,SVCPXEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPXSTAT1,CPXOFFMI                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE USES OFFICE MI RECORDS FIELDS                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALOMI   LA    R1,SVCPXEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPXSTAT1,FF-CPXOFFMI                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         USING CPYELD,RF                                                        
         LA    RF,SVCPYEL                                                       
         TM    CPYSTAT4,CPYSMINT        USES MI RECORDS? (AGY LEVEL)            
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$OFMIA)   CAN'T USE OFFICE LVL WITHOUT            
         B     EXITL                    AGENCY LEVEL                            
         OI    CPXSTAT1,CPXOFFMI                                                
         B     EXITOK                                                           
         DROP  R1,RF                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRODUCT REQUIRED FOR EXP. ANALYSIS                  *         
* =FILE LABEL:  EXPROD                                                *         
* =AFM  LABEL:  PRODUCT REQ'D FOR EXP ANALYSIS                        *         
***********************************************************************         
         SPACE 1                                                                
PREQ     NTRDO                                                                  
*                                                                               
PREQTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPREQ)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPREQ)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PRODUCT REQUIRED FOR EXP. ANALYSIS FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISPREQ  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         CLI   CPYLN,CPYLN2Q       MAKE SURE 2ND ELEMENT LENGTH                 
         BL    EXITOK                                                           
         TM    CPYSTAT5,CPYSEXPP                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PRODUCT REQUIRED FOR EXP. ANALYSIS FIELD                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALPREQ  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT5,FF-CPYSEXPP                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT5,CPYSEXPP                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COPY PROD VENDOR TO EXP VEND ON TY61                *         
* =FILE LABEL:  VENCOP                                                *         
* =AFM  LABEL:  COPY PROD VENDOR TO EXP VENDOR                        *         
***********************************************************************         
         SPACE 1                                                                
VCOP     NTRDO                                                                  
*                                                                               
VCOPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISVCOP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVCOP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY COPY PROD VEND TO EXP VEND ON TY61 FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISVCOP  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         CLI   CPYLN,CPYLN2Q       MAKE SURE 2ND ELEMENT LENGTH                 
         BL    EXITOK                                                           
         TM    CPYSTAT5,CPYSVEND                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COPY PROD VEND TO EXP VEND ON TY61 FIELD                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALVCOP  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT5,FF-CPYSVEND                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT5,CPYSVEND                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR GENERATE RECORD ACTIVITY POINTERS                   *         
* =FILE LABEL:  RAPTR                                                 *         
* =AFM  LABEL:  USES PRESTO                                           *         
***********************************************************************         
         SPACE 1                                                                
RAPP     NTRDO                                                                  
*                                                                               
RAPPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRAPP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRAPP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY GENERATE RECORD ACTIVITY POINTERS FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISRAPP  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         CLI   CPYLN,CPYLN2Q       MAKE SURE 2ND ELEMENT LENGTH                 
         BL    EXITOK                                                           
         TM    CPYSTAT6,CPYSRAPP                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE GENERATE RECORD ACTIVITY POINTER FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALRAPP  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT6,FF-CPYSRAPP                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT6,CPYSRAPP                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRINT LOGOS ON REMOTE TURNAROUND                    *         
* =FILE LOGO:  RLOGO                                                  *         
* =AFM  LOGO:  PRINT LOGO ON PQ T/A REPORTS                           *         
***********************************************************************         
         SPACE 1                                                                
RLOGO    NTRDO                                                                  
*                                                                               
RLOGOTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRLOG)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRLOG)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PRINT LOGOS ON REMOTE TURNAROUND FIELD                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISRLOG  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         CLI   CPYLN,CPYLN2Q       MAKE SURE 2ND ELEMENT LENGTH                 
         BL    EXITOK                                                           
         TM    CPYSTAT8,CPYSRLOG                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE PRINT LOGOS ON REMOTE TURNAROUND FIELD                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALRLOG  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT8,FF-CPYSRLOG                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT8,CPYSRLOG                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EDIT HOURS DOWNLOAD FOR TEMPO                       *         
* =FILE LABEL:  MINHOURS                                              *         
* =AFM  LABEL:  MINIMUM HOURS REQUIRED (TEMPO)                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
EDH      NTRDO                                                                  
*                                                                               
EDHTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEDH)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEDH)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YES OR NO                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISEDH   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT9,CPYSEDHO                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE EDIT HRS DOWLOAD FOR TEMPO FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALEDH   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT9,FF-CPYSEDHO                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   RF,FVXLEN                                                        
         EXCLC RF,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC RF,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT9,CPYSEDHO                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* STATUS BYTE 9                                                       *         
* DATA OBJECT FOR UNIQUE ID ON TRANSACTION                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
UID      NTRDO                                                                  
*                                                                               
UIDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISUID)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YES OR NO                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISUID   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    CPYSTAT9,CPYSSRNM                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR COMPANY USES OFFICES/OFFICE LISTS IN PRESTO         *         
* =FILE LABEL:  PTXTRDATA=OF/OL/ALL                                   *         
* =AFM  LABEL:  PRESTO EXTRA DATA ACCESS                              *         
***********************************************************************         
         SPACE 1                                                                
OFO      NTRDO                                                                  
*                                                                               
OFOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFOLP)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFOLP)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY COMPANY USING OFFICE/OFFICE LIST IN PRESTO                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISOFOLP LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@ALL),AC@ALL                                          
         CLI   CPYLN,CPYLN3Q       MAKE SURE 3RD ELEMENT LENGTH                 
         BL    EXITOK                                                           
         TM    CPYSTATA,CPYSPOFU                                                
         BZ    *+14                                                             
         MVC   FVIFLD(L'AC@OFFC),AC@OFFC OF                                     
         B     EXITOK                                                           
         TM    CPYSTATA,CPYSPOLU                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@OFLST),AC@OFLST  OL                                  
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COMPANY USING OFFICE/OFFICE LIST IN PRESTO FIELD           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALOFOLP LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTATA,FF-(CPYSPOFU+CPYSPOLU)                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@ALL    ALL                                          
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@OFFC   OF                                           
         BNE   *+12                                                             
         OI    CPYSTATA,CPYSPOFU                                                
         B     EXITOK                                                           
         EXCLC R3,FVIFLD,AC@OFLST  OL                                           
         BNE   EXITNV                                                           
         OI    CPYSTATA,CPYSPOLU                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ALLOW TIME OUTSIDE FISCAL YEAR                      *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  ALLOW TIME OUTSIDE FISCAL YEAR                        *         
***********************************************************************         
         SPACE 1                                                                
TMSFY    NTRDO                                                                  
*                                                                               
TMSFYTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DTMSFY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VTMSFY)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY ALLOW TIME OUTSIDE OF FISCAL YEAR FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DTMSFY   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTATA,CPYTMSFY                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ALLOW TIME OUTSIDE OF FISCAL YEAR FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VTMSFY   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTATA,FF-CPYTMSFY                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATA,CPYTMSFY                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR AUTO SETUP 2C ACCOUNT FOR 1099                      *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  AUTO SETUP 2C ACCOUNT FOR 1099                        *         
***********************************************************************         
         SPACE 1                                                                
AUT2C    NTRDO                                                                  
*                                                                               
AUT2CTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DAUT2C)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAUT2C)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY AUTO SETUP 2C ACCOUNT FOR 1099 PROFILE                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DAUT2C   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTATA,CPYAUT2C                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AUTO SETUP 2C ACCOUNT FOR 1099 PROFILE                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VAUT2C   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTATA,FF-CPYAUT2C                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATA,CPYAUT2C                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPANY USING NEW 21 BILLING (FLEXI-BILL)           *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  USES SOON BILLING                                     *         
***********************************************************************         
         SPACE 1                                                                
FLEXB    NTRDO                                                                  
*                                                                               
FLEXBTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISFLXB)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFLXB)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY COMPANY USING NEW 21 BILLING PROFILE                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISFLXB  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTATA,CPYSADBL                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COMPANY USING NEW 21 BILLING PROFILE                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALFLXB  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTATA,FF-CPYSADBL                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATA,CPYSADBL                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR VENDOR BANKING INFO REQUIRED                        *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  VENDOR BANKING INFO REQUIRED                          *         
***********************************************************************         
         SPACE 1                                                                
VBANK    NTRDO                                                                  
*                                                                               
VBANKTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISVBNK)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVBNK)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY VENDOR BANKING INFO REQUIRED PROFILE                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISVBNK  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTATA,CPYVBINF                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE VENDOR BANKING INFO REQUIRED PROFILE                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALVBNK  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTATA,FF-CPYVBINF                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATA,CPYVBINF                                                
         B     EXITOK                                                           
         DROP  R1                                                               
***********************************************************************         
* DATA OBJECT FOR TY 10 USERS POST XJOB TO DR OFF                     *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  TY 10 USERS POST XJOB TO DR OFF                       *         
***********************************************************************         
         SPACE 1                                                                
T10XJ    NTRDO                                                                  
*                                                                               
T10XJTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISTY10)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTY10)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY TYPE 10 USERS POST XJOB TO DR OFFICE                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISTY10  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT7,CPYSXJOF                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TYPE 10 USERS POST XJOB TO DR OFFICE                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALTY10  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT7,FF-CPYSXJOF                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT7,CPYSXJOF                                                
         B     EXITOK                                                           
***********************************************************************         
* STATUS BYTE 11                                                      *         
* DATA OBJECT FOR AUTOMATIC JOB LEVEL APPROVEL                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AJLA     NTRDO                                                                  
*                                                                               
AJLATBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISAJLA)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAJLA)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AUTO JOB LEVEL APPROVER                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISAJLA  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    CPYSTATB,CPYSAJLA   AUTO JOB LEVEL APPROVEL                      
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AUTO JOB LEVEL APPROVER                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALAJLA  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATB,FF-CPYSAJLA MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATB,CPYSAJLA   USING AUTO JOB LEVEL APPROVER                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* STATUS BYTE 11                                                      *         
* DATA OBJECT FOR AUTO JOB NUMBERING/MCS                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AJNM     NTRDO                                                                  
*                                                                               
AJNMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISAJNM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAJNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AUTO JOB NUMBERING/MCS                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISAJNM  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@YES                                           
         TM    CPYSTATB,CPYSAJNM                                                
         BO    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AUTO JOB NUMBERING/MCS                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALAJNM  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATB,FF-CPYSAJNM MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATB,CPYSAJNM                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR JOB OPENED START PAYMENT DATE                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
JBOSP    NTRDO                                                                  
*                                                                               
JBOSPTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISJBOSP)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALJBOSP)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE JOB OPENED START PAYMENT DATE                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R4                                                        
DISJBOSP LA    R4,SVCPXEL          SAVE COMPANY ELEMENT                         
         OC    CPXJBOSP,CPXJBOSP                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BODMCB,(2,CPXJBOSP),(17,FVIFLD)                          
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE JOB OPENED START PAYMENT DATE                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R4                                                        
VALJBOSP LA    R4,SVCPXEL          SAVE COMPANY ELEMENT                         
         XC    CPXJBOSP,CPXJBOSP                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTO1 VDATVAL,BODMCB,(0,FVIFLD),BODUB1                                 
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         BZ    EXITL               INVAILD DATE                                 
         GOTO1 VDATCON,BODMCB,(0,BODUB1),(2,CPXJBOSP)                           
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* STATUS BYTE 11                                                      *         
* DATA OBJECT FOR CLAIM NUMBERS ON OFFICES                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CLOB     NTRDO                                                                  
*                                                                               
CLOBTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLOB)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCLOB)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 'CLAIM NUMBERS ON OFFICE'                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISCLOB  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@YES                                           
         TM    CPYSTATB,CPYSCNOB                                                
         BO    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 'CLAIM NUMBERS ON OFFICE'                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALCLOB  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATB,FF-CPYSCNOB MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATB,CPYSCNOB                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* STATUS BYTE 11                                                      *         
* DATA OBJECT FOR BRANDOCEAN MOBILE TIME                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BMT      NTRDO                                                                  
*                                                                               
BMTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBMT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBMT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 'MOBILE TIME IN USE'                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISBMT   LA    R1,SVCPYEL      SAVE COMPANY ELEMENT                             
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@YES                                           
         TM    CPYSTATB,CPYSBOMT                                                
         BO    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 'MOBILE TIME IN USE'                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALBMT   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATB,FF-CPYSBOMT MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATB,CPYSBOMT                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* STATUS BYTE 12 CPYSTATC                                             *         
* DATA OBJECT FOR COMPANY ON BRANDOCEAN TIME                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
COBT     NTRDO                                                                  
*                                                                               
COBTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOBT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOBT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COMPANY ON BRANDOCEAN TIME                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISCOBT  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@YES                                           
         TM    CPYSTATC,CPYCOBOT                                                
         BO    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COMPANY ON BRANDOCEAN TIME                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALCOBT  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATC,FF-CPYCOBOT MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATC,CPYCOBOT                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* STATUS BYTE 12 CPYSTATC = CPYRQBFE                                  *         
* DATA OBJECT FOR COMPANY ON BRANDOCEAN TIME                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RBFE     NTRDO                                                                  
*                                                                               
RBFETBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRBFE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRBFE)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COMPANY REQ BALANCED FILES FOR EXPORT                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISRBFE  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@YES                                           
         TM    CPYSTATC,CPYRQBFE                                                
         BO    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COMPANY REQ BALANCED FILES FOR EXPORT                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALRBFE  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATC,FF-CPYRQBFE MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATC,CPYRQBFE                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* STATUS BYTE 12 CPYSRMEM                                             *         
* DATA OBJECT FOR EMAIL OVERDUE REMINDER                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
EMOVR    NTRDO                                                                  
*                                                                               
EMOVRTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISEMOV)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEMOV)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EMAIL OVERDUE REMINDER STATUS                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISEMOV  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@YES                                           
         TM    CPYSTATC,CPYSRMEM                                                
         BO    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE EMAIL OVERDUE REMINDER STATUS                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALEMOV  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATC,FF-CPYSRMEM MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATC,CPYSRMEM                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* STATUS BYTE 12                                                      *         
* DATA OBJECT FOR ENFORCING OFFICES WITHIN RESOURCES                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RESOF    NTRDO                                                                  
*                                                                               
RESOFTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRESOF)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRESOF)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ENFORCE OFFICES ON RESOURCES                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISRESOF LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@YES                                           
         TM    CPYSTATC,CPYSROFF   N=NOT ENFORCING OFFICES ON RESOURCES         
         BO    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ENFORCE OFFICES ON RESOURCES                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALRESOF LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN2Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATC,FF-CPYSROFF MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATC,CPYSROFF   ENFORCE OFFICES ON RESOURCES                 
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
*                                                                               
***********************************************************************         
* DATA OBJECT FOR ANALYZE EXPENSE ORDERS AT ORDER ENTRY               *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  ANALYZE EXP ORDERS AT ORDER ENTRY                     *         
***********************************************************************         
         SPACE 1                                                                
AEXPO    NTRDO                                                                  
*                                                                               
AEXPOTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISEXPO)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEXPO)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY ANALYZE EXP ORDERS AT ORDER ENTRY PROFILE                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISEXPO  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT5,CPYSAEOE                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COMPANY USING NEW 21 BILLING PROFILE                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALEXPO  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT5,FF-CPYSAEOE                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT5,CPYSAEOE                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* STATUS BYTE 13                                                      *         
* DATA OBJECT FOR 'USE PAY2JOB'                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                           *SGAV                
PAY2J    NTRDO                                                                  
*                                                                               
PAY2JTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPAY2J)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPAY2J)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 'USE PAY2JOB'                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISPAY2J LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         CLI   CPYLN,CPYLN4Q                                                    
         BL    EXITOK                                                           
         TM    CPYSTATD,CPYSAP2J   USE PAY2JOB?                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BCSPACES                                         
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 'USE PAY2JOB'                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALPAY2J LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN4Q                                                    
         BL    EXITELTS            OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATD,FF-CPYSAP2J                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATD,CPYSAP2J   USE PAY2JOB?                                 
         B     EXITOK                                                           
         DROP  R1                                          *SGAV                
         EJECT                                                                  
***********************************************************************         
* STATUS BYTE 13                                                      *         
* DATA OBJECT FOR 'GREENPLUM DATA EXTRACT'                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
GPDX     NTRDO                                                                  
*                                                                               
GPDXTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISGPDX)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGPDX)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
                                                                                
***********************************************************************         
* DISPLAY 'GREENPLUM DATA EXTRACT'                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISGPDX  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         CLI   CPYLN,CPYLN4Q                                                    
         BL    EXITOK                                                           
         TM    CPYSTATD,CPYSGPDX   GREENPLUM DATA EXTRACT                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BCSPACES                                         
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
                                                                                
***********************************************************************         
* VALIDATE 'GREENPLUM DATA EXTRACT'                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALGPDX  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN4Q                                                    
         BL    EXITELTS            OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATD,FF-CPYSGPDX                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATD,CPYSGPDX   GREENPLUM DATA EXTRACT                       
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* DATA OBJECT FOR GL SYSTEM START MONTH                               *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  GL SYSTEM START MONTH                                 *         
***********************************************************************         
         SPACE 1                                                                
GLSMO    NTRDO                                                                  
*                                                                               
GLSMOTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISGLMO)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGLMO)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY GL SYSTEM START MONTH                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R3                                                        
DISGLMO  LA    R3,SVCPYEL                SAVE COMPANY ELEMENT                   
         MVC   AMOAFLD,FVADDR            SAVE THIS FIELD ADDR                   
         NI    FVATRB,X'FF'-FVAPROT      UNPROTECT THIS FIELD                   
*                                                                               
         MVC   FVIFLD(L'AC@NFNA),AC@NFNA DEFAULT TO (N/A)                       
         OC    CPYGLMOA,CPYGLMOA         ANY GL START MOA?                      
         BZ    EXITOK                    NO,EXIT                                
         MVC   BOWORK1(12),BCSPACES      YES, GET CURRENT DATE                  
         GOTO1 VDATCON,BOPARM,(5,BOWORK1),(1,BOWORK1+11)                        
         CLC   CPYGLMOA,BOWORK1+11       COMPARE GCPYGLMOA WITH CUR DT          
         BH    DISGLM02                  HIGHER, EXIT                           
         OI    FVATRB,FVAPROT            NO, PROTECT THE FIELD                  
*                                                                               
DISGLM02 MVC   BOWORK1(12),BCSPACES                                             
         MVC   BOWORK1(2),CPYGLMOA       CONVERT GL MOA                         
         MVI   BOWORK1+2,X'01'           FORCE IN A DAY                         
         GOTO1 VDATCON,BOPARM,(1,BOWORK1),(6,FVIFLD) DISPLAY GL MOA             
         B     EXITOK                    EXIT                                   
         DROP  R3                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE GL SYSTEM START MONTH                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R3                                                        
VALGLMO  LA    R3,SVCPYEL                SAVE COMPANY ELEMENT                   
         MVC   AMOAFLD,FVADDR            SAVE GL MOA FIELD                      
         NI    FVATRB,X'FF'-FVAPROT      UNPROTECT THIS FIELD                   
         OC    CPYGLMOA,CPYGLMOA         G/L START DATE AVAILABLE?              
         BZ    VALGLMO2                  NO                                     
         MVC   BOWORK1(12),BCSPACES      YES, GET CURRENT DATE                  
         GOTO1 VDATCON,BOPARM,(5,BOWORK1),(1,BOWORK1+11)                        
         CLC   CPYGLMOA,BOWORK1+11       COMPARE GCPYGLMOA WITH CUR DT          
         BH    VALGLMO2                  IF HIGH, ALLOW USER TO CHANGE          
         OI    FVATRB,FVAPROT            NO, PROTECT THE FIELD                  
         B     EXITOK                    EXIT                                   
*                                                                               
VALGLMO2 CLI   FVILEN,0                  G/L START MOA ENETRED?                 
         BNE   VALGLMO4                  NO, EXIT                               
         XC    CPYGLMOA,CPYGLMOA         INIT G/L MOA                           
         B     EXITOK                    EXIT                                   
*                                                                               
VALGLMO4 CLC   FVIFLD(L'AC@NFNA),AC@NFNA DEFAULT TO (N/A)                       
         BE    EXITOK                    EXIT                                   
         MVC   BOWORK1(12),BCSPACES      VALIDATE G/L START MONTH               
         MVC   BOWORK1(3),FVIFLD                                                
         MVC   BOWORK1+3(2),=C'01'       MOVE REFRESH DATE MMMDD                
         GOTO1 VDATVAL,BOPARM,(X'01',BOWORK1),BOWORK1                           
         CLI   3(R1),0                   VAILD G/L START MONTH                  
         BNE   VALGLMO6                  YES                                    
         MVC   FVMSGNO,=AL2(AE$INDAT)    NO,INVALID DATE FORMAT                 
         B     EXITL                                                            
*                                                                               
VALGLMO6 MVC   BOWORK1(2),FVIFLD+4       INSERT YEAR                            
         GOTO1 VDATCON,BOPARM,(0,BOWORK1),(1,BOWORK1+11)                        
         MVC   CPYGLMOA,BOWORK1+11       GET YMD PACKED                         
         MVC   BOWORK1(12),BCSPACES                                             
         GOTO1 VDATCON,BOPARM,(5,BOWORK1),(1,BOWORK1+11)                        
         CLC   CPYGLMOA,BOWORK1+11       COMPARE CURRENT YMD PACKED             
         BH    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INMOA)    IF LESS, DISPLAY AN ERROR              
         B     EXITL                                                            
*                                                                               
         DROP  R3                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR GL IMMEDIATE UPDATE SETTING                         *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  IMMEDIATE UPDATE (Y/N)                                *         
***********************************************************************         
         SPACE 1                                                                
GLIUP    NTRDO                                                                  
*                                                                               
GLIUPTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISGLIU)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGLIU)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY GL IMMEDIATE UPDATE SETTING                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R3                                                        
DISGLIU  LA    R3,SVCPYEL                SAVE COMPANY ELEMENT                   
         NI    FVATRB,(X'FF'-FVAPROT)    PROTECT THIS FIELD                     
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO     SET TO (N/A)                           
         TM    CPYSTAT8,CPYNEWGL         NEW G/L?                               
         BZ    EXITOK                    NO, EXIT                               
         MVC   FVIFLD(L'AC@YES),AC@YES   YES, SET TO YES                        
         OC    CPYGLMOA,CPYGLMOA         G/L MOA START DATE?                    
         BNZ   DISGLIU2                  NO,CONTINUE                            
         MVC   FVIFLD(L'AC@NO),AC@NO     SET TO (N/A)                           
         B     EXITOK                    NO,CONTINUE                            
DISGLIU2 MVC   BOWORK1(12),BCSPACES      YES, GET CURRENT DATE                  
         GOTO1 VDATCON,BOPARM,(5,BOWORK1),(1,BOWORK1+11)                        
         CLC   CPYGLMOA,BOWORK1+11       COMP GCPYGLMOA WITH CUR DT             
         BH    EXITOK                    UNPROTECT IT                           
         OI    FVATRB,FVAPROT            PROTECT THIS FIELD                     
         B     EXITOK                    EXIT                                   
         DROP  R3                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE ON GL IMMEDIATE UPDATE SETTING                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R3                                                        
VALGLIU  LA    R3,SVCPYEL                SAVE COMPANY REC                       
         NI    FVATRB,(X'FF'-FVAPROT)    UNPROTECT THIS FIELD                   
*                                                                               
VALGLIU2 OC    CPYGLMOA,CPYGLMOA         G/L MOA START DATE?                    
         BZ    VALGLIU6                  NO,CONTINUE                            
         MVC   BOWORK1(12),BCSPACES      YES, GET CURRENT DATE                  
         GOTO1 VDATCON,BOPARM,(5,BOWORK1),(1,BOWORK1+11)                        
         CLC   CPYGLMOA,BOWORK1+11       COMP GCPYGLMOA WITH CUR DT             
         BH    VALGLIU4                  UNPROTECT IT                           
         TM    CPYSTAT8,CPYNEWGL                                                
         BZ    VALGLIU4                                                         
         OI    FVATRB,FVAPROT            PROTECT THIS FIELD                     
         B     EXITOK                    EXIT                                   
VALGLIU4 NI    CPYSTAT8,(X'FF'-CPYNEWGL)                                        
         CLC   FVIFLD(L'AC@YES),AC@YES                                          
         BNE   EXITOK                                                           
         OI    CPYSTAT8,CPYNEWGL         SET IMMEDIATE UPDATE                   
         B     EXITOK                                                           
*                                                                               
VALGLIU6 CLC   FVIFLD(L'AC@YES),AC@YES   IF IMMEDIATE UPDATE?                   
         BNE   EXITOK                    NO                                     
         MVC   FVMSGNO,=AL2(AE$MISGL)    YES, SET CURSOR TO MOA                 
         MVC   FVADDR,AMOAFLD            AND EXIT WITH ERROR                    
         B     EXITL                     EXIT                                   
*                                                                               
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 5                                                       *         
* DATA OBJECT FOR P&L OFFICES                                         *         
* =FILE LABEL:  OFF=P&L                                               *         
* =AFM  LABEL:  ALLOW VEND/OFFSTS ACROSS OFFICES                      *         
***********************************************************************         
         SPACE 1                                                                
PLOF     NTRDO                                                                  
*                                                                               
PLOFTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFP)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YES OR NO                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISOFP   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT5,CPYSOFPL                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON P&L OFFICES                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALOFP   LA    R1,SVCPYEL                                                       
*                                                                               
         NI    CPYSTAT5,X'FF'-CPYSOFPL                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT5,CPYSOFPL                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR REGULAR AND EFFECTIVE BATCH APPROVAL                          
* =FILE LABEL:  APP=REG AND APP=EFF                                             
* =AFM  LABEL:  TYPE OF BATCH APPROVAL REQUIRED                                 
***********************************************************************         
         SPACE 1                                                                
BAPP     NTRDO                                                                  
*                                                                               
BAPPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISBAP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBAP)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTBAP)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTBAP)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTBAP)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY NONE, REG, EFF OR ALL                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISBAP   LA    R1,SVCPYEL                                                       
*                                                                               
         TM    CPYSTAT5,(CPYSBAPR+CPYSBAPE)                                     
         BNO   *+14                                                             
         MVC   FVIFLD(L'AC@ALL),AC@ALL                                          
         B     EXITOK                                                           
         TM    CPYSTAT5,CPYSBAPR                                                
         BZ    *+14                                                             
         MVC   FVIFLD(L'AC@REG2),AC@REG2                                        
         B     EXITOK                                                           
         TM    CPYSTAT5,CPYSBAPE                                                
         BZ    *+14                                                             
         MVC   FVIFLD(L'AC@EFF3),AC@EFF3                                        
         B     EXITOK                                                           
         MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON REGULAR AND EFFECTIVE BATCH APPROVAL                              
* VALID ENTRIES:                                                                
* NONE                                                                          
* EFF - EFFECTIVE BATCH APPROVAL ONLY                                           
* REG - REGULARY BATCH APPROVAL ONLY                                            
* ALL - BOTH EFFECTIVE AND REGULAR BATCH APPROVAL                               
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALBAP   LA    R1,SVCPYEL                                                       
*                                                                               
         NI    CPYSTAT5,X'FF'-(CPYSBAPR+CPYSBAPE)                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NONE                                                
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@REG2                                                
         BNE   *+12                                                             
         OI    CPYSTAT5,CPYSBAPR                                                
         B     EXITOK                                                           
         EXCLC R3,FVIFLD,AC@EFF3                                                
         BNE   *+12                                                             
         OI    CPYSTAT5,CPYSBAPE                                                
         B     EXITOK                                                           
         EXCLC R3,FVIFLD,AC@ALL                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT5,CPYSBAPR+CPYSBAPE                                       
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY REUGLAT BATCH APPROVAL        FILTER                        *         
***********************************************************************         
         SPACE 1                                                                
DFLTBAP  DS    0H                                                               
         B     DISPFLT                                                          
*        B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON REGULAR BATCH APPROVAL        FILTER                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTBAP  B     VALFLTR                                                          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON REGULAR BATCH APPROVAL                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTBAP  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLI   FLTIFLD,1                                                        
         BNE   DOFTOBAP                                                         
         TM    CPYSTAT5,CPYSBAPR                                                
         BO    FLTXE                                                            
         BZ    FLTXL                                                            
DOFTOBAP TM    CPYSTAT5,CPYSBAPR                                                
         BZ    FLTXE                                                            
         BO    FLTXL                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR NEW COST SYTEM IN USE (INCORPORTATES THE 'COST'     *         
* STATUS ALSO                                                         *         
* =FILE LABEL:  COST AND NEWCOST                                      *         
* =AFM  LABEL:  USES COST ACCOUNTING SYSTEM                           *         
***********************************************************************         
         SPACE 1                                                                
NCST     NTRDO                                                                  
*                                                                               
NCSTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISNCS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNCS)                                 
*        DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTNCS)                               
*        DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTNCS)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTNCS)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY YES OR NO                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISNCS   LA    R1,SVCPYEL                                                       
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT5,CPYSNCST                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON NEW COST SYSTEM IN USE                                  *         
* WE NO LONGER SUPPORT BOTH THE OLD 'COST' STATUS BUT I TURN ON THE   *         
* SINCE IT'S STILL REFERENCED BY PROGRAMS.                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALNCS   LA    R1,SVCPYEL                                                       
*                                                                               
         NI    CPYSTAT5,X'FF'-CPYSNCST                                          
         NI    CPYSTAT1,X'FF'-CPYSCOST                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT5,CPYSNCST                                                
         OI    CPYSTAT1,CPYSCOST        TURN ON ORIGINAL COST BIT TOO           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* DISPLAY NEW COST SYSTEM IN USE            FILTER                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTNCS  DS    0H                                                               
         B     DISPFLT                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ON NEW COST SYSTEM IN USE        FILTER                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTNCS  B     VALFLTR                                                          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON NEW COST SYSTEM IN USE                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2           A(RECORD) IN R2 - SEE ABOVE...              
DOFTNCS  GOTO1 FNDCPEL,BOPARM,CPYRECD                                           
         L     R1,BOPARM                                                        
         USING CPYELD,R1                                                        
*                                                                               
         CLI   FLTIFLD,1                                                        
         BNE   DOFTNCS2                                                         
         TM    CPYSTAT5,CPYSNCST                                                
         BO    FLTXE                                                            
         BZ    FLTXL                                                            
DOFTNCS2 TM    CPYSTAT5,CPYSNCST                                                
         BZ    FLTXE                                                            
         BO    FLTXL                                                            
         DROP  R1,R2                                                            
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR LENGTH OF DEPARTMENT CODES                          *         
* =FILE LABEL:  DPTL=                                                 *         
* =AFM  LABEL:  DEPARTMENT CODE LENGTH                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DPTL     NTRDO                                                                  
*                                                                               
DPTLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDPTL)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDPTL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LENGTH OF DEPARTMENT CODES FIELD                        *         
* DEFAULT IS 2                                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISDPTL  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYDEPTL,0                                                       
         BH    *+12                                                             
         MVI   FVIFLD,C'2'                                                      
         B     EXITOK                                                           
         MVC   FVIFLD(L'CPYDEPTL),CPYDEPTL                                      
         OI    FVIFLD,X'F0'                                                     
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE LENGTH OF DEPARTMENT CODES FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALDPTL  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVI   CPYDEPTL,2          DEFAULT TO 2                                 
         CLI   FVILEN,0                                                         
         BE    VDPTL10                                                          
         TM    FVIIND,FVINUM       IS IT A NUMERIC                              
         BZ    EXITNOTN            ERROR - NOT A NUMBER                         
         CLI   FVIFLD,C'2'         ONLY 2 AND 3 ARE VALID                       
         BE    *+12                (USED TO BE 1,2,3,4 BUT 1 AND 4              
         CLI   FVIFLD,C'3'         DO NOT WORK.)                                
         BNE   EXITNV                                                           
         MVC   CPYDEPTL,FVIFLD                                                  
VDPTL10  NI    CPYDEPTL,X'0F'                                                   
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRESTO LEDGERS                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PRLDG    NTRDO                                                                  
*                                                                               
PRLDGTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPLDG)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPLDG)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE PRESTO LEDGERS                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISPLDG  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
         CLI   CPYLN,CPYLN4Q       MAKE SURE BIGGEST LENGTH                     
         BL    EXITOK                                                           
         MVC   FVIFLD(L'CPYPRSTL),CPYPRSTL                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE PRESTO LEDGERS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALPLDG  LA    R1,SVCPYEL          SAVED COMPANY ELEMENT                        
         MVC   CPYPRSTL,BCSPACES                                                
         MVC   SVPRSTL,BCSPACES                                                 
         LA    RF,SVPRSTL                                                       
         ST    RF,SVADDR           SAVE ADDRESS OF CURRENT POSITION             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   R4,FVILEN           RE=# OF LEDGERS ENTERED BY USER              
         LA    R3,FVIFLD                                                        
VALPL10  CLI   0(R3),C' '          IS THIS A BLANK ENTRY?                       
         BNH   VALPL20             YES SO IGNORE IT                             
         ST    R3,SVADDR2          SAVE POSITION OF THIS ENTRY IN FIELD         
         MVC   BCBYTE1,0(R3)                                                    
         BAS   RE,VPLDGRS                                                       
         BL    EXITNV                                                           
         L     RF,SVADDR                                                        
         MVC   0(1,RF),BCBYTE1                                                  
         LA    RF,1(RF)            BUMP TO NEXT POSITION IN SAVED FIELD         
         ST    RF,SVADDR                                                        
VALPL20  LA    R3,1(R3)            BUMP TO NEXT ENTRY USER ENTERED              
         BCT   R4,VALPL10                                                       
*                                                                               
         MVC   CPYPRSTL,SVPRSTL                                                 
         B     EXITOK                                                           
         DROP  R1                                                               
***********************************************************************         
* ROUTINE TO VALIDATE THE PRESTO LEDGERS ENTERED BY USER AGAINST THE  *         
* VALID ENTRIES AND TO MAKE SURE THERE ARE NO DUPLICATES AND TO MAKE  *         
* SURE THAT THEY DID NOT DUPLICATE THE LEDGER THAT'S IN THE EXTRA     *         
* SUPPLIER FIELD.                                                     *         
* ENTRY - BCBYTE1 CONTAINS THE CURRENT PRESTO LEDGER BEING VALIDATED  *         
*         SVADDR2 POINTS TO THE CURRENT POSITION OF THE LEDGER BEING  *         
*         VALIDATED                                                   *         
* EXIT  - EQUAL IF VALID ENTRY                                        *         
*         LOW IF INVALID ENTRY                                        *         
***********************************************************************         
         SPACE 1                                                                
VPLDGRS  NTR1  ,                                                                
         CLC   BCBYTE1,SVXSUPP     SAME AS LEDGER IN EXTRA SUPPLIER?            
         BE    VPLDGL              YES - INVALID                                
*                                                                               
         LA    RF,PLDGTB           TABLE OF VALID PRESTO LEDGERS                
VPLDG10  CLI   0(RF),X'FF'                                                      
         BE    VPLDGL              INVALID ENTRY                                
         CLC   BCBYTE1,0(RF)                                                    
         BE    VPLDG20             VALID ENTRY                                  
         LA    RF,1(RF)                                                         
         B     VPLDG10                                                          
*                                                                               
VPLDG20  LA    RF,FVIFLD           NOW CHECK FOR DUPLICATES                     
         L     RE,SVADDR2                                                       
         LHI   R1,5                5 LEDGERS MAX                                
VPLDG30  CR    RF,RE               DON'T CHECK THE LDGR BEING VALIDATED         
         BE    *+14                                                             
         CLC   BCBYTE1,0(RF)                                                    
         BE    VPLDGL                                                           
         LA    RF,1(RF)                                                         
         BCT   R1,VPLDG30                                                       
         B     VPLDGE                                                           
*                                                                               
VPLDGL   CLI   *,FF                                                             
         B     VPLDGX                                                           
VPLDGE   CR    RB,RB                                                            
VPLDGX   XIT1                                                                   
*                                                                               
PLDGTB   DC    C'ABEXY'                                                         
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR REP CODE FOR COMM. ONLY                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
REPC     NTRDO                                                                  
*                                                                               
REPCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISREPC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALREPC)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY REP CODE FOR COMM. ONLY FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISREPC  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
         OC    CPYREPC,CPYREPC                                                  
         BZ    EXITOK                                                           
         SR    R3,R3                                                            
         ICM   R3,3,CPYREPC                                                     
         CVD   R3,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  BCWORK(5),BODUB1+5(3)                                            
         MVC   FVIFLD(4),BCWORK+1                                               
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE REP CODE FOR COMM. ONLY FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALREPC  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         XC    CPYREPC,CPYREPC                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVIFLD,C'0'         MUST START WITH A ZERO                       
         BNE   EXITNV                                                           
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         CVB   R3,BODUB1                                                        
         STCM  R3,3,CPYREPC                                                     
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PRINCIPLE ID NUMBER                                 *         
* =FILE LABEL:  ID=                                                   *         
* =AFM  LABEL:  ACCOUNTING PRINCIPAL ID #                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PRID     NTRDO                                                                  
*                                                                               
PRIDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRID)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRID)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PRINCIPAL ID NUMBER                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISPRID  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
         EDIT  CPYUID,(5,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1              
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PRINCIPLE ID NUMBER                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALPRID  LA    R1,SVCPYEL                                                       
                                                                                
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)                                                 
         CVB   R3,BODUB1                                                        
         STCM  R3,3,SVCUID                                                      
*                                                                               
         LA    R3,IOKEY                                                         
         USING CTIKEY,R3                                                        
         XC    CTIKEY,CTIKEY             CLEAR KEY FOR ID REC LOOKUP            
         MVI   CTIKTYP,C'I'              RECORD TYPE I FOR ID REC               
         MVC   CTIKID+8(L'CPYUID),SVCUID ID NUMBER                              
         LHI   R1,XOREAD+XOCONFIL+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$COFIL)                                           
         B     EXITL                                                            
*                                                                               
         L     R3,AIO2                                                          
         LA    R4,CTIDATA                 R4=A(FIRST ELEMENT)                   
VPRID10  CLI   0(R4),0                    END OF RECORD                         
         BE    VPRID60                                                          
         CLI   0(R4),CTDSCELQ             DESCRIP EL FOR ABBR                   
         BE    VPRID30                                                          
         CLI   0(R4),CTAGYELQ             AGENCY ALPHA EL                       
         BE    VPRID40                                                          
         CLI   0(R4),CTSYSELQ             AUTH EL TO CHECK ID NUMBER            
         BE    VPRID50                                                          
VPRID20  ZIC   R1,1(R4)                   LENGTH IF EL INTO R1                  
         AR    R4,R1                      POINT R4 TO NEXT EL                   
         B     VPRID10                                                          
*                                                                               
         USING CTDSCD,R4                                                        
VPRID30  MVC   SVCLOGO,CTDSC              AGENCY SIGN ON INTO CO EL             
         B     VPRID20                                                          
*                                                                               
         USING CTAGYD,R4                                                        
VPRID40  MVC   SVCALPHA,CTAGYID           AGENCY ALPHA                          
         B     VPRID20                                                          
*                                                                               
         USING CTSYSD,R4                                                        
VPRID50  CLI   CTSYSNUM,X'06'             IS IT ACCOUNTING                      
         BNE   VPRID20                    NO - GO LOOK FOR ANOTHER 21           
         CLC   CTSYSAGB,CPYKCPY           SAME HEXCOMP AS SCREEN                
         BE    VPRID20                    YES - ITS OK                          
         MVC   FVMSGNO,=AL2(AE$COFIL)                                           
         B     EXITL                                                            
*                                                                               
         USING CPYELD,R1                                                        
VPRID60  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
         MVC   CPYUID,SVCUID                                                    
         MVC   CPYLOGO,SVCLOGO                                                  
         MVC   CPYALPHA,SVCALPHA                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR COMPANY CODE FOR CONVERSION USE                     *         
* =FILE LABEL:  TID=                                                  *         
* =AFM  LABEL:  CONVERSION HEX CODE                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TCMP     NTRDO                                                                  
*                                                                               
TCMPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCMP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTCMP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY HEX COMPANY CONVERSION CODE                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R3                                                        
DISTCMP  LA    R3,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BL    DTCMP10                                                          
         CLI   CPYTCMP,0                                                        
         BE    DTCMP10                                                          
         GOTO1 VHEXOUT,BOPARM,CPYTCMP,FVIFLD,1,0,0                              
         B     EXITOK                                                           
*                                                                               
DTCMP10  MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE HEX COMPANY CONVERSION CODE                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R3                                                        
VALTCMP  LA    R3,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVI   CPYTCMP,0                                                        
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ZIC   RF,FVXLEN                                                        
         EXCLC RF,FVIFLD,AC@NONE                                                
         BE    EXITOK                                                           
         GOTO1 VHEXIN,BOPARM,FVIFLD,CPYTCMP,2                                   
         MVC   IOKEY(L'CPYKEY),BCSPACES                                         
         MVC   IOKEY(L'CPYTCMP),CPYTCMP   COMPANY EXISTS ON FILE                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                 READ COMPANY RECORD                          
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INCPY)                                           
         B     EXITL               INVALID COMPANY CODE                         
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING REQUISITION PREFIX                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                     I           *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RQPF     NTRDO                                                                  
*                                                                               
RQPFTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRQPF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRQPF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE REQUISITION PREFIX                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISRQPF  LA    R1,SVCPYEL                                                       
         CLI   CPYLN,CPYLN4Q                                                    
         BL    EXITOK                                                           
         MVC   FVIFLD(L'CPYREQPF),CPYREQPF                                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE REQUISITION PREFIX                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,RE                                                        
VALRQPF  LA    RE,SVCPYEL          SAVE COMPANY ELEMENT                         
         MVC   CPYREQPF,FVIFLD                                                  
         B     EXITOK                                                           
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING REQUISITION SUFFIX                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RQSF     NTRDO                                                                  
*                                                                               
RQSFTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRQSF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRQSF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE REQUISITION SUFFIX                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISRQSF  LA    R1,SVCPYEL                                                       
         CLI   CPYLN,CPYLN4Q                                                    
         BL    EXITOK                                                           
         MVC   FVIFLD(L'CPYREQSF),CPYREQSF                                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE REQUISITION SUFFIX                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,RE                                                        
VALRQSF  LA    RE,SVCPYEL          SAVE COMPANY ELEMENT                         
         MVC   CPYREQSF,FVIFLD                                                  
         B     EXITOK                                                           
         DROP  RE                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING REQUISITION NAME                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RQNM     NTRDO                                                                  
*                                                                               
RQNMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRQNM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRQNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE REQUISITION NAME                                        *         
***********************************************************************         
         SPACE 1                                                                
DISRQNM  GOTO1 AGETFFT,BOPARM,(R2),FFTTRALI                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE REQUISITION NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
VALRQNM  GOTO1 ABLDFFT,BOPARM,(R2),FFTTRALI                                     
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING DRAFT CLAIM NUMBER - 5TH PAGE            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DCLN     NTRDO                                                                  
*                                                                               
DCLNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDCLN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDCLN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DRAFT CLAIM NUMBER                                      *         
***********************************************************************         
         SPACE 1                                                                
DISDCLN  GOTO1 AGETFFT,BOPARM,(R2),FFTTDCLN                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE DRAFT CLAIM NUMBER                                     *         
***********************************************************************         
         SPACE 1                                                                
VALDCLN  TM    FVIIND,FVITHIS       FIELD INPUT THIS TIME                       
         BO    VALDCL10                                                         
         GOTO1 AGETFFT,BOPARM,(R2),FFTTDCLN READ CURRENT COPY OF RECORD         
*                        AND MOVE TO FVIFLD                                     
*                        IT MAY HAVE CHANGED BY BRANDOCEAN EXPENSES             
         B     VALDCLNX                                                         
*                                                                               
VALDCL10 TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
VALDCLNX GOTO1 ABLDFFT,BOPARM,(R2),FFTTDCLN                                     
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING LIVE CLAIM NUMBER - 5TH PAGE             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LCLN     NTRDO                                                                  
*                                                                               
LCLNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCLN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLCLN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LIVE CLAIM NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
DISLCLN  GOTO1 AGETFFT,BOPARM,(R2),FFTTLCLN                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE LIVE CLAIM NUMBER                                      *         
***********************************************************************         
         SPACE 1                                                                
VALLCLN  TM    FVIIND,FVITHIS       FIELD INPUT THIS TIME                       
         BO    VALLCL10                                                         
         GOTO1 AGETFFT,BOPARM,(R2),FFTTLCLN READ CURRENT COPY OF RECORD         
*                        AND MOVE TO FVIFLD                                     
*                        IT MAY HAVE CHANGED BY BRANDOCEAN EXPENSES             
         B     VALLCLNX                                                         
VALLCL10 TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
VALLCLNX GOTO1 ABLDFFT,BOPARM,(R2),FFTTLCLN                                     
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING ESTIMATE NUMBER - BRAND OCEAN CONTROLS   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ESTN     NTRDO                                                                  
*                                                                               
ESTNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISESTN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALESTN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ESTIMATE NUMBER                                             *         
***********************************************************************         
         SPACE 1                                                                
DISESTN  GOTO1 AGETFFT,BOPARM,(R2),FFTTESTN                                     
         B     EXITOK                                                           
       SPACE 2                                                                  
***********************************************************************         
* VALIDATE ESTIMATE NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
VALESTN  TM    FVIIND,FVITHIS       FIELD INPUT THIS TIME                       
         BO    VALESTNX                                                         
         GOTO1 AGETFFT,BOPARM,(R2),FFTTESTN READ CURRENT COPY OF RECORD         
*                        AND MOVE TO FVIFLD                                     
*                        IT MAY HAVE CHANGED BY BRANDOCEAN ESTIMATES            
         B     VALESTNX                                                         
*                                                                               
VALEST10 TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
VALESTNX GOTO1 ABLDFFT,BOPARM,(R2),FFTTESTN                                     
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING EXPENDITURE TYPE                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
EXTYP    NTRDO                                                                  
*                                                                               
EXTYPTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISEXTY)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEXTY)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE EXPENDITURE TYPE                                        *         
***********************************************************************         
         SPACE 1                                                                
DISEXTY  GOTO1 AGETFFT,BOPARM,(R2),FFTTEXTY                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE EXPENDITURE TYPE                                       *         
***********************************************************************         
         SPACE 1                                                                
VALEXTY  CLI   FVILEN,0                                                         
         BE    VALDEXT2                                                         
T        USING ETYRECD,IOKEY                                                    
         XC    T.ETYKEY,T.ETYKEY   CHECK IF EXPENDITURE TYPE EXISTS             
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CPYKCPY                                                
         MVC   T.ETYKCODE,FVIFLD                                                
         MVC   IOKEYSAV,IOKEY                                                   
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(ETYKOFFC-ETYRECD),IOKEY                                 
         BE    VALDEXT2                                                         
         MVC   FVMSGNO,=AL2(AE$INETY)                                           
         B     EXITL                                                            
*                                                                               
VALDEXT2 GOTO1 ABLDFFT,BOPARM,(R2),FFTTEXTY                                     
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING STAFF PAYABLES ACCOUNTS                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
STFP     NTRDO                                                                  
*                                                                               
STFPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTFP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTFP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE STAFF PAYABLES ACCOUNTS                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING SPAELD,R4                                                        
DISSTFP  LA    R4,CPYRFST                                                       
         LA    R6,FVIFLD                                                        
         SR    RF,RF                                                            
DSTFP10  CLI   SPAEL,0                                                          
         BE    EXITOK                                                           
         CLI   SPAEL,SPAELQ                                                     
         BNE   DSTFP20                                                          
         CLI   SPATYPE,SPATSTFP    STAFF PAYABLES ACCOUNT                       
         BNE   DSTFP20                                                          
         CLC   SPAAULA,BCSPACES                                                 
         BNH   DSTFP20             MAKE SURE THERE IS AN ACCOUNT!!!             
         LA    R1,FVIFLD                                                        
         CR    R6,R1               FIRST STAFF PAYABLES ACCOUNT?                
         BE    *+14                NO - DON'T ADD A COMMAS                      
         MVC   0(L'BCCOMMA,R6),BCCOMMA                                          
         LA    R6,1(,R6)                                                        
*                                                                               
         MVC   0(L'SPAAULA,R6),SPAAULA                                          
         LA    R6,L'SPAAULA-1(R6)                                               
         CLI   0(R6),C' '          FIND END OF ACCOUNT CODE                     
         BH    *+8                                                              
         BCT   R6,*-8                                                           
         LA    R6,1(,R6)                                                        
*                                                                               
DSTFP20  IC    RF,SPALN                                                         
         AR    R4,RF                                                            
         B     DSTFP10                                                          
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE STAFF PAYABLES ACCOUNTS                                *         
***********************************************************************         
         SPACE 1                                                                
VALSTFP  GOTO1 ADELEL,BOPARM,('SPAELQ',CPYRECD),(1,=AL1(SPATSTFP))              
         CLI   FVILEN,0            ANY STAFF PAYABLE ACCOUNTS?                  
         BE    EXITOK                                                           
*                                                                               
         GOTO1 VSCANNER,BOPARM,(L'SPAAULA,FVIHDR),(5,BLOCK),C',=   '            
         SR    R4,R4                                                            
         ICM   R4,1,4(R1)          NUMBER OF LINES USED                         
         BZ    EXITNV                                                           
         CHI   R4,3                                                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMACT)                                           
         B     EXITL               TOO MANY ACCOUNTS                            
*                                                                               
T        USING SPAELD,BOELEM                                                    
         XC    BOELEM,BOELEM                                                    
         MVI   T.SPAEL,SPAELQ      STAFF PAYABLES ACCOUNT ELEMENT               
         MVI   T.SPATYPE,SPATSTFP                                               
         MVI   T.SPALN,SPALNQ                                                   
         XC    BOWORK1,BOWORK1                                                  
*                                                                               
         LA    R6,BLOCK                                                         
         USING SCANBLKD,R6                                                      
VSTFP04  CLI   SC2NDLEN,0                                                       
         BE    *+20                                                             
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         MVC   FVXTRA(L'SC2NDFLD),SC2NDFLD                                      
         B     EXITL               MUST HAVE NO 2ND DIVIDED FIELD               
         CLI   SC1STLEN,2                                                       
         BNL   *+20                                                             
         MVC   FVMSGNO,=AL2(AE$FLDTS)                                           
         MVC   FVXTRA(L'SC1STFLD),SC1STFLD                                      
         B     EXITL               MUST BE 2 OR HIGHER                          
*                                                                               
         CLC   =C'SA',SC1STFLD                                                  
         BE    VSTFP08                                                          
         CLC   =C'SV',SC1STFLD                                                  
         BE    VSTFP08                                                          
         CLC   =C'SX',SC1STFLD                                                  
         BE    VSTFP08                                                          
         CLC   =C'SY',SC1STFLD                                                  
         BE    VSTFP08                                                          
         CLC   =C'SW',SC1STFLD                                                  
         BE    VSTFP08                                                          
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         MVC   FVXTRA(2),SC1STFLD                                               
         B     EXITL               INVALID LEDGER                               
*                                                                               
VSTFP08  MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(1),CPYKCPY    COMPANY CODE                                 
         MVC   IOKEY+1(L'ACTKULA),SC1STFLD                                      
*                                                                               
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BE    VSTFP12                                                          
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         CLI   SC1STLEN,2          IS IT A LEDGER?                              
         BE    *+10                                                             
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         MVC   FVXTRA(L'ACTKULA),SC1STFLD                                       
         B     EXITL               INVALID LEDGER/ACCOUNT                       
*                                                                               
VSTFP12  CLI   SC1STLEN,2          IS IT A LEDGER?                              
         BE    VSTFP14                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('ABLELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VSTFP14                                                          
         MVC   FVMSGNO,=AL2(AE$NHLAC)                                           
         MVC   FVXTRA(L'ACTKULA),SC1STFLD                                       
         B     EXITL               NOT A HIGHER LEVEL ACCOUNT                   
*                                                                               
VSTFP14  LA    RF,BOWORK1                                                       
*                                                                               
VSTFP18  OC    0(L'ACTKULA,RF),0(RF)                                            
         BZ    VSTFP30                                                          
         CLC   0(2,RF),SC1STFLD    SAME LEDGER?                                 
         BNE   VSTFP26             NO - CHECK NEXT ITEM                         
         CLI   SC1STLEN,2          LEDGER INPUT?                                
         BE    VSTFP22                                                          
         CLC   2(L'ACTKACT,RF),BCSPACES                                         
         BE    VSTFP22                                                          
         CLC   0(L'ACTKULA,RF),SC1STFLD                                         
         BNE   VSTFP26                                                          
*                                                                               
VSTFP22  MVC   FVMSGNO,=AL2(AE$DUPAC)                                           
         MVC   FVXTRA(L'ACTKULA),SC1STFLD                                       
         B     EXITL               DUPLICATE ACCOUNT CODE                       
*                                                                               
VSTFP26  LA    RF,L'ACTKULA(,RF)                                                
         B     VSTFP18                                                          
*                                                                               
VSTFP30  MVC   0(L'ACTKULA,RF),SC1STFLD                                         
         MVC   T.SPAAULA,SC1STFLD                                               
         GOTO1 AADDEL,BOPARM,CPYRECD                                            
         LA    R6,SCBLKLQ+4(,R6)   NEXT SCANBLK LINE                            
         BCT   R4,VSTFP04                                                       
         B     EXITOK                                                           
         DROP  T,R6                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INTEROFFICE BALANCING ACCOUNT                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
IBAC     NTRDO                                                                  
*                                                                               
IBACTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISIBAC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIBAC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE INTEROFFICE BALANCING ACCOUNT                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SPAELD,R4                                                        
DISIBAC  LA    R4,CPYRFST                                                       
         SR    RF,RF                                                            
DIBAC10  CLI   SPAEL,0                                                          
         BE    EXITOK                                                           
         CLI   SPAEL,SPAELQ                                                     
         BNE   DIBAC20                                                          
         CLI   SPATYPE,SPATIBAC                                                 
         BNE   DIBAC20                                                          
         CLC   SPAAULA,BCSPACES                                                 
         BNH   DIBAC20             MAKE SURE THERE IS AN ACCOUNT!!!             
         MVC   FVIFLD(L'SPAAULA),SPAAULA                                        
         B     EXITOK                                                           
*                                                                               
DIBAC20  IC    RF,SPALN                                                         
         AR    R4,RF                                                            
         B     DIBAC10                                                          
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE INTEROFFICE BALANCE ACCOUNT                            *         
* MUST ENTER AN SB ACCOUNT AT THE LEVEL ABOVE POSTING LEVEL                     
***********************************************************************         
         SPACE 1                                                                
VALIBAC  GOTO1 ADELEL,BOPARM,('SPAELQ',CPYRECD),(1,=AL1(SPATIBAC))              
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(2),=C'SB'                                                 
         BNE   EXITNV                                                           
*                                                                               
         USING LDGRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN                                                   
         MVC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),=C'SB'                              
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         USING LDGTABD,RE                                                       
         ICM   RE,15,ACALDG                                                     
         SR    R1,R1                                                            
         ZIC   R1,LDGTLVB                                                       
         AHI   R1,1                MIMIMUM INPUT FOR 4 LVL LEDGER               
         ZIC   RF,LDGTLVC          MAXIMUM INPUT FOR 4 LVL LEDGER               
         CLI   LDGTLVD,L'ACTKACT   FOUR LEVEL LEDGER?                           
         BE    VIBAC10                                                          
         ZIC   R1,LDGTLVA                                                       
         AHI   R1,1                MIMIMUM INPUT FOR 3 LVL LEDGER               
         ZIC   RF,LDGTLVB          MAXIMUM INPUT FOR 3 LVL LEDGER               
         CLI   LDGTLVC,L'ACTKACT   THREE LEVEL LEDGER?                          
         BE    VIBAC10                                                          
         ZIC   RF,LDGTLVA          MAXIMUM INPUT FOR 2 LVL LEDGER               
         CLI   LDGTLVB,L'ACTKACT   TWO LEVEL LEDGER?                            
         BNE   EXITNV              NOT VALID FOR A 1 LVL LEDGER                 
*                                                                               
VIBAC10  ZIC   RE,FVILEN           ACTUAL LENGTH ENTERED                        
         SHI   RE,2                SUBTRACT 2 FOR UL                            
         LTR   R1,R1                                                            
         BZ    VIBAC10                                                          
         CR    RE,R1               MAKE SURE MINIMUM LENGTH ENTERED             
         BL    EXITNV                                                           
VIBAC20  CR    RE,RF               MAKE SURE NOT A POSTING LEVEL                
         BH    EXITNV                                                           
*******                                                                         
*                                                                               
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(1),CPYKCPY    COMPANY CODE                                 
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,IOKEY+1,FVIFLD                                                
*                                                                               
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BE    VIBAC30                                                          
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         MVC   FVXTRA(L'ACTKULA),FVIFLD                                         
         B     EXITL               INVALID LEDGER/ACCOUNT                       
*                                                                               
T        USING SPAELD,BOELEM                                                    
VIBAC30  XC    BOELEM,BOELEM                                                    
         MVI   T.SPAEL,SPAELQ      STAFF PAYABLES ACCOUNT ELEMENT               
         MVI   T.SPATYPE,SPATIBAC                                               
         MVI   T.SPALN,SPALNQ                                                   
         MVC   T.SPAAULA,FVIFLD                                                 
         GOTO1 AADDEL,BOPARM,CPYRECD                                            
         B     EXITOK                                                           
         DROP  T,R3                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BULK APID PID                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BAPID    NTRDO                                                                  
*                                                                               
BAPIDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISBAPID)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VBAPID)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BULK API PID                                              *         
***********************************************************************         
         SPACE 1                                                                
T        USING PIDELD,BOELEM                                                    
DISBAPID GOTO1 AGETEL,BOPARM,('PIDELQ',CPYRECD),0                               
         BNE   EXITOK                                                           
         GOTOX ('GETPID',AGROUTS),T.PIDNO                                       
         BNE   EXITOK                                                           
         MVC   FVIFLD(8),BCWORK                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A BULK API PID                                             *         
***********************************************************************         
         SPACE 1                                                                
VBAPID   CLC   FVIFLD(8),BCSPACES                                               
         BH    VBAPID05                                                         
*                                                                               
         CLI   CSACT,A#CHA                                                      
         BNE   EXITOK                                                           
         GOTO1 AGETEL,BOPARM,('PIDELQ',CPYRECD),0                               
         BNE   EXITOK                                                           
         GOTO1 ADELEL,BOPARM,('PIDELQ',CPYRECD),0                               
         B     EXITOK                                                           
*                                                                               
VBAPID05 GOTOX ('VALPID',AGROUTS),FVIFLD                                        
         BNE   VPIDERR                                                          
         MVC   SVPIDBIN,BCWORK                                                  
         CLI   CSACT,A#CHA                                                      
         BNE   EXITOK                                                           
         GOTO1 AGETEL,BOPARM,('PIDELQ',CPYRECD),0                               
         BE    VBAPID20           REPLACE ELEMNET                               
*ADD ELEMENT                                                                    
VBAPID10 XC    BOELEM,BOELEM                                                    
         MVI   T.PIDEL,PIDELQ     PID ELEMENT                                   
         MVI   T.PIDLN,PIDLNQ                                                   
         MVC   T.PIDNO,SVPIDBIN   PID ELEMENT                                   
         GOTO1 AADDEL,BOPARM,CPYRECD                                            
         B     EXITOK                                                           
*REPLACE ELEMENT                                                                
VBAPID20 CLC   T.PIDNO,SVPIDBIN                                                 
         BE    EXITOK                                                           
         XC    BOELEM,BOELEM                                                    
         MVI   T.PIDEL,PIDELQ     PID ELEMENT                                   
         MVI   T.PIDLN,PIDLNQ                                                   
         MVC   T.PIDNO,SVPIDBIN   PID ELEMENT                                   
         GOTO1 AREPEL,BOPARM,('PIDELQ',CPYRECD),0,BOELEM                        
         B     EXITOK                                                           
         DROP  T                                                                
*                                                                               
VPIDERR  MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 9 EXTRA                                                 *         
* DATA OBJECT FOR HTML TEXT FOR ORDERS IN USE                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
HTOR     NTRDO                                                                  
*                                                                               
HTORTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISHTOR)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHTOR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY HTML TEXT FOR ORDERS IN USE                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISHTOR  LA    R1,SVCPXEL                SAVE COMPANY EXTRA ELEMENT             
*                                                                               
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         TM    CPXSTATA,CPXORDRT         HTML TEXT FOR ORDER = ON ?             
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
*                                                                               
***********************************************************************         
* VALIDATE HTML TEXT FOR ORDERS INPUT DETAILS                         *         
***********************************************************************         
*                                                                               
         USING CPXELD,R1                                                        
VALHTOR  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
*                                                                               
         NI    CPXSTATA,FF-(CPXORDRT) SET DEFULT TO - NO                        
         CLC   FVIFLD(1),BC@NO     HTML TEXT FOR ORDERS = NO ?                  
         BE    EXITOK              EXIT                                         
*                                                                               
         CLC   FVIFLD(1),BC@YES    HTML TEXT FOR ORDERS = YES?                  
         BNE   EXITNV              NO, PROCESS ERROR                            
*                                  YES                                          
         OI    CPXSTATA,CPXORDRT   SET HTML TEXT FOR ORDERS TO YES              
         B     EXITOK                                                           
         DROP  R1                                                               
*                                                                               
***********************************************************************         
* STATUS BYTE 1 EXTRA                                                 *         
* DATA OBJECT FOR JOB EMAIL ALERTS                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
JOEM     NTRDO                                                                  
*                                                                               
JOEMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISJOEM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALJOEM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISJOEM  LA    R1,SVCPXEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@YES                                           
         TM    CPXSTAT1,CPXSJOEM   USING JOB EMAIL ALERTS                       
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 'JOB EMAIL ALERTS' SETTING                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALJOEM  LA    R1,SVCPXEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPXSTAT1,FF-CPXSJOEM MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTAT1,CPXSJOEM   USING JOB EMAIL ALERTS                       
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY 'JOB EMAIL ALERTS' SETTING                                  *         
***********************************************************************         
***********************************************************************         
* DATA OBJECT FOR INTERNAL APPROVAL                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
INAPP    NTRDO                                                                  
*                                                                               
INAPPTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISIAPP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIAPP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EMAIL OVERDUE REMINDER STATUS                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISIAPP  LA    R1,SVCPXEL          SAVE COMPANY ELEMENT                         
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPXSTAT1,CPXSEIAY+CPXSEIAO                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NO),AC@YES                                           
         TM    CPXSTAT1,CPXSEIAY                                                
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'AC@OFFC),AC@OFFC                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ESTIMATE INTERNAL APPROVALS                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALIAPP  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT1,FF-(CPXSEIAY+CPXSEIAO)                                  
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@YES                                                 
         BNE   VALEIAP2                                                         
         TM    CPXSTAT2,CPXSFAPE   PREVENT SETTING IF AUTO APPROVAL             
         BNZ   EXITNV                                                           
         OI    CPXSTAT1,CPXSEIAY                                                
         B     EXITOK                                                           
VALEIAP2 CLC   FVIFLD(1),AC@OFFC                                                
         BNE   EXITNV                                                           
         TM    CPXSTAT2,CPXSFAPE   PREVENT SETTING IF AUTO APPROVAL             
         BNZ   EXITNV                                                           
         OI    CPXSTAT1,CPXSEIAO                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 2 EXTRA                                                 *         
* DATA OBJECT FOR SETTING INTERNAL USE ONLY DEFAULT SETTING           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
INUSD    NTRDO                                                                  
*                                                                               
INUSDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPIUSD)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIUSD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INTERNAL USE ONLY DEFAULT SETTING                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPIUSD LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD,AC@OFFC                                                   
         TM    CPXSTAT2,CPXIUSDO   SET AT OFFICE?                               
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPXSTAT2,CPXIUSDY          SET AT COMPANY LEVEL                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INTERNAL USE ONLY DEFAULT SETTING                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALIUSD  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT2,FF-(CPXIUSDO+CPXIUSDY)                                  
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@OFFC   TEST SET AT OFFICE                           
         BNE   VALIUSD2                                                         
         OI    CPXSTAT2,CPXIUSDO                                                
         B     EXITOK                                                           
*                                                                               
VALIUSD2 CLC   FVIFLD(1),AC@NO     SET AT COMPANY?                              
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTAT2,CPXIUSDY                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 2 EXTRA                                                 *         
* DATA OBJECT FOR SELF APPROVAL IN ESTIMATES                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SFAPE    NTRDO                                                                  
*                                                                               
SFAPETBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPSFAP)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSFAP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SELF APPROVAL IN ESTIMATES                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPSFAP LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPXSTAT2,CPXSFAPE          SET AT COMPANY LEVEL                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SELF APPROVAL IN ESTIMATES                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALSFAP  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT2,FF-CPXSFAPE                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
*                                                                               
         CLC   FVIFLD(1),AC@NO     SET AT COMPANY?                              
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@YES                                                 
         BNE   EXITNV                                                           
         TM    CPXSTAT1,CPXSEIAY+CPXSEIAO                                       
         BNZ   EXITNV              PREVENT SETTING IF INTERNAL APPROVAL         
         OI    CPXSTAT2,CPXSFAPE                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 12                                                      *         
* DATA OBJECT FOR TEAM LIST IN JOBS                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TEAJ     NTRDO                                                                  
*                                                                               
TEAJTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISTEAJ)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTEAJ)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 'TEAM LIST IN JOBS' SETTING                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISTEAJ  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@YES                                           
         TM    CPYSTATC,CPYSTEAJ   USING TEAM LIST IN JOBS                      
         BO    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 'TEAM LIST IN JOBS' SETTING                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALTEAJ  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN2Q                                                    
         BL    EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATC,FF-CPYSTEAJ MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATC,CPYSTEAJ   USING APPROVAL NOTICE BY EMAIL               
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* STATUS BYTE 11                                                      *         
* DATA OBJECT FOR APPROVAL NOTICE BY EMAIL                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ANBE     NTRDO                                                                  
*                                                                               
ANBETBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISANBE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALANBE)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 'APPROVAL NOTICE BY EMAIL' SETTING                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISANBE  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@YES                                           
         TM    CPYSTATB,CPYSANBE   USING APPROVAL NOTICE BY EMAIL               
         BO    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@NO                                           
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 'APPROVAL NOTICE BY EMAIL' SETTING                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALANBE  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATB,FF-CPYSANBE MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATB,CPYSANBE   USING APPROVAL NOTICE BY EMAIL               
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT ,                                                                
***********************************************************************         
* STATUS BYTE 11                                                      *         
* DATA OBJECT FOR EXCLUDE LOCKED CLIENTS AND 1N ACCTS                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
XC1N     NTRDO                                                                  
*                                                                               
XC1NTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISXC1N)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXC1N)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 'EXCLUDE LOCKED CLIENTS AND 1N ACCTS'                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DISXC1N  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         TM    CPYSTATB,CPYXLC1N                                                
         BO    EXITOK                                                           
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 'EXCLUDE LOCKED CLIENTS AND 1N ACCTS'                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VALXC1N  LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         CLI   CPYLN,CPYLN3Q                                                    
         BNH   EXITOK              OK - SHORT COMPANY ELEMENT                   
         NI    CPYSTATB,FF-CPYXLC1N MUST SET TO NO                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTATB,CPYXLC1N                                                
         B     EXITOK                                                           
         DROP  R1                                                               
***********************************************************************         
* STATUS BYTE 3 EXTRA                                                 *         
* DATA OBJECT FOR ACCESSING JOBS IN LIMIT LIST                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AJOB     NTRDO                                                                  
*                                                                               
AJOBTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPAJOB)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAJOB)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCESS JOBS IN LIMIT LIST                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPAJOB LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'BC@ALL),BC@ALL                                          
         TM    CPXSTAT3,CPXAJOBS                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCESS JOBS IN LIMIT LIST                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALAJOB  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         NI    CPXSTAT3,FF-CPXAJOBS                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@ALL    ALL                                          
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NONE   NONE OR                                      
         BNE   EXITNV                                                           
         OI    CPXSTAT3,CPXAJOBS                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 3 EXTRA                                                 *         
* DATA OBJECT FOR ACCESS MEDIA IN LIMIT LIST                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AMED     NTRDO                                                                  
*                                                                               
AMEDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPAMED)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAMED)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCESS MEDIA IN LIMIT LIST     S                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPAMED LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'BC@ALL),BC@ALL                                          
         TM    CPXSTAT3,CPXAMED                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCESS MEDIA IN LIMIT LIST                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALAMED  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         NI    CPXSTAT3,FF-CPXAMED                                              
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@ALL    ALL                                          
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NONE   NONE OR                                      
         BNE   EXITNV                                                           
         OI    CPXSTAT3,CPXAMED                                                 
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 3 EXTRA                                                 *         
* DATA OBJECT FOR ACCESS EXPENDITURE TYPES IN LIMIT LIST              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AETYP    NTRDO                                                                  
*                                                                               
AETYPTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPAETY)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAETY)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCESS EXPENDITURE TYPE IN LIMIT LIST                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPAETY LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'BC@ALL),BC@ALL                                          
         TM    CPXSTAT3,CPXAETYP                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCESS EXPENDITURE TYPE IN LIMIT LIST                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALAETY  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         NI    CPXSTAT3,FF-CPXAETYP                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@ALL    ALL                                          
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NONE   NONE OR                                      
         BNE   EXITNV                                                           
         OI    CPXSTAT3,CPXAETYP                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 3 EXTRA                                                 *         
* DATA OBJECT FOR ACCESSING 1N ACCOUNTS IN LIMIT LIST                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
A1NAC    NTRDO                                                                  
*                                                                               
A1NACTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPA1NA)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALA1NA)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCESS 1N ACCOUNTS IN LIMIT LIST                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPA1NA LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'BC@ALL),BC@ALL                                          
         TM    CPXSTAT3,CPXA1NAC                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCESS 1N ACCOUNTS IN LIMIT LIST                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALA1NA  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         NI    CPXSTAT3,FF-CPXA1NAC                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@ALL    ALL                                          
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NONE   NONE OR                                      
         BNE   EXITNV                                                           
         OI    CPXSTAT3,CPXA1NAC                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 3 EXTRA                                                 *         
* DATA OBJECT FOR ACCESSING 1R ACCOUNTS IN LIMIT LIST                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ASTAF    NTRDO                                                                  
*                                                                               
ASTAFTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPASTA)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALASTA)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCESS 1R ACCOUNTS IN LIMIT LIST                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPASTA LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'BC@ALL),BC@ALL                                          
         TM    CPXSTAT3,CPXASTAF                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCESS 1R ACCOUNTS IN LIMIT LIST                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALASTA  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         NI    CPXSTAT3,FF-CPXASTAF                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@ALL    ALL                                          
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NONE   NONE OR                                      
         BNE   EXITNV                                                           
         OI    CPXSTAT3,CPXASTAF                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 3 EXTRA                                                 *         
* DATA OBJECT FOR ACCESSING WORKCODES IN LIMIT LIST                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AWC      NTRDO                                                                  
*                                                                               
AWCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPAWC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAWC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCESS WORKCODES IN LIMIT LIST                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPAWC  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'BC@ALL),BC@ALL                                          
         TM    CPXSTAT3,CPXAWC                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCESS WORKCODES IN LIMIT LIST                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALAWC   LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         NI    CPXSTAT3,FF-CPXAWC                                               
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@ALL    ALL                                          
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NONE   NONE OR                                      
         BNE   EXITNV                                                           
         OI    CPXSTAT3,CPXAWC                                                  
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 3 EXTRA                                                 *         
* DATA OBJECT FOR ACCESSING ESTIMATE SCHEMES IN LIMIT LIST            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ASCHM    NTRDO                                                                  
*                                                                               
ASCHMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPASCH)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALASCH)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCESSING ESTIMATE SCHEMES IN LIMIT LIST                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPASCH LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'BC@ALL),BC@ALL                                          
         TM    CPXSTAT3,CPXASCHM                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCESSING ESTIMATE SCHEMES IN LIMIT LIST                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALASCH  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         NI    CPXSTAT3,FF-CPXASCHM                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@ALL    ALL                                          
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NONE   NONE OR                                      
         BNE   EXITNV                                                           
         OI    CPXSTAT3,CPXASCHM                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 3 EXTRA                                                 *         
* DATA OBJECT FOR ACCESSING SUPPLIER CODE PAGE IN LIMIT LIST          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ASUPP    NTRDO                                                                  
*                                                                               
ASUPPTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPASUP)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALASUP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCESSING ESTIMATE SCHEMES IN LIMIT LIST                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPASUP LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'BC@ALL),BC@ALL                                          
         TM    CPXSTAT4,CPXASUPP                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NONE),AC@NONE                                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCESSING ESTIMATE SCHEMES IN LIMIT LIST                   *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALASUP  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         NI    CPXSTAT4,FF-CPXASUPP                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@ALL    ALL                                          
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NONE   NONE OR                                      
         BNE   EXITNV                                                           
         OI    CPXSTAT4,CPXASUPP                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 4 EXTRA                                                 *         
* DATA OBJECT FOR SENDING AN IMMEDIATE EMAIL IN TIMESHEETS            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AETIM    NTRDO                                                                  
*                                                                               
AETIMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPETI)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSETI)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SEND IMMEDIATE EMAIL FOR TIMESHEETS                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPETI  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'AC@OFFC),AC@OFFC  TEST SET AT OFFICE                    
         TM    CPXSTAT4,CPXAETIO                                                
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         TM    CPXSTAT4,CPXAETIM          SET AT COMPANY LEVEL                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SEND IMMEDIATE EMAIL FOR TIMESHEETS                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALSETI  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT4,FF-(CPXAETIM+CPXAETIO)                                  
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@OFFC   OFFICE LEVEL SETTING?                        
         BNE   VALSET2                                                          
         OI    CPXSTAT4,CPXAETIO                                                
         B     EXITOK                                                           
*                                                                               
VALSET2  CLC   FVIFLD(1),BC@YES    SET AT COMPANY?                              
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
         OI    CPXSTAT4,CPXAETIM                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 4 EXTRA                                                 *         
* DATA OBJECT FOR SENDING AN IMMEDIATE EMAIL IN ORDERS                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AEORD    NTRDO                                                                  
*                                                                               
AEORDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPEORD)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEORD)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SEND IMMEDIATE EMAIL FOR ORDERS                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPEORD LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'AC@OFFC),AC@OFFC  TEST SET AT OFFICE                    
         TM    CPXSTAT4,CPXAEORO                                                
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         TM    CPXSTAT4,CPXAEORD          SET AT COMPANY LEVEL                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SEND IMMEDIATE EMAIL FOR ORDERS                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALSEORD LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT4,FF-(CPXAEORD+CPXAEORO)                                  
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@OFFC   OFFICE LEVEL SETTING?                        
         BNE   VALSEOR2                                                         
         OI    CPXSTAT4,CPXAEORO                                                
         B     EXITOK                                                           
*                                                                               
VALSEOR2 CLC   FVIFLD(1),BC@YES    SET AT COMPANY?                              
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
         OI    CPXSTAT4,CPXAEORD                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 4 EXTRA                                                 *         
* DATA OBJECT FOR SENDING AN IMMEDIATE EMAIL IN ESTIMATES             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AEEST    NTRDO                                                                  
*                                                                               
AEESTTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPEEST)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEEST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SEND IMMEDIATE EMAIL FOR ESTIMATES                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPEEST LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'AC@OFFC),AC@OFFC  TEST SET AT OFFICE                    
         TM    CPXSTAT4,CPXAEESO                                                
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         TM    CPXSTAT4,CPXAEEST          SET AT COMPANY LEVEL                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SEND IMMEDIATE EMAIL FOR ESTIMATES                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALSEEST LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT4,FF-(CPXAEEST+CPXAEESO)                                  
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@OFFC   OFFICE LEVEL SETTING?                        
         BNE   VALSEES2                                                         
         OI    CPXSTAT4,CPXAEESO                                                
         B     EXITOK                                                           
*                                                                               
VALSEES2 CLC   FVIFLD(1),BC@YES    SET AT COMPANY?                              
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
         OI    CPXSTAT4,CPXAEEST                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 4 EXTRA                                                 *         
* DATA OBJECT FOR NEW AURA EMAILS IN USE                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AURAE    NTRDO                                                                  
*                                                                               
AURAETBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPNAUR)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSNAUR)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NEW AURA EMAILS IN USE                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPNAUR LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    CPXSTAT4,CPXAURAE                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE NEW AURA EMAILS IN USE                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALSNAUR LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT4,FF-CPXAURAE                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTAT4,CPXAURAE                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 5 EXTRA                                                 *         
* DATA OBJECT FOR SENDING AN IMMEDIATE EMAIL IN EXPENSES              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AEEXP    NTRDO                                                                  
*                                                                               
AEEXPTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPEEXP)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEEXP)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SEND IMMEDIATE EMAIL FOR EXPENSES                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPEEXP LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'AC@OFFC),AC@OFFC  TEST SET AT OFFICE                    
         TM    CPXSTAT5,CPXAEEXO                                                
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         TM    CPXSTAT5,CPXAEEXP          SET AT COMPANY LEVEL                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SEND IMMEDIATE EMAIL FOR EXPENSES                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALSEEXP LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT5,FF-(CPXAEEXP+CPXAEEXO)                                  
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@OFFC   OFFICE LEVEL SETTING?                        
         BNE   VALSEEX2                                                         
         OI    CPXSTAT5,CPXAEEXO                                                
         B     EXITOK                                                           
*                                                                               
VALSEEX2 CLC   FVIFLD(1),BC@YES    SET AT COMPANY?                              
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
         OI    CPXSTAT5,CPXAEEXP                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 5 EXTRA                                                 *         
* DATA OBJECT FOR SENDING AN IMMEDIATE EMAIL IN JOBS                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AEJOB    NTRDO                                                                  
*                                                                               
AEJOBTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPEJOB)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEJOB)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
         SPACE 1                                                                
***********************************************************************         
* DISPLAY SEND IMMEDIATE EMAIL FOR JOBS                               *         
***********************************************************************         
         USING CPXELD,R1                                                        
DISPEJOB LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'AC@OFFC),AC@OFFC  TEST SET AT OFFICE                    
         TM    CPXSTAT5,CPXAEJOO                                                
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         TM    CPXSTAT5,CPXAEJOB          SET AT COMPANY LEVEL                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SEND IMMEDIATE EMAIL FOR JOBS                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALSEJOB LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT5,FF-(CPXAEJOB+CPXAEJOO)                                  
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@OFFC   OFFICE LEVEL SETTING?                        
         BNE   VALSEJOO                                                         
         OI    CPXSTAT5,CPXAEJOO                                                
         B     EXITOK                                                           
*                                                                               
VALSEJOO CLC   FVIFLD(1),BC@YES    SET AT COMPANY?                              
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
         OI    CPXSTAT5,CPXAEJOB                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 5 EXTRA                                                 *         
* DATA OBJECT FOR SENDING AN IMMEDIATE EMAIL IN INVOICES              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AEINV    NTRDO                                                                  
*                                                                               
AEINVTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPEINV)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEINV)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SEND IMMEDIATE EMAIL FOR INVOICES                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISPEINV LA    R1,SVCPXEL                SAVE COMPANY EXTRA ELEMENT             
*                                                                               
         MVC   FVIFLD(L'AC@OFFC),AC@OFFC  TEST SET AT OFFICE                    
         TM    CPXSTAT5,CPXAEINO                                                
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         TM    CPXSTAT5,CPXAEINV          SET AT COMPANY LEVEL                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SEND IMMEDIATE EMAIL FOR INVOICES                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALSEINV LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT5,FF-(CPXAEINV+CPXAEINO)                                  
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@OFFC   OFFICE LEVEL SETTING?                        
         BNE   VALSEIN2                                                         
         OI    CPXSTAT5,CPXAEINO                                                
         B     EXITOK                                                           
*                                                                               
VALSEIN2 CLC   FVIFLD(1),BC@YES    SET AT COMPANY?                              
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
         OI    CPXSTAT5,CPXAEINV                                                
         B     EXITOK                                                           
         DROP  R1                                                               
***********************************************************************         
* DATA OBJECT FOR HONOR LIMIT ACCESS FOR PROD/LIST                    *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  HONOR LIMITED ACCESS FOR PROD/LIST                    *         
***********************************************************************         
         SPACE 1                                                                
PLACC    NTRDO                                                                  
*                                                                               
PLACCTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DPLACC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VPLACC)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY HONOR LIMITED ACCESS FOR PROD SETTING                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DPLACC   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT9,CPYLACCP                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE HONOR LIMITED ACCESS FOR PROD SETTING                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VPLACC   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT9,FF-CPYLACCP                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT9,CPYLACCP                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* DATA OBJECT FOR POST MEDIA BILLING START OF DAY                     *         
* =FILE LABEL:  NO LABEL (NEW PROFILE)                                *         
* =AFM  LABEL:  POST MEDIA BILLING START OF DAY                       *         
***********************************************************************         
         SPACE 1                                                                
MBSOD    NTRDO                                                                  
*                                                                               
MBSODTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DMBSOD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VMBSOD)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY POST MEDIA BILLING START OF DAY SETTING                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
DMBSOD   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPYSTAT9,CPYMBSOD                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE POST MEDIA BILLING START OF DAY SETTING                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYELD,R1                                                        
VMBSOD   LA    R1,SVCPYEL          SAVE COMPANY ELEMENT                         
*                                                                               
         NI    CPYSTAT9,FF-CPYMBSOD                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         ZIC   R3,FVXLEN                                                        
         EXCLC R3,FVIFLD,AC@NO                                                  
         BE    EXITOK                                                           
         EXCLC R3,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPYSTAT9,CPYMBSOD                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* STATUS BYTE 8 EXTRA                                                 *         
* DATA OBJECT FOR RESOURCE MANAGEMENT IN USE                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RSRM     NTRDO                                                                  
*                                                                               
RSRMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRSRM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRSRM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RESOURCE MANAGEMENT IN USE                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISRSRM  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'AC@NO),AC@NO                                            
         TM    CPXSTAT8,CPXRSRMA                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RESOURCE MANAGEMENT IN USE                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALRSRM  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT8,FF-CPXRSRMA                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTAT8,CPXRSRMA                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 9 EXTRA                                                 *         
* DATA OBJECT FOR NEW AURA JOBS EMAILS                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AUJB     NTRDO                                                                  
*                                                                               
AUJBTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISAUJB)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAUJB)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AURA JOBS EMAILS                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISAUJB  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    CPXSTAT9,CPXAURJB                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AURA JOBS EMAILS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALAUJB  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT9,FF-CPXAURJB                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTAT9,CPXAURJB                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 9 EXTRA                                                 *         
* DATA OBJECT FOR NEW AURA ESTIMATES EMAIL                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AUES     NTRDO                                                                  
*                                                                               
AUESTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISAUES)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAUES)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AURA ESTIMATE EMAILS                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISAUES  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    CPXSTAT9,CPXAURES                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AURA ESTIMATE EMAILS                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALAUES  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT9,FF-CPXAURES                                             
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTAT9,CPXAURES                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 9 EXTRA                                                 *         
* DATA OBJECT FOR AURA ORDERS EMAILS                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AUOR     NTRDO                                                                  
*                                                                               
AUORTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISAUOR)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAUOR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AURA ORDERS EMAILS                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISAUOR  LA    R1,SVCPXEL                SAVE COMPANY EXTRA ELEMENT             
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    CPXSTAT9,CPXAUROR         TEST STATUS BIT IS SET                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AURA ORDERS EMAILS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALAUOR  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT9,FF-(CPXAUROR)                                           
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTAT9,CPXAUROR                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 9 EXTRA                                                 *         
* DATA OBJECT FOR AURA EXPENSE EMAILS                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AUEX     NTRDO                                                                  
*                                                                               
AUEXTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISAUEX)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAUEX)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AURA EXPENSE EMAILS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISAUEX  LA    R1,SVCPXEL                SAVE COMPANY EXTRA ELEMENT             
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    CPXSTAT9,CPXAUREX         TEST STATUS BIT IS SET                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AURA EXPENSE EMAILS                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALAUEX  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT9,FF-(CPXAUREX)                                           
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTAT9,CPXAUREX                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE 9 EXTRA                                                 *         
* DATA OBJECT FOR AURA INVOICE EMAILS                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AUIV     NTRDO                                                                  
*                                                                               
AUIVTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISAUIV)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAUIV)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AURA INVOICE EMAILS                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISAUIV  LA    R1,SVCPXEL                SAVE COMPANY EXTRA ELEMENT             
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    CPXSTAT9,CPXAURIV         TEST STATUS BIT IS SET                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AURA INVOICE EMAILS                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALAUIV  LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTAT9,FF-(CPXAURIV)                                           
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTAT9,CPXAURIV                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* STATUS BYTE A EXTRA                                                 *         
* DATA OBJECT FOR USE FEDERATED SECURITY IN AURA                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FEDA     NTRDO                                                                  
*                                                                               
FEDATBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFED)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFED)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY USE FEDERATED SECURITY IN AURA                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISFED   LA    R1,SVCPXEL                SAVE COMPANY EXTRA ELEMENT             
*                                                                               
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         TM    CPXSTATA,CPXFEDAT         TEST STATUS BIT IS SET                 
         BNZ   EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE USE FEDERATED SECURITY IN AURA                             *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALFED   LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
         MVC   AFEDFLD,FVADDR                                                   
*                                                                               
         NI    CPXSTATA,FF-(CPXFEDAT)                                           
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTATA,CPXFEDAT                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*MN DSRD-24361                                                                  
***********************************************************************         
* STATUS BYTE A EXTRA                                                 *         
* DATA OBJECT FOR ESTIMATE RICH TEXT SUPPORT                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ESRT     NTRDO                                                                  
*                                                                               
ESRTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISERT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALERT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ESTIMATE RICH TEXT SUPPORT                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
DISERT   LA    R1,SVCPXEL                SAVE COMPANY EXTRA ELEMENT             
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    CPXSTATA,CPXESTRT         TEST STATUS BIT IS SET                 
         BNO   EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ESTIMATE RICH TEXT SUPPORT                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R1                                                        
VALERT   LA    R1,SVCPXEL          SAVE COMPANY EXTRA ELEMENT                   
*                                                                               
         NI    CPXSTATA,FF-(CPXESTRT)                                           
         CLI   FVILEN,0            NO INPUT - DEFAULT IS NO                     
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    CPXSTATA,CPXESTRT                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
*MN DSRD-24361                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING DISTANCE RERESH DATE                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DRDT     NTRDO                                                                  
*                                                                               
DRDTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDRDT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDRDT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISTANCE REFRESH DATE                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R4                                                        
DISDRDT  LA    R4,SVCPXEL          SAVE COMPANY ELEMENT                         
         OC    CPXDRDTE,CPXDRDTE                                                
         BZ    EXITOK                                                           
         XC    BOWORK1(6),BOWORK1                                               
         MVI   BOWORK1,X'B0'       SET DUMMY YEAR                               
         MVC   BOWORK1+1(2),CPXDRDTE                                            
         GOTO1 VDATCON,BOPARM,(1,BOWORK1),(16,FVIFLD)                           
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DISTANCE REFRESH DATE                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPXELD,R4                                                        
VALDRDT  LA    R4,SVCPXEL          SAVE COMPANY ELEMENT                         
         XC    CPXDRDTE,CPXDRDTE                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   BOWORK1(12),BCSPACES                                             
         MVC   BOWORK1(5),FVIFLD      MOVE REFRESH DATE DDMMM                   
         GOTO1 VDATVAL,BOPARM,(X'01',BOWORK1),BOWORK1                           
         CLI   3(R1),0                                                          
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT) INVALID DATE FORMAT                       
         B     EXITL                                                            
         MVC   BOWORK1(2),=C'10'      INSERT DUMMY YEAR                         
         GOTO1 VDATCON,BOPARM,(0,BOWORK1),(1,BOWORK1+10)                        
         MVC   CPXDRDTE,BOWORK1+11  DISTANCE REFRESH DATE                       
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT ,                                                                
FIL1B    CSECT                                                                  
         EJECT ,                                                                
*                                                                               
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
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO A LIST RECORD                
         BNE   EXITOK                                                           
         CLI   SREC,R#CPY          COMPANY RECORD                               
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (OUT)                    *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (BACK)                   *         
***********************************************************************         
         SPACE 1                                                                
XITIN    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
LIST     DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
         LR    R6,RF                                                            
         USING LIST,R6                                                          
         LM    R0,R3,SVPARMS                                                    
THIS     USING CPYRECD,R2                                                       
LAST     USING CPYRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     DS    0H                                                               
*                                                                               
         MVC   IOKEY(L'ACTKEY),THIS.CPYRECD                                     
         LHI   R1,XOHIGH+XOACCDIR+XIO11                                         
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
TEMP     USING CPYRECD,IOKEY                                                    
NLST     XR    RF,RF                                                            
         IC    RF,TEMP.CPYKCPY                                                  
         LA    RF,1(RF)                                                         
         STC   RF,TEMP.CPYKCPY                                                  
         MVC   IOKEY,BCSPACES                                                   
*                                                                               
         CLM   RF,1,=XL1'FF'       ENSURE DON'T OVERSHOOT                       
         BE    EXITL                                                            
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO11                                         
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLI   TEMP.CPYKCPY,C' '                                                
         BNH   NLST                REACHED COMPANIES YET?                       
         CLC   TEMP.CPYKEY+CPYKEND(L'CPYKEY-1),BCSPACES                         
         BNE   NLST                IS REST OF KEY SPACES                        
*                                                                               
         MVC   THIS.CPYKEY(ACCKLEN),IOKEY WANT THIS KEY                         
         B     EXITOK                                                           
         DROP  THIS,LAST,TEMP                                                   
         SPACE 2                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
MONLST   DC    C'123456789ABC'                                                  
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
DCLIST   DCDDL AC#YES,4,L                                                       
         DCDDL AC#NO,4,L                                                        
         DCDDL AC#NONE,4,L                                                      
         DCDDL AC#OLIST,5,L                                                     
         DCDDL AC#REG2,3,L                                                      
         DCDDL AC#EFF3,3,L                                                      
         DCDDL AC#ALL,3,L                                                       
         DCDDL AC#OFFC,3,L                                                      
         DCDDL AC#OFLST,3,L                                                     
         DCDDL AC#VEND,4,L                                                      
         DCDDL AC#LESS,4,L                                                      
         DCDDL AC#NFNA,5,L                                                      
DCLISTX  DC    X'00'                                                            
         SPACE 2                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*        ACFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
SVADDR   DS    A                                                                
SVADDR2  DS    A                                                                
APASSW   DS    A                   A(PASSWORD FIELD)                            
SVPASSW  DS    CL3                 PASSWORD                                     
SVCUID   DS    CL(L'CPYUID)        PRINCIPLE ID                                 
SVCLOGO  DS    CL(L'CPYLOGO)       COMPANY LOGO                                 
SVCALPHA DS    CL(L'CPYALPHA)      COMPANY ALPHA                                
SVTMSSD  DS    CL(L'CPYTMSSD)      TMS START DATE                               
SVPRSTL  DS    CL(L'CPYPRSTL)      SAVED PRESTO LEDGER FIELDS                   
SVXSUPP  DS    CL1                 SAVED EXTRA SUPPLIER                         
SVCPYEL  DS    CL(CPYLN4Q)         SAVED COMPANY ELEMENT                        
SVCPXEL  DS    CL(CPXLNQ)          SAVED COMPANY EXTRA ELEMENT                  
SVPIDBIN DS    XL(L'PIDNO)         BINARY PID NUMBER                            
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
         SPACE 2                                                                
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
*                                                                               
AFEDFLD  DS    A                   A(FEDERATED AUTH FIELD)                      
AMOAFLD  DS    A                   A(G/L MOA FIELD)                             
*                                                                               
DSLISTU  DS    0F                                                               
AC@YES   DS    CL4                 YES                                          
AC@NO    DS    CL4                 NO                                           
AC@NONE  DS    CL4                 NONE                                         
AC@OLIST DS    CL5                 OLIST                                        
AC@REG2  DS    CL3                 REG                                          
AC@EFF3  DS    CL3                 EFF                                          
AC@ALL   DS    CL3                 ALL                                          
AC@OFFC  DS    CL3                 OF                                           
AC@OFLST DS    CL3                 OL                                           
AC@VEND  DS    CL4                 VEND                                         
AC@LESS  DS    CL4                 LESS                                         
AC@NFNA  DS    CL5                 (N/A)                                        
*                                                                               
BLOCK    DS    10XL(SCBLKLQ)                                                    
OVERWRKN EQU   *-OVERWRKD                                                       
         SPACE 2                                                                
***********************************************************************         
* OTHER DSECTS                                                                  
***********************************************************************         
         SPACE 1                                                                
LDGD     DSECT                                                                  
LDGUL    DS    CL2                 UNIT/LEDGER TO VALIDATE                      
LDGSTA1  DS    XL1                                                              
LDGLVL   EQU   X'80'               MAKE SURE THIS LDGR IS > 1 LEVEL             
LDGLNQ1  EQU   *-LDGD                                                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035ACFIL1B   03/20/20'                                      
         END                                                                    
