*          DATA SET ACFIL43    AT LEVEL 056 AS OF 12/15/16                      
*PHASE T62343B                                                                  
                                                                                
FIL43    TITLE 'ETYPE RECORD (EBUYER)'                                          
                                                                                
* YNGX 002 06JAN06 <LO01-5056> NEW FIELD - APPROVAL NEEDED                      
* NSHE 003 15MAR06 <1030315> BUG FIX TO CATEGORY                                
* TKLU 004 23AUG06 <LO01-5729> FOREIGN NAME FIELD FOR GERMANY                   
* TKLU 005 05FEB07 LVL **4 BUG FIX                                              
* JFOS 006 21MAR07 <DU01-6151> ALLOW UP TO 102 SUPPLIERS/6 LIDELS               
* TKLU 007 07JAN08 <LO01-6456> APPLICATION LOCKS (INV/EXP/ORD)                  
* TKLU 008 11MAR08 <LO01-6456> BUG FIX TO PREVIOUS                              
* MPEN 009 12JUN09 <LO01-8967> ADD NEW FIELD TO ETYPE RECORD                    
* MPEN     12JUN09 <LO01-9013> CHANGE KEY TO INCLUDE OFFICE/OFFICE LIST         
* MPEN 010 10AUG09 FIX FOR ADDITIONAL SCREEN                                    
* YNGX 011 23JUL09 <LO01-8677> NEW DOWNLOAD ACTION                              
* SMAN 012 21JUL10 <BR34705L> ALLOW MULTIPLE LIDTEXPS TYPES                     
* TKLU 015 04JUN10 <PR000250> GERMANY: ALLOW HIGH LEVEL SUPPLIERS               
* JFOS 016 22OCT10 <PR000995> USE PADDLE TO GENERATE PASSIVES                   
* JFOS 017 07JAN11 <BR34897L> TWEAK MULTIPLE LIDTEXPS                           
* NRAK 018 19JAN11 <BR39265L> DON'T CALL ACSRCHDIR IF ADDING                    
* NSHE 018 11FEB11 ALLOW LEVEL 15 CHANGE FOR UK                                 
* YNGX 019 31JAN11 <PR001426> RELINK TO INCLUDE NEW TLSTD                       
* TKLU 020 25AUG11 <PR002061> GERMANY: ALLOW HIGH LEVEL EXPENSE A/CS            
* NRAK 021 01MAY13 <BR55416L> ADDRESSING FIX TO STOP CRAPPER                    
* MPEN 022 06JUN13 <PR003554> RELINK FOR NEW GEFILWORK                          
* NSHE 023 22NOV16 <DSRD-13979> ADD CANADIA GST AND PROVINCE FIELDS             
                                                                                
FIL43    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL43**,RA,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
         L     R6,ATWA                                                          
         MVC   SESNL,TWASESNL-TWAD(R6)                                          
         AH    R6,=Y(TWUSER-TWAD)                                               
         USING SAVED,R6                                                         
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
EXIT     L     R1,CALLR1           RETURN PARAMETERS TO CALLER                  
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
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
EXITOFF  MVC   FVMSGNO,=AL2(AE$EXPNV) EXPENDITURE TYPE NOT VALID FOR            
         B     EXITL                  THIS OFFICE                               
EXITEXS  MVC   FVMSGNO,=AL2(AE$EXPEX) EXPENDITURE TYPE CODE EXISTS ON           
         B     EXITL                  OTHER OFFICE                              
EXITNOL  MVC   FVMSGNO,=AL2(AE$NLIMA) NOT LIMIT ACCESS LOGON                    
         B     EXITL                                                            
EXIT2CH  MVC   FVMSGNO,=AL2(AE$OFFN2) OFFICE MUST BE 2 CHARACTERS               
         B     EXITL                                                            
EXITOFNV MVC   FVMSGNO,=AL2(AE$OFFNV) NOT A VALID OFFICE/OFFICE LIST            
         B     EXITL                                                            
*                                                                               
EXITIACT MVC   FVMSGNO,=AL2(AE$IACTS)   INVALID ACTION FOR THIS SCREEN          
         LH    RF,GSDSPACT         SET CURSOR TO ACTION FIELD                   
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
INIT     GOTO1 VDICTAT,BOPARM,C'LL ',DCLISTL,DSLISTL                            
                                                                                
         MVC   SVOFF,BCSPACES                                                   
         CLI   CSACT,A#DLOAD       DOWNLOAD?                                    
         BNE   *+8                                                              
         MVI   WHENOK,WHENOV+WHENSOON+WHENNOW  NOTIFY VALID INPUTS              
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
         DC    AL1(ODLOAD),AL1(0,0,0),AL4(DLOAD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
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
SCRKSET  MVI   GSSKCODE,0                                                       
         TM    BCCPYST1,CPYSOROE   ARE WE ON OFFICE BASED AGENCY                
         BZ    EXITOK                                                           
*&&UK                                                                           
         TM    BCCPYST5,CPYSNVAT   COMPANY USE NEW VAT RULES                    
         BZ    SCRKSET2                                                         
         MVI   GSSKCODE,C'2'       CHANGE KEY SCREEN CODE                       
         B     EXITOK                                                           
*&&                                                                             
*                                                                               
SCRKSET2 MVI   GSSKCODE,C'3'                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET MAINTENANCE DATA SCREEN                                         *         
***********************************************************************         
         SPACE 1                                                                
SCRMSET  MVI   GSSMCODE,0                                                       
*&&UK                                                                           
         TM    BCCPYST5,CPYSNVAT   COMPANY USE NEW VAT RULES                    
         BZ    SCRMSET2                                                         
         MVI   GSSMCODE,C'1'       CHANGE MAINTENANCE PAGE CODE                 
         TM    BCCPYST1,CPYSOROE   ARE WE ON OFFICE BASED AGENCY                
         BZ    EXITOK                                                           
         MVI   GSSMCODE,C'2'                                                    
         B     EXITOK                                                           
*&&                                                                             
*                                                                               
SCRMSET2 TM    BCCPYST1,CPYSOROE   ARE WE ON OFFICE BASED AGENCY                
         BZ    EXITOK                                                           
         MVI   GSSMCODE,C'3'       CHANGE MAINTENANCE PAGE CODE                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET LIST SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLSET  MVI   GSSLCODE,0                                                       
         TM    BCCPYST1,CPYSOROE   ARE WE ON OFFICE BASED AGENCY                
         BZ    EXITOK                                                           
*&&UK                                                                           
         TM    BCCPYST5,CPYSNVAT   COMPANY USE NEW VAT RULES                    
         BZ    SCRLSET2                                                         
         MVI   GSSLCODE,C'2'       CHANGE MAINTENANCE PAGE CODE                 
         B     EXITOK                                                           
*&&                                                                             
*                                                                               
SCRLSET2 MVI   GSSLCODE,C'3'                                                    
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
         USING ETYRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
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
KFKVAL   XC    ETYKEY,ETYKEY                                                    
         MVI   ETYKTYP,ETYKTYPQ    EXPENDITURE TYPE RECORD TYPE                 
         MVI   ETYKSUB,ETYKSUBQ    AND SUBTYPE                                  
         MVC   ETYKCPY,CUABIN      CONNECTED USER                               
*                                                                               
         XC    SVFLTS(SVFLTLQ),SVFLTS FOR DOWNLOADING                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    ETYKEY,ETYKEY                                                    
         MVI   ETYKTYP,ETYKTYPQ    EXPENDITURE TYPE RECORD TYPE                 
         MVI   ETYKSUB,ETYKSUBQ    AND SUBTYPE                                  
         MVC   ETYKCPY,CUABIN                                                   
*                                                                               
         XC    SVFLTS(SVFLTLQ),SVFLTS                                           
         B     EXITOK                                                           
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
         USING ETYRECD,R2                                                       
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
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD OR COPY                          *         
***********************************************************************         
         SPACE 1                                                                
RFADD    CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
         CLI   GSSMPAGE,1                                                       
         BNE   EXITIACT                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD OR COPY                          *         
***********************************************************************         
         SPACE 1                                                                
RFWRT    GOTO1 DELPAS,BOPARM,ETYRECD                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
RECLAST  L     R1,SVPARMS4                                                      
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE 1                                                                
RLADD    GOTO1 ADDPAS,BOPARM,ETYRECD                                            
         GOTO1 VACSRCHP,BOPARM,C'AT  ',ETYRECD,GSRECDA,0,ACOM,0                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE 1                                                                
RLDEL    GOTO1 DELPAS,BOPARM,ETYRECD                                            
         GOTO1 VACSRCHP,BOPARM,C'DT  ',ETYRECD,GSRECDA,0,ACOM,0                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE 1                                                                
RLWRT    GOTO1 ADDPAS,BOPARM,ETYRECD                                            
         GOTO1 VACSRCHP,BOPARM,C'CT  ',ETYRECD,GSRECDA,0,ACOM,0                 
         B     EXITOK                                                           
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
         USING ETYRECD,R2                                                       
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
         USING ETYRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                         *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(F#ETY#ETCOD),AL4(ETC)     EXP. TYPE CODE                     
         DC    AL2(F#ETY#ETCODK),AL4(ETC)    EXP. TYPE CODE (DOWNLOAD)          
*                                            EXP. TYPE NAME - GEFIL04           
         DC    AL2(F#ETY#ETSLK),AL4(SLK)     LOCKED STATUS                      
         DC    AL2(F#ETY#ETSLKK),AL4(SLK)    LOCKED STATUS (DOWNLOAD)           
         DC    AL2(F#ETY#BILL),AL4(BIL)      BILLABLE STATUS                    
         DC    AL2(F#ETY#BILLK),AL4(BIL)     BILLABLE STATUS (DOWNLOAD)         
         DC    AL2(F#ETY#NBILL),AL4(NBL)     NON-BILLABLE STATUS                
         DC    AL2(F#ETY#NBILLK),AL4(NBL)    NON-BILLABLE (DOWNLOAD)            
         DC    AL2(F#ETY#ADVNC),AL4(ADV)     ADVANCE STATUS                     
         DC    AL2(F#ETY#ADVNCK),AL4(ADV)    ADVANCE STATUS (DOWNLOAD)          
         DC    AL2(F#ETY#APPND),AL4(APN)     APPROVAL NEEDED FOR NON-BI         
         DC    AL2(F#ETY#APPNDK),AL4(APN)    APPROVAL NEEDED (DOWNLOAD)         
         DC    AL2(F#ETY#EXCAT),AL4(CAT)     EXPENSE CATEGORY                   
         DC    AL2(F#ETY#EXCATK),AL4(CAT)    EXPENSE CATEGORY(DOWNLOAD)         
         DC    AL2(F#ETY#FNAME),AL4(FNM)     FOREIGN NAME                       
         DC    AL2(F#ETY#VATCD),AL4(VATC)    VAT CODE                           
*&&UK*&& DC    AL2(F#ETY#VATAC),AL4(VATA)    VAT ACCOUNT                        
*&&UK*&& DC    AL2(F#ETY#VATNM),AL4(VATN)    VAT NAME                           
         DC    AL2(F#ETY#ETDEF),AL4(DFT)     DEFAULT INDICATOR DATA             
         DC    AL2(F#ETY#ETWCL),AL4(WCL)     WORKCODE LIST                      
         DC    AL2(F#ETY#ETXAL),AL4(XAL)     EXPENSE ACCOUNT LIST               
         DC    AL2(F#ETY#ETSAL),AL4(SAL)     SUPPLIER ACCOUNT LIST              
         DC    AL2(F#ETY#ETWCN),AL4(WCN)     WORKCODE NAME LIST                 
         DC    AL2(F#ETY#ETWCD),AL4(WCD)     WORKCODE DESCRIPTION LIST          
         DC    AL2(F#ETY#ETXAN),AL4(XAN)     EXPENSE ACC NAME LIST              
         DC    AL2(F#ETY#ETSAN),AL4(SAN)     SUPPLIER ACC NAME LIST             
         DC    AL2(F#ETY#ALPEX),AL4(LEX)     LOCK EXPENSES STATUS               
         DC    AL2(F#ETY#ALPEXK),AL4(LEX)    LOCK EXPENSES (DOWNLOAD)           
         DC    AL2(F#ETY#ALPOR),AL4(LOR)     LOCK ORDERS STATUS                 
         DC    AL2(F#ETY#ALPORK),AL4(LOR)    LOCK ORDERS (DOWNLOAD)             
         DC    AL2(F#ETY#ALPIN),AL4(LIN)     LOCK INVOICES STATUS               
         DC    AL2(F#ETY#ALPIVK),AL4(LIN)    LOCK INVOICES (DOWNLOAD)           
         DC    AL2(F#ETY#SFAP),AL4(SFAP)     SELF APPROVAL ALLOWED              
         DC    AL2(F#ETY#SFAPK),AL4(SFAP)    SELF APPROVAL (DOWNLOAD)           
         DC    AL2(F#ETY#OFF),AL4(OFFL)      OFFICE/OFFICE LIST CODE            
*                                                                               
         DC    AL2(F#ETY#ETCODD),AL4(ETCD)   EXP TYPE CODE (DOWNLOAD)           
         DC    AL2(F#ETY#ETND),AL4(ETND)     EXP TYPE NAME (DOWNLOAD)           
         DC    AL2(F#ETY#OFFD),AL4(OFLD)     OFF/OFF LIST (DOWNLOAD)            
         DC    AL2(F#ETY#ADVNCD),AL4(ADVD)   ADVANCE (DOWNLOAD)                 
         DC    AL2(F#ETY#APPNDD),AL4(APND)   APPROVAL NEEDED (DOWNLOAD)         
         DC    AL2(F#ETY#FNAMED),AL4(FNMD)   FOREIGN NAME (DOWNLOAD)            
         DC    AL2(F#ETY#ALPIND),AL4(LIND)   LOCK INVOICES (DOWNLOAD)           
         DC    AL2(F#ETY#ALPORD),AL4(LORD)   LOCK ORDERS (DOWNLOAD)             
         DC    AL2(F#ETY#ALPEXD),AL4(LEXD)   LOCK EXPENSES (DOWNLOAD)           
         DC    AL2(F#ETY#ETSLKD),AL4(SLKD)   LOCK STATUS (DOWNLOAAD)            
         DC    AL2(F#ETY#EXCATD),AL4(CATD)   EXP CATEGORY (DOWNLOAD)            
*&&UK*&& DC    AL2(F#ETY#VATCDD),AL4(VCDD)   VAT CODE:NEW VAT(DOWNLOAD)         
*&&UK*&& DC    AL2(F#ETY#VATACD),AL4(VACD)   VAT ACCOUNT (DOWNLOAD)             
*&&UK*&& DC    AL2(F#ETY#VATNMD),AL4(VNMD)   VAT NAME (DOWNLOAD)                
         DC    AL2(F#ETY#BILLD),AL4(BILD)    BILLABLE (DOWNLOAD)                
         DC    AL2(F#ETY#NBILLD),AL4(NBLD)   NON-BILLABLE (DOWNLOAD)            
         DC    AL2(F#ETY#SFAPD),AL4(SAPD)    SELF AP ALLOWED (DOWNLOAD)         
         DC    AL2(F#ETY#EXPDFD),AL4(EXDD)   DEFAULT EXP  (DOWNLOAD)            
         DC    AL2(F#ETY#WCDDFD),AL4(WCDD)   DEFAULT WC (DOWNLOAD)              
*&&US*&& DC    AL2(F#ETY#PRVC),AL4(PRVC)     PROVINCE CODE LIST                 
*&&US*&& DC    AL2(F#ETY#PRNM),AL4(PRVN)     PROVINCE NAME LIST                 
*&&US*&& DC    AL2(F#ETY#PSTC),AL4(PSTC)     PST CODE LIST                      
*&&US*&& DC    AL2(F#ETY#PSTN),AL4(PSTN)     PST NAME LIST                      
*&&US*&& DC    AL2(F#ETY#PRVD),AL4(PRVC)     PROVINCE CODE LIST (DOWN)          
*&&US*&& DC    AL2(F#ETY#PRNMD),AL4(PRVN)    PROVINCE NAME LIST (DOWN)          
*&&US*&& DC    AL2(F#ETY#PSTCD),AL4(PSTC)    PST CODE LIST (DOWNLOAD)           
*&&US*&& DC    AL2(F#ETY#PSTND),AL4(PSTN)    PST NAME LIST (DOWNLOAD)           
         DC    AL2(EOT)                                                         
*                                                                               
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
FIL43    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
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
DFDDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   DS    0H                                                               
         CLI   CSACT,A#ADD                                                      
         JE    DFDVL10                                                          
         GOTO1 DELPAS,BOPARM,ETYRECD                                            
DFDVL10  CLM   R2,7,ATLST+1        TEST PROCESSING LIST 1 (NOT LIST 0)          
         BNE   EXITOK                                                           
         OI    WCLBLK,X'40'        RESET ANY 'DEFAULT' INDS SET BY LIST         
         OI    XALBLK,X'40'        BLOCKS NOW USED FOR DUP CHECKING             
         OI    SALBLK,X'40'                                                     
         B     EXITOK                                                           
         EJECT ,                                                                
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
DLDVAL   B     EXITOK                                                           
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
FIL43N   CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXPENDITURE TYPE CODE                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ETC      NTRDO                                                                  
*                                                                               
ETCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISETC)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISETC)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETETC)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTETC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALETC)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTETC)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTETC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHETC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN EXPENDITURE TYPE CODE                                    *         
***********************************************************************         
         SPACE 1                                                                
DISETC   MVC   FVIFLD(L'ETYKCODE),ETYKCODE                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT EXPENDITURE FIELD ON NTRSES                               *         
***********************************************************************         
         SPACE 1                                                                
DSETETC  DS    0H                                                               
         B     FLTXX               RETURN 'NOT VALID' TO FORCE UNPROT           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN EXPENDITURE TYPE CODE                                   *         
***********************************************************************         
         SPACE 1                                                                
VALETC   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTETC                                                          
         CLI   CSACT,A#ADD         ADDING                                       
         BNE   VALETC2                                                          
         GOTO1 ACHKFLD,BOPARM,('CKKEYQ',CKTAB1Q)                                
         BNE   EXITNV              INVALID                                      
         CLI   FVILEN,L'ETYKCODE                                                
         BH    EXITLONG                                                         
*                                                                               
T        USING ETYRECD,IOKEY                                                    
VALETC2  CLI   GSSKCODE,C'2'       DO WE HAVE OFFICE BASED AGENCY?              
         BE    *+12                                                             
         CLI   GSSKCODE,C'3'                                                    
         BNE   VALETC24                                                         
         CLI   CSACT,A#CHA                                                      
         BE    VALETC08                                                         
         CLI   CSACT,A#DIS                                                      
         BE    VALETC08                                                         
         CLI   CSACT,A#ADD                                                      
         BE    VALETC22                                                         
         B     VALETC26                                                         
*                                                                               
VALETC08 XC    IOKEY,IOKEY          MAKE SURE EXPENDITURE CODE IS               
         MVI   T.ETYKTYP,ETYKTYPQ   VALID FOR THIS LOGON                        
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CUABIN                                                 
         MVC   T.ETYKCODE,FVIFLD                                                
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEY(ETYKOFFC-ETYRECD),IOKEYSAV                                 
         BNE   EXITNV             EXP TYP NOT VALID                             
         CLI   CUACCS,0           GLOBAL LOGON?                                 
         BE    VALETC26           THEN CAN VIEW IT                              
         CLC   T.ETYKOFFC,BCSPACES GLOBAL EXPENDITURE TYPE? IF SO ALLOW         
         BNH   VALETC26           USER TO VIEW IT AND EDIT IT                   
*                                                                               
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   VALETC18                                                         
         CLI   CUACCS,C'$'         IS IT OFFICE LIST LOGON?                     
         BNE   VALETC10                                                         
         CLI   T.ETYKOFFC,C'$'     OFFICE LIST EXPENDITURE TYPE                 
         BNE   VALETC10                                                         
         CLC   CUACCS(2),T.ETYKOFFC CHECK WHETHER OFFICE LIST MATCHES           
         BE    VALETC26                                                         
         B     EXITOFF             EXPENDITURE TYPE NOT VALID ON THIS           
*                                  LOGON                                        
X        USING OFLPASD,IOKEY                                                    
VALETC10 CLI   T.ETYKOFFC,C'$'                                                  
         BNE   VALETC16                                                         
         MVC   SVIOKEY,IOKEY                                                    
         XC    X.OFLPAS,X.OFLPAS    CHECK WHETHER OFFICE IS PART OF             
         MVI   X.OFLPTYP,OFLPTYPQ   OFFICE LIST                                 
         MVC   X.OFLPCPY,CUABIN                                                 
         MVI   X.OFLPSUB,OFLPSUBQ                                               
         MVC   X.OFLPOFF,CUACCS+1  STORE 1 CHARACTER OFFICE                     
         L     R1,=AL4(XOHI+XOACCDIR+XIO2)                                      
         GOTO1 AIO                                                              
         BE    VALETC14                                                         
         DC    H'0'                                                             
*                                                                               
VALETC12 L     R1,=AL4(XIO2+XOACCDIR+XOSEQ)                                     
         GOTOR AIO                                                              
*                                                                               
VALETC14 CLC   IOKEYSAV(OFLPOFL-OFLPASD),IOKEY                                  
         BNE   EXITOFF             EXPENDITURE TYPE NOT VALID ON THIS           
         LA    RF,SVIOKEY          OFFICE                                       
         CLC   ETYKOFFC-ETYRECD(L'ETYKOFFC,RF),X.OFLPOFL                        
         BE    VALETC26            OFFICE LIST CODE VALID?                      
         B     VALETC12                                                         
         DROP  X                                                                
*                                                                               
VALETC16 GOTO1 ATSTOFF,T.ETYKOFFC  CHECK OFFICE VALID FOR THIS LOGON            
         BNE   EXITOFF                                                          
         B     VALETC26                                                         
*                                                                               
X        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
VALETC18 MVC   SVIOKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   X.OFFKTYP,OFFKTYPQ                                               
         MVC   X.OFFKCPY,CUABIN                                                 
         MVC   X.OFFKOFF,CUACCS+2                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    X.OFFKSTAT,OFFSLIST  OFFICE LIST?                                
         BZ    VALETC20             NO THEN VALIDATE OFFICE                     
         MVC   IOKEY,SVIOKEY                                                    
         CLC   CUACCS+2(2),T.ETYKOFFC CHECK WHETHER MATCH ON                    
         BE    VALETC26             OFFICE LIST                                 
*                                                                               
VALETC20 MVC   IOKEY,SVIOKEY       CHECK OFFICE IS VALID ON THIS LOGON          
         GOTO1 ATSTOFF,T.ETYKOFFC                                               
         BNE   EXITOFF                                                          
         B     VALETC26                                                         
*                                  ON THIS OFFICE                               
VALETC22 XC    IOKEY,IOKEY         FOR ADD CHECK WHETHER ETYPE ALREADY          
         MVI   T.ETYKTYP,ETYKTYPQ  EXISTS                                       
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CUABIN                                                 
         MVC   T.ETYKCODE,FVIFLD                                                
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEY(ETYKOFFC-ETYRECD),IOKEYSAV                                 
         BE    EXITEXS            EXP TYP ALREADY EXISTS ON OTHER OFF           
         B     VALETC24                                                         
*                                                                               
VALETC24 MVC   ETYKOFFC,BCSPACES NON OFFICE-BASED AGENCY EXIT                   
         MVC   ETYKCODE,FVIFLD                                                  
         B     VALETCX                                                          
*                                                                               
VALETC26 MVC   ETYKOFFC,BCSPACES OFFICE BASED AGENCY EXIT                       
         MVC   ETYKCODE,FVIFLD                                                  
         MVC   ETYKOFFC,T.ETYKOFFC                                              
VALETCX  B     EXITOK                                                           
         DROP  T,X                                                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN EXPENDITURE TYPE FILTER FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DFLTETC  MVC   FVIFLD(L'ETYKCODE),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN EXPENDITURE TYPE FILTER FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
VFLTETC  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         CLI   FVILEN,L'ETYKCODE                                                
         BH    EXITLONG                                                         
                                                                                
         MVC   ETYKCODE,FVIFLD                                                  
         MVC   SVFETCL,FVILEN                                                   
         MVC   SVFETC,ETYKCODE                                                  
         MVC   FLTIFLD(L'ETYKCODE),ETYKCODE                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON AN EXPENDITURE TYPE CODE                            *         
* OVERLAY WILL DO ITS OWN FILTERING FOR DOWNLOADING - SEE DOFLT       *         
***********************************************************************         
         SPACE 1                                                                
DOFTETC  CLC   FLTIFLD,BCSPACES                                                 
         BNH   FLTXE               ALL BCSPACES - OK                            
         LA    RF,FLTIFLD+2        GET THE SIGNIFICANT LENGTH                   
         LA    RE,L'ETYKCODE-1     -1 FOR EX                                    
         CLI   0(RF),C' '                                                       
         BH    *+12                                                             
         SHI   RF,1                                                             
         BCT   RE,*-12                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   ETYKCODE(0),FLTIFLD                                              
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         B     FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON EXPENDITURE TYPE NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHETC  DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,EXPTYP,ACOM,0           
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR LOCKED STATUS                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SLK      NTRDO                                                                  
*                                                                               
SLKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSLK)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSLK)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSLK)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSLK)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSLK)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LOCKED STATUS FIELD                                        *          
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
DISSLK   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.ETYRSTAT,ETYSLOCK                                              
         BZ    *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LOCKED STATUS FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
VALSLK   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTSLK                                                          
         NI    T.ETYRSTAT,FF-ETYSLOCK                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.ETYRSTAT,ETYSLOCK                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LOCKED STATUS FILTER FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
DFLTSLK  MVC   FVIFLD(L'BC@YES),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LOCKED STATUS FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VFLTSLK  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         MVI   SVFSLK,YES                                                       
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVI   SVFSLK,NO                                                        
         MVC   FLTIFLD(L'BC@YES),BC@NO                                          
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A LOCKED STATUS FIELD                               *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTSLK  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR LOCK EXPENSES APPLICATION STATUS                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LEX      NTRDO                                                                  
*                                                                               
LEXTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEX)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLEX)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLEX)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTLEX)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTLEX)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LOCK EXPENSES APPLICATION STATUS FIELD                     *          
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
DISLEX   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.ETYRSAPP,ETYKSAEQ                                              
         BZ    *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LOCK EXPENSES APPLICATION STATUS FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
VALLEX   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTLEX                                                          
         NI    T.ETYRSAPP,FF-ETYKSAEQ                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.ETYRSAPP,ETYKSAEQ                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LOCK EXPENSES APPLICATION STATUS FILTER FIELD             *         
***********************************************************************         
         SPACE 1                                                                
DFLTLEX  MVC   FVIFLD(L'BC@YES),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LOCK EXPENSES APPLICATION STATUS FILTER FIELD            *         
***********************************************************************         
         SPACE 1                                                                
VFLTLEX  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         MVI   SVFLEX,YES                                                       
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVI   SVFLEX,NO                                                        
         MVC   FLTIFLD(L'BC@YES),BC@NO                                          
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A LOCK EXPENSES APPLICATION STATUS FILTER FIELD     *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTLEX  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR LOCK INVOICES APPLICATION STATUS                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LIN      NTRDO                                                                  
*                                                                               
LINTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLIN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLIN)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLIN)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTLIN)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTLIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LOCK INVOICES APPLICATION STATUS FIELD                     *          
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
DISLIN   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.ETYRSAPP,ETYKSAIQ                                              
         BZ    *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LOCK INVOICES APPLICATION STATUS FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
VALLIN   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTLIN                                                          
         NI    T.ETYRSAPP,FF-ETYKSAIQ                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.ETYRSAPP,ETYKSAIQ                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LOCK INVOICES APPLICATION STATUS FILTER FIELD             *         
***********************************************************************         
         SPACE 1                                                                
DFLTLIN  MVC   FVIFLD(L'BC@YES),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LOCK INVOICES APPLICATION STATUS FILTER FIELD            *         
***********************************************************************         
         SPACE 1                                                                
VFLTLIN  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         MVI   SVFLIN,YES                                                       
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVI   SVFLIN,NO                                                        
         MVC   FLTIFLD(L'BC@YES),BC@NO                                          
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A LOCK INVOICES APPLICATION STATUS FILTER FIELD     *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTLIN  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR LOCK ORDERS APPLICATION STATUS                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LOR      NTRDO                                                                  
*                                                                               
LORTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLOR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLOR)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLOR)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTLOR)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTLOR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LOCK ORDERS APPLICATION STATUS FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
DISLOR   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.ETYRSAPP,ETYKSAOQ                                              
         BZ    *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LOCK ORDERS APPLICATION STATUS FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
VALLOR   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTLOR                                                          
         NI    T.ETYRSAPP,FF-ETYKSAOQ                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.ETYRSAPP,ETYKSAOQ                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LOCK ORDERS APPLICATION STATUS FILTER FIELD               *         
***********************************************************************         
         SPACE 1                                                                
DFLTLOR  MVC   FVIFLD(L'BC@YES),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LOCK ORDERS APPLICATION STATUS FILTER FIELD              *         
***********************************************************************         
         SPACE 1                                                                
VFLTLOR  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         MVI   SVFLOR,YES                                                       
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVI   SVFLOR,NO                                                        
         MVC   FLTIFLD(L'BC@YES),BC@NO                                          
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A LOCK ORDERS APPLICATION STATUS FIELD              *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTLOR  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR BILLABLE STATUS                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BIL      NTRDO                                                                  
*                                                                               
BILTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBIL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBIL)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTBIL)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTBIL)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTBIL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BILLABLE STATUS FIELD                                      *          
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
DISBIL   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.ETYRSTA2,ETYSBILY                                              
         BZ    *+14                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         TM    T.ETYRSTA2,ETYSBILD                                              
         BZ    *+10                                                             
         MVC   FVIFLD(L'AC@DEF),AC@DEF                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BILLABLE STATUS FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
VALBIL   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTBIL                                                          
         NI    T.ETYRSTA2,FF-(ETYSBILY+ETYSBILD)                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@DEF                                                 
         BE    VBIL02                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.ETYRSTA2,ETYSBILY                                              
         B     EXITOK                                                           
VBIL02   TM    T.ETYRSTA2,ETYSADVD+ETYSNBLD CHECK FOR OTHER DEFAULTS            
         BZ    VBIL04              IF PRESENT NOT VALID INPUT                   
         MVC   FVMSGNO,=AL2(AE$1TYPR) CAN'T HAVE MORE THAN ONE DEFAULT          
         B     EXITL                                                            
VBIL04   OI    T.ETYRSTA2,ETYSBILD                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BILLABLE STATUS FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTBIL  MVC   FVIFLD(L'AC@DEF),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A BILLABLE STATUS FILTER FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VFLTBIL  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         MVI   SVFBIL,YES                                                       
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVI   SVFBIL,NO                                                        
         MVC   FLTIFLD(L'BC@YES),BC@NO                                          
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVFBIL,DEF                                                       
         MVC   FLTIFLD(L'AC@DEF),AC@DEF                                         
         CLC   FVIFLD(1),AC@DEF                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A BILLABLE STATUS FIELD                             *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTBIL  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR NON-BILLABLE STATUS                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NBL      NTRDO                                                                  
*                                                                               
NBLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNBL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNBL)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTNBL)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTNBL)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTNBL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NON-BILLABLE STATUS FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
DISNBL   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.ETYRSTA2,ETYSNBLY                                              
         BZ    *+14                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         TM    T.ETYRSTA2,ETYSNBLD                                              
         BZ    *+10                                                             
         MVC   FVIFLD(L'AC@DEF),AC@DEF                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE NON-BILLABLE STATUS FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
VALNBL   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTNBL                                                          
         NI    T.ETYRSTA2,FF-(ETYSNBLY+ETYSNBLD)                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@DEF                                                 
         BE    VNBL02                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.ETYRSTA2,ETYSNBLY                                              
         B     EXITOK                                                           
VNBL02   TM    T.ETYRSTA2,ETYSBILD+ETYSADVD CHECK FOR OTHER DEFAULTS            
         BZ    VNBL04              IF PRESENT NOT VALID INPUT                   
         MVC   FVMSGNO,=AL2(AE$1TYPR) CAN'T HAVE MORE THAN ONE DEFAULT          
         B     EXITL                                                            
VNBL04   OI    T.ETYRSTA2,ETYSNBLD                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A NON-BILLABLE STATUS FILTER FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTNBL  MVC   FVIFLD(L'AC@DEF),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A NON-BILLABLE STATUS FILTER FIELD                         *         
***********************************************************************         
         SPACE 1                                                                
VFLTNBL  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         MVI   SVFNBL,YES                                                       
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BC@NO                                          
         MVI   SVFNBL,NO                                                        
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVFNBL,DEF                                                       
         MVC   FLTIFLD(L'AC@DEF),AC@DEF                                         
         CLC   FVIFLD(1),AC@DEF                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A NON-BILLABLE STATUS FIELD                         *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTNBL  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR ADVANCE STATUS                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ADV      NTRDO                                                                  
*                                                                               
ADVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADV)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTADV)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTADV)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTADV)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ADVANCE STATUS FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
DISADV   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.ETYRSTA2,ETYSADVY                                              
         BZ    *+14                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         TM    T.ETYRSTA2,ETYSADVD                                              
         BZ    *+10                                                             
         MVC   FVIFLD(L'AC@DEF),AC@DEF                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ADVANCE STATUS FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
VALADV   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTADV                                                          
         NI    T.ETYRSTA2,FF-(ETYSADVY+ETYSADVD)                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),AC@DEF                                                 
         BE    VADV02                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.ETYRSTA2,ETYSADVY                                              
         B     EXITOK                                                           
VADV02   TM    T.ETYRSTA2,ETYSBILD+ETYSNBLD CHECK FOR OTHER DEFAULTS            
         BZ    VADV04              IF PRESENT NOT VALID INPUT                   
         MVC   FVMSGNO,=AL2(AE$1TYPR) CAN'T HAVE MORE THAN ONE DEFAULT          
         B     EXITL                                                            
VADV04   OI    T.ETYRSTA2,ETYSADVD                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A ADVANCE STATUS FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTADV  MVC   FVIFLD(L'AC@DEF),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A ADVANCE STATUS FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
VFLTADV  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         MVI   SVFADV,YES                                                       
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BC@NO                                          
         MVI   SVFADV,NO                                                        
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVFADV,DEF                                                       
         MVC   FLTIFLD(L'AC@DEF),AC@DEF                                         
         CLC   FVIFLD(1),AC@DEF                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A ADVANCE STATUS FIELD                              *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTADV  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR APPROVAL NEEDED STATUS                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APN      NTRDO                                                                  
*                                                                               
APNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPN)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTAPN)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTAPN)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTAPN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY APPROVAL NEEDED STATUS FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
DISAPN   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.ETYRSTA2,ETYSAPND                                              
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE APPROVAL NEEDED STATUS FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
VALAPN   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTAPN                                                          
         NI    T.ETYRSTA2,FF-ETYSAPND                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         TM    T.ETYRSTA2,ETYSNBLY+ETYSNBLD                                     
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVAN)                                           
         B     EXITL               NON-BILL MUST SET TO YES OR DEFAULT          
         OI    T.ETYRSTA2,ETYSAPND                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A APPROVAL NEEDED STATUS FILTER FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTAPN  MVC   FVIFLD(L'BC@YES),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A APPROVAL NEEDED STATUS FILTER FIELD                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTAPN  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         MVI   SVFAPN,NO                                                        
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   *+14                                                             
         MVC   FLTIFLD(L'BC@NO),BC@NO                                           
         B     EXITOK                                                           
         MVI   SVFAPN,YES                                                       
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A APPROVAL NEEDED STATUS FIELD                      *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTAPN  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CATEGORY                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CAT      NTRDO                                                                  
*                                                                               
CATTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCAT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCAT)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCAT)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCAT)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCAT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CATEGORY FIELD                                             *          
***********************************************************************         
         SPACE 1                                                                
DISCAT   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('XNMELQ',ETYRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         USING XNMELD,RF                                                        
         CLI   XNMLN,XNMLN1Q                                                    
         BNH   EXITOK                                                           
         SR    RE,RE                                                            
         IC    RE,XNMSUBL                                                       
         AHI   RE,-1                                                            
         MVC   FVIFLD(0),XNMSUBN                                                
         EX    RE,*-6                                                           
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CATEGORY FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALCAT   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTCAT                                                          
         MVC   OLDCATEG,BCSPACES                                                
         GOTO1 AGETEL,BOPARM,('XNMELQ',ETYRECD),0                               
         BNE   VALCAT02                                                         
T        USING XNMELD,BOELEM                                                    
         SR    RE,RE                                                            
         IC    RE,T.XNMSUBL                                                     
         SHI   RE,1                                                             
         MVC   OLDCATEG(0),T.XNMSUBN                                            
         EX    RE,*-6                                                           
         GOTO1 ADELEL,BOPARM,('XNMELQ',ETYRECD),0                               
*                                                                               
VALCAT02 SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    EXITOK                                                           
         SHI   RF,1                                                             
         MVI   T.XNMEL,XNMELQ                                                   
         MVI   T.XNMLN,XNMLN1Q                                                  
         MVI   T.XNMSTAT,XNMSECTQ                                               
         MVC   T.XNMSUBN(0),FVIFLD                                              
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,T.XNMSUBL                                                     
         AHI   RF,XNMLN1Q+1                                                     
         STC   RF,T.XNMLN                                                       
         GOTO1 AADDEL,BOPARM,ETYRECD                                            
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CATEGORY FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTCAT  MVC   FVIFLD(L'ECTPCAT),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CATEGORY FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTCAT  DS    0H                                                               
         SR    R4,R4                                                            
         ICM   R4,1,FVILEN                                                      
         BZ    EXITOK                                                           
T        USING ECTPASD,IOKEY                                                    
         MVC   T.ECTPAS,BCSPACES                                                
         MVI   T.ECTPSEQ,0                                                      
         MVI   T.ECTPTYP,ECTPTYPQ                                               
         MVI   T.ECTPSUB,ECTPSUBQ                                               
         MVC   T.ECTPCPY,CUABIN                                                 
         MVC   T.ECTPCAT,BCSPACES                                               
         SHI   R4,1                                                             
         MVC   T.ECTPCAT(0),FVIFLD                                              
         EX    R4,*-6                                                           
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         AHI   R4,L'ECTPTYP+L'ECTPSUB+L'ECTPCPY                                 
         EX    R4,*+8                                                           
         BE    VFCAT02                                                          
         CLC   IOKEY(0),IOKEYSAV                                                
         MVC   FVMSGNO,=AL2(AE$ECTNV) EXP CATEGORY DOESN'T EXIST                
         B     EXITL                                                            
*                                                                               
VFCAT02  MVC   FLTIFLD(L'ECTPCAT),FVIFLD                                        
         MVC   SVFCAT(L'ECTPCAT),T.ECTPCAT FILTER FIELD                         
         MVC   SVFCATL,FVILEN                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON CATEGORY                                 *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTCAT  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FOREIGN NAME                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FNM      NTRDO                                                                  
*                                                                               
FNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FOREIGN NAME FIELD                                         *          
***********************************************************************         
         SPACE 1                                                                
DISFNM   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('ENMELQ',ETYRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         USING ENMELD,RF                                                        
         CLI   ENMLN,ENMLNQ                                                     
         BNH   EXITOK                                                           
         XR    RE,RE                                                            
         IC    RE,ENMLN                                                         
         AHI   RE,-(ENMLNQ+1)                                                   
         MVC   FVIFLD(0),ENMNAME                                                
         EX    RE,*-6                                                           
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FOREIGN NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALFNM   GOTO1 ADELEL,BOPARM,('ENMELQ',ETYRECD),0                               
         XR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    EXITOK                                                           
         USING ENMELD,R1                                                        
         LA    R1,BOELEM                                                        
         XC    ENMEL(ENMLNQ+L'ENMNAME+1),ENMEL                                  
         MVI   ENMEL,ENMELQ                                                     
         LR    RE,RF                                                            
         AHI   RE,ENMLNQ                                                        
         STC   RE,ENMLN                                                         
         AHI   RF,-1                                                            
         MVC   ENMNAME(0),FVIFLD                                                
         EX    RF,*-6                                                           
         DROP  R1                                                               
         GOTO1 AADDEL,BOPARM,ETYRECD                                            
         B     EXITOK                                                           
         SPACE 2                                                                
*                                                                               
***********************************************************************         
* DATA OBJECT FOR VAT CODE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
VATC     NTRDO                                                                  
*                                                                               
VATCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISVATC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVATC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY VAT CODE FIELD                                             *          
***********************************************************************         
         SPACE 1                                                                
DISVATC  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FFTELQ',ETYRECD),       X        
               (1,=AL1(FFTTVATC))                                               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         USING FFTELD,RF                                                        
         CLI   FFTLN,FFTLN1Q                                                    
         BNH   EXITOK                                                           
         SR    RE,RE                                                            
         IC    RE,FFTDLEN                                                       
         AHI   RE,-1                                                            
         MVC   FVIFLD(0),FFTDATA                                                
         EX    RE,*-6                                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE VAT CODE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALVATC  GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('FFTELQ',ETYRECD),       X        
               (1,=AL1(FFTTVATC))                                               
*                                                                               
         SR    R3,R3                                                            
         ICM   R3,1,FVILEN                                                      
         BZ    EXITOK                                                           
         OI    FVIFLD,X'40'                                                     
         LA    R4,IOKEY                                                         
         USING TAXRECD,R4                                                       
         XC    TAXKEY,TAXKEY       READ GROUP RECORD                            
         MVI   TAXKTYP,TAXKTYPQ                                                 
         MVC   TAXKOFF,XFFS                                                     
         XR    R1,R1                                                            
         ICM   R1,7,BCTODAYP                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TAXKDATE                                                    
         MVC   TAXKCPY,CUABIN      CONNECTED ID                                 
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         CLC   TAXKEY(TAXKDATE-TAXKEY),IOKEYSAV                                 
         BNE   EXITNV                                                           
         SR    R0,R0                                                            
         L     R4,AIO2                                                          
         LA    R4,TAXRFST                                                       
         USING TAXELD,R4                                                        
VVATC02  CLI   TAXEL,0                                                          
         BE    EXITNV                                                           
         CLI   TAXEL,TAXIELQ                                                    
         BNE   VVATC06                                                          
         CLC   TAXCODE,FVIFLD                                                   
         BE    VVATC10                                                          
                                                                                
VVATC06  IC    R0,TAXLN                                                         
         AR    R4,R0                                                            
         B     VVATC02                                                          
                                                                                
T        USING FFTELD,BOELEM                                                    
VVATC10  SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         AHI   RF,-1                                                            
         XC    T.FFTEL(255),T.FFTEL                                             
         MVI   T.FFTEL,FFTELQ                                                   
         MVI   T.FFTTYPE,FFTTVATC                                               
         MVC   T.FFTDATA(0),FVIFLD                                              
         EX    RF,*-6                                                           
         AHI   RF,1                                                             
         STC   RF,T.FFTDLEN                                                     
         AHI   RF,FFTLN1Q+1                                                     
         STC   RF,T.FFTLN                                                       
         GOTO1 AADDEL,BOPARM,ETYRECD                                            
         B     EXITOK                                                           
         SPACE 2                                                                
*&&UK                                                                           
***********************************************************************         
* DATA OBJECT FOR VAT ACCOUNT                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
VATA     NTRDO                                                                  
*                                                                               
VATATBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISVATA)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVATA)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHVA)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY VAT ACCOUNT FIELD                                          *          
***********************************************************************         
         SPACE 1                                                                
DISVATA  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('SPAELQ',ETYRECD),       X        
               (1,=AL1(SPATITAX))                                               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         USING SPAELD,RF                                                        
         MVC   FVIFLD(L'SPAAACT),SPAAACT                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A VAT ACCOUNT FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
SRCHVA   GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,SGUL,ACOM,     C        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE VAT ACCOUNT FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALVATA  GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('SPAELQ',ETYRECD),       X        
               (1,=AL1(SPATITAX))                                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    EXITOK                                                           
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         MVC   ACTKUNT(L'SGUL),SGUL                                             
         AHI   RF,1                                                             
         MVC   ACTKACT(0),FVIFLD                                                
         EX    RF,*-6                                                           
         OC    ACTKACT,BCSPACES                                                 
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         L     R4,AIO2                                                          
         TM    ACTRSTAT,ACTSABLP                                                
         BNZ   VVATA02                                                          
         MVC   FVMSGNO,=AL2(AE$NLOWA) NOT A LOW LEVEL ACCOUNT                   
         B     EXITL                                                            
*                                                                               
T        USING SPAELD,BOELEM                                                    
VVATA02  XC    T.SPAEL(SPALNQ),T.SPAEL                                          
         MVI   T.SPAEL,SPAELQ                                                   
         MVI   T.SPALN,SPALNQ                                                   
         MVI   T.SPATYPE,SPATITAX                                               
         MVC   T.SPAAULA,ACTKULA                                                
         GOTO1 AADDEL,BOPARM,ETYRECD                                            
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR VAT NAME                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
VATN     NTRDO                                                                  
*                                                                               
VATNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISVATN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY VAT CODE FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISVATN  TM    BCCPYST5,CPYSNVAT                                                
         BZ    DVATN10                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FFTELQ',ETYRECD),       X        
               (1,=AL1(FFTTVATC))                                               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         USING FFTELD,RF                                                        
         CLI   FFTLN,FFTLN1Q                                                    
         BNH   EXITOK                                                           
         LLC   RE,FFTDLEN                                                       
         SHI   RE,1                                                             
         BM    EXITOK                                                           
         MVC   BOWORK1(0),FFTDATA                                               
         EX    RE,*-6                                                           
         DROP  RF                                                               
         LA    R4,IOKEY                                                         
         USING TAXRECD,R4                                                       
         XC    TAXKEY,TAXKEY       READ GROUP RECORD                            
         MVI   TAXKTYP,TAXKTYPQ                                                 
         MVC   TAXKCPY,CUABIN      CONNECTED ID                                 
         MVC   TAXKOFF,XFFS                                                     
         XR    R1,R1                                                            
         ICM   R1,7,BCTODAYP                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,TAXKDATE                                                    
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         CLC   TAXKEY(TAXKDATE-TAXKEY),IOKEYSAV                                 
         BNE   EXITOK                                                           
*                                                                               
         L     R4,AIO2                                                          
         LA    R4,TAXRFST                                                       
         SR    R0,R0                                                            
         USING TAXELD,R4                                                        
DVATN02  CLI   TAXEL,0                                                          
         BE    EXITOK                                                           
         CLI   TAXEL,TAXIELQ                                                    
         BNE   DVATN04                                                          
         CLC   TAXCODE,BOWORK1                                                  
         BE    DVATN06                                                          
*                                                                               
DVATN04  IC    R0,TAXLN                                                         
         AR    R4,R0                                                            
         B     DVATN02                                                          
DVATN06  MVC   FVIFLD(L'TAXTYPE),TAXTYPE                                        
         B     EXITOK                                                           
         DROP  R4                                                               
*                                                                               
DVATN10  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('SPAELQ',ETYRECD),       X        
               (1,=AL1(SPATITAX))                                               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         USING SPAELD,RF                                                        
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,SPAAULA                                                  
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         DROP  R4,RF                                                            
*                                                                               
         L     R4,AIO2                                                          
         LA    R4,ACTRFST-ACTRECD(R4)                                           
         SR    R0,R0                                                            
         USING NAMELD,R4                                                        
DVATN12  CLI   NAMEL,0                                                          
         BE    EXITOK                                                           
         CLI   NAMEL,NAMELQ                                                     
         BE    DVATN16                                                          
*                                                                               
DVATN14  IC    R0,NAMLN                                                         
         AR    R4,R0                                                            
         B     DVATN12                                                          
*                                                                               
DVATN16  SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         MVC   FVIFLD(0),NAMEREC                                                
         EX    RF,*-6                                                           
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR DEFAULT INDICATOR                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
DFT      NTRDO                                                                  
*                                                                               
DFTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDFT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDFT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DEFAULT INDICATOR FIELD                                    *          
***********************************************************************         
         SPACE 1                                                                
DISDFT   LA    RF,TLKDEF                                                        
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDSA                                                         
*                                                                               
         CLI   0(RF),C' '          TEST NULL ENTRY                              
         BNH   EXITOK              NOTHING TO DO                                
         TM    0(RF),X'40'                                                      
         BNZ   *+10                                                             
         MVC   FVIFLD(1),BC@YES                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DEFAULT INDICATOR FIELD                                   *          
***********************************************************************         
         SPACE 1                                                                
VALDFT   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         OI    TLKDEF,X'40'                                                     
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         NI    TLKDEF,FF-X'40'                                                  
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR WORKCODE LIST                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
WCL      NTRDO                                                                  
*                                                                               
WCLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWCL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWCL)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHWC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY WORKCODE LIST FIELD                                        *          
***********************************************************************         
         SPACE 1                                                                
DISWCL   LA    RF,TLKWC                                                         
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDWC                                                         
         MVC   FVIFLD(L'TLKWC),0(RF)                                            
         OI    FVIFLD,X'40'      ENSURE X'40' BIT ON FOR DISPLAY                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE WORKCODE LIST FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALWCL   XC    TLKWC,TLKWC                                                      
         MVC   TLRLEN,=AL2(TLLNQ)  SET TSAR RECLEN                              
         CLI   FVILEN,1                                                         
         BNL   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
         BE    VALWCINV            1 = TOO SHORT, INVALID WORK CODE             
T        USING WCORECD,IOKEY                                                    
         MVC   T.WCOKEY,BCSPACES READ FOR WORKCODE RECORD                       
         MVI   T.WCOKTYP,WCOKTYPQ                                               
         MVC   T.WCOKCPY,CUABIN                                                 
         MVC   T.WCOKUNT(L'PRODUL),PRODUL                                       
         MVC   T.WCOKWRK,FVIFLD                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   VALWCINV            NOT FOUND - INVALID WORK CODE                
         MVC   TLKWC,T.WCOKWRK                                                  
         B     EXITOK                                                           
*                                     * ERROR EXITS *                           
VALWCINV MVC   FVMSGNO,=AL2(AE$INWRK) INVALID WORKCODE                          
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(2),FVIFLD                                                 
         B     EXITL                                                            
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A WORKCODE FIELD                                          *         
***********************************************************************         
SRCHWC   DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,WC,ACOM,       >        
               (X'44',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A WORK CODE NAME FIELD                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
WCN      NTRDO                                                                  
*                                                                               
WCNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWCN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A WORKCODE NAME FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING WCOKEY,IOKEY                                                     
DISWCN   LA    RF,TLKWC                                                         
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDWC                                                         
*                                                                               
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,0(RF)       WORKCODE FROM TSAR REC                       
         OI    WCOKWRK,X'40'       IN CASE 'DEFAULT' IND SET                    
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A WORK CODE DESCRIPTION FIELD            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
WCD      NTRDO                                                                  
*                                                                               
WCDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWCD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A WORKCODE DESCRIPTION FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
         USING WCOKEY,IOKEY                                                     
DISWCD   LA    RF,TLKWC                                                         
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDWC                                                         
*                                                                               
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(L'PRODUL),PRODUL                                         
         MVC   WCOKWRK,0(RF)       WORKCODE FROM TSAR REC                       
         OI    WCOKWRK,X'40'       IN CASE 'DEFAULT' IND SET                    
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     RF,AIO2                                                          
         LA    RF,WCORFST-WCORECD(RF)                                           
         SR    R0,R0                                                            
         USING WCOELD,RF                                                        
DISWCD02 CLI   WCOEL,0                                                          
         BE    EXITOK                                                           
         CLI   WCOEL,WCOELQ                                                     
         BE    *+14                                                             
         IC    R0,WCOLN                                                         
         AR    RF,R0                                                            
         B     DISWCD02                                                         
         MVC   FVIFLD(L'WCODESC),WCODESC                                        
         B     EXITOK                                                           
         DROP  RF                                                               
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXPENSE ACCOUNT LIST                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
XAL      NTRDO                                                                  
*                                                                               
XALTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISXAL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXAL)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHXA)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EXPENSE ACCOUNT LIST FIELD                                 *          
***********************************************************************         
         SPACE 1                                                                
DISXAL   LA    RF,TLKXA                                                         
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDXA                                                         
         MVC   FVIFLD(L'TLKXA),0(RF)                                            
         OI    FVIFLD,X'40'        ENSURE X'40' BIT ON FOR DISPLAY              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE EXPENSE ACCOUNT LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VALXAL   XC    TLKXA,TLKXA                                                      
         MVC   TLRLEN,=AL2(TLLNQ)                                               
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
         CLI   FVILEN,2                                                         
         BNH   VALXAINV            INVALID ACCOUNT                              
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES READ FOR EXPENSE ACCOUNT RECORD                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(2),=C'SE'                                              
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         MVC   T.ACTKUNT(0),FVIFLD                                              
         EX    RE,*-6                                                           
         CLC   =C'SA',T.ACTKUNT                                                 
         BE    *+10                                                             
         CLC   =C'SE',T.ACTKUNT                                                 
         BE    *+10                                                             
*&&US*&& CLC   =C'SB',T.ACTKUNT                                                 
*&&UK*&& CLC   =C'SQ',T.ACTKUNT                                                 
         BNE   VALXAINV                                                         
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   VALXAINV            INVALID ACCOUNT                              
         GOTO1 AGETEL,BOPARM,('ABLELQ',AIO2),0                                  
         BE    VALXAL2                                                          
         CLI   CUCTRY,CTRYGER      GERMANY ALLOWS LOW LVL A/CS                  
         BNE   VALXAINV            ELSE ERROR                                   
*                                                                               
VALXAL2  MVC   TLKXA,T.ACTKUNT                                                  
         B     EXITOK                                                           
*                                     * ERROR EXITS *                           
VALXAINV MVC   FVMSGNO,=AL2(AE$INACC) INVALID ACCOUNT                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'ACTKACT),FVIFLD                                         
         B     EXITL                                                            
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON AN EXPENSE A/C FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
SRCHXA   DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,ACOM,        >        
               (X'44',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN EXPENSE ACCOUNT NAME FIELD            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
XAN      NTRDO                                                                  
*                                                                               
XANTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISXAN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN EXPENSE ACCOUNT NAME FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTKEY,IOKEY                                                     
DISXAN   LA    RF,TLKXA                                                         
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDXA                                                         
*                                                                               
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,0(RF)       EXPENSE ACC TSAR REC                         
         OI    ACTKULA,X'40'       IN CASE 'DEFAULT' IND SET                    
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR SUPPLIER ACCOUNT LIST                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
SAL      NTRDO                                                                  
*                                                                               
SALTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSAL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSAL)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHSA)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUPPLIER ACCOUNT LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISSAL   LA    RF,TLKSA                                                         
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDSA                                                         
*                                                                               
         MVC   FVIFLD(L'TLKSA),0(RF)                                            
         OI    FVIFLD,X'40'        ENSURE X'40' BIT ON FOR DISPLAY              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUPPLIER ACCOUNT LIST FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VALSAL   XC    TLKSA,TLKSA                                                      
         MVC   TLRLEN,=AL2(TLLNQ)  SET TSAR RECLEN                              
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
         CLI   FVILEN,2                                                         
         BNH   VALSAINV            INVALID ACCOUNT                              
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES READ FOR SUPPLIER ACCOUNT                      
         MVC   T.ACTKCPY,CUABIN                                                 
         SR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         MVC   T.ACTKUNT(0),FVIFLD                                              
         EX    RE,*-6                                                           
         CLC   =C'ST',T.ACTKUNT                                                 
         BE    VALSAL2                                                          
         CLC   =C'SV',T.ACTKUNT                                                 
         BE    VALSAL2                                                          
         CLC   =C'SX',T.ACTKUNT                                                 
         BE    VALSAL2                                                          
*&&US                                                                           
         CLC   =C'SW',T.ACTKUNT                                                 
         BE    VALSAL2                                                          
         CLC   =C'SY',T.ACTKUNT                                                 
         BE    VALSAL2                                                          
*&&                                                                             
         CLC   =C'SK',T.ACTKUNT                                                 
         BE    VALSAL2                                                          
         CLI   CUCTRY,CTRYGER      GERMANY ONLY ALLOWS SK                       
         BE    VALSAINV            UK ALLOWS SI AND SK                          
         CLC   =C'SI',T.ACTKUNT                                                 
         BNE   VALSAINV            INVALID                                      
*                                                                               
VALSAL2  LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   VALSAINV            INVALID ACCOUNT                              
         GOTO1 AGETEL,BOPARM,('ABLELQ',AIO2),0                                  
         BE    VALSAL6                                                          
*        CLI   CUCTRY,CTRYGER                                                   
*        BNE   VALSAINV            NOT A LOW-LEVEL ACCOUNT                      
         CLC   =C'SV',T.ACTKUNT                                                 
         BE    VALSAL4                                                          
         CLC   =C'SX',T.ACTKUNT                                                 
         BNE   VALSAINV                                                         
*                                                                               
VALSAL4  DS    0H                  NO LIMITATIONS FOR NOW IF GER/SV/SX          
*                                                                               
VALSAL6  MVC   TLKSA,T.ACTKUNT                                                  
         B     EXITOK                                                           
*                                     * ERROR EXITS *                           
VALSAINV MVC   FVMSGNO,=AL2(AE$INACC) INVALID ACCOUNT                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'ACTKULA),FVIFLD                                         
         B     EXITL                                                            
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON AN SUPPLIER A/C FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHSA   DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,ACOM,        >        
               (X'44',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A SUPPLIER ACCOUNT NAME FIELD            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
SAN      NTRDO                                                                  
*                                                                               
SANTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSAN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUPPLIER ACCOUNT NAME FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTKEY,IOKEY                                                     
DISSAN   LA    RF,TLKSA                                                         
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDSA                                                         
*                                                                               
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,0(RF)       SUPPLIER U/L/A FROM TSAR REC                 
         OI    ACTKULA,X'40'       IN CASE 'DEFAULT' IND SET                    
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR SELF APPROVAL IN ETYPE                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SFAP     NTRDO                                                                  
*                                                                               
SFAPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFAP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSFAP)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSFAP)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSFAP)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSFAP)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SELF APPROVAL IN ETYPE                                     *          
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
DISFAP   CLI   CSACT,A#DLOAD       DOWNLOADING ?                                
         BE    EXITOK              YES - OK                                     
         MVC   FVIFLD(L'BC@YES),BC@YES DEFAULT IS YES                           
         TM    T.ETYRSTA2,ETYSFAP      SELF APPROVAL NOT ALLOWED?               
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BC@NO   IF BIT SET THEN NO                       
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SELF APPROVAL ALLOWED                                      *         
***********************************************************************         
         SPACE 1                                                                
T        USING ETYRSTA,GSRECSTA                                                 
VALSFAP  CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTSFAP                                                         
         NI    T.ETYRSTA2,FF-(ETYSFAP)                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES        DEFAULT IS YES                           
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
         OI    T.ETYRSTA2,ETYSFAP                                               
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY SELF APPROVAL ALLOWED FOR AN ETYPE                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTSFAP MVC   FVIFLD(L'BC@YES),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON SELF APPROVAL FOR ETYPE                        *         
***********************************************************************         
         SPACE 1                                                                
VFLTSFAP CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         MVI   SVFSFAP,YES                                                      
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVI   SVFSFAP,NO                                                       
         MVC   FLTIFLD(L'BC@NO),BC@NO                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON SELF APPROVAL FOR AN ETYPE                          *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTSFAP B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR OFFICE/OFFICE LIST CODE (ONLY FOR GSMCODE=4)        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFL     NTRDO                                                                  
*                                                                               
OFFLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOFF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFF)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOFF)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTOFF)                               
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTOFF)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE/OFFICE LIST CODE                                    *          
***********************************************************************         
         SPACE 1                                                                
DISOFF   MVC   FVIFLD(L'ETYKOFFC),ETYKOFFC                                      
         MVC   SVOFF,ETYKOFFC      SAVE OFF OFFICE CODE                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT OFFICE/OFFICE LIST ON NTRSES                              *         
***********************************************************************         
         SPACE 1                                                                
DSETOFF  DS    0H                                                               
         B     FLTXX               RETURN 'NOT VALID' TO FORCE UNPROT           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE/OFFICE LIST                                         *         
***********************************************************************         
         SPACE 1                                                                
T        USING OFFRECD,IOKEY       IF OFFICE GROUP CHECK WHETHER RECORD         
VALOFF   CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    VFLTOFF                                                          
*                                                                               
         CLC   FVIFLD,BCSPACES     ANY OFFICE ENTERED?                          
         BH    VALOFF2                                                          
         CLI   CUACCS,0            GLOBAL LOGON?                                
         BE    VALOFFX             OK TO HAVE NO OFFICE                         
         B     EXITNO              NOT ALLOWED TO ENTER WITHOUT OFFICE          
*                                                                               
VALOFF2  TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BZ    VALOFF4                                                          
         CLI   FVILEN,2            MUST BE 2 CHARACTERS                         
         BNE   EXIT2CH                                                          
*                                                                               
         MVC   T.OFFKEY,BCSPACES   CHECK OFFICE/OFFICE LIST ENTERED IS          
         MVI   T.OFFKTYP,OFFKTYPQ  VALID                                        
         MVC   T.OFFKCPY,CUABIN                                                 
         MVC   T.OFFKOFF,FVIFLD                                                 
         L     R1,=AL4(XOHI+XOACCDIR+XIO2)                                      
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(OFFKEND),IOKEY                                          
         BNE   EXITOFNV            NOT A VALID OFFICE/OFFICE LIST               
         CLI   CUACCS,0            GLOBAL LOGON?                                
         BE    VALOFFX             OK THEN                                      
         TM    T.OFFKSTAT,OFFSLIST OFFICE LIST?                                 
         BNZ   VALOFFX                                                          
         GOTO1 ATSTOFF,FVIFLD      2 CHAR OFFICE                                
         BNE   EXITL               NOT VALID                                    
         B     VALOFFX                                                          
*                                  1 CHAR OFFICE                                
VALOFF4  CLI   FVILEN,2            IS IT 2 CHARS?                               
         BE    VALOFF6             THEN MUST BE LIMIT LIST                      
*                                                                               
         GOTO1 ATSTOFF,FVIFLD                                                   
         BNE   EXITL                                                            
         B     VALOFFX                                                          
*                                                                               
VALOFF6  CLI   FVIFLD,C'$'         FIRST CHARACTER MUST BE $                    
         BNE   EXITNV                                                           
         L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  GLOBAL LOGON?                                
         BE    VALOFF8                                                          
         CLI   TWAACCS-TWAD(RF),C'$' MUST BE LIMIT LIST LOGON                   
         BNE   EXITNOL             NOT LIMIT LIST LOGON                         
         CLC   FVIFLD(2),TWAACCS-TWAD(RF)                                       
         BNE   EXITNV              IF LIMIT LIST LOGON OFFICE LIST              
         B     VALOFFX             MUST BE LIMIT LIST CODE                      
*                                                                               
X        USING CTUREC,IOKEY        CHECK SECURITY RECORD FOR GLOBAL             
VALOFF8  XC    X.CTUKEY,X.CTUKEY   LOGON TO SEE IF OFFICE LIST EXISTS           
         MVI   X.CTUKTYP,CTUKTYPQ                                               
         MVI   X.CTUKSYS,C'A'                                                   
         MVC   X.CTUKPROG+1(2),FVIFLD                                           
         MVC   X.CTUKAGY,CUAALF                                                 
         LHI   R1,XOREAD+XOCONFIL+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITNV             ERROR SEC FILE DOESN'T EXIST                  
*                                                                               
VALOFFX  MVC   ETYKOFFC,FVIFLD                                                  
         B     EXITOK                                                           
         DROP  X,T                                                              
***********************************************************************         
* DISPLAY OFFICE/OFFICE LIST FILTER                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  MVC   FLTIFLD(L'ETYKOFFC),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON OFFICE/OFFICE LIST                             *         
***********************************************************************         
         SPACE 1                                                                
VFLTOFF  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         XR    RF,RF                                                            
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   VFLTOFF2                                                         
         CLI   FVIFLD,C'$'                                                      
         BNE   *+8                                                              
         LA    RF,1                                                             
         B     VFLTOFF4                                                         
*                                                                               
VFLTOFF2 CLI   FVILEN,2           IF 2 CHARACTER OFFICES FILTER                 
         BNE   EXIT2CH            NEEDS TO BE 2 CHARACTERS                      
*                                                                               
VFLTOFF4 MVC   FLTIFLD(0),FVIFLD                                                
         EX    RF,*-6                                                           
         MVC   ETYKOFFC,FLTIFLD                                                 
         MVC   SVFOFF,ETYKOFFC                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR OFFICE                                              *         
***********************************************************************         
         SPACE 1                                                                
DDFTOFF  CLI   CSACT,A#DLOAD       DOWNLOAD                                     
         BE    EXITOK                                                           
         MVC   FVIFLD(L'ETYKOFFC),ETYKOFFC                                      
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'ETYKOFFC),SVOFF                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON OFFICE/OFFICE LIST                                  *         
* OVERLAY WILL DO ITS OWN FILTERING FOR DOWNLOADING - SEE DOFLT       *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  XR    RF,RF                                                            
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BZ    *+8                                                              
         LA    RF,1                                                             
         CLI   FLTIFLD,C'$'                                                     
         BNE   *+8                                                              
         LA    RF,1                                                             
         CLC   ETYKOFFC(0),FLTIFLD                                              
         EX    RF,*-6                                                           
         BE    FLTXE                                                            
         B     FLTXX                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXPENDITURE TYPE CODE (DOWNLOAD)                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
ETCD     NTRDO                                                                  
*                                                                               
ETCDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISETCD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN EXPENDITURE TYPE CODE                                    *         
***********************************************************************         
         SPACE 1                                                                
DISETCD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLKETC),TLKETC                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXPENDITURE TYPE NAME (DOWNLOAD)                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
ETND     NTRDO                                                                  
*                                                                               
ETNDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISETND)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN EXPENDITURE TYPE NAME                                    *         
***********************************************************************         
         SPACE 1                                                                
DISETND  L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLDETN),TLDETN                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR OFFICE/OFFICE LIST CODE (DOWNLOAD)                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
OFLD     NTRDO                                                                  
*                                                                               
OFLDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFLD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN OFFICE/OFFICE LIST CODE                                  *         
***********************************************************************         
         SPACE 1                                                                
DISOFLD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLKOFF),TLKOFF                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR ADVANCE STATUS (DOWNLOAD)                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
ADVD     NTRDO                                                                  
*                                                                               
ADVDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISADVD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A ADVANCE STATUS FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISADVD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSTA2,ETYSADVY                                                 
         BZ    *+14                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         TM    TLDSTA2,ETYSADVD                                                 
         BZ    *+10                                                             
         MVC   FVIFLD(L'AC@DEF),AC@DEF                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR APPROVAL NEEDED STATUS (DOWNLOAD)                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
APND     NTRDO                                                                  
*                                                                               
APNDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPND)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A APPROVAL NEEDED STATUS FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISAPND  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSTA2,ETYSAPND                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FOREIGN NAME (DOWNLOAD)                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
FNMD     NTRDO                                                                  
*                                                                               
FNMDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNMD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FOREIGN NAME FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DISFNMD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLDFNM),TLDFNM                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR LOCK INVOICES APPLICATION STATUS (DOWNLOAD)         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
LIND     NTRDO                                                                  
*                                                                               
LINDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLIND)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LOCK INVOICES APPLICATION STATUS FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
DISLIND  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSAPP,ETYKSAIQ                                                 
         BZ    *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR LOCK ORDERS APPLICATION STATUS (DOWNLOAD)           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
LORD     NTRDO                                                                  
*                                                                               
LORDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLORD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LOCK ORDERS APPLICATION STATUS FIELD                      *         
***********************************************************************         
         SPACE 1                                                                
DISLORD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSAPP,ETYKSAOQ                                                 
         BZ    *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR LOCK EXPENSES APPLICATION STATUS (DOWNLOAD)         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
LEXD     NTRDO                                                                  
*                                                                               
LEXDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEXD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LOCK EXPENSES APPLICATION STATUS FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
DISLEXD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSAPP,ETYKSAEQ                                                 
         BZ    *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR LOCKED STATUS (DOWNLOAD)                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
SLKD     NTRDO                                                                  
*                                                                               
SLKDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSLKD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LOCKED STATUS FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSLKD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSTAT,ETYSLOCK                                                 
         BZ    *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CATEGORY (DOWNLOAD)                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
CATD     NTRDO                                                                  
*                                                                               
CATDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCATD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CATEGORY FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISCATD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLDCAT),TLDCAT                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
* DATA OBJECT FOR VAT CODE (DOWNLOAD)                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
VCDD     NTRDO                                                                  
*                                                                               
VCDDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISVCDD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A VAT CODE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISVCDD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLDVATC),TLDVATC                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR VAT ACCOUNT (DOWNLOAD)                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
VACD     NTRDO                                                                  
*                                                                               
VACDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISVACD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A VAT ACCOUNT FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISVACD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLDVATA),TLDVATA                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR VAT NAME (DOWNLOAD)                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
VNMD     NTRDO                                                                  
*                                                                               
VNMDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISVNMD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A VAT NAME FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISVNMD  L     R2,ATLST                                                         
         TM    BCCPYST5,CPYSNVAT                                                
         BZ    DVNMD10                                                          
         CLC   TLDVATC,BCSPACES                                                 
         BNH   EXITOK                                                           
T        USING TAXRECD,IOKEY                                                    
         XC    T.TAXKEY,T.TAXKEY   READ VAT RECORD                              
         MVI   T.TAXKTYP,TAXKTYPQ                                               
         MVC   T.TAXKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.TAXKOFF,XFFS                                                   
         XR    R1,R1                                                            
         ICM   R1,7,BCTODAYP                                                    
         LNR   R1,R1                                                            
         STCM  R1,7,T.TAXKDATE                                                  
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         CLC   T.TAXKEY(TAXKDATE-TAXKEY),IOKEYSAV                               
         BNE   EXITOK                                                           
         L     R4,AIO2                                                          
         LA    R4,TAXRFST-TAXRECD(R4)                                           
         SR    R0,R0                                                            
         USING TAXELD,R4                                                        
DVNMD04  CLI   TAXEL,0                                                          
         BE    EXITOK                                                           
         CLI   TAXEL,TAXIELQ                                                    
         BNE   *+14                                                             
         CLC   TAXCODE,TLDVATC                                                  
         BE    *+14                                                             
         IC    R0,TAXLN                                                         
         AR    R4,R0                                                            
         B     DVNMD04                                                          
*                                                                               
         MVC   FVIFLD(L'TAXTYPE),TAXTYPE                                        
         B     EXITOK                                                           
         DROP  T,R4                                                             
*                                                                               
T        USING ACTRECD,IOKEY                                                    
DVNMD10  CLC   TLDVATA,BCSPACES                                                 
         BNH   EXITOK                                                           
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'SGUL),SGUL                                           
         MVC   T.ACTKACT,TLDVATA                                                
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR BILLABLE STATUS (DOWNLOAD)                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
BILD     NTRDO                                                                  
*                                                                               
BILDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISBILD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BILLABLE STATUS FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISBILD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSTA2,ETYSBILY                                                 
         BZ    *+14                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         TM    TLDSTA2,ETYSBILD                                                 
         BZ    *+10                                                             
         MVC   FVIFLD(L'AC@DEF),AC@DEF                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR NON-BILLABLE STATUS (DOWNLOAD)                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
NBLD     NTRDO                                                                  
*                                                                               
NBLDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISNBLD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A NON-BILLABLE STATUS FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISNBLD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    TLDSTA2,ETYSNBLY                                                 
         BZ    *+14                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         TM    TLDSTA2,ETYSNBLD                                                 
         BZ    *+10                                                             
         MVC   FVIFLD(L'AC@DEF),AC@DEF                                          
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR SELF APPROVAL IN ETYPE (DOWNLOAD)                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
SAPD     NTRDO                                                                  
*                                                                               
SAPDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSAPD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SELF APPROVAL IN ETYPE FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISSAPD  L     R2,ATLST                                                         
         MVC   FVIFLD(L'BC@YES),BC@YES DEFAULT IS YES                           
         TM    TLDSTA2,ETYSFAP         SELF APPROVAL NOT ALLOWED?               
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@NO),BC@NO   IF BIT SET THEN NO                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DEFAULT EXPENSE INDICATOR                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
EXDD     NTRDO                                                                  
*                                                                               
EXDDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISEXDD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DEFAULT SUPPLIER INDICATOR FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DISEXDD  L     R2,ATLST                                                         
*                                                                               
         CLI   TLDXA,C' '          TEST NULL ENTRY                              
         BNH   EXITOK              NOTHING TO DO                                
         TM    TLDXA,X'40'                                                      
         BNZ   *+10                                                             
         MVC   FVIFLD(1),BC@YES                                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEFAULT WORKCODE INDICATOR                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
WCDD     NTRDO                                                                  
*                                                                               
WCDDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISWCDD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DEFAULT WORKCODE INDICATOR FIELD                           *          
***********************************************************************         
         SPACE 1                                                                
DISWCDD  L     R2,ATLST                                                         
*                                                                               
         CLI   TLDWC,C' '          TEST NULL ENTRY                              
         BNH   EXITOK              NOTHING TO DO                                
         TM    TLDWC,X'40'                                                      
         BNZ   *+10                                                             
         MVC   FVIFLD(1),BC@YES                                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
*&&US                                                                           
***********************************************************************         
* PROVINCE CODE                                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
PRVC     NTRDO                                                                  
*                                                                               
PRVCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRVC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRVC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROVINCE CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
DISPRVC  LA    RF,TLKPRV                                                        
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDPRVC                                                       
         MVC   FVIFLD(L'TLKPRV),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROVINCE CODE                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPRVC  XC    TLKPRV,TLKPRV                                                    
         MVC   TLRLEN,=AL2(TLLNQ)  SET TSAR RECLEN                              
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
         GOTO1 VALPRV,BOPARM,FVIFLD                                             
         BNE   EXITNV                                                           
         MVC   TLKPRV,FVIFLD                                                    
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PROVINCE NAME                                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
PRVN     NTRDO                                                                  
*                                                                               
PRVNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRVN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROVINCE NAME                                             *         
***********************************************************************         
         SPACE 1                                                                
DISPRVN  MVC   BOHALF1,TLKPRV                                                   
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+14                                                             
         L     R2,ATLST                                                         
         MVC   BOHALF1,TLDPRVC                                                  
*                                                                               
         GOTO1 VALPRV,BOPARM,BOHALF1                                            
         MVC   FVIFLD(L'SVPRVN),SVPRVN                                          
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* PST CODE                                                            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
PSTC     NTRDO                                                                  
*                                                                               
PSTCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPSTC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPSTC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PST CODE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISPSTC  LA    RF,TLAPST                                                        
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLAPST                                                        
*                                                                               
         MVC   FVIFLD(L'TLAPST),0(RF)                                           
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A PST CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALPSTC  XC    TLAPST,TLAPST                                                    
         MVC   TLRLEN,=AL2(TLLNQ)  SET TSAR RECLEN                              
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
         GOTO1 VALPST,BOPARM,TLKPRV                                             
         BNE   EXITNV                                                           
         MVC   TLAPST,FVIFLD                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
         POP   USING                                                            
***********************************************************************         
* PST NAME                                                            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
PSTN     NTRDO                                                                  
*                                                                               
PSTNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPSTN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PST NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISPSTN  MVC   BOHALF1,TLAPST                                                   
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+14                                                             
         L     R2,ATLST                                                         
         MVC   BOHALF1,TLDPSTC                                                  
*                                                                               
         GOTO1 VALPST,BOPARM,TLKPRV                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVIFLD(L'SVPSTN),SVPSTN                                          
         B     EXITOK                                                           
         SPACE 1                                                                
         POP   USING                                                            
*&&                                                                             
***********************************************************************         
* DOWNLOAD OBJECT LEVEL 2                                             *         
***********************************************************************         
         SPACE 1                                                                
THIS     USING ETYRECD,R2                                                       
LAST     USING ETYRECD,R3                                                       
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
* INITIALIZE FOR DOWNLOAD LIST                                        *         
***********************************************************************         
         SPACE 1                                                                
DLINIT   DS    0H                                                               
         OI    LSSTAT1,LSSTSAR     LIST IS TSAR RECORDS ONLY                    
         MVI   DWNINDS,DWNNOALL    READ RECORD                                  
         GOTO1 INILST              INITIALIZE LIST                              
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
         CHI   R1,F#ETY#OFFD       TEST OFFICE/OFFICE LIST FIELD                
         BNE   DLSCOL04                                                         
         TM    BCCPYST1,CPYSOROE   ARE WE ON OFFICE BASED AGENCY                
         BZ    DLSCOL20            NO - DON'T SHOW THIS FIELD                   
         B     DLSCOL16                                                         
DLSCOL04 CHI   R1,F#ETY#FNAMED     TEST FOREIGN NAME FIELD                      
*&&UK*&& BNE   DLSCOL06                                                         
*&&US*&& BNE   DLSCOL16                                                         
         CLI   CUCTRY,CTRYGER      IS IT A GERMAN AGENCY?                       
         BNE   DLSCOL20            NO - DON'T SHOW THIS FIELD                   
         B     DLSCOL16                                                         
*&&UK                                                                           
DLSCOL06 CHI   R1,F#ETY#VATCDD     TEST VAT CODE FIELD                          
         BNE   DLSCOL08                                                         
         TM    BCCPYST5,CPYSNVAT   COMPANY USE NEW VAT RULES?                   
         BZ    DLSCOL20            NO - DON'T SHOW THIS FIELD                   
         B     DLSCOL16                                                         
DLSCOL08 CHI   R1,F#ETY#VATACD     TEST VAT ACCOUNT FIELD                       
         BNE   DLSCOL16                                                         
         TM    BCCPYST5,CPYSNVAT   COMPANY USE NEW VAT RULES?                   
         BO    DLSCOL20            YES - DON'T SHOW THIS FIELD                  
*&&                                                                             
*                                                                               
DLSCOL16 XC    0(DCTABL,R5),0(R5)  CLEAR COLUMN ENTRY                           
         MVC   DCTFLD#,0(RF)       SET FIELD NUMBER                             
         MVC   DCTCOL#,MYBYTE      SET COLUMN NUMBER                            
         LLC   R1,MYBYTE                                                        
         AHI   R1,1                                                             
         STC   R1,MYBYTE           NEXT COLUMN NUMBER                           
         LA    R5,DCTABL(,R5)                                                   
DLSCOL20 LA    RF,2(,RF)                                                        
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
DLCLMS   DC    AL2(F#ETY#ETCODD)   EXP TYPE CODE                                
         DC    AL2(F#ETY#ETND)     EXP TYPE NAME                                
         DC    AL2(F#ETY#OFFD)     OFF/OFF LIST                                 
         DC    AL2(F#ETY#FNAMED)   FOREIGN NAME                                 
         DC    AL2(F#ETY#EXCATD)   EXP CATEGORY                                 
*&&UK*&& DC    AL2(F#ETY#VATCDD)   VAT CODE (NEW VAT)                           
*&&UK*&& DC    AL2(F#ETY#VATACD)   VAT ACCOUNT                                  
*&&UK*&& DC    AL2(F#ETY#VATNMD)   VAT NAME                                     
         DC    AL2(F#ETY#APPNDD)   APPROVAL NEEDED                              
         DC    AL2(F#ETY#ADVNCD)   ADVANCE                                      
         DC    AL2(F#ETY#BILLD)    BILLABLE                                     
         DC    AL2(F#ETY#NBILLD)   NON-BILLABLE                                 
         DC    AL2(F#ETY#ETSLKD)   LOCK STATUS                                  
         DC    AL2(F#ETY#ALPIND)   LOCK INVOICES                                
         DC    AL2(F#ETY#ALPORD)   LOCK ORDERS                                  
         DC    AL2(F#ETY#ALPEXD)   LOCK EXPENSES                                
         DC    AL2(F#ETY#SFAPD)    SELF AP ALLOWED                              
         DC    AL2(F#ETY#ETDEF)    DEFAULT SUPPLIER INDICATOR                   
         DC    AL2(F#ETY#ETSAL)    SUPPLIER ACCOUNT                             
         DC    AL2(F#ETY#ETSAN)    SUPPLIER ACC NAME                            
         DC    AL2(F#ETY#EXPDFD)   DEFAULT EXP                                  
         DC    AL2(F#ETY#ETXAL)    EXPENSE ACCOUNT                              
         DC    AL2(F#ETY#ETXAN)    EXPENSE ACC NAME                             
         DC    AL2(F#ETY#WCDDFD)   DEFAULT WC                                   
         DC    AL2(F#ETY#ETWCL)    WORKCODE                                     
         DC    AL2(F#ETY#ETWCD)    WORKCODE DESCRIPTION                         
         DC    AL2(F#ETY#ETWCN)    WORKCODE NAME                                
*&&US*&& DC    AL2(F#ETY#PRVD)     PROVINCE CODE LIST (DOWN)                    
*&&US*&& DC    AL2(F#ETY#PRNMD)    PROVINCE NAME LIST (DOWN)                    
*&&US*&& DC    AL2(F#ETY#PSTCD)    PST CODE LIST (DOWNLOAD)                     
*&&US*&& DC    AL2(F#ETY#PSTND)    PST NAME LIST (DOWNLOAD)                     
DLCLMSX  EQU   (*-DLCLMS)/2                                                     
         SPACE 2                                                                
***********************************************************************         
* SET LIST HEADERS FOR COLUMNS                                        *         
***********************************************************************         
         SPACE 1                                                                
DLSETC   DS    0H                                                               
         OI    DLINDS,DLBALL       BUILD LIST IN ONE GO                         
         B     EXITOK                                                           
         SPACE 2                                                                
FIL43    CSECT                                                                  
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
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   EXITOK                                                           
         CLI   SREC,R#ETYPE        ETYPE RECORD                                 
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         DROP  R2                                                               
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
         USING ETYRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING ETYRECD,R2                                                       
LAST     USING ETYRECD,R3                                                       
         LA    RF,LISTABL                                                       
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMPAGE    CHECK PAGE OK (0 FOR LIST)                   
         BE    LITER04                                                          
         LA    RF,OBJTABL(,RF)                                                  
         B     LITER                                                            
*                                                                               
LITER04  ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
LISTABL  DC    AL1(LDEFCLM),AL1(0,0,0),AL4(DEFCLM)                              
         DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,254),AL4(FLST)                             
         DC    AL1(LGETNEXT),AL1(0,0,254),AL4(NLST)                             
         DC    AL1(LTSARDIR),AL1(0,0,254),AL4(TSARDIR)                          
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LLSTLAST),AL1(0,0,1),AL4(LTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,2),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,2),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,2),AL4(FTFLST1)                            
         DC    AL1(LLSTLAST),AL1(0,0,2),AL4(LTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,2),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,2),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,2),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,2),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,2),AL4(UPDLAST1)                           
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,3),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,3),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,3),AL4(FTFLST1)                            
         DC    AL1(LLSTLAST),AL1(0,0,3),AL4(LTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,3),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,3),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,3),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,3),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,3),AL4(UPDLAST1)                           
*&&US                                                                           
         DC    AL1(LGETFRST),AL1(0,0,4),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,4),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,4),AL4(FTFLST1)                            
         DC    AL1(LLSTLAST),AL1(0,0,4),AL4(LTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,4),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,4),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,4),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,4),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,4),AL4(UPDLAST1)                           
*&&                                                                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DEFINE COLUMNS FOR LIST                                             *         
***********************************************************************         
         SPACE 1                                                                
DEFCLM   CLI   GSSMCODE,C'2'       ON SET 2 DON'T CHANGE BACK                   
         BE    EXITOK                                                           
         CLI   GSSMCODE,C'3'       ON SET 3 DON'T CHANGE BACK                   
         BE    EXITOK                                                           
         MVI   GSSMCODE,0                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INIT LIST                                                           *         
***********************************************************************         
         SPACE 1                                                                
ILST     GOTO1 INILST              INITIALIZE LIST                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING ETYRECD,IOKEY                                                    
FLST     MVC   X.ETYKEY,THIS.ETYKEY                                             
         LHI   R1,XOHI+XOACCDIR+XIO11                                           
         GOTO1 AIO                                                              
         BE    NLST04                                                           
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BNE   *+12                                                             
         TM    DWNINDS,DWNNOALL    NO MORE LIST ELEMENTS?                       
         BNO   NLST06              NO - GET NEXT LIST ELEMENTS                  
NLST02   LHI   R1,XOSQ+XOACCDIR+XIO11                                           
         GOTO1 AIO                                                              
         BE    NLST04                                                           
         B     EXITL                                                            
*                                                                               
NLST04   CLC   X.ETYKEY(ETYKREM-ETYRECD),THIS.ETYKEY                            
         BNE   EXITL               CHANGE OF COMPANY                            
         MVI   DWNINDS,DWNGDATA    GET DATA INTO TSAR                           
*                                                                               
NLST06   MVC   THIS.ETYKEY(ACCKLEN),IOKEY  WE WANT THIS KEY HERE...             
         CLI   CSACT,A#DLOAD               DOWNLOADING?                         
         BNE   *+12                                                             
         CLI   DWNINDS,DWNGDATA    FIRST TIME READING RECORD?                   
         BNE   EXITOK              NO - SKIP CALLING DOFLT                      
*                                                                               
         NI    LSTIND,FF-LSTOK                                                  
         GOTO1 DOFLT                                                            
         BNE   NLSTXX                                                           
         CLC   X.ETYKOFFC,BCSPACES NOT LIMITED BY OFFICE                        
         BNH   EXITOK                                                           
         CLI   CUACCS,0            GLOBAL LOGON                                 
         BE    EXITOK              THEN CAN SEE EVERYTHING                      
*                                                                               
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   NLST14                                                           
         CLI   CUACCS,C'$'         LIMIT LIST LOGON                             
         BNE   NLST08                                                           
         CLI   ETYKOFFC,C'$'       OFFICE LIST EXP TYPE?                        
         BNE   NLST08                                                           
         CLC   CUACCS(2),ETYKOFFC  MUST BE SAME AS LOGON                        
         BE    EXITOK                                                           
         B     NLSTXX                                                           
*                                                                               
NLST08   CLI   ETYKOFFC,C'$'       OFFICE LIST EXP TYPE?                        
         BNE   NLST12                                                           
         LA    R4,SVOFFLS                                                       
         LA    R5,SVOFFLS+L'SVOFFLS                                             
*                                                                               
NLST10   CR    R4,R5                                                            
         BNL   NLST12                                                           
         CLC   0(L'OFLPOFL,R4),BCSPACES                                         
         BNH   NLST12              CHECK WHETHER MATCH ON OFFICE LIST           
         CLC   0(L'OFLPOFL,R4),ETYKOFFC                                         
         BE    EXITOK                                                           
         AHI   R4,L'OFLPOFL                                                     
         B     NLST10                                                           
*                                                                               
NLST12   GOTO1 ATSTOFF,ETYKOFFC    CHECK OFFICE IS VALID                        
         BNE   NLSTX                                                            
         OI    LSTIND,LSTOK                                                     
         B     NLSTX                                                            
*                                                                               
         USING OFFALD,R1                                                        
NLST14   TM    LSTIND,OFFLIS          CHECK WHETHER MATCH ON OFFICE             
         BZ    NLST16                 LIST                                      
         CLC   CUACCS+2(2),ETYKOFFC                                             
         BNE   NLST16                                                           
         B     EXITOK                                                           
*                                                                               
NLST16   LA    R4,SVOFFLS                                                       
         LA    R5,SVOFFLS+L'SVOFFLS                                             
*                                                                               
NLST18   CR    R4,R5                                                            
         BNL   NLST20                                                           
         CLC   0(L'OFLPOFL,R4),BCSPACES                                         
         BNH   NLST20              CHECK WHETHER MATCH ON OFFICE LIST           
         CLC   0(L'OFLPOFL,R4),ETYKOFFC                                         
         BE    EXITOK                                                           
         AHI   R4,L'OFLPOFL                                                     
         B     NLST18                                                           
*                                                                               
NLST20   GOTO1 ATSTOFF,ETYKOFFC       CHECK WHETHER OFFICE IS VALID             
         BNE   NLSTX                                                            
         OI    LSTIND,LSTOK                                                     
*                                                                               
NLSTX    MVC   FVXTRA,BCSPACES                                                  
         LHI   R1,XORD+XOACCDIR+XIO11 RESTORE READ SEQUENCE                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    LSTIND,LSTOK                                                     
         BNZ   EXITOK                                                           
*                                                                               
NLSTXX   B     NLST02                                                           
         DROP  THIS,LAST,X                                                      
         SPACE 2                                                                
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM FILE                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ETYRECD,R2                                                       
         USING TLSTD,R3                                                         
TSARDIR  LM    R2,R3,SVPARMS3                                                   
*                                                                               
         MVC   TLKETC,ETYKCODE                                                  
         MVC   TLKOFF,ETYKOFFC                                                  
         L     R2,AIOREC                                                        
*                                                                               
         TM    DWNINDS,DWNGDATA     GET DATA FROM RECORD                        
         BZ    TSDIR30              NO - PUT DATA INTO TSAR                     
         LHI   R1,XOGET+XOACCMST+XIO11                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
*                                                                               
         MVC   SVDAT(SVDATLQ),BCSPACES                                          
         LA    R4,ETYRFST                                                       
         XR    R0,R0                                                            
TSDIR04  CLI   0(R4),0                                                          
         BE    TSDIR20                                                          
         CLI   0(R4),NAMELQ                                                     
         BE    TSDIR10                                                          
         CLI   0(R4),FFTELQ                                                     
         BE    TSDIR12                                                          
         CLI   0(R4),SPAELQ                                                     
         BE    TSDIR14                                                          
         CLI   0(R4),ENMELQ                                                     
         BE    TSDIR16                                                          
         CLI   0(R4),XNMELQ                                                     
         BE    TSDIR18                                                          
TSDIR08  IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     TSDIR04                                                          
*                                                                               
         USING NAMELD,R4                                                        
TSDIR10  LLC   RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1        RE=LENGTH OF NAME - 1                        
         BM    TSDIR08                                                          
         MVC   SVETN(0),NAMEREC    EXPENDITURE TYPE NAME                        
         EX    RF,*-6                                                           
         B     TSDIR08                                                          
         DROP  R4                                                               
*                                                                               
         USING FFTELD,R4                                                        
TSDIR12  CLI   FFTTYPE,FFTTVATC                                                 
         BNE   TSDIR08                                                          
         LLC   RF,FFTDLEN                                                       
         SHI   RF,1                                                             
         BM    TSDIR08                                                          
         MVC   SVVATC(0),FFTDATA   VAT CODE                                     
         EX    RF,*-6                                                           
         B     TSDIR08                                                          
         DROP  R4                                                               
*                                                                               
         USING SPAELD,R4                                                        
TSDIR14  CLI   SPATYPE,SPATITAX                                                 
         BNE   TSDIR08                                                          
         MVC   SVVATA,SPAAACT      VAT ACCOUNT                                  
         B     TSDIR08                                                          
         DROP  R4                                                               
*                                                                               
         USING ENMELD,R4                                                        
TSDIR16  CLI   ENMLN,ENMLNQ                                                     
         BNH   TSDIR08                                                          
         LLC   RF,ENMLN                                                         
         SHI   RF,ENMLNQ+1                                                      
         MVC   SVFNM(0),ENMNAME    FOREIGN NAME (GERMAN ONLY)                   
         EX    RF,*-6                                                           
         B     TSDIR08                                                          
         DROP  R4                                                               
*                                                                               
         USING XNMELD,R4                                                        
TSDIR18  CLI   XNMLN,XNMLN1Q                                                    
         BNH   TSDIR08                                                          
         LLC   RF,XNMSUBL                                                       
         SHI   RF,1                                                             
         MVC   SVCAT(0),XNMSUBN    EXPENDITURE TYPE CATEGORY                    
         EX    RF,*-6                                                           
         B     TSDIR08                                                          
         DROP  R4                                                               
*                                                                               
TSDIR20  XC    MNTDISPD(MNTDISPL),MNTDISPD                                      
         GOTO1 GETNLE,BOPARM,('LIDTSUPP',MNTSUP),ETYRECD                        
         BE    *+8                                                              
         OI    DWNINDS,DWNNOSUP                                                 
         GOTO1 GETNLE,BOPARM,('LIDTEXPS',MNTEXP),ETYRECD                        
         BE    *+8                                                              
         OI    DWNINDS,DWNNOEXP                                                 
         GOTO1 GETNLE,BOPARM,('LIDTWKCD',MNTWCD),ETYRECD                        
         BE    *+8                                                              
         OI    DWNINDS,DWNNOWCD                                                 
*&&US                                                                           
         GOTO1 GETNLE,BOPARM,('LIDTGSTR',MNTPRV),ETYRECD                        
         BE    *+8                                                              
         OI    DWNINDS,DWNNOPRV                                                 
*&&                                                                             
*                                                                               
TSDIR30  NI    DWNINDS,FF-DWNGDATA                                              
         MVC   TLRLEN,=AL2(TLDLLNQ) LENGTH OF TSAR RECORD FOR DOWNLOAD          
*                                                                               
         XC    TLDLDAT(TLDLDATL),TLDLDAT                                        
         MVC   TLDSTAT,ETYRSTAT                                                 
         MVC   TLDSTA2,ETYRSTA2                                                 
         MVC   TLDSAPP,ETYRSAPP                                                 
         MVC   TLDVATC,SVVATC                                                   
         MVC   TLDVATA,SVVATA                                                   
         MVC   TLDCAT,SVCAT                                                     
         MVC   TLDETN,SVETN                                                     
         MVC   TLDFNM,SVFNM                                                     
*                                                                               
         TM    DWNINDS,DWNNOSUP                                                 
         BO    TSDIR34                                                          
         GOTO1 GETLIT,BOPARM,('LIDTSUPP',MNTSUP),ETYRECD                        
         BE    *+8                                                              
         OI    DWNINDS,DWNNOSUP                                                 
         L     RF,8(R1)                                                         
         MVC   TLDSA,0(RF)                                                      
*                                                                               
TSDIR34  TM    DWNINDS,DWNNOEXP                                                 
         BO    TSDIR38                                                          
         GOTO1 GETLIT,BOPARM,('LIDTEXPS',MNTEXP),ETYRECD                        
         BE    *+8                                                              
         OI    DWNINDS,DWNNOEXP                                                 
         L     RF,8(R1)                                                         
         MVC   TLDXA,0(RF)                                                      
*                                                                               
TSDIR38  TM    DWNINDS,DWNNOWCD                                                 
         BO    TSDIR40                                                          
         GOTO1 GETLIT,BOPARM,('LIDTWKCD',MNTWCD),ETYRECD                        
         BE    *+8                                                              
         OI    DWNINDS,DWNNOWCD                                                 
         L     RF,8(R1)                                                         
         MVC   TLDWC,0(RF)                                                      
*                                                                               
TSDIR40  DS    0H                                                               
*&&US                                                                           
         TM    DWNINDS,DWNNOPRV                                                 
         BO    TSARDIRX                                                         
         GOTO1 GETLIT,BOPARM,('LIDTGSTR',MNTPRV),ETYRECD                        
         BE    *+8                                                              
         OI    DWNINDS,DWNNOPRV                                                 
         L     RF,8(R1)                                                         
         MVC   TLDPRVC,0(R1)                                                    
         MVC   TLDPSTC,L'TLDPRVC(R1)                                            
*&&                                                                             
TSARDIRX B     EXITOK                                                           
         DROP  R2,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1/2/3/4                                 (LINIT) *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         OI    LSSTAT2,LSSADD+LSSNOSEQ                                          
         MVC   LSCOLLIN,=AL2(78)  NUMBER OF COLUMNS PER LIST LINE               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR LIST 1/2/3/4                              (LLSTLAST)  *         
***********************************************************************         
         SPACE 1                                                                
LTFLST1  LH    RF,LS1STLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1/2/3/4                              (LLSTFRST) *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  CLI   GSSMPAGE,WCLISTQ                                                 
         BNE   *+14                                                             
         XC    WCLBLK,WCLBLK                                                    
         B     FTFL10                                                           
*                                                                               
         CLI   GSSMPAGE,XALISTQ                                                 
         BNE   FTFL00                                                           
         LA    R0,XALBLK           EXPENSE BLOCK IS LONGER                      
         LHI   R1,XALBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     FTFL10                                                           
*                                                                               
FTFL00   CLI   GSSMPAGE,SALISTQ                                                 
         BNE   FTFL02                                                           
         LA    R0,SALBLK           SUPPLIER BLOCK IS LONGER                     
         LHI   R1,SALBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         B     FTFL10                                                           
*                                                                               
FTFL02   DS    0H                                                               
*&&US                                                                           
         CLI   GSSMPAGE,PRLISTQ                                                 
         BNE   FTFL10                                                           
         LA    R0,PRVBLK           PROVINCE BLOCK IS LONGER                     
         LHI   R1,PRVBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*&&                                                                             
FTFL10   LA    RF,ETYRFST-ETYRECD                                               
         STH   RF,DISPGEN                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1/2/3                                     (LGETFRST) *         
* BUILD WCLBLK/XALBLK/SALBLK/PRVBLK FROM ETYPE RECORD LIDELS          *         
***********************************************************************         
         SPACE 1                                                                
         USING ETYRECD,R2                                                       
FLST1    XR    R0,R0                                                            
         MVI   ANYLIST,NO                                                       
         CLI   GSSMPAGE,WCLISTQ    WORKCODE LIST?                               
         BNE   *+12                                                             
         LA    RF,WCLBLK                                                        
         ST    RF,WCLDISP                                                       
         CLI   GSSMPAGE,XALISTQ    EXPENSE LIST?                                
         BNE   *+12                                                             
         LA    RF,XALBLK                                                        
         ST    RF,XALDISP                                                       
         CLI   GSSMPAGE,SALISTQ    SUPPLIER LIST?                               
         BNE   *+12                                                             
         LA    RF,SALBLK                                                        
         ST    RF,SALDISP                                                       
*&&US                                                                           
         CLI   GSSMPAGE,PRLISTQ    PROVINCE LIST?                               
         BNE   *+12                                                             
         LA    RF,PRVBLK                                                        
         ST    RF,PRVDISP                                                       
*&&                                                                             
*                                                                               
         ST    RF,SBLKDIS          INIT DISP INTO CURRENT BLK                   
         LH    RF,DISPGEN          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         C     RF,AIOREC           MAKE SURE DISPGEN INITIALISED                
         BH    *+8                                                              
         LA    RF,ETYRFST(RF)                                                   
*                                                                               
         USING LIDELD,RF                                                        
FLS02    CLI   LIDEL,0             RECORD END?                                  
         BE    FLSEX               YES                                          
         CLI   LIDEL,LIDELQ        LIST DATA ELEMENT                            
         BE    FLS06               YES                                          
                                                                                
FLS04    IC    R0,LIDLN                                                         
         AR    RF,R0                                                            
         B     FLS02                                                            
*                                                                               
FLS06    XR    RE,RE                                                            
*                                                                               
         CLI   LIDTYPE,LIDTWKCD    TEST WORKCODE LIST                           
         BNE   FLS08                                                            
         CLI   GSSMPAGE,WCLISTQ    TEST WORKCODE LIST PAGE                      
         BNE   FLS04                                                            
         IC    RE,LIDLN                                                         
         AHI   RE,-(LIDDATA-LIDELD+1)                                           
         MVC   WCLBLK(0),LIDDATA   COPY TO WORKCODE LIST BLOCK                  
         EX    RE,*-6                                                           
         MVI   ANYLIST,YES                                                      
         B     FLSEX               EXIT                                         
*                                                                               
FLS08    CLI   LIDTYPE,LIDTEXPS    TEST EXPENSE ACCOUNT LIST                    
         BNE   FLS10                                                            
         CLI   GSSMPAGE,XALISTQ    TEST EXPENSE ACCOUNT LIST PAGE               
         BNE   FLS04                                                            
         IC    RE,LIDLN                                                         
         AHI   RE,-(LIDDATA-LIDELD+1)                                           
         L     R1,SBLKDIS                                                       
         MVC   0(0,R1),LIDDATA     COPY TO EXPENSE ACC LIST BLOCK               
         EX    RE,*-6                                                           
         LA    R1,1(RE,R1)                                                      
         ST    R1,SBLKDIS          UPDATE DISP INTO BLK                         
         MVI   ANYLIST,YES                                                      
         B     FLS04               MAYBE > 1 EXP ACCOUNT LIDEL                  
*                                                                               
FLS10    CLI   LIDTYPE,LIDTSUPP    TEST SUPPLIER ACCOUNT LIST                   
         BNE   FLS12                                                            
         CLI   GSSMPAGE,SALISTQ    TEST SUPPLIER ACCOUNT LIST PAGE              
         BNE   FLS04                                                            
         IC    RE,LIDLN                                                         
         AHI   RE,-(LIDDATA-LIDELD+1)                                           
         L     R1,SBLKDIS                                                       
         MVC   0(0,R1),LIDDATA     COPY TO SUPPLIER ACC LIST BLOCK              
         EX    RE,*-6                                                           
         LA    R1,1(RE,R1)                                                      
         ST    R1,SBLKDIS          UPDATE DISP INTO BLK                         
         MVI   ANYLIST,YES                                                      
         B     FLS04               MAYBE > 1 SUPPLIER LIDEL                     
*                                                                               
FLS12    DS    0H                                                               
*&&US                                                                           
         CLI   LIDTYPE,LIDTGSTR    TEST GST/PST TAX LIST                        
         BNE   FLS04                                                            
         CLI   GSSMPAGE,PRLISTQ    TEST PROVINCE LIST PAGE                      
         BNE   FLS04                                                            
         IC    RE,LIDLN                                                         
         AHI   RE,-(LIDDATA-LIDELD+1)                                           
         L     R1,SBLKDIS                                                       
         MVC   0(0,R1),LIDDATA     COPY TO PROVINCE LIST BLOCK                  
         EX    RE,*-6                                                           
         LA    R1,1(RE,R1)                                                      
         ST    R1,SBLKDIS          UPDATE DISP INTO BLK                         
         MVI   ANYLIST,YES                                                      
*&&                                                                             
         B     FLS04                                                            
*                                                                               
FLSEX    CLI   ANYLIST,YES                                                      
         BNE   EXITL               NO LIST FOR THIS PAGE                        
         B     EXITOK              ELSE OK                                      
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* NEXT FOR LIST 1/2/3/4                                    (LGETNEXT) *         
* UPDATE WCL/XAL/SAL/PRV DISPLACEMENTS                                *         
***********************************************************************         
         SPACE 1                                                                
NLST1    MVI   ANYLIST,NO                                                       
*                                                                               
         CLI   GSSMPAGE,WCLISTQ    TEST WORKCODE LIST PAGE                      
         BNE   NLST102                                                          
         L     RF,WCLDISP                                                       
         LA    RF,L'TLKWC(RF)      BUMP TO NEXT ENTRY                           
         OC    0(L'TLKWC,RF),0(RF) TEST END OF W/C LIST                         
         BZ    NLSEX                                                            
         ST    RF,WCLDISP                                                       
         MVI   ANYLIST,YES                                                      
         B     NLSEX                                                            
*                                                                               
NLST102  CLI   GSSMPAGE,XALISTQ    TEST EXPENSE ACCOUNT LIST                    
         BNE   NLST104                                                          
         L     RF,XALDISP                                                       
         LA    RF,L'TLKXA(RF)      BUMP TO NEXT ENTRY                           
         OC    0(L'TLKXA,RF),0(RF) TEST END OF EXPENSE ACCOUNT LIST             
         BZ    NLSEX                                                            
         ST    RF,XALDISP                                                       
         MVI   ANYLIST,YES                                                      
         B     NLSEX                                                            
*                                                                               
NLST104  CLI   GSSMPAGE,SALISTQ    TEST SUPPLIER ACCOUNT LIST                   
         BNE   NLST106                                                          
         L     RF,SALDISP                                                       
         LA    RF,L'TLKSA(RF)      BUMP TO NEXT ENTRY                           
         OC    0(L'TLKSA,RF),0(RF) TEST END OF SUPPLIER ACCOUNT LIST            
         BZ    NLSEX                                                            
         ST    RF,SALDISP                                                       
         MVI   ANYLIST,YES                                                      
         B     NLSEX                                                            
*                                                                               
NLST106  DS    0H                                                               
*&&US                                                                           
         CLI   GSSMPAGE,PRLISTQ    TEST PROVINCE LIST PAGE                      
         BNE   NLSEX                                                            
         L     RF,PRVDISP                                                       
         LA    RF,LIDGSTLQ(,RF)      BUMP TO NEXT ENTRY                         
         OC    0(L'TLKPRV,RF),0(RF)  TEST END OF SUPPLIER ACCOUNT LIST          
         BZ    NLSEX                                                            
         ST    RF,PRVDISP                                                       
         MVI   ANYLIST,YES                                                      
         B     NLSEX                                                            
*&&                                                                             
NLSEX    CLI   ANYLIST,YES         TEST ANYTHING MORE TO SHOW                   
         BE    EXITOK                                                           
         B     EXITL               NO - FINISHED                                
         EJECT ,                                                                
***********************************************************************         
* SET UP TSAR RECORD:                                      (LTSARFIL) *         
* TAKE AN ENTRY FROM WC/XA/SALBLK ACCORDING TO LIST PAGE              *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)                                               
*                                                                               
         CLI   GSSMPAGE,WCLISTQ    TEST WORKCODE LIST PAGE                      
         BNE   TSARFI02                                                         
         L     RF,WCLDISP          DISPLACE INTO WC LIST BLOCK                  
         MVC   TLKWC,0(RF)                                                      
         B     EXITOK                                                           
*                                                                               
TSARFI02 CLI   GSSMPAGE,XALISTQ    TEST EXPENSE ACCOUNT LIST PAGE               
         BNE   TSARFI04                                                         
         L     RF,XALDISP          DISPLACE INTO EXPENSE AC LIST BLK            
         MVC   TLKXA,0(RF)                                                      
         B     EXITOK                                                           
*                                                                               
TSARFI04 CLI   GSSMPAGE,SALISTQ    TEST SUPPLIER ACCOUNT LIST PAGE              
         BNE   TSARFI06                                                         
         L     RF,SALDISP          DISPLACE INTO SUPPLIER AC LIST BLK           
         MVC   TLKSA,0(RF)                                                      
         B     EXITOK                                                           
*                                                                               
TSARFI06 DS    0H                                                               
*&&US                                                                           
         CLI   GSSMPAGE,PRLISTQ    TEST PROVINCE/PST LIST PAGE                  
         BNE   EXITOK                                                           
         L     RF,PRVDISP          DISPLACE INTO PROVINCE LIST BLK              
         MVC   TLKPRV,0(RF)                                                     
         MVC   TLAPST,L'TLKPRV(RF)                                              
*&&                                                                             
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1/2/3/4                            (LUPDFRST) *         
* CLEAR WCLBLK/XALBLK/SALBLK/PRVBLK TO REBUILD FROM TSAR RECORDS      *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 CLI   CSACT,A#CHA         TEST ACTION IS CHANGE                        
         BE    *+8                                                              
         CLI   CSACT,A#ADD                     OR ADD                           
         BNE   EXITOK                                                           
*                                                                               
         MVI   ANYINPUT,NO                                                      
         CLI   GSSMPAGE,WCLISTQ                                                 
         BNE   UPDF02                                                           
         XC    WCLBLK,WCLBLK       CLEAR BLOCKS FOR RE-BUILDING LIDELS          
         LA    RF,WCLBLK                                                        
         ST    RF,WCLDISP                                                       
         B     EXITOK                                                           
*                                                                               
UPDF02   CLI   GSSMPAGE,XALISTQ                                                 
         BNE   UPDF04                                                           
         LA    R0,XALBLK                                                        
         LHI   R1,XALBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    RF,XALBLK                                                        
         ST    RF,XALDISP                                                       
         B     EXITOK                                                           
*                                                                               
UPDF04   CLI   GSSMPAGE,SALISTQ                                                 
         BNE   UPDF06                                                           
         LA    R0,SALBLK                                                        
         LHI   R1,SALBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    RF,SALBLK                                                        
         ST    RF,SALDISP                                                       
         B     EXITOK                                                           
*                                                                               
UPDF06   DS    0H                                                               
*&&US                                                                           
         CLI   GSSMPAGE,PRLISTQ                                                 
         BNE   EXITOK                                                           
         LA    R0,PRVBLK                                                        
         LHI   R1,PRVBLKL                                                       
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
         LA    RF,PRVBLK                                                        
         ST    RF,PRVDISP                                                       
*&&                                                                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* UPDATE WC/XA/SA BLOCKS FROM TSAR RECORD 1/2/3/4           (LUPDREC) *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         TEST ACTION IS CHANGE                        
         BE    *+8                                                              
         CLI   CSACT,A#ADD                     OR ADD                           
         BNE   EXITOK                                                           
*                                                                               
         L     R3,SVPARMS4                                                      
         CLI   GSSMPAGE,WCLISTQ    TEST WORKCODE LIST PAGE                      
         BNE   UPDR02                                                           
         CLC   TLKWC,BCSPACES      IF NON-BLANK                                 
         BNH   UPDREX                                                           
         L     RF,WCLDISP                                                       
         LA    RE,WCLBLK+L'WCLBLK-L'TLKWC                                       
         CR    RF,RE               TEST ROOM FOR ANOTHER LIST ENTRY             
         BH    UPDRERR                                                          
         MVC   0(L'TLKWC,RF),TLKWC ADD TO LIST BLOCK                            
         LA    RF,L'TLKWC(RF)      BUMP FOR NEXT TIME                           
         ST    RF,WCLDISP                                                       
         MVI   ANYINPUT,YES                                                     
         B     UPDREX                                                           
*                                                                               
UPDR02   CLI   GSSMPAGE,XALISTQ    TEST EXPENSE ACCOUNT LIST PAGE               
         BNE   UPDR04                                                           
         CLC   TLKXA,BCSPACES                                                   
         BNH   UPDREX                                                           
         L     RF,XALDISP                                                       
         LA    RE,XALBLK+L'XALBLK-L'TLKXA                                       
         CR    RF,RE               TEST ROOM FOR ANOTHER LIST ENTRY             
         BH    UPDRERR                                                          
         MVC   0(L'TLKXA,RF),TLKXA                                              
         LA    RF,L'TLKXA(RF)                                                   
         ST    RF,XALDISP                                                       
         MVI   ANYINPUT,YES                                                     
         B     UPDREX                                                           
*                                                                               
UPDR04   CLI   GSSMPAGE,SALISTQ    TEST SUPPLIER ACCOUNT LIST PAGE              
         BNE   UPDR06                                                           
         CLC   TLKSA,BCSPACES                                                   
         BNH   UPDREX                                                           
         L     RF,SALDISP                                                       
         LA    RE,SALBLK+SALBLKL-L'TLKSA                                        
         CR    RF,RE               TEST ROOM FOR ANOTHER LIST ENTRY             
         BH    UPDRERR                                                          
         MVC   0(L'TLKSA,RF),TLKSA                                              
         LA    RF,L'TLKSA(RF)                                                   
         ST    RF,SALDISP                                                       
         MVI   ANYINPUT,YES                                                     
         B     UPDREX                                                           
*                                                                               
UPDR06   DS    0H                                                               
*&&US                                                                           
         CLI   GSSMPAGE,PRLISTQ    TEST PROVINCE LIST PAGE                      
         BNE   UPDREX                                                           
         CLC   TLKPRV,BCSPACES                                                  
         BNH   UPDREX                                                           
         L     RF,PRVDISP                                                       
         LAY   RE,PRVBLK+PRVBLKL-L'TLKPRV                                       
         CR    RF,RE               TEST ROOM FOR ANOTHER LIST ENTRY             
         BH    UPDRERR                                                          
         MVC   0(L'TLKPRV,RF),TLKPRV                                            
         MVC   L'TLKPRV(L'TLAPST,RF),TLAPST                                     
         LA    RF,LIDGSTLQ(,RF)                                                 
         ST    RF,PRVDISP                                                       
         MVI   ANYINPUT,YES                                                     
         B     UPDREX                                                           
*&&                                                                             
UPDREX   B     EXITOK                                                           
UPDRERR  MVC   FVMSGNO,=AL2(AE$TMILS) TOO MANY LINES IN LIST                    
         LH    RF,LSCURLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO CURRENT LINE                   
         NI    LSLTIND1,FF-(LSLTIBLD) REBUILD THE LIST                          
         B     EXITL                                                            
         SPACE 1                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                   (LUPDLAST) *         
* DELETE/RE-ADD LIDELS USING WCL/XA/SA/PRVBLK BUILT IN UPDREC1        *         
* P3 = A(FILE RECORD)                                                 *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 XC    BOELEM,BOELEM                                                    
*&&US*&& CLI   GSSMPAGE,PRLISTQ    SKIP PROVINCE LIST DOESN'T HAVE              
*&&US*&& BE    UPDLA04             DEFAULT                                      
         LA    RE,L'TLKWC-1        DO FINAL VALIDATION                          
         LA    RF,WCLBLK                                                        
         CLI   GSSMPAGE,WCLISTQ    ACCORDING TO LIST PAGE                       
         BE    UPDLA00                                                          
         LA    RE,L'TLKXA-1                                                     
         LA    RF,XALBLK                                                        
         CLI   GSSMPAGE,XALISTQ                                                 
         BE    UPDLA00                                                          
         LA    RE,L'TLKSA-1                                                     
         LA    RF,SALBLK                                                        
         CLI   GSSMPAGE,SALISTQ                                                 
         BE    UPDLA00                                                          
         DC    H'0'                                                             
         TM    0(RF),X'40'         TEST ANY DEFAULT - ALWAYS 1ST ENTRY          
         BNZ   UPDLA04             NO - OK                                      
UPDLA00  MVC   BOWORK1(0),0(RF)    SAVE 1ST ENTRY                               
         EX    RE,*-6                                                           
         OI    BOWORK1,X'40'       RESTORE X'40' BIT FOR COMPARE                
*                                                                               
UPDLA01  LA    RF,1(RE,RF)                                                      
         CLI   0(RF),0             TEST END OF BLOCK                            
         BNH   UPDLA04             YES - OK                                     
         EX    RE,*+8                                                           
         BE    UPDLAER1            SAME ENTRY WITH/OUT DEFAULT IND              
         CLC   BOWORK1(0),0(RF)                                                 
         TM    0(RF),X'40'                                                      
         BZ    UPDLAER2            > 1 DEFAULT - INVALID                        
         B     UPDLA01                                                          
UPDLAER1 MVC   FVMSGNO,=AL2(AE$DUPAC)                                           
         B     *+10                                                             
UPDLAER2 MVC   FVMSGNO,=AL2(AE$ODPC)                                            
         B     *+10                                                             
UPDLA02  MVC   FVMSGNO,=AL2(AE$NLINE) ELSE ERROR                                
         LH    RF,LS1STLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         CLI   CSACT,A#ADD                                                      
         BE    EXITL                                                            
         CLI   CSACT,A#CHA                                                      
         BNE   EXITOK                                                           
         NI    LSLTIND1,FF-LSLTIBLD REBUILD THE LIST                            
         XC    GCLASKEY,GCLASKEY    SET KEY HAS BEEN CHANGED                    
         NI    GSINDSL1,FF-GSIXMNT  TURN OF MAINT SCREEN LOADED FLAG            
         B     EXITL                                                            
*                                                                               
UPDLA04  CLI   CSACT,A#CHA         ONLY UPDATE ON CHANGE/ADD                    
         BE    *+8                                                              
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
*                                                                               
UPDLA06  GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),0                
         ORG   *-2                                                              
*                                  INCLUDE SEARCH ARG TO HELLO PARMLIST         
*                                  TO ENSURE CORRECT LIDEL IS DELETED           
         CLI   GSSMPAGE,WCLISTQ    WORKCODE LIST PAGE                           
         BNE   *+12                                                             
         LA    RE,=AL1(L'TLKWC,LIDTWKCD)                                        
         B     UPDLA08                                                          
         CLI   GSSMPAGE,XALISTQ    EXPENSE ACCOUNT LIST PAGE                    
         BNE   *+12                                                             
         LA    RE,=AL1(L'TLKXA,LIDTEXPS)                                        
         B     UPDLA08                                                          
         CLI   GSSMPAGE,SALISTQ    SUPPLIER ACCOUNT LIST PAGE                   
         BNE   *+12                                                             
         LA    RE,=AL1(L'TLKSA,LIDTSUPP)                                        
         B     UPDLA08                                                          
*&&US                                                                           
         CLI   GSSMPAGE,PRLISTQ    PROVINCE LIST PAGE                           
         BNE   *+12                                                             
         LA    RE,=AL1(LIDGSTLQ,LIDTGSTR)                                       
         B     UPDLA08                                                          
*&&                                                                             
UPDLA08  ST    RE,8(R1)            SET A(ARG)                                   
         MVI   8(R1),2             AND L'ARG                                    
         BASR  RE,RF               CALL HELLO                                   
*                                                                               
         USING LIDELD,BOELEM                                                    
         XC    BOELEM,BOELEM                                                    
         MVI   LIDEL,LIDELQ                                                     
         CLI   GSSMPAGE,WCLISTQ                                                 
         BNE   UPDLA12                                                          
         OC    WCLBLK(L'TLKWC),WCLBLK                                           
         BZ    UPDLA12             TEST ANY WORKCODES TO ADD                    
         MVI   LIDITLN,L'TLKWC     LIST ITEM LENGTH                             
         MVI   LIDTYPE,LIDTWKCD    SET LIST TYPE                                
UPDLA10  LA    RE,WCLBLK                                                        
         L     RF,WCLDISP                                                       
         SR    RF,RE                                                            
         AHI   RF,-1                                                            
         MVC   LIDDATA(0),WCLBLK                                                
         EX    RF,*-6                                                           
         LA    RF,(LIDDATA-LIDELD)+1(RF)                                        
         STC   RF,LIDLN                                                         
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BNE   UPDLAERR                                                         
         B     UPDLAEX                                                          
*                                                                               
UPDLA12  CLI   GSSMPAGE,XALISTQ                                                 
         BNE   UPDLA16                                                          
         OC    XALBLK(L'TLKXA),XALBLK                                           
         BZ    UPDLA16             TEST ANY EXPENSE ACCS TO ADD                 
         MVI   LIDITLN,L'TLKXA     LIST ITEM LENGTH                             
         MVI   LIDTYPE,LIDTEXPS    SET LIST TYPE                                
         LA    RE,XALBLK                                                        
         LR    R0,RE                                                            
UPDLA14  L     RF,XALDISP          A(END OF BLOCK)                              
         SR    RF,RE                                                            
         CHI   RF,XALMAX*L'TLKXA   TEST TOO MANY FOR ONE LIDEL                  
         BH    *+10                YES                                          
         SR    R0,R0               ELSE CLEAR SAVED A(NEXT CHUNK)...            
         B     *+10                ...AS THIS WILL BE THE LAST                  
         LHI   RF,XALMAX*L'TLKXA   SET MAX                                      
         AR    R0,RF               R0=A(NEXT CHUNK)                             
         AHI   RF,-1                                                            
         MVC   LIDDATA(0),0(RE)                                                 
         EX    RF,*-6                                                           
         LA    RF,(LIDDATA-LIDELD)+1(RF)                                        
         STC   RF,LIDLN                                                         
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BNE   UPDLAERR                                                         
         LTR   RE,R0               LOAD/TEST A(NEXT CHUNK)                      
         BZ    UPDLAEX                                                          
         XC    LIDDATA(255-(LIDDATA-LIDELD)),LIDDATA                            
         B     UPDLA14             GO ROUND AGAIN                               
*                                                                               
UPDLA16  CLI   GSSMPAGE,SALISTQ                                                 
         BNE   UPDLA20                                                          
         OC    SALBLK(L'TLKSA),SALBLK                                           
         BZ    UPDLAEX             TEST ANY SUPPLIER ACCS TO ADD                
         MVI   LIDITLN,L'TLKSA     LIST ITEM LENGTH                             
         MVI   LIDTYPE,LIDTSUPP    SET LIST TYPE                                
         LA    RE,SALBLK                                                        
         LR    R0,RE                                                            
UPDLA18  L     RF,SALDISP          A(END OF BLOCK)                              
         SR    RF,RE                                                            
         CHI   RF,SALMAX*L'TLKSA   TEST TOO MANY FOR ONE LIDEL                  
         BH    *+10                YES                                          
         SR    R0,R0               ELSE CLEAR SAVED A(NEXT CHUNK)...            
         B     *+10                ...AS THIS WILL BE THE LAST                  
         LHI   RF,SALMAX*L'TLKSA   SET MAX                                      
         AR    R0,RF               R0=A(NEXT CHUNK)                             
         AHI   RF,-1                                                            
         MVC   LIDDATA(0),0(RE)                                                 
         EX    RF,*-6                                                           
         LA    RF,(LIDDATA-LIDELD)+1(RF)                                        
         STC   RF,LIDLN                                                         
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BNE   UPDLAERR                                                         
         LTR   RE,R0               LOAD/TEST A(NEXT CHUNK)                      
         BZ    UPDLAEX             FINISHED                                     
         XC    LIDDATA(255-(LIDDATA-LIDELD)),LIDDATA                            
         B     UPDLA18             GO ROUND AGAIN                               
*                                                                               
UPDLA20  DS    0H                                                               
*&&US                                                                           
         CLI   GSSMPAGE,PRLISTQ    PROVINCE LIST                                
         BNE   UPDLAEX                                                          
         OC    PRVBLK(LIDGSTLQ),PRVBLK                                          
         BZ    UPDLAEX             TEST ANY PROVINCE CODE TO ADD                
*        BZ    UPDLA24             TEST ANY PROVINCE CODE TO ADD                
         MVI   LIDITLN,LIDGSTLQ    LIST ITEM LENGTH                             
         MVI   LIDTYPE,LIDTGSTR    SET LIST TYPE                                
         LA    RE,PRVBLK                                                        
         LR    R0,RE                                                            
UPDLA22  L     RF,PRVDISP          A(END OF BLOCK)                              
         SR    RF,RE                                                            
         CHI   RF,PRVMAX*LIDGSTLQ  TEST TOO MANY FOR ONE LIDEL                  
         BH    *+10                YES                                          
         SR    R0,R0               ELSE CLEAR SAVED A(NEXT CHUNK)...            
         B     *+10                ...AS THIS WILL BE THE LAST                  
         LHI   RF,PRVMAX*LIDGSTLQ  SET MAX                                      
         AR    R0,RF               R0=A(NEXT CHUNK)                             
         AHI   RF,-1                                                            
         MVC   LIDDATA(0),0(RE)                                                 
         EX    RF,*-6                                                           
         LA    RF,(LIDDATA-LIDELD)+1(RF)                                        
         STC   RF,LIDLN                                                         
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BNE   UPDLAERR                                                         
         LTR   RE,R0               LOAD/TEST A(NEXT CHUNK)                      
         BZ    UPDLAEX                                                          
         XC    LIDDATA(255-(LIDDATA-LIDELD)),LIDDATA                            
         B     UPDLA22             GO ROUND AGAIN                               
*                                                                               
*&&                                                                             
UPDLAEX  B     EXITOK                                                           
*                                                                               
UPDLAERR MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
UPDL1ERR OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* UPDATE ETYPE PASSIVES                                               *         
*                                                                     *         
* NTRY - P1  = ETYPE RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ETYRECD,R3                                                       
ADDPAS   NTR1  ,                                                                
         L     R3,0(R1)                                                         
*                                                                               
         MVC   IOKEY,0(R3)         GET IODA FOR PADDLE                          
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
         XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BOPARM,(C'A',(R3)),(C'A',T.CPTRBLK),IODA,0,ACOM          
         B     EXITOK                                                           
         DROP  T,R3                                                             
         EJECT ,                                                                
***********************************************************************         
* DELETE ETYPE PASSIVES                                               *         
*                                                                     *         
* NTRY - P1  = ETYPE RECORD                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ETYRECD,R3                                                       
DELPAS   NTR1  ,                                                                
         L     R3,0(R1)                                                         
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
         XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',T.CPTRBLK),0,0,ACOM             
         B     EXITOK                                                           
         DROP  T,R3                                                             
         EJECT ,                                                                
***********************************************************************         
* UPDATE PASSIVE EXPENDITURE CATEGORY RECORDS                         *         
*                                                                     *         
* NTRY - P1  = GROUP LIST RECORD                                      *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
ADDRECS  NTR1  ,                                                                
         CLI   GSSMPAGE,1          ARE WE ON FIRST PAGE                         
         BNE   EXITOK              NO                                           
AREC01   L     R3,0(R1)                                                         
         USING ETYRECD,R3                                                       
         LA    R2,IOKEY                                                         
         MVC   IOKEY,0(R3)                                                      
         L     R1,=AL4(XORDD+XOACCDIR+XIO4) READ DIR FOR DA                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   EXPTYKEY,IOKEY                                                   
         LA    R5,ETYRFST                                                       
         USING XNMELD,R5                                                        
         XR    R0,R0                                                            
AREC02   CLI   XNMEL,0             RECORD END?                                  
         BE    EXITOK              YES                                          
         CLI   XNMEL,XNMELQ                                                     
         BE    AREC06                                                           
         IC    R0,XNMLN            GET NEXT ELEMENT                             
         AR    R5,R0                                                            
         B     AREC02                                                           
*                                                                               
K        USING ECTPAS,IOKEY                                                     
AREC06   XC    K.ECTPAS,K.ECTPAS                                                
         LA    R3,EXPTYKEY                                                      
         MVI   K.ECTPTYP,ECTPTYPQ                                               
         MVI   K.ECTPSUB,ECTPSUBQ                                               
         MVC   K.ECTPCPY,CUABIN   CONNECTED ID                                  
         MVC   K.ECTPCAT,BCSPACES                                               
         XR    RF,RF                                                            
         IC    RF,XNMSUBL                                                       
         SHI   RF,1                                                             
         MVC   K.ECTPCAT(0),XNMSUBN                                             
         EX    RF,*-6                                                           
         MVC   K.ECTPCODE,ETYKCODE                                              
         L     R1,=AL4(XIO4+XOACCDIR+XORDUPD)                                   
         GOTO1 AIO                                                              
         BE    AREC20                                                           
         TM    IOERR,FF-IOEDEL         RECORD WAS MARKED DELETED?               
         BNZ   AREC12                                                           
         NI    K.ECTPSTAT,FF-ECTSDELT  UNDELETE IT                              
         MVC   K.ECTPSTA,ETYKSTA                                                
         MVC   K.ECTPDA,ETYKDA                                                  
         LHI   R1,XIO4+XOACCDIR+XOWRITE                                         
         GOTO1 AIO                                                              
         BE    AREC20                                                           
         DC    H'0'                                                             
*                                                                               
AREC12   MVC   K.ECTPAS,IOKEYSAV                                                
         MVC   K.ECTPSTA,ETYKSTA                                                
         MVC   K.ECTPDA,ETYKDA                                                  
         LHI   R1,XOADD+XOACCDIR+XIO4                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                ERROR - CAN'T PUT RECORD BACK                
*                                                                               
AREC20   B     EXITOK                                                           
         DROP  K,R5,R3                                                          
         EJECT ,                                                                
***********************************************************************         
* DELETE LIMLIST RECORDS AND PASSIVES                                 *         
*                                                                     *         
* NTRY - P1  = PID LIST ELEMENT                                       *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
DELRECS  NTR1  ,                                                                
         CLI   CSACT,A#DEL         ACTION DELETE                                
         BE    *+12                YES - DELETE ALL PASSIVE POINTERS            
         CLI   GSSMPAGE,1          ARE WE ON PID PAGE                           
         BNE   EXITOK              NO                                           
         L     R3,0(R1)                                                         
         USING ETYRECD,R3                                                       
*                                                                               
K        USING ECTPAS,IOKEY                                                     
DREC06   XC    K.ECTPAS,K.ECTPAS                                                
         MVI   K.ECTPTYP,ECTPTYPQ                                               
         MVI   K.ECTPSUB,ECTPSUBQ                                               
         MVC   K.ECTPCPY,CUABIN   CONNECTED ID                                  
         MVC   K.ECTPCAT,OLDCATEG                                               
         MVC   K.ECTPCODE,ETYKCODE                                              
         L     R1,=AL4(XIO4+XOACCDIR+XORDUPD)                                   
         GOTO1 AIO                                                              
         BNE   DREC12                                                           
*                                                                               
         OI    K.ECTPSTAT,ECTSDELT DELETE RECORD                                
         LHI   R1,XIO4+XOACCDIR+XOWRITE                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DREC12   B     EXITOK                                                           
         DROP  K,R3                                                             
         EJECT ,                                                                
***********************************************************************         
* DO FILTERING FOR CATEGORY FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
X        USING ETYRECD,IOKEY                                                    
DOFLT    NTR1  ,                                                                
         CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BNE   DOFLT10                                                          
         XR    RF,RF                                                            
         ICM   RF,1,SVFETCL        LENGTH OF EXP TYPE CODE                      
         BZ    DOFLT04                                                          
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVFETC(0),X.ETYKCODE                                             
         BNE   DOFLTL                                                           
*                                                                               
DOFLT04  OC    SVFOFF,SVFOFF                                                    
         BZ    DOFLT10                                                          
         XR    RF,RF                                                            
         CLI   SVFOFF+1,C' '                                                    
         BNH   *+8                                                              
         LHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SVFOFF(0),X.ETYKOFFC                                             
         BNE   DOFLTL                                                           
*                                                                               
DOFLT10  OC    SVSTAFL(SVSTAFLQ),SVSTAFL                                        
         BZ    DOFLT50             OK - NO FILTER ON STATUS                     
         CLI   SVFSLK,0                                                         
         BE    DOFLT12                                                          
         LA    RF,X'70'            BNZ CC                                       
         CLI   SVFSLK,YES                                                       
         BE    *+8                                                              
         LA    RF,X'80'            BZ CC                                        
         TM    X.ETYKSTAT,ETYSLOCK                                              
         EX    RF,*+8                                                           
         B     DOFLTL                                                           
         NOP   DOFLT12                                                          
*                                                                               
DOFLT12  CLI   SVFLEX,0                                                         
         BE    DOFLT14                                                          
         LA    RF,X'70'            BNZ CC                                       
         CLI   SVFLEX,YES                                                       
         BE    *+8                                                              
         LA    RF,X'80'            BZ CC                                        
         TM    X.ETYKSAPP,ETYKSAEQ                                              
         EX    RF,*+8                                                           
         B     DOFLTL                                                           
         NOP   DOFLT14                                                          
*                                                                               
DOFLT14  CLI   SVFLIN,0                                                         
         BE    DOFLT16                                                          
         LA    RF,X'70'            BNZ CC                                       
         CLI   SVFLIN,YES                                                       
         BE    *+8                                                              
         LA    RF,X'80'            BZ CC                                        
         TM    X.ETYKSAPP,ETYKSAIQ                                              
         EX    RF,*+8                                                           
         B     DOFLTL                                                           
         NOP   DOFLT16                                                          
*                                                                               
DOFLT16  CLI   SVFLOR,0                                                         
         BE    DOFLT18                                                          
         LA    RF,X'70'            BNZ CC                                       
         CLI   SVFLOR,YES                                                       
         BE    *+8                                                              
         LA    RF,X'80'            BZ CC                                        
         TM    X.ETYKSAPP,ETYKSAOQ                                              
         EX    RF,*+8                                                           
         B     DOFLTL                                                           
         NOP   DOFLT18                                                          
*                                                                               
DOFLT18  CLI   SVFBIL,0                                                         
         BE    DOFLT24                                                          
         CLI   SVFBIL,YES                                                       
         BNE   DOFLT20                                                          
         TM    X.ETYKSTA2,ETYSBILY                                              
         BZ    DOFLTL                                                           
         B     DOFLT24                                                          
*                                                                               
DOFLT20  CLI   SVFBIL,DEF                                                       
         BNE   DOFLT22                                                          
         TM    X.ETYKSTA2,ETYSBILD                                              
         BZ    DOFLTL                                                           
         B     DOFLT24                                                          
                                                                                
DOFLT22  TM    X.ETYKSTA2,ETYSBILD+ETYSBILY                                     
         BNZ   DOFLTL                                                           
*                                                                               
DOFLT24  CLI   SVFNBL,0                                                         
         BE    DOFLT30                                                          
         CLI   SVFNBL,YES                                                       
         BNE   DOFLT26                                                          
         TM    X.ETYKSTA2,ETYSNBLY                                              
         BZ    DOFLTL                                                           
         B     DOFLT30                                                          
*                                                                               
DOFLT26  CLI   SVFNBL,DEF                                                       
         BNE   DOFLT28                                                          
         TM    X.ETYKSTA2,ETYSNBLD                                              
         BZ    DOFLTL                                                           
         B     DOFLT30                                                          
                                                                                
DOFLT28  TM    X.ETYKSTA2,ETYSNBLD+ETYSNBLY                                     
         BNZ   DOFLTL                                                           
*                                                                               
DOFLT30  CLI   SVFADV,0                                                         
         BE    DOFLT36                                                          
         CLI   SVFADV,YES                                                       
         BNE   DOFLT32                                                          
         TM    X.ETYKSTA2,ETYSADVY                                              
         BZ    DOFLTL                                                           
         B     DOFLT36                                                          
*                                                                               
DOFLT32  CLI   SVFADV,DEF                                                       
         BNE   DOFLT34                                                          
         TM    X.ETYKSTA2,ETYSADVD                                              
         BZ    DOFLTL                                                           
         B     DOFLT36                                                          
                                                                                
DOFLT34  TM    X.ETYKSTA2,ETYSADVY+ETYSADVD                                     
         BNZ   DOFLTL                                                           
*                                                                               
DOFLT36  CLI   SVFAPN,0                                                         
         BE    DOFLT38                                                          
         LA    RF,X'70'            BNZ CC                                       
         CLI   SVFAPN,YES                                                       
         BE    *+8                                                              
         LA    RF,X'80'            BZ CC                                        
         TM    X.ETYKSTA2,ETYSAPND                                              
         EX    RF,*+8                                                           
         B     DOFLTL                                                           
         NOP   DOFLT38                                                          
*                                                                               
DOFLT38  CLI   SVFSFAP,0                                                        
         BE    DOFLT50                                                          
         LA    RF,X'70'            BNZ CC                                       
         CLI   SVFSFAP,NO                                                       
         BE    *+8                                                              
         LA    RF,X'80'            BZ CC                                        
         TM    X.ETYKSTA2,ETYSFAP                                               
         EX    RF,*+8                                                           
         B     DOFLTL                                                           
         NOP   DOFLT50                                                          
         DROP  X                                                                
*                                                                               
DOFLT50  OC    SVELMFL(SVELMFLQ),SVELMFL                                        
         BZ    DOFLTOK             OK - NO FILTER ON ELEMENTS                   
         LHI   R1,XOGET+XOACCMST+XIO11                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
                                                                                
         L     R4,AIOREC                                                        
         LA    R4,ETYRFST-ETYRECD(R4)                                           
         USING XNMELD,R4                                                        
         XR    R0,R0                                                            
DOFLT54  CLI   XNMEL,0                                                          
         BE    DOFLTL              NO - WE DON`T WANT IT THEN                   
         CLI   XNMEL,XNMELQ                                                     
         BE    *+14                                                             
         IC    R0,XNMLN                                                         
         AR    R4,R0                                                            
         B     DOFLT54                                                          
                                                                                
         CLI   XNMLN,XNMLN1Q                                                    
         BNH   DOFLTL                                                           
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         ICM   RE,1,XNMSUBL                                                     
         BZ    DOFLTL                                                           
         ICM   RF,1,SVFCATL                                                     
         BZ    DOFLTL                                                           
         CR    RF,RE                                                            
         BH    DOFLTL                                                           
         LR    RE,RF                                                            
         SHI   RE,1                                                             
         EX    RE,*+8                                                           
         BE    DOFLTOK                                                          
         CLC   SVFCAT(0),XNMSUBN                                                
         B     DOFLTL                                                           
*                                                                               
DOFLTL   B     EXITL                                                            
*                                                                               
DOFLTOK  B     EXITOK                                                           
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO INITIALIZATE LIST                                        *         
***********************************************************************         
         SPACE 1                                                                
INILST   NTR1  ,                                                                
         XC    LSTIND,LSTIND                                                    
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BZ    ILST02                                                           
                                                                                
T        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
         XC    IOKEY,IOKEY                                                      
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKCPY,CUABIN                                                 
         MVC   T.OFFKOFF,CUACCS+2                                               
         L     R1,=AL4(XOHI+XOACCDIR+XIO2)                                      
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    T.OFFKSTAT,OFFSLIST OFFICE LIST?                                 
         BZ    ILST02                                                           
         OI    LSTIND,OFFLIS       SET GOT OFFICE LIST                          
         B     EXITOK                                                           
         DROP  T                                                                
*                                                                               
T        USING OFLPASD,IOKEY                                                    
ILST02   XC    T.OFLPAS,T.OFLPAS   CHECK WHETHER OFFICE IS PART OF              
         MVI   T.OFLPTYP,OFLPTYPQ  OFFICE LIST                                  
         MVC   T.OFLPCPY,CUABIN                                                 
         MVI   T.OFLPSUB,OFLPSUBQ                                               
*                                                                               
         LA    R4,SVOFFLS          LIST OF OFFICE LISTS                         
         LA    R5,SVOFFLS+L'SVOFFLS                                             
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BZ    ILST03                                                           
         MVC   T.OFLPOFF,CUACCS+2  STORE 2 CHARACTER OFFICE                     
         B     ILST04                                                           
*                                                                               
ILST03   CLI   CUACCS,C'$'         DON'T BOTHER IF OFFICE LIST                  
         BE    EXITOK                                                           
         MVC   T.OFLPOFF,BCSPACES                                               
         MVC   T.OFLPOFF(1),CUACCS+1                                            
*                                                                               
ILST04   L     R1,=AL4(XOHI+XOACCDIR+XIO2)                                      
         GOTO1 AIO                                                              
         BE    ILST08                                                           
         DC    H'0'                                                             
*                                                                               
ILST06   L     R1,=AL4(XIO2+XOACCDIR+XOSEQ)                                     
         GOTOR AIO                                                              
*                                                                               
ILST08   CLC   IOKEYSAV(OFLPOFL-OFLPASD),IOKEY                                  
         BNE   EXITOK                                                           
         MVC   0(L'OFLPOFL,R4),T.OFLPOFL SAVE OFFICE LIST CODE                  
         AHI   R4,L'OFLPOFL                                                     
         CR    R4,R5                HIT MAX NO. OF OFFICE LISTS                 
         BL    ILST06                                                           
         DC    H'0'                                                             
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GET LIST ITEM                                            *         
*                                                                     *         
* NTRY: P1=LIST TYPE,(CURR DISP INTO RECORD/ELEMENT PAIR)             *         
* EXIT: SET A(LIST ENTRY) IN P3                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING LIDELD,R4                                                        
         USING ETYRECD,R2                                                       
GETLIT   NTR1  ,                                                                
         L     R2,4(R1)            A(ETYRECD)                                   
         ICM   R5,B'0111',1(R1)                                                 
         LH    R4,0(,R5)                                                        
         AR    R4,R2               R4=A(CURRENT LIST ELEMENT)                   
*                                                                               
         LH    RE,2(,R5)                                                        
         AR    RE,R4               RE=A(CURRENT LIST ENTRY INTO ELEM)           
         ST    RE,8(R1)                                                         
*                                                                               
         XR    RF,RF                                                            
         IC    RF,LIDITLN          LENGTH OF LIST ENTRY                         
         AR    RE,RF               POINTS TO NEXT LIST ENTRY                    
         SR    RE,R4               RE=CURR DISP INTO ELEMENT                    
         IC    RF,LIDLN            LENGHT OF ELEMENT                            
         CR    RE,RF               ANY MORE LIST ENTRY IN THIS ELEM?            
         BNL   *+12                NO - GET THE NEXT LIST ELEMENT               
         STH   RE,2(,R5)                                                        
         B     EXITOK                                                           
*                                                                               
         GOTO1 GETNLE,(R1)         GET NEXT LIST ELEMENT                        
         BNE   EXITL                                                            
         B     EXITOK                                                           
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
         USING ETYRECD,R2                                                       
GETNLE   NTR1  ,                                                                
         L     R2,4(R1)            A(ETYRECD)                                   
         MVC   MYBYTE,0(R1)                                                     
         ICM   R5,B'0111',1(R1)                                                 
         LH    R4,0(,R5)                                                        
         LTR   R4,R4               IF NO DISPLACEMENT, SET TO 1ST ELEM          
         BZ    *+10                                                             
         AR    R4,R2               R4=A(CURRENT ELEMENT)                        
         B     GETNL10                                                          
         LA    R4,ETYRFST                                                       
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
         B     EXITOK                                                           
*                                                                               
GETNLEL  XC    0(4,R5),0(R5)       CLEAR DISPLACEMENT TO RECORD/ELEMENT         
         B     EXITL               SET CC LOW                                   
         DROP  R2,R4                                                            
         EJECT ,                                                                
*&&US                                                                           
***********************************************************************         
* ROUTINE TO VALIDATE THE PROVINCE CODE AND EXTRACT THE NAME          *         
*                                                                     *         
* NTRY: P1=(PROVINCE CODE TO BE VALIDATED)                            *         
***********************************************************************         
         SPACE 1                                                                
         USING PRVTABD,RF                                                       
VALPRV   NTR1  ,                                                                
         L     R2,0(R1)            A(FIELD)                                     
         LA    RF,PRVTAB                                                        
         MVC   SVPRVN,BCSPACES                                                  
*                                                                               
VALPRV02 CLI   PRVCODE,X'FF'       EOT                                          
         JE    EXITL                                                            
         CLC   0(L'CIDMPROV,RF),0(R2)                                           
         JE    VALPRV04                                                         
         LA    RF,PRVTABL(,RF)                                                  
         J     VALPRV02                                                         
*                                                                               
VALPRV04 XR    RE,RE                                                            
         ICM   RE,3,PRVNDIC        PROVINCE NAME DDICT                          
         N     RE,=X'00000FFF'                                                  
         A     RE,AOVERWRK                                                      
         MVC   SVPRVN,0(RE)                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE THE PST CODE AND EXTRACT THE NAME               *         
*                                                                     *         
* NTRY: P1=(PROVINCE CODE)                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING VTCD,R4                                                          
         USING ETYRECD,GSRECKEY                                                 
VALPST   NTR1  ,                                                                
         XR    R3,R3                                                            
         ICM   R3,1,FVILEN                                                      
         BZ    EXITOK                                                           
         OI    FVIFLD,X'40'                                                     
         LA    R4,BOWORK1                                                       
         XC    VTCD(VTCLNQ),VTCD                                                
         MVI   VTCACTN,VTCAIVAL                                                 
         MVC   VTCCPY,CUABIN                                                    
         MVC   VTCOFFC,XFFS                                                     
         CLC   ETYKOFFC,BCSPACES                                                
         BNH   *+10                                                             
         MVC   VTCOFFC,ETYKOFFC    (NO OFFICE VALIDATION)                       
         MVC   VTCCOMF,ACOM                                                     
         L     RF,0(R1)                                                         
         MVC   VTCPRV,0(RF)        PROVINCE                                     
         MVC   VTCCPYS1,BCCPYST1                                                
         GOTO1 VDATCON,BOPARM,(5,0),(1,VTCINVD)                                 
         LA    RE,FVIHDR                                                        
         STCM  RE,B'1111',VTCAFLDH A(FIELD)                                     
         GOTO1 VVATICAN,VTCD                                                    
         BNZ   EXITL                                                            
         MVC   SVPSTN,VTCACTNM                                                  
         B     EXITOK                                                           
*&&                                                                             
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
*&&US                                                                           
PRVTAB   DS    0D                                                               
         DC    C'BC',S(AC@PRVC)   BRITISH COLUMBIA                              
         DC    C'AL',S(AC@PRVAL)   ALBERTA                                      
         DC    C'SA',S(AC@PRVSA)   SASKATCHEWAN                                 
         DC    C'MA',S(AC@PRVMA)   MANITOBA                                     
         DC    C'ON',S(AC@PRVON)   ONTARIO                                      
         DC    C'PQ',S(AC@PRVPQ)   QUEBEC                                       
         DC    C'NB',S(AC@PRVNB)   NEW BRUNSWICK                                
         DC    C'NS',S(AC@PRVNS)   NOVA SCOTIA                                  
         DC    C'PE',S(AC@PRVPE)   PRINCE EDWARD ISLAND                         
         DC    C'NF',S(AC@PRVNF)   NEWFOUNDLAND                                 
         DC    X'FF'               EOT                                          
*&&                                                                             
PRODUL   DC    C'SJ'                                                            
SGUL     DC    C'SG'                                                            
WC       DC    C'WC'                                                            
EXPTYP   DC    C'ET'                                                            
XFFS     DC    X'FFFF'                                                          
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
DEF      EQU   C'D'                                                             
NO       EQU   C'N'                                                             
STMPSTRQ EQU   X'03'                                                            
SALISTQ  EQU   1                                                                
XALISTQ  EQU   2                                                                
WCLISTQ  EQU   3                                                                
*&&US                                                                           
PRLISTQ  EQU   4                                                                
*&&                                                                             
         SPACE 2                                                                
DCLISTL  DS    0D                                                               
         DCDDL AC#DEF,L'AC@DEF,L                                                
*&&US                                                                           
         DCDDL AC#PRVC,L'AC@PRVC                                                
         DCDDL AC#PRVAL,L'AC@PRVAL                                              
         DCDDL AC#PRVSA,L'AC@PRVSA                                              
         DCDDL AC#PRVMA,L'AC@PRVMA                                              
         DCDDL AC#PRVON,L'AC@PRVON                                              
         DCDDL AC#PRVPQ,L'AC@PRVPQ                                              
         DCDDL AC#PRVNB,L'AC@PRVNB                                              
         DCDDL AC#PRVNS,L'AC@PRVNS                                              
         DCDDL AC#PRVPE,L'AC@PRVPE                                              
         DCDDL AC#PRVNF,L'AC@PRVNF                                              
*&&                                                                             
DCLISTX  DC    AL1(EOT)                                                         
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* PROVINCE TABLE DSECT                                                *         
***********************************************************************         
         SPACE 1                                                                
PRVTABD  DSECT                                                                  
PRVCODE  DS    CL2                 PROVINCE CODE                                
PRVNDIC  DS    XL2                 PROVINCE NAME DDICT                          
PRVTABL  EQU   *-PRVTABD                                                        
         EJECT                                                                  
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
SESNL    DS    XL1                 CURRENT SESSION                              
*                                                                               
MYBYTE   DS    X                                                                
WCLDISP  DS    F                   DISP INTO WCLBLK                             
XALDISP  DS    F                   DISP INTO XALBLK                             
SALDISP  DS    F                   DISP INTO SALBLK                             
PRVDISP  DS    F                   DISP INTO PRVBLK                             
SBLKDIS  DS    F                   DISP INTO CURRENT BLK                        
DISPGEN  DS    F                   CURRENT DISPLACEMENT INTO RECORD             
SVIOKEY  DS    XL42                                                             
EXPTYKEY DS    XL(ACCKLEN)                                                      
OLDCATEG DS    CL35                                                             
*                                                                               
DSLISTL  DS    0D                                                               
AC@DEF   DS    CL7                                                              
AC@PRVC  DS    CL20                                                             
AC@PRVAL DS    CL20                                                             
AC@PRVSA DS    CL20                                                             
AC@PRVMA DS    CL20                                                             
AC@PRVON DS    CL20                                                             
AC@PRVPQ DS    CL20                                                             
AC@PRVNB DS    CL20                                                             
AC@PRVNS DS    CL20                                                             
AC@PRVPE DS    CL20                                                             
AC@PRVNF DS    CL20                                                             
*                                                                               
LBLKLNQ  EQU   255-(LIDDATA-LIDELD)                                             
ANYLIST  DS    CL1                                                              
ANYINPUT DS    CL1                                                              
*                                                                               
CPTRWRK  DS    XL128                                                            
*                                                                               
WCLBLK   DS    CL(L'TLKWC*WCLMAX)                                               
WCLMAX   EQU   LBLKLNQ/L'TLKWC                                                  
*                                                                               
XALBLK   DS    CL(XALBLKL)                                                      
         DS    XL(L'TLKXA)         DUMMY ENTRY TO AVOID OVERFLOW                
XALMAX   EQU   LBLKLNQ/L'TLKXA     N'ENTRIES IN A LIDEL                         
XALBLKL  EQU   XALMAX*L'TLKXA*6    ALLOW 6 EXPENSE LIDELS                       
*                                                                               
SALBLK   DS    CL(SALBLKL)                                                      
         DS    XL(L'TLKSA)         DUMMY ENTRY TO AVOID OVERFLOW                
SALMAX   EQU   LBLKLNQ/L'TLKSA     N'ENTRIES IN A LIDEL                         
SALBLKL  EQU   SALMAX*L'TLKSA*6    ALLOW 6 SUPPLIER LIDELS                      
*                                                                               
*&&US                                                                           
PRVBLK   DS    CL(PRVBLKL)                                                      
         DS    XL(LIDGSTLQ)        DUMMY ENTRY TO AVOID OVERFLOW                
PRVMAX   EQU   LBLKLNQ/LIDGSTLQ    N'ENTRIES IN A LIDEL                         
PRVBLKL  EQU   PRVMAX*LIDGSTLQ*6   ALLOW 6 PROVINCE LIDELS                      
*&&                                                                             
*                                                                               
OVERWRKX EQU   *-OVERWRKD                                                       
         SPACE 1                                                                
***********************************************************************         
* SAVED DSECT                                                         *         
***********************************************************************         
         SPACE 2                                                                
SAVED    DSECT                                                                  
DWNINDS  DS    XL1                 DOWNLOAD INDICATOR                           
DWNGDATA EQU   X'80'               GET DATA INTO TSAR                           
DWNNOALL EQU   X'7F'                                                            
DWNNOSUP EQU   X'40'               NO MORE SUPPLIER CODE                        
DWNNOEXP EQU   X'20'               NO MORE EXPENSE CODE                         
DWNNOWCD EQU   X'10'               NO MORE WORK CODE                            
DWNNOPRV EQU   X'08'               NO MORE PROVINCE CODE                        
*                                                                               
LSTIND   DS    XL1                 LIST INDICATOR                               
OFFLIS   EQU   X'80'               OFFICE LIST                                  
LSTOK    EQU   X'40'               ENTRY OK                                     
SVOFFLS  DS    CL20                SAVED OFFICE LIST CODE                       
SVOFF    DS    CL2                 SAVED OFFICE FROM PREVIOUS TIME              
*                                                                               
MNTDISPD DS    0H                                                               
MNTSUP   DS    H                                                                
CURSUP   DS    H                                                                
MNTEXP   DS    H                                                                
CUREXP   DS    H                                                                
MNTWCD   DS    H                                                                
CURWCD   DS    H                                                                
MNTPRV   DS    H                                                                
CURPRV   DS    H                                                                
MNTDISPL EQU   *-MNTDISPD                                                       
*                                                                               
SVDAT    DS    0C                                                               
SVETN    DS    CL(L'TLDETN)                                                     
SVVATC   DS    CL(L'TLDVATC)                                                    
SVVATA   DS    CL(L'TLDVATA)                                                    
SVFNM    DS    CL(L'TLDFNM)                                                     
SVCAT    DS    CL(L'TLDCAT)                                                     
SVPRVN   DS    CL20                                                             
SVPSTN   DS    CL36                                                             
SVDATLQ  EQU   *-SVDAT                                                          
*                                                                               
SVFLTS   DS    0F                                                               
SVFETCL  DS    XL1                 LENGTH OF EXP TYPE CODE                      
SVFETC   DS    CL(L'ETYKCODE)      FILTER ON ETYPE CODE (DOWNLOAD)              
SVFOFF   DS    CL(L'ETYKOFFC)      FILTER ON OFF/OFF LIST (DOWNLOAD)            
SVSTAFL  DS    0F                  FILTER ON STATUS                             
SVFSLK   DS    CL1                 FILTER ON LOCKED                             
SVFLEX   DS    CL1                 FILTER ON LOCK EXPENSES APPLICATION          
SVFLIN   DS    CL1                 FILTER ON LOCK INVOICES APPLICATION          
SVFLOR   DS    CL1                 FILTER ON LOCK ORDERS APPLICATION            
SVFBIL   DS    CL1                 FILTER ON BILLABLE                           
SVFNBL   DS    CL1                 FILTER ON NON-BILLABLE                       
SVFADV   DS    CL1                 FILTER ON ADVANCE                            
SVFAPN   DS    CL1                 FILTER ON APPROVAL NEEDED                    
SVFSFAP  DS    CL1                 FILTER ON SELF APPROVAL FOR AN ETYPE         
SVSTAFLQ EQU   *-SVSTAFL                                                        
SVELMFL  DS    0F                  FILTER ON ELEMENTS                           
SVFCATL  DS    XL1                 LENGTH OF CATEGORY                           
SVFCAT   DS    CL(L'ECTPCAT)       FILTER ON CATEGORY                           
SVELMFLQ EQU   *-SVELMFL                                                        
SVFLTLQ  EQU   *-SVFLTS                                                         
*                                                                               
SAVEDX   EQU   *-SAVED                                                          
         EJECT ,                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKETC   DS    CL(L'ETYKCODE)                                                   
TLKOFF   DS    CL(L'ETYKOFFC)                                                   
         ORG   TLKSRT                                                           
TLKDEF   DS    0XL1                                                             
TLKWC    DS    CL2                 WORKCODE                                     
         ORG   TLKSRT                                                           
TLKXA    DS    CL14                EXPENSE U/L/ACCOUNT                          
         ORG   TLKSRT                                                           
TLKSA    DS    CL14                SUPPLIER U/L/ACCOUNT                         
*&&US                                                                           
         ORG   TLKSRT                                                           
TLKPRV   DS    CL(L'LIDGPROV)      PROVINCE CODE                                
*&&                                                                             
         ORG   TLUSER                                                           
*&&US                                                                           
TLAPST   DS    CL(L'LIDGPSTC)      PST CODE                                     
*&&                                                                             
TLLNQ    EQU   *-TLSTD                                                          
         ORG   TLUSER                                                           
TLDSTAT  DS    XL(L'ETYRSTAT)                                                   
TLDSTA2  DS    XL(L'ETYRSTA2)                                                   
TLDSAPP  DS    XL(L'ETYRSAPP)                                                   
TLDLDAT  DS    0C                                                               
TLDWC    DS    CL2                                                              
TLDXA    DS    CL14                                                             
TLDSA    DS    CL14                                                             
TLDLDATL EQU   *-TLDLDAT                                                        
TLDETN   DS    CL20                                                             
TLDVATC  DS    CL1                                                              
TLDVATA  DS    CL12                                                             
TLDFNM   DS    CL20                                                             
TLDCAT   DS    CL35                                                             
*&&US                                                                           
TLDPRVC  DS    CL(L'LIDGPROV)      PROVINCE CODE FOR CANADA                     
TLDPSTC  DS    CL(L'LIDGPSTC)      PST CODE FOR CANADA                          
*&&                                                                             
TLDLLNQ  EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'056ACFIL43   12/15/16'                                      
         END                                                                    
