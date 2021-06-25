*          DATA SET ACFIL56    AT LEVEL 006 AS OF 08/28/12                      
*PHASE T62356A                                                                  
                                                                                
         TITLE 'EXTRA DATA LIST SCREEN'                                         
*                                                                               
*MPEN  001 22JUL08                                                    *         
*MPEN  002 03JUL09 LO01-9013 CHANGE TO EXPENDITURE TYPE RECORD        *         
*TKLU  003 21AUG09 DU01-8003 KEEP INPUT SEQUENCE INTACT               *         
*TKLU  004 25AUG09 DU01-8003 USE TLSNUM RATHER THAN LSLINE#           *         
*YNGX  005 31JAN11 <PR001426> RELINK TO INCLUDE NEW TLSTD                       
*NRAK 006 02JUL12 <BR19914D> NO CHANGE/DELETE UNLESS ADDED TODAY                
*NRAK 006         <BR19914D> SUPPORT CUT-OFF DATE AT ENTRY LEVEL                
*                                                                               
FIL56    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL56**,R6,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         LH    RA,=Y(TWUSER-TWAD)  SCREEN SAVE AREA                             
         A     RA,ATWA                                                          
         USING SAVED,RA                                                         
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
                                                                                
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
                                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
                                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
                                                                                
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
EXITOFF  MVC   FVMSGNO,=AL2(AE$EXPNV) EXPENDITURE TYPE NOT VALID FOR            
         B     EXITL                  THIS OFFICE                               
*                                                                               
EXITNLDG MVC   FVMSGNO,=AL2(AE$MSJL)    MISSING SJ LEDGER RECORD                
         LH    R0,GSDSPREC                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR      SET CURSOR TO RECORD FIELDD                  
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
                                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
                                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
*                                                                               
         OC    BCCPYPRD,BCCPYPRD                                                
         BNZ   *+6                                                              
         DC    H'0'                PRODUCTION LEDGER MISSING                    
*                                                                               
         LA    R2,IOKEY            READ SJ LEDGER RECORD                        
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,BCSPACES                                                  
         MVC   LDGKCPY,CUABIN      READ PRODUCT LEDGER RECORD                   
         MVC   LDGKUNT(L'BCCPYPRD),BCCPYPRD                                     
         GOTO1 AGETLDG                                                          
         BE    *+12                                                             
         OI    DATERR,DATNPLDG     PRODUCT LEDGER MISSING                       
         B     EXITOK                                                           
*                                                                               
         ICM   RF,15,ACALDG        SET L'CLIENT, PRODUCT AND JOB CODES          
         MVC   CLILEN,LDGTLVA-LDGTABD(RF)                                       
         B     EXITOK                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS R1 TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
                                                                                
         USING OBJTABD,RF                                                       
ITER     TM    DATERR,DATNPLDG   ANY ERROR                                      
         BNZ   EXITNLDG                                                         
*                                                                               
ITER02   CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER04              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE TABLE                                
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER04              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
*                                                                               
ITER04   ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
                                                                                
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
                                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO                                                       
         B     ITERH                                                            
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
                                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         LA    RF,KEYTABL                                                       
         USING XDLRECD,R2                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
                                                                                
KEYFRST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
                                                                                
KFKVAL   XC    XDLKEY,XDLKEY       INITIALIZE KEY OF RECORD                     
         MVI   XDLKTYP,XDLKTYPQ                                                 
         MVI   XDLKSUB,XDLKSUBQ                                                 
         MVC   XDLKCPY,CUABIN                                                   
         B     EXITOK                                                           
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
                                                                                
KFKFVAL  XC    XDLKEY,XDLKEY       INITIALIZE KEY OF RECORD                     
         MVI   XDLKTYP,XDLKTYPQ    LIST FIELD RECORD TYPE                       
         MVI   XDLKSUB,XDLKSUBQ    LIST FIELD SUB-RECORD TYPE                   
         MVC   XDLKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
                                                                                
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
                                                                                
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING XDLRECD,R2                                                       
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
         LA    RF,KNOWLQ(,RF)                                                   
         B     DATA04                                                           
                                                                                
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING XDLRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
                                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
                                                                                
KNOWTAB  DC    AL2(F#XLST#CLI),AL4(CLIDTA)     CLIENT CODE                      
         DC    AL2(F#XLST#CLINM),AL4(CLINDTA)  CLIENT NAME                      
         DC    AL2(F#XLST#OFF),AL4(OFFDTA)     OFFICE CODE                      
         DC    AL2(F#XLST#OFFNM),AL4(OFFNDTA)  OFFICE NAME                      
         DC    AL2(F#XLST#OTYP),AL4(OTYDTA)    ORDER TYPE                       
         DC    AL2(F#XLST#ETY),AL4(ETYDTA)     EXP. TYPE CODE                   
         DC    AL2(F#XLST#ETYNM),AL4(ETYNDTA)  EXP. TYPE NAME                   
         DC    AL2(F#XLST#WC),AL4(WCDTA)       WORK CODE                        
         DC    AL2(F#XLST#WCNM),AL4(WCNDTA)    WORK CODE NAME                   
         DC    AL2(F#XLST#MED),AL4(MEDDTA)     MEDIA CODE                       
         DC    AL2(F#XLST#MEDNM),AL4(MEDNDTA)  MEDIA NAME                       
         DC    AL2(F#XLST#SCMCD),AL4(SCHEMDTA) SCHEME CODE                      
         DC    AL2(F#XLST#SCMNM),AL4(SCHENDTA) SCHEME NAME                      
         DC    AL2(F#XLST#CODE),AL4(CODDTA)    XDATA CODE                       
         DC    AL2(F#XLST#NAME),AL4(NAMDTA)    XDATA NAME                       
         DC    AL2(F#XLST#EDIT),AL4(EDTDTA)    EDIT RULE                        
         DC    AL2(F#XLST#MXLN),AL4(MXLNDTA)   MAXIMUM LENGTH                   
         DC    AL2(F#XLST#REQD),AL4(REQDDTA)   REQUIRED FIELD                   
         DC    AL2(F#XLST#CUT),AL4(CUTDDTA)    CUTOFF DATE                      
         DC    AL2(F#XLST#DEF),AL4(DEFDTA)     DEFAULT FIELD                    
         DC    AL2(F#XLST#ENTRY),AL4(XDFDTA)   XDATA LIST ENTRIES               
         DC    AL2(F#XLST#ECUT),AL4(ECUTDTA)   ENTRY-LEVEL CUT-OFF              
         DC    AL2(EOT)                                                         
                                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL56    CSECT                                                                  
                                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
                                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                             *** FIRST TIME FOR DATA OBJECT ***                
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)    VALIDATE                     
         DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)                                 
         DC    AL1(EOT)                                                         
                                                                                
**********************************************************************          
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                           *          
**********************************************************************          
                                                                                
DFDVAL   DS    0H                                                               
         LH    RF,LSCURLIN         MOVE CURSOR TO FIRST LIST LINE               
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR                                                   
         B     EXITOK                                                           
                                                                                
**********************************************************************          
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                            *          
**********************************************************************          
                                                                                
DFDDIS   DS    0H                                                               
         MVC   BOCURSOR,FVADDR     MOVE CURSOR TO FIELD ADDRESS                 
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR CLIENT CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
CLIDTA   LA    RF,CLITBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CLITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLI)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCLI)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCLI)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETCLI)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCLI)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCLI)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCLI)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCLI)                               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
                                                                                
DSETCLI  B     FLTXX               UNPROTECT FIELD                              
                                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE FIELD                                         *         
***********************************************************************         
                                                                                
DISCLI   CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         OC    XDLKCLI,XDLKCLI     ALL CLIENT                                   
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDLKCLI),XDLKCLI                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A CLIENT CODE FIELD                                        *         
***********************************************************************         
                                                                                
VALCLI   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
VCLI04   CLC   CLILEN,FVILEN       INPUT LENGTH SHORT ENOUGH?                   
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)   INPUT TOO LONG                           
         B     EXITL                                                            
*                                                                               
         MVC   XDLKCLI,FVIFLD      MOVE IN CLIENT CODE                          
*                                                                               
T        USING ACTRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   T.ACTKCPY,XDLKCPY   COMPANY                                      
         MVC   T.ACTKUNT(L'PRODUL),PRODUL    UNIT/LEDGER                        
         MVC   T.ACTKACT(L'XDLKCLI),XDLKCLI  CLIENT CODE                        
         DROP  T                                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE FILTER FIELD                                  *         
***********************************************************************         
                                                                                
DFLTCLI  MVC   FVIFLD(L'XDLKCLI),FLTIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A CLIENT CODE FILTER FIELD                                 *         
***********************************************************************         
                                                                                
VFLTCLI  CLC   CLILEN,FVILEN       INPUT LENGTH SHORT ENOUGH?                   
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         MVC   XDLKCLI,FVIFLD      MOVE IN CLIENT CODE                          
         MVC   FLTIFLD(L'XDLKCLI),FVIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* SEARCH ON A CLIENT CODE FIELD                                       *         
***********************************************************************         
                                                                                
SRCHCLI  CLI   CSACT,A#LST                                                      
         BNE   SRCLI10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SRCLI10  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   C        
               (X'11',0)                                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DO FILTERING FOR CLIENT CODE                                        *         
***********************************************************************         
                                                                                
DOFTCLI  CLC   XDLKCLI,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
                                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A CLIENT NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
CLINDTA  LA    RF,CLINTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CLINTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLIN)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A CLIENT NAME FIELD FROM THE KEY                            *         
***********************************************************************         
                                                                                
DISCLIN  OC    XDLKCLI,XDLKCLI     TEST ALL CLIENT CODE                         
         BZ    EXITOK                                                           
*                                                                               
T        USING ACTRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   T.ACTKCPY,XDLKCPY   COMPANY                                      
         MVC   T.ACTKUNT(L'PRODUL),PRODUL     UNIT/LEDGER                       
         MVC   T.ACTKACT(L'XDLKCLI),XDLKCLI  CLIENT CODE                        
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET CLIENT NAME                              
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
OFFDTA   LA    RF,OFFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFF)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOFF)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOFF)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOFF)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFF)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHOFF)                               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
                                                                                
DSETOFF  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
                                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FIELD                                           *         
***********************************************************************         
                                                                                
DISOFF   CLI   CSACT,A#LST                                                      
         BNE   DOFF02                                                           
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
                                                                                
         OC    XDLKOFF,XDLKOFF                                                  
         BNZ   DOFF02                                                           
         CLI   XDLKORTY,XDLKDFT                                                 
         BNE   EXITOK                                                           
         OC    XDLKCLI(XDLKCODE-XDLKCLI),XDLKCLI                                
         BNZ   EXITOK                                                           
         MVI   FVIFLD,C'*'         ALL                                          
         B     EXITOK                                                           
*                                                                               
DOFF02   MVC   FVIFLD(L'XDLKOFF),XDLKOFF                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE OFFICE CODE FIELD                                          *         
***********************************************************************         
                                                                                
VALOFF   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   XDLKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'XDLKOFF),XDLKOFF                                       
*                                                                               
         CLI   CSACT,A#DIS         DON'T TEST OFFICE CODE                       
         BE    EXITOK              IF DISPLAY                                   
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE CODE                             
         BNE   EXITL               INVALID OFFICE                               
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FILTER FIELD                                    *         
***********************************************************************         
                                                                                
DFLTOFF  MVC   FVIFLD(L'XDLKOFF),FLTIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE CODE                                        *         
* OVERLAY WILL DO ITS OWN FILTERING                                   *         
***********************************************************************         
                                                                                
DOFTOFF  CLC   XDLKOFF,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
                                                                                
***********************************************************************         
* SEARCH ON OFFICE CODE FIELD                                         *         
***********************************************************************         
                                                                                
SRCHOFF  CLI   CSACT,A#LST                                                      
         BNE   SROFF10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SROFF10  GOTO1 VACSRCHC,BOPARM,(3,FVADDR),ATWA,OFFUL,ACOM,(X'11',0)             
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFFICE CODE NAME FIELD                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
OFFNDTA  LA    RF,OFFNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFFN)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE NAME FIELD FROM THE KEY                       *         
***********************************************************************         
                                                                                
T        USING ACTRECD,IOKEY                                                    
DISOFFN  OC    XDLKOFF,XDLKOFF     TEST ALL OFFICE                              
         BZ    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         MVC   IOKEY,BCSPACES                READ THE ACCOUNT RECORD            
         MVC   T.ACTKCPY,XDLKCPY             COMPANY                            
         MVC   T.ACTKUNT(L'OFFUL),OFFUL      UNIT/LEDGER                        
         MVC   T.ACTKACT(L'XDLKOFF),XDLKOFF  OFFICE CODE CODE                   
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET OFFICE CODE NAME                         
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR ORDER TYPE XDLKORTY                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
OTYDTA   LA    RF,OTYTBL                                                        
         B     ITER                                                             
*                                                                               
OTYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOTY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOTY)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOTY)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOTY)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOTY)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOTY)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOTY)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
                                                                                
DSETOTY  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
                                                                                
***********************************************************************         
* DISPLAY A ORDER TYPE FIELD                                          *         
***********************************************************************         
                                                                                
DISOTY   CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         CLI   XDLKORTY,XDLKTYEO   EXPENSE ORDERS?                              
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLEO),UC@FLEO                                        
         B     EXITOK                                                           
         CLI   XDLKORTY,XDLKTYPO   PRODUCTION ORDERS?                           
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLPO),UC@FLPO                                        
         B     EXITOK                                                           
         CLI   XDLKORTY,XDLKTYAO   ARTIST ORDERS?                               
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLAO),UC@FLAO                                        
         B     EXITOK                                                           
         CLI   XDLKORTY,XDLKTYES   ESTIMATES?                                   
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLES),UC@FLES                                        
         B     EXITOK                                                           
         CLI   XDLKORTY,XDLKTYEX   EXPENSE CLAIMS?                              
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLEX),UC@FLEX                                        
         B     EXITOK                                                           
         CLI   XDLKORTY,XDLKINT    INTERNAL?                                    
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@INT),UC@INT  UNKNOWN TYPE!!!!!!                      
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A ORDER TYPE FIELD                                         *         
***********************************************************************         
                                                                                
VALOTY   MVI   XDLKORTY,XDLKDFT                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,2                                                         
         BNE   VOTY04                                                           
         CLC   UC@FLEO,FVIFLD      EXPENSE ORDERS?                              
         BNE   *+12                                                             
         MVI   XDLKORTY,XDLKTYEO                                                
         B     VOTY10                                                           
         CLC   UC@FLPO,FVIFLD      PRODUCTION ORDERS?                           
         BNE   *+12                                                             
         MVI   XDLKORTY,XDLKTYPO                                                
         B     VOTY10                                                           
         CLC   UC@FLAO,FVIFLD      ARTIST ORDERS?                               
         BNE   *+12                                                             
         MVI   XDLKORTY,XDLKTYAO                                                
         B     VOTY10                                                           
         CLC   UC@FLES,FVIFLD      ESTIMATES?                                   
         BNE   *+12                                                             
         MVI   XDLKORTY,XDLKTYES                                                
         B     VOTY10                                                           
         CLC   UC@FLEX,FVIFLD      EXPENSE CLAIMS?                              
         BNE   *+12                                                             
         MVI   XDLKORTY,XDLKTYEX                                                
         B     VOTY10                                                           
         CLC   UC@INT,FVIFLD       INTERNAL?                                    
         BNE   *+12                                                             
         MVI   XDLKORTY,XDLKINT                                                 
         B     VOTY10                                                           
*                                                                               
VOTY04   MVC   FVMSGNO,=AL2(AE$IVTYP)                                           
         B     EXITL               INVALID TYPE                                 
*                                                                               
VOTY10   MVC   FLTIFLD(L'XDLKORTY),XDLKORTY                                     
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DISPLAY A ORDER TYPE FILTER FIELD                                   *         
***********************************************************************         
                                                                                
DFLTOTY  CLI   FLTIFLD,XDLKDFT     DEFAULT?                                     
         BE    EXITOK                                                           
         CLI   FLTIFLD,XDLKTYEO    EXPENSE ORDERS?                              
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLEO),UC@FLEO                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDLKTYPO    PRODUCTION ORDERS?                           
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLPO),UC@FLPO                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDLKTYAO    ARTIST ORDERS?                               
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLAO),UC@FLAO                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDLKTYES    ESTIMATES?                                   
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLES),UC@FLES                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDLKTYEX    EXPENSE CLAIMS?                              
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@FLEX),UC@FLEX                                        
         B     EXITOK                                                           
         CLI   FLTIFLD,XDLKINT     INTERNAL?                                    
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@INT),UC@INT                                          
         B     EXITOK                                                           
*                                                                               
         B     EXITOK              UNKNOWN TYPE!!!!!!                           
                                                                                
***********************************************************************         
* DO FILTERING FOR ORDER TYPE                                         *         
***********************************************************************         
                                                                                
DOFTOTY  CLC   XDLKORTY,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
                                                                                
***********************************************************************         
* DATA OBJECT FOR EXP. TYPE CODE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
ETYDTA   LA    RF,ETYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ETYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISETY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALETY)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISETY)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETETY)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTETY)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTETY)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTETY)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHETY)                               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
                                                                                
DSETETY  B     FLTXX               UNPROTECT FIELD                              
                                                                                
***********************************************************************         
* DISPLAY A EXP. TYPE CODE FIELD                                      *         
***********************************************************************         
                                                                                
DISETY   CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         OC    XDLKETY,XDLKETY     ALL EXP. TYPE                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDLKETY),XDLKETY                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A EXP. TYPE CODE FIELD                                     *         
***********************************************************************         
                                                                                
VALETY   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   XDLKORTY,XDLKTYES   ESTIMATES?                                   
         BNE   VALETY2                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITL               INVALID EXP. TYPE                            
*                                                                               
VALETY2  MVC   XDLKETY,FVIFLD      MOVE IN EXP. TYPE CODE                       
*                                                                               
T        USING ETYRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE EXP. TYPE RECORD                    
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,XDLKCPY   COMPANY                                      
         MVC   T.ETYKCODE,XDLKETY  EXP. TYPE CODE                               
         MVC   IOKEYSAV,IOKEY                                                   
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(ETYKOFFC-ETYRECD),IOKEY                                 
         BE    VALETY4                                                          
         MVC   FVMSGNO,=AL2(AE$INETY)                                           
         B     EXITL               INVALID EXP. TYPE                            
*                                                                               
VALETY4  CLC   XDLKOFF,BCSPACES                                                 
         BNH   VALETY6                                                          
         CLC   T.ETYKOFFC,BCSPACES OK TO HAVE GLOBAL EXPENDITURE TYPE           
         BE    EXITOK                                                           
         CLC   T.ETYKOFFC,XDLKOFF  MATCH OFFICE TO EXPENDITURE TYPE             
         BE    EXITOK                                                           
         B     EXITOFF                                                          
*                                  NO OFFICE THEN CHECK EXPENDITURE             
VALETY6  CLC   T.ETYKOFFC,BCSPACES TYPE IS VALID FOR THIS OFFICE                
         BE    EXITOK              OK TO ADD GLOBAL                             
         CLI   CUACCS,0            GLOBAL LOGON?                                
         BE    EXITOK              THEN CAN ADD ANYTHING                        
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   VALETY10                                                         
         CLI   CUACCS,C'$'         IS IT OFFICE LIST LOGON?                     
         BNE   VALETY8                                                          
         CLI   T.ETYKOFFC,C'$'     OFFICE LIST EXPENDITURE TYPE                 
         BNE   VALETY8                                                          
         CLC   CUACCS(2),T.ETYKOFFC CHECK WHETHER OFFICE LIST MATCHES           
         BE    EXITOK                                                           
         B     EXITOFF             EXPENDITURE TYPE NOT VALID ON THIS           
*                                                                               
VALETY8  GOTO1 ATSTOFF,T.ETYKOFFC                                               
         BE    EXITOK                                                           
         B     EXITOFF             EXPENDITURE TYPE NOT VALID                   
*                                                                               
X        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
VALETY10 MVC   SVIOKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   X.OFFKTYP,OFFKTYPQ                                               
         MVC   X.OFFKCPY,CUABIN                                                 
         MVC   X.OFFKOFF,CUACCS+2                                               
         L     R1,=AL4(XOHI+XOACCDIR+XIO2)                                      
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    X.OFFKSTAT,OFFSLIST  OFFICE LIST?                                
         BZ    VALETY12             NO THEN VALIDATE OFFICE                     
         MVC   IOKEY,SVIOKEY                                                    
         CLC   CUACCS+2(2),T.ETYKOFFC CHECK WHETHER MATCH ON                    
         BE    EXITOK               OFFICE LIST                                 
*                                                                               
VALETY12 MVC   IOKEY,SVIOKEY                                                    
         GOTO1 ATSTOFF,T.ETYKOFFC                                               
         BE    EXITOK                                                           
         B     EXITOFF             EXPENDITURE TYPE NOT VALID                   
         DROP  X,T                                                              
                                                                                
***********************************************************************         
* DISPLAY A EXP. TYPE CODE FILTER FIELD                               *         
***********************************************************************         
                                                                                
DFLTETY  MVC   FVIFLD(L'XDLKETY),FLTIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A EXP. TYPE CODE FILTER FIELD                              *         
***********************************************************************         
                                                                                
VFLTETY  MVC   XDLKETY,FVIFLD      MOVE IN EXP. TYPE CODE                       
         MVC   FLTIFLD(L'XDLKETY),FVIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DO FILTERING FOR EXP. TYPE CODE                                     *         
***********************************************************************         
                                                                                
DOFTETY  CLC   XDLKETY,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
                                                                                
***********************************************************************         
* SEARCH ON A WORK CODE FIELD                                         *         
***********************************************************************         
                                                                                
SRCHETY  CLI   CSACT,A#LST                                                      
         BNE   SRETY10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SRETY10  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,EXTYP,ACOM,0            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A EXP. TYPE NAME FIELD                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
ETYNDTA  LA    RF,ETYNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ETYNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISETYN)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A EXP. TYPE NAME FIELD FROM THE KEY                         *         
***********************************************************************         
                                                                                
DISETYN  OC    XDLKETY,XDLKETY     TEST ALL EXP. TYPE CODE                      
         BZ    EXITOK                                                           
*                                                                               
T        USING ETYRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE EXP. TYPE RECORD                    
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,XDLKCPY   COMPANY                                      
         MVC   T.ETYKCODE,XDLKETY  EXP. TYPE CODE                               
         MVC   IOKEYSAV,IOKEY                                                   
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEYSAV(ETYKOFFC-ETYRECD),IOKEY                                 
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET EXP. TYPE NAME                           
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR WORK CODE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
WCDTA    LA    RF,WCTBL            TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
WCTBL    DC    AL1(DDIS),AL1(0,0,0),AL4(DISWC)                                  
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWC)                                  
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISWC)                                  
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETWC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTWC)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTWC)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTWC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHWC)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
                                                                                
DSETWC   B     FLTXX               UNPROTECT FIELD                              
                                                                                
***********************************************************************         
* DISPLAY A WORK CODE FIELD                                           *         
***********************************************************************         
                                                                                
DISWC    CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         OC    XDLKWC,XDLKWC       ALL WORK CODE                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDLKWC),XDLKWC                                          
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A WORK CODE FIELD                                          *         
***********************************************************************         
                                                                                
VALWC    CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   XDLKORTY,XDLKTYES   ESTIMATES?                                   
         BNE   VALWC02                                                          
         MVC   FVMSGNO,=AL2(AE$INVIF)                                           
         B     EXITL               INVALID WORK CODE                            
*                                                                               
VALWC02  MVC   XDLKWC,FVIFLD       MOVE IN WORK CODE                            
T        USING WCORECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE WORK RECORD                         
         MVI   T.WCOKTYP,WCOKTYPQ                                               
         MVC   T.WCOKCPY,XDLKCPY            COMPANY                             
         MVC   T.WCOKUNT(L'PRODUL),PRODUL   SJ LEDGER                           
         MVC   T.WCOKWRK,XDLKWC             WORK CODE                           
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITL               INVALID WORK CODE                            
                                                                                
***********************************************************************         
* DISPLAY A WORK CODE FILTER FIELD                                    *         
***********************************************************************         
                                                                                
DFLTWC   MVC   FVIFLD(L'XDLKWC),FLTIFLD                                         
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A WORK CODE FILTER FIELD                                   *         
***********************************************************************         
                                                                                
VFLTWC   MVC   XDLKWC,FVIFLD       MOVE IN WORK CODE                            
         MVC   FLTIFLD(L'XDLKWC),FVIFLD                                         
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DO FILTERING FOR WORK CODE                                          *         
***********************************************************************         
                                                                                
DOFTWC   CLC   XDLKWC,FLTIFLD                                                   
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
***********************************************************************         
* SEARCH ON A WORK CODE FIELD                                         *         
***********************************************************************         
                                                                                
SRCHWC   CLI   CSACT,A#LST                                                      
         BNE   SRWC10                                                           
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SRWC10   GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,WCQ,ACOM,      C        
               PRODUL                                                           
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A WORK NAME FIELD                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
WCNDTA   LA    RF,WCNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
WCNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWCN)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A WORK NAME FIELD FROM THE KEY                              *         
***********************************************************************         
                                                                                
DISWCN   OC    XDLKWC,XDLKWC       TEST ALL WORK CODE                           
         BZ    EXITOK                                                           
*                                                                               
T        USING WCORECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE WORK RECORD                         
         MVI   T.WCOKTYP,WCOKTYPQ                                               
         MVC   T.WCOKCPY,XDLKCPY            COMPANY                             
         MVC   T.WCOKUNT(L'PRODUL),PRODUL   SJ LEDGER                           
         MVC   T.WCOKWRK,XDLKWC             WORK CODE                           
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('WCOELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         MVC   FVIFLD(L'WCODESC),WCODESC-WCOELD(RF)                             
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR MEDIA CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
MEDDTA   LA    RF,MEDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MEDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMED)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMED)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISMED)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETMED)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTMED)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTMED)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTMED)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHMED)                               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
                                                                                
DSETMED  B     FLTXX               UNPROTECT FIELD                              
                                                                                
***********************************************************************         
* DISPLAY A MEDIA CODE FIELD                                          *         
***********************************************************************         
                                                                                
DISMED   CLI   CSACT,A#LST                                                      
         BNE   *+14                                                             
         CLC   LSROWREP,=AL2(1)                                                 
         BH    EXITOK              EXIT - IF NOT THE FIRST ROW                  
*                                                                               
         OC    XDLKMED,XDLKMED     ALL MEDIA CODE                               
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDLKMED),XDLKMED                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A MEDIA CODE FIELD                                         *         
***********************************************************************         
                                                                                
VALMED   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVILEN,1                                                         
         BNE   EXITNV                                                           
         MVC   XDLKMED,FVIFLD      MOVE IN MEDIA CODE                           
*                                                                               
T        USING PMDRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES      READ THE MEDIA RECORD                        
         MVI   T.PMDKTYP,PMDKTYPQ                                               
         MVC   T.PMDKCPY,XDLKCPY            COMPANY                             
         MVC   T.PMDKMED,XDLKMED            MEDIA CODE                          
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$IVMED)                                           
         B     EXITL               INVALID MEDIA CODE                           
                                                                                
***********************************************************************         
* DISPLAY A MEDIA CODE FILTER FIELD                                   *         
***********************************************************************         
                                                                                
DFLTMED  MVC   FVIFLD(L'XDLKMED),FLTIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A MEDIA CODE FILTER FIELD                                  *         
***********************************************************************         
                                                                                
VFLTMED  CLI   FVILEN,1                                                         
         BNE   EXITNV                                                           
         MVC   XDLKMED,FVIFLD      MOVE IN MEDIA CODE                           
         MVC   FLTIFLD(L'XDLKMED),FVIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DO FILTERING FOR MEDIA CODE                                         *         
***********************************************************************         
                                                                                
DOFTMED  CLC   XDLKMED,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
                                                                                
***********************************************************************         
* SEARCH ON A MEDIA CODE FIELD                                        *         
***********************************************************************         
                                                                                
SRCHMED  CLI   CSACT,A#LST                                                      
         BNE   SRMED10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SRMED10  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MEDIAQ,ACOM             
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A MEDIA NAME FIELD                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
MEDNDTA  LA    RF,MEDNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MEDNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEDN)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A MEDIA NAME FIELD FROM THE KEY                             *         
***********************************************************************         
                                                                                
DISMEDN  OC    XDLKMED,XDLKMED     TEST ALL MEDIA CODE                          
         BZ    EXITOK                                                           
*                                                                               
T        USING PMDRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES               READ THE MEDIA RECORD               
         MVI   T.PMDKTYP,PMDKTYPQ                                               
         MVC   T.PMDKCPY,XDLKCPY            COMPANY                             
         MVC   T.PMDKMED,XDLKMED            MEDIA CODE                          
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('PMDELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         MVC   FVIFLD(L'PMDDESC),PMDDESC-PMDELD(RF)                             
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR SCHEME CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
SCHEMDTA LA    RF,SCHEMTBL         TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SCHEMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCHEM)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSCH)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISSCHEM)                               
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETSCH)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSCH)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSCH)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSCH)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
                                                                                
DSETSCH  B     FLTXX               UNPROTECT FIELD                              
                                                                                
***********************************************************************         
* DISPLAY A SCHEME CODE FIELD                                         *         
***********************************************************************         
                                                                                
DISSCHEM OC    XDLKSCH,XDLKSCH     ALL SCHEME CODE                              
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDLKSCH),XDLKSCH                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A SCHEME CODE FIELD                                        *         
***********************************************************************         
                                                                                
VALSCH   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         MVC   XDLKSCH,FVIFLD      MOVE IN SCHEME CODE                          
*                                                                               
T        USING SCHRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE SCHEME RECORD                       
         MVI   T.SCHKTYP,SCHKTYPQ                                               
         MVI   T.SCHKSUB,SCHKSUBQ                                               
         MVC   T.SCHKCPY,XDLKCPY   COMPANY                                      
         MVC   T.SCHKUNT,PRODUL    UNIT                                         
         MVC   T.SCHKLDG,PRODUL+1  LEDGER                                       
         MVC   T.SCHKCODE,XDLKSCH                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
*                                                                               
VALSCH1  GOTO1 AIO                                                              
         BE    EXITOK                                                           
VALSCHE  MVC   FVMSGNO,=AL2(AE$INSCH)                                           
         B     EXITL               INVALID SCHEME CODE                          
         DROP  T                                                                
                                                                                
***********************************************************************         
* DISPLAY A SCHEME CODE FILTER FIELD                                  *         
***********************************************************************         
                                                                                
DFLTSCH  MVC   FVIFLD(L'XDLKSCH),FLTIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A SCHEME CODE FILTER FIELD                                 *         
***********************************************************************         
                                                                                
VFLTSCH  MVC   XDLKSCH,FVIFLD      MOVE IN SCHEME CODE                          
         MVC   FLTIFLD(L'XDLKSCH),FVIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DO FILTERING FOR SCHEME CODE                                        *         
***********************************************************************         
                                                                                
DOFTSCH  CLC   XDLKSCH,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
                                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A SCHEME NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
SCHENDTA LA    RF,SCHENTBL         TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SCHENTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCHN)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A SCHEME NAME FIELD FROM THE KEY                            *         
***********************************************************************         
                                                                                
DISSCHN  OC    XDLKSCH,XDLKSCH     TEST ALL SCHEME CODE                         
         BZ    EXITOK                                                           
*                                                                               
T        USING SCHRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE SCHEME RECORD                       
         MVI   T.SCHKTYP,SCHKTYPQ                                               
         MVI   T.SCHKSUB,SCHKSUBQ                                               
         MVC   T.SCHKCPY,XDLKCPY   COMPANY                                      
         MVC   T.SCHKUNT,PRODUL    UNIT                                         
         MVC   T.SCHKLDG,PRODUL+1  LEDGER                                       
         MVC   T.SCHKCODE,XDLKSCH                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
*                                                                               
DISSCH1  GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
DISSCH2  LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('SCHELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(,R1)                                                       
         MVC   FVIFLD(L'SCHNAME),SCHNAME-SCHELD(RF)   DISPLAY                   
         B     EXITOK                                 SCHEME NAME               
         DROP  T                                                                
                                                                                
***********************************************************************         
* DATA OBJECT FOR XDATA CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
         PUSH  USING                                                            
         USING XDFELD,R5                                                        
CODDTA   LA    RF,CODTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CODTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOD)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETCOD)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCOD)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCOD)                               
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTCOD)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCOD)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
                                                                                
DSETCOD  B     FLTXX               UNPROTECT FIELD                              
                                                                                
***********************************************************************         
* DISPLAY XDATA CODE FIELD                                            *         
***********************************************************************         
                                                                                
DISCOD   CLI   CSACT,A#LST         ARE WE DOING A LIST?                         
         BNE   DISCOD14                                                         
*                                                                               
DISCOD2  LA   R5,IOKEY             IF DOING LIST READ XDF RECORD TO             
R        USING XDFRECD,R5          GET XDFELD INFO                              
         XC  IOKEY,IOKEY                                                        
         NI  XDATAIND,FF-READSEQ   SET READ SEQUENTIAL OFF                      
         MVI  R.XDFKTYP,XDFKTYPQ                                                
         MVI  R.XDFKSUB,XDFKSUBQ                                                
         MVC  R.XDFKCPY,XDLKCPY                                                 
         MVC  R.XDFKOFF,XDLKOFF                                                 
         MVC  R.XDFKORTY,XDLKORTY                                               
         MVC  R.XDFKCLI,XDLKCLI                                                 
         MVC  R.XDFKETY,XDLKETY                                                 
         MVC  R.XDFKWC,XDLKWC                                                   
         MVC  R.XDFKMED,XDLKMED                                                 
         MVC  R.XDFKSCH,XDLKSCH                                                 
         LHI   R1,XOREAD+XOACCMST+XIO2  READ XDF RECORD TO GET ELEM INF         
         GOTO1 AIO                                                              
         BNE  DISCOD10                                                          
         L    R5,AIO2                                                           
         LA   R5,R.XDFRFST                                                      
         XR   R0,R0                                                             
*                                                                               
DISCOD4  CLI  XDFEL,0                                                           
         BE   DISCOD8                                                           
         CLI  XDFEL,XDFELQ                                                      
         BE   DISCOD12                                                          
*                                                                               
DISCOD6  IC   R0,XDFLN            ELEMENT LENGTH                                
         AR   R5,R0                                                             
         B    DISCOD4                                                           
*                                                                               
DISCOD8  TM   XDATAIND,READSEQ     CHECK WHETHER SEQUENTIAL READ                
         BNZ  DISCOD10                                                          
         LA   R5,IOKEY                                                          
         IC   R0,R.XDFKSEQ         READ SEQUENTIAL RECORD                       
         AHI  R0,1                                                              
         STC  R0,R.XDFKSEQ                                                      
         LHI  R1,XOREAD+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE  EXITOK                                                            
         LA   R5,AIO2                                                           
         LA   R5,R.XDFRFST                                                      
         OI   XDATAIND,READSEQ                                                  
         B    DISCOD4                                                           
*                                                                               
DISCOD10 XC   SVXDFELD,SVXDFELD    XDF RECORD DOES NOT EXIST                    
         B    EXITOK               SO MAKE SURE XDFELD IS CLEARED               
*                                                                               
DISCOD12 CLC   XDFCODE,XDLKCODE    CHECK CODE MATCHES XDL RECORD                
         BNE   DISCOD6                                                          
         XC    SVXDFELD,SVXDFELD                                                
         XR    RF,RF                                                            
         IC    RF,XDFLN                                                         
         SHI   RF,1                                                             
         EXMVC RF,SVXDFELD,XDFELD  NOW STORE THE ELEMENT                        
                                                                                
*                                                                               
DISCOD14 OC    XDLKCODE,XDLKCODE   ALL CODE FIELD                               
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'XDLKCODE),XDLKCODE                                      
         B     EXITOK                                                           
         DROP  R                                                                
                                                                                
***********************************************************************         
* VALIDATE XDATA CODE FIELD                                           *         
***********************************************************************         
VALCOD   CLI   FVILEN,0                                                         
         BE    ERRVALC2                                                         
         MVC   XDLKCODE,FVIFLD      EXISTING XDATA ELEMENT                      
         XC    IOKEY,IOKEY                                                      
         NI    XDATAIND,FF-READSEQ  SET READ SEQUENTIAL OFF                     
         LA    R5,IOKEY                                                         
T        USING XDFRECD,R5           GET ELEMENT INFO                            
         MVI   T.XDFKTYP,XDFKTYPQ   EXTRA DATA FIELD RECORD TYPE                
         MVI   T.XDFKSUB,XDFKSUBQ   EXTRA DATA FIELD SUB-RECORD TYPE            
         MVC   T.XDFKCPY,CUABIN     CONNECTED ID                                
         MVC   T.XDFKOFF,XDLKOFF                                                
         MVC   T.XDFKORTY,XDLKORTY                                              
         MVC   T.XDFKCLI,XDLKCLI                                                
         MVC   T.XDFKETY,XDLKETY                                                
         MVC   T.XDFKWC,XDLKWC                                                  
         MVC   T.XDFKMED,XDLKMED                                                
         MVC   T.XDFKSCH,XDLKSCH                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   ERRVALC1                                                         
         L     R5,AIO2                                                          
         LA    R5,T.XDFRFST                                                     
         MVC   SVRPTR,T.XDFRPTR                                                 
         XR    R0,R0                                                            
*                                                                               
VALCOD1  CLI   XDFEL,0             CODE ON FILE?                                
         BE    VALCOD4             NO - SHOW ERROR                              
         CLI   XDFEL,XDFELQ                                                     
         BE    VALCOD3                                                          
*                                                                               
VALCOD2  IC    R0,XDFLN                                                         
         AR    R5,R0                                                            
         B     VALCOD1               YES - OK                                   
*                                                                               
VALCOD3  CLC   XDFCODE,XDLKCODE                                                 
         BE    VALCOD5                                                          
         B     VALCOD2                                                          
*                                                                               
VALCOD4  TM   XDATAIND,READSEQ     CHECK WHETHER SEQUENTIAL READ                
         BNZ  ERRVALC1                                                          
         LA   R5,IOKEY                                                          
         IC   R0,T.XDFKSEQ         READ SEQUENTIAL RECORD                       
         AHI  R0,1                                                              
         STC  R0,T.XDFKSEQ                                                      
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE  EXITOK                                                            
         LA   R5,AIO2                                                           
         LA   R5,T.XDFRFST                                                      
         OI   XDATAIND,READSEQ                                                  
         B    VALCOD1                                                           
*                                                                               
VALCOD5  CLI   XDFEDIT,C'X'        CHECK IF IT IS DROPDOWN LIST                 
         BNE   ERRVALC1                                                         
         XC    SVXDFELD,SVXDFELD                                                
         SR    RF,RF                                                            
         IC    RF,XDFLN                                                         
         SHI   RF,1                                                             
         EXMVC RF,SVXDFELD,XDFELD  NOW STORE THE ELEMENT                        
         B     EXITOK                                                           
*                                                                               
ERRVALC1 MVC   FVMSGNO,=AL2(AE$INVCD) DATA CODE NOT VALID                       
         B     EXITL                                                            
         B     EXITOK                                                           
*                                                                               
ERRVALC2 MVC   FVMSGNO,=AL2(AE$MISIF) INPUT REQUIRED                            
         B     EXITL                                                            
         DROP  R5,T                                                             
                                                                                
***********************************************************************         
* DISPLAY A DATA CODE FILTER                                          *         
***********************************************************************         
                                                                                
DFLTCOD  MVC   FVIFLD(L'XDFCODE),FLTIFLD                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A SCHEME CODE FILTER FIELD                                 *         
***********************************************************************         
                                                                                
VFLTCOD  CLI   FVILEN,1                                                         
         BNE   EXITNV                                                           
         MVC   XDLKCODE,FVIFLD                                                  
         MVC   FLTIFLD(L'XDLKCODE),FVIFLD                                       
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DISPLAY XDATA CODE FROM XDRECORD                                    *         
***********************************************************************         
                                                                                
DDFTCOD  TM    XDATAIND,PASSKEY    TEST KEY PASSED ON                           
         BZ    VALCOD              NO THEN NEED TO VALIDATE DATA CODE           
         MVC   FVIFLD(L'SAVECODE),SAVECODE                                      
         MVC   XDLKCODE,SAVECODE                                                
         NI    XDATAIND,X'FF'-PASSKEY                                           
         XC    SAVECODE,SAVECODE                                                
         B     EXITOK                                                           
***********************************************************************         
* DO FILTERING FOR XDATA CODE                                         *         
***********************************************************************         
                                                                                
DOFTCOD  CLC   XDLKCODE,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         POP   USING                                                            
***********************************************************************         
* DATA OBJECT FOR XDATA NAME                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
         USING XDFELD,SVXDFELD                                                  
NAMDTA   LA    RF,NAMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
NAMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY XDATA NAME                                                  *         
***********************************************************************         
                                                                                
DISNAM   MVC   FVIFLD(L'XDFNAME),XDFNAME                                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR EDIT RULE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
EDTDTA   LA    RF,EDTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
EDTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEDT)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY EDIT RULE                                                   *         
***********************************************************************         
                                                                                
DISEDT   MVI   FVIFLD,C'?'         UNKNOWN                                      
         OC    XDFEDIT,XDFEDIT                                                  
         BZ    EXITOK                                                           
         MVI   FVIFLD,C'X'         SET EDIT RULE =X                             
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR MAX LEN                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
MXLNDTA  LA    RF,MXLNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MXLNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMXLN)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY MAX LEN                                                     *         
***********************************************************************         
                                                                                
DISMXLN  OC    XDFMXLN,XDFMXLN                                                  
         BZ    EXITOK                                                           
         LH    RE,XDFMXLN                                                       
         CURED (RE),(3,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR REQUIRED FIELD                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
REQDDTA  LA    RF,REQDTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
REQDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISREQD)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY REQUIRED FIELD                                              *         
***********************************************************************         
                                                                                
DISREQD  TM   XDFSTAT1,XDFSRECP     NO RECEIPT COMPULSORY                       
         BZ   DISREQ2                                                           
         MVC  FVIFLD(L'UC@RCPTS),UC@RCPTS                                       
         B    EXITOK                                                            
*                                                                               
DISREQ2  MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    XDFSTAT1,XDFSREQD                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR CUTOFF DATE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
CUTDDTA  LA    RF,CUTDTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CUTDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCUTD)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCUTD)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY CUTOFF DATE                                                 *         
***********************************************************************         
                                                                                
DISCUTD  OC    XDFCUT,XDFCUT                                                    
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(1,XDFCUT),(17,FVIFLD),0                          
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE CUTOFF DATE                                                *         
***********************************************************************         
                                                                                
VALCUTD  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         BZ    EXITL               INVALID DATE                                 
*                                                                               
         GOTO1 VDATCON,BODMCB,BODUB1,(1,XDFCUT)                                 
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DISPLAY EXTRA DATA DEFAULT ENTRY FIELD                              *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
DEFDTA   LA    RF,DEFLTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
DEFLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDEF)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY DEFAULT FIELD VALUES                                        *         
***********************************************************************         
                                                                                
DISDEF   TM    TLKSTAT,TLKDFT                                                   
         BZ    EXITOK                                                           
         OI    DEFSET,YESSET       SET DEFAULT SET ON                           
         MVC   FVIFLD(1),BC@YES    NO SET Y IF DEFAULT SET                      
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE DEFAULT VALUE                                              *         
***********************************************************************         
                                                                                
VALDEF   MVC   TLRLEN,=AL2(TLLNQ2) SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0            DEFAULT IS NOT COMPULSORY                    
         BE    EXITOK                                                           
*                                                                               
VALDEF2  CLC   FVIFLD(1),BC@NO     CHECK IF N ENTERED                           
         BNE   VALDEF3                                                          
         TM    TLKSTAT,TLKDFT      CHECK IF SET                                 
         BZ    EXITOK                                                           
         NI    TLKSTAT,FF-TLKDFT   TURN OFF DEFAULT                             
         OI    TLKSTAT,TLKNDFT     TURN ON NOT DEFAULT                          
         NI    DEFSET,FF-YESSET    TURN OFF DEFAULT SET                         
         B     EXITOK                                                           
*                                                                               
VALDEF3  CLC   FVIFLD(1),BC@YES    CHECK IF Y ENTERED                           
         BNE   VALDERR2            ERROR ENTRY NOT VALID                        
*                                                                               
VALDEF6  TM    DEFSET,YESSET       HAS DEFAULT BEEN SET?                        
         BNZ   VALDERR             YES CAN'T SET DEFAULT                        
         OI    TLKSTAT,TLKDFT      TURN ON DEFAULT                              
         NI    TLKSTAT,FF-TLKNDFT  TURN OFF NOT DEFAULT                         
         OI    DEFSET,YESSET       TURN ON DEFAULT SET                          
         B     EXITOK                                                           
*                                                                               
VALDERR  MVC   FVMSGNO,=AL2(AE$DUPEN)   ERROR DUPLICATE ENTRY                   
         MVC   BOCURSOR,FVADDR          SET CURSOR TO OFFENDING ENTRY           
         B     EXITL                                                            
*                                                                               
VALDERR2 MVC   BOCURSOR,FVADDR          SET CURSOR TO OFFENDING ENTRY           
         TM    TLKSTAT,TLKDFT           IF DEFAULT PREVIOUSLY SET               
         BZ    EXITNV                                                           
         NI    DEFSET,FF-YESSET         TURN OFF DEFAULT SET                    
         B     EXITNV                                                           
                                                                                
***********************************************************************         
* DISPLAY EXTRA DATA LIST FIELDS                                      *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
         PUSH  USING                                                            
XDFDTA   LA    RF,XDFLTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
XDFLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISXDF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXDF)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY LIST FIELD VALUES                                           *         
***********************************************************************         
                                                                                
DISXDF   MVC   FVIFLD(L'TLKDATN),TLKDATN                                        
***      XOUT  TLKDAT#,FVIFLD+4,2                                               
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE EXTRA DATA LIST FIELD                                      *         
***********************************************************************         
                                                                                
VALXDF   DS    0H                                                               
         CLI   FVILEN,0           CHECK WHETHER FIELD IS BLANK                  
         BNE   VALXD30                                                          
         TM    DEFSET,YESSET      CANNOT DELETE IF DEFAULT                      
         BNZ   VALXD10                                                          
         OC    TLKSEQX,TLKSEQX    CANNOT DELETE UNLESS ADDED TODAY              
         JZ    VALXD05               ENTRY NOT ON YET ON REC ANYWAY             
         CLC   TLADATE,BCTODAYC                                                 
         JNE   VALXD15                                                          
VALXD05  OI    LSLNIND1,LSLNIDEL  DELETE THIS LINE                              
         B     EXITOK                                                           
*                                                                               
VALXD10  MVC   BOCURSOR,FVADDR    YES MUST ENTER SOMETHING                      
         NI    DEFSET,FF-YESSET   TURN OFF DEFAULT                              
         B     EXITNO             PRETEND WE HAVEN'T PROCESSED -                
*                                       TRIGGERS 'MISSING' ERROR MSG            
VALXD15  MVC   BOCURSOR,FVADDR                                                  
         MVC   FVMSGNO,=AL2(AE$CDLIN)                                           
         OI    GCINDS1,GCIREDIS                                                 
         B     EXITL                                                            
*                                                                               
VALXD20  MVC   BOCURSOR,FVADDR                                                  
         MVC   FVMSGNO,=AL2(AE$CCFLD)                                           
         OI    GCINDS1,GCIREDIS                                                 
         B     EXITL                                                            
*                                                                               
VALXD30  DS    0H                                                               
         CLC   TLNDATA,FVILEN     ACTUAL CHANGE?                                
         JNE   VALXD40                                                          
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EXCLC RE,TLKDATN,FVIFLD                                                
         JE    VALXD50            NO                                            
*                                                                               
VALXD40  DS    0H                                                               
         OC    TLKSEQX,TLKSEQX    CHECK WHEN ENTRY ADDED TO FILE                
         JZ    VALXD50                    NOT YET, OK                           
         CLC   TLADATE,BCTODAYC                                                 
         JNE   VALXD20                    BEFORE TODAY, DO NOT CHANGE           
*                                                                               
VALXD50  DS    0H                                                               
         MVC   TLNDATA,FVILEN     STORE LENGTH OF LIST ENTRY                    
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EXMVC RE,TLKDATN,FVIFLD                                                
         L     RF,ATLST                                                         
         XR    R1,R1                                                            
         ICM   R1,3,TLNUM-TLSTD(RF)                                             
         AHI   R1,X'1000'                                                       
         STCM  R1,3,TLKDAT#                                                     
         LH    RF,LSLINE#         CURRENT LINE NUMBER                           
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITEMS        MAX OF 72 LINES                               
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL              TOO MANY LINES IN LIST                        
         POP   USING                                                            
***********************************************************************         
* DATA OBJECT FOR ENTRY-LEVEL CUTOFF DATE                                       
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
         USING TLSTD,R2                                                         
ECUTDTA  LA    RF,ECUTTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ECUTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISECUT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALECUT)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY ENTRY-LEVEL CUTOFF DATE                                     *         
***********************************************************************         
                                                                                
DISECUT  OC    TLCDATE,TLCDATE                                                  
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(2,TLCDATE),(17,FVIFLD),0                         
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE ENTRY-LEVEL CUTOFF DATE                                    *         
***********************************************************************         
                                                                                
VALECUT  CLI   FVILEN,0                                                         
         JNE   VECUT10                                                          
         XC    TLCDATE,TLCDATE                                                  
         J     EXITOK                                                           
*                                                                               
VECUT10  GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         BZ    EXITL               INVALID DATE                                 
*                                                                               
         GOTO1 VDATCON,BODMCB,BODUB1,(2,TLCDATE)                                
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* NTRSES OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                  *         
***********************************************************************         
                                                                                
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
                                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   EXITOK                                                           
         CLI   SREC,R#XLST         EXTRA DATA FIELD RECORD                      
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
NTROU2   OI    SNINDS1,SNIPARMS    PASS INFO                                    
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
* SVPARMS3 = A(PSSAV)                                                 *         
* SVPARMS4 = A(FESD TO BE RESTORED)                                   *         
***********************************************************************         
                                                                                
NTRIN    NI    DEFSET,FF-YESSET                                                 
         CLI   PSREC,R#XDAT        CHECK PREVIOUS RECORD TYPE                   
         BNE   EXITOK                                                           
         CLI   SACT,A#ADD          CHECK IF WE'RE DOING AN ADD                  
         BE    NTRI1                                                            
         CLI   SACT,A#CHA          A CHANGE                                     
         BE    NTRI1                                                            
         CLI   SACT,A#DIS          A DISPLAY                                    
         BNE   EXITOK                                                           
*                                                                               
NTRI1    MVC   SAVECODE(L'XDFCODE),SDATA STORE XDFCODE FROM XDATA               
         OC    SAVECODE,SAVECODE         CHECK IF SAVECODE IS BLANK             
         BZ    ERRDFTD                                                          
         OI    XDATAIND,PASSKEY          SET KEY PASSED ON                      
         NI    DEFSET,FF-YESSET          CLEAR DEFAULT SET INDICATOR            
*                                                                               
NTRI2    XC    IOKEY,IOKEY                                                      
         XR    R0,R0                                                            
         NI    XDATAIND,FF-READSEQ       SET READ SEQUENTIAL OFF                
T        USING XDFRECD,R5                GET ELEMENT INFO                       
         MVC   IOKEY(L'XDFKEY),PSRECKEY  READ PREVIOUS XDF RECORD               
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   ERRDFTD                                                          
         L     R5,AIO2                                                          
         MVC   SVRPTR,T.XDFRPTR    SAVE POINTERS TO LINK WITH XDFRECORD         
         LA    R5,T.XDFRFST                                                     
         USING XDFELD,R5                                                        
         XR    R0,R0                                                            
*                                                                               
NTRI3    CLI   XDFEL,0             CODE ON FILE?                                
         BE    NTRI6               NO - READ SEQUENTIAL RECORD                  
         CLI   XDFEL,XDFELQ                                                     
         BE    NTRI5                                                            
*                                                                               
NTRI4    IC    R0,XDFLN                                                         
         AR    R5,R0                                                            
         B     NTRI3               YES - OK                                     
*                                                                               
NTRI5    CLC   XDFCODE,SAVECODE                                                 
         BE    NTRI7                                                            
         B     NTRI4                                                            
*                                                                               
NTRI6    TM   XDATAIND,READSEQ     CHECK WHETHER SEQUENTIAL READ                
         BNZ  ERRDFTD                                                           
         LA   R5,IOKEY                                                          
         IC   R0,T.XDFKSEQ         READ SEQUENTIAL RECORD                       
         AHI  R0,1                                                              
         STC  R0,T.XDFKSEQ                                                      
         LHI  R1,XOREAD+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE  EXITOK                                                            
         LA   R5,AIO2                                                           
         LA   R5,T.XDFRFST                                                      
         OI   XDATAIND,READSEQ                                                  
         B    NTRI4                                                             
*                                                                               
NTRI7    CLI   XDFEDIT,C'X'        CHECK IF IT IS DROPDOWN LIST                 
         BNE   ERRDFTD                                                          
         XC    SVXDFELD,SVXDFELD                                                
         SR    RF,RF                                                            
         IC    RF,XDFLN                                                         
         SHI   RF,1                                                             
         EXMVC RF,SVXDFELD,XDFELD                                               
         B     EXITOK                                                           
*                                                                               
ERRDFTD  MVC   FVMSGNO,=AL2(AE$INVCD) DATA CODE NOT VALID                       
         B     EXITL                                                            
         DROP  R5,T                                                             
                                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
***********************************************************************         
                                                                                
XITIN    CLI   SACT,A#LST          PAGE DISPLAY SCREEN FROM LIST SCREEN         
         BNE   EXITH                                                            
         NI    SNINDS1,FF-SNIUSECR TURN OFF USE CURRENT RECORD BIT              
         MVI   LSLTIND1,0          TURN OFF LIST INDICATORS                     
         OI    LSSCIND1,LSSCIBLD   AND REBUILD LIST                             
         B     EXITOK                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
                                                                                
         USING XDLRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING XDLRECD,R2                                                       
LAST     USING XDLRECD,R3                                                       
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
                                                                                
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* FIRST FOR LIST (SCREEN 0 LIST SCREEN)                               *         
***********************************************************************         
X        USING XDLRECD,IOKEY                                                    
FLST     XC    IOKEY,IOKEY                                                      
         MVC   X.XDLKEY,THIS.XDLKEY                                             
         L     R1,=AL4(XOHI+XOACCDIR+XIO11)                                     
         GOTO1 AIO                 READ XDL RECORD                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL     IGNORE IF ERROR OTHER THAN DELETE            
         BNZ   EXITL                                                            
         B     NLST02                                                           
                                                                                
***********************************************************************         
* NEXT FOR LIST (SCREEN 0 LIST SCREEN)                                *         
***********************************************************************         
                                                                                
NLST     L     R1,=AL4(XOSQ+XOACCDIR+XIO11)                                     
         GOTO1 AIO                 READ SEQUENTIALLY                            
         BE    NLST02                                                           
         B     EXITL                                                            
*                                                                               
NLST02   CLC   X.XDLKEY(XDLKREM-XDLRECD),THIS.XDLKEY                            
         BNE   EXITL                                                            
*                                                                               
         MVC  THIS.XDLKEY(ACCKLEN),IOKEY   STORE KEY                            
         B    EXITOK                                                            
         DROP X,LAST                                                            
***********************************************************************         
* INITIALISE FOR LIST 1 (SCREEN 1 XDLIST FIELDS)                      *         
***********************************************************************         
                                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         OI    LSSTAT2,LSSADD+LSSNOSEQ                                          
         MVC   LSCOLLIN,=AL2(240)                                               
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1 (SCREEN 1 XDLIST SCREEN)                      *         
***********************************************************************         
                                                                                
FTFLST1  LA    RF,XDLRFST-XDLRECD                                               
         STH   RF,MNTDISP          SAVED DISPLACEMENT TO FIRST ELEM             
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* FIRST FOR LIST 1 (SCREEN 1 XDLIST SCREEN)                           *         
***********************************************************************         
                                                                                
FLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         C     RF,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    RF,XDLRFST-XDLRECD(,RF) IT IS NOW.                               
         XC    RECSEQ#,RECSEQ#                                                  
*                                                                               
         USING XDFELD,RF                                                        
FML02    CLI   XDFEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   XDFEL,XDFELQ        XDFEL?                                       
         BE    FML04                                                            
         LLC   R0,XDFLN                                                         
         AR    RF,R0                                                            
         BNE   FML02               NO                                           
*                                                                               
FML04    S     RF,AIOREC           A(RECORD)                                    
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
                                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
                                                                                
NLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         XR    RE,RE                                                            
         C     RF,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    NML04                                                            
         LA    RF,XDLRFST-XDLRECD(,RF)  IT IS NOW.                              
*                                                                               
         USING XDFELD,RF                                                        
NML02    CLI   XDFEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   XDFEL,XDFELQ        XDFEL?                                       
         BE    NML06               YES                                          
*                                                                               
NML04    IC    RE,XDFLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML06    S     RF,AIOREC                                                        
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
                                                                                
***********************************************************************         
* SET UP TSAR FROM FILE MOVE IN DATA                                  *         
***********************************************************************         
                                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ2) SET LENGTH OF TSAR RECORD                    
         LH    RF,MNTDISP                                                       
         L     R1,AIOREC           A(RECORD)                                    
*                                                                               
         AR    RF,R1               A(RECORD)                                    
         USING XDFELD,RF           MOVE IN DETAILS FROM ELEMENT                 
         MVC   TLADATE,XDFLADAT                                                 
         MVC   TLCDATE,XDFLCDAT                                                 
         MVC   TLKSEQX,XDFLSEQ                                                  
         MVI   TLKSTAT,0                                                        
         TM    XDFLSTAT,XDFLDFT    SEE IF DEFAULT SET                           
         BZ    *+12                                                             
         OI    TLKSTAT,TLKDFT      YES SET DEFAULT                              
         B     *+8                                                              
         OI    TLKSTAT,TLKNDFT     NO SET NOT DEFAULT                           
         SR    RE,RE                                                            
         IC    RE,XDFLN                                                         
         SHI   RE,XDFLN2Q          USE MAX. LENGTH                              
         STCM  RE,1,TLNDATA                                                     
         BCTR  RE,0                SUBTRACT 1 FOR MOVE                          
         EXMVC RE,TLKDATN,XDFLDATA                                              
         LH    R1,RECSEQ#                                                       
         AHI   R1,1                                                             
         STH   R1,RECSEQ#                                                       
         MVC   TLKDAT#,RECSEQ#                                                  
         B     EXITOK                                                           
*                                                                               
         DROP  RF,R3                                                            
                                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
*                                                                     *         
* PRE-DELETE ALL ELEMENTS SO WE CAN REBUILD FROM AMENDED TSAR COPIES  *         
***********************************************************************         
                                                                                
UPDFRST1 MVI   ANYLINES,NO                                                      
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         LH    RF,LSLST#X                                                       
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITEMS         MAX OF 72 LINES                              
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
         L     R1,AIOREC                                                        
         BAS   RE,GETLSN           GET LAST SEQUENCE NUMBER                     
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('XDFELQ',AIOREC),0                
*                                                                               
         L     RE,AIOREC                                                        
R        USING XDLRECD,RE          SET POINTER ON XDLRECD SAME AS               
         MVC   R.XDLRPTR,SVRPTR    XDFRECD                                      
         B     EXITOK                                                           
         DROP  R                                                                
                                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
*                                                                               
         MVI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         LM    R2,R3,SVPARMS3                                                   
*                                                                               
T        USING XDFELD,BOELEM       BUILD NEW ELEMENT                            
         USING XDLRECD,R2                                                       
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    BOELEM,BOELEM                                                    
         MVI   T.XDFEL,XDFELQ                                                   
         MVI   T.XDFLN,XDFLN2Q                                                  
         MVC   T.XDFLSEQ,TLKSEQX                                                
*                                                                               
UREC4    CLI   TLKSEQX,0           NEW XDATA ELEMENT?                           
         BNE   UREC6                                                            
         CLI   LSTSEQNO,255        ANY SPARE SEQ. NO                            
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               NO - TOO MANY ITEMS                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,LSTSEQNO                                                      
         AHI   RE,1                                                             
         STC   RE,LSTSEQNO                                                      
         STC   RE,T.XDFLSEQ        NEW SEQUENCE NO                              
         MVC   TLADATE,BCTODAYC    ADDED DATE                                   
*                                                                               
UREC6    TM    TLKSTAT,TLKDFT      TEST IF DEFAULT SET                          
         BZ    *+8                 NO                                           
         OI    T.XDFLSTAT,XDFLDFT  YES SET DEFAULT ON ELEMENT                   
         MVC   T.XDFLADAT,TLADATE                                               
         MVC   T.XDFLCDAT,TLCDATE                                               
         SR    RE,RE                                                            
         IC    RE,TLNDATA          LENGTH OF XDLIST NAME                        
         BCTR  RE,0                                                             
         EXMVC RE,T.XDFLDATA,TLKDATN                                            
         AHI   RE,XDFLN2Q+1                                                     
         STC   RE,T.XDFLN          LENGTH OF ELEMENT                            
         MVC   T.XDFLPTX,XDLRPTR                                                
***      GOTO1 AADDEL,BOPARM,(R2)  ADD NEW XDATA ELEMENT                        
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),(R2),BOELEM,=CL8'ADD=END'          
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$RECTB)                                           
         B     EXITL               ERROR ADDING ELEMENT MAX ELEMENTS            
*                                                                               
         DROP  R3,T                                                             
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
                                                                                
UPDLAST1 CLI   CSACT,A#CHA                                                      
         BE    *+12                                                             
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
T        USING XDLRSTA,GSRECSTA                                                 
         MVC   T.XDLRPTR,SVRPTR    CREATE SAME POINTER AS XDF RECORD            
         CLI   ANYLINES,YES        EMPTY LIST?                                  
         BE    EXITOK              NO - OK                                      
*                                                                               
ULAST104 MVC   FVMSGNO,=AL2(AE$NLINE)                                           
         LH    RF,LS1STLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         CLI   CSACT,A#ADD                                                      
         BE    EXITL                                                            
         NI    LSLTIND1,FF-LSLTIBLD REBUILD THE LIST                            
         XC    GCLASKEY,GCLASKEY    SET KEY HAS BEEN CHANGED                    
         NI    GSINDSL1,FF-GSIXMNT  TURN OF MAINT SCREEN LOADED FLAG            
         B     EXITL                                                            
         DROP  T                                                                
***********************************************************************         
* ROUTINE TO GET LAST SEQUENCE NUMBER FOR XDATA ELEMENTS              *         
* ETRY - R1                         XDATA RECORD                                
* EXIT - LSTSEQNO                   LAST SEQUENCE NUMBER                        
***********************************************************************         
                                                                                
GETLSN   NTR1  ,                                                                
         LA    R1,XDLRFST-XDLRECD(R1)                                           
         USING XDFELD,R1                                                        
         SR    R0,R0                                                            
*                                                                               
GETLST10 CLI   XDFEL,0             END OF RECORD                                
         BE    EXITOK                                                           
         CLI   XDFEL,XDFELQ                                                     
         BNE   GETLST20                                                         
         CLC   LSTSEQNO,XDFLSEQ                                                 
         BH    GETLST20                                                         
         MVC   LSTSEQNO,XDFLSEQ    SET LAST SEQUENCE NUMBER                     
*                                                                               
GETLST20 IC    R0,XDFLN                                                         
         AR    R1,R0                                                            
         B     GETLST10                                                         
         DROP R1                                                                
                                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
                                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
MAXLINES EQU   9                   MAXIMUM LIST LINES                           
MAXITEMS EQU   72                                                               
IOMAXLNQ EQU   2000                                                             
*                                                                               
PRODUL   DC    C'SJ'               PRODUCTION UNIT/LEDGER CODE                  
OFFUL    DC    C'2D'                                                            
EXTYP    DC    C'ET'                                                            
WCQ      DC    C'WC'                                                            
MEDIAQ   DC    C'MEDIA'                                                         
CODEQ    DC    C'CODE'                                                          
                                                                                
DCLIST   DS    0D                                                               
         DCDDL AC#NUM,L'UC@NUM,L                                                
         DCDDL AC#LTRS2,L'UC@LTRS2,L                                            
         DCDDL AC#AMT,L'UC@AMT,L                                                
         DCDDL AC#FLPO,L'UC@FLPO,L                                              
         DCDDL AC#FLEO,L'UC@FLEO,L                                              
         DCDDL AC#FLAO,L'UC@FLAO,L                                              
         DCDDL AC#FLEX,L'UC@FLEX,L                                              
         DCDDL AC#FLES,L'UC@FLES,L                                              
         DCDDL AC#INT2,L'UC@INT,L                                               
         DCDDL AC#RCPTS,L'UC@RCPTS,L                                            
DCLISTX  DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
                                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
                                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
                                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
SVIOKEY  DS    XL(L'IOKEY)         OVERLAY SAVED KEY                            
*                                                                               
LSTSEQNO DS    XL(L'XDFLSEQ)                                                    
XDATAIND DS    XL1                 XDATA INDICATOR                              
READSEQ  EQU   X'80'               SEQUENTIAL RECORD READ                       
PASSKEY  EQU   X'40'               PASSED PREVIOUS KEY                          
DATERR   DS    XL1                 ERROR BYTE                                   
DATNPLDG EQU   X'80'               NO PRODUCTION LEDGER                         
*                                                                               
CLILEN   DS    XL1                   LENGTH OF CLIENT                           
SVXDFELD DS    XL(XDFLN1Q+L'XDFNAME) SAVED XDFELD ELEMENT                       
SAVECODE DS    XL(L'XDFCODE)         TEMP SAVED CODE FROM ACFIL48               
SVRPTR   DS    XL(L'XDLRPTR)         SAVED POINTERS                             
*                                                                               
ANYLINES DS    CL1                                                              
MNTDISP  DS    H - MOVE TO SAVED STORAGE                                        
*                                                                               
DSLISTU  DS    0F                                                               
UC@NUM   DS    CL3                 NUM                                          
UC@LTRS2 DS    CL7                 LETTERS                                      
UC@AMT   DS    CL6                 AMOUNT                                       
UC@FLPO  DS    CL2                 PRODUCTION ORDERS                            
UC@FLEO  DS    CL2                 EXPENSE ORDERS                               
UC@FLAO  DS    CL2                 ARTIST ORDERS                                
UC@FLEX  DS    CL2                 EXPENSE CLAIMS                               
UC@FLES  DS    CL2                 ESITMATE                                     
UC@INT   DS    CL2                 INTERNAL                                     
UC@RCPTS DS    CL2                 RECEIPTS                                     
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
                                                                                
***********************************************************************         
* SAVED TABLE ENTRIES FOR ACFIL56                                     *         
***********************************************************************         
                                                                                
SAVED    DSECT                                                                  
DEFSET   DS    XL1                 DEFAULT SET INDICATOR (ACFIL56)              
YESSET   EQU   X'80'               DEFAULT SET                                  
RECSEQ#  DS    H                                                                
SAVEDL   EQU   *-SAVED                                                          
                                                                                
***********************************************************************         
* TSAR DSECT FOR XDATA FIELDS                                         *         
***********************************************************************         
                                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKDAT#  DS    XL2                 SEQUENTIAL NUMBER                            
TLKDATN  DS    CL(L'XDFLDATA)      XDATA LIST ENTRY                             
         ORG   TLUSER                                                           
TLKSTAT  DS    XL(L'XDFLSTAT)      DEFAULT ENTRY ON PAGE 1                      
TLKSEQX  DS    XL1                 SEQUENCE NO.                                 
TLNDATA  DS    XL1                 LENGTH OF XDATA LIST ENTRY                   
TLKDFT   EQU   X'40'                                                            
TLKNDFT  EQU   X'80'                                                            
TLADATE  DS    XL2                 DATE ENTRY WAS ADDED                         
TLCDATE  DS    XL2                 CUT-OFF DATE FOR ENTRY                       
TLLNQ2   EQU   *-TLSTD                                                          
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACFIL56   08/28/12'                                      
         END                                                                    
