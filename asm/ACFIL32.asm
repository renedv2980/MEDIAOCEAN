*          DATA SET ACFIL32    AT LEVEL 012 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE T62332C,*                                                                
****************************************************************                
*      DISPLAYS DISTRIBUTOR SCHEME RECORD                      *                
*                                                              *                
****************************************************************                
         TITLE 'DISTRIBUTION SCHEME RECORD OBJECT VERSION'                      
         SPACE 2                                                                
FIL32    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL32**,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    RB,SAVRB                                                         
*                                                                               
         MVC   SVPARMS,0(R1)                                                    
         LH    R4,=Y(TWUSER-TWAD)                                               
         AR    R4,RA                                                            
         USING TWUSER,R4                                                        
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
*                                                                               
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
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
EXITLONG MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               EXIT WITH INPUT TOO LONG                     
EXITSHRT MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     EXITL               EXIT WITH INPUT TOO SHORT                    
EXITDUPE MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     EXITL               EXIT WITH DUPLICATE ENTRY                    
EXITINSA MVC   FVMSGNO,=AL2(AE$INSTA)                                           
         B     EXITL               EXIT WITH INVALID START ACCOUNT              
EXITCHNA MVC   FVMSGNO,=AL2(AE$CHANA)                                           
         B     EXITL               EXIT WITH CHANGE NOT ALLOWED                 
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
EXITRNF  MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL               EXIT WITH RECORD NOT FOUND                   
*                                                                               
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
*                                                                               
         USING CPYRECD,R2                                                       
INIT     DS    0H                                                               
         MVC   IOKEYLST,BCSPACES   INIT IOKEYLST FOR OUTLET/UNIT/DATE           
         ZAP   NEWAMT,=P'0'        UNITS AMOUNT'S TOTAL                         
         MVI   FLAG,0                                                           
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLIST                             
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS R1 TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
*                                                                               
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE TABLE                                
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
*                                                                               
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
*                                                                               
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO                                                       
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
*        DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
*                                                                               
KEY      LM    R0,R3,SVPARMS                                                    
         USING ACTRECD,R2          DISTRIBUTORS SCHEME RECORD                   
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
*                                                                               
KEYFRST  L     R1,SVPARMS4         TABLE OF KNOWN INVOKERS                      
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
*                                                                               
KFKVAL   MVC   ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVC   ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                     
         MVI   ACTKUNT,C'3'      DISTRIBUTOR ALWAYS HAVE UNIT C'3'              
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
*                                                                               
KFKFVAL  DS    0H                                                               
         MVC   ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVC   ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                     
         MVI   ACTKUNT,C'3'      DISTRIBUTOR ALWAYS HAVE UNIT C'3'              
         B     EXITOK                                                           
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
         USING ACTRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DS    0H                                                               
*        DC    AL1(RMASK),AL1(0,0,0),AL4(RFMASK)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TURN OFF DISPLAY, CHANGE  ACTION                                              
***********************************************************************         
RFMASK   DS    0H                                                               
*        ZIC   R5,LOWLEV                                                        
*        LA    R1,IOKEY+3(R5)                                                   
*        CLI   0(R1),C' '                                                       
*        BE    EXITOK                                                           
*        LA    R1,GSRECMSK                                                      
*        NC    0(4,R1),=AL4(X'FFFFFFFF'-MK#DIS-MK#CHA)                          
         B     EXITOK                                                           
         EJECT                                                                  
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
*                                                                               
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING ACTRECD,R2                                                       
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
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
*                                                                               
KNOWTAB  DC    AL2(DI#ACODE),AL4(ACODE)    LEDGER CODE /ADVERTISER CODE         
         DC    AL2(DI#ACDNM),AL4(ACDNM)    LEDGER/ADVERTISER CODE NAME          
         DC    AL2(DI#SCODE),AL4(SCODE)    SCHEME CODE                          
         DC    AL2(DI#SCDFLT),AL4(SCDFLT)  SCHEME CODE TSAR LEVEL               
         DC    AL2(DI#SCDNM),AL4(SCDNM)    SCHEME CODE NAME                     
         DC    AL2(DI#MCODE),AL4(MCODE)    MARKET CODE                          
         DC    AL2(DI#MCDNM),AL4(MKTNM)    MARKET CODE NAME                     
         DC    AL2(DI#FLTR),AL4(FLTR)      FLTR FLD ON MNTENANCE SCRN           
         DC    AL2(DI#OTLT1),AL4(GETOTLT)  OUTLET TSAR                          
         DC    AL2(DI#OLTN1),AL4(NAMDTA)   OUTLET NAME TSAR                     
         DC    AL2(DI#UNIT1),AL4(GETUNT)   UNIT TSAR                            
*                                                                               
         DC    AL2(DI#LSTDATA),AL4(LSTDATA) UNIT= / LEDGER=                     
*                                                                               
         DC    AL2(DI#LEV12),AL4(DOFLTLV)  ACCOUNT LEVEL FILTER                 
*                                                                               
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL32    CSECT                                                                  
*                                                                               
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
*                                                                               
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
*                                                                               
DFDVAL   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         GOTO1 AADDRST,ACTRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADVERISER CODE/ UNIT LEDGER                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
ACODE    LA    RF,ACODETBL                                                      
         B     ITER                                                             
*                                                                               
ACODETBL DC    AL1(DVAL),AL1(0,0,0),AL4(VALACDE)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISACDE)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTACDE)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTACDE)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTACDE)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY ADVERTISER CODE                                                       
***********************************************************************         
*                                                                               
DISACDE  MVC   FVIFLD(L'ACTKLDG),ACTKLDG                                        
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE ADVERTISER CODE                                                      
***********************************************************************         
*                                                                               
VALACDE  DS    0H                                                               
         TM    FVIIND,FVINUM       IS IT A NUMBER BETWEEN 0-9                   
         BO    VALACD10                                                         
         TM    FVIIND,FVIALF     IS IT A CHAR BETWEEN A-Z                       
         BO    VALACD10                                                         
         MVC   FVXTRA,BCSPACES                                                  
         B     EXITNV                                                           
*                                                                               
VALACD10 MVC   ACTKLDG,FVIFLD    PUT IN LDGR/ADVRTSR CODE IN REAL KEY           
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,FVIFLD    PUT IN LDGR/ADVRTSR CODE IN KEY           
         MVC   SVLDG,FVIFLD        SAVE OFF ADVRTSR/LDGR CODE                   
         DROP  TEMP                                                             
*                                                                               
         GOTO1 AGETACT,0                                                        
         BNE   EXITL                                                            
*                                                                               
         GOTO1 AGETEL,BOPARM,('ACLELQ',AIO1),0   GET X'16' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETLEVS          GET INDVIDUAL LVL LENS.                      
*                                                                               
         MVC   LOWLEV,LEVB         ASSUME LEV B HAS LOWEST LEN                  
         MVC   LASTLEV,LEVC        ASSUME LEVC HAS LASTLEV LENGTH               
*                                  USED FOR FILTER LEVEL FIELD                  
         CLI   LEVLNQC,0                                                        
         BNE   VALACD20                                                         
         MVC   LOWLEV,LEVA         MAKE LEVEL A LOWEST LEVL                     
         MVC   LASTLEV,LEVB        USED FOR FILTER FIELD LEVEL                  
*                                  LEVB IS LEV A + LEV B LENGTH                 
*                                                                               
VALACD20 MVC   IOKEY+2(1),SVLDG    MOVE IN LEDGER CODE                          
         ZIC   R5,LOWLEV           DON'T DECREMENT R5 READ LOWEST LVL           
*        BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   IOKEY+3(0),=12C'*'         MOVE STARS                            
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITRNF             ACCOUNT NOT FOUND EXIT WITH ERR              
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
         USING NAMELD,R6                                                        
         LA    R6,BOELEM                                                        
         GOTO1 AGETEL,BOPARM,('NAMELQ',AIO2),0   GET X'20' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVANAME,NAMEREC     SAVE ADVERTSER NAME                          
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY A ADVERTISE/LEDGER CODE FILTER FIELD                        *         
***********************************************************************         
*                                                                               
DFLTACDE MVC   FVIFLD(L'ACTKLDG),FLTIFLD                                        
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE A ADVERTISER/LEDGER CODE FILTER FIELD                      *         
***********************************************************************         
*                                                                               
VFLTACDE DS    0H                                                               
         TM    FVIIND,FVINUM       IS IT A NUMBER BETWEEN 0-9                   
         BO    VFLACD10                                                         
         TM    FVIIND,FVIALF       IS IT A CHAR BETWEEN A-Z                     
         BO    VFLACD10                                                         
         MVC   FVXTRA,BCSPACES                                                  
         B     EXITNV                                                           
VFLACD10 MVC   ACTKLDG,FVIFLD      MOVE WHAT USER TYPED IN KEY                  
         MVC   SVLDG,FVIFLD        SAVE OFF LEDGER/ADVERTISER CODE              
         ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,FLTIFLD,FVIFLD   MOVE IN LEDG CODE TO FILTER FIELD            
*                                                                               
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,FVIFLD    PUT IN LDGR/ADVRTSR CODE IN KEY           
         MVC   SVLDG,FVIFLD        SAVE OFF ADVRTSR/LDGR CODE                   
         DROP  TEMP                                                             
         GOTO1 AGETACT,0                                                        
         BNE   EXITL                                                            
*                                                                               
         GOTO1 AGETEL,BOPARM,('ACLELQ',AIO1),0   GET X'16' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,GETLEVS          GET INDVIDUAL LVL LENS.                      
*                                                                               
         MVC   LOWLEV,LEVB         ASSUME LEVEL B HAS LOWEST LEN                
         MVC   LASTLEV,LEVC        USED FOR FILTER LEVEL FIELD                  
         CLI   LEVLNQC,0                                                        
         BNE   VFLACD20                                                         
         MVC   LOWLEV,LEVA         MAKE LEVEL A LOWEST LEVL                     
         MVC   LASTLEV,LEVB                                                     
*                                                                               
VFLACD20 MVC   IOKEY+2(1),SVLDG    MOVE IN LEDGER CODE                          
         ZIC   R5,LOWLEV           DON'T DECREMENT R5 READ LOWEST LVL           
*        BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   IOKEY+3(0),=12C'*'         MOVE STARS                            
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITRNF             ACCOUNT NOT FOUND EXIT WITH ERR              
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
         USING NAMELD,R6                                                        
         LA    R6,BOELEM                                                        
         GOTO1 AGETEL,BOPARM,('NAMELQ',AIO2),0   GET X'20' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVANAME,NAMEREC     SAVE ADVERTSER NAME                          
VFLTAXX  B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DO FILTERING ON ADVERTISER/LEDGER CODE                                        
***********************************************************************         
*                                                                               
DOFTACDE CLC   ACTKLDG,BCSPACES  IS THERE A CODE TO COMPARE ON?                 
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   ACTKLDG,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING ADVERTISER NAME                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
ACDNM    LA    RF,ANAMTBL                                                       
         B     ITER                                                             
*                                                                               
ANAMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISANAM)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY ADVERTISER NAME                                             *         
***********************************************************************         
*                                                                               
DISANAM  DS    0H                                                               
         MVC   FVIFLD(L'SVANAME),SVANAME MOVE SVD ADVERTISER TO FIELD           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SCHEME  CODE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
SCODE    LA    RF,SCODETBL                                                      
         B     ITER                                                             
*                                                                               
SCODETBL DC    AL1(DVAL),AL1(0,0,0),AL4(VALSCDE)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCDE)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY SCHEME CODE                                                           
***********************************************************************         
*                                                                               
         USING WCORECD,R5          DSECT TO COVER MEDIA INTERFACE ELEM          
DISSCDE  DS    0H                                                               
         USING TLSTD,R3                                                         
         L     R3,ATLST                                                         
         OC    LISTSCCD,LISTSCCD                                                
         BZ    DISSCD10                                                         
         MVC   FVIFLD(L'LISTSCCD),LISTSCCD                                      
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
DISSCD10 DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         LA    R5,IOKEY                                                         
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ  LOOKING FOR X'0A' RECORD                       
         MVC   WCOKCPY,CUABIN    CONNECTED ID AGENCY BINARY                     
         MVI   WCOKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRIB RECS             
         MVC   WCOKLDG,SVLDG     PUT IN LDGR/ADVRTSR CODE IN KEY                
         MVC   WCOKWRK,FVIFLD    MOVE SCHEME CODE IN KEY                        
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO3                                          
         GOTO1 AIO                                                              
         BNE   EXITRNF             ACCOUNT NOT FOUND EXIT WITH ERR              
*                                                                               
         MVC   FVIFLD(L'WCOKWRK),WCOKWRK                                        
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* VALIDATE SCHEME CODE                                                          
***********************************************************************         
         USING WCORECD,R5                                                       
*                                                                               
VALSCDE  DS    0H                                                               
*                                                                               
* READ RECORD WITH SCHEME CODE FROM THE SCREEN                                  
*                                                                               
         LA    R5,IOKEY                                                         
         MVC   WCOKEY,BCSPACES                                                  
         MVI   WCOKTYP,WCOKTYPQ  LOOKING FOR X'0A' RECORD                       
         MVC   WCOKCPY,CUABIN    CONNECTED ID AGENCY BINARY                     
         MVI   WCOKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRIB RECS             
         MVC   WCOKLDG,SVLDG     PUT IN LDGR/ADVRTSR CODE IN KEY                
         MVC   WCOKWRK,FVIFLD    MOVE SCHEME CODE IN KEY                        
*                                                                               
         CLC   SVSCHMCD,FVIFLD   HAS SCHEME CODE BEEN CHANGED                   
         BE    VALSCD10          NO GOOD                                        
         XC    GCLASKEY,GCLASKEY YES, NEED TO REVALIDATE KEY                    
VALSCD10 MVC   SVSCHMCD,FVIFLD     SAVE SCHEME/WORK CODE HERE                   
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO3                                          
         GOTO1 AIO                                                              
         BNE   EXITRNF             ACCOUNT NOT FOUND EXIT WITH ERR              
         LHI   R1,XOGET+XOACCMST+XIO3                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
         DROP  R5                                                               
         PUSH  USING                                                            
         USING WCOELD,BOELEM      DSECT TO COVER SCHEME CODE INFO               
*                                                                               
         GOTO1 AGETEL,BOPARM,('WCOELQ',AIO3),0   GET X'12' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVSCHNM(L'WCODESC),WCODESC                                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SCHEME  CODE  FILTERS                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
SCDFLT   LA    RF,SCDFTBL                                                       
         B     ITER                                                             
*                                                                               
SCDFTBL  DS    0H                                                               
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISFSCDE)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSCCD)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSCCD)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSCCD)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY SCHEME CODE TSAR LEVEL                                                
***********************************************************************         
*                                                                               
DISFSCDE DS    0H                                                               
         USING TLSTD,R3                                                         
         L     R3,ATLST                                                         
         OC    LISTSCCD,LISTSCCD                                                
         BZ    DISSCD10                                                         
         MVC   FVIFLD(L'LISTSCCD),LISTSCCD                                      
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DISPLAY A SCHEME CODE FILTER FIELD                                  *         
***********************************************************************         
*                                                                               
DFLTSCCD MVC   FVIFLD(2),FLTIFLD                                                
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE SCHEME CODE FILTER FIELD                                   *         
***********************************************************************         
*                                                                               
VFLTSCCD DS    0H                                                               
         ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,FLTIFLD,FVIFLD   MOVE IN SCHEMCODE TO FILTER FIELD            
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DO FILTERING ON SCHEME CODE                                                   
***********************************************************************         
*                                                                               
         USING TLSTD,R3                                                         
DOFTSCCD L     R3,ATLST                                                         
         OC    LISTSCCD,LISTSCCD                                                
         BZ    FLTXX                                                            
         CLC   LISTSCCD,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING SCHEME CODE DESCRIPTION                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
SCDNM    LA    RF,SNAMTBL                                                       
         B     ITER                                                             
*                                                                               
SNAMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSNAM)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY SCHEME CODE DESCRIPTION                                     *         
***********************************************************************         
*                                                                               
DISSNAM  DS    0H                                                               
         MVC   FVIFLD(L'SVSCHNM),SVSCHNM  SCHEME CODE DESCRIPTION               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MARKET CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
MCODE    LA    RF,MCODETBL                                                      
         B     ITER                                                             
*                                                                               
MCODETBL DC    AL1(DVAL),AL1(0,0,0),AL4(VALMCDE)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISMCDE)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISMCDE)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY MARKET CODE                                                           
***********************************************************************         
*                                                                               
*        USING TLSTD,R3                                                         
DISMCDE  DS    0H                                                               
*                                                                               
         LHI   R1,L'ACTKACT        DISPLAY FULL ACCOUNT IF LIST                 
         CLI   CSACT,A#LST                                                      
         BE    DISMCD10                                                         
         SR    R1,R1                                                            
         IC    R1,LOWLEV           DISPLAY ONLY MARKET CODE                     
DISMCD10 BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   FVIFLD(0),ACTKACT                                                
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
***********************************************************************         
* VALIDATE MARKET CODE                                                          
***********************************************************************         
*                                                                               
VALMCDE  DS    0H                                                               
*                                                                               
         ZIC   R1,LOWLEV                                                        
         CLC   FVILEN,LOWLEV                                                    
         BH    EXITLONG                                                         
         BL    EXITSHRT                                                         
         MVC   ACTKACT,FVIFLD      MARKET CODE (ACCOUNT) IN REAL KEY            
         MVC   LOKEY(15),0(R2)     SAVE OFF FOR FILTER FIELD                    
*                                                                               
* READ RECORD WITH MARKET CODE FROM THE SCREEN                                  
*                                                                               
         MVC   SVMKTCD,FVIFLD      SAVE OFF MARKET CODE                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING MARKET  NAME                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
MKTNM    LA    RF,MKTNMTBL                                                      
         B     ITER                                                             
*                                                                               
MKTNMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISMKTNM)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY MARKET NAME                                                 *         
***********************************************************************         
DISMKTNM DS    0H                                                               
*                                                                               
DISMKT10 LR    R1,R2               A(ACCOUNT RECORD)                            
         GOTOX AGETNAM             GET ACCOUNT NAME                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING MARKET CODE/ACCOUNT NAME                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
NAMDTA   LA    RF,NAMTBL                                                        
         B     ITER                                                             
*                                                                               
NAMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAME)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY NAME OF ACCOUNT                                             *         
***********************************************************************         
         USING TLSTD,R3                                                         
DISNAME  DS    0H                                                               
         L     R3,ATLST                                                         
         MVC   FVIFLD(L'TLSNAME),TLSNAME MOVE ACCOUNT NAME TO FIELD             
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING/VALIDATING FILTER FIELD                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
FLTR     LA    RF,FLTRTBL                                                       
         B     ITER                                                             
*                                                                               
FLTRTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFLTR)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFLTR)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY FILTER FIELD                                                *         
***********************************************************************         
*                                                                               
DISFLTR  DS    0H                                                               
         ZIC   R1,LOWLEV           TOTAL NUMBER OF LEVELS                       
         ZIC   RE,LASTLEV                                                       
         SR    RE,R1               INDIVIDUAL LENGTH OF LAST LEVEL              
*                                                                               
         LA    R3,L'ACTKCULA-1(R2)   BUMP C/U/L + DISP IN KEY                   
DISFLT10 CLI   0(R3),C' '          IS IT A SPACE                                
         BNE   DISFLT20                                                         
         BCTR  R3,0                                                             
         BCT   RE,DISFLT10         LOWEST LEVEL LENGTH                          
         B     DISFLTX             NOT DISPLAYING LOWEST LEVEL                  
*                                                                               
DISFLT20 LA    RF,3(R1,R2)         BUMP C/U/L + DISP IN KEY                     
         ZIC   RE,LASTLEV          GET LENGTH                                   
         LA    R1,FVIFLD                                                        
         MVC   0(6,R1),=CL6'START='                                             
         LA    R1,6(R1)                                                         
         EX    RE,*+4                                                           
         MVC   0(0,R1),0(RF)       MOVE START ACCOUNT IF ANY                    
DISFLTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE FILTER FIELD                                               *         
***********************************************************************         
*                                                                               
VALFLTR  DS    0H                                                               
         BAS   RE,VALOPT           VALIDATE OPTIONS                             
         CLI   ERROR,OK                                                         
         BNE   EXITL                                                            
*                                                                               
         CLC   SVOPTN,OPFLDS        REVALIDATE KEY IF OPTN CHANGES              
         BE    *+16                                                             
         XC    GCLASKEY,GCLASKEY                                                
         MVC   SVOPTN,OPFLDS                                                    
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OUTLET/STORES NAMES                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
GETOTLT  LA    RF,OUTLTTBL                                                      
         B     ITER                                                             
*                                                                               
OUTLTTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISOUTLT)                               
*        DC    AL1(DVAL),AL1(0,0,0),AL4(VALOUTLT)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY OUTLET NUMBER  / STORE NUMBER                                         
***********************************************************************         
*                                                                               
DISOUTLT DS    0H                                                               
         USING TLSTD,R3                                                         
         L     R3,ATLST                                                         
         LA    R6,TLSKEY                                                        
         ZIC   R1,LOWLEV           GET DISPLACEMENT IN KEY                      
         LA    R1,0(R1,R6)         POINT FROM WHERE TO PRINT LEVD CD            
         ZIC   R5,LASTLEV          GET LENGTH OF CODE TO PRINT                  
         BCTR  R5,0                                                             
         EX    R5,*+4                                                           
         MVC   FVIFLD(0),0(R1)     MOVE ACCOUNT CODE TO TSAR                    
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* VALIDATE OUTLET NUMBER                                                        
***********************************************************************         
VALOUTLT DS    0H                                                               
*                                                                               
         MVC   SVNAME,BCSPACES                                                  
         MVC   SVLEN,FVILEN        SAVE LENGTH FOR UNIT VALIDATION              
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         LA    R5,IOKEY                                                         
TMP      USING ACTRECD,R5                                                       
*                                                                               
         MVC   TMP.ACTKEY,BCSPACES     INITIALIZE KEY OF RECORD                 
         MVC   TMP.ACTKCPY,CUABIN      CONNECTED ID AGENCY BINARY               
         MVI   TMP.ACTKUNT,C'3'    ALWAYS UNIT  C'3' FOR DISTRIB RECS           
         MVC   TMP.ACTKLDG,SVLDG       PUT IN LDGR/ADVRTSR CODE IN KEY          
         MVC   TMP.ACTKACT(L'SVMKTCD),SVMKTCD  MARKET CODE IN KEY               
         DROP  TMP                                                              
         ZIC   R1,LOWLEV                                                        
         LA    R5,IOKEY+3(R1)                                                   
         LA    R6,IOKEY+32                                                      
         SR    R6,R5                                                            
         EX    R6,*+4                                                           
         MVC   0(0,R5),BCSPACES                                                 
         IC    RF,FVILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+4              MOVE OUTLET CODE FROM SCRN TO KEY            
         MVC   0(0,R5),FVIFLD                                                   
*                                                                               
         L     R1,=AL4(XORDUP+XOACCDIR+XIO4)   READ FOR UPDATE                  
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO4) GETREC FOR UPDATE                
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
*                                                                               
         USING NAMELD,R3                                                        
         LA    R3,BOELEM                                                        
         GOTO1 AGETEL,BOPARM,('NAMELQ',AIO4),0   GET X'20' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVNAME,NAMEREC      SAVE ACCOUNT NAME                            
         B     EXITOK                                                           
         DROP  R3                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AND VALIDATING NUM OF UNITS.             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
GETUNT   LA    RF,UNITTBL                                                       
         B     ITER                                                             
*                                                                               
UNITTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISUNIT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALUNIT)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY NUMBER OF UNITS                                                       
***********************************************************************         
*                                                                               
         USING TLSTD,R3                                                         
DISUNIT  DS    0H                                                               
         L     R3,ATLST                                                         
         CLI   CSACT,A#LST         IF ACTION LIST SCHEME CODE IS SVD            
         BNE   DISUNT10                                                         
         MVC   FVIFLD(12),LISTUNTS                                              
         B     DISUNX                                                           
DISUNT10 DS    0H                                                               
         OC    TLPUNIT,TLPUNIT                                                  
         BZ    DISUNX                                                           
         EDIT  TLPUNIT,(12,FVIFLD),2,WRK=BOWORK1,DUB=BODUB1,ALIGN=LEFT          
DISUNX   B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
***********************************************************************         
* VALIDATE NUMBER OF UNITS TYPED IN BY USER                                     
***********************************************************************         
         USING DSCELD,R6                                                        
         USING TLSTD,R3                                                         
*                                                                               
VALUNIT  DS    0H                                                               
*                                                                               
         L     R3,ATLST                                                         
         ZAP   TLPUNIT,=P'0'                                                    
         CLI   FVILEN,0                                                         
         BE    VALUNITX                                                         
         CLI   FVIFLD,C'-'         DID USER PUT IN NEGATIVE UNITS               
         BE    EXITNV                                                           
         ZIC   R5,FVILEN                                                        
         GOTO1 VCASHVAL,BOPARM,(X'80',FVIFLD),(R5)                              
         CLI   BOPARM,0                                                         
         BNE   EXITNV                                                           
         ZAP   TLPUNIT,BOPARM+4(8)                                              
VALUNITX B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT LEVEL FILTER                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* GSRECKEY HOLDS KEY OF THE RECORD                                    *         
* GSRECSTA HOLDS STATUS                                               *         
***********************************************************************         
*                                                                               
DOFLTLV  LA    RF,FLTLVTBL                                                      
         B     ITER                                                             
*                                                                               
FLTLVTBL DS    0H                                                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLVLS)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTLVLS)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTLVLS)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY A ACCOUNT LEVELS FILTER FIELD                               *         
***********************************************************************         
*                                                                               
DFLTLVLS MVC   FVIFLD(3),FLTIFLD                                                
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE ACCOUNT LEVEL FILTER FIELD                                 *         
***********************************************************************         
*                                                                               
VFLTLVLS DS    0H                                                               
         ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,FLTIFLD,FVIFLD   MOVE IN LEVEL IN FILTER FIELD                
*                                                                               
         CLI   FVILEN,1            INPUT MUST EITHER BE 1 OR 3 CHAR             
         BH    VFTLV10                                                          
         CLI   FLTIFLD,C'1'        VALID ENTRIES ARE 1,2 1-2                    
         BE    EXITOK                                                           
         CLI   FLTIFLD,C'2'                                                     
         BE    EXITOK                                                           
         CLI   FLTIFLD,C'3'                                                     
         BE    EXITOK                                                           
*                                                                               
VFTLV10  CLI   FVILEN,3            INPUT LEN NOT 1 MUST BE 3                    
         BNE   EXITL                                                            
         CLC   FLTIFLD(3),=C'1-2'                                               
         BE    EXITOK                                                           
         B     EXITL                                                            
*                                                                               
***********************************************************************         
* DO FILTERING ON LEVELS                                              *         
* USER HAS THE FOLLOWING THREE OPTIONS                                *         
* 1) LEVEL=1   (MEANS SHOW ACCOUNTS ONLY ON HIGHEST LEVEL.            *         
* 2) LEVEL=2   (MEANS SHOW ACCOUNTS ONLY ON NEXT TO HIGHEST LEVEL.    *         
* 3) LEVEL=1-2 (MEANS SHOW ACCOUNTS ONLY ON FIRST AND SECOND LEVEL.   *         
***********************************************************************         
*                                                                               
T        USING ACTRECD,GSRECKEY                                                 
DOFTLVLS DS    0H                                                               
         CLC   FLTIFLD(3),=C'1-2'  USER WANTS 1-2 LEVEL                         
         BE    DOFTLV20                                                         
         CLI   FLTIFLD,C'1'        DOES USER WANT HIGHEST LEVEL                 
         BNE   DOFTLV10                                                         
         ZIC   R5,LEVA                                                          
         LA    R1,T.ACTKACT(R5)                                                 
         CLI   0(R1),C' '                                                       
         B     DOFTCHK                                                          
*                                                                               
DOFTLV10 DS    0H                                                               
         CLI   FLTIFLD,C'2'        DOES USER WANT HIGHEST LEVEL                 
         BNE   DOFTLV20            NOT 2 HAS TO BE 1-2                          
         ZIC   R5,LEVA                                                          
         LA    R1,T.ACTKACT(R5)                                                 
         CLI   0(R1),C' '                                                       
         BE    FLTXL                                                            
         ZIC   R5,LEVB                                                          
         LA    R1,T.ACTKACT(R5)                                                 
         CLI   0(R1),C' '                                                       
         B     DOFTCHK             DON'T NEED ACCOUNTS AT HIGHEST LVL           
*                                                                               
DOFTLV20 CLI   FLTIFLD,C'3'        DOES USER WANT HIGHEST LEVEL                 
         BNE   DOFTLV30            NOT 2 HAS TO BE 1-2                          
         LHI   RE,2                                                             
         ZIC   R5,LEVA                                                          
DOFTLV25 LA    R1,T.ACTKACT(R5)                                                 
         CLI   0(R1),C' '                                                       
         BE    FLTXL                                                            
         IC    R5,LEVB                                                          
         BCT   RE,DOFTLV25                                                      
         IC    R5,LEVC                                                          
         LA    R1,T.ACTKACT(R5)                                                 
         CLI   0(R1),C' '                                                       
         B     DOFTCHK             DON'T NEED ACCOUNTS AT HIGHEST LVL           
DOFTLV30 ZIC   R5,LEVB             GET LEVEL 2 ACCOUNTS SECOND LEVEL            
         LA    R1,T.ACTKACT(R5)                                                 
         CLI   0(R1),C' '                                                       
*                                                                               
DOFTCHK  BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  T                                                                
         EJECT                                                                  
**********************************************************************          
* GETLEVS SUBROUTINE MAKES LEVELS SOFT, RETURNS INDIVIDUAL LEVEL     *          
* LENGTHS AND GIVES NUMBER OF NESTED LEVELS IN LEDGER RECDS          *          
**********************************************************************          
GETLEVS  NTR1                                                                   
         USING ACLELD,R5                                                        
         LA    R5,BOELEM                                                        
         XC    LEVELS(LEVLNQ),LEVELS     CLEAR LEVELS LENGTH/DISC               
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LHI   R0,LEVELQ                 R0 = MAXIMUM NUMBER OF LEVELS          
         STC   R0,LEVNUM                 ASSUME 4 LEVEL STRUCTURE               
         LA    R1,ACLVALS                R1 = FIRST LEVEL LENGTH                
         LA    RE,LEVELS                 STORE ACCUMULATIVE LNTH HERE           
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R6,R6                                                            
*                                                                               
GLEV10   ICM   R6,1,0(R1)                CURRENT LEVEL LENGTH                   
         BZ    GLEV20                    NO MORE LEVELS - ADJUST LEVNUM         
         STC   R6,0(RE)                                                         
         SR    R6,R3                     SUBTRACT CURRENT FROM PREVIOUS         
*                                                                               
         STC   R6,0(R2)                  CURRENT INDIVIDUAL LENGTH              
         IC    R3,0(R1)                  UPDATE R3                              
         LA    R1,L'ACLVALS(R1)                                                 
         LA    R2,1(R2)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,GLEV10                                                        
         B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ                 R1 = MAXIMUM NUMBER OF LEVELS          
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
*                                                                               
GLEVX    B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO UPDATE MARKET RECORDS                                 *         
***********************************************************************         
         USING DSCELD,R6                                                        
UPD      NTR1                                                                   
         NI    FLAG,FF-(FLGSTAR+FLGHIGH) TURN OFF STAR/HIGH LEVEL ACCT          
         SR    R1,R1                                                            
         CLI   LEVC,0                                                           
         BE    UPD90                                                            
         IC    R1,LEVB                                                          
         LA    R5,IOKEY+3(R1)                                                   
         ZIC   R6,LEVC                                                          
         SR    R6,R1                                                            
         BCTR  R6,0                                                             
         EX    R6,*+4                                                           
         MVC   0(0,R5),BCSPACES                                                 
         LR    R6,R2                                                            
         B     UPD20                                                            
*                                                                               
UPD10    DS    0H                                                               
         L     R1,=AL4(XORDUP+XOACCDIR+XIO2)   READ FOR UPDATE                  
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2) GETREC FOR UPDATE                
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         L     R6,AIO2                                                          
*                                                                               
UPD20    LA    R6,ACTRFST-ACTRECD(R6)                                           
UPD30    CLI   0(R6),0             GET 62 ELEMENT                               
         BE    UPD110                                                           
         CLI   0(R6),DSCELQ                                                     
         BNE   UPD40                                                            
         CLC   DSCCODE,SVSCHMCD    IS IT SAME SCHEME/MARKET CODE                
         BE    UPD50                                                            
UPD40    ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     UPD30                                                            
*                                                                               
UPD50    DS    0H                                                               
         AP    DSCVAL,NEWAMT                                                    
         CP    DSCVAL,=P'0'                                                     
         BNH   UPD55                                                            
         TM    FLAG,FLGHIGH                                                     
         BO    UPD80                                                            
         B     UPD70                                                            
*                                                                               
UPD55    TM    FLAG,FLGSTAR        ARE WE UPDATING STAR RECS                    
         BO    UPD60                                                            
         TM    FLAG,FLGHIGH        DID USER INPUT AT HIGHEST LEVEL              
         BO    UPD60               NO                                           
         MVI   0(R6),X'FF'         DELETE X'62'ELEMENT                          
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',AIOREC),0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
         OI    FLAG,FLGHIGH        UPDATE HIGHEST LEVL IF 3 LVLS LDGR           
         B     UPD90                                                            
*                                                                               
UPD60    MVI   0(R6),X'FF'         DELETE X'62' ELEM                            
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',AIO2),0                     
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
UPD70    OI    FLAG,FLGHIGH        UPDATE HIGHEST LEVL IF 3 LVLS LDGR           
         TM    FLAG,FLGSTAR        ARE WE UPDATING STAR RECS                    
         BNO   UPD90                                                            
*                                                                               
UPD80    DS    0H                                                               
         LHI   R1,XOPUT+XOACCMST+XIO2  PUT THE RECORD BACK                      
         GOTO1 AIO                                                              
UPD90    DS    0H                                                               
         ZIC   R1,LOWLEV                                                        
*        BCTR  R1,0                                                             
         EX    R1,CLCSTAR                                                       
         BE    UPDX                                                             
         IC    R1,LEVA                                                          
         LA    R5,IOKEY+3(R1)                                                   
         ZIC   R6,LEVB                                                          
         SR    R6,R1                                                            
         BNP   UPD100                                                           
         BCTR  R6,0                                                             
         EX    R6,CLCSPA                                                        
         BE    UPD100                                                           
         EX    R6,MVSPA                                                         
*                                                                               
         TM    FLAG,FLGHIGH        DID USER INPUT AT LOWEST LEVEL               
         BO    UPD10               YES                                          
         LR    R6,R2                                                            
         B     UPD20                                                            
*                                                                               
UPD100   IC    R1,LOWLEV                                                        
*        BCTR  R1,0                                                             
         EX    R1,MVSTAR                                                        
         OI    FLAG,FLGSTAR        SET STATUS -UPDATING STAR RECORD             
         B     UPD10                                                            
*                                                                               
UPD110   LA    R6,BOELEM                                                        
         MVI   DSCEL,DSCELQ                                                     
         MVI   DSCLN,X'0A'                                                      
         MVC   DSCCODE,SVSCHMCD                                                 
         ZAP   DSCVAL,NEWAMT                                                    
         CP    DSCVAL,=P'0'                                                     
         BNH   UPDX                                                             
*                                                                               
         TM    FLAG,FLGSTAR        ARE WE UPDATING STAR RECS                    
         BO    UPD120                                                           
         TM    FLAG,FLGHIGH        DID USER INPUT AT HIGHEST LEVEL              
         BO    UPD120              NO                                           
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIOREC,BOELEM,0                    
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
         OI    FLAG,FLGHIGH        START UPDATING HIGHEST LEVEL ACCOUNT         
         B     UPD90                                                            
*                                                                               
UPD120   GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO2,BOELEM,0                      
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
         B     UPD80                                                            
UPDX     DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
CLCSTAR  CLC   IOKEY+3(0),=12C'*'                                               
CLCSPA   CLC   0(0,R5),BCSPACES                                                 
MVSTAR   MVC   IOKEY+3(0),=12C'*'                                               
MVSPA    MVC   0(0,R5),BCSPACES                                                 
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST LINE UNIT=3 NAME  /LEDGER=? NAME               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LSTDATA  LA    RF,LSTDTBL                                                       
         B     ITER                                                             
*                                                                               
LSTDTBL  DS    0H                                                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISLDTA)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY LIST LINE UNIT=3 NAME / LEDGER=? NAME                                 
***********************************************************************         
T        USING PLSTLND,FVIFLD                                                   
DISLDTA  DS    0H                                                               
         MVC   T.PUNITEQL(L'T.PUNITEQL),=C'UNIT='                               
         MVC   T.PUNT,ACTKUNT                                                   
*                                                                               
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         DROP  TEMP                                                             
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITRNF             ACCOUNT NOT FOUND EXIT WITH ERR              
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
         USING NAMELD,R6                                                        
         LA    R6,BOELEM                                                        
         GOTO1 AGETEL,BOPARM,('NAMELQ',AIO2),0   GET X'20' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   T.PUNTNM,NAMEREC     SAVE ADVERTSER NAME                         
         DROP  R6                                                               
*                                                                               
         MVI   T.PSLASH,C'/'                                                    
         MVC   T.PLEDGEQL,=C'LEDGER='                                           
         MVC   T.PLEDG,2(R2)                                                    
*                                                                               
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,2(R2)     PUT IN LDGR/ADVRTSR CODE IN KEY           
         DROP  TEMP                                                             
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITRNF             ACCOUNT NOT FOUND EXIT WITH ERR              
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
         USING NAMELD,R6                                                        
         LA    R6,BOELEM                                                        
         GOTO1 AGETEL,BOPARM,('NAMELQ',AIO2),0   GET X'20' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   T.PLEDGNM,NAMEREC     PRINT LEDGER NAME                          
         DROP  R6                                                               
*                                                                               
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
*                                                                               
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING ACTRECD,R2                                                       
LAST     USING ACTRECD,R3                                                       
         LA    RF,LISTABL                                                       
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMPAGE    CHECK PAGE OK (0 FOR LIST)                   
         BE    LITER04                                                          
         LA    RF,OBJTABL(RF)                                                   
         B     LITER                                                            
*                                                                               
LITER04  ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LINIT),AL1(0,0,0),AL4(INITL)                                 
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR)                            
         DC    AL1(LTSARFIL),AL1(0,0,0),AL4(TSARFIL)                            
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LTSARDIR),AL1(0,0,1),AL4(TSARDIR)                            
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
*                                                                               
FLST     MVC   IOKEY(L'ACTKEY),THIS.ACTRECD                                     
         L     R1,=AL4(XOHID+XOACCDIR+XIO1)                                     
         GOTO1 AIO                                                              
         BNE   EXITL               MESS UP ON THE READ HIGH                     
         OC    SVDISP,SVDISP                                                    
         BZ    NLST02                                                           
         OI    FLAG,FLGDISP        DISPLACEMENT IS AVAILABLE                    
         B     NLST02                                                           
*                                                                               
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
*                                                                               
TEMP     USING ACTRECD,IOKEY                                                    
NLST     DS    0H                                                               
         NI    FLAG,FF-FLGDISP     TURN OFF DISPLACEMENT CHK FLAG               
         CLC   SVDISP,=F'0'                                                     
         BNE   NLST03                                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TEMP.ACTKACT+L'ACTKACT-1                                      
         AHI   RF,1                                                             
         STC   RF,TEMP.ACTKACT+L'ACTKACT-1                                      
         L     R1,=AL4(XOHID+XOACCDIR+XIO1)                                     
         GOTO1 AIO                                                              
*                                                                               
NLST02   CLC   TEMP.ACTKCPY(L'ACTKCPY+L'ACTKUNT+L'ACTKLDG),THIS.ACTKCPY         
         BNE   EXITL               FOR ACCOUNT U/L IS REQUIRED & FIXED          
         CLC   TEMP.ACTKACT,BCSPACES                                            
         BNH   NLST                MUST HAVE AN ACCOUNT                         
         GOTO1 ATSTSEC                                                          
         MVC   FVXTRA,BCSPACES                                                  
         BNE   NLST                                                             
*                                                                               
NLST02A  L     R1,=AL4(XOGET+XOACCMST+XIO1)                                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD COMPANY RECORD                           
NLST03   BAS   RE,GETSCHM     GET SCHMCODES AND UNITS                           
         CLC   SVDISP,=F'0'                                                     
         BE    NLST                                                             
*                                                                               
         MVC   THIS.ACTKEY(ACCKLEN),TEMP.ACTRECD                                
         B     EXITOK                                                           
         DROP  TEMP                                                             
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
INITL    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
*                                                                               
FTFLST   DS    0H                                                               
         XC    SVDISP,SVDISP                                                    
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* SET UP TSAR FROM DIRECTORY 1                                        *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R3                                                         
TSARFIL  L     R3,ATLST                                                         
         MVC   TLRLEN,=AL2(LISTLNQ)                                             
         L     R5,SVDISP                                                        
         A     R5,AIOREC                                                        
*                                                                               
         USING DSCELD,R5                                                        
*                                                                               
TSARFL10 DS    0H                                                               
         MVC   LISTSCCD,DSCCODE                                                 
         EDIT  DSCVAL,(12,LISTUNTS),2,WRK=BOWORK1,DUB=BODUB1,ALIGN=LEFT         
         B     EXITOK                                                           
         DROP  R3,R5                                                            
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION FOR MAINTENANCE LIST                                 *         
***********************************************************************         
         SPACE 1                                                                
INITL1   DS    0H                                                               
*        OI    LSSTAT1,LSSBALL                                                  
         OI    LSSTAT2,LSSADD+LSSNOSEQ+LSS1HEAD                                 
         MVC   LSCOLLIN,=AL2(80)                                                
         MVC   LSLINROW,=AL2(1)                                                 
         GOTOX ('SAVFDR',AGROUTS),BOPARM,(C'C',7)  CLEAR KEY FIELDS             
*                                                  IMDB#2109531                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR MAINTENANCE LIST                                     *         
***********************************************************************         
*                                                                               
FTFLST1  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST FOR MAINTENANCE LIST                                          *         
***********************************************************************         
*                                                                               
FLST1    DS    0H                                                               
         L     R2,AIOREC                                                        
         MVC   IOKEY(L'ACTKEY),0(R2)                                            
         MVI   IOKEY+14,X'41'                                                   
         CLI   STRKEY,0                                                         
         BE    *+10                                                             
         MVC   IOKEY(L'STRKEY),STRKEY  START DISPLAYING AT THIS ACCOUNT         
         L     R1,=AL4(XOHID+XOACCDIR+XIO1)                                     
         GOTO1 AIO                                                              
         B     NML20                                                            
***********************************************************************         
* NEXT FOR MAINTENANCE LIST                                           *         
***********************************************************************         
*                                                                               
TEMP     USING ACTRECD,IOKEY                                                    
NLST1    DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'IOKEYLST),IOKEYLST  START READING FROM THIS KEY          
NML10    LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         CLC   TEMP.ACTKCPY(L'ACTKCPY+L'ACTKUNT+L'ACTKLDG),IOKEYSAV             
         BNE   EXITL               FOR ACCOUNT U/L IS REQUIRED & FIXED          
NML20    DS    0H                                                               
*        CLI   IOKEY+3,C'*'                                                     
*        BE    NML30                                                            
*                                                                               
         L     R1,=AL4(XOGET+XOACCMST+XIO1)                                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD COMPANY RECORD                           
         ZIC   R1,LOWLEV           LOWEST LEVEL LENGTH                          
         AHI   R1,2                PLUS C/U/L = LENGTH FOR EX INSTR.            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IOKEYSAV(0),IOKEY ARE WE STILL DOING SAME HIGHEST LEVEL          
         BNE   EXITL                                                            
*                                                                               
         BAS   RE,GETNAME          DID WE GET A NAME                            
         CLC   SVNAME,BCSPACES                                                  
         BE    NML30               NO READ NEXT RECORD                          
*                                                                               
         CLI   CSACT,A#MAINT                                                    
         BNE   *+12                                                             
         L     R3,AIO1                                                          
         BRAS  RE,GETSCUN                                                       
         BE    NML30               UNITS EXIST NOT GOOD FOR ADD ACTION          
*                                                                               
         MVC   SVKEY,IOKEY         SAVE IOKEY FOR VALIDATING UNITS              
         ZIC   R3,IOKEY+14                                                      
         AHI   R3,1                                                             
         STC   R3,IOKEY+14                                                      
         MVC   IOKEYLST,IOKEY                                                   
         B     EXITOK                                                           
*                                                                               
NML30    DS    0H                                                               
         ZIC   R3,IOKEY+14                                                      
         AHI   R3,1                                                             
         STC   R3,IOKEY+14                                                      
         B     NML10                                                            
***********************************************************************         
* FILL IN TSAR WITH MAINT SCREEN LIST VALUES                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R3                                                         
TSARFIL1 DS    0H                                                               
         L     R3,ATLST                                                         
         MVC   TLRLEN,=AL2(TLLNQ)                                               
*                                                                               
         L     R6,AIO1                                                          
         MVC   TLSKEY,3(R6)        C/U/L PASSED THE KEY                         
         MVC   TLSNAME,SVNAME                                                   
*                                                                               
*        CLI   CSACT,A#MAINT       FOR ADD ACTION NO UNITS TO DISPLAY           
*        BE    EXITOK                                                           
*                                                                               
         USING DSCELD,R5           LOOK FOR UNITS                               
         L     R5,AIO1                                                          
         LA    R5,ACTRFST-ACTRECD(R5)                                           
TSARFM20 CLI   0(R5),0             GET 62 ELEMENT                               
         BE    EXITOK                                                           
         CLI   0(R5),DSCELQ        IS IT SCHEME ELEMENT                         
         BNE   TSARFM25                                                         
         CLC   DSCCODE,SVSCHMCD    IS IT SAME SCHEME CODE                       
         BE    TSARFM30                                                         
TSARFM25 ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     TSARFM20                                                         
TSARFM30 DS    0H                                                               
         ZAP   TLPUNIT,DSCVAL                                                   
         B     EXITOK                                                           
         DROP  R3,R5                                                            
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  DS    0H                                                               
         LM    R2,R3,SVPARMS3                                                   
         CLI   CSACT,A#CHA         ONLY UPDATE ELEMENT IF WE HAVE               
         BE    *+12                A MAIN ACTION OF CHANGE OR MAINTAIN          
         CLI   CSACT,A#MAINT                                                    
         BNE   EXITOK                                                           
*                                                                               
         L     R3,ATLST                                                         
         OC    TLPUNIT,TLPUNIT                                                  
         BZ    EXITOK                                                           
*                                                                               
         CLI   TLSKEY,C'*'       CANNOT CHANGE/ADD UNITS TO STAR RECS           
         BE    EXITCHNA          EXIT WITH CHANGE NOT ALLOWED MESSAGE           
*                                                                               
         ZAP   NEWAMT,=P'0'                                                     
         LA    RF,IOKEY                                                         
T        USING ACTRECD,RF                                                       
         MVC   T.ACTKEY,BCSPACES     INITIALIZE KEY OF RECORD                   
         MVC   T.ACTKCPY,CUABIN      CONNECTED ID AGENCY BINARY                 
         MVI   T.ACTKUNT,C'3'        ALWAYS UNIT  C'3' FOR DISTRIB RECS         
         MVC   T.ACTKLDG,SVLDG       PUT IN LDGR/ADVRTSR CODE IN KEY            
         MVC   T.ACTKACT(L'TLSKEY),TLSKEY    OUTLET IN KEY                      
         DROP  T                                                                
*                                                                               
         L     R1,=AL4(XORDUP+XOACCDIR+XIO4)   READ FOR UPDATE                  
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         CLC   IOKEYSAV(0),IOKEY                                                
         BE    *+6                 NOT THIS CLUSTER OF ACCOUNTS                 
         DC    H'0'                                                             
*                                                                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO4) GETREC FOR UPDATE                
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
*                                                                               
UPDREC10 DS    0H                                                               
         USING DSCELD,R6                                                        
         L     R6,AIO4                                                          
         LA    R6,ACTRFST-ACTRECD(R6)                                           
UPDREC20 CLI   0(R6),0             GET 62 ELEMENT                               
         BE    UPDREC70                                                         
         CLI   0(R6),DSCELQ                                                     
         BNE   UPDREC30                                                         
         CLC   DSCCODE,SVSCHMCD    IS IT SAME SCHEME/MARKET CODE                
         BE    UPDREC40                                                         
UPDREC30 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     UPDREC20                                                         
*                                                                               
UPDREC40 CP    DSCVAL,TLPUNIT                                                   
         BE    EXITOK                                                           
         SP    NEWAMT,DSCVAL       FOR TOTAL **** RECORD                        
         CP    TLPUNIT,=P'0'                                                    
         BH    UPDREC50           DELETE ELEMENT 62 IF NEW AMNT ENTER           
*                                                                               
         MVI   0(R6),X'FF' DELETE X'62' ELEMENT                                 
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',AIO4),0                     
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
         B     UPDREC60                                                         
*                                                                               
UPDREC50 AP    NEWAMT,TLPUNIT                                                   
         ZAP   DSCVAL,TLPUNIT                                                   
*                                                                               
UPDREC60 DS    0H                                                               
         LHI   R1,XOPUT+XOACCMST+XIO4  PUT THE RECORD BACK                      
         GOTO1 AIO                                                              
         BE    UPDRECX                                                          
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
UPDREC70 DS    0H                                                               
         CP    TLPUNIT,=P'0'        DO THEY WANT TO PUT IN 0 UNITS              
         BNH   UPDRECX                                                          
         AP    NEWAMT,TLPUNIT                                                   
         LA    R6,BOELEM                                                        
         MVI   DSCEL,DSCELQ        BUILD X'62' ELEMENT                          
         MVI   DSCLN,X'0A'                                                      
         MVC   DSCCODE,SVSCHMCD     MOVE IN SCHEME CODE                         
         ZAP   DSCVAL,TLPUNIT                                                   
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO4,BOELEM,0                      
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
         B     UPDREC60                                                         
UPDRECX  DS    0H                                                               
         BAS   RE,UPD              UPDATE MARKET RECORD                         
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO SAVE OFF SCHEMECODES AND UNITS                        *         
* AIO1 CONTAINS ACCOUNT RECORD WITH PROBABLY 62 ELEMENT               *         
***********************************************************************         
*                                                                               
         USING DSCELD,R6           DISTRIBUTION SCHEME ELEMENT DSECT            
GETSCHM  NTR1                                                                   
         L     R6,AIO1                                                          
*                                                                               
         CLC   SVDISP,=F'0'        IS IT FIRST TIME FOR ACCOUNT                 
         BE    GTSCHM05                                                         
*                                                                               
         TM    FLAG,FLGDISP    SCHEME AT SVDISP HAS NOT BEEN PRCCESSED          
         BO    GTSCHMX        YET, PREVIOUS PAGE LAST RECORD IN PROCESS         
*                                                                               
         L     R6,SVDISP                                                        
         A     R6,AIO1                                                          
         B     GTSCHM25                                                         
*                                                                               
GTSCHM05 LA    R6,ACTRFST-ACTRECD(R6)                                           
GTSCHM10 CLI   0(R6),0             GET 62 ELEMENT                               
         BNE   GTSCHM20                                                         
         XC    SVDISP,SVDISP                                                    
         B     GTSCHMX                                                          
*                                                                               
GTSCHM20 CLI   0(R6),DSCELQ                                                     
         BE    GTSCHM30                                                         
GTSCHM25 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GTSCHM10                                                         
GTSCHM30 DS    0H                                                               
         S     R6,AIO1                                                          
         ST    R6,SVDISP                                                        
*                                                                               
GTSCHMX  B     EXITOK                                                           
         DROP  R6                                                               
*********************************************************************           
* SUBROUTINE TO GET NAMES OF ACCOUNT                                            
* AIO1= A(RECORD)                                                               
*********************************************************************           
GETNAME  NTR1                                                                   
         USING NAMELD,R3                                                        
         MVC   SVNAME,BCSPACES                                                  
         LA    R3,BOELEM                                                        
         GOTO1 AGETEL,BOPARM,('NAMELQ',AIO1),0   GET X'20' ELEMENT              
         BNE   EXITOK                                                           
         MVC   SVNAME,NAMEREC      SAVE ADVERTSER NAME                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE OPTIONS/FILTERS                                                      
**********************************************************************          
         SPACE 1                                                                
VALOPT   NTR1                                                                   
         XC    OPFLDS(OPFLDLQ),OPFLDS    CLEAR OPTIONS FIELD                    
         MVI   ERROR,OK                                                         
         CLI   FVILEN,0                                                         
         BE    VALOPTX                                                          
*                                                                               
         LA    R0,SCANRHSL                                                      
         LA    R3,SCANBLK                                                       
         GOTO1 VSCANNER,BOPARM,((R0),FVIHDR),(10,(R3))                          
         MVC   FLAG1,4(R1)                                                      
         MVI   ERROR,NV            NOT VALID                                    
         CLI   FLAG1,0                                                          
         BE    EXITNV                                                           
*                                                                               
         LA    R5,SCANBLK                                                       
         MVI   FNDX,1                                                           
*                                                                               
VALOPT2  CLC   FNDX,FLAG1          ALL PROCESSED - EXIT                         
         BH    VALOPTX                                                          
         MVI   ERROR,NV             CHECK L'KEYWORD                             
         SR    RE,RE                                                            
         IC    RE,0(R5)                                                         
         SHI   RE,1                                                             
         BM    EXITNO                                                           
*                                                                               
         LA    RF,OPTNTAB                                                       
         MVI   ERROR,NV                                                         
         USING PARMD,RF                                                         
         SR    R0,R0                                                            
*                                                                               
VALOPT4  CLI   PARMLEN,0           CHECK FOR VALID KEYWORD                      
         BE    EXITNV                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   PARMWORD(0),12(R5)  IS IT A VALID OPTION                         
         BE    VALOPT6                                                          
*                                                                               
VALOPT5  IC    R0,PARMLEN                                                       
         AR    RF,R0                                                            
         B     VALOPT4                                                          
*                                                                               
VALOPT6  LA    R1,PARMACTS         COMPATIBLE WITH ACTION                       
VALOPT7  CLI   0(R1),0                                                          
         BE    VALOPT5             END OF LIST                                  
         CLI   0(R1),255                                                        
         BE    VALOPT8             VALID FOR ALL ACTIONS                        
         CLC   CSACT,0(R1)         IS IT ACTION DISPLAY                         
         BE    VALOPT8                                                          
         LA    R1,1(R1)            BUMP TO NEXT ACTION                          
         B     VALOPT7                                                          
*                                                                               
VALOPT8  DS    0H                                                               
*        TM    PARMINDS,DDSONLY    OTHER CHECKS                                 
*        BZ    *+14                                                             
*        LR    R8,RA                                                            
*        USING TWAD,R8                                                          
*        CLI   TWAOFFC,C'*'                                                     
*        BNE   VALOPT5                                                          
         MVI   ERROR,NV                                                         
         CLC   1(1,R5),PARMMIN     CHECK MINIMUM LENGTH                         
         BL    EXITSHRT                                                         
         MVI   ERROR,NV                TOO LONG                                 
         CLC   1(1,R5),PARMMAX     CHECK MAX LENGTH                             
         BH    EXITLONG                                                         
*                                                                               
VALOPT9  SR    R6,R6               TEST FOR DUPLICATE                           
         ICM   R6,3,PARMDEST                                                    
         AR    R6,RA               R6 = A(FNDX+OUTPUT VALUE OF PARAM)           
         CLI   0(R6),0                                                          
         MVI   ERROR,NV                                                         
         BNE   EXITDUPE            DUPLICATE ENTRY                              
         MVC   0(1,R6),FNDX        INDICATE THIS IS PROCESSED                   
*                                                                               
VALOPT10 CLI   PARMSR,0            CALL VALIDATE/CONVERT SR IF ANY              
         BNE   VALOPT12                                                         
         MVI   ERROR,NV                                                         
         ICM   RF,15,PARMSR                                                     
         A     RF,BORELO                                                        
         GOTO1 (RF),BOPARM,(R5),(R6) DO SUBROUTINE CALL                         
         CLI   ERROR,OK                                                         
         BNE   EXIT                                                             
         B     VALOPT20                                                         
*                                                                               
VALOPT12 CLI   PARMSR,C'Y'         OR MOVE IN C'Y' TO OUTPUT VALUE              
         BNE   VALOPT14                                                         
         MVI   0(R6),C'Y'                                                       
         B     VALOPT20                                                         
*                                                                               
VALOPT14 ZIC   R1,PARMSR           OR MOVE INPUT TO OUTPUT                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R6),22(R5)                                                   
*                                                                               
VALOPT20 ZIC   R1,FNDX             BUMP TO NEXT FIELD                           
         LA    R1,1(R1)                                                         
         STC   R1,FNDX                                                          
         LA    R5,SCANTOTL(R5)                                                  
         B     VALOPT2                                                          
*                                                                               
VALOPTX  MVI   FNDX,0                                                           
         MVI   ERROR,OK                                                         
         B     EXITOK                                                           
         EJECT                                                                  
*********************************************************************           
*              FILTER VALIDATION ROUTINES                                       
*    R2 = INPUT FIELD SCAN FIELD                                                
*    R3 = DESTINATION                                                           
*              START ACCOUNT                                                    
*********************************************************************           
*                                                                               
VALACC   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(15,R3),LOKEY      HIGH ACCOUNT KEY                             
*                                                                               
         LA    R5,LOWLEV           GET DISPLACEMENT                             
         LA    R6,LASTLEV          GET INDIVIDUAL LENGTH                        
         SR    R6,R5               GET CORRECT LENGTH                           
*                                                                               
VALACC10 DS    0H                                                               
         SR    R1,R1                                                            
         CLC   1(1,R2),0(R6)       CHECK LENGTH OF INPUT VS MAX.                
         BH    VALACCX                                                          
         IC    R1,0(R5)            DISPLACEMENT TO LOW ACCOUNT                  
         LA    R5,3(R3,R1)                                                      
         IC    R1,1(R2)            LENGTH OF INPUT ACCOUNT                      
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R5),22(R2)      ACCOUNT CODE TO START KEY                    
         MVI   ERROR,OK                                                         
         XIT1                                                                   
VALACCX  MVI   ERROR,NV                                                         
         B     EXITINSA                                                         
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SCANMAX  EQU   40                                                               
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#YES,3,L                                                       
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
* TABLE OF INPUT OPTIONS.                                                       
*                                                                               
*              BYTE 0    =ENTRY LENGTH                                          
*              BYTE 1-8  =OPTION KEYWORD                                        
*              BYTE 9    =INDICATORS - BIT 0ON=DDS-ONLY OPTION                  
*              BYTE 10   =MIN LENGTH OF DATA VALUE                              
*              BYTE 11   =MAX LENGTH OF DATA VALUE                              
*              BYTE 12-13=DISPLACEMENT FROM START OF W/S OF ADDRESS OF          
*                        PROCESSED PARAMETER VALUE                              
*              BYTE 14-17=B0 = 0    - A(VALIDATE/CONVERT SR)                    
*                              C'Y' - MOVE C'Y' TO OUTPUT                       
*                              ELSE - MOVE IN TO OUT FOR L'B0                   
*              BYTE 18-  =STRING OF COMPATIBLE ACTION NUMBERS ENDED BY          
*                         ZERO                                                  
OPTNTAB  DS    0C                                                               
OPTSTR   DC    AL1(OPTSTRX-*),CL8'START',X'00010C',AL2(STRKEY-TWAD)             
         DC    AL4(VALACC),AL1(OK)                                              
OPTSTRX  DS    0H                                                               
         DC    X'00'                                                            
         EJECT                                                                  
*********************************************************************           
*            TO GET MATCHING SCHEME CODE AND UNITS                  *           
*            R3 = A(ACCOUNT)                                        *           
*********************************************************************           
         USING DSCELD,R3                                                        
GETSCUN  DS    0H                                                               
         LA    R3,ACTRFST-ACTRECD(R3)                                           
GETSCU10 CLI   0(R3),0             GET 62 ELEMENT                               
         JNE   GETSCU20                                                         
         CLI   0(R3),1             SET CC NOT EQUAL                             
         BR    RE                                                               
*                                                                               
GETSCU20 CLI   0(R3),DSCELQ        IS IT X'62'                                  
         JNE   GETSCU30                                                         
         CLC   DSCCODE,SVSCHMCD    IS IT SAME SCHEME/MARKET CODE                
         BER   RE                                                               
*                                                                               
GETSCU30 ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         J     GETSCU10                                                         
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
*                                                                               
*ACFILWORK                                                                      
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                               *         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
SVDISP   DS    F                                                                
SVSCHMCD DS    CL2                 SAVED SCHEME/WORK CODE                       
SVANAME  DS    CL36                SAVE ADVERTISR NAME                          
SVSCHNM  DS    CL15                SAVE SCHEME CODE DESCRIPTION                 
SVMKTCD  DS    CL9                 SAVED MARKET CODE                            
SVLDG    DS    CL1                 SAVED ADVERTISER/LEDGER CODE                 
FLAG     DS    XL1                 STATUS                                       
FLGDISP  EQU   X'80'               DISP AVLBL FRM LAST PAGE'S LAST RECD         
FLGHIGH  EQU   X'40'               USER INPUT IN MARKET IS AT LOWST LVL         
FLGSTAR  EQU   X'20'               UPDATING RECORD WITH STARS IN IT             
IOKEYLST DS    XL15                LAST KEY FOR UNITS/OUTLET/OULT NAME          
SVKEY    DS    XL64                LAST KEY FOR UNITS/OUTLET/OULT NAME          
LOKEY    DS    XL64                KEY AT MARKET LEVEL                          
LOWLEV   DS    XL1                 LOWEST LEVEL                                 
LASTLEV  DS    XL1                 LOW  LEVEL OF ACCOUNT                        
LEVELS   DS    0XL1                                                             
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVLNQ   EQU   *-LEVELS                                                         
*                                                                               
OPFLDS   DS    0C                                                               
STRKEY   DS    CL15                ACCOUNT START KEY                            
OPFLDLQ  EQU   *-OPFLDS                                                         
SCANBLK  DS    10CL(SCANTOTL)                                                   
SVOPTN   DS    CL(OPFLDLQ)         SAVED OPFLDS                                 
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
*                                                                               
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
SVREG    DS    A                   SAVED REGISTER                               
SAVRB    DS    F                                                                
DISPERR  DS    A                                                                
SVELADDR DS    A                   A(THIS ELEMENT)                              
*                                                                               
LEVLNQS  DS    0XL1                                                             
LEVLNQA  DS    XL1                 LEVEL A INDIVIDUAL LENGTH                    
LEVLNQB  DS    XL1                 LEVEL B INDIVIDUAL LENGTH                    
LEVLNQC  DS    XL1                 LEVEL C INDIVIDUAL LENGTH                    
LEVLNQD  DS    XL1                 LEVEL D INDIVIDUAL LENGTH                    
*                                                                               
LEVNUM   DS    XL1                 NUMBER OF LEVELS IN HEIRARCHY                
LEVELQ   EQU   4                   MAXIMUM NUMBER OF LEVELS                     
*                                                                               
SVNAME   DS    CL36                SAVE ACCOUNT NAME                            
SVLEN    DS    XL1                 SAVED INPUT LENGTH OF OUTLET FIELD           
NEWAMT   DS    PL6                 TOTAL AMOUNT TO UPDATE ACCT RECORD           
*                                                                               
SVLINE2H DS    CL8                                                              
SVLINE2  DS    0CL67                                                            
         DS    CL5                                                              
STARLQ   EQU   *-SVLINE2H                                                       
STAR     DS    CL1                 PUT STAR HERE IF NO DATE                     
         DS    CL61                                                             
SVADDR2  DS    A                                                                
*                                                                               
SVLINE3H DS    CL8                                                              
SVLINE3  DS    CL67                                                             
SVADDR3  DS    A                                                                
*                                                                               
SCANRHSL EQU   20                                                               
SCANTOTL EQU   (22+SCANRHSL)       LENGTH OF SCAN BLOCK                         
FLAG1    DS    CL1                                                              
FNDX     DS    CL1                                                              
ERROR    DS    CL1                                                              
NV       EQU   X'80'                                                            
DUP      EQU   X'40'                                                            
OK       EQU   X'FF'                                                            
*                                                                               
DSLIST   DS    0C                                                               
AC@YESDL DS    CL3                 YES                                          
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLUSER                                                           
LISTSCCD DS    XL(L'DSCCODE)       SCHEME/WORK CODES                            
LISTUNTS DS    XL12                UNITS                                        
LISTBIT  DS    CL1                 OFF IF SAME ACCOUNT IN LIST                  
LISTLNQ  EQU   *-TLSTD                                                          
*                                                                               
TLSTD    DSECT                     FOR MAINTENANCE LIST                         
         ORG   TLKSRT                                                           
TLSKEY   DS    CL12                                                             
         ORG                                                                    
         ORG   TLUSER                                                           
TLSCODE  DS    XL(L'DSCCODE)       SCHEME/WORK CODES                            
TLSNAME  DS    CL36                                                             
TLPUNIT  DS    PL6                 PACKED UNITS                                 
TLLNQ    EQU   *-TLSTD                                                          
*                                                                               
PLSTLND  DSECT               LOOKS LIKE UNIT=? NAME / LEDGER=? NAME             
PUNITEQL DS    CL5                 C'UNIT='                                     
PUNT     DS    CL1                 PRINT UNIT                                   
         DS    CL1                 SPACE                                        
PUNTNM   DS    CL8                 UNIT'S DESCRIPTION                           
         DS    CL1                 SPACE                                        
PSLASH   DS    CL1                 PRINT SLASH C'/'                             
         DS    CL1                 SPACE                                        
PLEDGEQL DS    CL7                 PRINT 'LEDGER='                              
PLEDG    DS    CL1                 PRINT LEDGER                                 
         DS    CL1                 SPACE                                        
PLEDGNM  DS    CL30                LEDGER DESCRIPTION                           
*                                                                               
*              DSECT TO COVER ACTION PARAMETER (OPTION) TABLE ENTRY             
*                                                                               
PARMD    DSECT                                                                  
PARMLEN  DS    CL1       B         LENGTH OF TABLE ENTRY                        
PARMWORD DS    CL8       C         PARAMETER KEYWORD                            
PARMINDS DS    XL1       X         INDICATORS                                   
PARMMIN  DS    XL1       B         MIN LENGTH OF RHS                            
PARMMAX  DS    XL1       B         MAX LENGTH OF RHS                            
PARMDEST DS    CL2       B         DISPLACEMENT FROM START OF W/S OF            
*                                  ADDRESS OF PROCESSED PARAMETER VALUE         
PARMSR   DS    CL4       B         B0 = 0    - A(VALIDATE/CONVERT SR)           
*                                  B0 = C'Y' - MOVE C'Y' TO OUTPUT              
*                                  ELSE      - MOVE IN TO OUT FOR L'B0          
PARMACTS DS    0C        B         STRING OF COMPATIBLE ACTNNUMS ENDED          
*                                  BY ZERO                                      
*                                                                               
         EJECT ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACFIL32   08/10/11'                                      
         END                                                                    
