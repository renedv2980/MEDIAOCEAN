*          DATA SET ACFIL33    AT LEVEL 019 AS OF 02/20/13                      
*&&      SET   NOP=N                                                            
*PHASE T62333A                                                                  
****************************************************************                
*      DISPLAYS RETAIL SCHEME RECORD                           *                
*                                                              *                
****************************************************************                
         TITLE 'RETAIL SCHEME RECORD OBJECT VERSION'                            
         SPACE 2                                                                
FIL33    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL33**,R7,RR=RE                                              
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
* EXIT                                                                *         
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
EXITLONG MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               EXIT WITH INPUT TOO LONG                     
EXITSHRT MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     EXITL               EXIT WITH INPUT TOO SHORT                    
EXITDUPE MVC   FVMSGNO,=AL2(FVFDUPE)                                            
         B     EXITL               EXIT WITH DUPLICATE ENTRY                    
EXITINSA MVC   FVMSGNO,=AL2(AE$INSTA)                                           
         B     EXITL               EXIT WITH INVALID START ACCOUNT              
EXITINUF MVC   FVMSGNO,=AL2(AE$INVUN)                                           
         B     EXITL               EXIT WITH INVALID UNITS FILTER               
EXITINJC MVC   FVMSGNO,=AL2(AE$INVJC)                                           
         B     EXITL               EXIT WITH INVALID JOB CODE                   
EXITMIF  MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITL               EXIT WITH MISSING INPUT FIELD                
*                                                                               
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
*                                                                               
         USING CPYRECD,R2                                                       
INIT     DS    0H                                                               
         CLC   CSACT,SVACT         DID ACTION CHANGE                            
         BE    *+16                NO                                           
         XC    GCLASKEY,GCLASKEY   ELSE INDICATE KEY CHANGE                     
         MVC   SVACT,CSACT                                                      
*                                                                               
         XC    OPFLDS(OPFLDLQ),OPFLDS    CLEAR OPTIONS FIELD                    
         MVI   LVLBYTE,0                                                        
         ZAP   ADJAMT,=P'0'                                                     
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
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
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
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
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
***********************************************************************         
* LAST  TIME FOR KEY OBJECT                                           *         
***********************************************************************         
*                                                                               
KEYLAST  L     R1,SVPARMS4         TABLE OF KNOWN INVOKERS                      
         LA    RF,KLASTBL          TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KLASTBL  DS    0H                                                               
         DC    AL1(KVAL),AL1(0,0,0),AL4(KLASVAL)    VALIDATE KEY LAST           
         DC    AL1(EOT)                                                         
***********************************************************************         
* LAST  TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
*                                                                               
KLASVAL  DS    0H                                                               
         CLI   GSRECKEY+3,C' '                                                  
         BNE   EXITOK                                                           
         CLI   LEVNUM,1                                                         
         BNH   EXITOK                                                           
         MVC   BOCURSOR,ALVLAFLD   POINT CURSOR TO LEVEL A FIELD                
         B     EXITMIF                                                          
*                                                                               
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
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
*                                                                               
KNOWTAB  DC    AL2(RE#ACODE),AL4(ACODE)    LEDGER CODE /ADVERTISER CODE         
         DC    AL2(RE#ADNAME),AL4(ADNAME)  LEDGER/ADVERTISER CODE NAME          
         DC    AL2(RE#SCODE),AL4(SCODE)    SCHEME CODE                          
         DC    AL2(RE#SCDFLT),AL4(SCDFLT)  SCHEME CODE TSAR LEVEL               
         DC    AL2(RE#SCDNM),AL4(SCDNM)    SCHEME CODE NAME                     
         DC    AL2(RE#FLTR),AL4(FLTR)      D-FLTR FIELD ON MAINTNC SCRN         
         DC    AL2(RE#LEVACD),AL4(LEVACD)  K-LEVEL A,B,C CODE                   
         DC    AL2(RE#KLEVNM),AL4(ACCNAME) K-ACCOUNT LEVEL'S NAMES              
         DC    AL2(RE#LEVBCD),AL4(LEVBCD)  K-LEVEL B CODE                       
         DC    AL2(RE#LEVCCD),AL4(LEVCCD)  K-LEVEL C CODE                       
         DC    AL2(RE#LEVDS),AL4(LEVDS)    K-LEVEL D DESCRIPTION                
         DC    AL2(RE#LEVDCD),AL4(LEVDCD)  D-LEVEL D CODES LIST                 
         DC    AL2(RE#NAME),AL4(LEVDCDNM)  D-NAME OF ACCOUNTS                   
         DC    AL2(RE#JOBCD),AL4(JOBCDFLT) JOB CODE TSAR LEVEL                  
         DC    AL2(RE#UNITS),AL4(GETUNT)   D-UNITS                              
         DC    AL2(RE#PRCNT),AL4(GETPCNT)  D-PERCENTAGE                         
*                                                                               
         DC    AL2(RE#LSTDATA),AL4(LSTDATA) UNIT= / LEDGER=                     
*                                                                               
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL33    CSECT                                                                  
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
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  DS     0H                                                              
         LH    R1,LSLST#1                                                       
         CHI   R1,0                                                             
         BH    DTALASX                                                          
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$NOREC)                                           
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
DTALASX  B     EXITOK              NO INPUT                                     
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
         TM    FVIIND,FVIALF       IS IT A CHAR BETWEEN A-Z                     
         BO    VALACD10                                                         
         MVC   FVXTRA,BCSPACES                                                  
         B     EXITNV                                                           
*                                                                               
VALACD10 MVI   ACTKUNT,C'3'      PUT IN UNIT 3                                  
         MVC   ACTKLDG,FVIFLD    PUT IN LDGR/ADVRTSR CODE IN REAL KEY           
         MVC   SVANAME,BCSPACES                                                 
*                                                                               
* READ RECORD WITH LEDGER/ADVRTSR CODE FROM THE SCREEN                          
*                                                                               
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,FVIFLD    PUT IN LDGR/ADVRTSR CODE IN KEY           
         MVC   SVLDG,FVIFLD           SAVE OFF ADVRTSR/LDGR CODE                
         DROP  TEMP                                                             
*                                                                               
         GOTO1 AGETACT,0                                                        
*        MVC   FVXTRA,BCSPACES                                                  
         BNE   EXITL                                                            
*                                                                               
         MVI   TYPE,0                                                           
*                                                                               
         GOTO1 AGETEL,BOPARM,('LDGELQ',AIO1),0   GET X'14' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING LDGELD,R3                                                        
         LA    R3,BOELEM                                                        
         TM    LDGSTAT,LDGRUPCT    X'01' ARE RETAIL UNITS PERCENTS              
         BNO   *+8                                                              
         MVI   TYPE,C'P'           TYPE IS 100%                                 
*                                                                               
         GOTO1 AGETEL,BOPARM,('ACLELQ',AIO1),0   GET X'16' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(GETLEVS),BOPARM,RR=BORELO    GET LEVEL LENGTHS                
         GOTO1 =A(ADDSTARS),BOPARM,RR=BORELO   ADD STAR RECS                    
         B     EXITOK                                                           
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
         BO    VFLTAC10                                                         
         TM    FVIIND,FVIALF       IS IT A CHAR BETWEEN A-Z                     
         BO    VFLTAC10                                                         
         MVC   FVXTRA,BCSPACES                                                  
         B     EXITNV                                                           
*                                                                               
VFLTAC10 MVI   ACTKUNT,C'3'      PUT IN UNIT 3                                  
         MVC   ACTKLDG,FVIFLD      MOVE WHAT USER TYPED IN KEY                  
         MVC   SVLDG,FVIFLD        SAVE OFF LEDGER/ADVERTISER CODE              
         ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLTIFLD(0),FVIFLD                                                
*                                                                               
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,FVIFLD    PUT IN LDGR/ADVRTSR CODE IN KEY           
         MVC   SVLDG,FVIFLD           SAVE OFF ADVRTSR/LDGR CODE                
         DROP  TEMP                                                             
*                                                                               
         GOTO1 AGETACT,0                                                        
*        MVC   FVXTRA,BCSPACES                                                  
         BNE   EXITL                                                            
*                                                                               
         MVI   TYPE,0                                                           
*                                                                               
         GOTO1 AGETEL,BOPARM,('LDGELQ',AIO1),0   GET X'14' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         USING LDGELD,R3                                                        
         LA    R3,BOELEM                                                        
         TM    LDGSTAT,LDGRUPCT    X'01' ARE RETAIL UNITS PERCENTS              
         BNO   *+8                                                              
         MVI   TYPE,C'P'           TYPE IS 100%                                 
*                                                                               
         GOTO1 AGETEL,BOPARM,('ACLELQ',AIO1),0   GET X'16' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 =A(GETLEVS),BOPARM,RR=BORELO    GET LEVEL LENGTHS                
*        GOTO1 =A(ADDSTARS),BOPARM,RR=BORELO   ADD STAR RECS                    
         USING NAMELD,R3                                                        
         LA    R3,BOELEM                                                        
         GOTO1 AGETEL,BOPARM,('NAMELQ',AIO1),0   GET X'20' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVANAME,NAMEREC     SAVE ADVERTISER NAME                         
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
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
ADNAME   LA    RF,ANAMTBL                                                       
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
         CLC   SVANAME,BCSPACES                                                 
         BE    *+14                                                             
         MVC   FVIFLD(L'SVANAME),SVANAME                                        
         B     EXITOK                                                           
         L     R1,AIO1                                                          
         GOTO1 AGETNAM                                                          
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
*************** *******************************************************         
* DISPLAY SCHEME CODE                                                           
***********************************************************************         
*                                                                               
         USING WCORECD,R5          DSECT TO COVER MEDIA INTERFACE ELEM          
DISSCDE  DS    0H                                                               
         USING TLSTD,R3        IF SEL FROM LIST SCHEME CODE IS IN TSAR          
         L     R3,ATLST                                                         
         OC    TLSCODE,TLSCODE                                                  
         BZ    DISSCD10                                                         
         MVC   FVIFLD(L'TLSCODE),TLSCODE                                        
         MVC   SVSCHMCD,TLSCODE   SAVE SCHEME CODE                              
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
         MVC   SVSCHMCD,FVIFLD     SAVE SCHEME/WORK CODE HERE                   
VALSCD10 DS    0H                                                               
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
         DROP  R3                                                               
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
DOFTSCCD DS    0H                                                               
         L     R3,ATLST                                                         
         OC    LISTSCCD,LISTSCCD                                                
         BZ    FLTXX                                                            
         CLC   LISTSCCD,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
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
T        USING CHDRECD,R2                                                       
         LA    R1,FVIFLD                                                        
         ST    R1,BOFULL1          SAVE ADDRESS                                 
*                                                                               
         CLC   T.CHDKCULC,BCSPACES                                              
         BE    DISFLT30                                                         
         MVC   0(4,R1),=CL4'JOB='  MOVE JOB IN FVIFLD                           
         LA    R1,4(R1)            BUMP PAST JOB=                               
*                                                                               
         LA    RE,L'CHDKCACT       LENGTH OF ACCOUNT                            
         LA    R3,T.CHDKCACT                                                    
         LA    R3,L'CHDKCACT-1(R3) START FROM END OF ACCOUNT                    
DISFLT10 CLI   0(R3),C' '          IS IT A SPACE                                
         BNE   DISFLT20                                                         
         BCTR  R3,0                BUMP BACKWARDS IN KEY                        
         BCT   RE,DISFLT10                                                      
*                                                                               
DISFLT20 BCTR  RE,0                SUBTRACT 1 FOR EX INSTR                      
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),T.CHDKCACT  JOB TO FVIFLD                                
         AR    R1,RE                                                            
         AHI   R1,1                ADD 1 WHICH WAS SUB FOR EX MVC               
         ST    R1,BOFULL1          SAVE DISP OF FVIFLD                          
         DROP  T                                                                
DISFLT30 DS    0H                                                               
         LA    R5,LEVLDSPS         GET DISPLACEMENT                             
         LA    R6,LEVLNQS          GET INDIVIDUAL LENGTH                        
*                                                                               
         ZIC   R1,LEVNUM           TOTAL NUMBER OF LEVELS                       
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    DISFLT70                                                         
*                                                                               
DISFLT40 LA    R5,1(R5)            GET CORRECT DISP                             
         LA    R6,1(R6)            GET CORRECT LENGTH                           
         SR    RE,RE                                                            
         IC    RE,0(R6)                                                         
         BCT   R1,DISFLT40                                                      
*                                                                               
         LA    R3,L'ACTKCULA-1(R2)   BUMP C/U/L + DISP IN KEY                   
DISFLT60 CLI   0(R3),C' '          IS IT A SPACE                                
         BNE   DISFLT70                                                         
         BCTR  R3,0                                                             
         BCT   RE,DISFLT60         LOWEST LEVEL LENGTH                          
         B     DISFLTX                                                          
*                                                                               
DISFLT70 ZIC   R1,0(R5)            GET DISPLACEMENT                             
         LA    RF,3(R1,R2)         BUMP C/U/L + DISP IN KEY                     
         ZIC   RE,0(R6)            GET LENGTH                                   
*                                                                               
         L     R1,BOFULL1                                                       
         CLI   FVIFLD,C' '         SPACE MEANS NO JOB=                          
         BE    DISFLT80                                                         
         MVI   0(R1),C','                                                       
         LA    R1,1(R1)                                                         
DISFLT80 DS    0H                                                               
         MVC   0(6,R1),=CL6'START='                                             
         LA    R1,6(R1)                                                         
         EX    RE,*+8                                                           
         B     *+10                                                             
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
         GOTO1 =A(GETTOT),BOPARM,RR=BORELO   CALCULATE TOTAL                    
*                                                                               
         GOTO1 =A(ADDSTARS),BOPARM,RR=BORELO   ADD STAR RECS                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING LEVEL A'S CODE AND DYNAMIC TAGS          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LEVACD   LA    RF,LEVATBL                                                       
         B     ITER                                                             
*                                                                               
LEVATBL  DS    0H                                                               
         DC    AL1(DMHED),AL1(0,0,0),AL4(DISLVATG)                              
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISLVACD)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLVACD)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* BUILD TAGS FOR LEVEL A                                              *         
***********************************************************************         
*                                                                               
DISLVATG DS    0H                                                               
         CLI   LEVNUM,1                                                         
         BNH   EXITOK                                                           
*                                                                               
         L     R5,SVPARMS6                                                      
         LA    R1,LEVELS         POINT WHERE DESCRIPTION IS                     
         AHI   R1,1                POINT TO DESCRIPTION NOT LEN                 
         MVC   0(L'LEVADSC,R5),0(R1)    LEVEL DESCRIPTION                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY LEVEL A CODE                                                          
***********************************************************************         
*                                                                               
DISLVACD DS    0H                                                               
         CLI   CSACT,A#LST                                                      
         BNE   DISLVA10                                                         
         MVC   FVIFLD(L'ACTKACT),ACTKACT                                        
         B     EXITOK                                                           
*                                                                               
DISLVA10 DS    0H                                                               
         GOTO1 =A(GETTOT),BOPARM,RR=BORELO   CALCULATE TOTAL                    
         ZIC   R1,LEVLNQA          GET LEVEL A'S LENGTH                         
         BCTR  R1,0                MINUS 1 FOR EX INST                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),ACTKACT                                                
         CLI   LEVNUM,1                                                         
         BNH   EXITOK                                                           
*                                                                               
         MVC   ALVLAFLD,FVADDR                                                  
         NI    FVATRB,X'FF'-FVAPROT   UNPROTECT FIELD                           
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE LEVEL/ACCOUNT  A CODE  TYPED  IN BY USER                             
***********************************************************************         
*                                                                               
VALLVACD DS    0H                                                               
         CLI   LEVNUM,1                                                         
         BH    VALA10                                                           
         MVC   FVIFLD,BCSPACES     NEW ADVERTISER ENTERED                       
         OI    FVATRB,FVAPROT      PROTECT FIELD                                
         B     EXITOK                                                           
*                                                                               
VALA10   OI    LVLBYTE,LVLADN      PROCESSING LEVEL A                           
         LA    RF,IOKEY                                                         
         MVC   IOKEY,BCSPACES                                                   
TEMP     USING ACTRECD,RF                                                       
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,SVLDG     PUT IN LDGR/ADVRTSR CODE IN KEY           
         DROP  TEMP                                                             
*                                                                               
* READ RECORD WITH LEVL/ACC CODE FROM THE SCREEN AND VALIDATE                   
*                                                                               
         LA    R1,LEVLDSPS       POINT TO DISPLACEMENTS                         
         LA    R5,LEVLNQS        MAXIMUM ACCOUNT LENGTHS                        
*                                                                               
         CLC   FVILEN,0(R5)        VALIDATE LENGTH OF ACCOUNT                   
         BH    EXITLONG                                                         
         BL    EXITSHRT                                                         
         ZIC   R5,0(R1)            GET DISPLACEMENT                             
         ZIC   R1,FVILEN           GET LENGTH OF INPUT                          
         LA    R3,ACTKACT(R5)      START MOVING FVIFLD VALUE HERE               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD                                                   
*                                                                               
         LA    R3,IOKEY+3(R5)      START MOVING FVIFLD VALUE IN KEY             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD                                                   
*                                                                               
         MVC   LOKEY,IOKEY                                                      
         GOTO1 AGETACT,0                                                        
         BNE   EXITRNF                                                          
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING LEVEL B'S CODE AND DYNAMIC TAGS          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LEVBCD   LA    RF,LEVBTBL                                                       
         B     ITER                                                             
*                                                                               
LEVBTBL  DS    0H                                                               
         DC    AL1(DMHED),AL1(0,0,0),AL4(DISLVBTG)                              
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISLVBCD)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLVBCD)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* BUILD TAGS FOR LEVEL B                                              *         
***********************************************************************         
*                                                                               
DISLVBTG DS    0H                                                               
         CLI   LEVNUM,2                                                         
         BNH   EXITOK                                                           
*                                                                               
         L     R5,SVPARMS6                                                      
         LA    R1,LEVELS         POINT WHERE DESCRIPTION IS                     
*                                                                               
         AHI   R1,L'LEVELS                                                      
         AHI   R1,1                POINT TO DESCRIPTION NOT LEN                 
         MVC   0(L'LEVADSC,R5),0(R1)    LEVEL DESCRIPTION                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY LEVEL B CODE                                                          
***********************************************************************         
*                                                                               
DISLVBCD DS    0H                                                               
         CLI   LEVNUM,2                                                         
         BNH   EXITOK                                                           
*                                                                               
         NI    FVATRB,X'FF'-FVAPROT                                             
*                                                                               
         LA    RE,ACTKACT                                                       
         ZIC   R3,LEVA            GET LENGTH OF LEVA TO BUMP                    
         AR    RE,R3              POINT TO START OF LEVEL B                     
         ZIC   R1,LEVLNQB         GET IND LENGTH OF LEVL B                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),0(RE)    FROM KEY TO FIELD                             
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE LEVEL/ACCOUNT  B CODE  TYPED  IN BY USER                             
***********************************************************************         
*                                                                               
VALLVBCD DS    0H                                                               
*                                                                               
         CLI   LEVNUM,2                                                         
         BH    VALB10                                                           
         MVC   FVIFLD,BCSPACES     NEW ADVERTISER ENTERED                       
         OI    FVATRB,FVAPROT      PROTECT FIELD                                
         B     EXITOK                                                           
*                                                                               
VALB10   OI    LVLBYTE,LVLBDN      PROCESSING LEVEL B                           
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,SVLDG     PUT IN LDGR/ADVRTSR CODE IN KEY           
         DROP  TEMP                                                             
*                                                                               
* READ RECORD WITH LEVL/ACC CODE FROM THE SCREEN AND VALIDATE                   
*                                                                               
         LA    R1,LEVLDSPS       POINT TO DISPLACEMENTS                         
         LA    R5,LEVLNQS        MAXIMUM ACCOUNT LENGTHS                        
*                                                                               
         LA    R1,1(R1)            GET CORRECT DISPLACEMENT                     
         LA    R5,1(R5)            GET CORRECT MAX ACC LENGTH                   
         DS    0H                  POINT TO DESCRIPTION NOT LEN                 
         CLC   FVILEN,0(R5)        VALIDATE LENGTH OF ACCOUNT                   
         BH    EXITLONG                                                         
         BL    EXITSHRT                                                         
         ZIC   R5,0(R1)            GET DISPLACEMENT                             
         ZIC   R1,FVILEN           GET LENGTH OF INPUT                          
         LA    R3,ACTKACT(R5)      START MOVING FVIFLD VALUE HERE               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD                                                   
*                                                                               
         LA    R3,IOKEY+3(R5)      START MOVING FVIFLD VALUE IN KEY             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD                                                   
*                                                                               
         MVC   LOKEY,IOKEY                                                      
         GOTO1 AGETACT,0                                                        
         BNE   EXITRNF                                                          
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING LEVEL C'S CODE AND DYNAMIC TAGS          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LEVCCD   LA    RF,LEVCTBL                                                       
         B     ITER                                                             
*                                                                               
LEVCTBL  DS    0H                                                               
         DC    AL1(DMHED),AL1(0,0,0),AL4(DISLVCTG)                              
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISLVCCD)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLVCCD)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* BUILD TAGS FOR LEVEL C                                              *         
***********************************************************************         
*                                                                               
DISLVCTG DS    0H                                                               
         CLI   LEVNUM,3                                                         
         BNH   EXITOK                                                           
*                                                                               
         L     R5,SVPARMS6                                                      
         LA    R1,LEVELS         POINT WHERE DESCRIPTION IS                     
*                                                                               
         AHI   R1,(2*L'LEVELS)+1                                                
         MVC   0(L'LEVCDSC,R5),0(R1)    LEVEL DESCRIPTION                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY LEVEL C CODE                                                          
***********************************************************************         
*                                                                               
DISLVCCD DS    0H                                                               
         CLI   LEVNUM,3                                                         
         BNH   DISLVC10                                                         
*                                                                               
         NI    FVATRB,X'FF'-FVAPROT                                             
DISLVC10 DS    0H                                                               
         LA    RE,ACTKACT                                                       
         ZIC   R3,LEVB            LENGTH OF LEVA + LEVB                         
         AR    RE,R3              POINT TO START OF LEVEL C                     
         ZIC   R1,LEVLNQC         GET IND LENGTH OF LEVL C                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),0(RE)    FROM KEY TO FIELD                             
DISLVCX  B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE LEVEL/ACCOUNT  C CODE  TYPED  IN BY USER                             
***********************************************************************         
*                                                                               
VALLVCCD DS    0H                                                               
         CLI   LEVNUM,3                                                         
         BH    VALC10                                                           
         MVC   FVIFLD,BCSPACES     NEW ADVERTISER ENTERED                       
         OI    FVATRB,FVAPROT      PROTECT FIELD                                
         B     EXITOK                                                           
*                                                                               
VALC10   OI    LVLBYTE,LVLCDN      PROCESSING LEVEL C                           
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,SVLDG     PUT IN LDGR/ADVRTSR CODE IN KEY           
         DROP  TEMP                                                             
*                                                                               
* READ RECORD WITH LEVL/ACC CODE FROM THE SCREEN AND VALIDATE                   
*                                                                               
         LA    R1,LEVLDSPS       POINT TO DISPLACEMENTS                         
         LA    R5,LEVLNQS        MAXIMUM ACCOUNT LENGTHS                        
*                                                                               
         LA    R1,2(R1)            GET CORRECT DISPLACEMENT                     
         LA    R5,2(R5)            GET CORRECT MAX ACC LENGTH                   
         CLC   FVILEN,0(R5)        VALIDATE LENGTH OF ACCOUNT                   
         BH    EXITLONG                                                         
         BL    EXITSHRT                                                         
         ZIC   R5,0(R1)            GET DISPLACEMENT                             
         ZIC   R1,FVILEN           GET LENGTH OF INPUT                          
         LA    R3,ACTKACT(R5)      START MOVING FVIFLD VALUE HERE               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD                                                   
*                                                                               
         LA    R3,IOKEY+3(R5)      START MOVING FVIFLD VALUE IN KEY             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),FVIFLD                                                   
*                                                                               
         MVC   LOKEY,IOKEY                                                      
         GOTO1 AGETACT,0                                                        
         BNE   EXITRNF                                                          
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING ACCOUNT LEVEL NAMES                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
ACCNAME  LA    RF,ACNAMTBL                                                      
         B     ITER                                                             
*                                                                               
ACNAMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISACNAM)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY ACCOUNT NAME                                                *         
***********************************************************************         
*                                                                               
DISACNAM DS    0H                                                               
*                                                                               
         CLI   CSACT,A#LST                                                      
         BNE   DISNM30                                                          
*                                                                               
         L     R6,AIOREC                                                        
DISNM01  DS    0H                 PULL JOB CODE TO PRINT IF AVLBL               
T        USING CHDRECD,R6                                                       
         CLI   T.CHDKCULC,C' '   ARE WE DOING ACC WITH CNTRA/JOB IN IT          
         BE    DISNM20                                                          
         DROP  T                                                                
*                                                                               
         LA    R6,ACTRFST-ACTRECD(R6)                                           
         USING CACELD,R6                                                        
DISNM02  CLI   0(R6),0            GET X'43' ELEMENT                             
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLI   0(R6),CACELQ        IS IT X'43' ELEMENT                          
         BE    DISNM03                                                          
         ZIC   R1,1(R6)            GET NEXT ELEMENT                             
         AR    R6,R1                                                            
         B     DISNM02                                                          
*                                                                               
DISNM03  DS    0H                                                               
         ZIC   R1,1(R6)                                                         
         SHI   R1,CACLN1Q                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),CACNAME OVERWITES ACCOUNT CODE WITH JOB CODE           
*                                 IN LISTING OF ACCOUNT                         
         DROP  R6                                                               
         B     EXITOK                                                           
*                                                                               
DISNM20  L     R1,AIOREC                                                        
         GOTO1 AGETNAM                                                          
         B     EXITOK                                                           
*                                                                               
DISNM30  TM    LVLBYTE,LVLADN      DO WE HAVE A NAME TO PRINT IN LVL A          
         BO    DISNM40                                                          
         TM    LVLBYTE,LVLBDN      DO WE HAVE A NAME TO PRINT IN LVL B          
         BO    DISNM40                                                          
         TM    LVLBYTE,LVLCDN      DO WE HAVE A NAME TO PRINT IN LVL C          
         BNO   DISNM50                                                          
DISNM40  L     R1,AIO1                                                          
         GOTO1 AGETNAM                                                          
DISNM50  NI    LVLBYTE,X'FF'-(LVLADN+LVLBDN+LVLCDN)                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING LEVEL  D'S (LOWEST LEVEL TAG)            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LEVDS    LA    RF,LEVDTBL                                                       
         B     ITER                                                             
*                                                                               
LEVDTBL  DS    0H                                                               
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISLVDTG)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* BUILD TAGS FOR LEVEL D                                              *         
***********************************************************************         
*                                                                               
DISLVDTG DS    0H                                                               
         CLI   LEVNUM,0                                                         
         BE    EXITOK                                                           
         CLI   LEVNUM,1                                                         
         BNE   DISLDTG1            IF ONLY ONE LEVEL THAN INIT LOKEY            
         LA    RF,IOKEY                                                         
         MVC   IOKEY,BCSPACES                                                   
TEMP     USING ACTRECD,RF                                                       
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,SVLDG     PUT IN LDGR/ADVRTSR CODE IN KEY           
         DROP  TEMP                                                             
         MVC   LOKEY,IOKEY                                                      
*        L     R5,SVPARMS6                                                      
DISLDTG1 LA    R1,LEVELS         POINT WHERE DESCRIPTION IS                     
         ZIC   R3,LEVNUM         HOW MANY TIMES BEEN HERE                       
         BCTR  R3,0              DONT BUMP PAST CORRECT ENTRY                   
         LTR   R3,R3                                                            
         BZ    DISLDTG2                                                         
*                                                                               
         AHI   R1,L'LEVELS                                                      
         BCT   R3,*-4                                                           
DISLDTG2 AHI   R1,1                POINT TO DESCRIPTION NOT LEN                 
         MVC   FVIFLD(L'LEVDDSC),0(R1)    LEVEL D TAG                           
         B     EXITOK       LAST KEY FIELD VALIDATED GO TO KLASVAL              
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEVEL D CODES ALL LOWER LEVEL ACCOUNTS              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LEVDCD   LA    RF,LEVDCDTB                                                      
         B     ITER                                                             
*                                                                               
LEVDCDTB DC    AL1(DDIS),AL1(0,0,0),AL4(DISLVDCD)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY LEVEL D (LOWEST LEVEL) ACCOUNT CODES                                  
***********************************************************************         
*                                                                               
DISLVDCD DS    0H                                                               
*                                                                               
         USING TLSTD,R3                                                         
         L     R3,ATLST                                                         
DISLVD10 MVC   FVIFLD(L'ACTKACT),TLSKEY                                         
         B     EXITOK                                                           
         DROP  R3                                                               
***********************************************************************         
* DATA OBJECT FOR DISPLAYING LEVEL D ACCOUNT NAMES                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LEVDCDNM LA    RF,DCDNMTBL                                                      
         B     ITER                                                             
*                                                                               
DCDNMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISDCDNM)                               
         DC    AL1(DDISBOT),AL1(0,0,0),AL4(DISMSG)                              
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY LEVEL D ACCOUNT NAME                                        *         
***********************************************************************         
         USING TLSTD,R3                                                         
*                                                                               
DISDCDNM DS    0H                                                               
*                                                                               
         L     R3,ATLST                                                         
         MVC   FVIFLD(L'TLSNAME),TLSNAME                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TOTALS MESSAGE                                              *         
***********************************************************************         
*                                                                               
DISMSG   DS    0H                                                               
         OC    SVTOTAL,SVTOTAL                                                  
         BE    DISMSGX                                                          
         CP    SVTOTAL,=P'0'                                                    
         BE    DISMSGX                                                          
         CLI   TYPE,C'P'                                                        
         BNE   DISMSG10                                                         
         CP    SVTOTAL,=P'1000000'  IS THE TOTAL 100%                           
         BNE   DISMSG20                                                         
DISMSG10 MVC   FVIFLD(L'AC@SCMSG),AC@SCMSG                                      
         MVC   FVIFLD+L'AC@SCMSG+2(L'SVSCHMCD),SVSCHMCD                         
         B     DISMSGX                                                          
DISMSG20 MVC   FVIFLD(L'AC@WARN),AC@WARN                                        
DISMSGX  B     EXITOK                                                           
*        DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR JOB CODE FILTERS                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
JOBCDFLT LA    RF,JOBFTBL                                                       
         B     ITER                                                             
*                                                                               
JOBFTBL  DS    0H                                                               
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISFJOB)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTJOB)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTJOB)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTJOB)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY JOB CODE TSAR LEVEL                                                   
***********************************************************************         
*                                                                               
DISFJOB  DS    0H                                                               
T        USING CHDRECD,R2                                                       
         MVC   FVIFLD(L'CHDKCACT),T.CHDKCACT                                    
         DROP  T                                                                
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DISPLAY A JOB CODE FILTER FIELD                                     *         
***********************************************************************         
*                                                                               
DFLTJOB  MVC   FVIFLD(L'CHDKCACT),FLTIFLD                                       
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE JOB CODE FILTER FIELD                                      *         
***********************************************************************         
*                                                                               
VFLTJOB  DS    0H                                                               
         MVC   FLTIFLD(L'CHDKCACT),FVIFLD                                       
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DO FILTERING ON JOB CODE                                                      
***********************************************************************         
*                                                                               
DOFTJOB  DS    0H                                                               
T        USING CHDRECD,R2                                                       
         OC    T.CHDKCACT,T.CHDKCACT                                            
         BZ    FLTXX                                                            
         CLC   T.CHDKCACT,FLTIFLD                                               
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
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
         DC    AL1(DDISBOT),AL1(0,0,0),AL4(DISTOT)                              
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY NUMBER OF UNITS                                                       
***********************************************************************         
DISUNIT  DS    0H                                                               
         USING TLSTD,R3                                                         
         L     R3,ATLST            TSAR RECORDS IN ATLST                        
         CLI   CSACT,A#LST         IS IT A LIST ACTION                          
         BNE   DISUN10                                                          
         MVC   FVIFLD(12),LISTUNTS                                              
         B     DISUNITX                                                         
DISUN10  OC    TLPUNIT,TLPUNIT                                                  
         BZ    DISUNITX                                                         
         CLI   TYPE,C'P'                                                        
         BE    DISUN20                                                          
         EDIT  TLPUNIT,(12,FVIFLD),2,WRK=BOWORK1,DUB=BODUB1,ALIGN=LEFT          
         B     DISUNITX                                                         
*                                                                               
DISUN20  EDIT  TLPUNIT,(12,FVIFLD),4,WRK=BOWORK1,DUB=BODUB1,           +        
               ALIGN=LEFT,DROP=2                                                
*                                                                               
DISUNITX B     EXITOK                                                           
         DROP  R3                                                               
***********************************************************************         
* DISPLAY TOTALS                                                      *         
***********************************************************************         
*                                                                               
DISTOT   DS    0H                                                               
         CP    SVTOTAL,=P'0'                                                    
         BE    DISTOTX                                                          
         CLI   TYPE,C'P'                                                        
         BE    DISTOT10                                                         
         EDIT  SVTOTAL,(12,FVIFLD),2,WRK=BOWORK1,DUB=BODUB1,ALIGN=LEFT          
         B     DISTOTX                                                          
DISTOT10 EDIT  SVTOTAL,(12,FVIFLD),4,WRK=BOWORK1,DUB=BODUB1,           +        
               ALIGN=LEFT,DROP=2                                                
DISTOTX  XC    IOKEYLST,IOKEYLST                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE NUMBER OF UNITS TYPED IN BY USER                                     
***********************************************************************         
         USING TLSTD,R3                                                         
*                                                                               
VALUNIT  DS    0H                                                               
*                                                                               
         L     R3,ATLST                                                         
*                                                                               
         OC    TLPUNIT,TLPUNIT                                                  
         BZ    VALUX                                                            
         SP    SVTOTAL,TLPUNIT                                                  
         ZAP   TLPUNIT,=P'0'       UNITS CAN ONLY BE PASSED VIA TSAR            
         CLI   FVILEN,0                                                         
         BE    VALUX               ANYTHING IN UNIT                             
         ZIC   R0,FVILEN           NO, NOTHING TO VALIDATE                      
         LA    RF,2                                                             
         CLI   TYPE,C'P'           FOR 100% MUST BE 4 DEC POINTS                
         BNE   *+8                                                              
         LA    RF,4                                                             
         CLI   FVIFLD,C'-'                                                      
         BE    EXITNV                                                           
*                                                                               
         GOTO1 VCASHVAL,BOPARM,((RF),FVIFLD),(R0)                               
         CLI   BOPARM,0                                                         
         BNE   EXITNV                                                           
         L     R1,BOPARM+4                                                      
         CVD   R1,BODUB1                                                        
         ZAP   TLPUNIT,BODUB1      UNITS CAN ONLY BE PASSED VIA TSAR            
         AP    SVTOTAL,TLPUNIT                                                  
         OI    LSSCIND2,LSSCIPAG   REDISPLAY CURRENT PAGE                       
VALUX    B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO UPDATE UNITS  FOR RECORD READ BY CONTROLLER           *         
***********************************************************************         
UPDTHIGH NTR1                                                                   
         LA    R6,IOKEY                                                         
         L     R3,AIOREC                                                        
         BRAS  RE,GETSCUN                                                       
         BNE   UPDTH10             X'62' ELEMENT NOT FOUND ADD ONE              
         USING DSCELD,R3                                                        
         AP    DSCVAL,ADJAMT                                                    
         CP    DSCVAL,=P'0'                                                     
         BE    UPDTH20                                                          
         B     UPDTHX                                                           
*                                                                               
UPDTH10  DS    0H                                                               
         LA    R3,BOELEM                                                        
         MVI   DSCEL,DSCELQ        MOVE BUILD X'62' ELEMENT                     
         MVI   DSCLN,X'0A'                                                      
         MVC   DSCCODE,SVSCHMCD                                                 
         ZAP   DSCVAL,UNTAMT                                                    
         ZAP   DSCVAL,ADJAMT                                                    
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIOREC,BOELEM,0                    
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
         B     UPDTHX                                                           
UPDTH20  DS    0H                                                               
         MVI   DSCEL,X'FF'         NEW UNIT IS ZERO DELETE X'62'                
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',AIOREC),0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
UPDTHX   XIT1                                                                   
         DROP  R3                                                               
         EJECT                                                                  
*********************************************************************           
* SUBROUTINE UPDATES STAR RECORDS AND ALL HIGH TO LOW LEVEL ACCOUNTS*           
* AT KEY LEVEL                                                      *           
*********************************************************************           
UPDRECD  NTR1                                                                   
         CP    ADJAMT,=P'0'        NET ADJUSTMENT IS ZERO                       
         BE    UPDRECX             NO NEED TO UPDATE                            
         MVI   ADJCODE,C'L'        SET ADJUSTMENT CODE TO LOW  LEVEL            
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(15),LOKEY                                                  
         SR    R0,R0                                                            
         IC    R0,LEVNUM           NUMBER OF LEVELS                             
         LR    R5,R0               NUMBER OF LEVELS                             
         SHI   R0,2                                                             
         BM    UPDREC30            ONLY ONE LEVEL                               
*                                                                               
         AHI   R0,1                R0=1 LESS NUMBER OF LEVELS                   
*                                                                               
         CLI   JOBKEY,0         ARE WE UPDATING CONTRA ACCOUNT                  
         BE    UPDREC10         NO                                              
         BCTR  R0,0             START UPDATING FROM 2ND TO LAST LEVEL           
         BCTR  R5,0                                                             
*                               FIX LOW LEVEL ACCOUNTS                          
UPDREC10 DS    0H                                                               
         CLI   JOBKEY,0         ARE WE UPDATING HIGHEST LEVEL WITH JO           
         BNE   UPDREC20         YES                                             
         CLC   IOKEY(15),LOKEY                                                  
         BNE   UPDREC20                                                         
         BAS   RE,UPDTHIGH         UPDATE ACCOUNT READ BY CONTROLLER            
         BCTR  R0,0                                                             
         BCTR  R5,0                                                             
         B     UPDREC25                                                         
UPDREC20 GOTO1 =A(UPDT),BOPARM,RR=BORELO   LOOK FOR SCHEME CODE                 
UPDREC25 LR    R1,R0                                                            
         LA    RE,LEVLNQS(R1)      GET LOWEST LEVEL INDIVIDUAL LEN              
         LA    R6,LEVLDSPS(R1)     GET LOWEST LEVEL'S DISPLMNT                  
         ZIC   R1,0(R6)            GET DISPLCEMENT IN KEY                       
         LA    R3,IOKEY+3(R1)                                                   
         ZIC   R1,0(RE)            GET INDIVIDUAL LENGTH OF ACC                 
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),BCSPACES    MOVE SPACES AT LOWER LEVEL                   
         BCTR  R0,0                                                             
         BCT   R5,UPDREC10                                                      
*                                  NOW FIX THE * ACCOUNTS                       
UPDREC30 DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(3),LOKEY                                                   
         ZIC   R0,LEVNUM                                                        
         LA    R5,LEVLNQS                                                       
         LA    R6,LEVLDSPS                                                      
UPDREC40 DS    0H                                                               
         ZIC   R1,0(R6)            GET DISPLACEMENT IN KEY                      
         LA    R3,IOKEY+3(R1)                                                   
         ZIC   R1,0(R5)            GET INDIVIDUAL LENGTH                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=12C'*'                                                  
         GOTO1 =A(UPDT),BOPARM,RR=BORELO   LOOK FOR SCHEME CODE                 
         LA    R5,1(R5)                                                         
         LA    R6,1(R6)                                                         
         BCT   R0,UPDREC40                                                      
UPDRECX  ZAP   ADJAMT,=P'0'                                                     
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AND VALIDATING PERCENTAGE                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
GETPCNT  LA    RF,PCNTTBL                                                       
         B     ITER                                                             
*                                                                               
PCNTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPCNT)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY PERCENT                                                               
***********************************************************************         
*                                                                               
DISPCNT  DS    0H                                                               
*                                                                               
         GOTO1 =A(PCNT),BOPARM,RR=BORELO   CALCULATE PERCENTAGE                 
         B     EXITOK                                                           
         EJECT                                                                  
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
         EX    R1,*+8                                                           
         B     *+10                                                             
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
         LA    R5,LEVLDSPS         GET DISPLACEMENT                             
         LA    R6,LEVLNQS          GET INDIVIDUAL LENGTH                        
*                                                                               
         ZIC   R1,LEVNUM                                                        
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    VALACC10                                                         
*                                                                               
         LA    R5,1(R5)            GET CORRECT DISP                             
         LA    R6,1(R6)            GET CORRECT LENGTH                           
         BCT   R1,*-8                                                           
*                                                                               
VALACC10 DS    0H                                                               
         SR    R1,R1                                                            
         CLC   1(1,R2),0(R6)       CHECK LENGTH OF INPUT VS MAX.                
         BH    VALACCX                                                          
         IC    R1,0(R5)            DISPLACEMENT TO LOW ACCOUNT                  
         LA    R5,3(R3,R1)                                                      
         IC    R1,1(R2)            LENGTH OF INPUT ACCOUNT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R5),22(R2)      ACCOUNT CODE TO START KEY                    
         MVI   ERROR,OK                                                         
         XIT1                                                                   
VALACCX  MVI   ERROR,NV                                                         
         B     EXITINSA                                                         
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE TO VALIDATE JOB OPTION ON FILTER FIELD                  *          
**********************************************************************          
*                                                                               
VALJOB   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVC   0(15,R3),BCSPACES   JOB FILTER KEY                               
         MVC   0(1,R3),CUABIN                                                   
         MVC   1(2,R3),=C'SJ'      MOVE IN UNIT LEDGER                          
         IC    R1,1(R2)            LENGTH OF INPUT ACCOUNT                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   3(0,R3),22(R2)      CLI/PROD/CODE TO KEY                         
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(15),0(R3)                                                  
*                                                                               
         GOTO1 =A(VALOFF),BOPARM,RR=BORELO,JOBKEY                               
         BNE   EXITL                                                            
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(15),0(R3)            RESET JOB LEVEL KEY                   
         GOTO1 AGETACT,0                                                        
         BNE   VALJOBX                                                          
*        CLC   IOKEY(15),IOKEYSAV                                               
*        BNE   VALJOBX                                                          
*                                                                               
         LA    R5,CNTRJOB          BUILD CONTRA ELEMENT                         
         USING CACELD,R5                                                        
         MVI   CACEL,CACELQ        X'43'                                        
         MVC   CACCNT,IOKEY        JOB CODE                                     
*                                                                               
         GOTO1 AGETEL,BOPARM,('NAMELQ',AIO1),0   GET X'20' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,BOELEM           GET JOB NAME                                 
         USING NAMELD,R3                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN            LENGTH OF 20 ELEMENT                         
         LA    R1,15(R1)           PLUS 15 FOR ACCOUNT                          
         STC   R1,CACLN            GIVES LENGTH OF 43 ELEMENT                   
         SHI   R1,18                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   CACNAME(0),NAMEREC  NAME TO CONTRA                               
         MVI   ERROR,OK                                                         
         MVC   IOKEY,BCSPACES                                                   
         XIT1                                                                   
VALJOBX  MVI   ERROR,NV                                                         
         MVC   IOKEY,BCSPACES                                                   
         B     EXITINJC            DISPLAY ERROR INVALID JOB CODE               
         DROP  R3,R5                                                            
         EJECT                                                                  
*********************************************************************           
* SUBROUTINE TO VALIDATE FILTERS F1,F2,F3 FROM FILTER FIELD         *           
*********************************************************************           
*                                                                               
VALFILT  NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   ERROR,OK                                                         
         MVC   0(1,R3),22(R2)      FILTER VALUE TO SAVE AREA                    
         CLI   1(R2),1                                                          
         BE    EXITOK                                                           
         CLI   22(R2),C'*'         IF 2 BYTES MUST START WITH *                 
         BE    *+12                                                             
         MVI   ERROR,NV                                                         
         B     EXITNV                                                           
         MVC   0(1,R3),23(R2)                                                   
         NI    0(R3),X'FF'-X'40'   ALL EXCEPT                                   
         MVI   ERROR,OK                                                         
         B     EXITOK                                                           
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE TO VALIDATE UNITS IN FILTER FIELD UNITS CAN BE NEGATIVE *          
**********************************************************************          
*                                                                               
VALUNT   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         MVI   ERROR,NV                                                         
         MVC   0(1,R3),22(R2)     FIRST BYTE OF UNITS VALUE                     
         SR    RF,RF                                                            
         IC    RF,1(R2)           LENGTH OF UNITS INPUT                         
         LA    R5,22(R2)          R4 TO START OF UNITS VALUE                    
         CLI   0(R3),C'*'         IS IT A NEGATIVE FILTER                       
         BNE   *+12                                                             
         LA    R5,1(R5)            IGNORE * FOR CASHVAL                         
         SH    RF,=H'1'            ADJUST INPUT LENGTH                          
         BNP   VALUNTX                                                          
         GOTO1 VCASHVAL,BOPARM,(X'80',(R5)),(RF)                                
         CLI   BOPARM,X'FF'                                                     
         BE    VALUNTX                                                          
         LA    RF,FLTUN                                                         
         CLI   FLTU,C'*'           IS FILTER NEGATIVE                           
         BNE   *+8                                                              
         LA    RF,FLTUNEG                                                       
         ZAP   0(6,RF),BOPARM+4(8)                                              
         MVI   ERROR,OK                                                         
         B     EXITOK                                                           
VALUNTX  MVI   ERROR,X'FE'                                                      
         B     EXITINUF                                                         
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
         USING ACTRECD,R2                                                       
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
* NTRSES OBJECT                                                       *         
* -------------                                                       *         
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
NSSTABL  DS    0H                                                               
         DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(NTRIN)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   DS    0H                                                               
         MVC   SDATA(1),TYPE       SAVE PERCENT OR NOT                          
         MVC   SDATA+1(1),SVLDG    SAVE LEDGER                                  
         MVC   SDATA+2(LEVEND-LEVELS),LEVELS   MOVE LEVELS IN PARAM             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    DS    0H                                                               
         MVC   TYPE,SDATA          RESTORE TYPE                                 
         MVC   SVLDG,SDATA+1                                                    
         MVC   LEVELS(LEVEND-LEVELS),SDATA+2                                    
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
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(LSTLAST)                            
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
*                                                                               
FLST     MVC   0(0,R2),CUABIN                                                   
         MVC   IOKEY(L'ACTKEY),THIS.ACTRECD                                     
         L     R1,=AL4(XOHIGH+XOACCDIR+XIO1)                                    
         GOTO1 AIO                                                              
         BNE   EXITL               MESS UP ON THE READ HIGH                     
         OC    SVDISP,SVDISP                                                    
         BZ    NLST10                                                           
         OI    FLAG,FLGDISP        DISPLACEMENT IS AVAILABLE                    
         B     NLST10              FROM PREVIOUS PAGE LAST RECORD               
*                                                                               
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
*                                                                               
TEMP     USING ACTRECD,IOKEY                                                    
NLST     DS    0H                                                               
         NI    FLAG,FF-FLGDISP     TURN OFF DISPLACEMENT CHK FLAG               
         CLC   SVDISP,=F'0'                                                     
         BNE   NLST30                                                           
         LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
*                                                                               
NLST10   CLC   TEMP.ACTKCPY(L'ACTKCPY+L'ACTKUNT+L'ACTKLDG),THIS.ACTKCPY         
         BNE   EXITL               FOR ACCOUNT U/L IS REQUIRED & FIXED          
         CLC   TEMP.ACTKACT,BCSPACES                                            
         BNH   NLST                MUST HAVE AN ACCOUNT                         
*                                                                               
         ZIC   R1,LEVNUM                                                        
         BCTR  R1,0                TO GET CORRECT DISPLACEMENT                  
         LA    R1,LEVLDSPS(R1)     DISPLACEMENT IN KEY                          
         ZIC   R3,0(R1)                                                         
         LA    RF,IOKEY                                                         
         CLI   3(RF),C'*'          SKIP ALL STAR ACCOUNTS                       
         BE    NLST                                                             
         LA    RF,3(R3,RF)                                                      
         CLI   0(RF),C' '          IS LAST LEVEL OF THIS ACC SPACES             
         BE    NLST                WE ONLY WANT LAST LEVEL OF ACCOUNTS          
*                                                                               
         LA    RF,IOKEY                                                         
         CLI   CHDKCULC-CHDRECD(RF),X'40' IS ANYTHING IN JOB                    
         BNH   NLST20                                                           
         MVC   SVKEY,IOKEY                                                      
         GOTO1 =A(VALOFF),BOPARM,RR=BORELO,SVKEY+17                             
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY,SVKEY               RESET JOB LEVEL KEY                    
         LHI   R1,XOREAD+XOACCDIR+XIO1   RESET SEQUENCIAL READING               
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    FLAG,FLGOFF              IS OFF IN JOB OK FOR LIST               
         BO    NLST                     NO READ NEXT                            
*                                                                               
NLST20   L     R1,=AL4(XOGET+XOACCMST+XIO1)                                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD COMPANY RECORD                           
*                                                                               
NLST30   BAS   RE,GETSCHM     GET SCHMCODES AND UNITS                           
         CLC   SVDISP,=F'0'   ARE THERE ANY SCHEMES AND UNITS FOR THIS          
         BE    NLST                                                             
         GOTO1 ATSTSEC                                                          
         MVC   FVXTRA,BCSPACES                                                  
         BNE   NLST                                                             
*                                                                               
         MVC   THIS.ACTKEY(ACCKLEN),TEMP.ACTRECD                                
         B     EXITOK                                                           
         DROP  TEMP                                                             
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
INITL    DS    0H                                                               
*        OI    LSSTAT1,LSSBALL         BUILD LIST IN ONE GO                     
         MVI   FLAG,0                                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
*                                                                               
FTFLST   DS    0H                                                               
         XC    SVDISP,SVDISP                                                    
         B     EXITOK                                                           
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
TSARFIL  DS    0H                                                               
         L     R3,ATLST                                                         
         MVC   TLRLEN,=AL2(LISTLNQ)                                             
*                                                                               
         USING DSCELD,R5                                                        
         L     R5,SVDISP                                                        
         A     R5,AIOREC                                                        
         MVC   LISTSCCD,DSCCODE    MOVE SCHEME CODE TO TSAR                     
         CLI   TYPE,C'P'                                                        
         BNE   TSARFL10                                                         
         EDIT  DSCVAL,(12,LISTUNTS),4,WRK=BOWORK1,DUB=BODUB1,          +        
               DROP=2                                                           
         B     EXITOK                                                           
TSARFL10 DS    0H                                                               
         EDIT  DSCVAL,(12,LISTUNTS),2,WRK=BOWORK1,DUB=BODUB1                    
         B     EXITOK                                                           
         DROP  R3,R5                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION FOR MAIN LIST                                        *         
***********************************************************************         
         SPACE 1                                                                
INITL1   DS    0H                                                               
         OI    LSSTAT1,LSSBALL                                                  
         OI    LSSTAT2,LSSADD+LSSNOSEQ+LSS1HEAD                                 
         MVC   LSCOLLIN,=AL2(80)                                                
         MVC   LSLINROW,=AL2(1)                                                 
         MVC   LSTOTBOT,=H'1'                                                   
         GOTOX ('SAVFDR',AGROUTS),BOPARM,(C'C',12) CLEAR KEY FIELDS             
*                                                  IMDB#2109531                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR MAIN LIST                                            *         
***********************************************************************         
*                                                                               
FTFLST1  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST FOR MAIN LIST                                                 *         
***********************************************************************         
*                                                                               
FLST1    DS    0H                                                               
         L     R2,AIOREC                                                        
         LA    R5,LEVLDSPS         GET DISPLACEMENT IN KEY                      
         LA    R6,LEVLNQS          GET INDIVIDUAL LENGTH OF LEDGER              
         ZIC   R1,LEVNUM           HOW MANY LEVELS DOES LEDGER HAVE             
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    FMLST10             WE ARE AT CORRECT DISPLACEMENT               
         LA    R5,1(R5)            GET CORRECT DISPLACEMENT                     
         LA    R6,1(R6)            GET CORRECT LENGTH                           
         BCT   R1,*-8                                                           
*                                                                               
         MVC   IOKEY,LOKEY                                                      
         MVI   IOKEY+14,X'41'                                                   
FMLST10  CLI   STRKEY,0                                                         
         BE    FMLST20                                                          
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'STRKEY),STRKEY  START AT SPECIFIED ACCOUNT               
FMLST20  DS    0H                                                               
         MVC   DISPKEY,0(R5)       SAVE DISP OF LOWEST LEVEL IN KEY             
         MVC   INDLEN,0(R6)        INDIVIDUAL LENGTH                            
         L     R1,=AL4(XOHIGH+XOACCDIR+XIO1)                                    
         GOTO1 AIO                                                              
         B     NML20                                                            
***********************************************************************         
* NEXT FOR MAIN LIST                                                  *         
***********************************************************************         
*                                                                               
TEMP     USING ACTRECD,IOKEY                                                    
NLST1    DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'IOKEYLST),IOKEYLST  START READING FROM THIS KEY          
NML10    L     R1,=AL4(XOHIGH+XOACCDIR+XIO1)                                    
*ML10    LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         CLC   TEMP.ACTKCPY(L'ACTKCPY+L'ACTKUNT+L'ACTKLDG),IOKEYSAV             
         BNE   EXITL               FOR ACCOUNT U/L IS REQUIRED & FIXED          
NML20    DS    0H                                                               
         CLI   IOKEY+3,C'*'                                                     
         BE    NML30                                                            
*                                                                               
         L     R1,=AL4(XOGET+XOACCMST+XIO1)                                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD COMPANY RECORD                           
         ZIC   R1,DISPKEY          DISPLACEMENT TO ACCOUNT LEVEL                
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
         BAS   RE,FILTR            FILTER THE ACCOUNT                           
         BNE   NML30                                                            
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
         ZIC   R1,DISPKEY          GET DISPLACEMENT IN KEY                      
         LA    R1,3(R1,R6)         POINT FROM WHERE TO PRINT LEVD CD            
         ZIC   R5,INDLEN           GET LENGTH OF CODE TO PRINT                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   TLSKEY(0),0(R1)     MOVE ACCOUNT CODE TO TSAR                    
         MVC   TLSNAME,SVNAME                                                   
         ZAP   TLPUNIT,=P'0'                                                    
*                                                                               
         CLI   CSACT,A#MAINT       FOR ADD ACTION NO UNITS TO DISPLAY           
         BE    EXITOK                                                           
*                                                                               
         USING DSCELD,R5           LOOK FOR UNITS                               
         L     R5,AIO1                                                          
         LA    R5,ACTRFST-ACTRECD(R5)                                           
TSARFM20 CLI   0(R5),0             GET 62 ELEMENT                               
         BE    EXITOK                                                           
         CLI   0(R5),DSCELQ        IS IT SCHEME ELEMENT                         
         BNE   TSARFM30                                                         
         CLC   DSCCODE,SVSCHMCD    IS IT SAME SCHEME CODE                       
         BE    TSARFM40                                                         
TSARFM30 ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     TSARFM20                                                         
*                                                                               
TSARFM40 ZAP   TLPUNIT,DSCVAL                                                   
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
         OC    TLPUNIT,TLPUNIT                                                  
         BZ    EXITOK                                                           
*                                                                               
         ZAP   UNTAMT,TLPUNIT                                                   
         MVI   ADJCODE,C'A'                                                     
         AP    ADJAMT,TLPUNIT                                                   
*                                                                               
         LA    R6,IOKEY                                                         
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'ACTKCULA),0(R2)                                          
         ZIC   R1,DISPKEY          GET DISPLACEMENT IN KEY                      
         LA    R1,3(R1,R6)         POINT TO WHERE TO MOVE TSARKEY               
         ZIC   R5,INDLEN           GET LENGTH OF CODE TO PRINT                  
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R1),TLSKEY      MOVE ACCOUNT CODE TO KEY                     
*                                                                               
         GOTO1 =A(UPDT),BOPARM,RR=BORELO   LOOK FOR SCHEME CODE                 
         BAS   RE,UPDRECD          UPDAT HIGHER AND LOWER LEVEL ACC             
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR UPDATE LIST                                           *         
***********************************************************************         
         SPACE 1                                                                
LSTLAST  CLI   CSACT,A#MAINT                                                    
         BNE   LSTLASX                                                          
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$RECAD)                                           
         B     EXITL               EXIT WITH MESSAGE SET                        
LSTLASX  B     EXITOK              NO INPUT                                     
         EJECT                                                                  
*                                                                               
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
         BE    GTSCHM10                                                         
*                                                                               
         TM    FLAG,FLGDISP    SCHEME AT SVDISP HAS NOT BEEN PRCCESSED          
         BO    GTSCHMX        YET, PREVIOUS PAGE LAST RECORD IN PROCESS         
*                                                                               
         L     R6,SVDISP                                                        
         A     R6,AIO1                                                          
         B     GTSCHM40                                                         
*                                                                               
GTSCHM10 LA    R6,ACTRFST-ACTRECD(R6)                                           
GTSCHM20 CLI   0(R6),0             GET 62 ELEMENT                               
         BNE   GTSCHM30                                                         
         XC    SVDISP,SVDISP                                                    
         B     GTSCHMX                                                          
*                                                                               
GTSCHM30 CLI   0(R6),DSCELQ                                                     
         BE    GTSCHM50                                                         
GTSCHM40 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GTSCHM20                                                         
GTSCHM50 S     R6,AIO1                                                          
         ST    R6,SVDISP                                                        
*                                                                               
GTSCHMX  B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SCANMAX  EQU   40                  40 MAX ENTRIES PER LINE                      
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#SCMSG,16,L                                                    
         DCDDL AC#WARN,22,L                                                     
DCLISTX  DC    X'00'                                                            
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
OPTJOB   DC    AL1(OPTJOBX-*),CL8'JOB  ',X'00070C',AL2(JOBKEY-TWAD)             
         DC    AL4(VALJOB),AL1(OK)                                              
OPTJOBX  DS    0H                                                               
OPTF1    DC    AL1(OPTF1X-*),CL8'F1      ',X'000102',AL2(FLT1-TWAD)             
         DC    AL4(VALFILT),AL1(OK)                                             
OPTF1X   DS    0H                                                               
OPTF2    DC    AL1(OPTF2X-*),CL8'F2      ',X'000102',AL2(FLT2-TWAD)             
         DC    AL4(VALFILT),AL1(OK)                                             
OPTF2X   DS    0H                                                               
OPTF3    DC    AL1(OPTF3X-*),CL8'F3      ',X'000102',AL2(FLT3-TWAD)             
         DC    AL4(VALFILT),AL1(OK)                                             
OPTF3X   DS    0H                                                               
OPTSC    DC    AL1(OPTSCX-*),CL8'SC      ',X'000102',AL2(FLSC-TWAD)             
         DC    AL4(VALFILT),AL1(OK)                                             
OPTSCX   DS    0H                                                               
OPTUN    DC    AL1(OPTUNX-*),CL8'UNITS   ',X'00010C',AL2(FLTU-TWAD)             
         DC    AL4(VALUNT),AL1(A#DIS,A#CHA),AL1(0)                              
OPTUNX   DS    0H                                                               
         DC    X'00'                                                            
         EJECT                                                                  
**********************************************************************          
* SUBROUTINE TO  FILTER THE ACCOUNT BASED ON FILTER FIELD INPUT      *          
**********************************************************************          
*                                                                               
FILTR    NTR1  BASE=*,LABEL=*                                                   
         OC    FLT1(4),FLT1                                                     
         BZ    FILTR10             NO ACCOUNT FILTERS                           
*                                                                               
         LA    R3,BOELEM                                                        
         GOTO1 AGETEL,BOPARM,('RSTELQ',AIO1),0   GET X'30' ELEMENT              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING RSTELD,R3                                                        
         LA    R1,RSTFILT1         CHECK FILTER 1                               
         LA    RF,FLT1                                                          
         BAS   RE,FILTER                                                        
         B     FILTNO                                                           
         LA    R1,RSTFILT2         CHECK FILTER 2                               
         LA    RF,FLT2                                                          
         BAS   RE,FILTER                                                        
         B     FILTNO                                                           
         LA    R1,RSTANAL          CHECK FILTER 3                               
         LA    RF,FLT3                                                          
         BAS   RE,FILTER                                                        
         B     FILTNO                                                           
         LA    R1,RSTSUBC          FILTER ON SUB-COMPANY   CHECK FLTR 4         
         LA    RF,FLSC                                                          
         BAS   RE,FILTER                                                        
         B     FILTNO                                                           
*                                                                               
         USING CHDRECD,R6                                                       
FILTR10  DS    0H                                                               
         CLI   CSACT,A#MAINT                                                    
         BE    FILTR30                                                          
         CLI   JOBKEY,0            FILTERING BY JOB                             
         BE    FILTR20                                                          
         MVC   SVKEY,IOKEY         SAVE CURRENT                                 
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(15),SVKEY                                                  
         LA    R6,IOKEY                                                         
         MVC   CHDKCULC,JOBKEY                                                  
         XC    CHDKNULL,CHDKNULL                                                
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
         CLC   IOKEY(L'CHDKEY),IOKEYSAV                                         
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'SVKEY),SVKEY RESTORE ACCOUNT KEY                         
         BNE   FILTNO               JOB RECORD NOT FOUND                        
*                                                                               
         USING DSCELD,R3                                                        
FILTR20  L     R3,AIO1                                                          
         BRAS  RE,GETSCUN          GET MATCHING 62 ELEMENT SCHM CD ELEM         
         BNE   FILTNO              ELEMENT NOT FOUND                            
         OC    FLTUN,FLTUN                                                      
         BZ    *+14                NO PCNTS FILTER                              
         CP    DSCVAL,FLTUN                                                     
         BNE   FILTNO                                                           
         OC    FLTUNEG,FLTUNEG     NEGATIVE FILTER                              
         BZ    *+14                                                             
         CP    DSCVAL,FLTUNEG                                                   
         BE    FILTNO                                                           
         B     FILTYES                                                          
*                                                                               
FILTR30  DS    0H                  FOR NEW DISPLAY ACCOUNTS                     
         LA    R6,IOKEY                                                         
         CLI   JOBKEY,0                                                         
         BE    FILTR40                                                          
*                                                                               
         MVC   SVKEY,IOKEY         SAVE CURRENT                                 
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(15),SVKEY                                                  
         LA    R6,IOKEY                                                         
         MVC   CHDKCULC,JOBKEY                                                  
         XC    CHDKNULL,CHDKNULL                                                
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
         CLC   IOKEY(L'CHDKEY),IOKEYSAV                                         
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'SVKEY),SVKEY RESTORE ACCOUNT KEY                         
         BE    FILTR40             FOUND RECORD - LOOK FOR SCHEME               
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
         B     FILTYES             DISPLAY FOR NEW                              
*                                                                               
FILTR40  L     R3,AIO1                                                          
         BRAS  RE,GETSCUN          GET MATCHING 62 ELEMENT                      
         BNE   FILTYES             IF NOT FOUND - DISPLAY FOR NEW               
         B     FILTNO                                                           
*                                                                               
FILTYES  CR    RB,RB                                                            
         B     FILTX                                                            
FILTNO   LTR   RB,RB                                                            
FILTX    XIT1  ,                                                                
         EJECT                                                                  
         DROP  R3                                                               
* ROUTINE TO APPLY REQUEST FILTER TO ACCOUNT FILTER VALUE                       
         SPACE 1                                                                
FILTER   CLI   0(RF),0             NO FILTER                                    
         BE    4(RE)                                                            
         MVC   BOWORK1(1),0(RF)                                                 
         MVC   BOWORK1+1(1),0(R1)                                               
         LA    R1,X'80'            SET MASK TO EQUAL                            
         TM    BOWORK1,X'40'       TEST POSITIVE FILTER                         
         BNZ   *+12                                                             
         LA    R1,X'70'            SET MASK TO NOT EQUAL                        
         OI    BOWORK1,X'40'                                                    
         CLC   BOWORK1(1),BOWORK1+1  MATCH REQUEST FILTER TO VALUE              
         EX    R1,*+8                                                           
         B     0(RE)               EXCLUDE                                      
         NOP   4(RE)               INCLUDE                                      
         DROP  RB                                                               
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*            TO GET MATCHING SCHEME CODE AND UNITS                  *           
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
**********************************************************************          
* ADD STAR RECDS FOR EACH LEDGER LEVEL LENGTH  IF NOT YET ON FILE    *          
**********************************************************************          
         SPACE 1                                                                
*        USING TWUSER,R4                                                        
ADDSTARS NMOD1 0,**STARS**                                                      
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,SVLDG     PUT IN LDGR/ADVRTSR CODE IN KEY           
         DROP  TEMP                                                             
*                                                                               
         ZIC   R0,LEVNUM                                                        
         LA    R5,LEVLNQS                                                       
         LA    R6,LEVLDSPS                                                      
ADDSTR10 DS    0H                                                               
         ZIC   R1,0(R6)            GET DISPLACEMENT IN KEY                      
         LA    R3,IOKEY+3(R1)                                                   
         ZIC   R1,0(R5)            GET INDIVIDUAL LENGTH                        
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=12C'*'                                                  
*                                                                               
T        USING CHDRECD,RF                                                       
         LA    RF,IOKEY                                                         
         CLI   JOBKEY,0                                                         
         BE    *+16                                                             
         MVC   T.CHDKCULC,JOBKEY     FILL IN CONTRA ACCOUNT                     
         XC    T.CHDKNULL,T.CHDKNULL                                            
         DROP  T                                                                
*                                                                               
         L     R1,=AL4(XOHID+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         BNE   ADDSTR20                                                         
*                                                                               
T        USING ACTRECD,RF                                                       
         LA    RF,IOKEY                                                         
         TM    T.ACTKSTAT,ACTSDELT IS IT DELETED RECORD                         
         BNO   ADDSTR30            IF NOT THEN READ NEXT                        
         NI    T.ACTKSTAT,X'FF'-ACTSDELT  ELSE UNDELETE IT                      
*                                                                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD COMPANY RECORD                           
T        USING ACTRECD,RF                                                       
         L     RF,AIO2                                                          
         NI    T.ACTRSTAT,X'FF'-ACTSDELT  ELSE UNDELETE IT                      
*                                                                               
         GOTOX ('PUTRAC',AGROUTS),BOPARM,('RACTCHA',AIO2) CHG F9 ELEM           
*                                                                               
         LHI   R1,XOPUT+XOACCMST+XIO2  PUT THE RECORD BACK                      
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
         LHI   R1,XOWRITE+XOACCDIR+XIO2   WRITE DIRECTORY RECORD BACK           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  T                                                                
*                                                                               
ADDSTR20 MVC   IOKEY,IOKEYSAV      RESTORE IOKEY                                
         STC   R0,SVLEV                                                         
         BAS   RE,BLDSTR           BUILD AND ADD STAR RECORD                    
ADDSTR30 LA    R5,1(R5)            GET NEXT LEVEL STAR RECORD                   
         LA    R6,1(R6)                                                         
         BCT   R0,ADDSTR10                                                      
ADDSTARX XIT1                                                                   
         EJECT                                                                  
****************************************************************                
* BUILD ELEMENTS FOR STAR RECORDS                              *                
* BUILD NAME AND STATUS ELEMENT FOR EACH LEVEL OF STAR ACCOUNT *                
* BUILD NAME, STATUS,BALANCE AND PEEL OFF ELEMENT FOR LOWEST   *                
* LEVEL STAR ACCOUNT.                                          *                
* SVLEV NUM OF LEVEL IN PROCESS                                *                
* SVLEV IS USED TO ADD BALANCE AND PEEL ELEMENT WHEN SVLEV=1   *                
****************************************************************                
         SPACE 1                                                                
BLDSTR   NTR1                                                                   
         L     RE,AIO2                                                          
         LHI   RF,IOAREALN                                                      
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
T        USING ACTRECD,R6                                                       
         L     R6,AIO2            NOT ON FILE BUILD NEW RECORD                  
*                                                                               
         MVC   T.ACTKEY,BCSPACES   SPACE PADDED ACCOUNT KEY                     
         MVC   T.ACTKEY,IOKEYSAV   **** ACCOUNT TO KEY.                         
         LH    RE,=Y(ACTRFST-ACTRECD+1)  ACCOUNTS LENGTH                        
         STCM  RE,3,T.ACTRLEN                                                   
         XC    T.ACTRSTA,T.ACTRSTA  CLEAR STATUS FIELD                          
*                                                                               
         CLI   JOBKEY,0            ARE WE DOING CONTRA ACCOUNT                  
         BE    BLDSTR10            NO ADD 20,30 ETC ELEMENTS TO RECORD          
*                                                                               
         PUSH  USING               ADD X'43' ELEMENT SINCE IT IS CONTRA         
         XC    BOELEM,BOELEM                                                    
         USING CACELD,BOELEM                                                    
         MVC   BOELEM(L'CNTRJOB),CNTRJOB                                        
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO2,BOELEM,0                      
         CLI   12(R1),0                                                         
         BNE   BEXITNV                                                          
         XC    BOELEM,BOELEM                                                    
         POP   USING                                                            
         B     BLDSTR20                                                         
*                                                                               
         PUSH  USING                                                            
BLDSTR10 XC    BOELEM,BOELEM                                                    
         USING NAMELD,BOELEM                                                    
         MVI   NAMEL,NAMELQ       X'20' NAME FOR *** ACCOUNT                    
         MVI   NAMLN,14           MAX NO MORE THAN 12                           
         MVC   NAMEREC(12),T.ACTKEY+3                                           
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO2,BOELEM,0                      
         CLI   12(R1),0                                                         
         BNE   BEXITNV                                                          
         XC    BOELEM,BOELEM                                                    
         POP   USING                                                            
*                                                                               
         PUSH  USING                                                            
         USING RSTELD,BOELEM                                                    
         XC    RSTELD(RSTLN2Q),RSTELD                                           
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN2Q                                                    
         MVC   RSTBDATE,BCTODAYP                                                
         MVC   RSTTDATE,BCTODAYP                                                
         MVI   RSTCOSTG,C' '                                                    
         MVI   RSTFILT1,C' '                                                    
         MVI   RSTFILT2,C' '                                                    
         MVI   RSTFILT3,C' '                                                    
         MVI   RSTFILT4,C' '                                                    
         MVI   RSTFILT5,C' '                                                    
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO2,BOELEM,0                      
         CLI   12(R1),0                                                         
         BNE   BEXITL                                                           
         POP   USING                                                            
*                                                                               
         ZIC   R0,SVLEV                                                         
         CHI   R0,1           ARE WE AT LOWEST LEVEL YET                        
         BNE   BLDSTR20       NO, DON'T ADD BALANCE AND PEEL HERE               
*                                                                               
         OI    T.ACTRSTAT,ACTSABLP   ACCOUNT HAS BALANCE ELEMENT                
         XC    BOELEM,BOELEM                                                    
         PUSH  USING                                                            
         USING ABLELD,BOELEM                                                    
         XC    BOELEM,BOELEM                                                    
         MVI   ABLEL,ABLELQ       X'32'                                         
         MVI   ABLLN,ABLLN3Q      LENGTH                                        
         ZAP   ABLFRWD,=P'0'      BALANCE BROUGHT FORWARD                       
         ZAP   ABLDR,=P'0'        DEBIT SINCE LAST FORWARD                      
         ZAP   ABLCR,=P'0'        CREDITS SINCE BALANCE FORWARD                 
         ZAP   ABLURG,=P'0'       URGENT CREDITS                                
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO2,BOELEM,0                      
         CLI   12(R1),0                                                         
         BNE   BEXITNV                                                          
         POP   USING                                                            
*                                                                               
         XC    BOELEM,BOELEM                                                    
         PUSH  USING                                                            
         USING APOELD,BOELEM                                                    
         MVI   APOEL,APOELQ        X'33'                                        
         MVI   APOLN,APOLN2Q                                                    
         XC    APOPLDT,APOPLDT     DATE OF THIS PEEL-OFF                        
         XC    APOLBDT,APOLBDT     DATE OF LAST BAL B/FRWD                      
         ZAP   APODR,=P'0'                                                      
         ZAP   APOCR,=P'0'                                                      
         XC    APOLMOS,APOLMOS                                                  
         XC    APOCMOS,APOCMOS                                                  
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO2,BOELEM,0                      
         CLI   12(R1),0                                                         
         BNE   BEXITNV                                                          
         POP   USING                                                            
*                                                                               
BLDSTR20 DS    0H                                                               
*                                                                               
         GOTOX ('PUTRAC',AGROUTS),BOPARM,('RACTADD',AIO2) ADD F9 ELEM           
*                                                                               
         LHI   R1,XOADDREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
*                                                                               
BLDSTRX  XIT1                                                                   
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* EXITS - ADDSTAR NMOD                                                *         
***********************************************************************         
         SPACE 1                                                                
BEXITNV  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     BEXITL              EXIT WITH FIELD NOT VALID SET                
*                                                                               
BEXITH   CLI   *,0                 SET CC HIGH                                  
         B     BEXIT                                                            
BEXITL   CLI   *,FF                SET CC LOW                                   
         B     BEXIT                                                            
BEXITOK  CR    RB,RB               SET CC EQUAL                                 
*                                                                               
BEXIT    B     ADDSTARX                                                         
         EJECT                                                                  
***********************************************************************         
* LITERALS  FOR ADDSTARS NMOD                                         *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
* GET HEIRARCHY LEVEL STRUCTURE                                      *          
**********************************************************************          
         SPACE 1                                                                
         USING ACLELD,R5                                                        
*        USING TWUSER,R4                                                        
GETLEVS  NMOD1 0,**GETLEV**                                                     
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LA    R5,BOELEM                                                        
         MVC   LEVELS(LEVLNQ),BCSPACES                                          
         SR    R1,R1                                                            
         IC    R1,ACLLN            R1 = LENGTH OF ELEMENT                       
         SH    R1,=Y(ACLLN1Q+1)    SUBTRACT OVERHEAD                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LEVELS(0),ACLVALS   MVC LEVEL LENGTHS/NAMES INTO STORAGE         
*                                                                               
* CONVERT 1,3,5,12 -> 1,2,2,7                                                   
*                                                                               
         LA    R0,LEVELQ           R0 = MAXIMUM NUMBER OF LEVELS                
         STC   R0,LEVNUM           ASSUME 4 LEVEL STRUCTURE                     
         LA    R1,LEVELS           R1 = FIRST LEVEL LENGTH                      
         LA    R2,LEVLNQS          R2 = FIRST LEVEL INDIVIDUAL LENGTH           
         SR    R3,R3               R3 = PREV LEVEL LENGTH (INITIALLY 0)         
         SR    R6,R6                                                            
GLEV10   ICM   R6,1,0(R1)          CURRENT LEVEL LENGTH                         
         BZ    GLEV20              NO MORE LEVELS - ADJUST LEVNUM               
         SR    R6,R3               SUBTRACT CURRENT FROM PREVIOUS               
         STC   R6,0(R2)            CURRENT INDIVIDUAL LENGTH                    
         IC    R3,0(R1)            UPDATE R3                                    
         LA    R1,L'ACLVALS(R1)                                                 
         DROP  R5                                                               
         LA    R2,1(R2)                                                         
         BCT   R0,GLEV10                                                        
*        B     GLEVX                                                            
*                                                                               
GLEV20   LA    R1,LEVELQ           R1 = MAXIMUM NUMBER OF LEVELS                
         SR    R1,R0               R0 = NUM OF LEVELS OF MAX NOT USED           
         STC   R1,LEVNUM           LEVNUM = ACTUAL NUMBER OF LEVELS             
         LTR   R1,R1                                                            
         BZ    GLEVXIT                                                          
*                                                                               
GLEVX    DS    0H                                                               
         LA    R2,LEVLNQS          POINT TO INDIVIDUAL LENGTHS                  
         LA    R3,LEVELS                                                        
         LA    R6,LEVLDSPS         POINT TO DISPLACEMENTS                       
*                                  DISP=CUMULLEN - CURRENT INDIV LEN            
GLEVX10  ZIC   R0,0(R3)            GET CUMULATIVE LENGTH                        
         ZIC   R5,0(R2)            GET INDVIDUAL LENGTH                         
         SR    R0,R5               GET DISPLACEMENT IN KEY                      
         STC   R0,0(R6)            SAVE DISPLACEMENT                            
*                                                                               
         LA    R2,1(R2)            BUMP TO NEXT IND LEN                         
         LA    R3,L'LEVELS(R3)     BUMP TO NEXT CUMULATIVE LEN                  
         LA    R6,1(R6)            BUMP TO NXT EMPTY DISPLACEMENT               
         BCT   R1,GLEVX10                                                       
GLEVXIT  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS  FOR GETLEVS NMOD1                                         *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE TO UPDATE UNITS                                          *         
***********************************************************************         
UPDT     NMOD1 0,**UPDT**                                                       
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LA    R6,IOKEY                                                         
         CLI   JOBKEY,0                                                         
         BE    UPDT10                                                           
         USING CHDRECD,R6                                                       
         MVC   CHDKCULC,JOBKEY                                                  
         XC    CHDKNULL,CHDKNULL                                                
UPDT10   DS    0H                                                               
         L     R1,=AL4(XORDUP+XOACCDIR+XIO1)   READ FOR UPDATE                  
         GOTO1 AIO                                                              
         BNE   UPDT15                                                           
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO1) GETREC FOR UPDATE                
         GOTO1 AIO                                                              
*                                                                               
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         BE    UPDT30              RECORD ALREADY EXIST                         
UPDT15   L     RE,AIO1             CLEAR IO1                                    
         LHI   RF,IOAREALN                                                      
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R6,AIO1             NOT ON FILE BUILD NEW RECORD                 
         MVC   CHDKEY,BCSPACES                                                  
         MVC   CHDKEY,IOKEYSAV                                                  
         XC    CHDKNULL,CHDKNULL                                                
         LH    RE,=Y(CHDRFST-CHDRECD+1)                                         
         STCM  RE,3,CHDRLEN        UPDATE LENGTH OF NEW RECD                    
         XC    CHDRSTA,CHDRSTA                                                  
*                                                                               
         MVC   BOELEM(L'CNTRJOB),CNTRJOB                                        
         CLI   CHDKCULC,X'40'      FILTERING ON JOB                             
         BNE   UPDT20              YES,OK                                       
         DC    H'0'                NO, ALL OTHERS ADDED IN INIT                 
*                                                                               
UPDT20   DS    0H                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO1,BOELEM,0  ADD CON ELM         
         CLI   12(R1),0                                                         
         BNE   ERXITNV                                                          
         B     UPDT40                                                           
UPDT30   DS    0H                                                               
*        GOTO1 AGETACT,0                                                        
UPDT40   L     R3,AIO1                                                          
         BAS   RE,CHKSCUN                                                       
         BNE   UPDT60              X'62' ELEMENT NOT FOUND ADD ONE              
         USING DSCELD,R3                                                        
         CLI   ADJCODE,C'L'        ADJUSTING LOW LEVEL ACCOUNT                  
         BE    UPDT50                                                           
         SP    ADJAMT,DSCVAL                                                    
         CP    UNTAMT,=P'0'                                                     
         BE    UPDT80                                                           
         ZAP   DSCVAL,UNTAMT                                                    
         B     UPDT90                                                           
UPDT50   DS    0H                                                               
         AP    DSCVAL,ADJAMT                                                    
         CP    DSCVAL,=P'0'                                                     
         BE    UPDT80                                                           
         B     UPDT90                                                           
*                                                                               
UPDT60   DS    0H                                                               
         CP    UNTAMT,=P'0'                                                     
         BE    UPDTX                                                            
UPDT70   DS    0H                                                               
         LA    R3,BOELEM                                                        
         MVI   DSCEL,DSCELQ        MOVE BUILD X'62' ELEMENT                     
         MVI   DSCLN,X'0A'                                                      
         MVC   DSCCODE,SVSCHMCD                                                 
         ZAP   DSCVAL,UNTAMT                                                    
         CLI   ADJCODE,C'L'                                                     
         BNE   *+10                                                             
         ZAP   DSCVAL,ADJAMT                                                    
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO1,BOELEM,0                      
         CLI   12(R1),0                                                         
         BNE   ERXITNV                                                          
         B     UPDT90                                                           
UPDT80   DS    0H                                                               
         MVI   DSCEL,X'FF'         NEW UNIT IS ZERO DELETE X'62'                
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',AIO1),0                     
         CLI   12(R1),0                                                         
         BNE   ERXITNV                                                          
UPDT90   DS    0H                                                               
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         BE    UPDT100                                                          
*                                                                               
         GOTOX ('PUTRAC',AGROUTS),BOPARM,('RACTADD',AIO1) ADD F9 ELEM           
*                                                                               
         LHI   R1,XOADDREC+XOACCMST+XIO1  ADD THE RECORD                        
         GOTO1 AIO                                                              
         BNE   *+14                                                             
         MVC   IOKEY,IOKEYSAV      RESTORE IOKEY FOR NEXT ADD                   
         B     UPDTX                                                            
         DC    H'0'                BAD ACCOUNT RECORD                           
UPDT100  DS    0H                                                               
*                                                                               
         GOTOX ('PUTRAC',AGROUTS),BOPARM,('RACTCHA',AIO1) CHG F9 ELEM           
*                                                                               
         LHI   R1,XOPUT+XOACCMST+XIO1  PUT THE RECORD BACK                      
         GOTO1 AIO                                                              
         BE    UPDTX                                                            
         DC    H'0'                BAD ACCOUNT RECORD                           
UPDTX    XIT1                                                                   
         DROP  R3,R6                                                            
*                                                                               
ERXITNV  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         CLI   *,FF                SET CC LOW                                   
         B     UPDTX                                                            
***********************************************************************         
* LITERALS  FOR UPDT NMOD1                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
*********************************************************************           
*            TO GET MATCHING SCHEME CODE AND UNITS                  *           
*********************************************************************           
         USING DSCELD,R3                                                        
CHKSCUN  DS    0H                                                               
         LA    R3,ACTRFST-ACTRECD(R3)                                           
CHKSCU10 CLI   0(R3),0             GET 62 ELEMENT                               
         BNE   CHKSCU20                                                         
         LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
CHKSCU20 CLI   0(R3),DSCELQ        IS IT X'62'                                  
         BNE   CHKSCU30                                                         
         CLC   DSCCODE,SVSCHMCD    IS IT SAME SCHEME/MARKET CODE                
         BER   RE                                                               
*                                                                               
CHKSCU30 ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CHKSCU10                                                         
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* READ STAR RECORDS FOR TOTAL UNITS AND CALCULATE PERCENT TO DISPLAY  *         
* PERCENT CALCULATED WILL BE PUT INTO TSAR RECORD                     *         
* ASSUMES RECORD IS IN AIO1                                           *         
***********************************************************************         
PCNT     NMOD1 0,**PCNT**                                                       
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         USING TLSTD,R5                                                         
         L     R5,ATLST                                                         
         OC    TLPUNIT,TLPUNIT                                                  
         BZ    DISPCTX                                                          
         ZAP   SVUNITS,TLPUNIT                                                  
*                                                                               
         ZAP   TLTOTUNT,SVTOTAL                                                 
         CP    TLTOTUNT,=P'0'                                                   
         BE    DISPCTX                                                          
         MP    SVUNITS,=P'10000000'                                             
         DP    SVUNITS,TLTOTUNT                                                 
         SRP   SVUNITS(10),64-1,5                                               
*                                                                               
         EDIT  (P10,SVUNITS),(8,FVIFLD),4,WRK=BOWORK1,DUB=BODUB1,      +        
               ALIGN=LEFT,DROP=2                                                
*                                                                               
DISPCTX  DS    0H                                                               
         CR    RB,RB                                                            
         XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS  FOR PCNT DISPLAY NMOD                                     *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* READ STAR RECORDS FOR TOTAL UNITS                                   *         
* ASSUMES RECORD IS IN AIO1                                           *         
***********************************************************************         
GETTOT   NMOD1 0,*GETTOT*                                                       
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         ZAP   SVTOTAL,=P'0'                                                    
*                                                                               
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVI   TEMP.ACTKUNT,C'3'      ALWAYS UNIT  C'3' FOR DISTRI RECS         
         MVC   TEMP.ACTKLDG,SVLDG     PUT IN LDGR/ADVRTSR CODE IN KEY           
*                                                                               
         LA    R5,LEVELS           GET LENGTH TO MOVE STAR RECDS READ           
         ZIC   R1,LEVNUM                                                        
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    GETTOT10                                                         
         AHI   R5,L'LEVELS         POINT TO INDIVIDUAL LENGTH                   
         BCT   R1,*-4                                                           
         SHI   R5,L'LEVELS         POINT TO INDIVIDUAL LENGTH                   
GETTOT10 ZIC   R1,0(R5)            GET LENGTH OF STAR ACCOUNT                   
         BCTR  R1,0                FOR EX INSTRUCTION                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   TEMP.ACTKACT(0),=12C'*'                                          
         DROP  TEMP                                                             
         CLI   JOBKEY,0                                                         
         BE    GETTOT20                                                         
T        USING CHDRECD,RF          FILL THE KEY WITH JOB                        
         MVC   T.CHDKCULC,JOBKEY                                                
         XC    T.CHDKNULL,T.CHDKNULL NULLS IN CHDRECD SPACES OTHERWISE          
         DROP  T                                                                
*                                                                               
GETTOT20 LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         BNE   GETTOTX                                                          
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
         L     R6,AIO2                                                          
         LA    R6,ACTRFST-ACTRECD(R6)                                           
         USING DSCELD,R6                                                        
GETTOT30 CLI   0(R6),0             GET 62 ELEMENT                               
         BE    GETTOTX                                                          
         CLI   0(R6),DSCELQ        IS IT SCHEME ELEMENT                         
         BNE   GETTOT40                                                         
         CLC   DSCCODE,SVSCHMCD    IS IT SAME SCHEME CODE                       
         BE    GETTOT50                                                         
GETTOT40 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     GETTOT30                                                         
*                                                                               
GETTOT50 ZAP   SVTOTAL,DSCVAL                                                   
GETTOTX  DS    0H                                                               
         CR    RB,RB                                                            
         XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS  FOR GET TOTAL NMOD                                        *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* REAJ CLIENT OR JOB RECORD FIND THE OFFICE AND SEND IT TO OFFAL FOR  *         
* OFFICE/LIMITED ACCESS VALIDATION                                    *         
* ASSUMES RECORD P0=A(SJ/CLI/PRO/JOB)                                 *         
***********************************************************************         
VALOFF   NMOD1 0,*VALOFF*                                                       
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         NI    FLAG,X'FF'-FLGOFF                                                
         L     R5,0(R1)                                                         
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY(9),0(R5)  MOVE C/U/L/CLI/PRO JOB                    
         DROP  TEMP                                                             
*                                                                               
VALOF10  DS    0H                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BNE   VALOFL                                                           
*        GOTO1 AGETACT,0                                                        
*        BNE   VALOFL                                                           
*                                                                               
         GOTO1 AGETEL,BOPARM,('PPRELQ',AIO1),0   GET X'24' ELEMENT              
         BNE   VALOF15                                                          
         USING PPRELD,R3                                                        
         LA    R3,BOELEM                                                        
         CLC   PPRGAOFF,BCSPACES    IS THERE AN OFFICE TO WORK WITH             
         BH    VALOF20                                                          
VALOF15  CLI   IOKEYSAV+6,X'40'     DONE WITH VALIDATION OF OFFICE              
         BE    VALOFFX                                                          
TEMP     USING ACTRECD,RF                                                       
         LA    RF,IOKEY                                                         
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY(6),0(R5)  MOVE C/U/L/CLI CODE                       
         B     VALOF10                                                          
         DROP  TEMP                                                             
VALOF20  DS     0H                                                              
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK          TEST OFFICE SECURITY                         
         MVI   OFFAACT,OFFAVAL     VALIDATE INPUT OFFICE                        
         MVC   OFFAOFFC,PPRGAOFF   OFFICE TO VALIDATE                           
         GOTO1 VOFFAL                                                           
         BE    VALOFFX                                                          
         OI    FLAG,FLGOFF               THIS OFFICE IS NOT GOOD                
         MVC   FVMSGNO,=AL2(AE$SECLK)    SECURITY LOCKOUT INVALID OFF           
VALOFL   CLI   *,FF                      EXIT LOW                               
         B     *+6                                                              
VALOFFX  CR    RB,RB                                                            
         XIT1                                                                   
         DROP  R1,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS  FOR VALIDATE OFFICE NMOD                                  *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
**********************************************************************          
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
TYPE     DS    CL1                 PERCENT P                                    
SVACT    DS    X                   SAVED ACTION NUMBER                          
SVDISP   DS    F                                                                
SVUNITS  DS    PL16                SAVED NUMBER OF UNITS                        
SVTOTAL  DS    PL6                 TOTAL FROM **** RECORDS                      
ADJAMT   DS    PL6                                                              
SVSCHMCD DS    CL2                 SAVED SCHEME/WORK CODE                       
SVLDG    DS    CL1                 SAVED ADVERTISER/LEDGER CODE                 
FLAG     DS    XL1                 STATUS                                       
FLGDISP  EQU   X'80'               DISP AVLBL FRM LAST PAGE'S LAST RECD         
FLGOFF   EQU   X'40'               OFFICE IN JOB DOES NOT HAVE ACCESS           
SVLEV    DS    XL1                 SAVED LEVELS                                 
LVLBYTE  DS    XL1                 CHECKS WHICH LEVELS HAS BEEN PRCESD          
LVLADN   EQU   X'80'               ON LEVEL A PROCESSED                         
LVLBDN   EQU   X'40'               ON LEVEL B PROCESSED                         
LVLCDN   EQU   X'20'               ON LEVEL C PROCESSED                         
IOKEYLST DS    XL15                LAST KEY FOR DISLVDCD CODES                  
SVKEY    DS    XL64                SVKEY                                        
LOKEY    DS    XL64                KEY AT LOWEST LEVEL ACCOUNT                  
*                                                                               
LEVELS   DS    0XL16                                                            
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVADSC  DS    CL15                LEVEL A NAME                                 
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVBDSC  DS    CL15                LEVEL B NAME                                 
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVCDSC  DS    CL15                LEVEL C NAME                                 
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVDDSC  DS    CL15                LEVEL D NAME                                 
LEVLNQ   EQU   *-LEVELS                                                         
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
LEVLDSPS DS    0XL1                DISPLACEMENT OF LVLS IN KEY                  
LEVLDSPA DS    XL1                 LEVEL A DISPLACEMENT                         
LEVLDSPB DS    XL1                 LEVEL B DISPLACEMENT                         
LEVLDSPC DS    XL1                 LEVEL C DISPLACEMENT                         
LEVLDSPD DS    XL1                 LEVEL D DISPLACEMENT                         
LEVEND   EQU   *                                                                
*                                                                               
OPFLDS   DS    0C                                                               
STRKEY   DS    CL15                ACCOUNT START KEY                            
JOBKEY   DS    CL15                JOB FILTER                                   
FLT1     DS    CL1                 FILTER 1 VALUE                               
FLT2     DS    CL1                 FILTER 2 VALUE                               
FLT3     DS    CL1                 FILTER 3 VALUE                               
FLSC     DS    CL1                 FILTER 4 SUB-COMPANY                         
FLTU     DS    CL1                 UNITS FILTER                                 
FLTUN    DS    PL6                 UNITS VALUE                                  
FLTUNEG  DS    PL6                 ALL EXCEPT UNITS VALUE                       
OPFLDLQ  EQU   *-OPFLDS                                                         
SCANBLK  DS    10CL(SCANTOTL)                                                   
CNTRJOB  DS    CL53                SAVE JOB FROM FILTER FIELD                   
UNTAMT   DS    PL6                 RETURNED FROM CASHVAL                        
DISPKEY  DS    X                   DISPLACEMENT IN KEY                          
INDLEN   DS    X                   INDIVIDUAL LENGTH                            
SVOPTN   DS    CL(OPFLDLQ)         SAVED OPFLDS                                 
*                                                                               
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
ALVLAFLD DS    A                   A(LEVEL FIELD A TO UNPROTECT)                
*                                                                               
SVANAME  DS    CL36                SAVE ADVERTISR NAME                          
SVNAME   DS    CL36                SAVE ACCOUNT NAME                            
SVSCHNM  DS    CL15                SAVE SCHEME CODE DESCRIPTION                 
ADJCODE  DS    CL1                 WHICH LEVEL IS BEING ADJUSTED                
*                                                                               
SCANRHSL EQU   20                                                               
SCANTOTL EQU   (22+SCANRHSL)       LENGTH OF SCAN BLOCK                         
FLAG1    DS    CL1                                                              
FNDX     DS    CL1                                                              
ERROR    DS    CL1                                                              
NV       EQU   X'80'                                                            
DUP      EQU   X'40'                                                            
OK       EQU   X'FF'                                                            
DSLIST   DS    0C                                                               
AC@SCMSG DS    CL16                TOTAL FOR SCHEME                             
AC@WARN  DS    CL22                WARNING TOTAL NOT 100%                       
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLUSER                                                           
LISTSCCD DS    XL(L'DSCCODE)       SCHEME/WORK CODES FOR LIST                   
LISTUNTS DS    XL12                UNITS FOR LIST                               
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
TLTOTUNT DS    PL6                 SAVED TOTAL UNITS                            
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
         EJECT ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACFIL33   02/20/13'                                      
         END                                                                    
