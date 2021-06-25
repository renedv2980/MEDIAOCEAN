*          DATA SET CTFIL2A    AT LEVEL 079 AS OF 01/14/20                      
*&&      SET   NOP=N                                                            
*PHASE TA132AA                                                                  
         TITLE 'FORMAT RECORD'                                                  
FIL2A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL2A*,R6,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
         LH    R7,=Y(TWUSER-TWAD)                                               
         AR    R7,RA                                                            
         USING TWUSER,R7                                                        
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
         B     EXIT                                                             
*                                                                               
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         SPACE 2                                                                
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITTOOM MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               EXIT WITH TOO MUCH DATA SET                  
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     OI    GSINDSL1,GSIXKEY    DON'T ASK 'ENTER KEY'                        
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
SCRN     LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
SCRTABL  DC    AL1(SSET),AL1(0,0,0),AL4(SCRSET)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SCRSET - SET MAINT CODE BASED ON INPUT TYPE                         *         
***********************************************************************         
         SPACE 1                                                                
SCRSET   MVC   GSSMCODE,KEYCODE                                                 
         B     EXITOK                                                           
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
         USING BANKRECD,R2                                                      
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
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    BANKEY,BANKEY                                                    
         MVI   BANKTYP,BANKTYPQ    B - BANK REC                                 
         MVI   BANKSUB,BANKSFQ     F - FORMAT                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    BANKEY,BANKEY                                                    
         MVI   BANKTYP,BANKTYPQ    B - BANK                                     
         MVI   BANKSUB,BANKSFQ     F - FORMAT                                   
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
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING BANKRECD,R2                                                      
         LLC   R1,SVPARMS3         GET GLOBAL VERB                              
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
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING BANKRECD,R2                                                      
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F                                                               
KNOWTAB  DC    AL2(600),AL4(BKCDTA)    BANK CODE   (KEY)                        
         DC    AL2(601),AL4(BKNDTA)    BANK NAME                                
         DC    AL2(602),AL4(FMCDTA)    FORMAT CODE (KEY)                        
         DC    AL2(603),AL4(FMNDTA)    FORMAT NAME                              
         DC    AL2(703),AL4(FMNDTA1)   FORMAT NAME                              
         DC    AL2(611),AL4(LINDTA)    LINE NUMBER                              
*                                                                               
         DC    AL2(604),AL4(RCCDTA)    RECORD CODE (KEY)                        
         DC    AL2(612),AL4(RCNDTA)    RECORD NAME                              
*                                                                               
         DC    AL2(605),AL4(RSZDTA)    RECORD SIZE                              
         DC    AL2(705),AL4(RSZDTA1)   RECORD SIZE                              
         DC    AL2(610),AL4(BSZDTA)    BLOCK SIZE                               
         DC    AL2(710),AL4(BSZDTA1)   BLOCK SIZE                               
         DC    AL2(632),AL4(HTADTA)    HDR/TRL BY ACCOUNT                       
         DC    AL2(633),AL4(HTTDTA)    HDR/TRL BY TOTAL                         
         DC    AL2(634),AL4(HTCDTA)    HDR/TRL INCLUDED IN COUNT                
         DC    AL2(635),AL4(INVDTA)    INCLUDE VOIDS IN TOTAL COUNT             
         DC    AL2(636),AL4(HTRDTA)    HDR/TRL REQUIRED FOR DATA                
         DC    AL2(637),AL4(TOTDTA)    ALWAYS PASS TOTAL TRAILER                
         DC    AL2(638),AL4(EXVDTA)    EXCLUDE VOIDS IN CNT/AMT                 
         DC    AL2(639),AL4(FRDDTA)    FOREIGN DEPENDANT                        
         DC    AL2(640),AL4(FRGDTA)    FOREIGN ONLY COUNT                       
         DC    AL2(641),AL4(ADVDTA)    ADD VOIDS TO TOTAL                       
         DC    AL2(642),AL4(USODTA)    US ONLY                                  
         DC    AL2(643),AL4(FDEDTA)    RECORD IS FIELD DEPENDANT                
         DC    AL2(644),AL4(BVLDTA)    BUILD VARIABLE LENGTH FIELDS             
         DC    AL2(645),AL4(VLDDTA)    VAR LEN FIELD DELIMITER - COMMA          
         DC    AL2(646),AL4(FSCDTA)    FIELD SURROUNDING CHAR - QUOTES          
         DC    AL2(613),AL4(VRFDTA)    VARIABLE RECORD FORMAT                   
         DC    AL2(614),AL4(ADRDTA)    BREAK UP ADDRESS                         
         DC    AL2(615),AL4(CANDTA)    CANADA AS FOREIGN                        
         DC    AL2(616),AL4(RNRDTA)    REMOVE * IN NARRATIVE                    
         DC    AL2(617),AL4(CTYDTA)    OMIT CITY DUPLICATION                    
*MN                                                                             
         DC    AL2(618),AL4(CSVCMA)    CSV - STRIP COMMAS FROM TEXT             
*MN                                                                             
         DC    AL1(EOF)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL2A    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL                 TABLE OF KNOWN INVOKERS                
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFD)                                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFD)                                    
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* FIRST TIME FOR A DATA OBJECT                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKRECD,R2                                                      
DFD      CLI   BANKLIN#,0                                                       
         BNE   DFD02                                                            
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
*                                                                               
         XC    BOELEM,BOELEM     IF IT ISNT THERE, ADD IT                       
         LA    R4,BOELEM                                                        
         USING BFIELD,R4                                                        
         MVI   BFIEL,BFIELQ                                                     
         MVI   BFILN,BFILNQ                                                     
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),(R2),BOELEM,0                      
         B     EXITOK                                                           
*                                                                               
DFD02    GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
*                                                                               
         XC    BOELEM,BOELEM     IF IT ISNT THERE, ADD IT                       
         LA    R4,BOELEM                                                        
         USING BFRELD,R4                                                        
         MVI   BFREL,BFRELQ                                                     
         MVI   BFRLN,BFRLNQ                                                     
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),(R2),BOELEM,0                      
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BANK CODE OBJECT                                                    *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
BKCDTA   LA    RF,BKCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BKCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBKC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFBKC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBKC)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFBKC)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOBKC)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BANK CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISBKC   MVC   FVIFLD(L'BANKCDE),BANKCDE                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY A BANK CODE FIELD FILTER                                    *         
***********************************************************************         
         SPACE 1                                                                
DISFBKC  MVC   FVIFLD(L'BANKCDE),FLTIFLD                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE BANK CODE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALBKC   MVC   BANKCDE,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE BANK CODE FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALFBKC  MVC   BANKCDE,FVIFLD                                                   
         MVC   FLTIFLD(L'BANKCDE),FVIFLD                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FILTER ON BANK CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DOBKC    CLC   BANKCDE,FVIFLD                                                   
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* BANK NAME OBJECT                                                    *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
BKNDTA   LA    RF,BKNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BKNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBKN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BANK NAME FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
T        USING BANKRECD,R3                                                      
DISBKN   LA    R3,IOKEY                                                         
         XC    T.BANKEY,T.BANKEY                                                
         MVI   T.BANKTYP,BANKTYPQ                                               
         MVI   T.BANKSUB,BANKSGQ                                                
         MVC   T.BANKCDE,BANKCDE   MOVE IN THE BANK CODE                        
         L     R1,=A(XOGENDIR+XOREAD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
         L     R1,=A(XOGENFIL+XOGET+XIO2)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
*                                                                               
         L     R3,AIO2                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',(R3)),0                  
         L     R3,12(R1)                                                        
         USING NAMELD,R3                                                        
         LLC   RF,NAMLN                                                         
         AHI   RF,-(NAMLN1Q+1)                                                  
         EX    RF,*+8                                                           
         B     EXITOK                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         B     EXITOK                                                           
         DROP  T,R3                                                             
         EJECT                                                                  
***********************************************************************         
* FORMAT OBJECT                                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FMCDTA   LA    RF,FMCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FMCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFMC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFFMC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFMC)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFFMC)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOFMC)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FORMAT CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISFMC   MVC   FVIFLD(L'BANKFORM),BANKFORM                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY A FORMAT CODE FIELD FILTER                                  *         
***********************************************************************         
         SPACE 1                                                                
DISFFMC  MVC   FVIFLD(L'BANKFORM),FLTIFLD                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE FORMAT CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALFMC   MVC   BANKFORM,FVIFLD                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE FORMAT CODE FIELD FILTER                                   *         
***********************************************************************         
         SPACE 1                                                                
VALFFMC  MVC   BANKFORM,FVIFLD                                                  
         MVC   FLTIFLD(L'BANKFORM),FVIFLD                                       
         EJECT                                                                  
***********************************************************************         
* FILTER ON FORMAT CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DOFMC    CLC   BANKFORM,FVIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FORMAT NAME                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FMNDTA   LA    RF,FMNTB                                                         
         B     ITER                                                             
*                                                                               
FMNTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFMN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFMN)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A FORMAT NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISFMN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING NAMELD,R3                                                        
         LLC   RF,NAMLN                                                         
         AHI   RF,-(NAMLN1Q+1)                                                  
         EX    RF,*+8                                                           
         B     EXITOK                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A FORMAT NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALFMN   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('NAMELQ',(R2)),0                  
         CLI   FVILEN,0            WAS A NAME INPUT?                            
         BE    EXITOK              NO                                           
*                                                                               
         XC    BOWORK2,BOWORK2                                                  
         LA    R3,BOWORK2          BUILD A NAMEL                                
         USING NAMELD,R3                                                        
         MVI   NAMEL,NAMELQ                                                     
         LLC   RE,FVILEN                                                        
         AHI   RE,NAMLN1Q                                                       
         STC   RE,NAMLN                                                         
         MVC   NAMEREC,FVIFLD                                                   
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),(R2),NAMELD                        
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DATA OBJECT FOR FORMAT NAME (DISPLAY ONLY)                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FMNDTA1  LA    RF,FMNTB1                                                        
         B     ITER                                                             
*                                                                               
FMNTB1   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFMN1)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A FORMAT NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKRECD,R2                                                      
DISFMN1  MVC   IOKEY(L'BANKEY),BANKEY                                           
         MVI   IOKEY+BANKREC-BANKRECD,0                                         
         MVI   IOKEY+BANKLIN#-BANKRECD,0                                        
         L     R1,=A(XOGENDIR+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
         L     R1,=A(XOGENFIL+XOGET+XIO1)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
         L     R2,AIO1                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING NAMELD,R3                                                        
         LLC   RF,NAMLN                                                         
         AHI   RF,-(NAMLN1Q+1)                                                  
         EX    RF,*+8                                                           
         B     EXITOK                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD CODE NAME                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RCNDTA   LA    RF,RCNTB                                                         
         B     ITER                                                             
*                                                                               
RCNTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISRCN)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD CODE NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING FRCTABD,RF                                                       
DISRCN   LA    RF,FRCTAB                                                        
*                                                                               
DRCN02   CLI   FRCNAM,X'FF'                                                     
         BE    EXITOK                                                           
         CLC   FRCEQU,BANKREC                                                   
         BE    *+12                                                             
         AHI   RF,FRCTABLQ                                                      
         B     DRCN02                                                           
*                                                                               
         MVC   FVIFLD(L'FRCDSC),FRCDSC                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*ISRCN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
*        L     R3,12(R1)                                                        
*        USING BFRELD,R3                                                        
*        LA    RF,FRCTAB                                                        
*        USING FRCTABD,RF                                                       
*RCN02   CLI   FRCNAM,X'FF'                                                     
*        BE    EXITOK                                                           
*        CLC   FRCEQU,BFRREC                                                    
*        BE    *+12                                                             
*        AHI   RF,FRCTABLQ                                                      
*        B     DRCN02                                                           
*                                                                               
*        MVC   FVIFLD(L'FRCDSC),FRCDSC                                          
*        B     EXITOK                                                           
*        DROP  R3,RF                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RCCDTA   LA    RF,RCCTB                                                         
         B     ITER                                                             
*                                                                               
RCCTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISRCC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRCC)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING FRCTABD,RF                                                       
DISRCC   LA    RF,FRCTAB                                                        
*                                                                               
DRCC02   CLI   FRCNAM,X'FF'                                                     
         BE    EXITOK                                                           
         CLC   FRCEQU,BANKREC                                                   
         BE    *+12                                                             
         AHI   RF,FRCTABLQ                                                      
         B     DRCC02                                                           
*                                                                               
         MVC   FVIFLD(L'FRCNAM),FRCNAM                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*ISRCC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
*        L     R3,12(R1)                                                        
*        USING BFRELD,R3                                                        
*        LA    RF,FRCTAB                                                        
*        USING FRCTABD,RF                                                       
*RCC02   CLI   FRCNAM,X'FF'                                                     
*        BE    EXITOK                                                           
*        CLC   FRCEQU,BFRREC                                                    
*        BE    *+12                                                             
*        AHI   RF,FRCTABLQ                                                      
*        B     DRCC02                                                           
*                                                                               
*        MVC   FVIFLD(L'FRCNAM),FRCNAM                                          
*        B     EXITOK                                                           
*        DROP  RF,R3                                                            
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A RECORD CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING FRCTABD,RF                                                       
VALRCC   LA    RF,FRCTAB                                                        
*                                                                               
VRCC02   CLI   FRCNAM,X'FF'                                                     
         BE    EXITNV                                                           
         CLC   FRCNAM,FVIFLD                                                    
         BE    *+12                                                             
         AHI   RF,FRCTABLQ                                                      
         B     VRCC02                                                           
*                                                                               
         MVC   BANKREC,FRCEQU                                                   
*                                                                               
         MVI   KEYCODE,0                                                        
         CLC   FRCNAM,=CL8'BASE'                                                
         BNE   *+8                                                              
         MVI   KEYCODE,C'A'                                                     
*                                                                               
         B     EXITOK                                                           
         DROP  RF                                                               
*ALRCC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
*        L     R3,12(R1)                                                        
*        USING BFRELD,R3                                                        
*        LA    RF,FRCTAB                                                        
*        USING FRCTABD,RF                                                       
*RCC02   CLI   FRCNAM,X'FF'                                                     
*        BE    EXITNV                                                           
*        CLC   FRCNAM,FVIFLD                                                    
*        BE    *+12                                                             
*        AHI   RF,FRCTABLQ                                                      
*        B     VRCC02                                                           
*                                                                               
*        MVC   BFRREC,FRCEQU                                                    
*        B     EXITOK                                                           
*        DROP  R3,RF                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LINE NUMBER                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LINDTA   LA    RF,LINTB                                                         
         B     ITER                                                             
*                                                                               
LINTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISLIN)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFLIN)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLIN)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFLIN)                               
*        DC    AL1(DFDO),AL1(0,0,0),AL4(DOLIN)                                  
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A LINE NUMBER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISLIN   LLC   R0,BANKLIN#                                                      
         CURED (R0),(5,FVIFLD),0,ALIGN=LEFT,DMCB=BODMCB                         
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A LINE NUMBER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISFLIN  LLC   R0,FLTIFLD                                                       
         CURED (R0),(5,FVIFLD),0,ALIGN=LEFT,DMCB=BODMCB                         
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A LINE NUMBER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALLIN   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM         IF IT IS A NUMBER                          
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL          THEN IT GETS SET IN BCFULL                 
         CLI   KEYCODE,C'A'                                                     
         BNE   VLIN10                                                           
         SR    RF,RF                 FORCE 0 IF BASE                            
*        CHI   RF,0                                                             
*        BNE   EXITNV                                                           
         B     VLIN20                                                           
VLIN10   CHI   RF,255                                                           
         BH    EXITTOOM                                                         
         CHI   RF,0                                                             
         BE    EXITNV                                                           
VLIN20   STC   RF,BANKLIN#                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A LINE NUMBER FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VALFLIN  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM         IF IT IS A NUMBER                          
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL          THEN IT GETS SET IN BCFULL                 
         CHI   RF,255                                                           
         BH    EXITTOOM                                                         
         STC   RF,BANKLIN#                                                      
         STC   RF,FLTIFLD                                                       
         B     EXITOK                                                           
***********************************************************************         
* FILTER ON LINE NUMBER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DOLIN    CLC   BANKLIN#,FVIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD SIZE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RSZDTA   LA    RF,RSZTB                                                         
         B     ITER                                                             
*                                                                               
RSZTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISRSZ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRSZ)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A RECORD SIZE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISRSZ   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         XR    R0,R0                                                            
         ICM   R0,3,BFIRLEN                                                     
         BZ    EXITOK                                                           
         CURED (R0),(5,FVIFLD),0,ALIGN=LEFT,ZERO=NOBLANK,DMCB=BODMCB            
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A RECORD SIZE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALRSZ   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         TM    FVIIND,FVINUM         IF IT IS A NUMBER                          
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL          THEN IT GETS SET IN BCFULL                 
         TMH   RF,X'FFFF'                                                       
         BNZ   EXITTOOM                                                         
         STCM  RF,3,BFIRLEN                                                     
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD SIZE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RSZDTA1  LA    RF,RSZTB1                                                        
         B     ITER                                                             
*                                                                               
RSZTB1   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRSZ1)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A RECORD SIZE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKRECD,R2                                                      
DISRSZ1  MVC   IOKEY(L'BANKEY),BANKEY                                           
         MVI   IOKEY+BANKREC-BANKRECD,0                                         
         MVI   IOKEY+BANKLIN#-BANKRECD,0                                        
         L     R1,=A(XOGENDIR+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
         L     R1,=A(XOGENFIL+XOGET+XIO1)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
         L     R2,AIO1                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         XR    R0,R0                                                            
         ICM   R0,3,BFIRLEN                                                     
         BZ    EXITOK                                                           
         CURED (R0),(5,FVIFLD),0,ALIGN=LEFT,ZERO=NOBLANK,DMCB=BODMCB            
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BLOCK SIZE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BSZDTA1  LA    RF,BSZTB1                                                        
         B     ITER                                                             
*                                                                               
BSZTB1   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBSZ1)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A BLOCK SIZE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKRECD,R2                                                      
DISBSZ1  MVC   IOKEY(L'BANKEY),BANKEY                                           
         MVI   IOKEY+BANKREC-BANKRECD,0                                         
         MVI   IOKEY+BANKLIN#-BANKRECD,0                                        
         L     R1,=A(XOGENDIR+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
         L     R1,=A(XOGENFIL+XOGET+XIO1)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
         L     R2,AIO1                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         XR    R0,R0                                                            
         ICM   R0,3,BFIBSZE                                                     
         BZ    EXITOK                                                           
         CURED (R0),(5,FVIFLD),0,ALIGN=LEFT,ZERO=NOBLANK,DMCB=BODMCB            
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BLOCK SIZE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BSZDTA   LA    RF,BSZTB                                                         
         B     ITER                                                             
*                                                                               
BSZTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISBSZ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBSZ)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A BLOCK SIZE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISBSZ   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         XR    R0,R0                                                            
         ICM   R0,3,BFIBSZE                                                     
         BZ    EXITOK                                                           
         CURED (R0),(5,FVIFLD),0,ALIGN=LEFT,ZERO=NOBLANK,DMCB=BODMCB            
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A BLOCK SIZE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALBSZ   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         TM    FVIIND,FVINUM         IF IT IS A NUMBER                          
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL          THEN IT GETS SET IN BCFULL                 
         TMH   RF,X'FFFF'                                                       
         BNZ   EXITTOOM                                                         
         STCM  RF,3,BFIBSZE                                                     
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR VARIABLE RECORD FORMAT                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
VRFDTA   LA    RF,VRFTB                                                         
         B     ITER                                                             
*                                                                               
VRFTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISVRF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVRF)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A VARIABLE RECORD FORMAT OPTION                             *         
***********************************************************************         
         SPACE 1                                                                
DISVRF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFISTAT,BFIVRLN                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A VARIABLE RECORD FORMAT OPTION                            *         
***********************************************************************         
         SPACE 1                                                                
VALVRF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         CLI   FVIFLD,C'N'                                                      
         BNE   *+12                                                             
         NI    BFISTAT,X'FF'-BFIVRLN                                            
         B     EXITOK                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    BFISTAT,BFIVRLN                                                  
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FORMAT BREAKS UP ADDRESS                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ADRDTA   LA    RF,ADRTB                                                         
         B     ITER                                                             
*                                                                               
ADRTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISADR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADR)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A FORMAT BREAKS UP ADDRESS OPTION                           *         
***********************************************************************         
         SPACE 1                                                                
DISADR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFISTAT,BFIADDRB                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A FORMAT BREAKS UP ADDRESS OPTION                          *         
***********************************************************************         
         SPACE 1                                                                
VALADR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         CLI   FVIFLD,C'N'                                                      
         BNE   *+12                                                             
         NI    BFISTAT,X'FF'-BFIADDRB                                           
         B     EXITOK                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    BFISTAT,BFIADDRB                                                 
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CANADA AS FOREIGN                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CANDTA   LA    RF,CANTB                                                         
         B     ITER                                                             
*                                                                               
CANTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCAN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCAN)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A CANADA AS FOREIGN OPTION                                  *         
***********************************************************************         
         SPACE 1                                                                
DISCAN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFISTAT,BFICANF                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A CANADA AS FOREIGN OPTION                                 *         
***********************************************************************         
         SPACE 1                                                                
VALCAN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         CLI   FVIFLD,C'N'                                                      
         BNE   *+12                                                             
         NI    BFISTAT,X'FF'-BFICANF                                            
         B     EXITOK                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    BFISTAT,BFICANF                                                  
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR REMOVE * FROM NARRATIVE                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RNRDTA   LA    RF,RNRTB                                                         
         B     ITER                                                             
*                                                                               
RNRTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISRNR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRNR)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A REMOVE * FROM NARRATIVE OPTION                            *         
***********************************************************************         
         SPACE 1                                                                
DISRNR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFISTAT,BFIREMNR                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A REMOVE * FROM NARRATIVE OPTION                           *         
***********************************************************************         
         SPACE 1                                                                
VALRNR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         CLI   FVIFLD,C'N'                                                      
         BNE   *+12                                                             
         NI    BFISTAT,X'FF'-BFIREMNR                                           
         B     EXITOK                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    BFISTAT,BFIREMNR                                                 
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OMIT CITY DUPLICATION                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CTYDTA   LA    RF,CTYTB                                                         
         B     ITER                                                             
*                                                                               
CTYTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCTY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCTY)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A OMIT CITY DUPLICATION OPTION                              *         
***********************************************************************         
         SPACE 1                                                                
DISCTY   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFISTAT,BFICTYDP                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A OMIT CITY DUPLICATION OPTION                             *         
***********************************************************************         
         SPACE 1                                                                
VALCTY   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         CLI   FVIFLD,C'N'                                                      
         BNE   *+12                                                             
         NI    BFISTAT,X'FF'-BFICTYDP                                           
         B     EXITOK                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    BFISTAT,BFICTYDP                                                 
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
*MN                                                                             
***********************************************************************         
* DATA OBJECT FOR CSV FILE - STRIP COMMAS FROM TEXT                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CSVCMA   LA    RF,CSVTB                                                         
         B     ITER                                                             
*                                                                               
CSVTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCSV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCSV)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A CSV FILE - STRIP COMMAS FROM TEXT                         *         
***********************************************************************         
         SPACE 1                                                                
DISCSV   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFISTAT,BFICSVCM                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A CSV FILE - STRIP COMMAS FROM TEXT                        *         
***********************************************************************         
         SPACE 1                                                                
VALCSV   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFIELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFIELD,R3                                                        
         CLI   FVIFLD,C'N'                                                      
         BNE   *+12                                                             
         NI    BFISTAT,X'FF'-BFICSVCM                                           
         B     EXITOK                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV                                                           
         OI    BFISTAT,BFICSVCM                                                 
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
*MN                                                                             
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - HDR/TRL BY ACCOUNT (BFRACC)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
HTADTA   LA    RF,HTATB                                                         
         B     ITER                                                             
*                                                                               
HTATB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISHTA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHTA)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - HDR/TRL BY ACCOUNT                  *         
***********************************************************************         
         SPACE 1                                                                
DISHTA   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTAT,BFRACC                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - HDR/TRL BY ACCOUNT                 *         
***********************************************************************         
         SPACE 1                                                                
VALHTA   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTAT,255-BFRACC                                              
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VHTA04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VHTA04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VHTA06                                                           
*                                                                               
VHTA04   OI    BFRRSTAT,BFRACC                                                  
         B     EXITOK                                                           
*                                                                               
VHTA06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - HDR/TRL BY TOTAL   (BFRTOT)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
HTTDTA   LA    RF,HTTTB                                                         
         B     ITER                                                             
*                                                                               
HTTTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISHTT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHTT)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - HDR/TRL BY TOTAL                    *         
***********************************************************************         
         SPACE 1                                                                
DISHTT   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTAT,BFRTOT                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - HDR/TRL BY TOTAL                   *         
***********************************************************************         
         SPACE 1                                                                
VALHTT   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTAT,255-BFRTOT                                              
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VHTT04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VHTT04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VHTT06                                                           
*                                                                               
VHTT04   OI    BFRRSTAT,BFRTOT                                                  
         B     EXITOK                                                           
*                                                                               
VHTT06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - HDR/TRL INCLUDE  (BFRHTCNT)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
HTCDTA   LA    RF,HTCTB                                                         
         B     ITER                                                             
*                                                                               
HTCTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISHTC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHTC)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - HDR/TRL INCLUDE IN COUNT            *         
***********************************************************************         
         SPACE 1                                                                
DISHTC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTAT,BFRHTCNT                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - HDR/TRL INCLUDE IN COUNT           *         
***********************************************************************         
         SPACE 1                                                                
VALHTC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTAT,255-BFRHTCNT                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VHTC04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VHTC04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VHTC06                                                           
*                                                                               
VHTC04   OI    BFRRSTAT,BFRHTCNT                                                
         B     EXITOK                                                           
*                                                                               
VHTC06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - INCL VOIDS IN TOT (BFRIVCT)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
INVDTA   LA    RF,INVTB                                                         
         B     ITER                                                             
*                                                                               
INVTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISINV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALINV)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - INCLUDE VOIDS IN TOTAL COUNT        *         
***********************************************************************         
         SPACE 1                                                                
DISINV   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTAT,BFRIVCT                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - INCLUDE VOIDS IN TOTAL COUNT       *         
***********************************************************************         
         SPACE 1                                                                
VALINV   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTAT,255-BFRIVCT                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VINV04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VINV04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VINV06                                                           
*                                                                               
VINV04   OI    BFRRSTAT,BFRIVCT                                                 
         B     EXITOK                                                           
*                                                                               
VINV06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - HDR/TRL REQUIRED  (BFRREQ)    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
HTRDTA   LA    RF,HTRTB                                                         
         B     ITER                                                             
*                                                                               
HTRTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISHTR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHTR)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - HDR/TRL REQUIRED FOR DATA           *         
***********************************************************************         
         SPACE 1                                                                
DISHTR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTAT,BFRREQ                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - HDR/TRL REQUIRED FOR DATA          *         
***********************************************************************         
         SPACE 1                                                                
VALHTR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTAT,255-BFRREQ                                              
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VHTR04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VHTR04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VHTR06                                                           
*                                                                               
VHTR04   OI    BFRRSTAT,BFRREQ                                                  
         B     EXITOK                                                           
*                                                                               
VHTR06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - ALWAYS PASS TOTAL (BFRATOT)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TOTDTA   LA    RF,TOTTB                                                         
         B     ITER                                                             
*                                                                               
TOTTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISTOT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTOT)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - ALWAYS PASS TOTAL TRAILER           *         
***********************************************************************         
         SPACE 1                                                                
DISTOT   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTAT,BFRATOT                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - ALWAYS PASS TOTAL TRAILER          *         
***********************************************************************         
         SPACE 1                                                                
VALTOT   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTAT,255-BFRATOT                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VTOT04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VTOT04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VTOT06                                                           
*                                                                               
VTOT04   OI    BFRRSTAT,BFRATOT                                                 
         B     EXITOK                                                           
*                                                                               
VTOT06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - EXCLUDE VOIDS     (BFREXVD)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
EXVDTA   LA    RF,EXVTB                                                         
         B     ITER                                                             
*                                                                               
EXVTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISEXV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEXV)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - EXCLUDE VOIDS IN TOTAL CNT/AMT      *         
***********************************************************************         
         SPACE 1                                                                
DISEXV   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTAT,BFREXVD                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - EXCLUDE VOIDS IN TOTAL CNT/AMT     *         
***********************************************************************         
         SPACE 1                                                                
VALEXV   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTAT,255-BFREXVD                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VEXV04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VEXV04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VEXV06                                                           
*                                                                               
VEXV04   OI    BFRRSTAT,BFREXVD                                                 
         B     EXITOK                                                           
*                                                                               
VEXV06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FOREIGN DEPENDANT             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FRDDTA   LA    RF,FRDTB                                                         
         B     ITER                                                             
*                                                                               
FRDTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFRD)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FOREIGN DEPENDANT                   *         
***********************************************************************         
         SPACE 1                                                                
DISFRD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTAT,BFRFRDEP                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FOREIGN DEPENDANT                  *         
***********************************************************************         
         SPACE 1                                                                
VALFRD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTAT,255-BFRFRDEP                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VFRD04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFRD04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFRD06                                                           
*                                                                               
VFRD04   OI    BFRRSTAT,BFRFRDEP                                                
         B     EXITOK                                                           
*                                                                               
VFRD06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FOREIGN ONLY COUNT (BFRFRGN)  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FRGDTA   LA    RF,FRGTB                                                         
         B     ITER                                                             
*                                                                               
FRGTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFRG)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FOREIGN ONLY COUNT                  *         
***********************************************************************         
         SPACE 1                                                                
DISFRG   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTA2,BFRFRGN                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FOREIGN ONLY COUNT                 *         
***********************************************************************         
         SPACE 1                                                                
VALFRG   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTA2,255-BFRFRGN                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VFRG04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFRG04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFRG06                                                           
*                                                                               
VFRG04   OI    BFRRSTA2,BFRFRGN                                                 
         B     EXITOK                                                           
*                                                                               
VFRG06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - ADD VOIDS TO TOTAL (BFRADV)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ADVDTA   LA    RF,ADVTB                                                         
         B     ITER                                                             
*                                                                               
ADVTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISADV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADV)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - ADD VOIDS TO TOTAL                  *         
***********************************************************************         
         SPACE 1                                                                
DISADV   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTA2,BFRADV                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - ADD VOIDS TO TOTAL                 *         
***********************************************************************         
         SPACE 1                                                                
VALADV   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTA2,255-BFRADV                                              
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VADV04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VADV04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VADV06                                                           
*                                                                               
VADV04   OI    BFRRSTA2,BFRADV                                                  
         B     EXITOK                                                           
*                                                                               
VADV06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - US ONLY           (BFRUSON)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
USODTA   LA    RF,USOTB                                                         
         B     ITER                                                             
*                                                                               
USOTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISUSO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALUSO)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - US ONLY                             *         
***********************************************************************         
         SPACE 1                                                                
DISUSO   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTA2,BFRUSON                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - US ONLY                            *         
***********************************************************************         
         SPACE 1                                                                
VALUSO   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTA2,255-BFRUSON                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VUSO04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VUSO04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VUSO06                                                           
*                                                                               
VUSO04   OI    BFRRSTA2,BFRUSON                                                 
         B     EXITOK                                                           
*                                                                               
VUSO06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FIELD DEPENDANT   (BFRFDEP)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FDEDTA   LA    RF,FDETB                                                         
         B     ITER                                                             
*                                                                               
FDETB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFDE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFDE)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FIELD DEPENDANT                     *         
***********************************************************************         
         SPACE 1                                                                
DISFDE   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTA2,BFRFDEP                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FIELD DEPENDANT                    *         
***********************************************************************         
         SPACE 1                                                                
VALFDE   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTA2,255-BFRFDEP                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VFDE04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFDE04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFDE06                                                           
*                                                                               
VFDE04   OI    BFRRSTA2,BFRFDEP                                                 
         B     EXITOK                                                           
*                                                                               
VFDE06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - BLD VAR LEN FLD   (BFRBVLF)   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BVLDTA   LA    RF,BVLTB                                                         
         B     ITER                                                             
*                                                                               
BVLTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISBVL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBVL)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - BUILD VAR LENGTH FIELDS             *         
***********************************************************************         
         SPACE 1                                                                
DISBVL   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTA2,BFRBVLF                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - BUILD VAR LENGTH FIELDS            *         
***********************************************************************         
         SPACE 1                                                                
VALBVL   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTA2,255-BFRBVLF                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VBVL04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VBVL04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VBVL06                                                           
*                                                                               
VBVL04   OI    BFRRSTA2,BFRBVLF                                                 
         B     EXITOK                                                           
*                                                                               
VBVL06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FLD DELIM - COMMA (BFRDCOMA)  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
VLDDTA   LA    RF,VLDTB                                                         
         B     ITER                                                             
*                                                                               
VLDTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISVLD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVLD)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - VAR LEN FIELD DELIMITER - COMMA     *         
***********************************************************************         
         SPACE 1                                                                
DISVLD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTA2,BFRDCOMA                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - VAR LEN FIELD DELIMITER - COMMA    *         
***********************************************************************         
         SPACE 1                                                                
VALVLD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTA2,255-BFRDCOMA                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VVLD04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VVLD04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VVLD06                                                           
*                                                                               
VVLD04   OI    BFRRSTA2,BFRDCOMA                                                
         B     EXITOK                                                           
*                                                                               
VVLD06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FLD SUR CHAR - QTS (BFRSCQUO) *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FSCDTA   LA    RF,FSCTB                                                         
         B     ITER                                                             
*                                                                               
FSCTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFSC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFSC)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FIELD SURROUNDING CHAR - QUOTES     *         
***********************************************************************         
         SPACE 1                                                                
DISFSC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFRRSTA2,BFRSCQUO                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FIELD SURROUNDING CHAR - QUOTES    *         
***********************************************************************         
         SPACE 1                                                                
VALFSC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFRELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFRELD,R3                                                        
         NI    BFRRSTA2,255-BFRSCQUO                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VFSC04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFSC04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFSC06                                                           
*                                                                               
VFSC04   OI    BFRRSTA2,BFRSCQUO                                                
         B     EXITOK                                                           
*                                                                               
VFSC06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING BANKRECD,R2                                                      
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
         EJECT                                                                  
*                                                                               
LISTABL  DS    0A                                                               
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'BANKEY),THIS.BANKRECD                                    
         L     R1,=A(XOGENDIR+XOHIGH+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    NLST02                                                           
         B     EXITL                                                            
         SPACE 1                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
X        USING BANKRECD,IOKEY                                                   
NLST     L     R1,=A(XOGENDIR+XOSEQ+XIO1)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
*                                                                               
NLST02   CLI   X.BANKTYP,BANKTYPQ                                               
         BNE   EXITL                                                            
         CLI   X.BANKSUB,BANKSFQ                                                
         BNE   EXITL                                                            
         CLI   X.BANKCOL#,0                                                     
         BE    NLST04                                                           
         LLC   RF,X.BANKLIN#                                                    
         CHI   RF,255              STOP LOOP IF 255 LINES IN RECORD             
         BNE   *+6                                                              
         DC    H'0'                                                             
         AHI   RF,1                                                             
         STC   RF,X.BANKLIN#                                                    
         MVI   X.BANKCOL#,0                                                     
         L     R1,=A(XOGENDIR+XOHIGH+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         B     NLST02                                                           
*                                                                               
NLST04   MVC   THIS.BANKRECD(BANKLEN),IOKEY   WE WANT THIS KEY                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
EOF      EQU   X'FF'                                                            
*                                                                               
FRCTAB   DS    0F                                                               
         DC    CL8'BASE    ',AL1(00,0,0,0)                                      
         DC    CL32'BASE RECORD                    '                            
         DC    CL8'THDR    ',AL1(01,0,0,0)                                      
         DC    CL32'TRANSMISSION HEADER            '                            
         DC    CL8'HDR     ',AL1(02,0,0,0)                                      
         DC    CL32'FILE / CHECK HEADER            '                            
         DC    CL8'DTL     ',AL1(03,0,0,0)                                      
         DC    CL32'DETAIL                         '                            
         DC    CL8'TRL     ',AL1(04,0,0,0)                                      
         DC    CL32'TRAILER                        '                            
         DC    CL8'TRL2    ',AL1(05,0,0,0)                                      
         DC    CL32'TOTAL TRAILER                  '                            
         DC    CL8'CTRL    ',AL1(06,0,0,0)                                      
         DC    CL32'CHECK TRAILER                  '                            
*MN SPEC-41133                                                                  
         DC    CL8'TRLB    ',AL1(07,0,0,0)                                      
         DC    CL32'BLOCK TRAILER                  '                            
*MN SPEC-41133                                                                  
         DC    X'FF'                                                            
*                                                                               
FRCTABD  DSECT                                                                  
FRCNAM   DS    CL8                                                              
FRCEQU   DS    X                                                                
         DS    XL3                                                              
FRCDSC   DS    CL32                                                             
FRCTABLQ EQU   *-FRCTABD                                                        
*                                                                               
FIL2A    CSECT                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
FFFFFFFF EQU   X'FFFFFFFF'                                                      
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
DCLIST   DCDDL CT#YES,4,L                                                       
         DCDDL GE#NO,4,L                                                        
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* CTFILWORK                                                                     
* CTDDEQUS                                                                      
* CTFILEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWAPPLIC                                                         
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
DUB      DS    D                                                                
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
KEYCODE  DS    X                                                                
*                                                                               
DSLISTU  DS    0D                                                               
UE@YES   DS    CL4                                                              
UE@NO    DS    CL4                                                              
*                                                                               
DSLISTL  DS    0D                                                               
LC@YES   DS    CL4                                                              
LC@NO    DS    CL4                                                              
         SPACE 2                                                                
***********************************************************************         
* INCLUDES                                                            *         
***********************************************************************         
         SPACE 2                                                                
*        GEGENBNK                                                               
         PRINT OFF                                                              
       ++INCLUDE GEGENBNK                                                       
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'079CTFIL2A   01/14/20'                                      
         END                                                                    
