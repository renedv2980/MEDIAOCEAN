*          DATA SET CTFIL2B    AT LEVEL 040 AS OF 09/03/20                      
*&&      SET   NOP=N                                                            
*PHASE TA132BA                                                                  
         TITLE 'FORMAT RECORD FIELD DEFINITIONS'                                
FIL2B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL2B*,R6,R7,R5,RR=RE                                        
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
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
         B     EXIT                                                             
*                                                                               
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
                                                                                
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITTOOM MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               EXIT WITH TOO MUCH DATA SET                  
EXITINVD MVC   FVMSGNO,=AL2(FVFINVDT)                                           
         B     EXITL               EXIT WITH INVALID DATE                       
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
                                                                                
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
                                                                                
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
                                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
                                                                                
INIT     OI    GSINDSL1,GSIXKEY                                                 
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
                                                                                
SCRN     LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
SCRTABL  DC    AL1(SSET),AL1(0,0,0),AL4(SCRSET)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* SCRSET - SET MAINT CODE BASED ON INPUT TYPE                         *         
***********************************************************************         
                                                                                
SCRSET   MVC   GSSMCODE,KEYCODE                                                 
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
                                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING BANKRECD,R2                                                      
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
                                                                                
KEYFRST  L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
                                                                                
KFKVAL   XC    BANKEY,BANKEY                                                    
         MVI   BANKTYP,BANKTYPQ    B - BANK REC                                 
         MVI   BANKSUB,BANKSFQ     F - FORMAT                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
                                                                                
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
                                                                                
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING BANKRECD,R2                                                      
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
                                                                                
*                                                                               
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
                                                                                
         DS    0F                                                               
KNOWTAB  DC    AL2(600),AL4(BKCDTA)    BANK CODE   (KEY)                        
         DC    AL2(601),AL4(BKNDTA)    BANK NAME                                
         DC    AL2(602),AL4(FMCDTA)    FORMAT CODE (KEY)                        
         DC    AL2(603),AL4(FMNDTA)    FORMAT NAME                              
         DC    AL2(614),AL4(RCCDTA)    RECORD CODE (KEY)                        
         DC    AL2(616),AL4(RCNDTA)    RECORD NAME                              
         DC    AL2(611),AL4(LINDTA)    LINE NUMBER                              
         DC    AL2(615),AL4(FLDDTA)    FIELD NUMBER                             
*                                                                               
         DC    AL2(610),AL4(KWDDTA)    KEYWORD                                  
         DC    AL2(630),AL4(DSPDTA)    DISPLACEMENT                             
         DC    AL2(643),AL4(FLNDTA)    FIELD LENGTH                             
*                                                                               
         DC    AL2(617),AL4(SPCPDTA)   SPACE PADDED FIELD                       
         DC    AL2(618),AL4(LJFDTA)    LEFT JUSTIFIED FIELD                     
         DC    AL2(619),AL4(DPCDTA)    COUNTRY DEPENDENT                        
         DC    AL2(620),AL4(VDPDTA)    VOID DEPENDENT                           
         DC    AL2(621),AL4(UPFDTA)    USES PREFIX                              
         DC    AL2(622),AL4(DPFDTA)    USES DEFAULT PREFIX                      
         DC    AL2(623),AL4(IVDDTA)    FIELD IS A VOID                          
         DC    AL2(632),AL4(FDSDTA)    FIELD FOR DEST NOT SOURCE                
*                                                                               
         DC    AL2(624),AL4(VNMDTA)    VOID IF NET < $0                         
         DC    AL2(625),AL4(SZDDTA)    SUPPRESS ZERO DOLLARS                    
         DC    AL2(626),AL4(CANDTA)    FIELD IS CANADIAN                        
         DC    AL2(633),AL4(MEYDTA)    TRAILING MINUS                           
         DC    AL2(634),AL4(FEYDTA)    LEADING MINUS                            
         DC    AL2(635),AL4(LDPDTA)    FOR FUTURE DEVELOPMENT                   
         DC    AL2(636),AL4(DP2DTA)    2DP AMOUNTS                              
         DC    AL2(637),AL4(FPPDTA)    FIELD DEPENDENT                          
*                                                                               
         DC    AL2(638),AL4(NZRDTA)    VOID IF BKNET < 0                        
         DC    AL2(639),AL4(CCRDTA)    CANADIAN CURRENCY                        
         DC    AL2(640),AL4(MIADTA)    MARK INVALID ADDRESS                     
         DC    AL2(641),AL4(CTDDTA)    ADD COMMAS TO DOLLARS                    
         DC    AL2(644),AL4(OVFDTA)    FIELD MAY OVERFLOW                       
         DC    AL2(604),AL4(CMADTA)    NO DELIMETER , FOR THIS FIELD            
         DC    AL2(605),AL4(LENDTA)    USE FLDLEN, NO SPACES ADJUSTMENT         
*MN      DC    AL2(606),AL4(FMTDTA)    USES ADDITIONAL FORMAT                   
*                                                                               
         DC    AL2(628),AL4(DFMDTA)    DATE FORMAT                              
         DC    AL2(642),AL4(XTRDTA)    COMPANY LIST                             
         DC    AL2(631),AL4(SRCDTA)    SOURCE DATA                              
*                                                                               
         DC    AL2(645),AL4(DPNDTA)    DEPENDENCIES                             
         DC    AL2(646),AL4(ADDDTA)    ADDRESS LINE                             
         DC    AL2(647),AL4(DFODTA)    OVERRIDE                                 
*MN SPEC-36440                                                                  
         DC    AL2(648),AL4(XMLDTA)    XML TAG FIELD                            
*MN SPEC-36440                                                                  
*MN                                                                             
* THIS IS READY TO USE - JUST NEEDS FIELD ADDED TO SCREEN                       
         DC    AL2(629),AL4(IDSDTA)    INPUT DISPLACEMENT                       
*MN                                                                             
         DC    AL1(FF)                                                          
                                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL2B    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL                 TABLE OF KNOWN INVOKERS                
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFD)                                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFD)                                    
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* FIRST TIME FOR A DATA OBJECT                                        *         
***********************************************************************         
                                                                                
DFD      GOTO1 AGETEL,BOPARM,('BFLELQ',BANKRECD),0                              
         BE    EXITOK                                                           
*                                                                               
         XC    BOELEM,BOELEM     IF IT ISNT THERE, ADD IT                       
         LA    R4,BOELEM                                                        
         USING BFLELD,R4                                                        
         MVI   BFLEL,BFLELQ                                                     
         MVI   BFLLN,BFLLNQ                                                     
         GOTO1 AADDEL,BOPARM,('BFLELQ',BANKRECD),0                              
         B     DFD                                                              
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BANK CODE OBJECT                                                    *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
                                                                                
BKCDTA   LA    RF,BKCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BKCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBKC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFBKC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBKC)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFBKC)                               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A BANK CODE FIELD                                           *         
***********************************************************************         
                                                                                
DISBKC   MVC   FVIFLD(L'BANKCDE),BANKCDE                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY A BANK CODE FIELD FILTER                                    *         
***********************************************************************         
                                                                                
DISFBKC  MVC   FVIFLD(L'BANKCDE),FLTIFLD                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE BANK CODE FIELD                                            *         
***********************************************************************         
                                                                                
VALBKC   MVC   BANKCDE,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE BANK CODE FIELD                                            *         
***********************************************************************         
                                                                                
VALFBKC  MVC   BANKCDE,FVIFLD                                                   
         MVC   FLTIFLD(L'BANKCDE),FVIFLD                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BANK NAME OBJECT                                                    *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
                                                                                
BKNDTA   LA    RF,BKNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BKNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBKN)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A BANK NAME FIELD                                           *         
***********************************************************************         
                                                                                
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
                                                                                
FMCDTA   LA    RF,FMCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FMCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFMC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFFMC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFMC)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFFMC)                               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A FORMAT CODE FIELD                                         *         
***********************************************************************         
                                                                                
DISFMC   MVC   FVIFLD(L'BANKFORM),BANKFORM                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY A FORMAT CODE FIELD FILTER                                  *         
***********************************************************************         
                                                                                
DISFFMC  MVC   FVIFLD(L'BANKFORM),FLTIFLD                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE FORMAT CODE FIELD                                          *         
***********************************************************************         
                                                                                
VALFMC   MVC   BANKFORM,FVIFLD                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE FORMAT CODE FIELD FILTER                                   *         
***********************************************************************         
                                                                                
VALFFMC  MVC   BANKFORM,FVIFLD                                                  
         MVC   FLTIFLD(L'BANKFORM),FVIFLD                                       
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FORMAT NAME                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
FMNDTA   LA    RF,FMNTB                                                         
         B     ITER                                                             
*                                                                               
FMNTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFMN)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A FORMAT NAME FIELD                                         *         
***********************************************************************         
                                                                                
T        USING BANKRECD,R3                                                      
DISFMN   LA    R3,IOKEY                                                         
         XC    T.BANKEY,T.BANKEY                                                
         MVI   T.BANKTYP,BANKTYPQ                                               
         MVI   T.BANKSUB,BANKSGQ                                                
         MVC   T.BANKCDE,BANKCDE   MOVE IN THE BANK CODE                        
         MVC   T.BANKFORM,BANKFORM MOVE IN THE FORMAT CODE                      
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
* DATA OBJECT FOR SOURCE                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
SRCDTA   LA    RF,SRCTB                                                         
         B     ITER                                                             
*                                                                               
SRCTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISSRC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSRC)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY SOURCE                                                      *         
***********************************************************************         
                                                                                
DISSRC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
*MN SPEC-41133                                                                  
         CLI   BFLFLD#,BK#FILL     ONLY FOR KEYWORK VAR                         
         BE    DISS010                                                          
                                                                                
*MN SPEC-41133                                                                  
         CLI   BFLFLD#,BK#VAR      ONLY FOR KEYWORK VAR                         
         BNE   DISSXIT                                                          
                                                                                
DISS010  LLC   RF,BFLLN            GET LENGTH OF DATA                           
         SHI   RF,BFLLNQ                                                        
         BZ    EXITOK                                                           
                                                                                
         LA    R4,BFLSRCE                                                       
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
DISSXIT  B     EXITOK                                                           
*                                                                               
         MVC   FVIFLD(0),0(R4)                                                  
                                                                                
***********************************************************************         
* DISPLAY A FORMAT NAME FIELD                                         *         
***********************************************************************         
                                                                                
VALSRC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
                                                                                
*MN SPEC-41133                                                                  
         CLI   BFLFLD#,BK#FILL     ONLY FOR KEYWORK VAR                         
         BE    VALS010                                                          
                                                                                
*MN SPEC-41133                                                                  
         CLI   BFLFLD#,BK#VAR                                                   
         BNE   EXITOK                                                           
                                                                                
VALS010  XC    BOWORK2,BOWORK2     MOVE IN FIXED PART OF ELEMENT                
         MVC   BOWORK2(BFLLNQ),BFLELD                                           
                                                                                
         LA    R3,BOWORK2          NOW USE THIS AS ELEMENT                      
         LLC   RF,FVILEN                                                        
         AHI   RF,BFLLNQ                                                        
         STC   RF,BFLLN            SET NEW ELEMENT LENGTH                       
         CLI   FVILEN,0            WAS ANYTHING INPUT?                          
         BE    VSRC03              NO                                           
*                                                                               
         LA    R4,BFLSRCE                                                       
         LLC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BFLSRCE(0),FVIFLD   MOVE COMPANY LIST BACK IN                    
*                                                                               
VSRC03   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('BFLELQ',(R2)),0                  
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),(R2),BFLELD                        
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LINE NUMBER                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
LINDTA   LA    RF,LINTB                                                         
         B     ITER                                                             
*                                                                               
LINTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISLIN)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFLIN)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLIN)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFLIN)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOLIN)                                  
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A LINE NUMBER FIELD                                         *         
***********************************************************************         
                                                                                
DISLIN   LLC   R0,BANKLIN#                                                      
         EDIT  (R0),(5,FVIFLD),ALIGN=LEFT                                       
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DISPLAY A LINE NUMBER FIELD                                         *         
***********************************************************************         
                                                                                
DISFLIN  LLC   R0,FLTIFLD                                                       
         EDIT  (R0),(5,FVIFLD),ALIGN=LEFT                                       
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A LINE NUMBER FIELD                                        *         
***********************************************************************         
                                                                                
VALLIN   MVI   KEYCODE,0                                                        
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM         IF IT IS A NUMBER                          
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL          THEN IT GETS SET IN BCFULL                 
         CHI   RF,255                                                           
         BH    EXITTOOM                                                         
         MVI   KEYCODE,C'A'                                                     
         STC   RF,BANKLIN#                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A LINE NUMBER FILTER FIELD                                 *         
***********************************************************************         
                                                                                
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
                                                                                
DOLIN    CLC   BANKLIN#,FVIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FIELD NUMBER                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
FLDDTA   LA    RF,FLDTB                                                         
         B     ITER                                                             
*                                                                               
FLDTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFLD)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFFLD)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFLD)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFFLD)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLD)                                  
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY A FIELD NUMBER FIELD                                        *         
***********************************************************************         
                                                                                
DISFLD   LLC   R0,BANKCOL#                                                      
         EDIT  (R0),(5,FVIFLD),ALIGN=LEFT                                       
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DISPLAY A FIELD NUMBER FIELD                                        *         
***********************************************************************         
                                                                                
DISFFLD  LLC   R0,FLTIFLD                                                       
         EDIT  (R0),(5,FVIFLD),ALIGN=LEFT                                       
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* VALIDATE A FIELD NUMBER FIELD                                       *         
***********************************************************************         
                                                                                
VALFLD   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM         IF IT IS A NUMBER                          
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL          THEN IT GETS SET IN BCFULL                 
         CHI   RF,255                                                           
         BH    EXITTOOM                                                         
         STC   RF,BANKCOL#                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A FIELD NUMBER FILTER FIELD                                *         
***********************************************************************         
                                                                                
VALFFLD  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM         IF IT IS A NUMBER                          
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL          THEN IT GETS SET IN BCFULL                 
         CHI   RF,255                                                           
         BH    EXITTOOM                                                         
         STC   RF,BANKCOL#                                                      
         STC   RF,FLTIFLD                                                       
         B     EXITOK                                                           
***********************************************************************         
* FILTER ON FIELD NUMBER FIELD                                        *         
***********************************************************************         
                                                                                
DOFLD    CLC   BANKCOL#,FVIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
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
                                                                                
         USING FRCTABD,RF                                                       
DISRCC   LA    RF,FRCTAB2          DON'T ALLOW BASE RECORDS                     
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
                                                                                
***********************************************************************         
* VALIDATE A RECORD CODE FIELD                                        *         
***********************************************************************         
                                                                                
         USING FRCTABD,RF                                                       
VALRCC   LA    RF,FRCTAB2          DON'T ALLOW BASE RECORDS                     
*                                                                               
VRCC02   CLI   FRCNAM,X'FF'                                                     
         BE    EXITNV                                                           
         CLC   FRCNAM,FVIFLD                                                    
         BE    *+12                                                             
         AHI   RF,FRCTABLQ                                                      
         B     VRCC02                                                           
*                                                                               
         MVC   BANKREC,FRCEQU                                                   
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD CODE NAME                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
RCNDTA   LA    RF,RCNTB                                                         
         B     ITER                                                             
*                                                                               
RCNTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISRCN)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD CODE NAME                                          *         
***********************************************************************         
                                                                                
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
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - SPACE PADDED FIELD            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
SPCPDTA  LA    RF,SPCPTB                                                        
         B     ITER                                                             
*                                                                               
SPCPTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSPCP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSPCP)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - SPACE PAD FIELD                     *         
***********************************************************************         
                                                                                
DISSPCP  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT,BFSPCPAD                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - SPACE PAD FIELD                    *         
***********************************************************************         
                                                                                
VALSPCP  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT,255-BFSPCPAD                                             
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
         BE    VSPCP04                                                          
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VSPCP04                                                          
         CLC   FVIFLD(0),LC@YES                                                 
         B     VSPCP06                                                          
*                                                                               
VSPCP04  OI    BFLSTAT,BFSPCPAD                                                 
         B     EXITOK                                                           
*                                                                               
VSPCP06  B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - LEFT JUSTIFIED FIELD          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
LJFDTA   LA    RF,LJFTB                                                         
         B     ITER                                                             
*                                                                               
LJFTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISLJF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLJF)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - LEFT JUSTIFIED FIELD                *         
***********************************************************************         
                                                                                
DISLJF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT,BFLFT                                                    
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - LEFT JUSTIFIED FIELD               *         
***********************************************************************         
                                                                                
VALLJF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT,255-BFLFT                                                
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
         BE    VLJF04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VLJF04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VLJF06                                                           
*                                                                               
VLJF04   OI    BFLSTAT,BFLFT                                                    
         B     EXITOK                                                           
*                                                                               
VLJF06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - COUNTRY DEPENDENT             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
DPCDTA   LA    RF,DPCTB                                                         
         B     ITER                                                             
*                                                                               
DPCTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISDPC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDPC)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - COUNTRY DEPENDENT                   *         
***********************************************************************         
                                                                                
DISDPC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT,BFCTRY                                                   
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - COUNTRY DEPENDENT                  *         
***********************************************************************         
                                                                                
VALDPC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT,255-BFCTRY                                               
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
         BE    VDPC04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VDPC04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VDPC06                                                           
*                                                                               
VDPC04   OI    BFLSTAT,BFCTRY                                                   
         B     EXITOK                                                           
*                                                                               
VDPC06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - VOID DEPENDENT                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
VDPDTA   LA    RF,VDPTB                                                         
         B     ITER                                                             
*                                                                               
VDPTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISVDP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVDP)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - VOID DEPENDENT                      *         
***********************************************************************         
                                                                                
DISVDP   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT,BFVDEP                                                   
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - VOID DEPENDENT                     *         
***********************************************************************         
                                                                                
VALVDP   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT,255-BFVDEP                                               
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
         BE    VVDP04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VVDP04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VVDP06                                                           
*                                                                               
VVDP04   OI    BFLSTAT,BFVDEP                                                   
         B     EXITOK                                                           
*                                                                               
VVDP06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - USES PREFIX                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
UPFDTA   LA    RF,UPFTB                                                         
         B     ITER                                                             
*                                                                               
UPFTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISUPF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALUPF)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - USES PREFIX                         *         
***********************************************************************         
                                                                                
DISUPF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT,BFPREFIX                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - USES PREFIX                        *         
***********************************************************************         
                                                                                
VALUPF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT,255-BFPREFIX                                             
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
         BE    VUPF04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VUPF04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VUPF06                                                           
*                                                                               
VUPF04   OI    BFLSTAT,BFPREFIX                                                 
         B     EXITOK                                                           
*                                                                               
VUPF06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - USES DEFAULT PREFIX           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
DPFDTA   LA    RF,DPFTB                                                         
         B     ITER                                                             
*                                                                               
DPFTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISDPF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDPF)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - USES DEFAULT PREFIX                 *         
***********************************************************************         
                                                                                
DISDPF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT,BFPRFXDF                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - USES DEFAULT PREFIX                *         
***********************************************************************         
                                                                                
VALDPF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT,255-BFPRFXDF                                             
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
         BE    VDPF04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VDPF04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VDPF06                                                           
*                                                                               
VDPF04   OI    BFLSTAT,BFPRFXDF                                                 
         B     EXITOK                                                           
*                                                                               
VDPF06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FIELD IS A VOID               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
IVDDTA   LA    RF,IVDTB                                                         
         B     ITER                                                             
*                                                                               
IVDTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISIVD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIVD)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FIELD IS A VOID                     *         
***********************************************************************         
                                                                                
DISIVD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT,BFVOID                                                   
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FIELD IS A VOID                    *         
***********************************************************************         
                                                                                
VALIVD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT,255-BFVOID                                               
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
         BE    VIVD04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VIVD04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VIVD06                                                           
*                                                                               
VIVD04   OI    BFLSTAT,BFVOID                                                   
         B     EXITOK                                                           
*                                                                               
VIVD06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FIELD FOR DEST NOT SOURCE     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
FDSDTA   LA    RF,FDSTB                                                         
         B     ITER                                                             
*                                                                               
FDSTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFDS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFDS)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FIELD FOR DEST NOT SOURCE           *         
***********************************************************************         
                                                                                
DISFDS   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT,BFDEST                                                   
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FIELD FOR DEST NOT SOURCE          *         
***********************************************************************         
                                                                                
VALFDS   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT,255-BFDEST                                               
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
         BE    VFDS04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFDS04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFDS06                                                           
*                                                                               
VFDS04   OI    BFLSTAT,BFDEST                                                   
         B     EXITOK                                                           
*                                                                               
VFDS06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FIELD VOID IF NET < $1        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
VNMDTA   LA    RF,VNMTB                                                         
         B     ITER                                                             
*                                                                               
VNMTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISVNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVNM)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FIELD VOID IF NET < $1              *         
***********************************************************************         
                                                                                
DISVNM   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT2,BFNETNPS                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FIELD VOID IF NET < $1             *         
***********************************************************************         
                                                                                
VALVNM   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT2,255-BFNETNPS                                            
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
         BE    VVNM04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VVNM04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VVNM06                                                           
*                                                                               
VVNM04   OI    BFLSTAT2,BFNETNPS                                                
         B     EXITOK                                                           
*                                                                               
VVNM06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - SUPPRESS ZERO DOLLARS         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
SZDDTA   LA    RF,SZDTB                                                         
         B     ITER                                                             
*                                                                               
SZDTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISSZD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSZD)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - SUPPRESS ZERO DOLLARS               *         
***********************************************************************         
                                                                                
DISSZD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT2,BFZERO                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - SUPPRESS ZERO DOLLARS              *         
***********************************************************************         
                                                                                
VALSZD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT2,255-BFZERO                                              
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
         BE    VSZD04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VSZD04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VSZD06                                                           
*                                                                               
VSZD04   OI    BFLSTAT2,BFZERO                                                  
         B     EXITOK                                                           
*                                                                               
VSZD06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FIELD IS CANADIAN             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
CANDTA   LA    RF,CANTB                                                         
         B     ITER                                                             
*                                                                               
CANTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCAN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCAN)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FIELD IS CANADIAN                   *         
***********************************************************************         
                                                                                
DISCAN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT2,BFCAN                                                   
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FIELD IS CANADIAN                  *         
***********************************************************************         
                                                                                
VALCAN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT2,255-BFCAN                                               
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
         BE    VCAN04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VCAN04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VCAN06                                                           
*                                                                               
VCAN04   OI    BFLSTAT2,BFCAN                                                   
         B     EXITOK                                                           
*                                                                               
VCAN06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - MINUS=Y FOR NEGATIVE          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
MEYDTA   LA    RF,MEYTB                                                         
         B     ITER                                                             
*                                                                               
MEYTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMEY)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - MINUS=Y FOR NEGATIVE                *         
***********************************************************************         
                                                                                
DISMEY   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT2,BFMINUS                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - MINUS=Y FOR NEGATIVE               *         
***********************************************************************         
                                                                                
VALMEY   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT2,255-BFMINUS                                             
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
         BE    VMEY04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VMEY04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VMEY06                                                           
*                                                                               
VMEY04   OI    BFLSTAT2,BFMINUS                                                 
         B     EXITOK                                                           
*                                                                               
VMEY06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FLOAT=- FOR NEGATIVE          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
FEYDTA   LA    RF,FEYTB                                                         
         B     ITER                                                             
*                                                                               
FEYTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFEY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFEY)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FLOAT=- FOR NEGATIVE                *         
***********************************************************************         
                                                                                
DISFEY   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT2,BFFLOAT                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FLOAT=- FOR NEGATIVE               *         
***********************************************************************         
                                                                                
VALFEY   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT2,255-BFFLOAT                                             
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
         BE    VFEY04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFEY04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFEY06                                                           
*                                                                               
VFEY04   OI    BFLSTAT2,BFFLOAT                                                 
         B     EXITOK                                                           
*                                                                               
VFEY06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - LEDGER DEPENDENT              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
LDPDTA   LA    RF,LDPTB                                                         
         B     ITER                                                             
*                                                                               
LDPTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLDP)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - LEDGER DEPENDENT                    *         
***********************************************************************         
                                                                                
DISLDP   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT2,BLKWLNK                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - LINK KEYWORD                       *         
***********************************************************************         
                                                                                
VALLDP   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT2,255-BLKWLNK                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
                                                                                
         LLC   RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
                                                                                
         EX    RE,*+8              YES                                          
         BE    VLDP04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VLDP04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VLDP06                                                           
                                                                                
VLDP04   OI    BFLSTAT2,BLKWLNK                                                 
         B     EXITOK                                                           
                                                                                
VLDP06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - 2DP AMOUNTS                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
DP2DTA   LA    RF,DP2TB                                                         
         B     ITER                                                             
*                                                                               
DP2TB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISDP2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDP2)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - 2DP AMOUNTS                         *         
***********************************************************************         
                                                                                
DISDP2   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT2,BF2DEC                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - 2DP AMOUNTS                        *         
***********************************************************************         
                                                                                
VALDP2   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT2,255-BF2DEC                                              
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
         BE    VDP204                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VDP204                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VDP206                                                           
*                                                                               
VDP204   OI    BFLSTAT2,BF2DEC                                                  
         B     EXITOK                                                           
*                                                                               
VDP206   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FIELD DEPENDENT               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
FPPDTA   LA    RF,FPPTB                                                         
         B     ITER                                                             
*                                                                               
FPPTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFPP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFPP)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FIELD DEPENDENT                     *         
***********************************************************************         
                                                                                
DISFPP   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT2,BFFLDDEP                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FIELD DEPENDENT                    *         
***********************************************************************         
                                                                                
VALFPP   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT2,255-BFFLDDEP                                            
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
         BE    VFPP04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VFPP04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VFPP06                                                           
*                                                                               
VFPP04   OI    BFLSTAT2,BFFLDDEP                                                
         B     EXITOK                                                           
*                                                                               
VFPP06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - VOID IF BKNET < 0             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
NZRDTA   LA    RF,NZRTB                                                         
         B     ITER                                                             
*                                                                               
NZRTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISNZR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNZR)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - VOID IF BKNET < 0                   *         
***********************************************************************         
                                                                                
DISNZR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT3,BFNETZRO                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - VOID IF BKNET < 0                  *         
***********************************************************************         
                                                                                
VALNZR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT3,255-BFNETZRO                                            
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
         BE    VNZR04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VNZR04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VNZR06                                                           
*                                                                               
VNZR04   OI    BFLSTAT3,BFNETZRO                                                
         B     EXITOK                                                           
*                                                                               
VNZR06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - CANADIAN CURRENCY             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
CCRDTA   LA    RF,CCRTB                                                         
         B     ITER                                                             
*                                                                               
CCRTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCCR)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - CANADIAN CURRENCY                   *         
***********************************************************************         
                                                                                
DISCCR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT3,BFCADC                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - CANADIAN CURRENCY                  *         
***********************************************************************         
                                                                                
VALCCR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT3,255-BFCADC                                              
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
         BE    VCCR04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VCCR04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VCCR06                                                           
*                                                                               
VCCR04   OI    BFLSTAT3,BFCADC                                                  
         B     EXITOK                                                           
*                                                                               
VCCR06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - MARK INVALID ADDRESS          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
MIADTA   LA    RF,MIATB                                                         
         B     ITER                                                             
*                                                                               
MIATB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISMIA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMIA)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - MARK INVALID ADDRESS                *         
***********************************************************************         
                                                                                
DISMIA   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT3,BFINVADR                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - MARK INVALID ADDRESS               *         
***********************************************************************         
                                                                                
VALMIA   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT3,255-BFINVADR                                            
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
         BE    VMIA04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VMIA04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VMIA06                                                           
*                                                                               
VMIA04   OI    BFLSTAT3,BFINVADR                                                
         B     EXITOK                                                           
*                                                                               
VMIA06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - ADD COMMAS TO DOLLARS         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
CTDDTA   LA    RF,CTDTB                                                         
         B     ITER                                                             
*                                                                               
CTDTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCTD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCTD)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - ADD COMMAS TO DOLLARS               *         
***********************************************************************         
                                                                                
DISCTD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT3,BFCOMMAS                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - ADD COMMAS TO DOLLARS              *         
***********************************************************************         
                                                                                
VALCTD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT3,255-BFCOMMAS                                            
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
         BE    VCTD04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VCTD04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VCTD06                                                           
*                                                                               
VCTD04   OI    BFLSTAT3,BFCOMMAS                                                
         B     EXITOK                                                           
*                                                                               
VCTD06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - FIELD MAY OVERFLOW            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
OVFDTA   LA    RF,OVDTB                                                         
         B     ITER                                                             
*                                                                               
OVDTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISOVF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOVF)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - FIELD MAY OVERFLOW                  *         
***********************************************************************         
                                                                                
DISOVF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT3,BFROVRFL                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - FIELD MAY OVERFLOW                 *         
***********************************************************************         
                                                                                
VALOVF   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT3,255-BFROVRFL                                            
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
         BE    VOVF04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VOVF04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VOVF06                                                           
*                                                                               
VOVF04   OI    BFLSTAT3,BFROVRFL                                                
         B     EXITOK                                                           
*                                                                               
VOVF06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - NO DELIMITER COMMAS ON FIELD  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
CMADTA   LA    RF,CMATB                                                         
         B     ITER                                                             
*                                                                               
CMATB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCMA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCMA)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - NO DELIMITER COMMAS ON FIELD        *         
***********************************************************************         
                                                                                
DISCMA   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT3,BFRNDLIM                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - NO DELIMITER COMMAS ON FIELD       *         
***********************************************************************         
                                                                                
VALCMA   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT3,255-BFRNDLIM                                            
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
         BE    VCMA04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VCMA04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VCMA06                                                           
*                                                                               
VCMA04   OI    BFLSTAT3,BFRNDLIM                                                
         B     EXITOK                                                           
*                                                                               
VCMA06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - USE ACTUAL FLD LEN/NO OVERRID *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
LENDTA   LA    RF,LENTB                                                         
         B     ITER                                                             
*                                                                               
LENTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLEN)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - USE ACTUAL FIELD LEN/NO OVERRIDES   *         
***********************************************************************         
                                                                                
DISLEN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    BFLSTAT3,BFRACTLN                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - USE ACTUAL FIELD LEN/NO OVERRIDES  *         
***********************************************************************         
                                                                                
VALLEN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         NI    BFLSTAT3,255-BFRACTLN                                            
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
         BE    VLEN04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VLEN04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VLEN06                                                           
*                                                                               
VLEN04   OI    BFLSTAT3,BFRACTLN                                                
         B     EXITOK                                                           
*                                                                               
VLEN06   B     EXITNV              NOTHING ELSE IS VALID                        
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD STATUS FIELD - USES ADDITIONAL FORMAT        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
*FMTDTA   LA    RF,FMTTB                                                        
*         B     ITER                                                            
*                                                                               
*FMTTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFMT)                                
*         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFMT)                                
*         DC    AL1(EOT)                                                        
*        EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD STATUS FIELD - USES ADDITIONA FORMAT               *         
***********************************************************************         
                                                                                
*DISFMT   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                 
*         L     R3,12(R1)                                                       
*         USING BFLELD,R3                                                       
*         MVC   FVIFLD(L'LC@NO),LC@NO                                           
*         TM    BFLSTAT3,BFRADLFM                                               
*         BZ    EXITOK                                                          
*         MVC   FVIFLD(L'LC@YES),LC@YES                                         
*         B     EXITOK                                                          
*         DROP  R3                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD STATUS FIELD - USES ADDITIONAL FORMAT             *         
***********************************************************************         
                                                                                
*VALFMT   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                 
*         L     R3,12(R1)                                                       
*         USING BFLELD,R3                                                       
*         NI    BFLSTAT3,255-BFRADLFM                                           
*         CLI   FVILEN,0                                                        
*         BE    EXITOK                                                          
*                                                                               
*         LLC   RE,FVXLEN                                                       
*         EX    RE,*+8                                                          
*         BE    EXITOK                                                          
*         CLC   FVIFLD(0),UE@NO                                                 
*         EX    RE,*+8              NO                                          
*         BE    EXITOK                                                          
*         CLC   FVIFLD(0),LC@NO                                                 
*                                                                               
*         EX    RE,*+8              YES                                         
*         BE    VFMT04                                                          
*         CLC   FVIFLD(0),UE@YES                                                
*         EX    RE,*+8              YES                                         
*         BE    VFMT04                                                          
*         CLC   FVIFLD(0),LC@YES                                                
*         B     VFMT06                                                          
*                                                                               
*VFMT04   OI    BFLSTAT3,BFRADLFM                                               
*         B     EXITOK                                                          
*                                                                               
*VFMT06   B     EXITNV              NOTHING ELSE IS VALID                       
*         DROP  R3                                                              
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD FIELD KEYWORD                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
KWDDTA   LA    RF,KWDTB                                                         
         B     ITER                                                             
*                                                                               
KWDTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISKWD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALKWD)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD FIELD KEYWORD                                      *         
***********************************************************************         
                                                                                
DISKWD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         OC    BFLFLD#,BFLFLD#                                                  
         BZ    EXITOK                                                           
*                                                                               
         LA    RF,KEYWTAB                                                       
         USING KEYWTBD,RF                                                       
         MVC   FVIFLD(3),=CL3'???'                                              
DISKW10  CLI   0(RF),FF                                                         
         BE    EXITOK                                                           
         CLC   BFLFLD#,KEYWNUM                                                  
         BE    *+12                                                             
         AHI   RF,KEYWLNQ                                                       
         B     DISKW10                                                          
*                                                                               
         MVC   FVIFLD(L'KEYWDSC),KEYWDSC                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD FIELD KEYWORD                                     *         
***********************************************************************         
                                                                                
VALKWD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
*                                                                               
         TM    FVIIND,FVINUM       DID PUNTER ENTER A NUMBER?                   
         BZ    VALKW03             NO                                           
         L     R0,BCFULL                                                        
         LA    RF,KEYWTAB                                                       
         USING KEYWTBD,RF                                                       
VALKW02  CLI   0(RF),FF                                                         
         BE    EXITNV                                                           
         CLM   R0,1,KEYWNUM                                                     
         BE    *+12                                                             
         AHI   RF,KEYWLNQ                                                       
         B     VALKW02                                                          
*                                                                               
         MVC   BFLFLD#,KEYWNUM                                                  
         MVC   FVIFLD(L'KEYWDSC),KEYWDSC                                        
         B     EXITOK                                                           
*                                                                               
VALKW03  LLC   R1,FVXLEN                                                        
         LA    RF,KEYWTAB                                                       
         USING KEYWTBD,RF                                                       
VALKW04  CLI   0(RF),FF                                                         
         BE    EXITNV                                                           
         EX    R1,VKWCLC                                                        
         BE    *+12                                                             
         AHI   RF,KEYWLNQ                                                       
         B     VALKW04                                                          
*                                                                               
         MVC   BFLFLD#,KEYWNUM                                                  
         MVC   FVIFLD(L'KEYWDSC),KEYWDSC                                        
         B     EXITOK                                                           
*                                                                               
VKWCLC   CLC   FVIFLD(0),KEYWDSC                                                
         DROP  R3,RF                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD FIELD DISPLACEMENT                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
DSPDTA   LA    RF,DSPTB                                                         
         B     ITER                                                             
*                                                                               
DSPTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDSP)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD FIELD DISPLACEMENT                                 *         
***********************************************************************         
                                                                                
DISDSP   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         SR    R0,R0                                                            
         ICM   R0,3,BFLDSP                                                      
         EDIT  (R0),(5,FVIFLD),ZERO=NOBLANK,ALIGN=LEFT                          
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD FIELD DISPLACEMENT                                *         
***********************************************************************         
                                                                                
VALDSP   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CLC   BCFULL,=X'0000FFFF'                                              
         BH    EXITNV                                                           
         STCM  RF,3,BFLDSP                                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD FIELD LENGTH                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
FLNDTA   LA    RF,FLNTB                                                         
         B     ITER                                                             
*                                                                               
FLNTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISFLN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFLN)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A RECORD FIELD LENGTH                                       *         
***********************************************************************         
                                                                                
DISFLN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         OC    BFLLEN,BFLLEN                                                    
         BZ    EXITOK                                                           
         EDIT  BFLLEN,(3,FVIFLD),ZERO=NOBLANK,ALIGN=LEFT                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE A RECORD FIELD LENGTH                                      *         
***********************************************************************         
                                                                                
VALFLN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CHI   RF,255                                                           
         BH    EXITNV                                                           
         STC   RF,BFLLEN                                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DATE FORMAT                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
DFMDTA   LA    RF,DFMTB                                                         
         B     ITER                                                             
*                                                                               
DFMTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISDFM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDFM)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A DATE FORMAT FIELD                                         *         
***********************************************************************         
                                                                                
DISDFM   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         OC    BFLFRM,BFLFRM                                                    
         BZ    EXITOK                                                           
*                                                                               
         CLI   BFLFLD#,BK#TODDT                                                 
         BE    DISD05                                                           
         CLI   BFLFLD#,BK#TRNDT                                                 
         BE    DISD05                                                           
         CLI   BFLFLD#,BK#ACTDT                                                 
         BE    DISD05                                                           
         CLI   BFLFLD#,BK#CHKDT                                                 
         BE    DISD05                                                           
         CLI   BFLFLD#,BK#INVDT                                                 
         BE    DISD05                                                           
         CLI   BFLFLD#,BK#PERDT                                                 
         BE    DISD05                                                           
         CLI   BFLFLD#,BK#MOS                                                   
         BNE   DISDXIT                                                          
                                                                                
         USING DATETABD,RF                                                      
DISD05   LA    RF,DATETAB                                                       
DISD10   CLI   0(RF),FF                                                         
         BE    EXITOK                                                           
         CLC   BFLFRM,DATTBTYP                                                  
         BE    *+12                                                             
         AHI   RF,DATTBLNQ                                                      
         B     DISD10                                                           
         CLC   BFLOVR,DATTBOVR                                                  
         BE    *+12                                                             
         AHI   RF,DATTBLNQ                                                      
         B     DISD10                                                           
*                                                                               
         MVC   FVIFLD(L'DATTBFRM),DATTBFRM                                      
*                                                                               
DISDXIT  B     EXITOK                                                           
         DROP  R3,RF                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE A DATE FORMAT FIELD                                        *         
***********************************************************************         
                                                                                
VALDFM   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         CLI   BFLFLD#,BK#TODDT                                                 
         BE    VALD20                                                           
         CLI   BFLFLD#,BK#TRNDT                                                 
         BE    VALD20                                                           
         CLI   BFLFLD#,BK#ACTDT                                                 
         BE    VALD20                                                           
         CLI   BFLFLD#,BK#CHKDT                                                 
         BE    VALD20                                                           
         CLI   BFLFLD#,BK#INVDT                                                 
         BE    VALD20                                                           
         CLI   BFLFLD#,BK#PERDT                                                 
         BE    VALD20                                                           
         CLI   BFLFLD#,BK#MOS                                                   
         BNE   VALDXIT                                                          
*                                                                               
         USING DATETABD,RF                                                      
VALD20   LA    RF,DATETAB                                                       
         LLC   R1,FVXLEN                                                        
VALD30   CLI   0(RF),FF                                                         
         BE    EXITINVD                                                         
         EX    R1,VALDCLC                                                       
         BE    *+12                                                             
         AHI   RF,DATTBLNQ                                                      
         B     VALD30                                                           
*                                                                               
         MVC   BFLFRM,DATTBTYP                                                  
         MVC   BFLOVR,DATTBOVR                                                  
         MVC   FVIFLD(L'DATTBFRM),DATTBFRM                                      
VALDXIT  B     EXITOK                                                           
*                                                                               
VALDCLC  CLC   FVIFLD(0),DATTBFRM                                               
*                                                                               
         DROP  R3,RF                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DATE FORMAT OVERRIDE                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
DFODTA   LA    RF,DFOTB                                                         
         B     ITER                                                             
*                                                                               
DFOTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISDFO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDFO)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY OVERRIDE                                                    *         
***********************************************************************         
                                                                                
DISDFO   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
         OC    BFLOVR,BFLOVR                                                    
         BZ    EXITOK                                                           
*                                                                               
*        DON'T NEED TO PROCESS THE OVERRIDE FIELD FOR DATES - THIS IS           
*        TAKEN CARE OF IN THE DATE FORMAT TABLE - DATETAB                       
*                                                                               
         CLI   BFLFLD#,BK#TODDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#TRNDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#ACTDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#CHKDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#INVDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#PERDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#MOS                                                   
         BE    EXITOK                                                           
                                                                                
         CLI   BFLFLD#,BK#IADDR                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#IBKAD                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#ADDR                                                  
         BE    EXITOK                                                           
*MN                                                                             
         CLI   BFLFLD#,BK#ACC#                                                  
         BE    EXITOK                                                           
*MN                                                                             
                                                                                
         CLI   BFLFTYP,4                                                        
         BE    EXITOK                                                           
         CLI   BFLFTYP,6                                                        
         BE    EXITOK                                                           
                                                                                
*                                                                               
         EDIT  BFLOVR,(3,FVIFLD),ZERO=NOBLANK,ALIGN=LEFT                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE OVERRIDE                                                   *         
***********************************************************************         
                                                                                
VALDFO   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
*                                                                               
*        DON'T NEED TO PROCESS THE OVERRIDE FIELD FOR DATES - THIS IS           
*        TAKEN CARE OF IN THE DATE FORMAT TABLE - DATETAB                       
*                                                                               
         CLI   BFLFLD#,BK#TODDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#TRNDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#ACTDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#CHKDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#INVDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#PERDT                                                 
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#MOS                                                   
         BE    EXITOK                                                           
                                                                                
         CLI   BFLFLD#,BK#IADDR      ONLY VALID FOR ADDRESS KEYWORD             
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#IBKAD      ONLY VALID FOR ADDRESS KEYWORD             
         BE    EXITOK                                                           
         CLI   BFLFLD#,BK#ADDR     CAN ONLY SPECIFY ADDRESS LINE                
         BE    EXITOK              FOR THIS KEYWORK                             
                                                                                
         CLI   BFLFTYP,4                                                        
         BE    EXITOK                                                           
         CLI   BFLFTYP,6                                                        
         BE    EXITOK                                                           
                                                                                
*                                                                               
VALDF05  TM    FVIIND,FVITHIS                                                   
         BZ    EXITOK                                                           
                                                                                
         CLI   FVILEN,0            WAS ANYTHING INPUT?                          
         BNE   *+14                IF NOT                                       
         XC    BFLOVR,BFLOVR       CLEAR ELEMENT VALUE AND EXIT                 
         B     EXITOK                                                           
*                                                                               
         CLI   BFLFLD#,BK#TIME     IS THIS TIME KEYWORD?                        
         BE    VALDF10                                                          
*        CLI   BFLFLD#,BK#RECCN                                                 
*        BE    VALDF10                                                          
         B     EXITOK                                                           
*                                                                               
VALDF10  TM    FVIIND,FVINUM       INPUT MUST BE NUMERIC                        
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CHI   RF,255                                                           
         BH    EXITNV                                                           
         STC   RF,BFLOVR                                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPANY LIST                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
XTRDTA   LA    RF,XTRTB                                                         
         B     ITER                                                             
*                                                                               
XTRTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISXTR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXTR)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY COMPANY LIST                                                *         
***********************************************************************         
                                                                                
DISXTR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
                                                                                
         CLI   BFLFLD#,BK#VAR       LIST NOT VALID FOR BK#VAR                   
         BE    DISXTRX                                                          
                                                                                
         CLI   BFLFTYP,4            IS COMPANY OVERRIDE SET?                    
         BNE   DISXTR10             IF NO EXIT                                  
         LLC   R1,BFLCPY#           IS NUMBER OF COMPANIES SET?                 
         LTR   R1,R1                IF ZERO EXIT                                
         BZ    DISXTRX                                                          
                                                                                
         LA    RF,FVIFLD            FIELD ON SCREEN                             
         LA    R4,BFLSRCE           COMPANY LIST IN ELEMENT                     
DISXTR05 MVC   0(2,RF),0(R4)        MOVE COMPANY LIST TO SCREEN                 
         AHI   R1,-1                                                            
         BZ    DISXTRX                                                          
         LA    RF,2(RF)                                                         
         MVI   0(RF),C','                                                       
         LA    RF,1(RF)                                                         
         LA    R4,2(R4)                                                         
         B     DISXTR05                                                         
                                                                                
DISXTR10 CLI   BFLFTYP,6            IS LEDGER OVERRIDE SET?                     
         BNE   DISXTRX              DISPLAY TEXT                                
                                                                                
         LLC   R1,BFLLN             GET LENGTH OF DATA                          
         SHI   R1,BFLLNQ                                                        
         BZ    DISXTRX              IF ZERO NO LEDGER ENTEREDD                  
                                                                                
         MVC   FVIFLD(1),BFLSRCE    MOVE LEDGER TO SCREEN                       
                                                                                
DISXTRX  B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE COMPANY LIST                                               *         
***********************************************************************         
                                                                                
VALXTR   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
                                                                                
         CLI   BFLFLD#,BK#VAR      FIELD CONFLICT WITH BK#VAR                   
         BE    EXITOK                                                           
                                                                                
         TM    FVIIND,FVITHIS                                                   
         BZ    EXITOK                                                           
                                                                                
         XC    BOWORK2,BOWORK2     MOVE IN WHOLE ELEMENT                        
         LLC   RF,BFLLN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BOWORK2(0),BFLELD                                                
                                                                                
         LA    R3,BOWORK2          NOW USE THIS AS ELEMENT                      
                                                                                
         CLI   FVILEN,0            WAS ANYTHING INPUT?                          
         BNE   VALXTR04            IF NO INPUT AND                              
         XC    BFLCPY#,BFLCPY#     ZERO COMPANIES                               
         MVI   BFLFTYP,0           RESET                                        
         MVI   BFLLN,BFLLNQ        STANDARD LENGTH                              
         B     VALXTR30            GO DELETE ORIGINAL AND ADD NEW               
                                                                                
VALXTR04 DS    0H                                                               
         CLI   FVILEN,1            LEDGER IS ONE CHARACTER                      
         BE    VALXTR10                                                         
                                                                                
         LA    R1,FVIFLD                                                        
         LA    RF,BFLSRCE                                                       
         SR    R0,R0                                                            
VALXTR05 DS    0H                                                               
         CLC   0(2,R1),=C'  '      VALIDATION CODE                              
         BNH   EXITNV                                                           
         MVC   0(2,RF),0(R1)       SCREEN TO ELEMENT                            
         LA    R1,2(R1)            SCREEN                                       
         LA    RF,2(RF)            ELEMENT                                      
         CLI   0(R1),C' '          VALIDATION CODE                              
         BE    VALXTR08                                                         
         CLI   0(R1),C','          VALIDATION CODE                              
         BNE   EXITNV                                                           
         LA    R1,1(R1)                                                         
         AHI   R0,1                ADD 1 TO COUNT                               
         B     VALXTR05                                                         
                                                                                
VALXTR08 DS    0H                                                               
         AHI   R0,1                ADD 1 TO COUNT                               
         STC   R0,BFLCPY#          STORE # OF COMPANIES                         
         MVI   BFLLN,BFLLNQ        CALCULATE NEW ELEMENT LENGTH                 
         LLC   R4,BFLLN            BASE LENGTH                                  
         LLC   RF,BFLCPY#          GET CURRENT NUMBER OF COMPANIES              
         MHI   RF,2                TIMES THE LENGTH OF EACH (2)                 
         AR    R4,RF               ADD TO BASE LENGTH                           
         STC   R4,BFLLN                                                         
         MVI   BFLFTYP,4           (FDCPY) SET DEPENDENCY TYPE                  
         B     VALXTR30            UPDATE ELEMENTS                              
                                                                                
VALXTR10 MVC   BFLSRCE(1),FVIFLD   SCREEN TO ELEMENT                            
         MVI   BFLLN,BFLLNQ+1      CALCULATE NEW ELEMENT LENGTH                 
         MVI   BFLFTYP,6           (FDCPY) SET DEPENDENCY TYPE                  
         MVI   BFLOVR,0            (FDCPY) SET DEPENDENCY TYPE                  
                                                                                
VALXTR30 GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('BFLELQ',(R2)),0                  
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),(R2),BFLELD                        
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INPUT DISPLACEMENT                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
IDSDTA   LA    RF,IDSTB                                                         
         B     ITER                                                             
*                                                                               
IDSTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISIDS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIDS)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY INPUT DISPLACEMENT                                          *         
***********************************************************************         
                                                                                
DISIDS   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
                                                                                
         CLI   BFLFLD#,BK#IACC#                                                 
         BE    DISIDS10                                                         
         CLI   BFLFLD#,BK#ACC#                                                  
         BNE   EXITOK                                                           
                                                                                
DISIDS10 EDIT  BFLDDSP,(3,FVIFLD),ALIGN=LEFT                                    
                                                                                
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE INPUT DISPLACEMENT                                         *         
***********************************************************************         
                                                                                
VALIDS   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
                                                                                
         CLI   BFLFLD#,BK#ACC#                                                  
         BE    VALIDS10                                                         
         CLI   BFLFLD#,BK#IACC#                                                 
         BNE   EXITOK                                                           
                                                                                
VALIDS10 CLI   FVILEN,0            WAS ANYTHING INPUT?                          
         BNE   *+12                IF NOT SET DISPLACEMENT TO ZERO              
         MVI   BFLDDSP,0           AND EXIT                                     
         B     EXITOK                                                           
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         STC   RF,BFLDDSP                                                       
*                                                                               
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
ADDDTA   LA    RF,ADDTB                                                         
         B     ITER                                                             
*                                                                               
ADDTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISADD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADD)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY ADDRESS LINE                                                *         
***********************************************************************         
                                                                                
DISADD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
                                                                                
         CLI   BFLFLD#,BK#IADDR      ONLY VALID FOR ADDRESS KEYWORD             
         BE    DISADD01                                                         
         CLI   BFLFLD#,BK#IBKAD      ONLY VALID FOR ADDRESS KEYWORD             
         BE    DISADD01                                                         
         CLI   BFLFLD#,BK#ADDR       ONLY VALID FOR ADDRESS KEYWORD             
         BNE   DISADD02                                                         
                                                                                
DISADD01 DS    0H                                                               
         CLI   BFLFTYP,4           IF COMPANY LIST DEPEN CANNOT USE             
         BE    DISADD02            ADDRESS OVERRIDE (FIELD CONFLICT)            
         CLI   BFLFTYP,6           IF LEDGER LIST DEPEN CANNOT USE              
         BE    DISADD02            ADDRESS OVERRIDE (FIELD CONFLICT)            
                                                                                
         EDIT  BFLADD#,(3,FVIFLD),ALIGN=LEFT                                    
DISADD02 B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADDRESS LINE                                               *         
***********************************************************************         
                                                                                
VALADD   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
                                                                                
         CLI   BFLFLD#,BK#IADDR      ONLY VALID FOR ADDRESS KEYWORD             
         BE    VALADD01                                                         
         CLI   BFLFLD#,BK#IBKAD      ONLY VALID FOR ADDRESS KEYWORD             
         BE    VALADD01                                                         
         CLI   BFLFLD#,BK#ADDR     CAN ONLY SPECIFY ADDRESS LINE                
         BNE   EXITOK              FOR THIS KEYWORK                             
                                                                                
VALADD01 CLI   FVILEN,0            IF NO INPUT SET TO ZERO AND EXIT             
         BNE   *+12                                                             
         MVI   BFLADD#,0                                                        
         B     EXITOK                                                           
*                                                                               
         TM    FVIIND,FVINUM       INPUT MUST BE NUMERIC                        
         BZ    EXITNOTN                                                         
*                                                                               
         CLI   BFLFTYP,4           ALREADY HAVE COMPANY LIST SET                
         BE    EXITNV              ERROR                                        
         CLI   BFLFTYP,6           ALREADY HAVE LEDGER LIST SET                 
         BE    EXITNV              ERROR                                        
*                                                                               
VALADD05 L     RF,BCFULL                                                        
         STC   RF,BFLADD#          STORE IN ELEMENT                             
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEPENDENCIES                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
DPNDTA   LA    RF,DPNTB                                                         
         B     ITER                                                             
*                                                                               
DPNTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISDPN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDPN)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEPENDENCY NUMBER                                           *         
***********************************************************************         
                                                                                
DISDPN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
                                                                                
         CLI   BFLFTYP,7       VALID DEPENDENCIES FOR THIS FIELD                
         BH    EXITOK          ARE 1 TO 7                                       
                                                                                
         CLI   BFLFTYP,0       VALID DEPENDENCIES FOR THIS FIELD                
         BH    DISDP10         ARE 1 TO 7                                       
         TM    BFLSTAT2,BFFLDDEP   ONLY DISPLAY ZERO IF STATUS IS SET           
         BZ    EXITOK                                                           
                                                                                
DISDP10  EDIT  BFLFTYP,(3,FVIFLD),ALIGN=LEFT,ZERO=NOBLANK                       
                                                                                
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE DEPENDENCY NUMBER                                          *         
***********************************************************************         
                                                                                
VALDPN   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BFLELQ',(R2)),0                  
         L     R3,12(R1)                                                        
         USING BFLELD,R3                                                        
                                                                                
         CLI   FVILEN,0            NO INPUT - EXIT                              
         BE    EXITOK                                                           
                                                                                
         TM    FVIIND,FVITHIS                                                   
         BZ    EXITOK                                                           
                                                                                
         CLI   BFLFTYP,7       VALID DEPENDENCIES FOR THIS FIELD                
         BH    EXITNOTN        ARE 1 TO 7                                       
*MN                                                                             
         CLI   BFLFTYP,4         ON ACTION CHANGE 4(COMPANY) AND                
         BE    EXITNOTN          6 (LEDGER) ARE DONE IN LIST FIELD              
         CLI   BFLFTYP,6                                                        
         BE    EXITNOTN                                                         
*MN                                                                             
                                                                                
         CLI   FVILEN,0            IF NO INPUT SET TO ZERO AND EXIT             
         BNE   VALDPN10                                                         
         MVI   BFLFTYP,0                                                        
         B     EXITOK                                                           
                                                                                
VALDPN10 TM    FVIIND,FVINUM       INPUT MUST BE NUMERIC                        
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         STC   RF,BFLFTYP          STORE IN ELEMENT                             
                                                                                
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
*MN SPEC-36440                                                                  
***********************************************************************         
* DATA OBJECT FOR XML TAG                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
                                                                                
XMLDTA   LA    RF,XMLTB                                                         
         B     ITER                                                             
*                                                                               
XMLTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISXML)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXML)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* DISPLAY XML TAG                                                     *         
***********************************************************************         
                                                                                
DISXML   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BXLELQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
                                                                                
         L     R3,12(R1)                                                        
         USING BXLELD,R3                                                        
         MVC   FVIFLD(50),BXLTAG1                                               
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DISPLAY A XML TAG FIELD                                             *         
***********************************************************************         
                                                                                
VALXML   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BXLELQ',(R2)),0                  
         CLI   12(R1),0            DOES ELEMENT ALREADY EXIST?                  
         BE    VALX050                                                          
         CLI   FVILEN,0            NOS ANYTHING INPUT?                          
         BE    EXITOK              NO                                           
                                                                                
         XC    BOELEM,BOELEM                                                    
                                                                                
         USING BXLELD,R3                                                        
         LA    R3,BOELEM                                                        
         MVI   BXLEL,BXLELQ                                                     
         MVI   BXLLN,BXLLNQ                                                     
         LLC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BXLTAG1(0),FVIFLD   MOVE COMPANY LIST BACK IN                    
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),(R2),BXLELD                        
         B     EXITOK                                                           
                                                                                
VALX050  DS    0H                                                               
         CLI   FVILEN,0            WAS ANYTHING INPUT?                          
         BH    VALX070             NO                                           
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('BXLELQ',(R2)),0                  
         B     EXITOK                                                           
                                                                                
VALX070  L     R3,12(R1)                                                        
         XC    BOELEM,BOELEM       MOVE IN FIXED PART OF ELEMENT                
         MVC   BOELEM(BXLLNQ),BXLELD                                            
                                                                                
         LA    R3,BOELEM           NOW USE THIS AS ELEMENT                      
         MVC   BXLTAG1,BCSPACES                                                 
*                                                                               
         LLC   RF,FVILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BXLTAG1(0),FVIFLD   MOVE COMPANY LIST BACK IN                    
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('BXLELQ',(R2)),0                  
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),(R2),BXLELD                        
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
*MN SPEC-36440                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
                                                                                
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
                                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
                                                                                
FLST     MVC   IOKEY(L'BANKEY),THIS.BANKRECD                                    
         L     R1,=A(XOGENDIR+XOHIGH+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    NLST02                                                           
         B     EXITL                                                            
                                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
                                                                                
X        USING BANKRECD,IOKEY                                                   
NLST     L     R1,=A(XOGENDIR+XOSEQ+XIO1)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
*                                                                               
NLST02   CLI   X.BANKTYP,BANKTYPQ                                               
         BNE   EXITL                                                            
         CLI   X.BANKSUB,BANKSFQ                                                
         BNE   EXITL                                                            
         CLI   X.BANKLIN#,0                                                     
         BE    NLST                                                             
         CLI   X.BANKCOL#,0                                                     
         BE    NLST                                                             
*                                                                               
         MVC   THIS.BANKRECD(BANKLEN),IOKEY   WE WANT THIS KEY                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
                                                                                
         LTORG                                                                  
EOF      EQU   X'FF'                                                            
*                                                                               
FRCTAB   DS    0F                                                               
         DC    CL8'BASE    ',AL1(00,0,0,0)                                      
         DC    CL32'BASE RECORD                    '                            
FRCTAB2  DC    CL8'THDR    ',AL1(01,0,0,0)                                      
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
KEYWTAB  DS    0F                                                               
         DC    C'BK#CLI    ',AL1(BK#CLI)                                        
         DC    C'BK#PROD   ',AL1(BK#PROD)                                       
         DC    C'BK#JOB    ',AL1(BK#JOB)                                        
         DC    C'BK#ACC#   ',AL1(BK#ACC#)                                       
         DC    C'BK#TRN#   ',AL1(BK#TRN#)                                       
         DC    C'BK#CHK#   ',AL1(BK#CHK#)                                       
         DC    C'BK#VAR    ',AL1(BK#VAR)                                        
         DC    C'BK#CUS#   ',AL1(BK#CUS#)                                       
         DC    C'BK#RTE#   ',AL1(BK#RTE#)                                       
         DC    C'BK#INV#   ',AL1(BK#INV#)                                       
         DC    C'BK#INVDS  ',AL1(BK#INVDS)                                      
         DC    C'BK#PTYP   ',AL1(BK#PTYP)                                       
         DC    C'BK#ENTID  ',AL1(BK#ENTID)                                      
         DC    C'BK#ACTCD  ',AL1(BK#ACTCD)                                      
         DC    C'BK#ADDR   ',AL1(BK#ADDR)                                       
         DC    C'BK#CTY    ',AL1(BK#CTY)                                        
         DC    C'BK#ST     ',AL1(BK#ST)                                         
         DC    C'BK#ZIP    ',AL1(BK#ZIP)                                        
         DC    C'BK#FAX#   ',AL1(BK#FAX#)                                       
         DC    C'BK#EML    ',AL1(BK#EML)                                        
         DC    C'BK#TIME   ',AL1(BK#TIME)                                       
         DC    C'BK#STY#   ',AL1(BK#STY#)                                       
         DC    C'BK#MCDE   ',AL1(BK#MCDE)                                       
         DC    C'BK#CTRY   ',AL1(BK#CTRY)                                       
         DC    C'BK#EST#   ',AL1(BK#EST#)                                       
         DC    C'BK#CHKTY  ',AL1(BK#CHKTY)                                      
         DC    C'BK#RLEN   ',AL1(BK#RLEN)                                       
         DC    C'BK#TVEN   ',AL1(BK#TVEN)                                       
         DC    C'BK#OVRF   ',AL1(BK#OVRF)                                       
         DC    C'BK#TREF   ',AL1(BK#TREF)                                       
         DC    C'BK#NETA   ',AL1(BK#NETA)                                       
         DC    C'BK#GRSA   ',AL1(BK#GRSA)                                       
         DC    C'BK#CSDA   ',AL1(BK#CSDA)                                       
         DC    C'BK#CHKA   ',AL1(BK#CHKA)                                       
         DC    C'BK#NETTA  ',AL1(BK#NETTA)                                      
         DC    C'BK#GRSTA  ',AL1(BK#GRSTA)                                      
         DC    C'BK#CSDTA  ',AL1(BK#CSDTA)                                      
         DC    C'BK#VOIDA  ',AL1(BK#VOIDA)                                      
         DC    C'BK#BTOT   ',AL1(BK#BTOT)                                       
         DC    C'BK#BTOTA  ',AL1(BK#BTOTA)                                      
         DC    C'BK#TRNDT  ',AL1(BK#TRNDT)                                      
         DC    C'BK#ACTDT  ',AL1(BK#ACTDT)                                      
         DC    C'BK#CHKDT  ',AL1(BK#CHKDT)                                      
         DC    C'BK#INVDT  ',AL1(BK#INVDT)                                      
         DC    C'BK#PERD   ',AL1(BK#PERD)                                       
         DC    C'BK#PERDT  ',AL1(BK#PERDT)                                      
         DC    C'BK#TODDT  ',AL1(BK#TODDT)                                      
         DC    C'BK#RECCN  ',AL1(BK#RECCN)                                      
         DC    C'BK#TRNCN  ',AL1(BK#TRNCN)                                      
         DC    C'BK#VDRCN  ',AL1(BK#VDRCN)                                      
         DC    C'BK#HDRCN  ',AL1(BK#HDRCN)                                      
         DC    C'BK#CHKCN  ',AL1(BK#CHKCN)                                      
         DC    C'BK#CHKS   ',AL1(BK#CHKS)                                       
         DC    C'BK#BCNT   ',AL1(BK#BCNT)                                       
         DC    C'BK#BCNTA  ',AL1(BK#BCNTA)                                      
         DC    C'BK#BLKC   ',AL1(BK#BLKC)                                       
         DC    C'BK#JOBNM  ',AL1(BK#JOBNM)                                      
         DC    C'BK#PAYNM  ',AL1(BK#PAYNM)                                      
         DC    C'BK#CPYNM  ',AL1(BK#CPYNM)                                      
         DC    C'BK#PCTNM  ',AL1(BK#PCTNM)                                      
         DC    C'BK#BNKNM  ',AL1(BK#BNKNM)                                      
         DC    C'BK#CLINM  ',AL1(BK#CLINM)                                      
         DC    C'BK#MEDNM  ',AL1(BK#MEDNM)                                      
         DC    C'BK#CTRNM  ',AL1(BK#CTRNM)                                      
         DC    C'BK#PRDNM  ',AL1(BK#PRDNM)                                      
         DC    C'BK#PUBNM  ',AL1(BK#PUBNM)                                      
         DC    C'BK#IDINM  ',AL1(BK#IDINM)                                      
         DC    C'BK#FSEQ#  ',AL1(BK#FSEQ#)                                      
         DC    C'BK#BSEQ#  ',AL1(BK#BSEQ#)                                      
         DC    C'BK#TSEQ#  ',AL1(BK#TSEQ#)                                      
         DC    C'BK#RSEQ#  ',AL1(BK#RSEQ#)                                      
         DC    C'BK#PUBIN  ',AL1(BK#PUBIN)                                      
         DC    C'BK#NMEIN  ',AL1(BK#NMEIN)                                      
         DC    C'BK#REQDT  ',AL1(BK#REQDT)                                      
         DC    C'BK#REQD2  ',AL1(BK#REQD2)                                      
         DC    C'BK#BLK1   ',AL1(BK#BLK1)                                       
         DC    C'BK#BLK2   ',AL1(BK#BLK2)                                       
         DC    C'BK#LOGO   ',AL1(BK#LOGO)                                       
         DC    C'BK#RADC   ',AL1(BK#RADC)                                       
         DC    C'BK#SIG    ',AL1(BK#SIG)                                        
         DC    C'BK#PHN    ',AL1(BK#PHN)                                        
         DC    C'BK#RMTDV  ',AL1(BK#RMTDV)                                      
         DC    C'BK#CKDIG  ',AL1(BK#CKDIG)                                      
         DC    C'BK#ENTR#  ',AL1(BK#ENTR#)                                      
         DC    C'BK#FILL   ',AL1(BK#FILL)                                       
         DC    C'BK#FLIDM  ',AL1(BK#FLIDM)                                      
         DC    C'BK#GEN#   ',AL1(BK#GEN#)                                       
         DC    C'BK#IADDR  ',AL1(BK#IADDR)                                      
         DC    C'BK#ICTY   ',AL1(BK#ICTY)                                       
         DC    C'BK#IST    ',AL1(BK#IST)                                        
         DC    C'BK#IZIP   ',AL1(BK#IZIP)                                       
         DC    C'BK#ICTRY  ',AL1(BK#ICTRY)                                      
         DC    C'BK#ICTRN  ',AL1(BK#ICTRN)                                      
         DC    C'BK#IDICD  ',AL1(BK#IDICD)                                      
         DC    C'BK#MOS    ',AL1(BK#MOS)                                        
         DC    C'BK#CNTR   ',AL1(BK#CNTR)                                       
         DC    C'BK#RECT   ',AL1(BK#RECT)                                       
         DC    C'BK#RECTL  ',AL1(BK#RECTL)                                      
         DC    C'BK#PCTY   ',AL1(BK#PCTY)                                       
         DC    C'BK#OFFCT  ',AL1(BK#OFFCT)                                      
         DC    C'BK#SWIFT  ',AL1(BK#SWIFT)                                      
         DC    C'BK#IACC#  ',AL1(BK#IACC#)                                      
         DC    C'BK#IBKNM  ',AL1(BK#IBKNM)                                      
         DC    C'BK#IBKAD  ',AL1(BK#IBKAD)                                      
         DC    C'BK#IBCTY  ',AL1(BK#IBCTY)                                      
         DC    C'BK#IBST   ',AL1(BK#IBST)                                       
         DC    C'BK#IBZIP  ',AL1(BK#IBZIP)                                      
         DC    C'BK#IBCTR  ',AL1(BK#IBCTR)                                      
         DC    C'BK#IBCTN  ',AL1(BK#IBCTN)                                      
         DC    C'BK#REPVN  ',AL1(BK#REPVN)                                      
         DC    C'BK#REPV2  ',AL1(BK#REPV2)                                      
         DC    C'BK#REPV3  ',AL1(BK#REPV3)                                      
*SPEC-40627                                                                     
         DC    C'BK#CKDLV  ',AL1(BK#CKDLV)                                      
         DC    C'BK#TRNCT  ',AL1(BK#TRNCT)                                      
         DC    C'BK#CTSTZ  ',AL1(BK#CTSTZ)                                      
*SPEC-40627                                                                     
*MN SPEC-46328                                                                  
         DC    C'BK#SCFF1  ',AL1(BK#SCFF1)                                      
         DC    C'BK#SCFF2  ',AL1(BK#SCFF2)                                      
         DC    C'BK#SCFF3  ',AL1(BK#SCFF3)                                      
         DC    C'BK#SCFF4  ',AL1(BK#SCFF4)                                      
         DC    C'BK#SCFF5  ',AL1(BK#SCFF5)                                      
         DC    C'BK#SCFF6  ',AL1(BK#SCFF6)                                      
         DC    C'BK#SCFF7  ',AL1(BK#SCFF7)                                      
         DC    C'BK#SCFF8  ',AL1(BK#SCFF8)                                      
*MN SPEC-46328                                                                  
         DC    X'FF'                                                            
*                                                                               
DATETAB  DS    0F                                                               
         DC    CL10'YYMMDD    ',AL1(32),XL1'00'                                 
         DC    CL10'MMDDYY    ',AL1(32),XL1'01'                                 
         DC    CL10'DDMMYY    ',AL1(32),XL1'04'                                 
         DC    CL10'MM/DD/YY  ',AL1(32),XL1'05'                                 
*                                                                               
         DC    CL10'MMMDD     ',AL1(07),XL1'00'                                 
         DC    CL10'MMM DD    ',AL1(07),XL1'09'                                 
*                                                                               
         DC    CL10'MMMDD/YY  ',AL1(08),XL1'00'                                 
*                                                                               
*MN                                                                             
*        CHECK THIS FOR BK#MOS                                                  
*        DC    CL10'MMM/YY    ',AL1(06),XL1'00'                                 
*MN                                                                             
         DC    CL10'MMM/YY    ',AL1(09),XL1'00'                                 
*                                                                               
         DC    CL10'MM/DD/YY  ',AL1(10),XL1'00'                                 
*                                                                               
         DC    CL10'YYYYMMDD  ',AL1(20),XL1'00'                                 
         DC    CL10'MMDDYYYY  ',AL1(20),XL1'02'                                 
         DC    CL10'MM/DD/YYYY',AL1(20),XL1'07'                                 
*                                                                               
         DC    CL10'MMMDD/YYYY',AL1(21),XL1'00'                                 
         DC    CL10'JULIAN    ',AL1(21),XL1'06'                                 
*                                                                               
         DC    CL10'YYYY-MM-DD',AL1(23),XL1'00'                                 
         DC    CL10'YYYY/MM/DD',AL1(23),XL1'08'                                 
*                                                                               
         DC    CL10'FDAYINPUT ',AL1(20),XL1'30'                                 
         DC    CL10'LDAYINPUT ',AL1(20),XL1'31'                                 
         DC    CL10'FDAYNEXTMN',AL1(20),XL1'32'                                 
         DC    CL10'LDAYNEXTMN',AL1(20),XL1'33'                                 
         DC    CL10'FDAYPRIORM',AL1(20),XL1'34'                                 
         DC    CL10'LDAYPRIORM',AL1(20),XL1'35'                                 
*                                                                               
         DC    X'FF'                                                            
*                                                                               
FRCTABD  DSECT                                                                  
FRCNAM   DS    CL8                                                              
FRCEQU   DS    X                                                                
         DS    XL3                                                              
FRCDSC   DS    CL32                                                             
FRCTABLQ EQU   *-FRCTABD                                                        
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
FFFFFFFF EQU   X'FFFFFFFF'                                                      
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
*                                                                               
KEYWTBD  DSECT                                                                  
KEYWDSC  DS    CL10                DESCRIPTVE KEYWORD                           
KEYWNUM  DS    XL1                 EQUIVELENT KEYWORD NUMBER                    
KEYWLNQ  EQU   *-KEYWTBD                                                        
*                                                                               
DATETABD DSECT                                                                  
DATTBFRM DS    CL10                DATE FORMAT                                  
DATTBTYP DS    X                   FLDTYPE                                      
DATTBOVR DS    X                   FLDOVR                                       
DATTBLNQ EQU   *-DATETABD                                                       
*                                                                               
FIL2B    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
                                                                                
         LTORG                                                                  
*                                                                               
         DS    0D                                                               
DCLIST   DCDDL CT#YES,4,L                                                       
         DCDDL GE#NO,4,L                                                        
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
                                                                                
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
MNTDISP  DS    H                                                                
KEYCODE  DS    C                                                                
*                                                                               
A5FEL    DS    A                                                                
AVBNK    DS    CL(L'BANKCDE)                                                    
ANYELEMS DS    CL1                 DO WE HAVE ANY ELEMENTS                      
*                                                                               
BYTE     DS    XL1                                                              
*                                                                               
DSLISTU  DS    0D                                                               
UE@YES   DS    CL4                                                              
UE@NO    DS    CL4                                                              
*                                                                               
DSLISTL  DS    0D                                                               
LC@YES   DS    CL4                                                              
LC@NO    DS    CL4                                                              
*                                                                               
WORK     DS    CL64                                                             
                                                                                
***********************************************************************         
* INCLUDES                                                            *         
***********************************************************************         
                                                                                
*        GEGENBNK                                                               
       ++INCLUDE GEGENBNK                                                       
*        CTFILWORK                                                              
       ++INCLUDE CTFILWORK                                                      
*        ACGENFILE                                                              
       ++INCLUDE ACGENFILE                                                      
*        ACKEYWRD                                                               
       ++INCLUDE ACKEYWRD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040CTFIL2B   09/03/20'                                      
         END                                                                    
