*          DATA SET ACFIL11    AT LEVEL 016 AS OF 08/23/17                      
*&&      SET   NOP=N                                                            
*PHASE T62311A,*                                                                
         TITLE 'CHECK AUTHORIZATION OBJECT VERSON'                              
         SPACE 2                                                                
FIL11    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL11**,R7,RR=RE                                              
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
         LH    R6,=Y(TWUSER-TWAD)                                               
         AR    R6,RA                                                            
         USING TWUSER,R6                                                        
         L     R2,=A(DCLIST)                                                    
         A     R2,BORELO                                                        
         GOTO1 VDICTAT,BOPARM,C'LU  ',(R2),DSLISTU                              
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
EXITMEDI MVC   FVMSGNO,=AL2(AE$MEDI)                                            
         B     EXITL               MISSING AGENCY LEVEL EDI820 BANK REC         
EXITMBPR MVC   FVMSGNO,=AL2(AE$MBPRF)                                           
         B     EXITL               MISSING BANK PROFILE FOR ACCOUNT             
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS R1 TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 2                                                                
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO                                                       
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
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
         USING CHARECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
*        DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    CHAKEY,CHAKEY     INITIALIZE KEY OF RECORD                       
         MVI   CHAKTYP,CHAKTYPQ    10 RECORD                                    
         MVC   CHAKCPY,CUABIN      CONNECTED ID                                 
         MVI   CHAKUNT,C'S'        ALWAYS UNIT S                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    CHAKEY,CHAKEY     INITIALIZE KEY OF RECORD                       
         MVI   CHAKTYP,CHAKTYPQ    10 RECORD                                    
         MVC   CHAKCPY,CUABIN      CONNECTED ID                                 
         MVI   CHAKUNT,C'S'        ALWAYS UNIT S                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
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
         CLC   CDOPTION,SDOPTION   HAS OPTIONS BEEN CHANGED?                    
         BE    EXITOK                                                           
         MVI   LSSCIND1,LSSCIFLT   REFRESH LIST                                 
         MVC   SDOPTION(SDOPTSL),CDOPTION                                       
         B     EXITOK                                                           
         DROP  R2                                                               
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
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING CHARECD,R2                                                       
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
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING CHARECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(CHK#CPY),AL4(CPYCODE)   COMPANY CODE                         
         DC    AL2(CHK#CPYNM),AL4(CPYNM)   COMPANY NAME                         
         DC    AL2(CHK#LDG),AL4(DOLEDG)    LEDGER                               
         DC    AL2(CHK#LDGNM),AL4(LEDGNM)  LEDGER NAME                          
         DC    AL2(CHK#UID),AL4(DOUSRID)   USER ID                              
         DC    AL2(CHK#NCHK),AL4(NCHECK)   NEXT CHECK NUMBER                    
         DC    AL2(CHK#BACCT),AL4(BACCT)   BANK ACCOUNT                         
         DC    AL2(CHK#OFCLI),AL4(OFFCLI)  OFFICE/CLIENT FILTER                 
*        DC    AL2(CHK#LCHK),AL4(LCHECK)   LAST CHECK IN STOCK                  
         DC    AL2(CHK#SORT),AL4(DOSORT)   SORT SEQUENCE                        
         DC    AL2(CHK#POFF),AL4(POFFC)    OFFICE FOR CASH POSTING              
         DC    AL2(CHK#STAT),AL4(DOSTATUS) STATUS OF RECORD                     
         SPACE 2                                                                
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL11    CSECT                                                                  
*                                                                               
         SPACE 1                                                                
*        DC    X'01',AL1(OCNSPRTQ),CL10'DIRECT'   REMOVED AS OF 5/23/03         
STATLST  DC    X'01',AL1(OCNSOFFR),CL10'OFFICE'                                 
         DC    X'01',AL1(OCNSPRTN),CL10'NUMBER'                                 
         DC    X'01',AL1(OCNSDREG),CL10'DIREG'                                  
*        DC    X'01',AL1(OCNSAUTH),CL10'AUTH'                                   
         DC    X'01',AL1(OCNSSHUT),CL10'SHUTTLE'                                
         DC    X'01',AL1(OCNSSOON),CL10'SOON'                                   
         DC    X'01',AL1(OCNSLOCL),CL10'LOCAL'                                  
         DC    X'01',AL1(0),CL10'STACK'                                         
         DC    X'FF'                                                            
*                                                                               
*        DC    X'01',AL1(OCNSMICR),CL10'MICR'    NO LONGER VALID 5/04           
*        DC    X'01',AL1(OCNSFTP),CL10'FTP'      NO LONGER VALID 5/05           
STATLST2 DC    X'01',AL1(OCNSLBLT),CL10'LASER'   BOTTOM LINE TECHNOLGY          
         DC    X'01',AL1(OCNS820),CL10'EDI820'   820 EDI STANDARD CHECK         
         DC    X'01',AL1(OCNSDFIL),CL10'FLATFILE'   DDS FLATFILE                
*MNDRFT                                                                         
*        DC    X'01',AL1(OCNSDRFT),CL10'STACKDRAFT'                             
*        DC    X'01',AL1(OCNSLIVE),CL10'STACKLIVE '                             
*MNDRFT                                                                         
         DC    X'FF'                                                            
*                                                                               
STATLASR DC    X'01',AL1(OCNRED),CL10'LASERRED'                                 
         DC    X'01',AL1(OCNBLU),CL10'LASERBLU'                                 
         DC    X'01',AL1(OCNGRN),CL10'LASERGRE'                                 
         DC    X'01',AL1(OCNGLD),CL10'LASERGLD'                                 
         DC    X'01',AL1(OCNBRN),CL10'LASERBRN'                                 
         DC    X'01',AL1(OCNVIO),CL10'LASERVIO'                                 
         DC    X'01',AL1(OCNWSP),CL10'LASERWSP'                                 
*MN SPEC-9590                                                                   
         DC    X'01',AL1(OCNWSL),CL10'LASERWSL'                                 
*MN SPEC-9590                                                                   
         DC    X'FF'                                                            
*                                                                               
SORTAB   DS    0CL9                                                             
         DC    C'A',CL8'AMOUNT'                                                 
         DC    C'C',CL8'CODE'                                                   
         DC    C'D',CL8'DISCOUNT'                                               
         DC    C'N',CL8'NAME'                                                   
         DC    X'FF'                                                            
*                                                                               
FIL11    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPANY CODE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CPYCODE  LA    RF,CPYTBL                                                        
         B     ITER                                                             
*                                                                               
CPYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPY)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE COMPANY CODE (DDS ONLY)                                           
***********************************************************************         
         SPACE 1                                                                
DISCPY   GOTO1 VHEXOUT,BOPARM,CUABIN,FVIFLD,L'ACTKCPY,0                         
         MVI   FVILEN,2                                                         
         OI    FVOIND,FVOXMT       TRANSMIT                                     
         OI    FVOIND,FVAPROT      PROTECT                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPANY NAME                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CPYNM    LA    RF,CPYNTBL                                                       
         B     ITER                                                             
*                                                                               
CPYNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPYN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COMPANY NAME (DDS ONLY)                                               
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R4                                                       
DISCPYN  LA    R4,IOKEY                                                         
         MVC   IOKEY,BCSPACES                                                   
         MVC   CPYKCPY,CUABIN      COMPANY CODE                                 
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0            NAME ON RECORD?                              
         BNE   EXITOK              NO                                           
*                                                                               
         USING NAMELD,RF                                                        
         L     RF,12(R1)                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EXMVC R1,FVIFLD,NAMEREC                                                
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEDGER                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DOLEDG   LA    RF,LEDGTBL                                                       
         B     ITER                                                             
*                                                                               
LEDGTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEDG)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLEDG)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDLDG)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFVLDG)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTLDG)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LEDGER                                                            
***********************************************************************         
         SPACE 1                                                                
DISLEDG  MVC   FVIFLD(L'CHAKLDG),CHAKLDG                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE LEDGER                                                           
***********************************************************************         
         SPACE 1                                                                
VALLEDG  CLI   FVILEN,0                                                         
         BE    EXITNO              MISSING INPUT                                
         CLI   FVILEN,1                                                         
         BNE   EXITNV                                                           
         CLI   CSACT,A#DIS         SKIP CHECK FOR DISPLAY                       
         BE    VLEDG10                                                          
         CLI   FVIFLD,C'P'         ONLY VALID FOR PAYABLE LGRS                  
         BE    VLEDG10             SP,SQ,SS,ST,SU,SV,SW,SX,SY                   
         CLI   FVIFLD,C'Q'                                                      
         BE    VLEDG10                                                          
         CLI   FVIFLD,C'Y'                                                      
         BH    *+12                                                             
         CLI   FVIFLD,C'S'                                                      
         BNL   VLEDG10                                                          
         MVC   FVMSGNO,=AL2(AE$NOPAY)                                           
         B     EXITL                                                            
*                                                                               
VLEDG10  ZIC   R1,FVXLEN                                                        
         EXMVC R1,BCWORK,FVIFLD                                                 
         BAS   RE,GETLEDG                                                       
         BNE   EXITL                                                            
         MVC   CHAKLDG,FVIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LEDGER AS A FILTER                                                
***********************************************************************         
         SPACE 1                                                                
DFDLDG   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FVIFLD,FLTIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE LEDGER AS A FILTER                                               
***********************************************************************         
         SPACE 1                                                                
DFVLDG   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FLTIFLD,FVIFLD                                                
         EXMVC R1,CHAKLDG,FVIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LEDGER                                                       
***********************************************************************         
         SPACE 1                                                                
DOFLTLDG CLI   FLTIFLD,0                                                        
         BE    EXITOK              NOTHING TO FILTERON                          
         CLC   CHAKLDG,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* GET LEDGER RECORD                                                             
***********************************************************************         
         USING ACTRECD,R5                                                       
GETLEDG  NTR1                                                                   
         LA    R5,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVI   ACTKUNT,C'S'        ALWAYS UNIT S                                
         MVC   ACTKLDG,BCWORK      MOVE IN LEDGER                               
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEDGER NAME                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LEDGNM   LA    RF,LEDGNMTB                                                      
         B     ITER                                                             
*                                                                               
LEDGNMTB DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEDGN)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LEDGER NAME                                                       
***********************************************************************         
         SPACE 1                                                                
DISLEDGN MVC   BCWORK(L'CHAKLDG),CHAKLDG                                        
         BAS   RE,GETLEDG          GET THE LEDGER RECORD                        
         L     R1,AIO1                                                          
         GOTO1 AGETNAM                                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR USER ID                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DOUSRID  LA    RF,USERTBL                                                       
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
USERTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISUSER)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALUSER)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDUSR)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFVUSR)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTUSR)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE USER ID                                                           
***********************************************************************         
         SPACE 1                                                                
DISUSER  CLC   TLKID,EFFS          IF DUMMY REC IGNORE                          
         BE    EXITOK                                                           
         MVI   CTBYTE,0            READ FOR ALPHA                               
         MVC   BCWORK(L'TLKID),TLKID                                            
         BAS   RE,GETID                                                         
         ZIC   R1,SVLEN            SVLEN IS READY FOR EXECUTE                   
         EXMVC R1,FVIFLD,BCWORK                                                 
         OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR  IF SOON OR LOCAL           
         BZ    EXITOK              DATE THAN CAN'T CHANGE THIS FIELD            
         OI    FVATRB,FVAPROT      SO PROTECT                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE USER ID                                                          
***********************************************************************         
         SPACE 1                                                                
*        OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR  IF SOON OR LOCAL           
*        BNZ   EXITNV              DATE THAN CAN'T CHANGE THIS FIELD            
VALUSER  CLI   FVILEN,0            USER DELETING THIS ENTRY?                    
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   TELL CONTROLLER                              
         B     EXITOK                                                           
*                                                                               
         OC    TLKID,TLKID         IF ID ALREADY THERE MUST BE CHANGING         
         BNZ   VALUS10             SO DON'T TREAT AS A NEW ENTRY                
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1             FIRST LIST REC #                          
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
*MN SPEC-15454                                                                  
*        CHI   RE,50               27 ENTRIES MAX(TO KEEP REC AT 2000)          
         CHI   RE,100              100 ENTRY MAX TO CONTROL # OF RECS           
*MN SPEC-15454                                                                  
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$CHKMX)                                           
         B     EXITL                                                            
*                                                                               
VALUS10  XC    TLKID,TLKID                                                      
         MVC   TLRLEN,=AL2(TLLNQ)                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVI   CTBYTE,1            READ FOR BINARY                              
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,BCWORK,FVIFLD                                                 
         BAS   RE,GETID                                                         
         BNE   EXITNV                                                           
         CLC   SVALPH,CUAALF       COMPARE ALPHA ID OF USER ID TO               
         BE    *+14                ALPHA ID OF SIGN ON ID                       
         MVC   FVMSGNO,=AL2(AE$INVID)                                           
         B     EXITL                                                            
         ZIC   R1,SVLEN                                                         
         EXMVC R1,TLKID,BCWORK                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE USER ID AS A FILTER                                               
***********************************************************************         
         SPACE 1                                                                
DFDUSR   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         MVC   FVIFLD(8),FLTIFLD   8 IS MAX                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE USER ID AS A FILTER                                              
***********************************************************************         
         SPACE 1                                                                
DFVUSR   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FLTIFLD,FVIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR USER ID                                                      
***********************************************************************         
         SPACE 1                                                                
DOFLTUSR CLI   FLTIFLD,0                                                        
         BE    EXITOK              NOTHING TO FILTERON                          
         OC    FLTIFLD,BCSPACES                                                 
         MVI   CTBYTE,0            READ FOR ALPHA                               
         MVC   BCWORK(L'TLKID),TLKID                                            
         BAS   RE,GETID                                                         
         ZIC   R1,SVLEN            SVLEN IS READY FOR EXECUTE                   
         EXCLC R1,FLTIFLD,BCWORK                                                
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* GETID- READ CONTROL FILE FOR ID                                               
***********************************************************************         
         USING CTIREC,R5                                                        
GETID    NTR1                                                                   
         MVC   SVALPH,BCSPACES                                                  
         LA    R5,IOKEY                                                         
         MVC   BOWORK2(L'IOKEY),IOKEY      SAVE KEY                             
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         CLI   CTBYTE,0            READ FOR ALPHA                               
         BNE   GET2                                                             
         MVC   CTIKID+8(2),BCWORK                                               
         B     GET4                                                             
*                                                                               
GET2     MVC   CTIKID,BCSPACES       READ FOR NUMBER                            
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,CTIKID,BCWORK                                                 
*                                                                               
GET4     LHI   R1,XOCONFIL+XOREAD+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
*                                                                               
         CLI   CSACT,A#LST         FOR LIST ONLY                                
         BNE   GET5                                                             
         MVC   IOKEY,BOWORK2       RESTORE LIST KEY                             
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                 RESTORE READ SEQUENCE                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GET5     MVC   BCWORK,BCSPACES                                                  
         L     RF,AIO2                                                          
         LA    RF,CTIDATA-CTIREC(RF) POINT TO FIRST ELEMENT                     
*                                                                               
GET8     CLI   0(RF),0             NO ELEMENT                                   
         BE    EXITOK                                                           
         CLI   0(RF),X'02'         ID DESCRIPTION ELEMENT                       
         BE    GET10                                                            
         CLI   0(RF),X'06'         ID AGENCY ALPHA ELEMENT                      
         BE    GET20                                                            
GET8NX   ZIC   R0,1(RF)                                                         
         AR    RF,R0                                                            
         B     GET8                                                             
*                                                                               
         USING CTDSCD,RF                                                        
GET10    ZIC   R1,CTDSCLEN                                                      
         AHI   R1,-3               SUBTRACT 2 FOR OVERHEAD+1 FOR EX             
         EXMVC R1,BCWORK,CTDSC                                                  
         STC   R1,SVLEN                                                         
         B     GET8NX                                                           
*                                                                               
         USING CTAGYD,RF                                                        
GET20    MVC   SVALPH,CTAGYID                                                   
         B     GET8NX                                                           
         DROP  R5,RF                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NEXT CHECK NUMBER                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NCHECK   LA    RF,NCHKTBL                                                       
         B     ITER                                                             
*                                                                               
NCHKTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISNCHK)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNCHK)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE NEXT CHECK NUMBER                                                 
***********************************************************************         
         SPACE 1                                                                
DISNCHK  CLC   TLKAFT,EFFS         IGNORE DUMMY TSAR ENTRY                      
         BE    EXITOK                                                           
         MVC   FVIFLD(L'TLKAFT),TLKAFT                                          
         CLI   TLKNTYP,0                                                        
         BE    *+10                                                             
         MVC   FVIFLD(1),TLKNTYP   DISPLAY NUMBER TYPE                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE NEXT CHECK NUMBER                                                
***********************************************************************         
         SPACE 1                                                                
VALNCHK  XC    TLKBEF,TLKBEF                                                    
         XC    TLKAFT,TLKAFT                                                    
         XC    TLKNTYP,TLKNTYP                                                  
         CLI   FVILEN,0                                                         
         BNE   VNCHK10                                                          
         CLC   TLKID,EFFS          IF NOTHING IN THE ID FIELD                   
         BE    EXITOK              THEN OKAY FOR NO INPUT TO CHECK FLD          
         OC    TLKID,TLKID                                                      
         BZ    EXITOK                                                           
         B     EXITNO              IF THERE'S AN ID NEED INPUT                  
VNCHK10  CLI   FVILEN,6            CHECK NUMBER MUST BE 6 LONG                  
         BNE   EXITNV                                                           
         MVC   BCWORK(5),=5X'F0'                                                
         MVZ   BCWORK(5),FVIFLD+1                                               
         CLC   BCWORK(5),=5X'F0'   2-6 MUST BE NUMERIC                          
         BNE   EXITNV                                                           
         MVC   TLKBEF,FVIFLD                                                    
         MVC   TLKAFT,FVIFLD                                                    
         CLI   FVIFLD,C'0'         CHECK FOR SPECIAL LETTER                     
         BNL   EXITOK                                                           
         MVC   TLKNTYP,FVIFLD                                                   
         MVI   TLKBEF,C'0'                                                      
         MVI   TLKAFT,C'0'                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BANK ACCOUNT                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BACCT    LA    RF,BACCTBL                                                       
         B     ITER                                                             
*                                                                               
BACCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISBACC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBACC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCBACC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE BANK ACCOUNT                                                      
***********************************************************************         
         SPACE 1                                                                
DISBACC  CLC   TLKBACC,EFFS        IGNORE DUMMY TSAR ENTRY                      
         BE    EXITOK                                                           
         MVC   FVIFLD(L'OCNBANKA),TLKBACC+3                                     
         CLC   TLKBACC(2),=C'SC'                                                
         BNE   *+10                                                             
         MVC   FVIFLD(L'OCNBANKA),TLKBACC                                       
         OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR  IF SOON OR LOCAL           
         BZ    EXITOK              DATE PRESENT CAN'T CHANGE THIS FLD           
         OI    FVATRB,FVAPROT      SO PROTECT                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE BANK ACCOUNT                                                     
***********************************************************************         
         SPACE 1                                                                
*        OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR  IF SOON OR LOCAL           
*        BNZ   EXITNV              DATE THAN CAN'T CHANGE THIS FIELD            
VALBACC  CLI   FVILEN,0                                                         
         BNE   VBACC10                                                          
         CLC   TLKID,EFFS          IF NO ENTRY TO ID THEN OKAY                  
         BE    EXITOK              FOR NO ENTRY TO ACCT                         
         OC    TLKID,TLKID                                                      
         BZ    EXITOK                                                           
         B     EXITNO              IF THERE'S AN ID NEED INPUT                  
*                                                                               
VBACC10  BAS   RE,GETACC                                                        
         BNE   VBACC20             INVALID ACCOUNT                              
*                                                                               
         USING ACTRECD,R3                                                       
         L     R3,AIO1                                                          
         TM    ACTRSTAT,ACTSABLP    LOW LEVEL ACCOUNT?                          
         BZ    *+14                NO INVALID ACCT FOR POSTING                  
         MVC   TLKBACC,IOKEY       MOVE BANK ACCOUNT                            
         B     EXITOK                                                           
*                                                                               
         MVC   FVMSGNO,=AL2(AE$INACP)  INVALID FOR POSTING                      
         B     EXITL                                                            
*                                                                               
VBACC20  MVC   FVXTRA(2),=C'SC'     MOVE ACCOUNT INTO ERROR MSG                 
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FVXTRA+2,FVIFLD                                               
         B     EXITL                                                            
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* SEARCH THE BANK ACCOUNT FIELD                                       *         
***********************************************************************         
SRCBACC  GOTO1 VACSRCHC,BOPARM,(3,FVADDR),ATWA,(0,=C'SC'),ACOM,(0,0)            
         B     EXITOK                                                           
***********************************************************************         
* GET THE BANK ACCOUNT                                                          
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
GETACC   NTR1                                                                   
         LA    R5,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'SC'   ALWAYS SC                                    
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,ACTKACT,FVIFLD                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFICE/CLIENT FILTER                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFCLI   LA    RF,OFCLITBL                                                      
         B     ITER                                                             
*                                                                               
OFCLITBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFCL)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFCL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE OFFICE/CLIENT FILTER                                              
***********************************************************************         
         SPACE 1                                                                
DISOFCL  CLC   TLKFILT,EFFS        IGNORE DUMMY TSAR ENTRY                      
         BE    EXITOK                                                           
         MVC   FVIFLD(L'TLKFILT),TLKFILT                                        
         OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR  IF SOON OR LOCAL           
         BZ    EXITOK              DATE PRESENT CAN'T CHANGE THIS FLD           
         OI    FVATRB,FVAPROT      SO PROTECT                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE OFFICE/CLIENT FILTER                                             
***********************************************************************         
         SPACE 1                                                                
*        OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR  IF SOON OR LOCAL           
*        BNZ   EXITNV              DATE THAN CAN'T CHANGE THIS FIELD            
VALOFCL  XC    TLKFILT,TLKFILT                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVIFLD,C'*'         * MEANS OFFICE FILTER                        
         BNE   VOFCL10             MUST BE CLIENT FILTER                        
         TM    BCCPYST4,CPYSOFF2   CAN'T BE ON NEW OFFICES                      
         BO    EXITNV                                                           
         CLI   FVILEN,2            MUST BE AT LEAST 2 BYTES                     
         BL    EXITNV                                                           
*                                                                               
         USING ACTRECD,R5                                                       
         LA    R5,IOKEY            READ 2D LEDGER REC                           
         MVC   ACTKEY,BCSPACES     FOR 1 BYTE OFFICES                           
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'2D'                                                
         MVC   ACTKACT(1),FVIFLD+1                                              
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         MVC   TLKFILT,FVIFLD                                                   
         B     EXITOK                                                           
*                                                                               
VOFCL10  BAS   RE,GETCLI           READ FOR CLIENT                              
         BNE   EXITL                                                            
         MVC   TLKFILT,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* READ CLIENT RECORD TO VALIDATE                                                
***********************************************************************         
         USING ACTRECD,R5                                                       
GETCLI   NTR1                                                                   
         LA    R5,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'SJ'                                                
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,ACTKACT,FVIFLD   MOVE IN CLIENT                               
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SORT SEQUENCE                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DOSORT   LA    RF,SORTTBL                                                       
         B     ITER                                                             
*                                                                               
SORTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSORT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSORT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE SORT SEQUENCE                                                     
***********************************************************************         
         SPACE 1                                                                
DISSORT  CLC   TLKSORT,EFFS        IGNORE DUMMY TSAR RECORD                     
         LA    RF,SORTAB                                                        
DSORT10  CLI   0(RF),X'FF'                                                      
         BE    DSORT20                                                          
         CLC   TLKSORT,0(RF)                                                    
         BE    *+12                                                             
         LA    RF,L'SORTAB(RF)                                                  
         B     DSORT10                                                          
         MVC   FVIFLD(L'SORTAB-1),1(RF) MOVE IN SORT DESCRIPTION                
         B     DSORT30                                                          
*                                                                               
DSORT20  MVC   FVIFLD(L'AC@NFA66),AC@NFA66   DEFAULT IS '(CODE)'                
*                                                                               
DSORT30  OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR  IF SOON OR LOCAL           
         BZ    EXITOK                       CAN'T CHANGE THIS FIELD             
         OI    FVATRB,FVAPROT               SO PROTECT                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE SORT SEQUENCE                                                    
***********************************************************************         
         SPACE 1                                                                
*        OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR  IF SOON OR LOCAL           
*        BNZ   EXITNV              DATE THAN CAN'T CHANGE THIS FIELD            
VALSORT  XC    TLKSORT,TLKSORT                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVIFLD,C'('         IF DEFAULT '(CODE)' IS DISPLAYED             
         BE    EXITOK              DO NOT ADD TO RECORD                         
         ZIC   R1,FVXLEN                                                        
         LA    RF,SORTAB                                                        
VSORT10  CLI   0(RF),X'FF'                                                      
         BE    EXITNV                                                           
         EXCLC R1,FVIFLD,1(RF)                                                  
         BE    *+12                                                             
         LA    RF,L'SORTAB(RF)                                                  
         B     VSORT10                                                          
         MVC   TLKSORT,0(RF)                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR POSTING OFFICE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
POFFC    LA    RF,POFFCTB                                                       
         B     ITER                                                             
*                                                                               
POFFCTB  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPOFF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPOFF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE POSTING OFFICE                                                    
***********************************************************************         
         SPACE 1                                                                
DISPOFF  CLC   TLKPOFF,EFFS        IGNORE DUMMY TSAR RECORD                     
         MVC   FVIFLD(L'TLKPOFF),TLKPOFF                                        
         OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR IF SOON OR LOCAL            
         BZ    EXITOK                  THEN CAN'T CHANGE THIS FIELD             
         OI    FVATRB,FVAPROT      PROTECT                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE POSTING OFFICE                                                   
***********************************************************************         
         SPACE 1                                                                
*        OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR IF SOON OR LOCAL            
*        BNZ   EXITNV                  THEN CAN'T CHANGE THIS FIELD             
VALPOFF  XC    TLKPOFF,TLKPOFF                                                  
         CLI   FVILEN,0                                                         
         BNE   VPOFF10                                                          
         CLC   TLKID,EFFS          IF NO ID THEN OKAY TO                        
         BE    EXITOK              HAVE NO OFFICE                               
         OC    TLKID,TLKID                                                      
         BZ    EXITOK                                                           
         B     EXITNO              IF THERE'S AN ID NEED INPUT                  
*                                                                               
VPOFF10  TM    BCCPYST4,CPYSOFF2   ON 2 BYTE OFFICES?                           
         BZ    VPOFF20             NO                                           
         USING OFFRECD,R5                                                       
         LA    R5,IOKEY            READ 01 OFFICE REC                           
         MVC   OFFKEY,BCSPACES     FOR 2 BYTE OFFICES                           
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUABIN                                                   
         MVC   OFFKOFF,FVIFLD                                                   
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    VPOFF15                                                          
         MVC   FVMSGNO,=AL2(AE$IVOFF)     INVALID OFFICE                        
         B     EXITL                                                            
VPOFF15  TM    OFFKSTAT,OFFSLIST   CAN'T USE AN OFFICE LIST AS A                
         BZ    VPOFF30             POSTING OFFICE                               
         MVC   FVMSGNO,=AL2(AE$OLNO)                                            
         B     EXITL                                                            
*                                                                               
         USING ACTRECD,R5                                                       
VPOFF20  CLI   FVILEN,1            CAN ONLY BE ONE BYTE                         
         BNE   EXITNV                                                           
         LA    R5,IOKEY            READ 2D LEDGER REC                           
         MVC   ACTKEY,BCSPACES     FOR 1 BYTE OFFICES                           
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'2D'                                                
         MVC   ACTKACT(1),FVIFLD                                                
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     VPOFF30                                                          
*                                                                               
VPOFF30  MVC   TLKPOFF,FVIFLD                                                   
         OC    TLKPOFF,BCSPACES                                                 
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR THE STATUS LINE                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DOSTATUS LA    RF,STATTBL                                                       
         B     ITER                                                             
*                                                                               
STATTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTAT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTAT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE STATUS LINE                                                       
***********************************************************************         
         SPACE 1                                                                
DISSTAT  CLC   TLKSTAT,EFFS        IGNORE DUMMY TSAR RECORD                     
         BE    EXITOK                                                           
         SR    R5,R5                                                            
         MVC   SCANBLK,BCSPACES                                                 
         LA    R3,SCANBLK                                                       
         LA    R4,TLKSTAT          R4=STATUS BYTE 1                             
         LA    R1,STATLST          R1=STATUS TABLE                              
         BAS   RE,BLDSTAT                                                       
*                                                                               
         LA    R4,TLKSTAT2         R4=STATUS BYTE 2                             
         LA    R1,STATLST2                                                      
         BAS   RE,BLDSTAT                                                       
*                                                                               
         CLI   TLKELEN,OCNLN3Q                                                  
         BL    DSTAT20                                                          
         LA    R4,TLKLASR          R4=STATUS BYTE FOR LASER PRINTING            
         LA    R1,STATLASR                                                      
         BAS   RE,BLDSTLSR         BUILD LINE FOR LASER PRINTING                
*                                                                               
         OC    TLKDPSR,TLKDPSR     TEST SOON REGISTRE PENDING                   
         BZ    DSTAT10                                                          
         LA    R4,=C'SOON'                                                      
         LA    R1,TLKDPSR                                                       
         BAS   RE,ADDDATE          ADD THE PENDING DATE TO SOON=                
*                                                                               
DSTAT10  OC    TLKDPLR,TLKDPLR     TEST LOCAL REGISTER PENDING                  
         BZ    DSTAT15                                                          
         LA    R4,=C'LOCAL'                                                     
         LA    R1,TLKDPLR                                                       
         BAS   RE,ADDDATE          ADD THE PENDING DATE TO LOCAL=               
*                                                                               
DSTAT15  OC    TLKSEQN,TLKSEQN     SEQ # FOR SOON (STACK=)                      
         BZ    DSTAT20                                                          
         LR    R3,R5               R3 = NUMBER OF ENTRIES                       
         MH    R3,=H'20'                                                        
         LA    R3,SCANBLK(R3)      R3 = NEXT AVAILABLE ENTRY                    
         MVC   0(20,R3),BCSPACES                                                
         MVC   0(5,R3),=C'STACK'                                                
         SR    R1,R1                                                            
         ICM   R1,3,TLKSEQN        SEQUENCE                                     
         CVD   R1,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  10(5,R3),BODUB1+5(3)   SEQUENCE NUMBER TO UNSCAN BLOCK           
         LA    R5,1(R5)            INCREMENT NUMBER OF ENTRIES                  
*                                                                               
DSTAT20  LTR   R5,R5               UNSCAN INTO THE STATUS FIELD                 
         BZ    DSTAT30                                                          
         LA    RF,FVIHDR                                                        
         GOTO1 VUNSCAN,BOPARM,((R5),SCANBLK),(RF)                               
*                                                                               
DSTAT30  OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR IF SOON OR LOCAL            
         BZ    EXITOK                  THEN CAN'T CHANGE THIS FIELD             
         OI    FVATRB,FVAPROT          SO PROTECT                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD THE STATUS LINE                                                         
* R5 KEEPS NUMBER OF OUTPUT ITEMS                                               
* R3 POINTS TO AVAILABLE SPOT IN SCANBLK                                        
***********************************************************************         
         SPACE 1                                                                
BLDSTAT  NTR1                                                                   
*        LR    R4,R1               SAVE ADDRESS                                 
BLDST05  MVC   BCWORK(1),1(R1)       BIT FROM TABLE                             
         NC    BCWORK(1),0(R4)       STATUS BIT                                 
         BZ    BLDST15             NOT ON                                       
BLDST10  LA    R5,1(R5)            COUNT OUTPUT ITEMS                           
         MVC   0(20,R3),BCSPACES     SPACES TO BLOCK                            
         MVC   0(10,R3),2(R1)      LEFT SIDE TO BLOCK                           
         CLI   0(R1),1             ONE SIDED                                    
         BE    *+10                YES                                          
         MVC   10(10,R3),12(R1)    RIGHT SIDE TO BLOCK                          
         LA    R3,20(R3)                                                        
*                                                                               
BLDST15  CLI   0(R1),1                                                          
         BE    *+8                                                              
         LA    R1,10(R1)                                                        
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   BLDST05                                                          
*                                                                               
         XIT1  REGS=(R3,R5)                                                     
         SPACE 2                                                                
*********************************************************************           
* BUILD THE STATUS LINE FOR LASER INDICATORS                                    
* R5 KEEPS NUMBER OF OUTPUT ITEMS                                               
* R3 POINTS TO AVAILABLE SPOT IN SCANBLK                                        
*********************************************************************           
         SPACE 1                                                                
BLDSTLSR NTR1                                                                   
*        LR    R4,R1               SAVE ADDRESS                                 
BLDLS05  MVC   BCWORK(1),1(R1)       BIT FROM TABLE                             
         CLC   BCWORK(1),0(R4)       STATUS BIT                                 
         BNE   BLDLS10             NOT ON                                       
         LA    R5,1(R5)            COUNT OUTPUT ITEMS                           
         MVC   0(20,R3),BCSPACES     SPACES TO BLOCK                            
         MVC   0(10,R3),2(R1)      LEFT SIDE TO BLOCK                           
         CLI   0(R1),1             ONE SIDED                                    
         BE    *+10                YES                                          
         MVC   10(10,R3),12(R1)    RIGHT SIDE TO BLOCK                          
         LA    R3,20(R3)                                                        
*                                                                               
BLDLS10  CLI   0(R1),1                                                          
         BE    *+8                                                              
         LA    R1,10(R1)                                                        
         LA    R1,12(R1)                                                        
         CLI   0(R1),X'FF'         END OF TABLE                                 
         BNE   BLDLS05                                                          
*                                                                               
         XIT1  REGS=(R3,R5)                                                     
         SPACE 2                                                                
***********************************************************************         
* ADD THE PENDING DATES TO SOON AND LASER OPTIONS                               
* R1 CONTAINS THE DATE                                                          
* R4 CONTAINS THE OPTION                                                        
***********************************************************************         
         SPACE 1                                                                
ADDDATE  NTR1                                                                   
*        LR    R4,R1               GET ADDR OF PENDING DATE                     
         MVC   SVDATE,0(R1)                                                     
         LR    R0,R5               NUMBER OF ENTRIES                            
         LA    R3,SCANBLK                                                       
*                                                                               
ADAT5    CLC   0(4,R3),0(R4)       FIND ENTRY FOR SOON/LOCAL                    
         BE    ADAT10                                                           
         LA    R3,20(R3)                                                        
         BCT   R0,ADAT5                                                         
         B     EXITOK                                                           
*                                                                               
ADAT10   GOTO1 VDATCON,BOPARM,(2,SVDATE),(8,10(R3))                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE STATUS LINE                                                      
***********************************************************************         
         SPACE 1                                                                
*        OC    TLKDPSR(L'TLKDPSR+L'TLKDPLR),TLKDPSR IF SOON OR LOCAL            
*        BNZ   EXITNV                  THEN CAN'T CHANGE THIS FIELD             
VALSTAT  MVI   TLKSTAT,0                                                        
         MVI   TLKSTAT2,0                                                       
         MVI   TLKLASR,0                                                        
         XC    TLKSEQN,TLKSEQN                                                  
         MVC   SVSNDATE,TLKDPSR    SAVE THE SOON DATE                           
         XC    TLKDPSR,TLKDPSR                                                  
         MVC   SVLCDATE,TLKDPLR    SAVE THE LOCAL DATE                          
         XC    TLKDPLR,TLKDPLR                                                  
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITNO              NEED SOMETHING IN THIS FIELD                 
         BAS   RE,STED             EDIT THE STATUS LINE                         
         BNE   EXITL                                                            
*        TM    TLKSTAT2,OCNSEDI                                                 
*        BZ    VSTAT10                                                          
*        BRAS  RE,CHKBNK          READ SC ACCT LOOKING TO BANK INFO             
*        BE    VSTAT10            IF NOT THERE - GIVE ERROR MSG                 
*        MVC   FVMSGNO,=AL2(AE$MSBNK)                                           
*        B     EXITL                                                            
*                                                                               
VSTAT10  TM    TLKSTAT2,OCNSLBLT+OCNS820  CAN'T HAVE LASER & 820                
         BO    EXITNV                                                           
*                                                                               
         TM    TLKSTAT2,OCNS820           IS IT EDI820 ?                        
         BZ    VSTAT20                                                          
*                                                                               
*MN SPEC-9590                                                                   
         CLI   TLKLASR,0          CANNOT HAVE EDI820 AND LASER                  
         BNE   EXITNV             PRINTING (BLU,WSP,WSL) TOGETHER               
         TM    TLKSTAT,OCNSLOCL                                                 
         BO    EXITNV                                                           
*MN SPEC-9590                                                                   
*                                                                               
         BRAS  RE,VALEDI           VALIDATE EDI820 STATUS                       
         BE    VSTAT20                                                          
         BH    EXITMEDI                                                         
         MVC   FVXTRA(L'ACTKULA),TLKBACC+1                                      
         BL    EXITMBPR                                                         
*                                                                               
VSTAT20  TM    TLKSTAT2,OCNSDFIL   USING FLATFILE?                              
         BZ    VSTAT30                                                          
         CLI   TLKLASR,0           IF USING FLATFILE MUST HAVE                  
         BNE   *+12                LASERXXX, LASERWSP                           
         TM    TLKSTAT2,OCNSLBLT   OR LASER                                     
         BZ    EXITNV                                                           
         TM    TLKSTAT2,OCNS820    AND CAN'T HAVE EDI820                        
         BO    EXITNV                                                           
*                                                                               
VSTAT30  TM    TLKSTAT,OCNSSOON+OCNSLOCL  TEST SOON OR LOCAL                    
         BNZ   VSTAT40                                                          
         CLI   TLKLASR,0           CAN'T HAVE LASERNNN & EDI820                 
         BE    EXITOK                                                           
         TM    TLKSTAT2,OCNS820                                                 
         BNZ   EXITNV                                                           
*                                                                               
*MN SPEC-9590                                                                   
VSTAT40  CLI   TLKLASR,OCNWSP      IF USING LASERWSP                            
         BNE   VSTAT45             SPEC-7542                                    
         TM    TLKSTAT,OCNSLOCL            IF SOON OR LOCAL                     
         BO    EXITNV                      CAN'T HAVE:                          
         B     VSTAT60                                                          
*                                                                               
VSTAT45  CLI   TLKLASR,OCNWSL      IF USING LASERWSL                            
         BNE   VSTAT50             SPEC-9589                                    
         TM    TLKSTAT,OCNSLOCL            IF SOON OR LOCAL                     
         BO    EXITNV                      CAN'T HAVE:                          
         B     VSTAT60                                                          
*MN SPEC-9590                                                                   
*                                                                               
VSTAT50  TM    TLKSTAT,OCNSLOCL+OCNSSOON   IF SOON OR LOCAL                     
         BZ    EXITOK                      CAN'T HAVE:                          
         CLI   TLKLASR,0                   LASER(XXX)                           
         BNE   EXITNV                      EDI820 (OR EDI)                      
*                                                                               
VSTAT60  TM    TLKSTAT,OCNSDREG+OCNSSHUT                                        
         BNZ   EXITNV                                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* STED-  EDIT THE STATUS LINE                                                   
***********************************************************************         
*                                                                               
STED     NTR1                                                                   
         GOTO1 VSCANNER,BOPARM,FVIHDR,(10,SCANBLK)                              
         SR    R0,R0                                                            
         IC    R0,4(R1)                                                         
         LTR   R0,R0                                                            
         BZ    EXITOK                                                           
         LA    R3,SCANBLK          MATCH ITEM IN BLOCK TO TABLE                 
*                                                                               
STED03   LA    R4,STATLST                                                       
         LA    RF,TLKSTAT                                                       
*                                                                               
STED05   CLC   12(10,R3),2(R4)     ITEM IN BLOCK - TABLE                        
         BE    STED09                                                           
         CLI   0(R4),1             ONE SIDED ENTRY                              
         BE    *+8                                                              
         LA    R4,10(R4)           NEXT TABLE ENTRY                             
         LA    R4,12(R4)                                                        
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BNE   STED05              LOOK AT NEXT                                 
*                                                                               
         LA    R4,STATLST2                                                      
         LA    RF,TLKSTAT2                                                      
STED07   CLC   12(10,R3),2(R4)     ITEM IN BLOCK - TABLE                        
         BE    STED09                                                           
         CLI   0(R4),1             ONE SIDED ENTRY                              
         BE    *+8                                                              
         LA    R4,10(R4)           NEXT TABLE ENTRY                             
         LA    R4,12(R4)                                                        
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BNE   STED07              LOOK AT NEXT                                 
*                                                                               
         LA    R4,STATLASR                                                      
         LA    RF,TLKLASR                                                       
STED08   CLC   12(10,R3),2(R4)     ITEM IN BLOCK - TABLE                        
         BE    STED08A                                                          
         CLI   0(R4),1             ONE SIDED ENTRY                              
         BE    *+8                                                              
         LA    R4,10(R4)           NEXT TABLE ENTRY                             
         LA    R4,12(R4)                                                        
         CLI   0(R4),X'FF'         END OF TABLE                                 
         BNE   STED08              LOOK AT NEXT                                 
         B     EXITL               IF NO TABLE ENTRY IS INVALID INPUT           
*                                                                               
STED08A  CLI   0(R4),1             ONE SIDED ENTRY                              
         BE    *+14                                                             
         CLC   22(10,R3),12(R4)    MUST MATCH RIGHT SIDE OF EQUAL               
         BNE   STED05                                                           
         CLI   0(RF),0                                                          
         BNE   EXITNV                                                           
         TM    TLKSTAT,OCNSSHUT    IF SHUTTLE IS ON ALSO, EXIT WITH             
         BO    EXITNV              ERROR                                        
         MVC   0(1,RF),1(R4)       TURN ON BIT                                  
         B     STED90                                                           
*                                                                               
STED09   CLI   0(R4),1             ONE SIDED ENTRY                              
         BE    *+14                                                             
         CLC   22(10,R3),12(R4)    MUST MATCH RIGHT SIDE OF EQUAL               
         BNE   STED05                                                           
         OC    0(1,RF),1(R4)       TURN ON BIT                                  
         LA    R5,TLKDPSR          SOON PENDING                                 
         MVC   TEMPDATE,SVSNDATE   SAVED SOON DATE                              
         CLC   12(10,R3),=CL10'SOON' USER NOT ALLOWED TO ADD DATE               
         BE    STED11                                                           
         LA    R5,TLKDPLR          LOCAL PENDING                                
         MVC   TEMPDATE,SVLCDATE   SAVED LOCAL DATE                             
         CLC   12(10,R3),=CL10'LOCAL'                                           
         BNE   STED13                                                           
*                                                                               
STED11   CLI   1(R3),0             SOON DATE ENTERED?                           
         BNE   STED11A             YES                                          
         OC    TEMPDATE,TEMPDATE   ANY DATE THERE ALREADY?                      
         BNZ   EXITNV              THAN MUST KEEP THE DATE                      
         B     STED13                                                           
STED11A  OC    TEMPDATE,TEMPDATE   ANY DATE ALREADY THERE?                      
         BZ    EXITNV              NO THAN DON'T LET THEM ADD ONE               
         GOTO1 VDATVAL,BOPARM,(0,22(R3)),BCWORK                                 
         OC    BOPARM(4),BOPARM                                                 
         BNZ   *+14                INVALID DATE                                 
         MVC   FVMSGNO,=AL2(FVFINVDT)    INVALID  DATE                          
         B     EXITL                                                            
         GOTO1 VDATCON,BOPARM,(0,BCWORK),(2,0(R5))                              
         CLC   0(2,R5),TEMPDATE    MUST NOT BE ABLE TO CHANGE THE DATE          
         BE    STED90                                                           
         BNE   EXITNV                                                           
*                                                                               
STED13   CLC   12(10,R3),=CL10'STACK'  STACK NUMBER                             
         BNE   STED90                                                           
         CLI   1(R3),1             MUST ENTER STACK NUMBER                      
         BL    EXITNV                                                           
         CLI   1(R3),5             MAX IS 65535                                 
         BH    EXITNV                                                           
         SR    R1,R1                                                            
         ICM   R1,15,8(R3)         STACK NUMBER                                 
         BZ    EXITNV              NOT NUMERIC                                  
         C     R1,=F'65534'                                                     
         BH    EXITNV                                                           
         STCM  R1,3,TLKSEQN                                                     
*                                                                               
*TED15   CLC   12(3,R3),=C'FTP'    USING FTP OPTION                             
*        BNE   STED90                                                           
*        CLC   CUAALF,=C'CC'      SKIP EDICT TEST FOR COKE                      
*        BE    STED90                                                           
*        BAS   RE,EDICT                                                         
*        BNE   EXITL                                                            
*                                                                               
STED90   LA    R3,32(R3)                                                        
         SR    R1,R1                                                            
         BCT   R0,STED03                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
**********************************************************************          
*              CHECK FOR EDICT RECORD IF USING FTP                   *          
**********************************************************************          
         SPACE 1                                                                
         USING EDIKEYD,R5                                                       
EDICT    NTR1                                                                   
         MVI   CTBYTE,0            READ FOR BINARY ID NUMBER                    
         BAS   RE,GETID            GET ID RECORD                                
*                                                                               
         LA    R5,IOKEY                                                         
         XC    EDIKEY,EDIKEY                                                    
         MVI   EDIKSYS,EDIKSYSQ    X'05' - KEY SYSTEM FOR ALL SYSTEMS           
         MVI   EDITYPE,EDITYPEQ    X'07' - EDICT TYPE                           
         MVC   EDINAME,BCWORK                                                   
         OC    EDINAME,BCSPACES                                                 
         LHI   R1,XOCONFIL+XOREAD+XIO1                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$NOEDI)  EDICT RECORD NOT SET UP                  
         B     EXITL                                                            
         DROP  R5                                                               
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
         USING CHARECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING CHARECD,R2                                                       
LAST     USING CHARECD,R3                                                       
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
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING CHARECD,IOKEY                                                    
FLST     MVC   X.CHAKEY,THIS.CHAKEY                                             
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   EXITL               MESS UP ON THE READ HIGH                     
         B     NLST02                                                           
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
NLST     LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   X.CHAKEY(CHAKLDG-CHARECD),THIS.CHAKEY                            
         BNE   EXITL                                                            
*                                                                               
         CLI   X.CHAKSEQ,0         FIRST CHECK RECORD                           
         BNE   NLST                NO - GET THE NEXT ONE                        
                                                                                
         MVC   SVKEY,IOKEY                                                      
         MVC   IOKEY,BCSPACES      READ LEDGER TO TEST SECURITY                 
         MVC   IOKEY(3),SVKEY+1    FILL IN COMP CODE AND U/L                    
         GOTO1 AGETACT,0                                                        
         MVC   FVXTRA,BCSPACES     CLEAR OUT ANY MSG'S FROM GETACT              
         MVC   IOKEY,SVKEY         RESTORE KEY                                  
         BE    NLST04                                                           
         LHI   R1,XOREAD+XOACCDIR+XIO1  RESTORE ORIGINAL READ                   
         GOTO1 AIO                                                              
         B     NLST                                                             
*                                                                               
NLST04   LHI   R1,XOREAD+XOACCDIR+XIO1  RESTORE ORIGINAL READ                   
         GOTO1 AIO                                                              
         MVC   THIS.CHAKEY(L'CHAKEY+L'CHAKSTA+L'CHAKDA),IOKEY                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   DS    0H                                                               
         OI    LSSTAT1,LSSTSAR+LSSMUROW                                         
*        OI    LSSTAT1,LSSBALL+LSSMUROW                                         
         OI    LSSTAT2,LSSNOSEQ+LSSADD                                          
         MVC   LSCOLLIN,=AL2(160)                                               
         MVC   LSROWLIN,=AL2(2)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,CHARFST-CHARECD                                               
         STH   RF,MNTDISP          SAVE DISPL TO FIRST ELEMENT                  
         MVI   READSEQ#,0                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
* AIO5 -> NEXT CHECK RECORD IF THE FIRST ONE IS NOT BIG ENOUGH        *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC                                                        
         CLI   READSEQ#,0                                                       
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    RF,R1               A(RECORD)                                    
         CR    RF,R1               MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    RF,CHARFST-CHARECD(,RF)     IT IS NOW.                           
         XR    RE,RE                                                            
*                                                                               
         USING OCNELD,RF                                                        
FML02    CLI   OCNEL,0             RECORD END?                                  
         BNE   FML04               YES                                          
         BAS   RE,READNXT                                                       
         BNE   EXITL                                                            
         LH    RF,MNTDISP                                                       
         A     RF,AIO5                                                          
         XR    RE,RE                                                            
*                                                                               
FML04    CLI   OCNEL,OCNELQ        54 ELEMENT                                   
         BNE   NML04               NO                                           
*                                                                               
FML08    L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST CHECK RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         SR    RF,R1                                                            
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
         SPACE 1                                                                
NLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST CHECK RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    RF,R1               A(RECORD)                                    
         XR    RE,RE                                                            
         CR    RF,R1               MAKE SURE MNTDISP INITIALISED                
         BH    NML06                                                            
         LA    RF,CHARFST-CHARECD(,RF)     IT IS NOW.                           
*                                                                               
         USING OCNELD,RF                                                        
NML02    CLI   OCNEL,0             RECORD END?                                  
         BNE   NML04               NO                                           
         BAS   RE,READNXT          READ NEXT CHECK RECORD                       
         BNE   EXITL               NO MORE RECORD                               
         LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIO5             NEW RECORD IN AIO5                           
         XR    RE,RE                                                            
*                                                                               
NML04    CLI   OCNEL,OCNELQ        54 ELEMENT                                   
         BE    NML08               YES                                          
                                                                                
NML06    IC    RE,OCNLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML08    L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST CHECK RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         SR    RF,R1                                                            
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)                                               
         LH    RF,MNTDISP                                                       
         L     R1,AIOREC                                                        
         CLI   READSEQ#,0                                                       
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    RF,R1                                                            
         USING OCNELD,RF                                                        
         CLC   MNTDISP,EFFS        IF MNTDISP IS EFFS                           
         BNE   TSARF05                                                          
         MVC   TLKID,EFFS          CREATE A DUMMY TSAR RECORD TO                
         MVC   TLKAFT(TLKNQ2),EFFS                                              
         OI    LSLTIND1,LSLTIFVL   FORCE VAL OF LIST LINES                      
         B     EXITOK                                                           
*                                                                               
TSARF05  MVC   TLKBEF,OCNBEF       NEXT CHECK NUMBER                            
         MVC   TLKAFT,OCNAFT       NEXT CHECK NUMBER                            
         MVC   TLKID,OCNOFFID      OFFICE ORIGIN ID                             
         MVC   TLKBACC,OCNBANK     BANK ACCOUNT                                 
         MVC   TLKNTYP,OCNNTYP     OPTIONAL NUMBER TYPE                         
         MVC   TLKFILT,OCNFILT     CLIENT OR OFFICE FILTER                      
*        MVC   TLKLCHK,OCNLAST     LAST CHECK IN STOCK AT DDS                   
         MVC   TLKSORT,OCNSORT     SORT SEQUENCE                                
         MVC   TLKPOFF,OCNPOFF     OFFICE FOR CASH POSTING                      
         MVC   TLKSTAT,OCNSTAT     STATUS BYTE 1                                
         MVC   TLKSTAT2,OCNSTAT2   STATUS BYTE 2                                
         MVC   TLKSEQN,OCNSEQN     SEQUENCE # FOR SOON (STACK=)                 
         MVC   TLKDPSR,OCNDPSR     DATE OF PENDING SOON REGISTER                
         MVC   TLKDPLR,OCNDPLR     DATE OF PENDING LOCAL REGISTER               
         MVC   TLKLASR,OCNLASR     CHECK INDICATOR FOR LASER PRINTING           
         MVC   TLKELEN,OCNLN       ELEMENT LENGTH (VARIABLE)                    
*                                                                               
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1 (DELETE ALL 54 ELEMENTS)                    *         
***********************************************************************         
         SPACE 1                                                                
         USING CHARECD,R2                                                       
UPDFRST1 MVI   ANYLINES,NO                                                      
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         MVI   ADDSEQ#,0                                                        
         L     RE,AIO5             CLEAR IO5                                    
         LA    RF,IOAREALN                                                      
         XCEF                                                                   
         L     R2,AIOREC                                                        
         B     UPDF10                                                           
*                                                                               
T        USING CHARECD,IOKEY                                                    
UPDF02   CLI   CHAKSEQ,0                                                        
         BNE   UPDF04                                                           
         MVC   IOKEY(L'CHAKEY),CHAKEY                                           
         B     UPDF08                                                           
*                                                                               
UPDF04   CLI   CHARFST,0           EMPTY RECORD                                 
         BNE   UPDF06                                                           
         OI    CHARSTA,CHASDELT    DELETE MASTER RECORD                         
*                                                                               
         TM    T.CHAKSTA,CHASDELT                                               
         BO    UPDF06              DIR HAS ALREADY BEEN DELETED                 
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)                                   
         GOTO1 AIO                                                              
         TM    IOERR,FF-IOEDEL                                                  
         BZ    *+6                                                              
         DC    H'0'                ERROR READING THE RECORD                     
         OI    T.CHAKSTA,CHASDELT DELETE CHECK DIR                              
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
UPDF08   SR    RF,RF               READ NEXT CHECK SUB-RECORD                   
         IC    RF,T.CHAKSEQ        THEY ARE ALWAYS IN SEQUENCE                  
         AHI   RF,1                                                             
         STC   RF,T.CHAKSEQ                                                     
*                                                                               
         L     R1,=AL4(XOHID+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         CLC   T.CHAKEY(CHAKSEQ-CHARECD),IOKEYSAV                               
         BNE   EXITOK              EXIT - NOT SUB-RECORD                        
         TM    IOERR,FF-IOEDEL                                                  
         BZ    *+6                                                              
         DC    H'0'                ERROR READING THE RECORD                     
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                  BAD MASTER RECORD                          
         L     R2,AIO2                                                          
*                                                                               
UPDF10   DS    0H                                                               
*        GOTO1 DELPAS,BOPARM,CHARECD DELETE PASSIVE POINTERS                    
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('OCNELQ',CHARECD),0               
         B     UPDF02                                                           
         DROP  T,R2                                                             
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
* AIO6 = A(CHECK RECORD) IF ADDSEQ# IS NOT 0                                    
***********************************************************************         
         SPACE 1                                                                
         USING CHARECD,R2                                                       
         USING TLSTD,R3                                                         
         USING OCNELD,R4                                                        
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
*                                                                               
         MVI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         LM    R2,R3,SVPARMS3                                                   
         CLI   ADDSEQ#,0           FIRST CHECK RECORD?                          
         BE    *+8                                                              
         L     R2,AIO6             R4 = NEW CHECK RECORD                        
*                                                                               
         LA    R4,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   OCNEL,OCNELQ                                                     
         MVI   OCNLN,OCNLN3Q                                                    
         MVC   OCNBEF,TLKBEF                                                    
         MVC   OCNAFT,TLKAFT                                                    
         MVC   OCNOFFID,TLKID                                                   
         MVC   OCNBANK,TLKBACC                                                  
         MVC   OCNNTYP,TLKNTYP                                                  
         MVC   OCNFILT,TLKFILT                                                  
         MVC   OCNSTAT,TLKSTAT                                                  
         MVC   OCNLAST,TLKLCHK                                                  
         MVC   OCNSTAT2,TLKSTAT2                                                
         MVC   OCNSEQN,TLKSEQN                                                  
         MVC   OCNDPSR,TLKDPSR                                                  
         MVC   OCNDPLR,TLKDPLR                                                  
         MVC   OCNSORT,TLKSORT                                                  
         MVC   OCNPOFF,TLKPOFF                                                  
         MVC   OCNLASR,TLKLASR                                                  
*                                                                               
UREC108  SR    RF,RF                                                            
         ICM   RF,3,CHARLEN                                                     
         CHI   RF,IOMAXLNQ         GREATER MAX RECORD ALLOWED ?                 
         BNH   UREC110             NO - ADD IT INTO CURRENT RECORD              
*                                                                               
         CLI   ADDSEQ#,0           FIRST CHECK RECORD ?                         
         BE    *+8                                                              
         BAS   RE,ADDREC           NO - ADD SAVED CHECK REC IN AIO6             
*                                                                               
         L     R2,AIO6             R2=A(NEW CHECK SUB-RECORD)                   
         L     RF,AIOREC                                                        
         MVC   CHAKEY(L'CHAKEY+L'CHARLEN+L'CHARSTA),0(RF)                       
         SR    RF,RF                                                            
         IC    RF,ADDSEQ#                                                       
         AHI   RF,1                                                             
         STC   RF,ADDSEQ#          NEXT SEQUENCE NUMBER                         
         STC   RF,CHAKSEQ                                                       
*                                                                               
         LA    RE,CHARFST                                                       
         MVI   0(RE),0                                                          
         SR    RE,R2                                                            
         AHI   RE,1                                                             
         STCM  RE,3,CHARLEN        LENGTH OF CHECK RECORD                       
*                                                                               
UREC110  GOTO1 AADDEL,BOPARM,(R2)  ADD NEW CHECK ELEMENT                        
         BE    EXITOK                                                           
         DC    H'0'                ERROR ADDING ELEMENT                         
         DROP  R2,R3                                                            
         SPACE 2                                                                
*        GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIOREC,BOELEM,0                    
*        B     EXITOK                                                           
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 CLI   CSACT,A#CHA                                                      
         BE    *+12                                                             
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
         CLI   ANYLINES,YES        EMPTY LIST?                                  
         BNE   ULAST104            NO - OK                                      
         CLI   ADDSEQ#,0           FIRST CHECK RECORD ?                         
         BE    EXITOK                                                           
         BAS   RE,ADDREC           NO - ADD SAVED CHECK REC IN AIO6             
*                                                                               
         L     RF,AIOREC           RESTORE IOADDR FOR CONTRALLER                
         ST    RF,IOADDR                                                        
         B     EXITOK                                                           
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
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO READ THE NEXT CHECK RECORD                               *         
* EXIT - READSEQ# : NEXT RECORD SEQUENCE NUMBER                       *         
*      - MNTDISP: DISPLACEMENT TO THE FIRST ELEMENT                   *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHARECD,IOKEY                                                    
READNXT  NTR1  ,                                                                
         L     RF,AIOREC           A(CURRENT CHECK RECORD)                      
         MVC   T.CHAKEY,0(RF)                                                   
         SR    RF,RF                                                            
         IC    RF,READSEQ#                                                      
         AHI   RF,1                                                             
         STC   RF,READSEQ#                                                      
         STC   RF,T.CHAKSEQ                                                     
*                                                                               
         LHI   R1,XOREAD+XOACCMST+XIO5                                          
         GOTO1 AIO                                                              
         BNE   EXITL               NO MORE CHECK RECORD                         
         LA    RF,CHARFST-CHARECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD CHECK RECORD                                         *         
* EXIT - AIO6: A(NEW RECORD)                                          *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHARECD,R4                                                       
ADDREC   NTR1  ,                                                                
         LA    R4,IOKEY                                                         
         L     RF,AIO6                                                          
         MVC   T.CHAKEY,0(RF)                                                   
*                                                                               
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)   READ FOR UPDATE                 
         GOTO1 AIO                                                              
         BE    ADDREC10                                                         
         TM    IOERR,IOERRS-(IOEDEL+IOERNF)                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVIOERR,IOERR       SAVE IOERR                                   
         TM    IOERR,IOERNF                                                     
         BNO   ADDREC10                                                         
         MVC   IOKEY,IOKEYSAV      RESTORE KEY IF NOT FOUND                     
         B     ADDREC20                                                         
*                                                                               
ADDREC10 NI    T.CHAKSTA,FF-CHASDELT   SET DELETE OFF                           
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
ADDREC20 L     R0,AIO6             COPY FROM SUB-RECORD                         
         L     RE,AIO2             COPY TO SUB-RECORD                           
         LA    RF,IOAREALN         L'IOAREA                                     
         LR    R1,RF               LENGTH IS LENGTH OF RECORD                   
         MVCL  RE,R0                                                            
*                                                                               
         LHI   R1,XOADDREC+XOACCMST+XIO2   ADD DIR + FILE RECORDS               
         TM    SVIOERR,IOERNF                                                   
         BO    *+8                                                              
         LHI   R1,XOPUTREC+XOACCMST+XIO2   CHANGE CHECK RECORD                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                ERROR - CAN'T PUT RECORD BACK                
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE EDI820 STATUS                                                        
* ON EXIT                                                                       
* LOW = MISSING BANK PROFILE FROM ACCOUNT RECORD                                
* HIGH= MISSING BANK RECORD FOR EDI820                                          
* THIS ROUTINE ALSO EXITS WITH ERROR WHEN NO DATASET OR MESSAGE CLASS           
* IS DEFINED AT ANY LEVEL OF ACC REC/ AFM BANK REC / CNTRL BANK RECD.           
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
         USING ACTRECD,R5                                                       
VALEDI   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         NI    FLAG,X'FF'-(FLGMSG+FLGDSN+FLGADVNT)                              
         L     R2,ATLST                                                         
         LA    R5,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCULA,TLKBACC                                                 
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO1                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CBPELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VALEDL              NO ELEM                                      
         L     RF,12(R1)                                                        
         USING CBPFELD,RF                                                       
         CLC   CBPBCDE,BCSPACES     BANK CODE                                   
         BNH   VALEDL                                                           
         CLC   CBPBRCH,BCSPACES     BRANCH CODE                                 
         BNH   VALEDL                                                           
         MVC   BCWORK(L'CBPBCDE+L'CBPBRCH),CBPBCDE SAVE OFF BANK/BRANCH         
*                                                                               
         LA    R5,ACTRFST                                                       
         DROP  R5,RF                                                            
         USING FFTELD,R5                                                        
VALED10  CLI   0(R5),0                                                          
         BE    VALED40                                                          
         CLI   0(R5),FFTELQ                                                     
         BNE   VALED20                                                          
         CLI   FFTTYPE,FFTTEDIP    IS IT TRNSMSN TYPE FREE FORM ELEMENT         
         BNE   VALED20                                                          
         CLI   FFTETYP,FFT820                                                   
         BE    VALED30                                                          
VALED20  LLC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     VALED10                                                          
*                                                                               
VALED30  DS    0H                                                               
         CLC   FFTMSGC,BCSPACES    IS MESSAGE CLASS DEFINED ?                   
         BNH   *+8                 NO                                           
         OI    FLAG,FLGMSG         MESSAGE CLASS FOUND                          
         CLC   FFTDNAM,BCSPACES    IS THERE A DATA SET DEFINED ?                
         BNH   *+8                                                              
         OI    FLAG,FLGDSN         DATA SET NAME FOUND.                         
         DROP  R5                                                               
*                                                                               
         USING BNKRECD,R5                                                       
VALED40  LA    R5,IOKEY                                                         
         MVC   BNKKEY,BCSPACES                                                  
         MVI   BNKTYP,BNKTYPQ                                                   
         MVI   BNKSUB,BNKSUBQ                                                   
         MVC   BNKCPY,CUABIN                                                    
         MVC   BNKBANK,BCWORK      BANK CODE                                    
         MVC   BNKBRNCH,BCWORK+3   BRANCH CODE                                  
         MVI   BNKETYP,BNK820                                                   
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   VALEDH                                                           
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BNE   VALEDH                                                           
         L     R5,AIO1                                                          
*                                                                               
         TM    FLAG,(FLGMSG+FLGDSN)                                             
         BO    VALEDEQ             MSGCLASS AND DSN FOUND.                      
*                                                                               
         LA    R5,BNKRFST                                                       
         DROP  R5                                                               
         USING BNKELD,R5                                                        
VALED50  CLI   0(R5),0                                                          
         BE    VALED90            READ CONTROL RECORD FOR MESSAGE CLASS         
         CLI   0(R5),BNKELQ                                                     
         BE    VALED60                                                          
         LLC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     VALED50                                                          
*                                                                               
VALED60  DS    0H                                                               
         CLC   BKADVID,BCSPACES    IS THIS ADVANTIS ?                           
         BNH   VALED70                                                          
         OI    FLAG,FLGADVNT       ADVANTIS INFO FOUND                          
VALED70  CLC   BKDSNM,BCSPACES     IS DATASET SETUP ?                           
         BNH   VALED80                                                          
         OI    FLAG,FLGDSN         DATA SET IS SETUP                            
VALED80  CLC   BKMSGCL,BCSPACES    IS MESSAGE CLASS SET ?                       
         BNH   VALED90                                                          
         OI    FLAG,FLGMSG         MESSAGE CLASS FOUND                          
*                                                                               
VALED90  DS     0H                                                              
         LA    RF,IOKEY                                                         
         USING BANKRECD,RF                                                      
         XC    BANKEY,BANKEY                                                    
         MVI   BANKTYP,BANKTYPQ    BANK RECORD 'B'                              
         MVI   BANKSUB,BANKSGQ     BANK GENERAL RECORD 'G'                      
         MVC   BANKCDE,BCWORK                                                   
         MVC   BANKHUB,BCWORK+3                                                 
         MVC   BANKBRN,BCWORK+6                                                 
         LHI   R1,XOREAD+XOGENDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    VALED110                                                         
VALED100 LA    RF,IOKEY                                                         
         MVC   IOKEY(L'BANKEY),IOKEYSAV                                         
         XC    BANKBRN,BANKBRN                                                  
         LHI   R1,XOREAD+XOGENDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   VALED160                                                         
*                                                                               
VALED110 LHI   R1,XOGET+XOGENFIL+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO2                                                          
         LA    R5,BANFIRST(RF)                                                  
         USING BATELD,R5                                                        
VALED120 CLI   0(R5),0                                                          
         BE    VALED150            CHECK IF WE R AT BANK BRANCH LEVEL           
         CLI   0(R5),BATELQ        IS IT BANK TRANSMISSION INFO ELEM            
         BE    VALED130                                                         
         LLC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     VALED120                                                         
*                                                                               
VALED130 DS    0H                                                               
         CLC   BATUSER,BCSPACES     IS IT ADVANTIS ?                            
         BNH   VALED140                                                         
         OI    FLAG,FLGADVNT        ADVANTIS SETUP                              
VALED140 CLC   BATCLAS,BCSPACES                                                 
         BNH   VALED150                                                         
         OI    FLAG,FLGMSG                                                      
VALED150 CLC   BANKBRN,BCWORK+6                                                 
         BE    VALED100                                                         
*                                                                               
VALED160 DS    0H                                                               
         TM    FLAG,FLGDSN         DO WE HAVE DSN SETUP ?                       
         BNO   VALEDDSN                                                         
*                                                                               
         TM    FLAG,FLGADVNT       DO WE HAVE ADVANTIS INFO ?                   
         BNO   VALEDEQ             IF NOT SET THEN WE DON'T                     
*                                  NEED TO TEST DATASET/MSGCLASS.               
         TM    FLAG,FLGMSG         MESSAGE CLASS SETUP ?                        
         BNO   VALEDMSG            MESSAGE CLASS NOT SETUP                      
         B     VALEDEQ                                                          
*                                                                               
VALEDDSN MVC   FVMSGNO,=AL2(AE$DSNND)                                           
         B     VALEDX2                                                          
VALEDMSG MVC   FVMSGNO,=AL2(AE$MSGND)                                           
         B     VALEDX2                                                          
*                                                                               
VALEDH   CLI   *,0                                                              
         B     VALEDIX                                                          
*                                                                               
VALEDL   CLI   *,FF                                                             
         B     VALEDIX                                                          
*                                                                               
VALEDEQ  CR    RB,RB                                                            
*                                                                               
VALEDIX  XIT1                                                                   
VALEDX2  CLI   *,FF                                                             
         XMOD1 2                                                                
*                                                                               
         DROP  R2,R5,RF                                                         
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
*&&DO                                                                           
***********************************************************************         
* IF EDI STATUS ADDED MAKE SURE SC ACCOUNT HAS REQUIRED BANK ACCT INFO*         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
         USING TLSTD,R2                                                         
CHKBNK   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY(ACTKEND),TLKBACC                                         
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   CHKBNKL                                                          
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE   CHKBNKL                                                          
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('BNKELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   CHKBNKL                                                          
*                                                                               
         USING BNKELD,RF                                                        
         L     RF,12(R1)                                                        
         OC    BNKACCT,BNKACCT     ANY BANK ACCOUNT?                            
         BZ    CHKBNKL             NO                                           
         OC    BNKROUT,BNKROUT     ANY ROUTING NUMBER?                          
         BZ    CHKBNKL                                                          
         B     CHKBNKE                                                          
*                                                                               
CHKBNKL  CLI   *,FF                                                             
         B     CHKBNKX                                                          
CHKBNKE  CR    RB,RB                                                            
CHKBNKX  XIT1                                                                   
         DROP  R2,R5                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
EFFS     DC    50X'FF'                                                          
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
IOMAXLNQ EQU   1900                                                             
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#NFA66,6,L                                                     
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*ACFILWORK                                                                      
*DDSCANBLKD                                                                     
*CTGENFILE                                                                      
*CTGENEDICT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
SVBACCH  DS    A                                                                
*ELMDSP   DS    H                                                               
SCANBLK  DS    CL250               BLOCK FOR UNSCAN                             
SVKEY    DS    CL64                                                             
*                                                                               
READSEQ# DS    XL(L'CHAKSEQ)                                                    
ADDSEQ#  DS    XL(L'CHAKSEQ)                                                    
LSTSEQ#  DS    XL(L'CHAKSEQ)                                                    
*                                                                               
TEMPDATE DS    XL2                                                              
SVSNDATE DS    XL2                                                              
SVLCDATE DS    XL2                                                              
SVDATE   DS    XL2                 SAVED DATE FOR SOON/LOCAL REGISTER           
SVALPH   DS    CL2                 SAVED AGENCY ALPHA ID                        
SVLEN    DS    XL1                 SAVED LENGTH                                 
CTBYTE   DS    XL1                 BYTE FOR CONTROL FILE READING                
BIT      DS    XL1                                                              
FLAG     DS    X                                                                
FLGMSG   EQU   X'80'               MESSAGE CLASS FOUND                          
FLGDSN   EQU   X'40'               DATA SET NAME FOUND                          
FLGADVNT EQU   X'20'               ADVANTIS INFO FOUND                          
BYTE     DS    XL1                                                              
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
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
SVADDR   DS    A                   SAVED ADDRESS                                
SAVRB    DS    F                                                                
         SPACE 1                                                                
DSLISTU  DS    0D                  DATA DICTIONARY OUTPUT                       
AC@NFA66 DS     CL6                (CODE)                                       
SVIOERR  DS    CL(L'IOERR)                                                      
*                                                                               
ANYLINES DS    CL1                                                              
MNTDISP  DS    H   MOVE TO SAVED STORAGE                                        
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKID    DS    XL2                 OFFICE ORIGIN ID                             
         ORG   TLUSER                                                           
TLKAFT   DS    CL6                 NEXT CHECK NUMBER(ENDING CHK#)               
TLKBEF   DS    CL6                 STARTING CHECK NUMBER                        
TLKBACC  DS    XL15                BANK ACCOUNT                                 
TLKNTYP  DS    CL1                 OPTIONAL NUMBER TYPE                         
TLKFILT  DS    CL3                 CLIENT/OFFICE FILTER                         
TLKLCHK  DS    CL6                 LAST CHECK IN STOCK AT DDS                   
TLKSORT  DS    XL1                 SORT SEQUENCE                                
TLKPOFF  DS    XL2                 OFFICE FOR CASH POSTING                      
TLKSTAT  DS    XL1                 STATUS BYTE 1                                
TLKSTAT2 DS    XL1                 STATUS BYTE 2                                
TLKSEQN  DS    XL2                 SEQUENCE # FOR SOON (STACK=)                 
TLKDPSR  DS    XL2                 DATE OF PENDING SOON REGISTER                
TLKDPLR  DS    XL2                 DATE OF PENDING LOCAL REGISTER               
TLKLASR  DS    XL1                 CHECK INDICATOR FOR LASER PRINTING           
TLKELEN  DS    XL1                 ELEMENT LENGTH (VARIABLE)                    
TLLNQ    EQU   *-TLSTD                                                          
TLKNQ2   EQU   *-TLKBEF            LENGTH OF RECORD TSAR DATA                   
*                                                                               
* GEGENBNK                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENBNK                                                       
         PRINT ON                                                               
         EJECT ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACFIL11   08/23/17'                                      
         END                                                                    
