*          DATA SET CTFIL2C    AT LEVEL 003 AS OF 08/20/07                      
*&&      SET   NOP=N                                                            
*PHASE TA132CA                                                                  
         TITLE 'BANK RECORD'                                                    
*                                                                               
FIL2C    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL2C**,RR=RE,R7                                              
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
*                                                                               
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
*                                                                               
EXIT     L     R1,CALLR1           RETURN PARAMETERS TO CALLER                  
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
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
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT W/ FLD NOT VALID                        
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT W/ FLD NOT INPUT                        
EXITNN   MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT W/ FLD NOT NUMERIC                      
EXITNF   MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL               EXIT W/ RECORD NOT ON FILE                   
EXITAS   MVC   FVMSGNO,=AL2(FVFIASQ)                                            
         B     EXITL               EXIT W/ ACTION NOT VALID FOR SCREEN          
                                                                                
XIT      XIT1                                                                   
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
INIT     DS    0H                                                               
         OI    GSINDSL1,GSIXKEY    DON'T ASK 'ENTER KEY'                        
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDING           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
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
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* SCREEN OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
SCREEN   LM    R0,R3,SVPARMS                                                    
         USING BANKRECD,R2                                                      
         LA    RF,SCRTABL                                                       
         B     ITER                                                             
*                                                                               
SCRTABL  DC    AL1(SSET),AL1(0,0,0),AL4(SCRMSET)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* SET DATA SCREEN CODE                                                          
*-------------------------------------------------------------                  
SCRMSET  XC    GSSMCODE,GSSMCODE                                                
         CLI   GSSMPAGE,1             FIRST SCREEN                              
         BE    EXITOK                                                           
         CLC   BANKHUB,BCSPACES                                                 
         BNH   EXITOK                                                           
         MVI   GSSMCODE,C'A'                                                    
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* KEY OBJECT                                                          *         
* ----------                                                          *         
* SVPARMS1 HOLDS EQUATED OBJECT                                       *         
* SVPARMS2 HOLDS EQUATED VERB                                         *         
* SVPARMS3 A(KEY)                                                     *         
* SVPARMS4 HOLDS SUB-ACTION                                           *         
***********************************************************************         
KEY      LM    R1,R2,SVPARMS2                                                   
         USING BANKRECD,R2                                                      
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                                     
***********************************************************************         
KEYFRST  L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* FIRST TIME FOR DISPLAY OF A KEY FILTER                                        
*-------------------------------------------------------------                  
KFKVAL   XC    BANKEY,BANKEY                                                    
         MVI   BANKTYP,BANKTYPQ                                                 
         MVI   BANKSUB,BANKSGQ                                                  
         B     EXITOK                                                           
*                                                                               
*-------------------------------------------------------------                  
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
*-------------------------------------------------------------                  
KFKFVAL  XC    BANKEY,BANKEY                                                    
         MVI   BANKTYP,BANKTYPQ                                                 
         MVI   BANKSUB,BANKSGQ                                                  
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* RECORD OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
RECORD   LM    R0,R3,SVPARMS                                                    
         USING BANKRECD,R2                                                      
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT                                        *         
***********************************************************************         
RECFRST  L     R1,SVPARMS4                                                      
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RFTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* FIRST TIME FOR RECORD OBJECT - ADD                                            
*-------------------------------------------------------------                  
RFADD    CLI   GSSMPAGE,1             ONLY ALLOW ADD ON FIRST SCREEN            
         BNE   EXITAS                                                           
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT                                                         *         
* -----------                                                         *         
* SVPARMS1 = EQUATED OBJECT IDENTIFIER                                *         
* SVPARMS2 = EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION       *         
* SVPARMS3 BYTE 0    = EQUATED DATA VERB IF SVPARMS2 IS ZERO          *         
* SVPARMS3 BYTES 1-3 = EQUATED ACTION VERB                            *         
* SVPARMS4 = A(RECORD AT CORRECT LEVEL)                               *         
* SVPARMS5 = A(FIELD TABLE ENTRY) OR ZERO IF SVPARMS2 IS ZERO OR IF   *         
*                                    ACTION IS DOWNLOAD               *         
*                                                                     *         
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
* THE DATA OBJECT PROVIDES FUNCTIONALITY FOR DEALING WITH ALL THE     *         
* DATA ASSOCIATED WITH A RECORD, WHEREVER IT MAY OCCUR.               *         
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
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING BANKRECD,R2         R2 HOLDS A(RECORD)                           
         BR    RF                                                               
         DROP  RF                                                               
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DC    AL2(BNK#BANC),AL4(BANCDTA) BANK CODE                             
         DC    AL2(BNK#BANN),AL4(BANNDTA) BANK NAME                             
         DC    AL2(BNK#HUBC),AL4(HUBCDTA) HUB CODE                              
         DC    AL2(BNK#HUBN),AL4(HUBNDTA) HUB NAME                              
         DC    AL2(BNK#BRAC),AL4(BRACDTA) BRANCH CODE                           
         DC    AL2(BNK#BRAN),AL4(BRANDTA) BRANCH NAME                           
*                                                                               
         DC    AL2(BNK#ADL1),AL4(ADL1DTA) ADDRESS LINE 1                        
         DC    AL2(BNK#ADL2),AL4(ADL2DTA) ADDRESS LINE 2                        
         DC    AL2(BNK#CITY),AL4(CITYDTA) CITY                                  
         DC    AL2(BNK#STP),AL4(STPDTA)   STATE/PROV                            
         DC    AL2(BNK#ZIP),AL4(ZIPDTA)   ZIP/POSTAL                            
         DC    AL2(BNK#CTRY),AL4(CTRYDTA) COUNTRY                               
*                                                                               
         DC    AL2(BNK#CCN),AL4(CCNDTA)   CLIENT SERVICE CONTACT NAME           
         DC    AL2(BNK#CCPA),AL4(CCPADTA) CS CON PHONE (AREA CODE)              
         DC    AL2(BNK#CCP1),AL4(CCP1DTA) CS CP (FIRST 3 DIGITS)                
         DC    AL2(BNK#CCP2),AL4(CCP2DTA) CS CP (LAST 4 DIGITS)                 
         DC    AL2(BNK#CCPE),AL4(CCPEDTA) CS CP EXTENSION                       
         DC    AL2(BNK#CCE),AL4(CCEDTA)   CS CONTACT EMAIL                      
*                                                                               
         DC    AL2(BNK#ADVA),AL4(ADVADTA) ADVANTIS ACCOUNT # (ACC)              
         DC    AL2(BNK#ADVI),AL4(ADVIDTA) ADVANTIS USER ID (USE)                
         DC    AL2(BNK#MESC),AL4(MESCDTA) MESSAGE CLASS (CLA)                   
         DC    AL2(BNK#CHRG),AL4(CHRGDTA) CHARGE (CHA)                          
         DC    AL2(BNK#TRNT),AL4(TRNTDTA) TRANSMISSION TYPE                     
         DC    AL2(BNK#TRNK),AL4(TRNKDTA) TRANSMISSION KEY                      
*                                                                               
         DC    AL2(BNK#BNKR),AL4(BNKRDTA) BANK ROUTING NUMBER                   
         DC    AL2(BNK#FORK),AL4(FORKDTA) FORMAT KEY                            
*        DC    AL2(BNK#DSN),AL4(DSNDTA)   DATASET NAME                          
         DC    AL2(BNK#CMPR),AL4(CMPRDTA) COMPRESSION                           
*                                                                               
         DC    AL2(BNK#TCN),AL4(TCNDTA)   TECHNICAL CONTACT NAME                
         DC    AL2(BNK#TCPA),AL4(TCPADTA) TECH PHONE (AREA CODE)                
         DC    AL2(BNK#TCP1),AL4(TCP1DTA) TECH (FIRST 3 DIGITS)                 
         DC    AL2(BNK#TCP2),AL4(TCP2DTA) TECH (LAST 4 DIGITS)                  
         DC    AL2(BNK#TCPE),AL4(TCPEDTA) TECH EXTENSION                        
         DC    AL2(BNK#TCE),AL4(TCEDTA)   TECH CONTACT EMAIL                    
*                                                                               
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL2C    CSECT                                                                  
                                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                                    
***********************************************************************         
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL                 TABLE OF KNOWN INVOKERS                
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)                                 
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                                      
*-------------------------------------------------------------                  
DFDVAL   XC    HOAREA(HOLNQ),HOAREA      CLEAR THE HOLDING AREA                 
         BRAS  RE,DEFELS                 ADD DEFAULT ELEMENTS TO REC            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                                     
***********************************************************************         
DTALAST  L     R1,SVPARMS3                                                      
         LA    RF,DLTABL                 TABLE OF KNOWN INVOKERS                
         B     ITER                                                             
*                                                                               
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)                                 
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* LAST TIME FOR VALIDATE OF A DATA OBJECT                                       
*-------------------------------------------------------------                  
DLDVAL   CLI   GSSMPAGE,1                1ST PAGE?                              
         BNE   DLDVX                                                            
*                                                                               
         GOTO1 AGETEL,BOPARM,('IADELQ',BANKRECD),0                              
         LA    R4,BOELEM                                                        
         USING IADELD,R4                                                        
*                                                                               
         CLC   IADCTRY,=C'US'            IF NOT US                              
         BE    DLDV010                                                          
         CLC   IADCTRY,=C'CA'            OR CANADA, THEN OPTIONAL               
         BNE   EXITOK                                                           
*                                                                               
DLDV010  L     R1,HOCITY                 CITY                                   
         ST    R1,FVADDR                                                        
         CLC   IADCITY,BCSPACES                                                 
         BNH   EXITNO                                                           
         L     R1,HOSTATE                STATE/PROVINCE                         
         ST    R1,FVADDR                                                        
         CLC   IADSTATE,BCSPACES                                                
         BNH   EXITNO                                                           
         L     R1,HOZIP                  ZIP/POSTAL CODE                        
         ST    R1,FVADDR                                                        
         CLC   IADZIP,BCSPACES                                                  
         BNH   EXITNO                                                           
*                                                                               
DLDVX    B     EXITOK                                                           
         DROP  R4                                                               
                                                                                
***********************************************************************         
* DATA OBJECT FOR BANK CODE                                                     
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
BANCDTA  LA    RF,BANCTB                                                        
         B     ITER                                                             
*                                                                               
BANCTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBANC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBANC)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTBANC)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTBANC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTBANC)                               
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY A BANK CODE FIELD                                                     
*-------------------------------------------------------------                  
DISBANC  MVC   FVIFLD(L'BANKCDE),BANKCDE                                        
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE BANK CODE FIELD                                                      
*-------------------------------------------------------------                  
VALBANC  MVC   HOBAN,FVADDR                                                     
*                                                                               
         MVC   BANKCDE,FVIFLD                                                   
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* DISPLAY BANK CODE FILTER                                                      
*-------------------------------------------------------------                  
DFLTBANC MVC   FVIFLD,FLTIFLD                                                   
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE BANK CODE FILTER                                                     
*-------------------------------------------------------------                  
VFLTBANC MVC   FLTIFLD,FVIFLD                                                   
         MVC   BANKCDE,FLTIFLD                                                  
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* PERFORM BANK CODE FILTER                                                      
*-------------------------------------------------------------                  
DOFTBANC CLC   BANKCDE,FLTIFLD                                                  
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         B     FLTXE                                                            
                                                                                
***********************************************************************         
* DATA OBJECT FOR BANK NAME                                                     
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
BANNDTA  LA    RF,BANNTB                                                        
         B     ITER                                                             
*                                                                               
BANNTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBANN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBANN)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY A BANK NAME FIELD                                                     
*-------------------------------------------------------------                  
DISBANN  MVC   HOBANN,FVADDR                                                    
         NI    FVATRB,X'FF'-FVAPROT                                             
*                                                                               
         CLI   CSACT,A#LST                                                      
         BE    DISBAN10                                                         
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(BANKHUB-BANKRECD),BANKRECD                                 
*                                                                               
         L     R1,=A(XOGENDIR+XOREAD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   DISBAN10                                                         
         L     R1,=A(XOGENFIL+XOGET+XIO2)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   DISBAN10                                                         
*                                                                               
         L     R2,AIO2                                                          
DISBAN10 GOTO1 AGETNAM,(R2)                                                     
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE BANK NAME FIELD                                                      
*-------------------------------------------------------------                  
VALBANN  BRAS  RE,REPNAM                 REPLACE THE NAME ELEMENT               
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR HUB CODE                                                      
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
HUBCDTA  LA    RF,HUBCTB                                                        
         B     ITER                                                             
*                                                                               
HUBCTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISHUBC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHUBC)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTHUBC)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTHUBC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTHUBC)                               
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY A HUB CODE FIELD                                                      
*-------------------------------------------------------------                  
DISHUBC  MVC   FVIFLD(L'BANKHUB),BANKHUB                                        
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE HUB CODE FIELD                                                       
*-------------------------------------------------------------                  
VALHUBC  MVC   HOHUB,FVADDR                                                     
*                                                                               
         CLI   FVILEN,0                   KEY FIELD, BUT OPTIONAL               
         BE    EXITOK                                                           
         MVC   BANKHUB,FVIFLD                                                   
*                                                                               
         LA    R2,IOKEY                   MUST HAVE VALID BANK                  
         XC    IOKEY,IOKEY                                                      
         MVI   BANKTYP,BANKTYPQ                                                 
         MVI   BANKSUB,BANKSGQ                                                  
         L     R1,HOBAN                                                         
         ST    R1,FVADDR                                                        
         MVC   BANKCDE,FVIFLD-FVIHDR(R1)                                        
         L     R1,=A(XOGENDIR+XOREAD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNV                                                           
         L     R1,HOHUB                                                         
         ST    R1,FVADDR                                                        
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
*-------------------------------------------------------------                  
* DISPLAY HUBK CODE FILTER                                                      
*-------------------------------------------------------------                  
DFLTHUBC MVC   FVIFLD,FLTIFLD                                                   
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE HUB CODE FILTER                                                      
*-------------------------------------------------------------                  
VFLTHUBC MVC   FLTIFLD,FVIFLD                                                   
         MVC   BANKHUB,FLTIFLD                                                  
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* PERFORM HUB CODE FILTER                                                       
*-------------------------------------------------------------                  
DOFTHUBC CLC   BANKHUB,FLTIFLD                                                  
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         B     FLTXE                                                            
                                                                                
***********************************************************************         
* DATA OBJECT FOR HUB NAME                                                      
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
HUBNDTA  LA    RF,HUBNTB                                                        
         B     ITER                                                             
*                                                                               
HUBNTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISHUBN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHUBN)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY A HUB NAME FIELD                                                      
*-------------------------------------------------------------                  
DISHUBN  MVC   HOHUBN,FVADDR                                                    
         NI    FVATRB,X'FF'-FVAPROT                                             
*                                                                               
         CLC   BANKHUB,BCSPACES                                                 
         BNH   EXITOK                                                           
         L     R1,HOBANN                                                        
         OI    FVATRB-FVIHDR(R1),FVAPROT                                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(BANKBRN-BANKRECD),BANKRECD                                 
*                                                                               
         L     R1,=A(XOGENDIR+XOREAD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   DISH020                                                          
         L     R1,=A(XOGENFIL+XOGET+XIO2)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   DISH020                                                          
*                                                                               
         L     R2,AIO2                                                          
DISH020  GOTO1 AGETNAM,(R2)                                                     
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE HUB NAME FIELD                                                       
*-------------------------------------------------------------                  
VALHUBN  CLC   BANKHUB,BCSPACES                                                 
         BNH   EXITOK                                                           
         BRAS  RE,REPNAM                 REPLACE THE NAME ELEMENT               
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR BRANCH CODE                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
BRACDTA  LA    RF,BRACTB                                                        
         B     ITER                                                             
*                                                                               
BRACTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBRAC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBRAC)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY A BRANCH CODE FIELD                                                   
*-------------------------------------------------------------                  
DISBRAC  MVC   FVIFLD(L'BANKBRN),BANKBRN                                        
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE BRANCH CODE FIELD                                                    
*-------------------------------------------------------------                  
VALBRAC  MVC   HOBRA,FVADDR                                                     
*                                                                               
         CLI   FVILEN,0                   KEY FIELD, BUT OPTIONAL               
         BZ    EXITOK                                                           
         MVC   BANKBRN,FVIFLD                                                   
*                                                                               
         L     R1,HOHUB                   MAKE SURE HUB EXISTS                  
         ST    R1,FVADDR                                                        
         LTR   R1,R1                                                            
         BZ    EXITNO                                                           
         CLI   FVILEN-FVIHDR(R1),0                                              
         BE    EXITNO                                                           
*                                                                               
         LA    R2,IOKEY                   MUST HAVE VALID BANK                  
         XC    IOKEY,IOKEY                                                      
         MVI   BANKTYP,BANKTYPQ                                                 
         MVI   BANKSUB,BANKSGQ                                                  
         L     R1,HOBAN                                                         
         MVC   BANKCDE,FVIFLD-FVIHDR(R1)                                        
         L     R1,HOHUB                                                         
         MVC   BANKHUB,FVIFLD-FVIHDR(R1)                                        
         L     R1,=A(XOGENDIR+XOREAD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNV                                                           
         L     R1,HOBRA                                                         
         ST    R1,FVADDR                                                        
         B     EXITOK                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* DATA OBJECT FOR BRANCH NAME                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
BRANDTA  LA    RF,BRANTB                                                        
         B     ITER                                                             
*                                                                               
BRANTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBRAN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBRAN)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY A BRANCH NAME FIELD                                                   
*-------------------------------------------------------------                  
DISBRAN  CLC   BANKBRN,BCSPACES                                                 
         BNH   EXITOK                                                           
         L     R1,HOHUBN                                                        
         OI    FVATRB-FVIHDR(R1),FVAPROT                                        
*                                                                               
         GOTO1 AGETNAM,(R2)                                                     
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE BRANCH NAME FIELD                                                    
*-------------------------------------------------------------                  
VALBRAN  CLC   BANKBRN,BCSPACES                                                 
         BNH   EXITOK                                                           
         BRAS  RE,REPNAM                 REPLACE THE NAME ELEMENT               
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 1                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
ADL1DTA  LA    RF,ADL1TB                                                        
         B     ITER                                                             
*                                                                               
ADL1TB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADL1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADL1)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY ADDRESS LINE 1                                                        
*-------------------------------------------------------------                  
DISADL1  MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADLINE1           LENGTH OF DATA                         
         MVI   PDIS,IADLINE1-IADELD      DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE ADDRESS LINE 1                                                       
*-------------------------------------------------------------                  
VALADL1  MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADLINE1           LENGTH OF DATA                         
         MVI   PDIS,IADLINE1-IADELD      DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR ADDRESS LINE 2                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
ADL2DTA  LA    RF,ADL2TB                                                        
         B     ITER                                                             
*                                                                               
ADL2TB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADL2)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADL2)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY ADDRESS LINE 2                                                        
*-------------------------------------------------------------                  
DISADL2  MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADLINE2           LENGTH OF DATA                         
         MVI   PDIS,IADLINE2-IADELD      DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE ADDRESS LINE 2                                                       
*-------------------------------------------------------------                  
VALADL2  MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADLINE2           LENGTH OF DATA                         
         MVI   PDIS,IADLINE2-IADELD      DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR CITY                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
CITYDTA  LA    RF,CITYTB                                                        
         B     ITER                                                             
*                                                                               
CITYTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCITY)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCITY)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY CITY                                                                  
*-------------------------------------------------------------                  
DISCITY  MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADCITY            LENGTH OF DATA                         
         MVI   PDIS,IADCITY-IADELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE CITY                                                                 
*-------------------------------------------------------------                  
VALCITY  MVC   HOCITY,FVADDR                                                    
*                                                                               
         MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADCITY            LENGTH OF DATA                         
         MVI   PDIS,IADCITY-IADELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR STATE/PROV                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
STPDTA   LA    RF,STPTB                                                         
         B     ITER                                                             
*                                                                               
STPTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTP)                                 
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY STATE                                                                 
*-------------------------------------------------------------                  
DISSTP   MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADSTATE           LENGTH OF DATA                         
         MVI   PDIS,IADSTATE-IADELD      DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE STATE                                                                
*-------------------------------------------------------------                  
VALSTP   MVC   HOSTATE,FVADDR                                                   
*                                                                               
         MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADSTATE           LENGTH OF DATA                         
         MVI   PDIS,IADSTATE-IADELD      DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR ZIP/POSTAL CODE                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
ZIPDTA   LA    RF,ZIPTB                                                         
         B     ITER                                                             
*                                                                               
ZIPTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISZIP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALZIP)                                 
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY ZIP/POSTAL CODE                                                       
*-------------------------------------------------------------                  
DISZIP   MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADZIP             LENGTH OF DATA                         
         MVI   PDIS,IADZIP-IADELD        DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE ZIP/POSTAL CODE                                                      
*-------------------------------------------------------------                  
VALZIP   MVC   HOZIP,FVADDR                                                     
*                                                                               
         MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADZIP             LENGTH OF DATA                         
         MVI   PDIS,IADZIP-IADELD        DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR COUNTRY                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
CTRYDTA  LA    RF,CTRYTB                                                        
         B     ITER                                                             
*                                                                               
CTRYTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCTRY)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCTRY)                                
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFLTCTRY)                              
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY COUNTRY                                                               
*-------------------------------------------------------------                  
DISCTRY  MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADCTRY            LENGTH OF DATA                         
         MVI   PDIS,IADCTRY-IADELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE COUNTRY                                                              
*-------------------------------------------------------------                  
VALCTRY  MVI   PELE,IADELQ               ELEMENT CODE                           
         MVI   PLEN,L'IADCTRY            LENGTH OF DATA                         
         MVI   PDIS,IADCTRY-IADELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* DEFAULT COUNTRY                                                               
*-------------------------------------------------------------                  
DFLTCTRY MVC   FVIFLD(2),=C'US'           ASSUME USA                            
         B     VALCTRY                                                          
                                                                                
***********************************************************************         
* DATA OBJECT FOR CLIENT SERVICE CONTACT NAME                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
CCNDTA   LA    RF,CCNTB                                                         
         B     ITER                                                             
*                                                                               
CCNTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCCN)                                 
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY CLIENT SERVICE CONTACT NAME                                           
*-------------------------------------------------------------                  
DISCCN   MVI   PELE,BCOELCQ              ELEMENT CODE                           
         MVI   PLEN,L'BCONA              LENGTH OF DATA                         
         MVI   PDIS,BCONA-BCOELD         DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE CLIENT SERVICE CONTACT NAME                                          
*-------------------------------------------------------------                  
VALCCN   MVI   PELE,BCOELCQ              ELEMENT CODE                           
         MVI   PLEN,L'BCONA              LENGTH OF DATA                         
         MVI   PDIS,BCONA-BCOELD         DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR CLIENT SERVICE CONTACT PHONE                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
CCPADTA  LA    RF,CCPATB                 PHONE AREA CODE                        
         B     ITER                                                             
*                                                                               
CCPATB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCPA)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCCPA)                                
         DC    AL1(EOT)                                                         
*----------------------------------------------------------------------         
CCP1DTA  LA    RF,CCP1TB                 PHONE FIRST 3 DIGITS                   
         B     ITER                                                             
*                                                                               
CCP1TB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCP1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCCP1)                                
         DC    AL1(EOT)                                                         
*----------------------------------------------------------------------         
CCP2DTA  LA    RF,CCP2TB                 PHONE LAST 4 DIGITS                    
         B     ITER                                                             
*                                                                               
CCP2TB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCP2)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCCP2)                                
         DC    AL1(EOT)                                                         
*----------------------------------------------------------------------         
CCPEDTA  LA    RF,CCPETB                 PHONE EXTENSION                        
         B     ITER                                                             
*                                                                               
CCPETB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCPE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCCPE)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY TECHNICAL CONTACT PHONE NUMBER                                        
*-------------------------------------------------------------                  
DISCCPA  MVI   PLEN,3                    3 DIGITS                               
         MVI   PDIS,BCOPH-BCOELD         +00 - PHONE AREA CODE                  
         B     DISCCP                                                           
*                                                                               
DISCCP1  MVI   PLEN,3                    3 DIGITS                               
         MVI   PDIS,3+(BCOPH-BCOELD)     +03 - PHONE FIRST 3 DIGITS             
         B     DISCCP                                                           
*                                                                               
DISCCP2  MVI   PLEN,4                    4 DIGITS                               
         MVI   PDIS,6+(BCOPH-BCOELD)     +06 - PHONE LAST 4 DIGITS              
         B     DISCCP                                                           
*                                                                               
DISCCPE  MVI   PLEN,5                    5 DIGITS                               
         MVI   PDIS,10+(BCOPH-BCOELD)    +10 - PHONE EXTENSION                  
*                                                                               
DISCCP   MVI   PELE,BCOELCQ              ELEMENT CODE                           
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE TECHNICAL CONTACT PHONE NUMBER                                       
*-------------------------------------------------------------                  
VALCCPA  MVI   PLEN,3                    3 DIGITS                               
         MVI   PDIS,BCOPH-BCOELD         +00 - PHONE AREA CODE                  
         B     VALCCP                                                           
*                                                                               
VALCCP1  MVI   PLEN,3                    3 DIGITS                               
         MVI   PDIS,3+(BCOPH-BCOELD)     +03 - PHONE FIRST 3 DIGITS             
         B     VALCCP                                                           
*                                                                               
VALCCP2  MVI   PLEN,4                    4 DIGITS                               
         MVI   PDIS,6+(BCOPH-BCOELD)     +06 - PHONE LAST 4 DIGITS              
         B     VALCCP                                                           
*                                                                               
VALCCPE  MVI   PLEN,5                    5 DIGITS                               
         MVI   PDIS,10+(BCOPH-BCOELD)    +10 - PHONE EXTENSION                  
*                                                                               
VALCCP   MVI   PELE,BCOELCQ              ELEMENT CODE                           
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR CLIENT SERVICE CONTACT EMAIL                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
CCEDTA   LA    RF,CCETB                                                         
         B     ITER                                                             
*                                                                               
CCETB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCCE)                                 
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY CLIENT SERVICE CONTACT EMAIL                                          
*-------------------------------------------------------------                  
DISCCE   MVI   PELE,BCOELCQ              ELEMENT CODE                           
         MVI   PDIS,BCOEM-BCOELD         DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'V'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE CLIENT SERVICE CONTACT EMAIL                                         
*-------------------------------------------------------------                  
VALCCE   MVI   PELE,BCOELCQ              ELEMENT CODE                           
         MVC   PLEN,FVILEN               LENGTH OF DATA                         
         MVI   PDIS,BCOEM-BCOELD         DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'V'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR ADVANTIS USER ID (USE)                                        
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
ADVIDTA  LA    RF,ADVITB                                                        
         B     ITER                                                             
*                                                                               
ADVITB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADVI)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADVI)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY ADVANTIS USER ID (USE)                                                
*-------------------------------------------------------------                  
DISADVI  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATUSER            LENGTH OF DATA                         
         MVI   PDIS,BATUSER-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE ADVANTIS USER ID (USE)                                               
*-------------------------------------------------------------                  
VALADVI  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATUSER            LENGTH OF DATA                         
         MVI   PDIS,BATUSER-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR ADVANTIS ACCOUNT # (ACC)                                      
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
ADVADTA  LA    RF,ADVATB                                                        
         B     ITER                                                             
*                                                                               
ADVATB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADVA)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADVA)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY ADVANTIS ACCOUNT # (ACC)                                              
*-------------------------------------------------------------                  
DISADVA  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATACCN            LENGTH OF DATA                         
         MVI   PDIS,BATACCN-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE ADVANTIS ACCOUNT # (ACC)                                             
*-------------------------------------------------------------                  
VALADVA  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATACCN            LENGTH OF DATA                         
         MVI   PDIS,BATACCN-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR MESSAGE CLASS (CLA)                                           
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
MESCDTA  LA    RF,MESCTB                                                        
         B     ITER                                                             
*                                                                               
MESCTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMESC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMESC)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY MESSAGE CLASS (CLA)                                                   
*-------------------------------------------------------------                  
DISMESC  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATCLAS            LENGTH OF DATA                         
         MVI   PDIS,BATCLAS-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE MESSAGE CLASS (CLA)                                                  
*-------------------------------------------------------------                  
VALMESC  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATCLAS            LENGTH OF DATA                         
         MVI   PDIS,BATCLAS-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR CHARGE (CHA)                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
CHRGDTA  LA    RF,CHRGTB                                                        
         B     ITER                                                             
*                                                                               
CHRGTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCHRG)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCHRG)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY CHARGE (CHA)                                                          
*-------------------------------------------------------------                  
DISCHRG  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATCHRG            LENGTH OF DATA                         
         MVI   PDIS,BATCHRG-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE CHARGE (CHA)                                                         
*-------------------------------------------------------------                  
VALCHRG  CLI   FVILEN,0                                                         
         BE    VALCH1                                                           
         CLI   FVIFLD,C'1'                                                      
         BL    EXITNN                                                           
         CLI   FVIFLD,C'9'                                                      
         BH    EXITNN                                                           
                                                                                
VALCH1   MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATCHRG            LENGTH OF DATA                         
         MVI   PDIS,BATCHRG-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT TRANSMISSION TYPE                                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
TRNTDTA  LA    RF,TRNTTB                                                        
         B     ITER                                                             
*                                                                               
TRNTTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTRNT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTRNT)                                
         DC    AL1(EOT)                                                         
*                                                                               
*-------------------------------------------------------------                  
* DISPLAY TRANSMISSION TYPE                                                     
*-------------------------------------------------------------                  
         USING BANKRECD,R2                                                      
DISTRNT  DS    0H                                                               
         GOTO1 AGETEL,BOPARM,('BATELQ',BANKRECD),0                              
         LA    RF,BOELEM                                                        
         USING BATELD,RF                                                        
*                                                                               
         CLI   BATTTYP,0           IS THERE ANY OVERRIDE?                       
         BE    EXITOK                                                           
         CLI   BATTTYP,X'40'                                                    
         BE    EXITOK                                                           
*                                                                               
         LA    RE,TRNTYTAB                                                      
DISTRT10 CLI   0(RE),EOF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   10(1,RE),BATTTYP                                                 
         BE    *+12                                                             
         AHI   RE,L'TRNTYTAB                                                    
         B     DISTRT10                                                         
*                                                                               
         MVC   FVIFLD(10),0(RE)                                                 
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
*-------------------------------------------------------------                  
* VALIDATE TRANSMISSION TYPE                                                    
*-------------------------------------------------------------                  
         USING BANKRECD,R2                                                      
VALTRNT  DS    0H                                                               
         GOTO1 AGETEL,BOPARM,('BATELQ',BANKRECD),0                              
         LA    RF,BOELEM                                                        
         USING BATELD,RF                                                        
*                                                                               
         MVI   BATTTYP,0                                                        
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LA    RE,TRNTYTAB                                                      
VALTRT10 CLI   0(RE),EOF                                                        
         BE    EXITNV                                                           
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,0(RE),FVIFLD                                                  
         BE    *+12                                                             
         AHI   RE,L'TRNTYTAB                                                    
         B     VALTRT10                                                         
*                                                                               
         MVC   FVIFLD(L'BATTTYP),10(RE)  OVERWRITE FOR FEL ROUTINE              
         MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATTTYP            LENGTH OF DATA                         
         MVI   PDIS,BATTTYP-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
TRNTYTAB DS    0CL11                                                            
         DC    CL10'EDICT',AL1(BATTTEDT)                                        
         DC    CL10'MQ',AL1(BATTTMQ)                                            
         DC    AL1(EOF)                                                         
*                                                                               
***********************************************************************         
* DATA OBJECT TRANSMISSION KEY                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
TRNKDTA  LA    RF,TRNKTB                                                        
         B     ITER                                                             
*                                                                               
TRNKTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTRNK)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTRNK)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY TRANSMISSION KEY                                                      
*-------------------------------------------------------------                  
         USING BANKRECD,R2                                                      
DISTRNK  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATTKEY            LENGTH OF DATA                         
         MVI   PDIS,BATTKEY-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE TRANSMISSION KEY                                                     
*-------------------------------------------------------------                  
         USING BANKRECD,R2                                                      
VALTRNK  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         GOTO1 AGETEL,BOPARM,('BATELQ',BANKRECD),0                              
         LA    RF,BOELEM                                                        
         USING BATELD,RF                                                        
*                                                                               
         CLI   BATTTYP,BATTTEDT    IS THIS AN EDICT TRANSMISSION                
         BNE   VALEK10                                                          
         MVI   BATTMOD,0           DEFAULT MOD TO 0                             
         DROP  RF                                                               
*                                                                               
         USING EDIKEYD,R5                                                       
         LA    R5,IOKEY                                                         
         XC    EDIKEY,EDIKEY                                                    
         MVI   EDIKSYS,EDIKSYSQ    X'05' - KEY SYSTEM FOR ALL SYSTEMS           
         MVI   EDITYPE,EDITYPEQ    X'07' - EDICT TYPE                           
         MVC   EDINAME,FVIFLD                                                   
         OC    EDINAME,BCSPACES                                                 
         L     R1,=A(XOCONFIL+XOREAD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNF                                                           
         DROP  R5                                                               
                                                                                
VALEK10  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATTKEY            LENGTH OF DATA                         
         MVI   PDIS,BATTKEY-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
         DROP  R2                                                               
                                                                                
***********************************************************************         
* DATA OBJECT FORMAT KEY                                                        
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
FORKDTA  LA    RF,FORKTB                                                        
         B     ITER                                                             
*                                                                               
FORKTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFORK)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFORK)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY FORMAT KEY                                                            
*-------------------------------------------------------------                  
DISFORK  MVI   PELE,BAGELQ               ELEMENT CODE                           
         MVI   PLEN,L'BAGFORM            LENGTH OF DATA                         
         MVI   PDIS,BAGFORM-BAGELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE FORMAT KEY                                                           
*-------------------------------------------------------------                  
VALFORK  MVI   PELE,BAGELQ               ELEMENT CODE                           
         MVI   PLEN,L'BAGFORM            LENGTH OF DATA                         
         MVI   PDIS,BAGFORM-BAGELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT DATASET NAME                                                      
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
*SNDTA   LA    RF,DSNTB                                                         
*        B     ITER                                                             
*                                                                               
*SNTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSN)                                 
*        DC    AL1(DVAL),AL1(0,0,0),AL4(VALDSN)                                 
*        DC    AL1(EOT)                                                         
*                                                                               
*-------------------------------------------------------------                  
* DISPLAY DATASET NAME                                                          
*-------------------------------------------------------------                  
*ISDSN   MVI   PELE,BAGELQ               ELEMENT CODE                           
*        MVI   PLEN,L'BAGDSN             LENGTH OF DATA                         
*        MVI   PDIS,BAGDSN-BAGELD        DISPLACEMENT INTO ELEMENT              
*        MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
*        BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
*        B     EXITOK                                                           
*                                                                               
*-------------------------------------------------------------                  
* VALIDATE DATASET NAME                                                         
*-------------------------------------------------------------                  
*ALDSN   MVI   PELE,BAGELQ               ELEMENT CODE                           
*        MVI   PLEN,L'BAGDSN             LENGTH OF DATA                         
*        MVI   PDIS,BAGDSN-BAGELD        DISPLACEMENT INTO ELEMENT              
*        MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
*        BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
*        B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DATA OBJECT BANK ROUTING NUMBER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
BNKRDTA  LA    RF,BNKRTB                                                        
         B     ITER                                                             
*                                                                               
BNKRTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBNKR)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBNKR)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY BANK ROUTING NUMBER                                                   
*-------------------------------------------------------------                  
DISBNKR  MVI   PELE,BAGELQ               ELEMENT CODE                           
         MVI   PLEN,L'BAGRNO             LENGTH OF DATA                         
         MVI   PDIS,BAGRNO-BAGELD        DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE BANK ROUTING NUMBER                                                  
*-------------------------------------------------------------                  
VALBNKR  MVI   PELE,BAGELQ               ELEMENT CODE                           
         MVI   PLEN,L'BAGRNO             LENGTH OF DATA                         
         MVI   PDIS,BAGRNO-BAGELD        DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT COMPRESSION                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
CMPRDTA  LA    RF,CMPRTB                                                        
         B     ITER                                                             
*                                                                               
CMPRTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCMPR)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCMPR)                                
         DC    AL1(EOT)                                                         
*                                                                               
*-------------------------------------------------------------                  
* DISPLAY COMPRESSION                                                           
*-------------------------------------------------------------                  
DISCMPR  MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATCMPR            LENGTH OF DATA                         
         MVI   PDIS,BATCMPR-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
*                                                                               
*-------------------------------------------------------------                  
* VALIDATE COMPRESSION                                                          
*-------------------------------------------------------------                  
VALCMPR  CLC   =C'YES',FVIFLD            Y/N ONLY                               
         BE    VALCMPR5                                                         
         CLC   =C'NO ',FVIFLD                                                   
         BNE   EXITNV                                                           
VALCMPR5 MVI   PELE,BATELQ               ELEMENT CODE                           
         MVI   PLEN,L'BATCMPR            LENGTH OF DATA                         
         MVI   PDIS,BATCMPR-BATELD       DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DATA OBJECT FOR TECHNICAL CONTACT NAME                                        
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
TCNDTA   LA    RF,TCNTB                                                         
         B     ITER                                                             
*                                                                               
TCNTB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTCN)                                 
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY TECHNICAL CONTACT NAME                                                
*-------------------------------------------------------------                  
DISTCN   MVI   PELE,BCOELTQ              ELEMENT CODE                           
         MVI   PLEN,L'BCONA              LENGTH OF DATA                         
         MVI   PDIS,BCONA-BCOELD         DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE TECHNICAL CONTACT NAME                                               
*-------------------------------------------------------------                  
VALTCN   MVI   PELE,BCOELTQ              ELEMENT CODE                           
         MVI   PLEN,L'BCONA              LENGTH OF DATA                         
         MVI   PDIS,BCONA-BCOELD         DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR TECHNICAL CONTACT PHONE                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
TCPADTA  LA    RF,TCPATB                 PHONE AREA CODE                        
         B     ITER                                                             
*                                                                               
TCPATB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCPA)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTCPA)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
TCP1DTA  LA    RF,TCP1TB                 PHONE FIRST 3 DIGITS                   
         B     ITER                                                             
*                                                                               
TCP1TB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCP1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTCP1)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
TCP2DTA  LA    RF,TCP2TB                 PHONE LAST 4 DIGITS                    
         B     ITER                                                             
*                                                                               
TCP2TB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCP2)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTCP2)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
TCPEDTA  LA    RF,TCPETB                 PHONE EXTENSION                        
         B     ITER                                                             
*                                                                               
TCPETB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCPE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTCPE)                                
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY TECHNICAL CONTACT PHONE NUMBER                                        
*-------------------------------------------------------------                  
DISTCPA  MVI   PLEN,3                    3 DIGITS                               
         MVI   PDIS,BCOPH-BCOELD         +00 - PHONE AREA CODE                  
         B     DISTCP                                                           
*                                                                               
DISTCP1  MVI   PLEN,3                    3 DIGITS                               
         MVI   PDIS,3+(BCOPH-BCOELD)     +03 - PHONE FIRST 3 DIGITS             
         B     DISTCP                                                           
*                                                                               
DISTCP2  MVI   PLEN,4                    4 DIGITS                               
         MVI   PDIS,6+(BCOPH-BCOELD)     +06 - PHONE LAST 4 DIGITS              
         B     DISTCP                                                           
*                                                                               
DISTCPE  MVI   PLEN,5                    5 DIGITS                               
         MVI   PDIS,10+(BCOPH-BCOELD)    +10 - PHONE EXTENSION                  
*                                                                               
DISTCP   MVI   PELE,BCOELTQ              ELEMENT CODE                           
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE TECHNICAL CONTACT PHONE NUMBER                                       
*-------------------------------------------------------------                  
VALTCPA  MVI   PLEN,3                    3 DIGITS                               
         MVI   PDIS,BCOPH-BCOELD         +00 - PHONE AREA CODE                  
         B     VALTCP                                                           
*                                                                               
VALTCP1  MVI   PLEN,3                    3 DIGITS                               
         MVI   PDIS,3+(BCOPH-BCOELD)     +03 - PHONE FIRST 3 DIGITS             
         B     VALTCP                                                           
*                                                                               
VALTCP2  MVI   PLEN,4                    4 DIGITS                               
         MVI   PDIS,6+(BCOPH-BCOELD)     +06 - PHONE LAST 4 DIGITS              
         B     VALTCP                                                           
*                                                                               
VALTCPE  MVI   PLEN,5                    5 DIGITS                               
         MVI   PDIS,10+(BCOPH-BCOELD)    +10 - PHONE EXTENSION                  
*                                                                               
VALTCP   MVI   PELE,BCOELTQ              ELEMENT CODE                           
         MVI   PFOV,C'F'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* DATA OBJECT FOR TECHNICAL CONTACT EMAIL                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* R3 HOLDS A(FIELD TABLE ENTRY)                                                 
***********************************************************************         
TCEDTA   LA    RF,TCETB                                                         
         B     ITER                                                             
*                                                                               
TCETB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTCE)                                 
         DC    AL1(EOT)                                                         
                                                                                
*-------------------------------------------------------------                  
* DISPLAY TECHNICAL CONTACT EMAIL                                               
*-------------------------------------------------------------                  
DISTCE   MVI   PELE,BCOELTQ              ELEMENT CODE                           
         MVI   PDIS,BCOEM-BCOELD         DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'V'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,ELF                    MOVE ELEMENT DATA TO FIELD             
         B     EXITOK                                                           
                                                                                
*-------------------------------------------------------------                  
* VALIDATE TECHNICAL CONTACT EMAIL                                              
*-------------------------------------------------------------                  
VALTCE   MVI   PELE,BCOELTQ              ELEMENT CODE                           
         MVC   PLEN,FVILEN               LENGTH OF DATA                         
         MVI   PDIS,BCOEM-BCOELD         DISPLACEMENT INTO ELEMENT              
         MVI   PFOV,C'V'                 C'F' FIXED OR C'V' VARIABLE            
         BRAS  RE,FEL                    STORE FIELD DATA IN ELEMENT            
         B     EXITOK                                                           
                                                                                
***********************************************************************         
* LIST OBJECT                                                                   
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 HOLDS CURRENT KEY BUILD AREA                                               
* P4 HOLDS PREVIOUS KEY                                                         
***********************************************************************         
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING BANKRECD,R2                                                      
LAST     USING BANKRECD,R3                                                      
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
                                                                                
***********************************************************************         
* FIRST FOR LIST                                                                
***********************************************************************         
FLST     MVC   IOKEY(L'BANKEY),THIS.BANKRECD                                    
*                                                                               
         L     R1,=A(XOGENDIR+XOHIGH+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
         B     NLST010                                                          
                                                                                
***********************************************************************         
* NEXT FOR LIST                                                                 
***********************************************************************         
X        USING BANKRECD,IOKEY                                                   
NLST     L     R1,=A(XOGENDIR+XOSEQ+XIO1)                                       
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
*                                                                               
NLST010  CLI   X.BANKTYP,BANKTYPQ                                               
         BNE   EXITL                                                            
         CLI   X.BANKSUB,BANKSGQ                                                
         BNE   EXITL                                                            
*                                                                               
         MVC   THIS.BANKRECD(BANKLEN),IOKEY   WE WANT THIS KEY                  
         B     EXITOK                                                           
         DROP  THIS,X,LAST                                                      
                                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
EOF      EQU   FF                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* ADD DEFAULT ELEMENTS                                                          
***********************************************************************         
         USING BANKRECD,R2                                                      
DEFELS   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   GSSMPAGE,1                1ST PAGE?                              
         BNE   DEFE020                                                          
*                                                                               
         GOTO1 AGETEL,BOPARM,('IADELQ',BANKRECD),0                              
         BE    DEFE010                                                          
         XC    BOELEM,BOELEM             ADDRESS ELEMENT                        
         LA    R4,BOELEM                                                        
         USING IADELD,R4                                                        
         MVI   IADEL,IADELQ                                                     
         MVI   IADLN,IADLNQ                                                     
         GOTO1 AADDEL,BOPARM,('IADELQ',BANKRECD),0                              
*                                                                               
DEFE010  GOTO1 AGETEL,BOPARM,('BCOELCQ',BANKRECD),0                             
         BE    DEFEX                                                            
         XC    BOELEM,BOELEM             CS CONTACT ELEMENT X'26'               
         LA    R4,BOELEM                                                        
         USING BCOELD,R4                                                        
         MVI   BCOEL,BCOELCQ                                                    
         MVI   BCOLN,BCOLNQ                                                     
         GOTO1 AADDEL,BOPARM,('BCOELCQ',BANKRECD),0                             
         B     DEFEX                                                            
*                                                                               
DEFE020  CLI   GSSMPAGE,2                2ND PAGE?                              
         BNE   DEFEX                                                            
*                                                                               
         GOTO1 AGETEL,BOPARM,('BCOELTQ',BANKRECD),0                             
         BE    DEFE030                                                          
         XC    BOELEM,BOELEM             TECH CONTACT ELEMENT X'27'             
         LA    R4,BOELEM                                                        
         USING BCOELD,R4                                                        
         MVI   BCOEL,BCOELTQ                                                    
         MVI   BCOLN,BCOLNQ                                                     
         GOTO1 AADDEL,BOPARM,('BCOELTQ',BANKRECD),0                             
*                                                                               
DEFE030  GOTO1 AGETEL,BOPARM,('BAGELQ',BANKRECD),0                              
         BE    DEFE040                                                          
         XC    BOELEM,BOELEM             BANK GENERAL ELEM      X'5B'           
         LA    R4,BOELEM                                                        
         USING BAGELD,R4                                                        
         MVI   BAGEL,BAGELQ                                                     
         MVI   BAGLN,BAGLNQ                                                     
         GOTO1 AADDEL,BOPARM,('BAGELQ',BANKRECD),0                              
*                                                                               
DEFE040  GOTO1 AGETEL,BOPARM,('BATELQ',BANKRECD),0                              
         BE    DEFEX                                                            
         XC    BOELEM,BOELEM             BANK TRANSMISSION ELEM X'5E'           
         LA    R4,BOELEM                                                        
         USING BATELD,R4                                                        
         MVI   BATEL,BATELQ                                                     
         MVI   BATLN,BATLNQ                                                     
         GOTO1 AADDEL,BOPARM,('BATELQ',BANKRECD),0                              
*                                                                               
DEFEX    J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* STORE FIELD DATA INTO ELEMENT                                                 
*        ON ENTRY R2   = A(RECORD)                                              
*                 PELE = ELEMENT CODE                                           
*                 PLEN = LENGTH OF DATA                                         
*                 PDIS = DISPLACEMENT INTO ELEMENT                              
*                 PFOV = C'F' FIXED OR C'V' VARIABLE LENGTH DATA                
***********************************************************************         
         USING BANKRECD,R2                                                      
FEL      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 AGETEL,BOPARM,(PELE,BANKRECD),0                                  
         LA    R4,BOELEM                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PLEN                   LENGTH OF DATA                         
         SR    R0,R0                                                            
         IC    R0,PDIS                   DSPLCMNT TO DATA IN ELEMENT            
*                                                                               
         CLI   PFOV,C'F'                 FIXED LENGTH VARIABLE?                 
         BE    FEL010                    . YES                                  
         AR    R1,R0                     ADD LENGTH OF DATA                     
         STC   R1,1(R4)                  STORE LENGTH OF ELEMENT                
         SR    R1,R0                                                            
FEL010   AR    R4,R0                     ADD DISPLACEMENT TO BOELEM             
*                                                                               
         SHI   R1,1                      DECREMENT LENGTH                       
         BM    FELX                                                             
         EX    R1,*+8                    MOVE FIELD VALUE INTO ELEMENT          
         B     *+10                                                             
         MVC   0(0,R4),FVIFLD                                                   
                                                                                
         GOTO1 ADELEL,BOPARM,(PELE,BANKRECD),0                                  
         GOTO1 AADDEL,BOPARM,(PELE,BANKRECD),0                                  
FELX     J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MOVE ELEMENT DATA INTO FIELD                                                  
*        ON ENTRY R2   = A(RECORD)                                              
*                 PELE = ELEMENT CODE                                           
*                 PLEN = LENGTH OF DATA                                         
*                 PDIS = DISPLACEMENT INTO ELEMENT                              
*                 PFOV = C'F' FIXED OR C'V' VARIABLE LENGTH DATA                
***********************************************************************         
         USING BANKRECD,R2                                                      
ELF      NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 AGETEL,BOPARM,(PELE,BANKRECD),0                                  
         LA    R4,BOELEM                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,PLEN                   LENGTH OF DATA                         
         SR    R0,R0                                                            
         IC    R0,PDIS                   DSPLCMNT TO DATA IN ELEMENT            
*                                                                               
         CLI   PFOV,C'F'                 FIXED LENGTH VARIABLE?                 
         BE    ELF010                    . YES                                  
         IC    R1,1(R4)                  PICK UP LENGTH                         
         SR    R1,R0                                                            
ELF010   AR    R4,R0                     ADD DISPLACEMENT TO BOELEM             
*                                                                               
         SHI   R1,1                      DECREMENT LENGTH                       
         BM    ELFX                                                             
         EX    R1,*+8                    MOVE FIELD VALUE INTO ELEMENT          
         B     *+10                                                             
         MVC   FVIFLD(0),0(R4)                                                  
                                                                                
ELFX     J     XIT                                                              
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* REPLACE NAME ELEMENT                                                          
***********************************************************************         
REPNAM   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         GOTO1 ADELEL,BOPARM,('NAMELQ',BANKRECD),0                              
         XC    BOELEM,BOELEM                                                    
         LA    R4,BOELEM                                                        
         USING NAMELD,R4                                                        
         MVI   NAMEL,NAMELQ                                                     
         SR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         AHI   R1,NAMLN1Q                                                       
         STC   R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         BM    REPNAMX                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   NAMEREC(0),FVIFLD                                                
         GOTO1 AADDEL,BOPARM,('NAMELQ',BANKRECD),0                              
*                                                                               
REPNAMX  J     XIT                                                              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
                                                                                
***********************************************************************         
* INCLUDED BOOKS                                                                
***********************************************************************         
* CTFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
                                                                                
* GEGENBNK                                                                      
         PRINT OFF                                                              
       ++INCLUDE GEGENBNK                                                       
         PRINT ON                                                               
                                                                                
* ACGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
                                                                                
* CTGENEDICT                                                                    
         PRINT OFF                                                              
       ++INCLUDE CTGENEDICT                                                     
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
*                                                                               
MYPARAM  DS    0F                                                               
PELE     DS    X               ELEMENT CODE                                     
PLEN     DS    X               LENGTH OF DATA                                   
PDIS     DS    X               DISPLACEMENT INTO ELEMENT                        
PFOV     DS    X               C'F' FIXED OR C'V' VARIABLE LENGTH DATA          
                                                                                
***********************************************************************         
* HOLDING AREA FOR DATA AND FIELD ADDRESSES                                     
***********************************************************************         
HOAREA   DS    0A                                                               
HOBAN    DS    A                   BANK CODE                                    
HOHUB    DS    A                   HUB CODE                                     
HOBRA    DS    A                   BRANCH CODE                                  
*                                                                               
HOBANN   DS    A                   BANK NAME                                    
HOHUBN   DS    A                   HUB NAME                                     
*                                                                               
HOCITY   DS    A                   CITY                                         
HOSTATE  DS    A                   STATE                                        
HOZIP    DS    A                   ZIP                                          
*                                                                               
HOLNQ    EQU   *-HOAREA                                                         
***********************************************************************         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003CTFIL2C   08/20/07'                                      
         END                                                                    
