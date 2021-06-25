*          DATA SET ACFIL53    AT LEVEL 002 AS OF 05/22/09                      
*PHASE T62353A,*                                                                
         SPACE 1                                                                
FIL53    TITLE 'KEY STAGE RECORD'                                               
         SPACE 2                                                                
FIL53    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL53**,RA,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         LH    R6,=Y(TWUSER-TWAD)                                               
         A     R6,ATWA                                                          
         USING SAVED,R6                                                         
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
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
         LR    RF,R2                                                            
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
EXITWLVL MVC   FVMSGNO,=AL2(AE$INLVL)                                           
         B     EXITL               EXIT WITH FIELD INVALID LEVEL SET            
EXITKST  MVC   FVMSGNO,=AL2(AE$TKYST)                                           
         B     EXITL               NO MORE THAN 15 KEYSTAGES ALLOWED            
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT DEFINITELY NOT VALID                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     OI    GSINDSL1,GSIXKEY    DON'T ASK 'ENTER KEY'                        
*                                                                               
         USING CPYRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   CPYKEY,BCSPACES                                                  
         MVC   CPYKCPY,CUABIN                                                   
         LHI   R1,XOREAD+XOACCMST+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                NO COMPANY RECORD?                           
         L     R2,AIO1             LOCATE COMPANY ELEMENT                       
         LA    RF,CPYRFST                                                       
         USING CPYEL,RF                                                         
         XR    R0,R0                                                            
INIT02   CLI   CPYEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BE    *+14                                                             
         IC    R0,CPYLN                                                         
         AR    RF,R0                                                            
         B     INIT02                                                           
         MVC   SCPYALP,CPYALPHA    AGENCY ALPHA CODE                            
*                                                                               
         MVC   SAVOFF,BCSPACES                                                  
         TM    BCCPYSTC,CPYSROFF   COMPANY ENFORCE OFFICES                      
         BNO   EXITOK                                                           
         CLI   CUACCS,0            LIMIT ACCESS LOGON?                          
         BE    INIT10              NO                                           
         TM    BCCPYST4,CPYSOFF2   ARE WE A 2 CHARACTER OFFICE SET UP           
         BO    INIT04              YES                                          
         CLI   CUACCS,C'*'         NO - IS THIS SINGLE OR LIST ACCESS           
         BNE   INIT10              LIST - DON'T SET OFFICE                      
         MVC   SAVOFF,CUACCS+1     SET CONNECTED OFFICE                         
         B     INIT10                                                           
*                                                                               
INIT04   MVC   SAVOFF,CUACCS+2     SET CONNECTED OFFICE                         
*                                                                               
INIT10   MVI   GSSKCODE,C'A'                                                    
         MVI   GSSLCODE,C'A'                                                    
         MVI   GSSMCODE,C'A'                                                    
         B     EXITOK                                                           
         DROP  RF,R2                                                            
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
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE TABLE                                
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO                                                       
         B     ITERH                                                            
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
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
         SPACE 1                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING KSTRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
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
KFKVAL   XC    KSTKEY,KSTKEY       INITIALIZE KEY OF RECORD                     
         MVI   KSTKTYP,KSTKTYPQ                                                 
         MVI   KSTKSUB,KSTKSUBQ                                                 
         MVC   KSTKCPY,CUABIN      CONNECTED ID                                 
         MVC   KSTKOFF,SAVOFF                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    KSTKEY,KSTKEY       INITIALIZE KEY OF RECORD                     
         MVI   KSTKTYP,KSTKTYPQ                                                 
         MVI   KSTKSUB,KSTKSUBQ                                                 
         MVC   KSTKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         EJECT ,                                                                
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
         SPACE 2                                                                
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
* RECORD OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R0,R3,SVPARMS                                                    
         USING KSTRECD,R2                                                       
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
RFADD    B     EXITOK                                                           
         EJECT ,                                                                
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
         L     R2,SVPARMS4                                                      
         USING KSTRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS3         GET GLOBAL VERB                              
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
         USING KSTRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
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
KNOWTAB  DC    AL2(F#KYST#OFF),AL4(KOFDTA)   OFFICE/OFFICE LIST                 
         DC    AL2(F#KYST#OFNM),AL4(OFNDTA)  NAME                               
         DC    AL2(F#KYST#NUM),AL4(KNODTA)   KEY STAGE NUMBER                   
         DC    AL2(F#KYST#NME),AL4(KNMDTA)   KEY STAGE NAME                     
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL53    CSECT                                                                  
         EJECT ,                                                                
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
DFDDIS   LA    R3,KSEL                                                          
SAVE     USING KSTELD,R3                                                        
*                                                                               
         XC    KSEL,KSEL                                                        
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('KSTELQ',KSTRECD),       X        
               (1,ONE)                                                          
         CLI   12(R1),0                                                         
         BNE   EXITOK              ELEMENT DOES NOT EXIST                       
*                                                                               
         L     R4,12(,R1)                                                       
         USING KSTELD,R4                                                        
         XR    RF,RF                                                            
         IC    RF,KSTLN                                                         
         MVC   SAVE.KSTELD(0),KSTELD                                            
         EX    RF,*-6                                                           
         B     EXITOK                                                           
         DROP  R4,SAVE                                                          
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   LA    R3,KSEL                                                          
SAVE     USING KSTELD,R3                                                        
*                                                                               
         XC    KSEL,KSEL                                                        
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('KSTELQ',KSTRECD),       X        
               (1,ONE)                                                          
         CLI   12(R1),0                                                         
         BNE   DFV02               ELEMENT DOES NOT EXIST                       
*                                                                               
         L     R4,12(,R1)          SAVE ELEMENT                                 
         USING KSTELD,R4                                                        
         XR    RF,RF                                                            
         IC    RF,KSTLN                                                         
         MVC   SAVE.KSTELD(0),KSTELD                                            
         EX    RF,*-6                                                           
*                                                                               
DFV02    GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('KSTELQ',KSTRECD),       X        
               (1,ONE)                                                          
         B     EXITOK                                                           
         DROP  R4,SAVE                                                          
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3                                                      
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   LA    R3,KSEL                                                          
SAVE     USING KSTELD,R3                                                        
*                                                                               
         CLI   SAVE.KSTLN,0        NO INPUT                                     
         BE    EXITOK                                                           
         MVI   SAVE.KSTEL,KSTELQ                                                
         MVI   SAVE.KSTTYPE,KSTTNAME                                            
         MVC   SAVE.KSTCODE,KSTKNUM                                             
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),KSTRECD,SAVE.KSTELD,0              
         CLI   12(R1),0                                                         
         BE    EXITOK              ERROR ON PUT IN HELLO                        
         MVC   FVMSGNO,=AL2(AE$RECTB)                                           
         B     EXITL                                                            
         DROP  SAVE                                                             
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR USER-ID                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
KOFDTA   LA    RF,KOFTBL                                                        
         B     ITER                                                             
*                                                                               
KOFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTOFF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTOFF)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISTOFF)                                
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETFOFF)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFOFF)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALTOFF)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFF)                                
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTOFF)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHOFF)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFOFF DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE/OFFICE LIST                                          *         
***********************************************************************         
         SPACE 1                                                                
DISTOFF  MVC   FVIFLD(L'KSTKOFF),KSTKOFF   OFFICE                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE/OFFICE LIST                                         *         
***********************************************************************         
         SPACE 1                                                                
VALTOFF  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         MVC   KSTKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'KSTKOFF),KSTKOFF                                       
                                                                                
         CLI   CSACT,A#DIS         DON'T TEST OFFICE CODE                       
         BE    EXITOK              IF DISPLAY                                   
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE CODE                             
         BNE   EXITL               INVALID OFFICE                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER ON OFFICE FIELD                                   *          
***********************************************************************         
         SPACE 1                                                                
DFLTFOFF MVC   FVIFLD(L'KSTKOFF),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR OFFICE                                              *         
***********************************************************************         
         SPACE 1                                                                
DDFTOFF  CLI   CUACCS,0                                                         
         BE    EXITNO                                                           
         TM    BCCPYST4,CPYSOFF2                                                
         BNO   DDFTOFF2                                                         
         MVC   FVIFLD(L'KSTKOFF),CUACCS+2                                       
         B     EXITOK                                                           
*                                                                               
DDFTOFF2 CLI   CUACCS,C'*'                                                      
         BNE   EXITOK                                                           
         MVC   FVIFLD(1),CUACCS+1                                               
         MVI   FVIFLD+1,X'40'                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON OFFICE                                   *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLC   KSTKOFF,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON OFFICE CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHOFF  GOTO1 VACSRCHC,BOPARM,(3,FVADDR),ATWA,OFFUL,ACOM,(X'11',0)             
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFFICE CODE NAME FIELD                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFNDTA   LA    RF,OFFNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFFN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISOFFN  OC    KSTKOFF,KSTKOFF     TEST ALL OFFICE                              
         BZ    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         TM    BCCPYST4,CPYSOFF2                                                
         BO    DISOFN02                                                         
         MVC   IOKEY,BCSPACES                 READ THE ACCOUNT RECORD           
         MVC   T.ACTKCPY,KSTKCPY              COMPANY                           
         MVC   T.ACTKUNT(L'OFFUL),OFFUL       UNIT/LEDGER                       
         MVC   T.ACTKACT(L'KSTKOFF),KSTKOFF   OFFICE CODE CODE                  
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET OFFICE CODE NAME                         
         B     EXITOK                                                           
*                                                                               
T        USING OFFRECD,IOKEY                                                    
DISOFN02 MVC   IOKEY,BCSPACES                 READ THE ACCOUNT RECORD           
         MVC   T.OFFKCPY,KSTKCPY              COMPANY                           
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKOFF,KSTKOFF           OFFICE CODE CODE                     
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET OFFICE CODE NAME                         
         B     EXITOK                                                           
         EJECT ,                                                                
*                                                                               
***********************************************************************         
* DATA OBJECT FOR KEY STAGE NUMBER                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
KNODTA   LA    RF,KNOTBL                                                        
         B     ITER                                                             
*                                                                               
KNOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISKNO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALKNO)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISKNO)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETKNO)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETKNO  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY STAGE NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
DISKNO   OC    KSTKNUM,KSTKNUM                                                  
         BZ    EXITOK                                                           
         CURED KSTKNUM,(5,FVIFLD),0,ALIGN=LEFT,DMCB=BODMCB,ZERO=BLANK           
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* VALIDATE A KEY STAGE NUMBER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
VALKNO   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         GOTO1 ACHKFLD,BOPARM,('CKKEYQ',CKTAB1Q)                                
         BNE   EXITNV              INVALID CHARS IN THE FIELD                   
         TM    FVIIND,FVINUM       CHECK FVIFLD IS NUMERIC                      
         BNO   EXITNV                                                           
*                                                                               
         ZIC   RE,FVILEN                                                        
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,FVIFLD(0)                                                    
         CVB   RF,DUB                                                           
         STCM  RF,B'0011',KSTKNUM                                               
         GOTO1 AVALKSTG                                                         
         BNE   EXITKST             NO MORE THAN 15 KEYSTAGES ALLOWED            
*                                                                               
         CLI   CSACT,A#ADD         SKIP CHECKING, IF ADDING OR COPYING          
         BE    EXITOK                                                           
         CLI   CSACT,A#CPY                                                      
         BE    EXITOK                                                           
         LA    R4,IOKEY                                                         
X        USING KSTRECD,R4                                                       
         XC    X.KSTKEY,X.KSTKEY   READ ROLE RECORD                             
         MVI   X.KSTKTYP,KSTKTYPQ                                               
         MVI   X.KSTKSUB,KSTKSUBQ                                               
         MVC   X.KSTKCPY,CUABIN                                                 
         MVC   X.KSTKOFF,KSTKOFF                                                
         MVC   X.KSTKNUM,KSTKNUM                                                
         MVC   SVIOKEY,X.KSTKEY                                                 
         L     R1,=AL4(XORDD+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         BE    VKSTC02                                                          
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITNV                                                           
VKSTC02  MVC   FLTIFLD,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  X                                                                
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR KEY STAGE NAME FIELD                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING KSTELD,KSEL                                                      
KNMDTA   LA    RF,KNMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
KNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISKNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALKNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY STAGE NAME                                              *         
***********************************************************************         
         SPACE 1                                                                
DISKNM   XR    RE,RE                                                            
         IC    RE,KSTLN                                                         
         SHI   RE,KSTLN1Q+1                                                     
         EXMVC RE,FVIFLD,KSTNAME                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE KEY STAGE NAME AND SAVE                                    *         
***********************************************************************         
         SPACE 1                                                                
VALKNM   MVI   KSTLN,0                                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EXMVC RF,KSTNAME,FVIFLD                                                
         LA    RF,KSTLN1Q+1(,RF)    MODIFY THE ELEMENT LENGTH                   
         STC   RF,KSTLN                                                         
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
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
         BNE   EXITOK                                                           
         CLI   SREC,R#TEMP         TEMPLATE RECORD                              
         BE    *+8                                                              
         CLI   SREC,R#KYST         KEY STAGE RECORD                             
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         DROP  R2                                                               
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
         USING KSTRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING KSTRECD,R2                                                       
LAST     USING KSTRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
         SPACE 2                                                                
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(INITL)                                 
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
INITL    NI    LSSTAT1,FF-LSSENTK    IS TSAR RECORDS ONLY                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING KSTRECD,IOKEY                                                    
FLST     TM    LSSCIND1,LSSCIFLT   TEST FILTERS CHANGED                         
         BO    FLST02              YES, IGNORE ERRIND                           
         TM    ERRIND,ERMAXIO                                                   
         BZ    FLST02                                                           
         MVC   THIS.KSTKEY,SKEYLAST                                             
FLST02   MVI   ERRIND,0            RESET ERROR INDICATOR                        
         XC    SKEYLAST,SKEYLAST   OR CLEAR SAVED LAST KEY                      
*                                                                               
         MVC   X.KSTKEY,THIS.KSTKEY                                             
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     L     R1,=AL4(XOSQD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
*                                                                               
NLST02   GOTO1 AIOCHK              CHECK FOR MAX IOS                            
         BE    NLST02A                                                          
         OI    ERRIND,ERMAXIO                                                   
         MVC   SKEYLAST,IOKEY      WHEN RESUMED, START HERE                     
         B     EXITL                                                            
*                                                                               
NLST02A  CLC   X.KSTKEY(KSTKREM-KSTRECD),THIS.KSTRECD                           
         BNE   EXITL               CHANGE OF TYPE/SUBTYPE/CPMPANY               
         CLI   CRECDEL,0           NO FILTER - DEFAULT IS DELETE=NO             
         BE    NLST04                                                           
         CLI   CRECDEL,YES         DELETE=YES                                   
         BE    NLST06                                                           
         CLI   CRECDEL,ONLY        DELETE=ONLY                                  
         BNE   NLST04                                                           
         TM    IOERR,IOEDEL        TEST IF RECORD IS DELETED                    
         BZ    NLST                NO - GET NEXT                                
         B     NLST06                                                           
*                                                                               
NLST04   TM    IOERR,IOEDEL        IT MUST BE DELETE=NO                         
         BO    NLST                                                             
*                                                                               
NLST06   TM    BCCPYSTC,CPYSROFF   COMPANY ENFORCE OFFICES                      
         BNO   NLST08                                                           
         CLI   CUACCS,0                                                         
         BE    NLST08                                                           
         GOTO1 ATSTOFF,X.KSTKOFF                                                
         BE    NLST08                                                           
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         B     NLST                                                             
NLST08   MVC   THIS.KSTKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
*                                                                               
         B     EXITOK                                                           
         DROP  THIS,LAST,X                                                      
         SPACE 2                                                                
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
         B     VALKSTG                                                          
         B     IOCHK                                                            
*                                                                               
OVROU1L  MVI   DUB,0                                                            
         B     *+8                                                              
OVROU1E  MVI   DUB,1                                                            
         B     *+8                                                              
OVROU1H  MVI   DUB,2                                                            
         CLI   DUB,1                                                            
OVROU1X  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY STAGE NUMBERING                                        *         
***********************************************************************         
         SPACE 2                                                                
VALKSTG  CLI   CSACT,A#ADD                                                      
         BE    VKST02                                                           
         CLI   CSACT,A#CPY                                                      
         BNE   OVROU1E                                                          
                                                                                
VKST02   ZAP   COUNT,=P'0'                                                      
K        USING KSTRECD,IOKEY                                                    
         XC    K.KSTKEY,K.KSTKEY                                                
         MVI   K.KSTKTYP,KSTKTYPQ                                               
         MVI   K.KSTKSUB,KSTKSUBQ                                               
         MVC   K.KSTKCPY,CUABIN                                                 
         MVC   K.KSTKOFF,SAVOFF                                                 
         MVC   SVIOKEY(KSTKNUM-KSTKEY),IOKEY                                    
         L     R1,=AL4(XOHID+XOACCDIR+XIO4)                                     
         B     *+8                                                              
VKST04   L     R1,=AL4(XOSQD+XOACCDIR+XIO4)                                     
         GOTO1 AIO                                                              
         CLC   SVIOKEY(KSTKNUM-KSTKEY),IOKEY                                    
         BNE   VKST06                                                           
         MVC   SVIOKEY(KSTKNUM-KSTKEY),IOKEY                                    
         TM    K.KSTKSTAT,KSTSDELT    MARKED AS DELETED?                        
         BO    VKST04                                                           
         CP    COUNT,=P'15'   NOT ALLOWED TO HAVE MORE THAN 15                  
         BE    OVROU1L                                                          
         AP    COUNT,=P'1'                                                      
         B     VKST04                                                           
VKST06   CP    COUNT,=P'15'   FINAL CHECK                                       
         BNL   OVROU1L                                                          
         B     OVROU1E                                                          
         DROP  K                                                                
         SPACE 1                                                                
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
         EJECT ,                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
ONE      DC    X'01'                                                            
OFFUL    DC    C'2D'                                                            
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
ONLY     EQU   C'O'                                                             
         SPACE 2                                                                
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
         SPACE 1                                                                
OVROUT1  DS    0A                                                               
AVALKSTG DS    A                                                                
AIOCHK   DS    A                                                                
OVROUT1N EQU   (*-OVROUT1)/L'OVROUT1                                            
         SPACE 1                                                                
*                                                                               
SCPYALP  DS    CL(L'CPYALPHA)      AGENCY ALPHA CODE                            
*                                                                               
SVIOKEY  DS    XL42                                                             
KSEL     DS    CL(KSTLN1Q+40)                                                   
COUNT    DS    PL2                                                              
*                                                                               
         DS    0D                                                               
DUB      DS    D                                                                
MYWORK   DS    XL255                                                            
WORK     DS    XL64                                                             
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT                                                                  
***********************************************************************         
* DSECT                                                               *         
***********************************************************************         
         SPACE 2                                                                
SAVED    DSECT                                                                  
SAVOFF   DS    XL(L'KSTKOFF)       2 BYTE BINARY USER-ID                        
IOCOUNT  DS    H                   COUNT IO'S                                   
SKEYLAST DS    XL(L'ACTKEY)        SAVED CONTINUATION KEY FOR FLST              
ERRIND   DS    XL1                 ERROR INDICATOR                              
ERMAXIO  EQU   X'80'               MAX IOS RETURNED FROM MEFILT                 
*                                                                               
         EJECT ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACFIL53   05/22/09'                                      
         END                                                                    
