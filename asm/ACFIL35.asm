*          DATA SET ACFIL35    AT LEVEL 019 AS OF 05/08/18                      
*&&      SET   NOP=N                                                            
*PHASE T62335C,*                                                                
****************************************************************                
*      MAINTAIN 1099 TAX INFORMATION RECORDS                   *                
*                                                              *                
****************************************************************                
         TITLE '1099 TAX INFORMATION MANAGER'                                   
*                                                                               
*VSAX   019  SPEC-21857 CANNOT ADD 1099 BEFORE MARCH 1 FOR CURRENT YEAR         
*                                                                               
         SPACE 2                                                                
FIL35    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL35**,R7,RR=RE                                              
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
         LH    R4,=Y(TWSAVE-TWAD)                                               
         AR    R4,RA                                                            
         USING TWSAVE,R4                                                        
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
EXITMIS  MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITTCC  MVC   FVMSGNO,=AL2(AE$MTCCD)                                           
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITDTE  MVC   FVMSGNO,=AL2(AE$1099E)                                           
         B     EXITL                                                            
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
*                                                                               
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
*                                                                               
INIT     DS    0H                                                               
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLIST                             
         B     EXITOK                                                           
         EJECT                                                                  
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
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
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
         USING T99RECD,R2          1099 TAX INFO RECORD                         
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
*        DC    AL1(KMASK),AL1(0,0,0),AL4(KEYMASK)                               
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
KFKVAL   DS    0H                                                               
         MVC   T99KEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   T99KTYP,T99KTYPQ  X'3E'                                          
         MVI   T99KSUB,T99KSUBQ  X'15'                                          
         MVC   T99KCPY,CUABIN    CONNECTED ID AGENCY BINARY                     
         XC    T99KOID,T99KOID     INITIALIZE ORIGIN ID                         
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
*                                                                               
KFKFVAL  DS    0H                                                               
         MVC   T99KEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   T99KTYP,T99KTYPQ  X'3E'                                          
         MVI   T99KSUB,T99KSUBQ  X'15'                                          
         MVC   T99KCPY,CUABIN    CONNECTED ID AGENCY BINARY                     
         XC    T99KOID,T99KOID     INITIALIZE ORIGIN ID                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCREEN   LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRTABL                                                       
         B     ITER                                                             
*                                                                               
SCRTABL  DC    AL1(SSET),AL1(0,0,0),AL4(SCRMSET)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET DATA SCREEN CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
SCRMSET  MVI   GSSMCODE,C'A'                                                    
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
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DS    0H                                                               
         DC    AL1(RMASK),AL1(0,0,0),AL4(RFMASK)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TURN OFF DISPLAY, CHANGE  ACTION                                              
***********************************************************************         
RFMASK   DS    0H                                                               
         TM    FLAG,FLGPRVYR       ARE WE DOING PREV YEAR                       
         BNO   EXITOK              NO WE ARE DOING CURRENT                      
         LA    R1,GSRECMSK                                                      
         TM    CUSTAT,CUSDDS IF THIS IS A DDS TERMINAL                          
         BNO   RFMASK10      ALLOW CHANGE OF PREVIOUS YEARS RECORDS.            
         NC    0(4,R1),=AL4(X'FFFFFFFF'-MK#ADD-MK#DEL-MK#CPY-MK#REN)            
         B     RFMASK20                                                         
RFMASK10 NC    0(4,R1),=AL4(X'FFFFFFFF'-MK#ADD-MK#CHA-MK#DEL-MK#CPY-MK#+        
               REN)                                                             
RFMASK20 NI    FLAG,X'FF'-FLGPRVYR                                              
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
         USING T99RECD,R2                                                       
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
         USING T99RECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(AT#LOGID),AL4(LOGID)    LOGIN ID                             
         DC    AL2(AT#YEAR),AL4(YEAR)      YEAR                                 
         DC    AL2(AT#CMPYN),AL4(CPYNAME)  COMPANY NAME                         
         DC    AL2(AT#IDNAM),AL4(IDINAM)   NAME FROM IDI MESSAGE                
         DC    AL2(AT#ADR1),AL4(ADDRESS1)  ADDRESS LINE 1                       
         DC    AL2(AT#IDAD1),AL4(IDIAD1)   IDI ADDRESS LINE 1                   
         DC    AL2(AT#ADR2),AL4(ADDRESS2)  ADDRESS LINE 2                       
         DC    AL2(AT#IDAD2),AL4(IDIAD2)   IDI ADDRESS LINE 2                   
         DC    AL2(AT#CITY),AL4(CITY)      CITY                                 
         DC    AL2(AT#IDAD3),AL4(IDIAD3)   IDI ADDRESS LINE 3                   
         DC    AL2(AT#STATE),AL4(STATE)    STATE                                
         DC    AL2(AT#ZIP),AL4(ZIP)        ZIP                                  
         DC    AL2(AT#ZIPRN),AL4(ZIPRN)    ZIP ROUTING NUMBER 4 DGTS            
         DC    AL2(AT#CNAME),AL4(CNAME)    CONTACT PERSON NAME                  
         DC    AL2(AT#TELN1),AL4(CPHONE)   FIRST THREE DGTS OF PHONE NO         
         DC    AL2(AT#TELN2),AL4(CPHONE)   SECOND THREE DGT OF PHONE NO         
         DC    AL2(AT#TELN3),AL4(CPHONE)   LAST FOUR DGTS OF PHONE NO.          
         DC    AL2(AT#EXT),AL4(PHNEX)      5 DGTS PHONE EXTENSION               
         DC    AL2(AT#CEMAIL),AL4(CEMAIL)  CONTACT PERSON'S EMAIL               
         DC    AL2(AT#NFORM),AL4(FORMS)    NO. OF FORMS                         
         DC    AL2(AT#LNFORM),AL4(LFORMS)  NO. OF FORMS FROM LAST YEAR          
         DC    AL2(AT#TCODE),AL4(TCODE)    TRANSMISSION CONTROL CODE            
         DC    AL2(AT#DWNLD),AL4(DWNLD)    DOWNLOAD                             
         DC    AL2(AT#TINEIN),AL4(TINEIN)  TIN/EIN NUMBER                       
         DC    AL2(AT#CLA),AL4(CLA)        LIMITED ACCESS OFFICE IDI            
         DC    AL2(AT#LRDT),AL4(LRDT)      DATE OF LIVE RUN                     
         DC    AL2(AT#ULA),AL4(ULA)        LIMITED ACCESS OFFICE USED           
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL35    CSECT                                                                  
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
DFDVAL   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',T99RECD),0               
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         GOTO1 AADDRST,T99RECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LOGIN ID                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LOGID    LA    RF,LOGIDTBL                                                      
         B     ITER                                                             
*                                                                               
LOGIDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISLOGID)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLOGID)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY LOGIN ID                                                              
***********************************************************************         
*                                                                               
DISLOGID DS    0H                                                               
         LA    RF,IOKEY                                                         
         USING CTIREC,RF                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKNUM,T99KOID     LOGIN ORIGIN ID TO CONTROL KEY               
         LHI   R1,XOREAD+XOCONFIL+XIO2                                          
         GOTO1 AIO                                                              
         BNE   DISLIDX                                                          
*        BE    *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         L     RF,AIO2                                                          
         LA    R1,CTIDATA                                                       
DISLID10 CLI   0(R1),0              THE END                                     
         BE    DISLIDX                                                          
*        BNE   *+6                                                              
*        DC    H'0'                                                             
*                                                                               
         CLI   0(R1),CTDSCELQ      IS IT DESCRIPTION ELEMENT                    
         BNE   DISLID20                                                         
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R1)            GET LENGTH OF ELEMENT                        
         SHI   RE,3                SUBTRACT OVERHEAD+1                          
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),2(R1)     MOVE LOGIN ID TO FIELD                       
*                                                                               
DISLID20 DS    0H                                                               
         CLI   0(R1),CTSYSELQ      X'21' SYSTEM AUTHORIZATION ELEMENT           
         BNE   DISLID30                                                         
*                                                                               
         USING CTSYSD,R1                                                        
         CLI   CTSYSNUM,X'06'      IS IT ACCOUNTING SYSTEM                      
         BNE   DISLID30                                                         
         MVC   SVLIM,CTSYSLMT      SAVE OFF LIMITED ACCESS IF ANY               
         DROP  R1                                                               
*                                                                               
DISLID30 SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DISLID10                                                         
*                                                                               
DISLIDX  B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
***********************************************************************         
* VALIDATE LOGIN ID                                                             
***********************************************************************         
*                                                                               
VALLOGID DS    0H                                                               
         LA    RF,IOKEY                                                         
         USING CTIREC,RF                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKID,FVIFLD       LOGIN ID TO CONTROL KEY                      
         LHI   R1,XOREAD+XOCONFIL+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
*                                                                               
         L     RF,AIO2                                                          
         LA    R1,CTIDATA                                                       
VALID10  CLI   0(R1),0              THE END                                     
         BE    VALID60                                                          
*                                                                               
         CLI   0(R1),CTAGYELQ      X'06' AGENCY ALPHA ELEMENT                   
         BNE   VALID20                                                          
         USING CTAGYD,R1                                                        
         CLC   CTAGYID,CUAALF      MAKE SURE IT IS THE SAME AGY                 
         BNE   EXITNV                                                           
         DROP  R1                                                               
*                                                                               
VALID20  CLI   0(R1),CTDSCELQ      IS IT DESCRIPTION ELEMENT                    
         BNE   VALID30                                                          
         MVC   SVOID,2(R1)         MOVE IN ORIGIN ID TO BUILD KEY               
*                                                                               
VALID30  CLI   0(R1),CTDSTELQ      IS IT COMPANY NAME AND ADDRESS ELEM          
         BNE   VALID40                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,1(R1)            GET LENGTH TO MOVE                           
         SHI   RE,1                                                             
         EX    RE,*+4                                                           
         MVC   SVDESTEL(0),0(R1)   SAVE OFF DESTINATION DETAIL ELEMENT          
*                                                                               
VALID40  CLI   0(R1),CTSYSELQ      X'21' SYSTEM AUTHORIZATION ELEMENT           
         BNE   VALID50                                                          
*                                                                               
         USING CTSYSD,R1                                                        
         CLI   CTSYSNUM,X'06'      IS IT ACCOUNTING SYSTEM                      
         BNE   VALID50                                                          
         MVC   SVLIM,CTSYSLMT      SAVE OFF LIMITED ACCESS IF ANY               
         DROP  R1                                                               
*                                                                               
VALID50  SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     VALID10                                                          
*                                                                               
VALID60  DS    0H                                                               
         MVC   SVLOGID,CTIKID      SAVE OFF LOGIN ID                            
         MVC   T99KOID,SVOID       BUILD T99 KEY                                
*                                                                               
VALIDX   B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING/VALIDATING YEAR                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
YEAR     LA    RF,YEARTBL                                                       
         B     ITER                                                             
*                                                                               
YEARTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISYEAR)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALYEAR)                                
*        DC    AL1(DDFLTF),AL1(0,0,0),AL4(DDFLYR)   DEFAULT VALUE               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTYR)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTYR)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTYR)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY YEAR                                                        *         
***********************************************************************         
*                                                                               
DISYEAR  DS    0H                                                               
         MVC   FVIFLD(L'T99KYEAR),T99KYEAR YEAR FROM KEY                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE YEAR                                                                 
***********************************************************************         
*                                                                               
VALYEAR  DS    0H                                                               
         MVC   T99KYEAR,FVIFLD     MOVE YEAR TO KEY                             
         XC    BOWORK1,BOWORK1                                                  
         GOTO1 VDATCON,BOPARM,(5,0),(20,SVTODAY)                                
         CLC   SVMON,=C'02'                                                     
         BH    VALYR10                                                          
*                                                                               
* SUBTRACT 1 FROM YEAR IF 2000 MAKE IT 1999 ETC                                 
*                                                                               
         GOTO1 VDATCON,BOPARM,(9,SVTODAY),(0,BOWORK1) WORK=YYMMDD               
         GOTO1 VADDAY,BOPARM,(C'Y',BOWORK1),BOWORK1+8,-1                        
         GOTO1 VDATCON,BOPARM,(0,BOWORK1+8),(20,BOWORK1) WORK=YYYYMMDD          
         CLC   FVIFLD(L'SVYEAR),BOWORK1                                         
         BL    VALYR20                                                          
         BH    EXITDTE                                                          
         B     EXITOK                                                           
*                                                                               
VALYR10  CLC   FVIFLD(L'SVYEAR),SVYEAR                                          
         BE    VALYRX                                                           
         BH    EXITDTE                                                          
*                                                                               
VALYR20  OI    FLAG,FLGPRVYR                                                    
VALYRX   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEFAULT YEAR IN LIST                                        *         
***********************************************************************         
*                                                                               
DDFLYR   DS    0H                                                               
         GOTO1 VDATCON,BOPARM,(5,0),(20,SVTODAY)                                
         CLC   SVMON,=C'02'                                                     
         BH    DDFLYR10                                                         
*                                                                               
* SUBTRACT 1 FROM YEAR IF 2000 MAKE IT 1999 ETC                                 
*                                                                               
         GOTO1 VDATCON,BOPARM,(9,SVTODAY),(0,BOWORK1) WORK=YYMMDD               
         GOTO1 VADDAY,BOPARM,(C'Y',BOWORK1),BOWORK1+8,-1                        
         GOTO1 VDATCON,BOPARM,(0,BOWORK1+8),(20,SVTODAY) SVTO=YYYYMMDD          
DDFLYR10 MVC   FVIFLD(L'SVYEAR),SVYEAR     DEFAULT YEAR                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY YEAR FILTER FIELD                                           *         
***********************************************************************         
*                                                                               
DFLTYR   MVC   FVIFLD(L'T99KYEAR),FLTIFLD                                       
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE YEAR FILTER FIELD                                          *         
***********************************************************************         
*                                                                               
VFLTYR   DS    0H                                                               
         ZIC   RF,FVXLEN                                                        
         EXMVC RF,FLTIFLD,FVIFLD                                                
         B     VALYEAR                                                          
*                                                                               
***********************************************************************         
* DO FILTERING ON YEAR                                                *         
***********************************************************************         
DOFTYR   CLC   T99KYEAR,BCSPACES  IS THERE A CODE TO COMPARE ON?                
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   T99KYEAR,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPANY NAME                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CPYNAME  LA    RF,CPYNTBL                                                       
         B     ITER                                                             
*                                                                               
CPYNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAME)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNAME)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY COMPANY NAME                                                *         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING NAMELD,BOELEM      DSECT TO COVER NAME ELEM                      
DISNAME  DS    0H                                                               
*                                                                               
         GOTO1 AGETEL,BOPARM,('NAMELQ',T99RECD),0                               
         BNE   DISNM10                                                          
         LA    R1,BOELEM                                                        
         SR    RE,RE                                                            
         IC    RE,1(R1)            GET LENGTH                                   
         SHI   RE,NAMLN1Q          SUBTRACT OVERHEAD                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),NAMEREC                                                
         B     DISNMX                                                           
         POP   USING                                                            
*                                                                               
DISNM10  OI    FLAG,FLGIDNAM       DISPLAY IDI MESSAGE                          
DISNMX   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COMPANY NAME                                                         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING NAMELD,BOELEM                                                    
VALNAME  DS    0H                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',T99RECD),0               
         CLI   12(R1),0                                                         
         BNE   VALNM10                                                          
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('NAMELQ',T99RECD),0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VALNM10  DS    0H                                                               
         CLI   FVILEN,0                                                         
         BNE   VALNM20                                                          
         OI    FLAG,FLGIDNAM       DISPLAY FROM IDI                             
         B     VALNMX                                                           
*                                                                               
VALNM20  XC    BOELEM,BOELEM       CLEAR ELEMENT BUILD AREA                     
         MVI   NAMEL,NAMELQ        BUILD X'20' ELEMENT IN BOELEM                
*                                                                               
         NI    FLAG,X'FF'-FLGIDNAM SOMETHING IS IN NAME FLD NO IDI NAM          
         ZIC   R1,FVILEN                                                        
         LR    RE,R1                                                            
         AHI   RE,NAMLN1Q          ADD OVERHEAD                                 
         STC   RE,NAMLN            MOVE IN LENGTH                               
         BCTR  R1,0                                                             
*                                                                               
         EX    R1,*+4                                                           
         MVC   NAMEREC(0),FVIFLD   MOVE NAME TO ELEMENT                         
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),T99RECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    VALNMX                                                           
         DC    H'0'                                                             
VALNMX   B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING IDI NAME AS DEFAULT                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
IDINAM   LA    RF,IDNAMTBL                                                      
         B     ITER                                                             
*                                                                               
IDNAMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISIDNAM)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY IDI NAME                                                    *         
***********************************************************************         
*                                                                               
DISIDNAM DS    0H                                                               
*        TM    FLAG,FLGIDNAM       DO WE NEED TO DISPLAY IDI MSG                
*        BNO   DISIDNX             NAME COMES FROM IDI RECORD                   
         USING CTDSTD,R1                                                        
         LA    R1,SVDESTEL         X'30' CTDSTELQ ELEMENT                       
         MVC   FVIFLD(L'CTDSTNAM),CTDSTNAM                                      
DISIDNX  B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPANY ADDRESS LINE 1 AND 2                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
ADDRESS1 LA    RF,ADDRTBL1                                                      
         B     ITER                                                             
*                                                                               
ADDRTBL1 DC    AL1(DDIS),AL1(0,0,0),AL4(DISADDR1)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADDR1)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY ADDRESS LINE 1                                              *         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM      DSECT TO COVER ADRR ELEM                      
         USING OADRELD,BOELEM                                                   
DISADDR1 DS    0H                                                               
*                                                                               
         XC    BOELEM1,BOELEM1                                                  
         GOTO1 AGETEL,BOPARM,('ADRELQ',T99RECD),0                               
         BNE   DISAD110                                                         
*                                                                               
         MVC   BOELEM1,BOELEM               SAVE OFF THE ADDRESS ELEM           
         CLI   ADRLN,ADRLNQ                                                     
         BNE   *+14                                                             
         MVC   FVIFLD(L'ADRLINE1),ADRLINE1  DIS LINE 1                          
         B     *+10                                                             
         MVC   FVIFLD(L'OADRLN1),OADRLN1   DIS OLD 33 BYTE ADDRESS 1            
         B     DISAD1X                                                          
         POP   USING                                                            
*                                                                               
DISAD110 OI    FLAG,FLGIDAD1                                                    
DISAD1X  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ADDRESS LINE 1                                                       
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM                                                    
VALADDR1 DS    0H                                                               
         XC    BOELEM,BOELEM       CLEAR ELEMENT BUILD AREA                     
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('ADRELQ',T99RECD),0               
         CLI   12(R1),0                                                         
         BNE   VALAD110                                                         
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('ADRELQ',T99RECD),0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
VALAD110 DS    0H                                                               
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BNE   VALAD120                                                         
         OI    FLAG,FLGIDAD1       DISPLAY FROM IDI                             
         B     VALAD1X             YES ADD NEW ELEMENT                          
*                                                                               
VALAD120 DS    0H                                                               
         NI    FLAG,X'FF'-FLGIDAD1 SOMETHING IN LINE 1                          
*                                                                               
         MVI   ADREL,ADRELQ        BUILD X'20' ELEMENT IN BOELEM                
         MVI   ADRLN,ADRLNQ                                                     
         OI    ADRSTAT,ADRCSZ      ADDRESS 3RD LN IS CTY,STATE,ZIP              
         MVC   ADRLINE1,FVIFLD     MOVE ADDRESS LINE 1                          
*                                                                               
VALAD1X  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING ADDRESS LINE 1 AS DEFAULT IDI            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
IDIAD1   LA    RF,IDAD1TBL                                                      
         B     ITER                                                             
*                                                                               
IDAD1TBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISIDAD1)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY IDI ADDRESS LINE 1                                          *         
***********************************************************************         
*                                                                               
         USING CTDSTD,R1                                                        
DISIDAD1 DS    0H                                                               
*        TM    FLAG,FLGIDAD1       DO WE NEED TO DISPLAY IDI ADDRESS            
*        BNO   DISIDA1X            NAME COMES FROM IDI RECORD                   
*                                                                               
         LA    R1,SVDESTEL                  X'30' CTDSTELQ ELEMENT              
         MVC   FVIFLD(L'CTDSTADD),CTDSTADD  ADD LINE 1                          
DISIDA1X B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPANY ADDRESS LINE 2                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
ADDRESS2 LA    RF,ADDRTBL2                                                      
         B     ITER                                                             
*                                                                               
ADDRTBL2 DC    AL1(DDIS),AL1(0,0,0),AL4(DISADDR2)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADDR2)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY ADDRESS LINE 2                                              *         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM1     DSECT TO COVER ADRR ELEM                      
         USING OADRELD,BOELEM1    DSECT TO COVER OLD ADRR ELEM                  
DISADDR2 DS    0H                                                               
         OC    BOELEM1,BOELEM1     WAS ADDRESS IN 3E15 X'22' RECORD             
         BZ    DISAD2X             NO COMING FROM IDI                           
         CLI   ADRLN,ADRLNQ                                                     
         BNE   *+14                                                             
         MVC   FVIFLD(L'ADRLINE2),ADRLINE2                                      
         B     *+10                                                             
         MVC   FVIFLD(L'OADRLN2),OADRLN2   OLD 33 BYTE ADDRESS LINE 2           
*                                                                               
DISAD2X  DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ADDRESS LINE 2                                                       
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM                                                    
VALADDR2 DS    0H                                                               
         TM    FLAG,FLGIDAD1       IS THERE ANYTHING IN ADD LINE1               
         BNO   VALAD210            YES,CHECK IF ANYTHING IN LINE 2              
         CLI   FVILEN,0                                                         
         BE    VALAD2X                                                          
*                                                                               
VALAD210 CLI   FVILEN,0                                                         
         BNE   VALAD220                                                         
         MVC   ADRLINE2,BCSPACES                                                
         B     VALAD2X                                                          
*                                                                               
VALAD220 TM    FLAG,FLGIDAD1       IS THERE ANYTHING IN ADD LINE1               
         BO    EXITNV              NO, VALIDATE THAT FIRST                      
         MVC   ADRLINE2,FVIFLD                                                  
VALAD2X  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING IDI ADDRESS LINE 2                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
IDIAD2   LA    RF,IDAD2TBL                                                      
         B     ITER                                                             
*                                                                               
IDAD2TBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISIDAD2)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY IDI ADDRESS LINE 2                                          *         
***********************************************************************         
*                                                                               
         USING CTDSTD,R1                                                        
DISIDAD2 DS    0H                                                               
*        TM    FLAG,FLGIDAD1       DO WE NEED TO DISPLAY IDI ADDRESS            
*        BNO   DISIDA2X            NAME COMES FROM IDI RECORD                   
*                                                                               
         MVC   FVIFLD,BCSPACES              CLEAR TO SPACES                     
         LA    R1,SVDESTEL                  X'30' CTDSTELQ ELEMENT              
         CLI   CTDSTLEN,166                 DOES IT HAVE LINE 2 AND 3           
         BNE   DISIDA2X                                                         
         MVC   FVIFLD(L'CTDSTAD2),CTDSTAD2  ADD LINE 2                          
*                                                                               
*        GOTO1 VADSCAN,BOPARM,(L'CTDSTADD,CTDSTAD2),                            
*              (L'SVCITY,SVCITY),SVSTATE,(L'SVZIP,SVZIP)                        
*        CLI   BOPARM+3,0          WORKING ON LINE 2 BUT HAVE CITY              
*        BE    DISIDA2X                                                         
*        CLI   BOPARM+3,2          WORKING ON LINE 2 BUT HAVE CITY              
*        BE    DISIDA2X                                                         
*        MVC   FVIFLD(L'CTDSTAD2),CTDSTAD2  ADD LINE 2                          
*                                                                               
*        GOTO1 VADSCAN,BOPARM,(L'CTDSTADD,CTDSTAD3),                            
*              (L'SVCITY,SVCITY),SVSTATE,(L'SVZIP,SVZIP)                        
*        CLI   BOPARM+3,0          WORKING ON LINE 3 BUT HAVE CITY              
*        BE    DISIDA2X                                                         
*        CLI   BOPARM+3,2          WORKING ON LINE 3 BUT HAVE CITY              
*        BE    DISIDA2X                                                         
DISIDA2X B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMPANY ADDRESS CITY                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CITY     LA    RF,CITYTBL                                                       
         B     ITER                                                             
*                                                                               
CITYTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCITY)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCITY)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY CITY                                                        *         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM1     DSECT TO COVER ADRR ELEM                      
         USING OADRELD,BOELEM1    DSECT TO COVER OLD ADRR ELEM                  
DISCITY  DS    0H                                                               
*                                                                               
         OC    BOELEM1,BOELEM1     WAS ADDRESS IN 3E15 X'22' RECORD             
         BZ    DISCITX             NO COMING FROM IDI                           
         CLI   ADRLN,ADRLNQ                                                     
         BNE   *+14                                                             
         MVC   FVIFLD(L'ADRCITY),ADRCITY                                        
         B     *+10                                                             
         MVC   FVIFLD(L'OADRCITY),OADRCITY    DIS FROM OLD 22 BYTE CITY         
         B     DISCITX                                                          
*                                                                               
DISCITX  B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CITY                                                                 
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM                                                    
VALCITY  DS    0H                                                               
         CLI   FVILEN,0                                                         
         BNE   VALCIT10                                                         
         TM    FLAG,FLGIDAD1       DID WE PRINT DEFAULT IDI                     
         BNO   EXITMIS             NO                                           
         B     VALCITX                                                          
*                                                                               
VALCIT10 TM    FLAG,FLGIDAD1       DID WE PRINT DEFAULT IDI                     
         BO    EXITNV                                                           
         MVC   ADRCITY,FVIFLD                                                   
VALCITX  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING IDI ADDRESS LINE 3                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
IDIAD3   LA    RF,IDAD3TBL                                                      
         B     ITER                                                             
*                                                                               
IDAD3TBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISIDAD3)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY IDI ADDRESS LINE 3                                          *         
***********************************************************************         
*                                                                               
         USING CTDSTD,R1                                                        
DISIDAD3 DS    0H                                                               
*        TM    FLAG,FLGIDAD1       DO WE NEED TO DISPLAY IDI ADDRESS            
*        BNO   DISIDA3X            NAME COMES FROM IDI RECORD                   
*                                                                               
         MVC   FVIFLD,BCSPACES              CLEAR TO SPACES                     
         LA    R1,SVDESTEL                  X'30' CTDSTELQ ELEMENT              
         CLI   CTDSTLEN,166                 DOES IT HAVE LINE 2 AND 3           
         BNE   DISIDA3X                                                         
         MVC   FVIFLD(L'CTDSTAD3),CTDSTAD3  ADD LINE 3                          
*                                                                               
DISIDA3X B     EXITOK                                                           
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATE                                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
STATE    LA    RF,STATTBL                                                       
         B     ITER                                                             
*                                                                               
STATTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTAT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTAT)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY STATE                                                       *         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM1     DSECT TO COVER SVD ADRR ELEM                  
         USING OADRELD,BOELEM1    DSECT TO COVER OLD SVD ADRR ELEM              
DISSTAT  DS    0H                                                               
         OC    BOELEM1,BOELEM1                                                  
         BZ    DISTATX                                                          
         CLI   ADRLN,ADRLNQ                                                     
         BNE   *+14                                                             
         MVC   FVIFLD(L'ADRSTATE),ADRSTATE                                      
         B     *+10                                                             
         MVC   FVIFLD(L'OADRST),OADRST                                          
*                                                                               
DISTATX  DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE STATE CODE                                                           
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM                                                    
VALSTAT  DS    0H                                                               
         CLI   FVILEN,0                                                         
         BNE   VALSTA10                                                         
         TM    FLAG,FLGIDAD1       DID WE PRINT DEFAULT IDI                     
         BNO   EXITMIS             NO                                           
         B     VALSTAX                                                          
*                                                                               
VALSTA10 TM    FLAG,FLGIDAD1       DID WE PRINT DEFAULT IDI                     
         BO    EXITNV                                                           
         MVC   ADRSTATE,FVIFLD                                                  
VALSTAX  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ZIP CODE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
ZIP      LA    RF,ZIPTBL                                                        
         B     ITER                                                             
*                                                                               
ZIPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISZIP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALZIP)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY ZIP                                                         *         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM1     DSECT TO COVER SVD ADRR ELEM                  
         USING OADRELD,BOELEM1    DSECT TO COVER OLD SVD ADRR ELEM              
DISZIP   DS    0H                                                               
         OC    BOELEM1,BOELEM1                                                  
         BZ    DISZIPX                                                          
         CLI   ADRLN,ADRLNQ                                                     
         BNE   *+14                                                             
         MVC   FVIFLD(L'ADRZIP),ADRZIP                                          
         B     *+10                                                             
         MVC   FVIFLD(L'OADRZIP),OADRZIP  DISP FROM OLD 33 BYTE ADDR 3          
*                                                                               
DISZIPX  B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ZIP CODE                                                             
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM                                                    
VALZIP   DS    0H                                                               
         CLI   FVILEN,0                                                         
         BNE   VALZIP10                                                         
         TM    FLAG,FLGIDAD1       DID WE PRINT DEFAULT IDI                     
         BNO   EXITMIS             NO                                           
         B     VALZIPX                                                          
*                                                                               
VALZIP10 TM    FLAG,FLGIDAD1       DID WE PRINT DEFAULT IDI                     
         BO    EXITNV                                                           
         MVC   ADRZIP,FVIFLD                                                    
VALZIPX  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ZIP ROUTING NUMBER                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
ZIPRN    LA    RF,ZIPRNTBL                                                      
         B     ITER                                                             
*                                                                               
ZIPRNTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISZIPRN)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALZIPRN)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY ZIP ROUTING NUMBER                                          *         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM1     DSECT TO COVER SVD ADRR ELEM                  
         USING OADRELD,BOELEM1    DSECT TO COVER OLD SVD ADRR ELEM              
DISZIPRN DS    0H                                                               
         OC    BOELEM1,BOELEM1                                                  
         BZ    DISZRNX                                                          
         CLI   ADRLN,ADRLNQ                                                     
         BNE   *+14                                                             
         MVC   FVIFLD(L'ADRZIPRN),ADRZIPRN                                      
         B     *+10                                                             
         MVC   FVIFLD(L'OADRZRN),OADRZRN  DISP FROM OLD 33 BYTE ADDR 3          
*                                                                               
DISZRNX  B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ZIP ROUTING CODE                                                     
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING ADRELD,BOELEM                                                    
VALZIPRN DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    VALZRN20                                                         
*                                                                               
VALZRN10 TM    FLAG,FLGIDAD1       DID WE PRINT DEFAULT IDI                     
         BO    EXITNV                                                           
         MVC   ADRZIPRN,FVIFLD                                                  
VALZRN20 OC    BOELEM,BOELEM                                                    
         BZ    VALZRNX                                                          
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),T99RECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    VALZRNX                                                          
         DC    H'0'                                                             
VALZRNX  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CONTACT NAME                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CNAME    LA    RF,CNMTBL                                                        
         B     ITER                                                             
*                                                                               
CNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCNM)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY CONTACT NAME                                                *         
***********************************************************************         
*                                                                               
         USING FFTELD,R6          DSECT TO COVER FREE FORM CONTAC NAME          
DISCNM   DS    0H                                                               
         LA    R6,T99RFST                                                       
DISCNM10 CLI   0(R6),0                                                          
         BE    DISCNMX                                                          
         CLI   0(R6),FFTELQ                                                     
         BNE   DISCNM20                                                         
         CLI   FFTTYPE,FFTTCNAM    IS IT TYPE 75 CONTACT NAME                   
         BE    DISCNM30                                                         
DISCNM20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISCNM10                                                         
*                                                                               
DISCNM30 MVC   FVIFLD(L'FFTTNAME),FFTTNAME                                      
DISCNMX  B     EXITOK                                                           
         DROP  R6                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CONTACT NAME                                                         
***********************************************************************         
*                                                                               
         USING FFTELD,R5                                                        
VALCNM   DS    0H                                                               
         LA    R5,BOELEM                                                        
         XC    BOELEM,BOELEM        CLEAR ELEMENT BUILD AREA                    
         MVI   FFTEL,FFTELQ         BUILD X'DB' ELEMENT IN BOELEM               
         MVI   FFTLN,FFTT75LN+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTCNAM     CONTACT NAME FREE FORM TYPE                 
         MVI   FFTDLEN,FFTT75LN     ACTUAL TEXT OF LENGTH                       
         MVC   FFTTNAME,FVIFLD      MOVE NAME TO ELEMENT                        
VALCNMX  B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CONTACT PHONE NUMBER                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CPHONE   LA    RF,CPHNTBL                                                       
         B     ITER                                                             
*                                                                               
CPHNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPHON)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPHON)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY CONTACT PERSONS PHONE NUMBER                                *         
***********************************************************************         
*                                                                               
         USING FFTELD,R6          DSECT TO COVER FREE FORM CONTAC NAME          
DISPHON  DS    0H                                                               
         LA    R6,T99RFST                                                       
DISPHN10 CLI   0(R6),0                                                          
         BE    DISPHONX                                                         
         CLI   0(R6),FFTELQ                                                     
         BNE   DISPHN20                                                         
         CLI   FFTTYPE,FFTTCNAM    IS IT TYPE 75 NAME AND PHONE ELEM            
         BE    DISPHN30                                                         
DISPHN20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISPHN10                                                         
*                                                                               
DISPHN30 DS    0H                                                               
         CLI   CSACT,A#LST                                                      
         BE    DISPHN70                                                         
         L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(AT#TELN1)                                              
         BE    DISPHN40                                                         
         CLM   R1,3,=AL2(AT#TELN2)                                              
         BE    DISPHN50                                                         
         CLM   R1,3,=AL2(AT#TELN3)                                              
         BE    DISPHN60                                                         
*                                                                               
DISPHN40 MVC   FVIFLD(3),FFTTPHON    DISPLAY AREA CODE                          
         B     DISPHONX                                                         
DISPHN50 MVC   FVIFLD(3),FFTTPHON+3  DISPLAY FIRST 3 DIGITS OF PHONE NO         
         B     DISPHONX                                                         
DISPHN60 MVC   FVIFLD(4),FFTTPHON+6  DISPLAY FOUR DIGITS OF PHONE NO.           
         B     DISPHONX                                                         
*                                                                               
DISPHN70 DS    0H                                                               
         MVI   FVIFLD,C'('                                                      
         MVC   FVIFLD+1(3),FFTTPHON                                             
         MVI   FVIFLD+4,C')'                                                    
         MVC   FVIFLD+5(3),FFTTPHON+3                                           
         MVI   FVIFLD+8,C'-'                                                    
         MVC   FVIFLD+9(4),FFTTPHON+6                                           
*                                                                               
DISPHONX B     EXITOK                                                           
         DROP  R6                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CONTACT PERSON'S PHONE NUMBER                                        
***********************************************************************         
*                                                                               
         USING FFTELD,R5           NEW ELEMENT TO PUT BACK                      
VALPHON  DS    0H                                                               
         LA    R5,BOELEM                                                        
         TM    FVIIND,FVINUM       PHONE NUMBER DIGITS MUST BE NUMERIC          
         BNO   EXITNV                                                           
*                                                                               
         L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(AT#TELN1)                                              
         BE    VALPHN10                                                         
         CLM   R1,3,=AL2(AT#TELN2)                                              
         BE    VALPHN20                                                         
         CLM   R1,3,=AL2(AT#TELN3)                                              
         BE    VALPHN30                                                         
         DC    H'0'                SHOULD NEVER GET HERE                        
*                                                                               
VALPHN10 MVC   FFTTPHON(3),FVIFLD     AREA CODE                                 
         B     VALPHONX                                                         
VALPHN20 MVC   FFTTPHON+3(3),FVIFLD   FIRST 3 DIGITS OF PHONE                   
         B     VALPHONX                                                         
VALPHN30 MVC   FFTTPHON+6(4),FVIFLD                                             
VALPHONX B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PHONE EXTENSION DISPLAY                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
PHNEX    LA    RF,PHNEXTBL                                                      
         B     ITER                                                             
*                                                                               
PHNEXTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISPEX)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPEX)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPPLAY PHONE EXTENSION NUMBER                                               
***********************************************************************         
*                                                                               
         USING FFTELD,R6          DSECT TO COVER FREE FORM CONTAC NAME          
DISPEX   DS    0H                                                               
         LA    R6,T99RFST                                                       
DISPEX10 CLI   0(R6),0                                                          
         BE    DISPEXX                                                          
         CLI   0(R6),FFTELQ                                                     
         BNE   DISPEX20                                                         
         CLI   FFTTYPE,FFTTCNAM    IS IT TYPE 75 NAME AND PHONE ELEM            
         BE    DISPEX30                                                         
DISPEX20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISPEX10                                                         
*                                                                               
DISPEX30 MVC   FVIFLD(5),FFTTPHON+10                                            
DISPEXX  B     EXITOK                                                           
         DROP  R6                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PHONE EXTENSION NUMBER                                               
***********************************************************************         
*                                                                               
         USING FFTELD,R5                                                        
VALPEX   DS    0H                                                               
         LA    R5,BOELEM           PUT THIS ELEMENT BACK                        
         CLI   FVILEN,0            IS THERE ANYTHING IN EXTENSION               
         BE    VALPEXX                                                          
         TM    FVIIND,FVINUM       PHONE NUMBER DIGITS MUST BE NUMERIC          
         BNO   EXITNV                                                           
         MVC   FFTTPHON+10(5),FVIFLD   EXTENSION                                
VALPEXX  B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CONTACT PERSON'S EMAIL ADDRESS                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CEMAIL   LA    RF,CEMTBL                                                        
         B     ITER                                                             
*                                                                               
CEMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCEM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCEM)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY CONTACT PERSON'S EMAIL ADDRESS                                        
***********************************************************************         
*                                                                               
         USING FFTELD,R6          DSECT TO COVER FREE FORM CONTAC NAME          
DISCEM   DS    0H                                                               
         LA    R6,T99RFST                                                       
DISCEM10 CLI   0(R6),0                                                          
         BE    DISCEMX                                                          
         CLI   0(R6),FFTELQ                                                     
         BNE   DISCEM20                                                         
         CLI   FFTTYPE,FFTTCNAM    IS IT TYPE 75 NAME AND PHONE ELEM            
         BNE   DISCEM20                                                         
         CLI   FFTDLEN,FFTT75LN    DO WE HAVE E-MAIL ADDRS IN THIS ELEM         
         BH    DISCEM30                                                         
DISCEM20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISCEM10                                                         
*                                                                               
DISCEM30 DS    0H                                                               
         ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFTT75LN         GET LENGTH OF EMAIL ADDRESS                  
         BCTR  RE,0                SUBTRACT 1 FOR EXMVC                         
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),FFTTCEM                                                
DISCEMX  B     EXITOK                                                           
         DROP  R6                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CONTACT PERSON'S EMAIL ADDRESS                                       
***********************************************************************         
*                                                                               
         USING FFTELD,R5                                                        
DEL      USING FFTELD,R6                                                        
VALCEM   DS    0H                                                               
         LA    R5,BOELEM           PUT THIS ELEMENT BACK                        
         LA    R6,T99RFST          FIND AND DELETE ELEM FROM THIS REC           
VALCEM10 CLI   0(R6),0                                                          
         BE    VALCEM40                                                         
         CLI   0(R6),FFTELQ        IS IT DB ELEMENT                             
         BNE   VALCEM20                                                         
         CLI   DEL.FFTTYPE,FFTTCNAM    IS IT TYPE 75 CONTACT NAME               
         BE    VALCEM30                                                         
VALCEM20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     VALCEM10                                                         
*                                                                               
VALCEM30 MVI   DEL.FFTEL,X'FF'         MOVE X'FF' TO DELETE                     
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',T99RECD),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  DEL                                                              
VALCEM40 DS    0H                                                               
         BAS   RE,EMVAL            VALIDATE E-MAIL ADDRESS                      
         BNE   EXITNV              NOT VALID E-MAIL                             
         ZIC   RE,FVILEN           GET INPUT LENGTH                             
         ZIC   R1,FFTLN                                                         
         AR    R1,RE                                                            
         STC   R1,FFTLN            UPDATE ELEMENT LENGTH                        
         ZIC   R1,FFTDLEN                                                       
         AR    R1,RE                                                            
         STC   R1,FFTDLEN          UPDATE DATA LENGTH                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   FFTTCEM(0),FVIFLD   EMAIL ADDRESS                                
VALCEMX  GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),T99RECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    BOELEM,BOELEM       CLEAR BOELEM FOR DB TYPE 76 BUILD            
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NO. OF FORMS                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
FORMS    LA    RF,FORMTBL                                                       
         B     ITER                                                             
*                                                                               
FORMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFRM)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY NO. OF FORMS FOR THIS YEAR                                  *         
***********************************************************************         
*                                                                               
         USING FFTELD,R6          DSECT TO COVER FREE FORM CONTAC NAME          
DISFRM   DS    0H                                                               
         XC    SVTXINEL,SVTXINEL   CLEAR TAX INFO ELEMENT                       
         NI    FLAG,X'FF'-FLGFRMS  INITIALIZE NO. OF FRMS FLAG                  
         NI    FVATRB,X'FF'-FVAPROT      UNPROTECT THIS FIELD                   
*                                                                               
         LA    R6,T99RFST                                                       
DISFRM10 CLI   0(R6),0                                                          
         BE    DISFRMX                                                          
         CLI   0(R6),FFTELQ        IS IT A FREE FORM ELEMENT X'DB'              
         BNE   DISFRM20                                                         
         CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76 1099 INFORMATION               
         BE    DISFRM30                                                         
DISFRM20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISFRM10                                                         
*                                                                               
DISFRM30 DS    0H                                                               
         SR    R5,R5                                                            
         ICM   R5,3,FFTTNOF                                                     
         EDIT  (R5),(6,FVIFLD),WRK=BOWORK1,DUB=BODUB1,                 +        
               ZERO=NOBLANK                                                     
         CLC   FFTTDLLR,BCSPACES   HAS IT RUN LIVE ALREADY                      
         BNH   DISFRM40                                                         
*                                                                               
         OI    FVATRB,FVAPROT      PROTECT THIS FIELD                           
*                                                                               
         SR    R1,R1                                                            
         ICM   R1,3,FFTTNOF        IF PROTECTED CHECK FOR NO. OF FORMS          
         CHI   R1,250                                                           
         BL    DISFRM40                                                         
         OI    FLAG,FLGFRMS                                                     
*                                                                               
DISFRM40 ZIC   R1,FFTLN                                                         
         SHI   R1,1                                                             
         EX    R1,*+4                                                           
         MVC   SVTXINEL(0),0(R6)   SAVE THIS ELEMENT                            
*                                                                               
DISFRMX  B     EXITOK                                                           
         DROP  R6                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE NO. OF FORMS                                                         
***********************************************************************         
*                                                                               
         USING FFTELD,R5                                                        
VALFRM   DS    0H                                                               
         TM    FVIIND,FVINUM                                                    
         BNO   EXITNV                                                           
         NI    FLAG,X'FF'-FLGFRMS  INITIALIZE NO. OF FRMS FLAG                  
         XC    BOELEM,BOELEM        CLEAR ELEMENT BUILD AREA                    
         LA    R5,BOELEM                                                        
         MVI   FFTEL,FFTELQ         BUILD X'DB' ELEMENT IN BOELEM               
         MVI   FFTLN,FFTT76LN+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTTNNI     1099 INFO    FREE FORM TYPE                 
         MVI   FFTDLEN,FFTT76LN     ACTUAL TEXT OF LENGTH                       
*                                                                               
         ZIC   R1,FVILEN                                                        
         SHI   R1,1                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,FVIFLD(0)    CONVERT NO. OF FORMS TO HEX                  
         CVB   R1,BODUB1                                                        
         STCM  R1,3,FFTTNOF        SAVE NO. OF FORMS                            
*                                                                               
         CHI   R1,250              ARE THERE MORE THAN 250 FORMS                
         BL    VALFRMX                                                          
         OI    FLAG,FLGFRMS        NEED TCC CODE                                
VALFRMX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NO. OF FORMS FROM LAST YEAR                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LFORMS   LA    RF,LFORMTBL                                                      
         B     ITER                                                             
*                                                                               
LFORMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISLFRM)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY NO. OF FORMS FROM LAST YEAR IF ANY                          *         
***********************************************************************         
*                                                                               
         USING FFTELD,R5          DSECT TO COVER FREE FORM CONTAC NAME          
DISLFRM  DS    0H                                                               
         LA    R5,T99RFST                                                       
DISLFM10 CLI   0(R5),0                                                          
         BE    DISLFMX                                                          
         CLI   0(R5),FFTELQ        IS IT A FREE FORM ELEMENT X'DB'              
         BNE   DISLFM20                                                         
         CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76 1099 INFORMATION               
         BE    DISLFM30                                                         
DISLFM20 ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISLFM10                                                         
*                                                                               
DISLFM30 EDIT  FFTTLNOF,(6,FVIFLD),WRK=BOWORK1,DUB=BODUB1,ZERO=NOBLANK          
*                                                                               
DISLFMX  B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TRANSMISSION CONTROL CODE                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
TCODE    LA    RF,TCODETBL                                                      
         B     ITER                                                             
*                                                                               
TCODETBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTCD)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY TRANSMISSION CONTROL CODE                                   *         
***********************************************************************         
*                                                                               
         USING FFTELD,R5          DSECT TO COVER FREE FORM CONTAC NAME          
DISTCD   DS    0H                                                               
         CLI   CSACT,A#LST                                                      
         BE    DISTCD05                                                         
         LA    R5,SVTXINEL         POINT TO TAX INFORMATION ELEMENT             
         MVC   FVIFLD(L'FFTTTCC),FFTTTCC                                        
         B     DISTCDX                                                          
*                                                                               
DISTCD05 LA    R5,T99RFST                                                       
DISTCD10 CLI   0(R5),0                                                          
         BE    DISTCDX                                                          
         CLI   0(R5),FFTELQ        IS IT A FREE FORM ELEMENT X'DB'              
         BNE   DISTCD20                                                         
         CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76 1099 INFORMATION               
         BE    DISTCD30                                                         
DISTCD20 ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     DISTCD10                                                         
*                                                                               
DISTCD30 DS    0H                                                               
         MVC   FVIFLD(L'FFTTTCC),FFTTTCC                                        
DISTCDX  B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TRANSMISSION CONTROL CODE                                            
***********************************************************************         
*                                                                               
         USING FFTELD,R5                                                        
DB76     USING FFTELD,R6                                                        
VALTCD   DS    0H                                                               
         NI    FLAG,X'FF'-FLGTCCD                                               
         CLI   FVILEN,0                                                         
         BE    *+8                                                              
         OI    FLAG,FLGTCCD        TCCCODE WAS PROVIDED                         
*                                                                               
         LA    R5,BOELEM                                                        
         LA    R6,SVTXINEL                                                      
*                                                                               
         OC    BOELEM,BOELEM       DID WE BUILD DB 76 ELEM AT VALFRMS           
         BNZ   VALTCD10                                                         
*                                                                               
         MVI   FFTEL,FFTELQ         BUILD X'DB' ELEMENT IN BOELEM               
         MVI   FFTLN,FFTT76LN+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTTNNI     1099 INFO    FREE FORM TYPE                 
         MVI   FFTDLEN,FFTT76LN     ACTUAL TEXT OF LENGTH                       
         MVC   FFTTNOF,DB76.FFTTNOF VALIDATE FORMS IF IT WAS PROTECTED          
         DROP  DB76                                                             
*                                                                               
VALTCD10 TM    FLAG,FLGFRMS        DO WE HAVE MORE THAN 250 FORMS               
         BNO   VALTCD20                                                         
         CLI   FVILEN,0                                                         
         BE    EXITMIS             MISSING INPUT FLD IF NO TCC CODE             
VALTCD20 DS    0H                                                               
         CLI   FVILEN,0                                                         
         BE    *+10                                                             
         MVC   FFTTTCC,FVIFLD      MOVE IN TCC CODE                             
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DOWNLOAD INFO                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
DWNLD    LA    RF,DWNLTBL                                                       
         B     ITER                                                             
*                                                                               
DWNLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDWNL)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDWNL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DOWNLOAD (Y/N)                                              *         
***********************************************************************         
*                                                                               
         USING FFTELD,R6          DSECT TO COVER FREE FORM 1099 INFO            
DISDWNL  DS    0H                                                               
         CLI   CSACT,A#LST                                                      
         BE    DISDWN05                                                         
         LA    R6,SVTXINEL         POINT TO TAX INFORMATION ELEMENT             
         MVC   FVIFLD(L'FFTTDWNL),FFTTDWNL                                      
         B     DISDWNX                                                          
*                                                                               
DISDWN05 LA    R6,T99RFST                                                       
DISDWN10 CLI   0(R6),0                                                          
         BE    DISDWNX                                                          
         CLI   0(R6),FFTELQ        IS IT A FREE FORM ELEMENT X'DB'              
         BNE   DISDWN20                                                         
         CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76 1099 INFORMATION               
         BE    DISDWN30                                                         
DISDWN20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISDWN10                                                         
*                                                                               
DISDWN30 DS    0H                                                               
         MVC   FVIFLD(L'FFTTDWNL),FFTTDWNL                                      
DISDWNX  B     EXITOK                                                           
         DROP  R6                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DOWNLOAD                                                             
***********************************************************************         
*                                                                               
         USING FFTELD,R5                                                        
VALDWNL  DS    0H                                                               
         LA    R5,BOELEM           BUILDING NEW ELEMENT                         
         TM    FLAG,FLGFRMS        DO WE HAVE MORE THAN 250 FORMS               
         BNO   VALDWN10                                                         
         MVI   FFTTDWNL,C'Y'       DOWNLOAD MUST BE YES IF >= 250               
         B     VALDWNX                                                          
*                                                                               
VALDWN10 MVI   FFTTDWNL,C'N'       DEFAULT IS NOT DOWNLOADING                   
         CLI   FVILEN,0                                                         
         BE    VALDWNX                                                          
         CLI   FVIFLD,C'N'                                                      
         BE    VALDWNX                                                          
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV              INVALID INPUT FIELD                          
         TM    FLAG,FLGTCCD        WAS TCC CODE PROVIDED                        
         BNO   EXITTCC                                                          
*                                                                               
         MVC   FFTTDWNL,FVIFLD                                                  
VALDWNX  B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TIN/EIN NUMBER                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
TINEIN   LA    RF,TNENTBL                                                       
         B     ITER                                                             
*                                                                               
TNENTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISTNEN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTNEN)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY TIN/EIN                                                     *         
***********************************************************************         
*                                                                               
         USING FFTELD,R6          DSECT TO COVER FREE FORM 1099 INFO            
DISTNEN  DS    0H                                                               
         CLI   CSACT,A#LST                                                      
         BE    DISTNE05                                                         
         LA    R6,SVTXINEL         POINT TO TAX INFORMATION ELEMENT             
         MVC   FVIFLD(L'FFTTTIN),FFTTTIN                                        
         B     DISTNENX                                                         
*                                                                               
DISTNE05 LA    R6,T99RFST                                                       
DISTNE10 CLI   0(R6),0                                                          
         BE    DISTNENX                                                         
         CLI   0(R6),FFTELQ        IS IT A FREE FORM ELEMENT X'DB'              
         BNE   DISTNE20                                                         
         CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76 1099 INFORMATION               
         BE    DISTNE30                                                         
DISTNE20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISTNE10                                                         
*                                                                               
DISTNE30 DS    0H                                                               
         MVC   FVIFLD(L'FFTTTIN),FFTTTIN                                        
DISTNENX B     EXITOK                                                           
         DROP  R6                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TIN/EIN                                                              
***********************************************************************         
*                                                                               
         USING FFTELD,R5                                                        
DELDB    USING FFTELD,R6                                                        
VALTNEN  DS    0H                                                               
         LA    R5,BOELEM                                                        
         ZIC   R0,FVILEN                                                        
         LA    R1,FVIFLD                                                        
VALTN10  CLI   0(R1),X'F0'                                                      
         BL    EXITNV                                                           
         CLI   0(R1),X'F9'                                                      
         BH    EXITNV                                                           
         LA    R1,1(R1)                                                         
         BCT   R0,VALTN10                                                       
*                                                                               
         MVC   FFTTTIN,FVIFLD                                                   
*                                                                               
         LA    R6,T99RFST                                                       
VALTN20  CLI   0(R6),0                                                          
         BE    VALTN50                                                          
         CLI   0(R6),FFTELQ         IS IT X'DB' ELEMENT                         
         BNE   VALTN30                                                          
         CLI   DELDB.FFTTYPE,FFTTTNNI    IS IT TYPE 76 1099 INFO ELEM           
         BE    VALTN40                                                          
VALTN30  ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     VALTN20                                                          
*                                                                               
VALTN40  DS    0H                                                               
         MVC   FFTTLAOC,DELDB.FFTTLAOC  MOVE FROM OLD TO NEW                    
         MVC   FFTTDLLR,DELDB.FFTTDLLR  MOVE FROM OLD TO NEW                    
         MVC   FFTTLNOF,DELDB.FFTTLNOF                                          
*                                                                               
         MVI   DELDB.FFTEL,X'FF'         MOVE X'FF' TO DELETE                   
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',T99RECD),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         DROP  DELDB                                                            
*                                                                               
VALTN50  GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),T99RECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
VALTNX   B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CURRENT LIMITED ACCESS OFFICE CODE FROM IDI         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CLA      LA    RF,CLATBL                                                        
         B     ITER                                                             
*                                                                               
CLATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLA)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY CURRENT LIMITED ACCESS OFFICE LIST CODE FROM IDI            *         
***********************************************************************         
*                                                                               
DISCLA   DS    0H                                                               
         LA    RF,IOKEY                                                         
         USING CTIREC,RF                                                        
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKNUM,T99KOID     LOGIN ORIGIN ID TO CONTROL KEY               
         LHI   R1,XOREAD+XOCONFIL+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO2                                                          
         LA    R1,CTIDATA                                                       
DISCLA10 CLI   0(R1),0              THE END                                     
         BE    DISCLAX                                                          
*                                                                               
         CLI   0(R1),CTSYSELQ      X'21' SYSTEM AUTHORIZATION ELEMENT           
         BE    DISCLA30                                                         
DISCLA20 SR    R0,R0                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     DISCLA10                                                         
*                                                                               
         USING CTSYSD,R1                                                        
DISCLA30 CLI   CTSYSNUM,X'06'      IS IT ACCOUNTING SYSTEM                      
         BNE   DISCLA20                                                         
         MVC   SVLIM,CTSYSLMT      SAVE OFF LIMITED ACCESS IF ANY               
         DROP  R1                                                               
*                                                                               
         MVC   FVIFLD(L'SVLIM),SVLIM        LIMITED ACCESS FROM IDI             
DISCLAX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DATE OF LAST LIVE RUN                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LRDT     LA    RF,LRDTTBL                                                       
         B     ITER                                                             
*                                                                               
LRDTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLRDT)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY DATE OF LAST LIVE RUN IF AVAILABLE                          *         
***********************************************************************         
*                                                                               
         USING FFTELD,R6          DSECT TO COVER FREE FORM 1099 INFO            
DISLRDT  DS    0H                                                               
         CLI   CSACT,A#LST                                                      
         BE    DISLRD05                                                         
         LA    R6,SVTXINEL         POINT TO TAX INFORMATION ELEMENT             
         MVC   FVIFLD(L'FFTTDLLR),FFTTDLLR                                      
         B     DISLRDX                                                          
*                                                                               
DISLRD05 LA    R6,T99RFST                                                       
DISLRD10 CLI   0(R6),0                                                          
         BE    DISLRDX                                                          
         CLI   0(R6),FFTELQ        IS IT A FREE FORM ELEMENT X'DB'              
         BNE   DISLRD20                                                         
         CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76 1099 INFORMATION               
         BE    DISLRD30                                                         
DISLRD20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISLRD10                                                         
*                                                                               
DISLRD30 DS    0H                                                               
         MVC   FVIFLD(L'FFTTDLLR),FFTTDLLR                                      
DISLRDX  B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR USED LIMITED ACCESS OFFICE CODE WHEN RAN LIVE       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
ULA      LA    RF,ULATBL                                                        
         B     ITER                                                             
*                                                                               
ULATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISULA)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY USED LIMITED ACCESS OFFICE LIST CODE FROM 1099 RECORD       *         
***********************************************************************         
*                                                                               
         USING FFTELD,R6          DSECT TO COVER FREE FORM 1099 INFO            
DISULA   DS    0H                                                               
         CLI   CSACT,A#LST                                                      
         BE    DISULA05                                                         
         LA    R6,SVTXINEL                                                      
         MVC   FVIFLD(L'FFTTLAOC),FFTTLAOC                                      
         B     DISULAX                                                          
*                                                                               
DISULA05 LA    R6,T99RFST                                                       
DISULA10 CLI   0(R6),0                                                          
         BE    DISULAX                                                          
         CLI   0(R6),FFTELQ        IS IT A FREE FORM ELEMENT X'DB'              
         BNE   DISULA20                                                         
         CLI   FFTTYPE,FFTTTNNI    IS IT TYPE 76 1099 INFORMATION               
         BE    DISULA30                                                         
DISULA20 ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DISULA10                                                         
*                                                                               
DISULA30 DS    0H                                                               
         MVC   FVIFLD(L'FFTTLAOC),FFTTLAOC                                      
DISULAX  B     EXITOK                                                           
         DROP  R6                                                               
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
THIS     USING T99RECD,R2                                                       
LAST     USING T99RECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
*        DC    AL1(LDEFCLM),AL1(0,0,0),AL4(DEFCLM)                              
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* SET UP DEFAULT COLUMN LIST                                          *         
***********************************************************************         
*        SPACE 1                                                                
* DEFCLM   MVI   GSSMCODE,C'A'                                                  
*        B     EXITOK                                                           
*        EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
*                                                                               
FLST     MVC   IOKEY(L'T99KEY),THIS.T99RECD                                     
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   EXITL               MESS UP ON THE READ HIGH                     
         B     NLST02                                                           
*                                                                               
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
*                                                                               
NLST     LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(T99KYEAR-T99RECD),THIS.T99RECD                             
* NLST02   CLC   IOKEY(T99KOID-T99RECD),THIS.T99RECD                            
         BE    *+8                                                              
         B     EXITL                                                            
         MVC   THIS.T99KEY(L'T99KEY+L'T99KSTA+L'T99KDA),IOKEY                   
         B     EXITOK                                                           
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
*                                                                               
FTFLST   B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CHECK IF VALID EMAIL ADDRESS                                           
*-------------------------------------------------------------------*           
EMVAL    NTR1                                                                   
         MVI   FLGEM,0             INIT FLAG                                    
         ZIC   R2,FVILEN           PICK UP THE LENGTH                           
         LR    R1,R2                                                            
         LA    R6,FVIFLD                                                        
         LR    R3,R6               EXTRA POINTER TO EMAIL ADDRS                 
         AR    R6,R2               CHECK FOR VALID .XXX                         
*                                                                               
EMVAL10  SHI   R6,1                                                             
         CLI   0(R6),C'.'          IS IT DOT SOMETHING                          
         BE    EMVAL20                                                          
         CLI   0(R6),C'A'          IS IT HIGHER THAN A                          
         BL    EMVALERR                                                         
         CLI   0(R6),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALERR                                                         
         SHI   R2,1                                                             
         LR    R2,R2                                                            
         BNZ   EMVAL10                                                          
         B     EMVALERR            NO '.' FOUND                                 
*                                                                               
EMVAL20  CR    R2,R1                                                            
         BE    EMVALERR            ENDS WITH '.'                                
         LR    R2,R1               RESET LENGTH                                 
*                                                                               
         CLI   0(R3),C'.'          DOES USER NAME START WITH DOT                
         BE    EMVALERR            ERROR CAN'T START WITH DOT                   
EMVAL30  DS    0H                                                               
         CLI   0(R3),C'0'          IS IT LESS THAN F0                           
         BL    EMVAL40             YES CHK NEXT                                 
         CLI   0(R3),C'9'          IS IT HIGHER THAN F9                         
         BNH   EMVAL50             IT IS A NUMBER                               
         B     EMVALERR                                                         
EMVAL40  CLI   0(R3),C'A'          IS IT HIGHER THAN A                          
         BL    EMVAL60             CHECK FOR SPECIAL CHARS                      
         CLI   0(R3),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALERR                                                         
*                                                                               
EMVAL50  DS    0H                                                               
         LA    R3,1(R3)            GET NEXT CHAR                                
         BCT   R2,EMVAL30                                                       
         B     EMVALGD                                                          
*                                                                               
EMVAL60  DS    0H                                                               
         LA    R1,EXCTAB           POINT TO SPECIAL CHARS TABLE                 
EMVAL60A CLI   0(R1),X'FF'         DID WE FIND SPECIAL CHARS                    
         BE    EMVALERR            NO SPCL CHAR FND, ERROR.                     
         CLI   0(R3),C'@'          HAVE WE REACHED @ YET                        
         BNE   EMVAL6AA                                                         
         TM    FLGEM,ATFOUND       MAKE SURE NO MORE THAN ONE @ SIGN            
         BO    EMVALERR                                                         
         OI    FLGEM,ATFOUND       NOW WE HAVE ONE @ IN E-MAIL                  
EMVAL6AA CLC   0(1,R1),0(R3)       IS IT SPCL CHAR                              
         BE    EMVAL60C                                                         
EMVAL60B LA    R1,1(R1)            POINT TO NEXT TABLE ENTRY                    
         B     EMVAL60A                                                         
*                                                                               
EMVAL60C DS    0H                  MAKE SURE NO 2 SPCL CHRS APEAR TOGE          
         LA    R5,EXCTAB                                                        
EMVAL60D CLI   0(R5),X'FF'                                                      
         BE    EMVAL50                                                          
         CLC   1(1,R3),0(R5)       ARE BOTH SPECIAL CHARS                       
         BE    EMVALERR                                                         
         LA    R5,1(R5)            POINT TO NEXT CHAR IN TAB                    
         B     EMVAL60D                                                         
*                                                                               
EMVALGD  DS    0H                                                               
         TM    FLGEM,ATFOUND       SHOULD HAVE ONE @ IN E-MAIL                  
         BZ    EMVALERR            NO @ FOUND ERROR                             
         CR    RB,RB                                                            
         B     *+6                                                              
*                                                                               
EMVALERR CR    RB,RD                                                            
EMVALX   XIT1                                                                   
EXCTAB   DC    C'@'                @ SIGN                                       
         DC    C'_'                UNDERSCORE                                   
         DC    C'.'                DOT                                          
         DC    C'-'                DASH                                         
         DC    X'FF'               END OF TABLE                                 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -             
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         LTORG                                                                  
         SPACE 2                                                                
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#ALL,3,L                                                       
DCLISTX  DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
                                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
*                                                                               
*ACFILWORK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DSECT TO COVER OLD ADDRESS ELEMENT ADRELD, WITH CITY, STATE ZIP     *         
***********************************************************************         
OADRELD  DSECT                                                                  
OADREL   DS    X                                                                
OADRELQ  EQU   X'22'                                                            
OADRLN   DS    XL1                                                              
OADRSTAT DS    XL1                 STATUS                                       
OADRCSZ  EQU   X'80'                                                            
OADRLN1  DS    CL33                ADDRESS LINE 1                               
OADRLN2  DS    CL33                ADDRESS LINE 2                               
OADRCSZP DS    0CL33               CITY, STATE, ZIP, ZIP ROUTE                  
OADRCITY DS    CL22                                                             
OADRST   DS    CL2                 STATE                                        
OADRZIP  DS    CL5                                                              
OADRZRN  DS    CL4                 ZIP ROUTING NO.                              
OADRLNQ  EQU   *-OADRELD           LENGTH OF THIS ELEMENT                       
         EJECT                                                                  
*                                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWSAVE                               *         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWSAVE                                                           
SAVEVALS EQU   *                                                                
SVLOGID  DS    CL10                SAVED LOGIN ID                               
SVLIM    DS    CL4                 SAVED LIMITED ACCESS OF 1099 USER ID         
FLAG     DS    X                   FLAG                                         
FLGGLBL  EQU   X'80'               DOING GLOBAL ID                              
FLGPRVYR EQU   X'40'               NOT A CURRENT YEAR                           
FLGIDNAM EQU   X'20'         NAME ADDRS IS TAKEN FROM IDI RECORD                
FLGIDAD1 EQU   X'10'         NAME ADDRS IS TAKEN FROM IDI RECORD                
FLGFRMS  EQU   X'08'         NO.OF FORMS >=250 NEED TCC CODE                    
FLGTCCD  EQU   X'04'         IF SET, TCC CODE IS PRESENT                        
*                                                                               
FLGEM    DS    XL1                 FLAG FOR EMAIL VALIDATION                    
ATFOUND  EQU   X'80'               @ SIGN FOUND                                 
*                                                                               
SVTXINEL DS    CL255               SAVE TAX INFO ELEMENT X'DB' TYPE 76          
         ORG   TWSAVE+L'TWSAVE-(*-SAVEVALS)                                     
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
*                                                                               
SVNAME   DS    CL36                SAVE ACCOUNT NAME                            
SVDESTEL DS    CL255               DEST DETAIL X'30' CONTROL ELEM               
SVOID    DS    XL2                 SAVE ORIGIN ID FROM CONTROL REC              
*                                                                               
SVTODAY  DS    0CL8                TODAY'S DATE YYYYMMDD                        
SVYEAR   DS    CL4                                                              
SVMON    DS    CL2                                                              
SVDAY    DS    CL2                                                              
*                                                                               
SVCITY   DS    CL(L'ADRCITY)                                                    
SVSTATE  DS    CL(L'ADRSTATE)                                                   
SVZIP    DS    CL(L'ADRZIP+L'ADRZIPRN)                                          
BOELEM1  DS    CL256                                                            
*                                                                               
DSLIST   DS    0C                                                               
AC@ALL   DS    CL3                 YES                                          
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLUSER                                                           
TLSCODE  DS    XL(L'DSCCODE)       SCHEME/WORK CODES                            
TLUNITS  DS    XL12                UNITS                                        
TLBIT    DS    CL1                 OFF IF SAME ACCOUNT IN LIST                  
TLLNQ    EQU   *-TLSTD                                                          
*                                                                               
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019ACFIL35   05/08/18'                                      
         END                                                                    
