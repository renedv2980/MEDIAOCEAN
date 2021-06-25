*          DATA SET ACFIL1D    AT LEVEL 007 AS OF 12/10/12                      
*&&      SET   NOP=N                                                            
*PHASE T6231DA,*                                                                
         SPACE 1                                                                
         TITLE 'GST RULE RECORD'                                                
         SPACE 2                                                                
FIL1D    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL1D**,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
         LH    R6,=Y(TWUSER-TWAD)                                               
         AR    R6,RA                                                            
         USING TWUSER,R6           R5=SAVED STORAGE                             
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
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
*                                                                               
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
*                                                                               
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         L     R1,CALLR1                                                        
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
*                                                                               
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
*                                                                               
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
*                                                                               
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
*                                                                               
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
*                                                                               
FLTXX    MVI   SVPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     MVI   VATINDS,0           CLEAR INDICATOR                              
         MVI   FILINDS1,0                                                       
         TM    BCCPYST1,CPYSOROE   OFFICE REQUIRED ON EXPENSES                  
         BZ    *+8                                                              
         OI    VATINDS,VATCOFF     COMPANY IS ON OFFICE                         
         TM    BCCPYST4,CPYSOFF2                                                
         BZ    *+8                                                              
         OI    VATINDS,VATNOFFS    NEW OFFICE SYSTEM                            
*                                                                               
* REMOVED THIS CODE BECAUSE SG IS ALWAYS THE TAX LEDGER SO NO NEED TO           
* READ COMPANY REC.  ALSO WAS DYING IN GETEL BECAUSE GCFILNAM WAS NOT           
* FILLED IN AND CAN'T FIGURE OUT WHY.                                           
*                                                                               
*        USING CPYRECD,R2                                                       
*        LA    R2,IOKEY                                                         
*        MVC   CPYKEY,BCSPACES   READ COMPANY RECORD                            
*        MVC   CPYKCPY,CUABIN      CONNECTED ID                                 
*        LHI   R1,XOREAD+XOACCDIR+XIO1                                          
*        GOTO1 AIO                                                              
*        BE    *+6                                                              
*        DC    H'0'                                                             
*        LHI   R1,XOGET+XOACCMST+XIO1                                           
*        GOTO1 AIO                                                              
*        BE    *+6                                                              
*        DC    H'0'                BAD COMPANY RECORD                           
*        GOTO1 AGETEL,BOPARM,('CPYELQ',AIO1),0                                  
*        BE    *+6                                                              
*        DC    H'0'                NO COMPANY ELEMENT                           
*                                                                               
*        USING CPYELD,R2                                                        
*        LA    R2,BOELEM                                                        
*        MVC   TAXUL,CPYTAX        SAVE THE TAX U/L                             
*        DROP  R2                                                               
*                                                                               
T        USING LDGRECD,IOKEY                                                    
         MVC   IOKEY,BCSPACES                                                   
         MVC   T.LDGKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.LDGKUNT(L'TAXUL),TAXUL  READ TAX LEDGER RECORD                 
         DROP  T                                                                
*                                                                               
         GOTO1 AGETLDG                                                          
         BNE   EXITL               LEDGER RECORD MISSING                        
*                                                                               
         USING LDGTABD,R2                                                       
         ICM   R2,15,ACALDG                                                     
         MVI   ACTHILEN,0                                                       
         CLI   LDGTLVA,L'ACTKACT   ONLY 1 LEVEL                                 
         BE    INIT02                                                           
         LA    RE,LDGTLVD          LEVEL D LENGTH                               
         CLI   0(RE),0                                                          
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         B     *-10                                                             
*                                                                               
         BCTR  RE,0                                                             
         MVC   ACTHILEN,0(RE)     PENULTIMATE LEVEL                             
*                                                                               
INIT02   MVC   SLDGOPOS,LDGTOFFP   SAVE LOCATION OF OFFICE FOR LEDGER           
* THE FOLLOWING WAS COPIED FROM UK CODE BUT REMOVED BECAUSE THIS RULE           
* WAS NOT HONORED IN =FILE.                                                     
*        CLI   SLDGOPOS,LDGOTRAN   IF CODE IN TRANSACTION RECORD                
*        BNE   *+8                 COMPANY EFFECTIVELY NOT ON OFFICES           
*        NI    VATINDS,FF-VATCOFF                                               
*                                                                               
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLISTU,DSLISTU                           
         GOTO1 (RF),(R1),C'LL  ',DCLISTL,DSLISTL                                
         B     EXITOK                                                           
         DROP  R2                                                               
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
         EJECT 1                                                                
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
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFK)                                    
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
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
         USING TAXRECD,R2                                                       
KEY      LM    R0,R3,SVPARMS                                                    
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4                                                      
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
KFKVAL   XC    TAXKEY,TAXKEY       INITIALIZE KEY OF VAT RULE RECORD            
         MVI   TAXKTYP,TAXKTYPQ    TAX RECORD TYPE                              
         MVC   TAXKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    TAXKEY,TAXKEY       INITIALIZE KEY OF VAT RULE RECORD            
         MVI   TAXKTYP,TAXKTYPQ    TAX RECORD TYPE                              
         MVC   TAXKCPY,CUABIN      CONNECTED ID                                 
*        MVC   TAXKOFF,EFFS        DEFAULT OFFICE                               
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
         USING TAXRECD,R2                                                       
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
RFTABL   DS    0H                                                               
         DC    AL1(RCPY),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT FOR ADD/COPY/CHANGE - 'FA' POINTER ELEM          
***********************************************************************         
         SPACE 1                                                                
RFADD    DS    0H                                                               
         GOTO1 ADDACTEL,AIOREC     ADD POINTER ELEMENT                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                        *          
***********************************************************************         
         SPACE 1                                                                
RECLAST  L     R1,SVPARMS4                                                      
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
******   DC    AL1(RCPY),AL1(0,0,0),AL4(RLADD)                                  
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR ADD/COPY/CHANGE - 1R RECORD ACTIVITY PASSIVE POINTER            
***********************************************************************         
         SPACE 2                                                                
RLADD    DS    0H                                                               
         GOTO1 ADDACTPT,AIOREC       ADD ACTIVITY POINTER                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GENERATE ACTIVITY POINTER ELEMENTS                                 
* R1 POINTS TO RECORD                                                           
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R5                                                       
ADDACTEL NTR1                                                                   
         TM    BCCPYST6,CPYSRAPP   TEST FOR RECORD ACTIVITY POINTERS            
         BZ    EXITOK              NO                                           
         L     R2,0(R1)                                                         
         L     R5,AIO3                                                          
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM    BUILD RAP PTR ELEM                           
         MVC   RAPCPY,CUABIN                                                    
         MVI   RAPRTYP,RAPKRTAX                                                 
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOM                                                     
         ST    R2,RAPAREC          ADDRESS OF RECORD                            
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GENERATE ACTIVITY POINTER                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R5                                                       
ADDACTPT NTR1                                                                   
         TM    BCCPYST6,CPYSRAPP   TEST FOR RECORD ACTIVITY POINTERS            
         BZ    EXITOK              NO                                           
         L     R5,AIO3                                                          
         MVI   RAPACTN,RAPAPTR     BUILD RAP PTR RECORD                         
         CLI   CSACT,A#CPY         FOR ACTN COPY CLEAR OUT THE OLD PTR          
         BNE   *+10                OR ELSE WON'T BUILD THE ACTIVITY             
         XC    RAPOLDP,RAPOLDP     REC                                          
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* PFKEY OBJECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
PFK      LM    R0,R3,SVPARMS                                                    
         USING ACTRECD,R2                                                       
         LA    RF,PFKTABL                                                       
         B     ITER                                                             
*                                                                               
PFKTABL  DC    AL1(PFLST),AL1(0,0,0),AL4(PPFLST)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DETERMINE WHETHER PFKEY VALID FOR INCLUSION                         *         
***********************************************************************         
         SPACE 1                                                                
PPFLST   DS    0H                                                               
*                                                                               
         L     RF,SVPARMS3                                                      
         USING FRPELD,RF                                                        
         CLI   CSACT,A#LST         FOR LIST CHANGE KEY IS PF3                   
         BNE   PPFLST05                                                         
         CLI   FRPPFK#,PFK03       GSTRULES/CHANGE PFKEY?                       
         BNE   EXITOK                                                           
         TM    CUSTAT,CUSDDS       ONLY VALID FOR DDS TERMINALS                 
         BO    EXITOK                                                           
         B     EXITL                                                            
*                                                                               
PPFLST05 CLI   FRPPFK#,PFK02       ALL OTHERS ACTIONS ARE PF2                   
         BNE   EXITOK                                                           
         TM    CUSTAT,CUSDDS       ONLY VALID FOR DDS TERMINALS                 
         BO    EXITOK                                                           
         B     EXITL                                                            
         DROP  RF                                                               
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
         USING TAXRECD,R2                                                       
         L     R2,SVPARMS+12                                                    
         XR    R1,R1                                                            
         IC    R1,SVPARMS+8        GET GLOBAL VERB                              
         LA    RF,DTATABL                                                       
         B     ITER                                                             
*                                                                               
         USING KNOWTABD,RF                                                      
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
*                                                                               
DATA04   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
*                                                                               
         USING TAXRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         BR    RF                                                               
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
KNOWTAB  DC    AL2(VAT#OFFC),AL4(OFCDTA)  OFFICE CODE          (KEY)            
         DC    AL2(VAT#PRV),AL4(PRVDTA)   PROVENCE             (KEY)            
         DC    AL2(VAT#EFFDT),AL4(EDTDTA) EFFECTIVE DATE       (KEY)            
         DC    AL2(VAT#TAXRG),AL4(TRGDTA) TAX REGISTRATION                      
         DC    AL2(VAT#OFFNM),AL4(NAMDTA) OFFICE NAME                           
*                                                                               
         DC    AL2(VAT#TXCD1I),AL4(TXCDTA) INPUT TAX CODE (THE 1ST ONE)         
         DC    AL2(VAT#TXCDI),AL4(TXCDTA) INPUT TAX CODE (THE REST)             
         DC    AL2(VAT#TYPEI),AL4(TTYDTA) INPUT TAX TYPE                        
         DC    AL2(VAT#TXACCI),AL4(TACDTA) INPUT ACCOUNT CODE                   
         DC    AL2(VAT#TXNMI),AL4(TXNDTA) INPUT ACCOUNT NAME                    
         DC    AL2(VAT#TXRTI),AL4(TXRDTA) INPUT TAX RATE                        
*                                                                               
         DC    AL2(VAT#TXCDOS),AL4(TXCDTA) OUTPUT TAX CODE "S"                  
         DC    AL2(VAT#TXCDOX),AL4(TXCDTA) OUTPUT TAX CODE "X"                  
         DC    AL2(VAT#TXCDOZ),AL4(TXCDTA) OUTPUT TAX CODE "Z"                  
         DC    AL2(VAT#TXCDO),AL4(TXCDTA) OUTPUT TAX CODE (THE REST)            
         DC    AL2(VAT#TYPEO),AL4(TTYDTA) OUTPUT TAX TYPE                       
         DC    AL2(VAT#TXACCO),AL4(TACDTA) OUTPUT ACCOUNT CODE                  
         DC    AL2(VAT#TXNMO),AL4(TXNDTA) OUTPUT ACCOUNT NAME                   
         DC    AL2(VAT#TXRTO),AL4(TXRDTA) OUTPUT TAX RATE                       
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL1D    CSECT                                                                  
REQOPVC  DC    C'X'                REQUIRED OUTPUT CODES                        
         DC    C'S'                                                             
         DC    C'Z'                                                             
REQOPVCN EQU   *-REQOPVC           # REQUIRED OUTPUT CODES                      
         SPACE 2                                                                
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
DFDDIS   DS    0H                                                               
         MVI   DISCNTER,1                                                       
         B     EXITOK                                                           
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   MVI   VALCNTER,1                                                       
         MVI   DISCNTER,1                                                       
         MVI   INPINDS,0                                                        
*        MVI   BIT,0                                                            
         NI    FILINDS1,FF-FILDNTAX                                             
         XC    TAXCTAB(DISMAX),TAXCTAB   CLEAR TAX ACCOUNT CODE TABLE           
         XC    ACCDTAB,ACCDTAB     CLEAR ACCOUNT CODE TABLE                     
         XC    TAXRSTA,TAXRSTA     CLEAR TAX RECORD STATUS                      
         L     RE,AIO6             CLEAR IO6 FOR STORING                        
         LA    RF,IOAREALN         NEW TAX ELEMENTS                             
         XCEF                                                                   
         L     RE,AIO6                                                          
         ST    RE,ACURTXEL         A(CURRENT NEW TAX ELEMENT)                   
DFDVALX  B     EXITOK                                                           
         EJECT                                                                  
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
DLDVAL   DS    0H                                                               
         TM    VATINDS,VATOFREC                                                 
         BZ    DLDV10                                                           
         CLI   CSACT,A#ADD         IF ADDING/CHANGING AN OFFICE LEVEL           
         BE    DLDV25              RECORD THAN RECORD ALREADY COPIED            
         CLI   CSACT,A#CHA         FROM COMPANY LEVEL RECORD                    
         BE    DLDV25                                                           
*                                                                               
DLDV10   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('TAXIELQ',TAXRECD),0              
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('TAXOELQ',TAXRECD),0              
*        NI    VATINDS,FF-VATCDFNO                                              
*                                                                               
         L     R4,AIO6             SAVE TAX ELEMENTS BUFFER                     
         USING SVTXELD,R4                                                       
         PUSH  USING                                                            
         USING TAXELD,BOELEM                                                    
         SR    RF,RF                                                            
DLDV15   XC    BOELEM,BOELEM                                                    
         CLI   SVTXCODE,0                                                       
         BE    DLDV20              NO MORE TAX ELEMENT                          
         MVC   TAXEL,SVTXCDE       BUILD TAX ELEMENT                            
         MVC   TAXLN,SVTXLN                                                     
         MVC   TAXTYPE,SVTXTYPE                                                 
         MVC   TAXACTU(L'TAXUL),TAXUL                                           
         MVC   TAXACTA,SVTXACTA                                                 
         MVC   TAXCODE,SVTXCODE                                                 
         MVC   TAXINDS,SVTXINDS                                                 
         MVC   TAXRATE,SVTXRATE                                                 
         ZIC   RF,TAXLN                                                         
         SH    RF,=Y(TAXLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   TAXNAME,SVTXNAME                                                 
         GOTO1 AADDEL,BOPARM,TAXRECD                                            
         LA    R4,SVTXELLN(R4)                                                  
         B     DLDV15                                                           
*                                                                               
         USING TAXELD,R3                                                        
DLDV20   TM    VATINDS,VATOFREC    FOR OFFICE REC DON'T CHECK                   
         BO    DLDV25              FOR REQUIRED TAX CODES                       
         LA    R3,TAXRFST                                                       
         NI    BIT2,X'FF'-(OUTCDS+OUTCDX+OUTCDZ)                                
DLDV20A  CLI   TAXEL,0             TEST E-O-R                                   
         BE    DLDV24                                                           
         CLI   TAXEL,TAXOELQ       OUTPUT?                                      
         BNE   DLDV22                                                           
         CLI   TAXCODE,C'S'                                                     
         BNE   *+12                                                             
         OI    BIT2,OUTCDS                                                      
         B     DLDV22                                                           
         CLI   TAXCODE,C'X'                                                     
         BNE   *+12                                                             
         OI    BIT2,OUTCDX                                                      
         B     DLDV22                                                           
         CLI   TAXCODE,C'Z'                                                     
         BNE   *+8                                                              
         OI    BIT2,OUTCDZ                                                      
DLDV22   ZIC   RF,TAXLN            BUMP R3 TO NEXT ELEMENT                      
         AR    R3,RF                                                            
         B     DLDV20A                                                          
*                                                                               
DLDV24   MVC   FVMSGNO,=AL2(AE$REQO)                                            
         MVC   FVADDR,AOUTADDR     PUT CURSOR INTO OUTPUT CODE FIELD            
         TM    BIT2,OUTCDS+OUTCDX+OUTCDZ                                        
         BNO   EXITL                                                            
*                                                                               
DLDV25   TM    VATINDS,VATOFREC    SKIP CHECKING IF OFFICE RECORD               
         BNZ   DLDV30                                                           
         MVC   FVADDR,ATAXCODE     PUT CURSOR INTO TAX CODE FIELD               
         TM    INPINDS,INPFNDCD    AT LEAST ONE INPUT SHOULD                    
         BZ    EXITNO              MUST BE DEFINED                              
         TM    INPINDS,INPFNDID    1 TYPE MUST SPECIFIED AS DEFAULT             
         BO    DLDV30                                                           
         MVC   FVMSGNO,=AL2(AE$DEFTP)                                           
         B     EXITL                                                            
         POP   USING                                                            
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*        UPDATE/ADD SG ACCOUNT RECORDS                                          
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         USING TAXELD,R3           R3=A(TAX ELEMENT)                            
DLDV30   LA    R3,TAXRFST                                                       
DLDV40   NI    FILINDS1,X'FF'-FILADDAC   BUILD NEW ACCOUNT RECORD               
         CLI   TAXEL,0             TEST E-O-R                                   
         BE    DLDVALX                                                          
*                                                                               
         CLI   0(R3),TAXIELQ       INPUT TAX CODE?                              
         BE    *+12                OR                                           
         CLI   0(R3),TAXOELQ       OUTPUT TAX CODE?                             
         BNE   DLDV110             NO-THAN DON'T WANT IT                        
         USING ACTRECD,R5          R5=A(ACCOUNT RECORD KEY)                     
         LA    R5,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,TAXKCPY                                                  
         MVC   ACTKUNT(L'TAXACT),TAXACT                                         
         L     R1,=AL4(XORDUP+XOACCDIR+XIO2)                                    
         GOTO1 AIO                                                              
         BE    DLDV80                                                           
*                                                                               
         OI    FILINDS1,FILADDAC   BUILD NEW ACCOUNT RECORD                     
         L     R5,AIO2                                                          
         MVC   IOKEY,IOKEYSAV                                                   
         MVC   ACTKEY,IOKEYSAV                                                  
         XC    ACTRSTA,ACTRSTA                                                  
         OI    ACTRSTAT,ACTSABLP   ACCOUNT HAS BALANCE ELEMENT                  
         LA    RE,ACTRFST                                                       
         MVI   0(RE),0                                                          
         SR    RE,R5                                                            
         LA    RE,1(,RE)           LENGTH OF ACCOUNT RECORD                     
         STCM  RE,3,ACTRLEN                                                     
*                                                                               
         LA    RF,BOELEM           ADD 30 ELEMENT W/ LONG LENGTH                
         USING RSTELD,RF                                                        
         XC    RSTELD(RSTLN3Q),RSTELD                                           
         MVI   RSTEL,RSTELQ                                                     
         MVI   RSTLN,RSTLN3Q                                                    
         MVC   RSTBDATE,BCTODAYP                                                
         MVC   RSTTDATE,BCTODAYP                                                
         MVI   RSTCOSTG,C' '                                                    
         MVI   RSTFILT1,C' '                                                    
         MVI   RSTFILT2,C' '                                                    
         MVI   RSTFILT3,C' '                                                    
         MVI   RSTFILT4,C' '                                                    
         MVI   RSTFILT5,C' '                                                    
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),(R5),RSTELD,0                      
         CLI   12(R1),0                                                         
         BNE   EXITL                                                            
         DROP  RF                                                               
*                                                                               
* ADDRST ROUTINE ADDS 30 ELEMENT WITH 2ND LENGTH NOT THE LONGEST                
*                                                                               
*        GOTO1 AADDRST,ACTRECD     ADD STATUS ELEMENT                           
         GOTO1 AGETEL,BOPARM,('RSTELQ',ACTRECD),0                               
T        USING RSTELD,BOELEM                                                    
         OI    T.RSTSTAT2,RSTSIVAT  SET TO INPUT                                
         CLI   TAXEL,TAXIELQ                                                    
         BE    *+8                                                              
         NI    T.RSTSTAT2,FF-RSTSIVAT OR OUTPUT                                 
         GOTO1 AREPEL,BOPARM,('RSTELQ',ACTRECD),0,BOELEM                        
         GOTO1 AADDBAL,ACTRECD     ADD BALANCE ELEMENT                          
         B     DLDV90                                                           
*                                                                               
DLDV80   L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R5,AIO2                                                          
         MVC   ACTKEY,IOKEY                                                     
*                                                                               
T        USING NAMELD,BOELEM       ADD/CHANGE ACCOUNT NAME ELEMENT              
DLDV90   XC    BOELEM,BOELEM                                                    
         MVI   T.NAMEL,NAMELQ                                                   
         MVC   T.NAMEREC,TAXNAME                                                
         XR    RF,RF                                                            
         IC    RF,TAXLN                                                         
         SHI   RF,TAXLN1Q-NAMLN1Q                                               
         STC   RF,T.NAMLN                                                       
         DROP  T                                                                
         GOTO1 AREPEL,BOPARM,('NAMELQ',ACTRECD),0,BOELEM                        
         BE    DLDV100             NAME HAS BEEN CHANGED                        
         TM    FILINDS1,FILADDAC   IF ACTION ADD,                               
         BO    DLDV100             DON'T ADD NAME CHANGE POINT                  
         GOTO1 ANAMCHA,ACTRECD     WRITE NAME CHANGE POINTER                    
*                                                                               
         USING RATELD,BOELEM       ADD/CHANGE ACCOUNT RATE ELEMENT              
DLDV100  XC    BOELEM,BOELEM                                                    
         MVI   RATEL,RATEVATQ                                                   
         MVI   RATLN,RATLN2Q                                                    
         MVC   RATRATE,TAXRATE                                                  
         OI    RATINDS,RATIDC3     3DP RATE                                     
         GOTO1 AREPEL,BOPARM,('RATEVATQ',ACTRECD),0,BOELEM                      
*                                                                               
         LHI   R1,XOADDREC+XOACCMST+XIO2                                        
         TM    FILINDS1,FILADDAC                                                
         BNZ   *+8                                                              
         LHI   R1,XOPUTREC+XOACCMST+XIO2   CHANGE ACCOUNT RECORD                
         GOTO1 AIO                                                              
         OI    FILINDS1,FILDNTAX   DISPLAY NEW TAX RECORD                       
*                                                                               
DLDV110  ZIC   RF,TAXLN            BUMP R3 TO NEXT ELEMENT                      
         AR    R3,RF                                                            
         B     DLDV40                                                           
*                                                                               
DLDVALX  MVC   FVADDR,AEFFDATE     PUT CURSOR INTO EFFDATE FIELD                
         B     EXITOK                                                           
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFCDTA   LA    RF,OFCTBL                                                        
         B     ITER                                                             
*                                                                               
OFCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFC)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOFC)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOFC)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOFC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOFC)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFC)                                
         DC    AL1(DHED),AL1(0,0,0),AL4(DHDOFC)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(HEDOFC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETOFC  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISOFC   TM    VATINDS,VATCOFF     OFFICE IS NOT REQUIRED                       
         BZ    EXITOK                                                           
         CLC   TAXKOFF,EFFS        IF COMPANY OFFICE RECORD,                    
         BE    EXITOK              DON'T DISPLAY OFFICE CODE                    
         MVC   FVIFLD(L'TAXKOFF),TAXKOFF                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A OFFICE CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALOFC   NI    VATINDS,FF-VATOFREC-VATCPREC                                     
         MVC   TAXKOFF,EFFS                                                     
         TM    VATINDS,VATCOFF     OFFICE IS NOT REQUIRED                       
         BZ    VOFC01                                                           
         CLI   FVILEN,0            NO INPUT                                     
         BNE   VOFC02                                                           
         OI    VATINDS,VATCPREC                                                 
         B     EXITOK                                                           
*                                                                               
VOFC01   OI    FVATRB,FVAPROT+FVAZERO  PROTECT FIELD                            
         OI    FVOIND,FVOXMT           TRANSMIT                                 
         OI    VATINDS,VATCPREC                                                 
         B     EXITOK                                                           
*                                                                               
VOFC02   OI    VATINDS,VATOFREC    OFFICE RECORD                                
         MVC   TAXKOFF,FVIFLD                                                   
         TM    FVIIND,FVIVAL                                                    
         BNZ   VALOFCX             OFFICE ALREADY VALIDATED                     
         CLI   CSACT,A#LST         ACTION = LIST                                
         BE    VALOFCX                                                          
*                                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK          TEST OFFICE SECURITY                         
         MVI   OFFAACT,OFFAVAL     VALIDATE REQUESTED OFFICE                    
         MVC   OFFAOFFC,FVIFLD     OFFICE TO VALIDATE                           
         GOTO1 VOFFAL                                                           
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$OFFID)   INVALID OFFICE FOR THIS ID              
         B     EXITL                                                            
         DROP  R1                                                               
*                                                                               
         MVC   IOKEY,BCSPACES      VALIDATE OFFICE ENTERED                      
         TM    VATINDS,VATNOFFS    NEW OR OLD OFFICE                            
         BZ    VOFC06              OLD                                          
*                                                                               
T        USING OFFRECD,IOKEY       NEW OFFICE SYSTEM                            
                                                                                
         MVI   T.OFFKTYP,OFFKTYPQ  SET UP OFFICE RECORD                         
         MVC   T.OFFKCPY,TAXKCPY                                                
         MVC   T.OFFKOFF,FVIFLD                                                 
         B     VOFC08                                                           
         DROP  T                                                                
*                                                                               
T        USING ACTRECD,IOKEY                                                    
                                                                                
VOFC06   MVC   FVMSGNO,=AL2(AE$FLDTL)  OFFICE TOO LONG                          
         CLI   FVILEN,1            OFFICE MUST BE ONE CHAR.                     
         BNE   EXITL                                                            
         MVC   T.ACTKCPY,TAXKCPY   SET UP ACCOUNT RECORD                        
         MVC   T.ACTKUNT(L'DEPTUL),DEPTUL                                       
         MVC   T.ACTKACT(1),FVIFLD                                              
*                                                                               
VOFC08   MVC   FVMSGNO,=AL2(AE$OFCNF)                                           
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITL               OFFICE DOES NOT EXIST                        
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         MVC   SAVEKEY,IOKEYSAV                                                 
         TM    VATINDS,VATNOFFS    NEW OFFICE SYSTEM ? (NOT SO NEW)             
         BZ    VALOFCX                                                          
         GOTO1 AGETEL,BOPARM,('OFIELQ',AIO5),0  IF NEW SYSTEM                   
         BNE   VALOFCX                                                          
         MVC   FVMSGNO,=AL2(AE$TODAL)  MUST NOT BE AN OFFICE LIST               
         LA    RF,BOELEM               OFFICE INFO ELEMENT                      
         TM    OFISTAT-OFIELD(RF),OFISLIST                                      
         BNZ   EXITL                                                            
*                                                                               
VALOFCX  MVC   FLTIFLD(L'TAXKOFF),FVIFLD                                        
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFC  TM    VATINDS,VATCOFF     OFFICE IS NOT REQUIRED                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'TAXKOFF),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFC  CLC   TAXKOFF,BCSPACES    IS THERE A OFFICE CODE                       
         BE    FLTXX               NO - WE DON'T WANT IT THEN.                  
         CLC   TAXKOFF,EFFS        COMPANY RECORD                               
         BNE   DOFOFC2                                                          
         TM    VATINDS,VATCPREC    IS IT A COMPANY RECORD,                      
         BNZ   FLTXE               YES - WE WANT IT THEN.                       
         B     FLTXX                                                            
DOFOFC2  CLC   TAXKOFF,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* MODIFY COLUMN HEADINGS FOR OFFICE CODE FIELD                        *         
***********************************************************************         
DHDOFC   TM    VATINDS,VATCOFF     OFFICE IS NOT REQUIRED                       
         BNZ   EXITOK                                                           
         L     RE,SVPARMS5         HEADLINE 1                                   
         L     RF,SVPARMS6         HEADLINE 2                                   
         XC    0(6,RE),0(RE)       CLEAR                                        
         XC    0(6,RF),0(RF)                                                    
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* SET OFFICE CODE TAG FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
HEDOFC   TM    VATINDS,VATCOFF     OFFICE IS NOT REQUIRED                       
         BNZ   EXITOK                                                           
         L     RF,SVPARMS6                                                      
         MVI   16(RF),C'A'         SUPPRESS TAG                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROVENCE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
* AIO5 = A(SAVE TAX RECORD)                                           *         
***********************************************************************         
         SPACE 1                                                                
PRVDTA   LA    RF,PRVTBL                                                        
         B     ITER                                                             
*                                                                               
PRVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRV)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISPRV)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETPRV)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPRV)                               
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DOFTPRV)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPRV)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPRV)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETPRV  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROVENCE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISPRV   XC    FVIFLD,FVIFLD       DISPLAY PRV OF RECORD                        
         MVC   SPRVCODE,TAXKPRV                                                 
         MVC   FVIFLD(L'TAXKPRV),TAXKPRV                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PROVENCE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALPRV   XC    TAXKPRV,TAXKPRV                                                  
         CLI   FVILEN,0            ANY PROVENCE?                                
         BE    EXITOK              NO                                           
         LA    RF,PRVTAB                                                        
         MVC   FVMSGNO,=AL2(2025)                                               
         CLC   FVIFLD,BCSPACES                                                  
         BNH   EXITOK              NO INPUT                                     
*                                                                               
VALPRV10 CLI   0(RF),X'FF'         END OF TABLE ?                               
         BE    EXITL                                                            
         CLC   FVIFLD(L'PRVTAB),0(RF)                                           
         BE    VALPRV20                                                         
         LA    RF,L'PRVTAB(,RF)                                                 
         B     VALPRV10                                                         
*                                                                               
VALPRV20 MVC   TAXKPRV,FVIFLD                                                   
         B     EXITOK                                                           
*                                                                               
PRVTAB   DS    0CL2                                                             
         DC    C'BC'               BRITISH COLUMBIA                             
         DC    C'AL'               ALBERTA                                      
         DC    C'SA'               SASKATCHEWAN                                 
         DC    C'MA'               MANITOBA                                     
         DC    C'ON'               ONTARIO                                      
         DC    C'PQ'               QUEBEC                                       
         DC    C'NB'               NEW BRUNSWICK                                
         DC    C'NS'               NOVA SCOTIA                                  
         DC    C'PE'               PRINCE EDWARD ISLAND                         
         DC    C'NF'               NEWFOUNDLAND                                 
         DC    X'FF'               EOT                                          
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROVENCE FILTER FIELD                                               
***********************************************************************         
         SPACE 1                                                                
DFLTPRV  MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'TAXKPRV),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROVENCE FILTER FIELD                                              
***********************************************************************         
         SPACE 1                                                                
VFLTPRV  ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,FLTIFLD,FVIFLD   MOVE IN LIST CODE TO FILTER FIELD            
         OC    FLTIFLD,BCSPACES                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON PROVENCE CODE                                                 
***********************************************************************         
         SPACE 1                                                                
DOFTPRV  OC    TAXKPRV,TAXKPRV   IS THERE A PROVENCE TO COMPARE ON?             
         BZ    FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   TAXKPRV,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EFFECTIVE DATE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
* AIO5 = A(SAVE TAX RECORD)                                           *         
***********************************************************************         
         SPACE 1                                                                
EDTDTA   LA    RF,EDTTBL                                                        
         B     ITER                                                             
*                                                                               
EDTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEDT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEDT)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISEDT)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETEDT)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTEDT)                               
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DFTEDT)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTEDT)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTEDT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETEDT  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EFFECTIVE DATE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISEDT   L     R1,SVPARMS2                                                      
         STCM  R1,3,SVFLDN         SAVE FIELD #                                 
         XC    FVIFLD,FVIFLD       DISPLAY DATE OF RECORD                       
         MVC   STAXDATE,TAXKDATE   TAXKDATE = COMPLEMENT OF DATE                
         XC    STAXDATE,EFFS                                                    
         GOTO1 VDATCON,BODMCB,(1,STAXDATE),(8,FVIFLD)                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EFFECTIVE DATE FIELD                                     *         
* IF ADDING AN OFFICE LEVEL GST RECORD READ FOR COMPANY LEVEL RECORD  *         
* AND KEEP IN AIO5 FOR LATER REFERENCE.  ALSO READ FOR HIGHEST DATE   *         
* AND MOVE THAT DATE INTO KEY DATE FIELD                                        
***********************************************************************         
         SPACE 1                                                                
VALEDT   NI    VATINDS,FF-VATCDFNO                                              
         MVC   AEFFDATE,FVADDR     SAVE A(EFFECTIVE DATE FIELD)                 
         CLI   CSACT,A#ADD         IF ADD/RESTORE                               
         BE    VEDT08              THEN A REAL DATE IS REQUIRED                 
         CLI   CSACT,A#RES                                                      
         BE    VEDT08                                                           
         ZIC   RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BNE   VEDT02                                                           
         CLC   FVIFLD(0),UC@TODAY  TODAY                                        
         MVI   DATTYPE,DATTTDAY                                                 
         MVC   EFFDATE,BCTODAYP                                                 
         B     VEDT09                                                           
*                                                                               
VEDT02   EX    RF,*+8                                                           
         BNE   VEDT04                                                           
         CLC   FVIFLD(0),UC@LAST   LAST                                         
         MVI   DATTYPE,DATTLAST                                                 
         MVC   EFFDATE,EFFS                                                     
         B     VEDT09                                                           
*                                                                               
VEDT04   EX    RF,*+8              'PREVIOUS'/ 'EARLIER' DATE                   
         BE    VEDT06                                                           
         CLC   FVIFLD(0),UC@PRV                                                 
         EX    RF,*+8                                                           
         BNE   VEDT08                                                           
*                                                                               
VEDT06   MVI   DATTYPE,DATTPREV                                                 
         SR    RE,RE                                                            
         ICM   RE,7,EFFDATE                                                     
         BCTR  RE,0                                                             
         STCM  RE,7,EFFDATE                                                     
         B     VEDT09                                                           
*                                                                               
VEDT08   GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1    A REAL DATE                      
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    EXITL                           INVAILD DATE                     
         MVI   DATTYPE,DATTDATE                                                 
         GOTO1 VDATCON,BODMCB,BODUB1,(1,EFFDATE)                                
*                                                                               
VEDT09   MVC   TAXKDATE,EFFDATE    TAXKDATE = COMPLEMENT OF DATE                
         XC    TAXKDATE,EFFS                                                    
*                                                                               
         CLI   CSACT,A#CPY         IF COPY OR ADD A RECORD,                     
         BE    VEDT10              COMPANY RECORD MUST EXIST                    
         CLI   CSACT,A#ADD                                                      
         BE    VEDT10                                                           
         CLI   CSACT,A#RES         IF RESTORE A RECORD,                         
         BE    VALEDTX             DON'T FIND MOST RECENT DATE                  
         B     VEDT13                                                           
*                                                                               
CPY      USING TAXRECD,IOKEY                                                    
VEDT10   TM    VATINDS,VATCPREC         SKIP IF NOT OFFICE TAX RECORD           
         BO    VALEDTX                                                          
         CLI   CSACT,A#CPY              IF ACTION COPY DO NOT SET BIT           
         BE    *+8                      TO DISPLAY COMPANY LVL RECORD           
         OI    VATINDS,VATCDFNO                                                 
         MVC   CPY.TAXKEY,TAXKEY                                                
         XC    CPY.TAXKPRV,CPY.TAXKPRV     CLEAR PROVINCE CODE                  
         MVC   CPY.TAXKOFF,EFFS    MOVE FF'S INTO THE OFFICE                    
         LHI   R1,XOREAD+XOACCDIR+XIO5                                          
         GOTO1 AIO                                                              
         BE    *+14                     COMPANY TAX RECORD MUST EXIST           
         MVC   FVMSGNO,=AL2(AE$CVRDE)                                           
         B     EXITL                                                            
         LHI   R1,XOGET+XOACCMST+XIO5                                           
         GOTO1 AIO                                                              
         BE    VALEDTX                                                          
         DC    H'0'                                                             
         DROP  CPY                                                              
*                                                                               
VEDT13   MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'TAXKEY),TAXKEY                                           
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                 READ FOR MOST RECENT DATE                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   TAXKEY,IOKEY        SAVE NEW KEY                                 
         CLC   TAXKEY(TAXKDATE-TAXKEY),IOKEYSAV                                 
         BE    VEDT14                                                           
*                                                                               
         CLI   DATTYPE,DATTLAST    TEST 'LAST'     ENTERED FOR DATE             
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AE$RCNOF)                                           
         CLI   DATTYPE,DATTTDAY    TEST 'TODAY'    ENTERED FOR DATE             
         BE    VEDT14                                                           
         MVC   FVMSGNO,=AL2(AE$NRICU)                                           
         CLI   DATTYPE,DATTPREV    TEST 'PREVIOUS' ENTERED FOR DATE             
         BNE   *+10                                                             
         MVC   FVMSGNO,=AL2(AE$NPROF)                                           
         CLI   DATTYPE,DATTDATE    TEST A DATE ENTERED FOR DATE                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NRUED)                                           
         B     EXITL                                                            
*                                                                               
VEDT14   MVC   EFFDATE,TAXKDATE    SET EFFDATE TO DATE OF RECORD                
         XC    EFFDATE,EFFS                                                     
*                                                                               
VALEDTX  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EFFECTIVE DATE FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTEDT  OC    FLTIFLD,FLTIFLD                                                  
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BODMCB,(1,FLTIFLD),(8,FVIFLD)                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULTS FOR FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFTEDT   XC    FVIFLD,FVIFLD       DISPLAY TODAY                                
*        GOTO1 VDATCON,BODMCB,(1,BCTODAYP),(8,FVIFLD)                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EFFECTIVE DATE FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
VFLTEDT  ZIC   RF,FVXLEN                                                        
         GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1  EFFECTIVE DATE                     
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         OC    0(4,R1),0(R1)                                                    
         BZ    EXITL               INVAILD DATE                                 
         GOTO1 VDATCON,BODMCB,BODUB1,(1,FLTIFLD)                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR EFFECTIVE DATE                                     *         
***********************************************************************         
         SPACE 1                                                                
DOFTEDT  MVC   STAXDATE,FLTIFLD                                                 
         XC    STAXDATE,EFFS       TAXKDATE = COMPLEMENT DATE                   
         CLC   STAXDATE,TAXKDATE                                                
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR REGISTRATION #                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
* AIO5 = A(SAVE TAX RECORD)                                           *         
***********************************************************************         
         SPACE 1                                                                
TRGDTA   LA    RF,TRGTBL                                                        
         B     ITER                                                             
*                                                                               
TRGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTRG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTRG)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISTRG)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY A REGISTRATION # FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,RF                                                        
DISTRG   XC    FVIFLD,FVIFLD       DISPLAY TRG OF RECORD                        
         SR    R1,R1                                                            
         LA    RF,TAXRFST                                                       
*                                                                               
DISTRG05 CLI   0(RF),0                                                          
         BE    EXITOK                                                           
         CLI   0(RF),FFTELQ        FREE FORM ELEMENT                            
         BNE   DISTRG10                                                         
         CLI   FFTTYPE,FFTTFREE    FREE FORM REGISTRATION                       
         BE    DISTRG20                                                         
DISTRG10 ZIC   R1,1(,RF)                                                        
         AR    RF,R1                                                            
         B     DISTRG05                                                         
*                                                                               
DISTRG20 ZIC   R1,1(,RF)                                                        
         SHI   R1,FFTLN1Q+1                                                     
         BM    EXITOK                                                           
         EX    R1,*+8                                                           
         B     EXITOK                                                           
         MVC   FVIFLD(0),FFTDATA   SEQUENCE NUMBER                              
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE REGISTRATION #                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
VALTRG   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('FFTELQ',TAXRECD),0               
*                                                                               
         CLI   FVILEN,0            ANY INPUT                                    
         BNE   VTRG10                                                           
         CLI   CSACT,A#ADD         IF ADDING AN OFFICE LEVEL REC                
         BNE   EXITOK              THAN GET FROM CO. LEVEL                      
         TM    VATINDS,VATOFREC    ELSE FORCE TO ENTER                          
         BZ    EXITNO                                                           
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FFTELQ',AIO5),0                  
         CLI   12(R1),0                                                         
         BNE   EXITNO              NO REGISTRATION # FORCE TO ENTER             
         L     R4,12(R1)                                                        
         ZIC   R1,FFTDLEN          ACTUAL LENGTH OF TEXT                        
         BCTR  R1,0                                                             
         EXMVC R1,FVIFLD,FFTDATA   FILL IN ON SCREEN                            
         STC   R1,FVXLEN           FILL IN EXECUTED LENGTH                      
         LA    R1,1(R1)                                                         
         STC   R1,FVILEN           NOW FILL IN REAL LENGTH                      
         OI    FVOIND,FVOXMT       TRANSMINT FIELD                              
*                                                                               
VTRG10   LA    R4,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTFREE                                                 
         MVC   FFTDLEN,FVILEN                                                   
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FFTDATA,FVIFLD   R1=EXECUTED LENGTH OF DATA                   
         AHI   R1,2                BUMP UP 1 FOR TRUE LENGTH AND 1 FOR          
         AHI   R1,FFTLN1Q          ELEMENT LENGTH                               
         STC   R1,FFTLN            ADD WITH ELEMENT OVERHEAD                    
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),TAXRECD,BOELEM,0                   
*                                                                               
         TM    VATINDS,VATOFREC    IF ADDING AN OFFICE LEVEL                    
         BZ    EXITOK              RECORD THAN COPY THE AGENCY LEVEL            
         CLI   CSACT,A#ADD         RECORD INTO R2 SINCE ALL THE FIELDS          
         BNE   EXITOK              ARE PROTECTED FOR OFFICE LEVEL RECS          
         BRAS  RE,CPYCOMP                                                       
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFFICE NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NAMDTA   LA    RF,NAMTBL                                                        
         B     ITER                                                             
*                                                                               
NAMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(DHDNAM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   TM    VATINDS,VATCOFF     OFFICE IS NOT REQUIRED                       
         BZ    EXITOK                                                           
         MVC   IOKEY,BCSPACES                                                   
         TM    VATINDS,VATCPREC    COMPANY RECORD                               
         BZ    DNAM02                                                           
T        USING CPYRECD,IOKEY                                                    
         MVC   T.CPYKCPY,CUABIN    READ COMPANY RECORD                          
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                COMPANY RECORD MISSING                       
         B     DISNAMX             DISPLAY COMPANY NAME                         
*                                                                               
DNAM02   TM    VATINDS,VATNOFFS                                                 
         BZ    DNAM04                                                           
T        USING OFFRECD,IOKEY       FOR NEW OFFICE SYSTEM                        
         MVI   T.OFFKTYP,OFFKTYPQ  SET UP OFFICE RECORD                         
         MVC   T.OFFKCPY,TAXKCPY                                                
         MVC   T.OFFKOFF,TAXKOFF                                                
         B     DNAM06                                                           
         DROP  T                                                                
T        USING ACTRECD,IOKEY                                                    
DNAM04   MVC   T.ACTKCPY,TAXKCPY   SET UP ACCOUNT RECORD                        
         MVC   T.ACTKUNT(L'DEPTUL),DEPTUL                                       
         MVC   T.ACTKACT(1),TAXKOFF                                             
         SPACE 1                                                                
         DROP  T                                                                
DNAM06   LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITL               OFFICE DOESN'T EXIST                         
DISNAMX  LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
         L     R1,AIO2             A(OFFICE RECORD)                             
         GOTO1 AGETNAM                                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* MODIFY COLUMN HEADINGS FOR OFFICE NAME FIELD                        *         
***********************************************************************         
DHDNAM   TM    VATINDS,VATCOFF     OFFICE IS NOT REQUIRED                       
         BNZ   EXITOK                                                           
         L     RE,SVPARMS5         HEADLINE 1                                   
         L     RF,SVPARMS6         HEADLINE 2                                   
         XC    0(L'NAMEREC,RE),0(RE)      CLEAR                                 
         XC    0(L'NAMEREC,RF),0(RF)                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAX CODE FIELD                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
* AIO5 = A(COMPANY TAX RECORD)                                        *         
***********************************************************************         
         SPACE 1                                                                
TXCDTA   LA    RF,TXCTBL                                                        
         B     ITER                                                             
*                                                                               
TXCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTXC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTXC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAX CODE                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING SVTXELD,R3                                                       
         USING TAXELD,R4                                                        
DISTXC   DS    0H                                                               
*        L     R1,SVPARMS2                                                      
*        CLM   R1,3,=AL2(VAT#TXCDOS) FOR OUTPUT TAX CODE 'S' FORCE IN           
*        BNE   *+8                   DISPLAY COUNTER OF 8 SO IT STARTS          
*        MVI   DISCNTER,8            FROM BEGINNING OF OUTPUT CODE              
*                                                                               
         XC    ATAXEL,ATAXEL                                                    
         NI    FVATRB,FF-FVAPROT   UNPROTECT FIELD IF COMPANY RECORD            
         TM    VATINDS,VATOFREC                                                 
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT      PROTECT FIELD   IF OFFICE  RECORD            
*                                                                               
         ZIC   R5,DISCNTER         POSITION OF TX ELEMENT IN TX RECORD          
         LR    RE,R5                                                            
         MVI   SVTAXEL,TAXIELQ     INPUT  TYPE                                  
         CLI   DISCNTER,DISMAX                                                  
         BNH   DTXC10                                                           
         SHI   R5,DISMAX           MAX # OF ENTIES OF INPUT/OUTPUT TYPE         
         MVI   SVTAXEL,TAXOELQ     OUTPUT TYPE                                  
*                                                                               
DTXC10   LA    RE,1(,RE)                                                        
         STC   RE,DISCNTER         INCREMENT DISPLAY COUNTER                    
         SR    RE,RE                                                            
         LA    R4,TAXRFST                                                       
*                                                                               
CPY      USING TAXRECD,RF                                                       
         L     RF,AIO5                                                          
         TM    VATINDS,VATCDFNO    DISPLAY COMPANY TAX RECORD                   
         BZ    DTXC20                                                           
         TM    FILINDS1,FILDNTAX   DISPLAY NEW TAX RECORD                       
         BNZ   *+8                                                              
         LA    R4,CPY.TAXRFST      NO - USE COMPANY TAX RECORD THEN.            
*                                                                               
DTXC20   CLI   TAXEL,0                                                          
         BE    EXITOK                                                           
         CLC   TAXEL,SVTAXEL                                                    
         BE    DTXC40                                                           
DTXC30   ZIC   RE,TAXLN                                                         
         AR    R4,RE                                                            
         B     DTXC20                                                           
*                                                                               
DTXC40   BCT   R5,DTXC30           FIND THE RIGHT ELEMENT IN TAX REC            
         ST    R4,ATAXEL           SAVE TAX ELEMENT                             
*                                                                               
         MVC   FVIFLD(L'TAXCODE),TAXCODE   DISPLAY TAX CODE                     
         TM    TAXINDS,TAXIDFLT            DEFAULT INPUT TAX RATE               
         BZ    *+12                                                             
         MVI   FVIFLD+1,C'*'                                                    
         MVI   FVILEN,2                                                         
         B     EXITOK                                                           
         DROP  R3,R4,CPY                                                        
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TAX CODE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING SVTXELD,R4                                                       
VALTXC   CLI   CSACT,A#ADD         IF OFFICE LEVEL RECORD BEING ADDED           
         BNE   *+12                THAN DON'T VALIDATE BECAUSE FIELDS           
         TM    VATINDS,VATOFREC    ARE PROTECTED AND COMP LEVEL RECORD          
         BO    EXITOK              HAS BEEN COPIED.                             
*                                                                               
         NI    FVATRB,X'FF'-FVAPROT                                             
         MVI   SVTAXEL,TAXIELQ     DEFAULT TO INPUT TYPE                        
         L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(VAT#TXCD1I) INPUT TAX CODE?                            
         BE    VTXC05                                                           
         CLM   R1,3,=AL2(VAT#TXCDI) INPUT TAX CODE?                             
         BE    VTXC05                                                           
         MVI   SVTAXEL,TAXOELQ     MOVE IN OUTPUT TAX TYPE                      
         L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(VAT#TXCDOS)  1ST OUTPUT TYPE?                          
         BNE   *+10                                                             
         XC    TAXCTAB(DISMAX),TAXCTAB  IF SO CLEAR TABLE OF CODES              
         CLM   R1,3,=AL2(VAT#TXCDOS) SAVE ADDRESS OF 1ST OUTPUT ADDR            
         BNE   *+10                                                             
         MVC   AOUTADDR,FVADDR                                                  
*                                                                               
VTXC05   CLI   FVILEN,0            ANY INPUT?                                   
         BNE   *+16                YES                                          
         TM    INPINDS,INPFNDCD    NO MAKE SURE WE HAVE AT LEAST ONE            
         BO    VTXCX                                                            
         B     EXITNO                                                           
         L     R4,ACURTXEL         CURRENT SAVE TAX ELEMENT                     
         OI    INPINDS,INPFNDCD    FIND A TAX CODE                              
         MVC   SVTXCDE,SVTAXEL     SAVE TAX ELEMENT CODE                        
         MVI   SVTXLN,TAXLN1Q      SAVE ELEMENT LENGTH                          
         MVC   SVTXCODE,FVIFLD     SAVE TAX CODE                                
         TM    INPINDS,PROTVAL     ARE WE VALIDATING A PROTECTED FIELD?         
         BZ    VTXC10                                                           
         USING FHD,RF              THEN MUST LOOK AT THE FIELD ADDRESS          
         L     RF,FVADDR                                                        
         MVC   SVTXCODE,FHDA                                                    
         CLI   FHDA+1,C' '                                                      
         BE    VTXC50                                                           
         B     VTXC20                                                           
*                                                                               
VTXC10   CLI   FVIFLD+1,C' '                                                    
         BE    VTXC50                                                           
VTXC20   MVC   FVMSGNO,=AL2(AE$FLDTL) CODE HAS LENGTH OF 2 CHARACTERS           
         CLI   SVTAXEL,TAXIELQ                                                  
         BNE   EXITL               ONLY VALID FOR INPUT TYPE                    
         TM    INPINDS,PROTVAL     ARE WE VALIDATING A PROTECTED FIELD?         
         BZ    VTXC30                                                           
         USING FHD,RF              THEN MUST LOOK AT THE FIELD ADDRESS          
         L     RF,FVADDR                                                        
         CLI   FHDA+1,C'*'                                                      
         BNE   EXITL                                                            
         B     VTXC40                                                           
VTXC30   CLI   FVIFLD+1,C'*'       TEST FOR STANDARD INPUT DEFAULT              
         BNE   EXITL                                                            
VTXC40   MVC   FVMSGNO,=AL2(AE$1IVTD)                                           
         TM    INPINDS,INPFNDID    DO WE ALREADY HAVE A DEFAULT?                
         BZ    *+16                NO                                           
         TM    INPINDS,PROTVAL     ARE WE VALIDATING A PROTECTED FIELD?         
         BO    *+8                 YES SO IGNORE                                
         B     EXITL               ELSE ERROR-ONLY 1 DEFAULT ALLOWED            
         OI    INPINDS,INPFNDID                                                 
         OI    SVTXINDS,TAXIDFLT                                                
         B     *+8                                                              
*                                                                               
VTXC50   NI    SVTXINDS,X'FF'-TAXIDFLT                                          
         MVC   FVMSGNO,=AL2(AE$DUPIF) DUPLICATED INPUT FIELD                    
         LA    RF,TAXCTAB          MAKE SURE CODE IS NOT DUPLICATED             
         LA    RE,DISMAX           MAX # OF INPUT(OR OUTPUT) TYPES              
VTXC60   CLI   0(RF),0                                                          
         BE    VTXC70                                                           
         CLC   SVTXCODE,0(RF)                                                   
         BE    EXITL                                                            
         LA    RF,L'SVTXCODE(RF)   NEXT ENTRY                                   
         BCT   RE,VTXC60                                                        
VTXC70   MVC   0(L'SVTXCODE,RF),SVTXCODE  ADD CURRENT CODE TO TABLE             
VTXCX    OI    INPINDS,INPVAL      TAX CODE HAS BEEN VALIDATED                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TAX TYPE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TTYDTA   LA    RF,TTYTBL                                                        
         B     ITER                                                             
*                                                                               
TTYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTTY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTTY)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAX TYPE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISTTY   NI    FVATRB,FF-FVAPROT   UNPROTECT FIELD IF COMPANY RECORD            
         TM    VATINDS,VATOFREC                                                 
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT      PROTECT FIELD   IF OFFICE  RECORD            
         OC    ATAXEL,ATAXEL                                                    
         BZ    EXITOK                                                           
*                                                                               
DISTY10  L     R4,ATAXEL                                                        
         MVC   FVIFLD(L'TAXTYPE),TAXTYPE-TAXELD(R4) DISPLAY TAX TYPE            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TAX TYPE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING SVTXELD,R4                                                       
VALTTY   CLI   CSACT,A#ADD         IF OFFICE LEVEL RECORD BEING ADDED           
         BNE   *+12                THAN DON'T VALIDATE BECAUSE FIELDS           
         TM    VATINDS,VATOFREC    ARE PROTECTED AND COMP LEVEL RECORD          
         BO    EXITOK              HAS BEEN COPIED.                             
*                                                                               
VTTY10   NI    FVATRB,X'FF'-FVAPROT                                             
         L     R4,ACURTXEL         CURRENT SAVE TAX ELEMENT                     
         CLI   SVTXCODE,C' '       IF NOT TAX CODE THAN IGNORE                  
         BNH   EXITOK                                                           
         CLI   FVILEN,0                                                         
         BE    EXITNO              NO INPUT - ERROR                             
         MVC   SVTXTYPE,FVIFLD                                                  
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAX ACCOUNT CODE FIELD                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TACDTA   LA    RF,TACTBL                                                        
         B     ITER                                                             
*                                                                               
TACTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTAC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTAC)                                 
*        DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHTAC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAX ACCOUNT CODE                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TAXELD,R4                                                        
DISTAC   NI    FVATRB,FF-FVAPROT                                                
         TM    VATINDS,VATOFREC                                                 
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT      PROTECT FIELD   IF OFFICE  RECORD            
         OC    ATAXEL,ATAXEL       NO ELEMENT                                   
         BZ    EXITOK                                                           
*                                                                               
DTAC02   DS    0H                                                               
         L     R4,ATAXEL                                                        
         MVC   FVIFLD(L'TAXACTA),TAXACTA    DISPLAY TAX ACCT CODE               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TAX ACCOUNT CODE                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SVTXELD,R4                                                       
VALTAC   CLI   CSACT,A#ADD         IF OFFICE LEVEL RECORD BEING ADDED           
         BNE   *+12                THAN DON'T VALIDATE BECAUSE FIELDS           
         TM    VATINDS,VATOFREC    ARE PROTECTED AND COMP LEVEL RECORD          
         BO    EXITOK              HAS BEEN COPIED.                             
*                                                                               
         NI    FVATRB,X'FF'-FVAPROT                                             
         L     R4,ACURTXEL         CURRENT SAVE TAX ELEMENT                     
         CLI   SVTXCODE,C' '       IF NO TAX CODE THAN IGNORE                   
         BNH   EXITOK                                                           
         CLI   FVILEN,0            ERROR - NO ACCOUNT CODE                      
         BE    EXITNO                                                           
         CLI   FVIFLD,C'?'         1ST CHAR CANNOT BE ?                         
         BE    EXITNV                                                           
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         SHI   RF,1                                                             
         CLM   RF,1,ACTHILEN       WHAT IS THIS                                 
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NLOWA)                                           
         B     EXITL               ACCOUNT MUST BE LOW LEVEL                    
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,SVTXACTA,FVIFLD                                               
         OC    SVTXACTA,BCSPACES                                                
*                                                                               
         LA    RF,ACCDTAB          SEE IF DUPLICATE ACCOUNT CODE                
         LA    R0,MAXTXEL          MAXIMUM ELEMENTS IN A RECORD                 
         MVC   FVMSGNO,=AL2(AE$DUPAC)                                           
VTAC10   OC    0(L'SVTXACTA,RF),0(RF)                                           
         BZ    VTAC20                                                           
         CLC   SVTXACTA,0(RF)                                                   
         BNE   *+8                                                              
         B     EXITL                                                            
         LA    RF,L'SVTXACTA(RF)   NEXT ACCOUNT CODE IN TABLE                   
         BCT   R0,VTAC10                                                        
VTAC20   MVC   0(L'SVTXACTA,RF),SVTXACTA  ADD CURRENT CODE TO TABLE             
*                                                                               
         LA    R5,IOKEY            MAKE SURE HIGHER LEVEL ACCT EXISTS           
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,TAXKCPY                                                  
         MVC   ACTKUNT(L'TAXUL),TAXUL                                           
         XR    RE,RE                                                            
         ICM   RE,1,ACTHILEN                                                    
         BZ    VTAC30              FORGET IT IF NO HIGHER LEVEL ACCOUNT         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FVIFLD                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    VTAC30                                                           
         MVC   FVMSGNO,=AL2(AE$HLACM)                                           
         B     EXITL                                                            
*                                                                               
VTAC30   MVC   ACTKACT,SVTXACTA    SEE IF LOW LEVEL ACCOUNT EXISTS              
         L     R1,=AL4(XIO2+XOACCDIR+XORDUPD)                                   
         GOTO1 AIO                                                              
         BE    VTAC40                                                           
         TM    IOERR,IOEDEL        TEST ACCOUNT IS DELETED                      
         BZ    VALTACX                                                          
         MVC   FVMSGNO,=AL2(AE$RECID)                                           
         B     EXITL                                                            
*                                                                               
VTAC40   L     R1,=AL4(XIO2+XOACCMST+XOGETRUP)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R5,AIO2                                                          
         MVC   ACTKEY,IOKEY                                                     
T        USING RSTELD,BOELEM       ELEMENT ARE CONSISTENT                       
         GOTO1 AGETEL,BOPARM,('RSTELQ',ACTRECD),0                               
         BE    VTAC50                                                           
         GOTO1 AADDRST,ACTRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         B     VTAC70                                                           
VTAC50   CLI   T.RSTLN,RSTLN3Q                                                  
         BNL   VTAC80                                                           
         MVI   T.RSTFILT5,C' '                                                  
         MVI   T.RSTLN,RSTLN3Q     NEW LENGTH                                   
         GOTO1 AREPEL,BOPARM,('RSTELQ',ACTRECD),0,BOELEM                        
*                                                                               
VTAC70   LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                 CHANGE ACCOUNT RECORD                        
*                                                                               
VTAC80   LA    RF,TAXIELQ                                                       
         TM    T.RSTSTAT2,RSTSIVAT                                              
         BNZ   *+8                                                              
         LA    RF,TAXOELQ                                                       
         CLM   RF,1,SVTAXEL                                                     
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$WTOVA)                                           
         B     EXITL                                                            
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
* OVERIDE NAME & RATE WITH ACCOUNT RECORD VALUES                     *          
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-          
         SPACE 2                                                                
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   VTAC90                                                           
         L     R3,12(,R1)                                                       
         USING NAMELD,R3                                                        
         XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         LA    RF,TAXLN1Q+1(,RE)                                                
         STC   RF,SVTXLN            SAVE ELEMENT LENGTH                         
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVTXNAME(0),NAMEREC  SAVE ACCOUNT NAME                           
         DROP  R3                                                               
*                                                                               
VTAC90   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RATEVATQ',ACTRECD),0             
         CLI   12(R1),0                                                         
         BNE   VALTACX                                                          
         L     R3,12(,R1)                                                       
         USING RATELD,R3                                                        
         TM    RATINDS,RATIDC3     IS RATE STORED AS 3DP ?                      
         BNO   *+8                                                              
         OI    SVTXINDS,RATIDC3                                                 
         MVC   SVTXRATE,RATRATE    SAVE RATE                                    
*                                                                               
VALTACX  OI    INPINDS,INPVAL                                                   
         B     EXITOK                                                           
         DROP  R3,R4,R5                                                         
         SPACE 2                                                                
*&&DO                                                                           
***********************************************************************         
* SEARCH ON A ACCOUNT                                                 *         
***********************************************************************         
         SPACE 1                                                                
SRCHTAC  DS    0H                                                               
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,TAXUL,ACOM,    C        
               (X'14',0)                                                        
         B     EXITOK                                                           
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR TAX NAME                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TXNDTA   LA    RF,TXNTBL                                                        
         B     ITER                                                             
*                                                                               
TXNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTXN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTXN)  NO LONGER ADD NAME             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAX NAME                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING TAXELD,R4                                                        
DISTXN   NI    FVATRB,FF-FVAPROT                                                
         TM    VATINDS,VATOFREC                                                 
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT                                                   
         OC    ATAXEL,ATAXEL       NO ELEMENT                                   
         BZ    EXITOK                                                           
*                                                                               
DTXN02   DS    0H                                                               
         L     R4,ATAXEL                                                        
         ZIC   RE,TAXLN                                                         
         SHI   RE,TAXLN1Q+1                                                     
         BM    EXITOK                                                           
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),TAXNAME                                                
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TAX NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING SVTXELD,R4                                                       
VALTXN   CLI   CSACT,A#ADD         IF OFFICE LEVEL RECORD BEING ADDED           
         BNE   *+12                THAN DON'T VALIDATE BECAUSE FIELDS           
         TM    VATINDS,VATOFREC    ARE PROTECTED AND COMP LEVEL RECORD          
         BO    EXITOK              HAS BEEN COPIED.                             
*                                                                               
         NI    FVATRB,X'FF'-FVAPROT                                             
         L     R4,ACURTXEL         CURRENT SAVE TAX ELEMENT                     
         CLI   SVTXCODE,C' '       IF NOT TAX CODE THAN IGNORE                  
         BNH   EXITOK                                                           
         CLI   FVILEN,0            ERROR - NO ACCOUNT NAME                      
         BE    EXITNO                                                           
         CLI   FVIFLD,C'?'         ? NOT VALID TO START A NAME                  
         BE    EXITNV                                                           
         ZIC   RE,FVILEN           CHECK FOR INVALID CHARACTERS                 
         LA    RF,FVIFLD                                                        
VTXN10   CLI   0(RF),C'?'                                                       
         BE    EXITNV                                                           
         LA    RF,1(RF)                                                         
         BCT   RE,VTXN10                                                        
*                                                                               
         ZIC   RE,FVILEN                                                        
         BCTR  RE,0                                                             
         LA    RF,TAXLN1Q+1(RE)    SET TAX ELEMENT LENGTH                       
         STC   RF,SVTXLN                                                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVTXNAME,FVIFLD                                                  
*                                                                               
VALTXNX  B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAX RATE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TXRDTA   LA    RF,TXRTBL                                                        
         B     ITER                                                             
*                                                                               
TXRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTXR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTXR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TAX RATE                                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING TAXELD,R4                                                        
DISTXR   NI    FVATRB,FF-FVAPROT                                                
         TM    VATINDS,VATOFREC                                                 
         BZ    *+8                                                              
         OI    FVATRB,FVAPROT                                                   
         OC    ATAXEL,ATAXEL       NO ELEMENT                                   
         BZ    EXITOK                                                           
*                                                                               
DTXR02   DS    0H                                                               
         L     R4,ATAXEL                                                        
         OC    TAXRATE,TAXRATE     DISPLAY RATE                                 
         BNZ   DTXR04                                                           
         MVC   FVIFLD(L'UC@ZERO),UC@ZERO                                        
         B     EXITOK                                                           
*                                                                               
DTXR04   DS    0H                                                               
         XC    BOFULL1,BOFULL1                                                  
         MVC   BOFULL1+2(L'TAXRATE),TAXRATE MAKE HOB NON -VE FOR CURED          
         TM    TAXINDS,TAXIDC3 IS RATE 3-DECIMAL PLACES ?                       
         BNO   DTXR06                                                           
         CURED (4,BOFULL1),(6,FVIFLD),3,ALIGN=LEFT,DMCB=BODMCB                  
         B      EXITOK                                                          
DTXR06   CURED (4,BOFULL1),(6,FVIFLD),2,ALIGN=LEFT,DMCB=BODMCB                  
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TAX RATE                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING SVTXELD,R4                                                       
VALTXR   CLI   CSACT,A#ADD         IF OFFICE LEVEL RECORD BEING ADDED           
         BNE   *+12                THAN DON'T VALIDATE BECAUSE FIELDS           
         TM    VATINDS,VATOFREC    ARE PROTECTED AND COMP LEVEL RECORD          
         BO    EXITOK              HAS BEEN COPIED.                             
*                                                                               
         NI    FVATRB,FF-FVAPROT                                                
         ZIC   RF,VALCNTER                                                      
         LA    RF,1(,RF)                                                        
         STC   RF,VALCNTER         INCREMENT THE COUNTER                        
*                                                                               
VTXR10   L     R4,ACURTXEL         CURRENT SAVE TAX ELEMENT                     
         CLI   SVTXCODE,C' '       IF NOT TAX CODE THAN IGNORE                  
         BNH   EXITOK                                                           
         CLI   FVILEN,0            ERROR - NO ACCOUNT RATE                      
         BE    EXITNO                                                           
         XC    SVTXRATE,SVTXRATE                                                
         CLC   FVIFLD(L'UC@ZERO),UC@ZERO                                        
         BE    VALTXRX                                                          
         ZIC   RF,FVILEN                                                        
         LA    RE,FVIFLD-1(RF)       ALLOW PERCENTAGE SIGN AT END               
         CLI   0(RE),C'%'                                                       
         BNE   *+6                                                              
         BCTR  RF,0                                                             
         GOTO1 VCASHVAL,BOPARM,(3,FVIFLD),(RF)                                  
         CLI   BOPARM,FF                                                        
         BNE   *+8                                                              
         B     EXITNOTN            NOT A NUMBER                                 
         MVC   FVMSGNO,=AL2(AE$INVVR)                                           
         CLC   4(4,R1),=F'65000'   ENSURE REASONABLE TAX RATE                   
         BH    EXITL                                                            
         MVC   SVTXRATE,BOPARM+6                                                
         OI    SVTXINDS,TAXIDC3    RATE IS 3-DECIMAL PLACES                     
*                                                                               
VALTXRX  LA    R4,SVTXELLN(R4)                                                  
         ST    R4,ACURTXEL          NEXT LINE IN BUFFER                         
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO FIND A(ACCOUNT FILTER) FOR OFFICE                        *         
*                                                                     *         
* NTRY: R1=A(STATUS ELEMENT)                                          *         
* EXIT: RF=A(ACCOUNT FILTER) OR CC=ZERO IF THERE IS NOT ONE           *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTELD,R1                                                        
OFTOFLT  XR    RF,RF                                                            
         TM    VATINDS,VATOFREC     TEST IS AN OFFICE RECORD                    
         BZ    OFTOFLTX                                                         
*                                                                               
         CLI   SLDGOPOS,LDGOFLT1   TEST IS ACCOUNT FILTER 1                     
         BL    OFTOFLTX            (OFFICE NOT ON A FILTER)                     
         BNE   *+12                                                             
         LA    RF,RSTFILT1                                                      
         B     OFTOFLTX                                                         
*                                                                               
         CLI   SLDGOPOS,LDGOFLT2   TEST IS ACCOUNT FILTER 2                     
         BNE   *+12                                                             
         LA    RF,RSTFILT2                                                      
         B     OFTOFLTX                                                         
*                                                                               
         CLI   SLDGOPOS,LDGOFLT3   TEST IS ACCOUNT FILTER 3                     
         BNE   *+12                                                             
         LA    RF,RSTFILT3                                                      
         B     OFTOFLTX                                                         
*                                                                               
         CLI   SLDGOPOS,LDGOFLT4   TEST IS ACCOUNT FILTER 4                     
         BNE   *+12                                                             
         LA    RF,RSTFILT4                                                      
         B     OFTOFLTX                                                         
*                                                                               
         CLI   SLDGOPOS,LDGOFLT4+1 TEST IS ACCOUNT FILTER 5                     
         BNE   *+8                                                              
         LA    RF,RSTFILT5                                                      
*                                                                               
OFTOFLTX LTR   RF,RF                                                            
         BR    RE                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING SSAVD,R2                                                         
NTRSES   LM    R0,R3,SVPARMS                                                    
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
         CLI   SREC,R#VATRU        VAT RULE RECORD                              
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
THIS     USING TAXRECD,R2                                                       
LAST     USING TAXRECD,R3                                                       
                                                                                
LIST     LM    R0,R3,SVPARMS                                                    
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
FTFLST   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST PAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'TAXKEY),THIS.TAXKEY                                      
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING TAXRECD,IOKEY                                                    
NLST     LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(TAXKPRV-TAXRECD),THIS.TAXRECD                              
         BNE   EXITL               NO MORE FOR THIS COMPANY                     
         CLC   T.TAXKOFF,=X'FFFF'    ANY OFFICE?                                
         BE    NLST04              NO DON'T CHECK LIMITED OFFICE                
*                                  ACCESS THAN                                  
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK          TEST OFFICE SECURITY                         
         MVI   OFFAACT,OFFAVAL     VALIDATE REQUESTED OFFICE                    
         MVC   OFFAOFFC,T.TAXKOFF    OFFICE TO VALIDATE                         
         GOTO1 VOFFAL                                                           
         BNE   NLST                                                             
NLST04   MVC   THIS.TAXKEY(L'TAXKEY+L'TAXKSTA+L'TAXKDA),IOKEY                   
         B     EXITOK                                                           
         DROP  R1,T                                                             
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* VALIDATE THAT TAX ACCOUNT DOES NOT CONTAIN CERTAIN CHARACTERS                 
***********************************************************************         
         SPACE 1                                                                
VALCHAR  NTR1  BASE=*,LABEL=*                                                   
         CLI   FVIFLD,C' '         1ST CHAR CANNOT BE A SPACE                   
         BE    EXITL                                                            
         ZIC   RE,FVIFLD                                                        
         ZIC   RF,FVILEN                                                        
VALCH10  CLI   0(RE),C'?'                                                       
         BE    EXITL                                                            
         CLI   0(RE),C'='                                                       
         BE    EXITL                                                            
         BCT   RF,VALCH10                                                       
         B     EXITOK                                                           
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* IF ADDING OFFICE LEVEL RECORDS USE THE COMPANY RECORD IN AIO5 TO              
* TO COPY THE TAX ELEMENTS TO RECORD BEING ADDED (IN R2) SINCE THOSE            
* FIELDS ARE PROTECTED.                                                         
***********************************************************************         
         SPACE 1                                                                
NEW      USING TAXRECD,R3                                                       
CPY      USING TAXRECD,R5                                                       
CPYCOMP  NTR1  BASE=*,LABEL=*                                                   
         LR    R3,R2               R3 POINTS TO RECORD BEING ADDED              
         LA    R3,NEW.TAXRFST                                                   
CPYC10   CLI   0(R3),0             FIND THE RIGHT POINT TO START                
         BE    CPYC20              COPYING THE ELEMENTS                         
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     CPYC10                                                           
*                                                                               
CPYC20   L     R5,AIO5             R5 POINTS TO COMPANY LEVEL RECORD            
         LA    R5,CPY.TAXRFST                                                   
CPYC30   CLI   0(R5),0             DONE                                         
         BE    CPYC50                                                           
         CLI   0(R5),TAXIELQ                                                    
         BE    CPYC40                                                           
         CLI   0(R5),TAXOELQ                                                    
         BE    CPYC40                                                           
CPYCNX   ZIC   R1,1(R5)                                                         
         AR    R5,R1                                                            
         B     CPYC30                                                           
*                                                                               
CPYC40   ZIC   R1,1(R5)            ELEMENT LENGTH                               
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),0(R5)      COPY ELEMENT                                 
         LA    R1,1(R1)            RESTORE LENGTH                               
         AR    R3,R1               BUMP TO NEXT AVAIL SPOT IN NEW REC           
         B     CPYCNX              SEE IF THERE'S MORE TO COPY                  
*                                                                               
CPYC50   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
DISMAX   EQU   7                   MAX INPUT OR OUTPUT TAX ENTRIES              
MAXTXEL  EQU   14                  MAXIMUM TAX ELEMENTS IN TAX RECORD           
TAXPLN   EQU   10                                                               
*                                                                               
EFFS     DC    8XL1'FF'                                                         
DEPTUL   DC    C'2D'               DEPARTMENT UNIT LEDGER                       
TAXUL    DC    C'SG'               TAX UNIT LEDGER                              
*                                                                               
DCLISTU  DS    0D                                                               
         DCDDL AC#LAST,L'UC@LAST,L                                              
         DCDDL AC#PRV,L'UC@PRV,L                                                
         DCDDL AC#ERLR,L'UC@ERLR,L                                              
         DCDDL AC#TODAY,L'UC@TODAY,L                                            
         DCDDL AC#ZERO,L'UC@ZERO,L                                              
         DCDDL AC#INP,L'UC@VTINP,L                                              
         DCDDL AC#OTPT,L'UC@VTOUT,L                                             
DCLISTUX DC    X'00'                                                            
*                                                                               
DCLISTL  DS    0D                                                               
         DCDDL AC#VATIT,L'LC@VATIT,L                                            
         DCDDL AC#VATOT,L'LC@VATOT,L                                            
DCLISTLX DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
RAPPERD  DSECT                                                                  
       ++INCLUDE ACRAPPERD                                                      
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                               *         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
AREGSAV  DS    A                   A(SAVE REGISTER)                             
ATAXCODE DS    A                   A(TAX CODE FIELD)                            
AEFFDATE DS    A                   A(EFFECTIVE DATE FIELD)                      
ATAXEL   DS    A                   A(TAXEL)                                     
ACURTXEL DS    A                   A(CURRENT SAVE TAX ELEMENT)                  
AFIELD   DS    A                   A(FIELD)                                     
AOUTADDR DS    A                   A(1ST OUTPUT CODE FIELD)                     
ELEMDSP  DS    H                   DISPLACEMENT TO ELEMENT                      
         SPACE 1                                                                
COUNT    DS    XL1                                                              
SVTAXEL  DS    CL1                 SAVE TAX ELEMENT CODE                        
SVFLDN   DS    XL2                 SAVED FIELD NUMBER                           
EFFDATE  DS    PL3                 SAVE EFFECTIVE DATE (PWOS)                   
DISCNTER DS    XL1                 DISPLAY  TAX ELEMENT COUNTER                 
VALCNTER DS    XL1                 VALIDATE TAX ELEMENT COUNTER                 
ACTHILEN DS    XL1                 LENGTH OF HIGH LEVEL ACCOUNT                 
SLDGOPOS DS    XL1                 SAVED LDGOPOS                                
TXTYPE   DS    XL1                 TAX TYPE (INPUT OR OUTPUT)                   
STAXDATE DS    PL3                                                              
SPRVCODE DS    CL2                 SAVED PROVENCE                               
         SPACE 1                                                                
FILINDS1 DS    XL1                 * FILE INDICATORS *                          
FILADDAC EQU   X'80'               ADD ACCOUNT RECORED, NOT WRITE               
FILDNTAX EQU   X'20'                                                            
         SPACE 1                                                                
VATINDS  DS    XL1                 * GENERAL INDICATORS *                       
VATCOFF  EQU   X'80'               COMPANY IS ON OFFICES                        
VATNOFFS EQU   X'40'               NEW OFFICE SYSTEM                            
VATCPREC EQU   X'20'               COMPANY RECORD                               
VATOFREC EQU   X'10'               OFFICE RECORD                                
VATCDFNO EQU   X'08'               COMPANY DISPLAY FOR A NEW OFFICE             
VATINPUT EQU   X'02'               INPUT  TYPE                                  
VATOUTPT EQU   X'01'               OUTPUT TYPE                                  
         SPACE 1                                                                
INPINDS  DS    XL1                 * INPUT INDICATORS *                         
INPFNDID EQU   X'80'               FOUND STANDARD INPUT DEFAULT                 
INPFNDCD EQU   X'40'               FOUND AT LEAST ONE CODE INPUT                
INPVAL   EQU   X'20'               TAX CODE FIELD HAS BEEN VALIDATED            
PROTVAL  EQU   X'10'               VALIDATE PROTECTED TAX CODE FIELD            
         SPACE 1                                                                
BIT      DS    XL1                                                              
DSTCODE  EQU   X'80'               DISPLAY COMPANY LEVEL TAX CODE               
DSTYPE   EQU   X'40'               DISPLAY COMPANY LEVEL I/O TYPE               
DSACODE  EQU   X'20'               DISPLAY COMPANY LEVEL ACCT CODE              
DSNAME   EQU   X'10'               DISPLAY COMPANY LEVEL ACCT NAME              
DSRATE   EQU   X'08'               DISPLAY COMPANY LEVEL RATE                   
DISINP   EQU   X'04'               DONE DISPLAYING COMPANY REC(INPUT)           
DISOUT   EQU   X'02'               DONE DISPLAYING COMPANY REC(OUTPUT)          
BIT2     DS    XL1                                                              
OUTCDS   EQU   X'80'               OUTPUT CODE S                                
OUTCDX   EQU   X'40'               OUTPUT CODE X                                
OUTCDZ   EQU   X'20'               OUTPUT CODE Z                                
         SPACE 1                                                                
DATTYPE  DS    XL1                 * TYPE OF INPUT TO DATE *                    
DATTPREV EQU   0                   'PREVIOUS' KEYWORD ENTERED                   
DATTTDAY EQU   1                   'TODAY' KEYWORD ENTERED                      
DATTLAST EQU   2                   'LAST' KEYWORD ENTERED                       
DATTDATE EQU   3                   ACTUAL DATE ENTERED                          
         SPACE 1                                                                
SAVEKEY  DS    XL(L'TAXKEY)                                                     
NAMESAVE DS    CL(NAMLN1Q+L'NAMEREC)                                            
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
*                                                                               
TAXCTAB  DS    7CL(L'TAXCODE)      TABLE OF INPUTTED TAX CODES                  
ACCDTAB  DS    14CL(L'TAXACTA)     TABLE OF INPUTTED ACCOUNT CODES              
         SPACE 1                                                                
DSLISTU  DS    0C                  DICTIONARY EQUATES USED                      
UC@LAST  DS    CL(TAXPLN)                                                       
UC@PRV   DS    CL(TAXPLN)                                                       
UC@ERLR  DS    CL(TAXPLN)                                                       
UC@TODAY DS    CL(TAXPLN)                                                       
UC@ZERO  DS    CL(TAXPLN)                                                       
UC@VTINP DS    CL(TAXPLN)                                                       
UC@VTOUT DS    CL(TAXPLN)                                                       
DSLISTL  DS    0C                                                               
LC@VATIT DS    CL(TAXPLN)                                                       
LC@VATOT DS    CL(TAXPLN)                                                       
         SPACE 2                                                                
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT                                                                  
*--------------------------------------------------------------------*          
*        DSECT                                                       *          
*--------------------------------------------------------------------*          
         SPACE 2                                                                
SVTXELD  DSECT                     ** SAVE TAX ELEMENT **                       
SVTXCDE  DS    CL(L'TAXEL)         ELEMENT CODE                                 
SVTXLN   DS    CL(L'TAXLN)         ELEMENT LENGTH                               
SVTXTYPE DS    CL(L'TAXTYPE)       TAX TYPE                                     
SVTXACTA DS    CL(L'TAXACTA)       TAX ACCOUNT CODE                             
SVTXRATE DS    CL(L'TAXRATE)       TAX RATE                                     
SVTXCODE DS    CL(L'TAXCODE)       TAX CODE                                     
SVTXINDS DS    CL(L'TAXINDS)       TAX INDICATOR                                
SVTXNAME DS    CL(L'TAXNAME)       TAX NAME                                     
SVTXELLN EQU   *-SVTXELD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACFIL1D   12/10/12'                                      
         END                                                                    
