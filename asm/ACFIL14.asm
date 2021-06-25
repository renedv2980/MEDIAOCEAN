*          DATA SET ACFIL14    AT LEVEL 004 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE T62314C,*                                                                
         SPACE 1                                                                
FIL14    TITLE 'APGRULE RECORD'                                                 
         SPACE 2                                                                
FIL14    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL14**,R7,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
         LH    R6,=Y(TWUSER-TWAD)                                               
         AR    R6,RA                                                            
         USING TWUSER,R6                                                        
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
INIT     OI    GCINDS3,GCIROWS                                                  
INIT10   GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLIST                             
         B     EXITOK                                                           
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
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SPLIT ACCOUNT/CONTRA A/C FROM SCREEN TO DSECT FORMAT...  *         
*                                                                     *         
* R2 = A(CURRENT TSAR RECORD)                                         *         
* FVIFLD = ACCOUNT /CONTRA ACCOUNT CODES                              *         
* BOFLAG1 = A (ACCOUNT) OR C (CONTRA A/C)                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
APGSPLIT ST    RE,SAVERE           SAVE REGISTER E                              
         MVI   SPLITERR,C' '       RESET ERROR SW                               
         MVI   SPLITSW,0                                                        
         LA    RF,TLKACC1          ACCOUNT                                      
         CLI   BOFLAG1,C'A'        IS IT AN ACCOUNT FIELD                       
         BE    *+8                                                              
         LA    RF,TLKCON1          CONTRA A/C                                   
         ST    RF,SAVEREG                                                       
         MVI   TOTFIELD,0                                                       
*                                                                               
         LA    R3,FVIFLD                                                        
         LA    R1,ACCFLDLN(R3)     SAVE END OF FIELD ADDR                       
APGS02   LA    RE,15                                                            
APGS04   ZIC   R4,TOTFIELD         KEEP TRACK OF NUMBER OF CHARACTERS           
         AHI   R4,1                VALIDATED                                    
         STC   R4,TOTFIELD                                                      
         CLI   0(R3),C'-'          SPLITTER                                     
         BE    APGS06                                                           
         CLI   0(R3),C'*'          WILD CARD ENTRY?                             
         BNE   *+8                                                              
         OI    SPLITSW,SPLWILD     WILD CARD                                    
         CLI   0(R3),C' '          IF SPACE MUST CHECK IF THIS MEANS            
         BNE   APGS05              FINISHED VALIDATING ACCOUNT OR               
         ZIC   R4,TOTFIELD         IF IT'S PART OF ACCOUNT                      
         ZIC   R5,FVILEN                                                        
         CR    R4,R5                                                            
         BH    APGS10              DONE                                         
APGS05   MVC   0(1,RF),0(R3)       MOVE ONE CHAR                                
         LA    RF,1(RF)            UP REGS BY ONE                               
         LA    R3,1(R3)                                                         
         CR    R1,R3               END OF FIELD                                 
         BE    APGS10                                                           
         BCT   RE,APGS04                                                        
         MVI   SPLITERR,SPLNOSPL   NO SPLITTER                                  
         B     APGSX                                                            
         SPACE 1                                                                
APGS06   TM    SPLITSW,SPLSPLIT    SPLIT                                        
         BZ    APGS08                                                           
         MVI   SPLITERR,SPL2SPL    CAN'T SPLIT TWICE                            
         B     APGSX                                                            
         SPACE 1                                                                
APGS08   OI    SPLITSW,SPLSPLIT                                                 
         L     RF,SAVEREG          GET TO TO FIELD IN DSECT                     
         LA    RF,14(RF)                                                        
         LA    R3,1(R3)            GET PAST SPLITTER                            
         B     APGS02              DO IT ALL AGAIN                              
         SPACE 1                                                                
APGS10   TM    SPLITSW,SPLSPLIT+SPLWILD    IS ACCOUNT WILD AND SPLIT?           
         BNO   APGSX                                                            
         MVI   SPLITERR,SPLMXSPL   YES, THAT'S NOT ALLOWED                      
APGSX    L     RE,SAVERE                                                        
         BR    RE                                                               
         POP   USING                                                            
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
         USING ACTRECD,R2                                                       
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
KFKVAL   MVC   ACTKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVC   ACTKCPY,CUABIN                                                   
         MVI   ACTKUNT,SUPERUNT                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   ACTKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVC   ACTKCPY,CUABIN                                                   
         MVI   ACTKUNT,SUPERUNT                                                 
         MVI   ACTKLDG,X'41'       GO PAST UNIT AND LEDGER RECORDS              
         B     EXITOK                                                           
         SPACE 2                                                                
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
         DC    AL1(RCPY),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD/COPY/RESTORE                     *         
***********************************************************************         
         SPACE 1                                                                
RFADD    MVC   FVADDR,AACCFLD      SET CURSOR TO ACCOUNT FIELD                  
         BAS   RE,VALHIGH          VALIDATE THAT THE HIGHER LVL                 
         BE    *+14                ALREADY EXISTS                               
         MVC   FVMSGNO,=AL2(AE$HLACM)                                           
         B     EXITL                                                            
         CLI   CSACT,A#CPY         COPY                                         
         BNE   RFADDX                                                           
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   RFADD10                                                          
         L     RF,12(,R1)                                                       
         USING RSTELD,RF                                                        
         MVC   RSTBDATE,BCTODAYP   SET TO TODAY                                 
         MVC   RSTTDATE,BCTODAYP                                                
*                                                                               
RFADD10  GOTO1 ADELEL,BOPARM,('ABLELQ',ACTRECD),0                               
         GOTO1 ADELEL,BOPARM,('APOELQ',ACTRECD),0                               
         GOTO1 ADELEL,BOPARM,('ASTELQ',ACTRECD),0                               
         MVC   IOKEY(L'ACTKEY),ACTKEY                                           
         BAS   RE,CHKACLN          TEST IF LOW LEVEL ACCOUNT                    
         BNE   RFADDX              NO DON'T ADD THESE ELEMENTS                  
         OI    GSRECSTA+(ACTKSTAT-ACTKSTA),ACTSABLP                             
         GOTO1 AADDAST,ACTRECD     ASTELD                                       
         GOTO1 AADDBAL,ACTRECD     ABLELD AND APOELD                            
*                                                                               
RFADDX   B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    MVC   FVADDR,AACCFLD      SET CURSOR TO ACCOUNT FIELD                  
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   RFDEL02                                                          
*                                                                               
         L     RF,12(R1)                                                        
         USING RSTELD,RF                                                        
         OC    RSTBDATE,RSTBDATE   OLD RECORDS                                  
         BZ    RFDEL02                                                          
         CLC   RSTTDATE,RSTBDATE   THIS CHECKS FOR SELF-BALANCING DR/CR         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACNVD)                                           
         B     EXITL                                                            
         DROP  RF                                                               
*                                                                               
RFDEL02  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('ABLELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BNE   RFDEL04                                                          
         L     RF,12(R1)                                                        
         USING ABLELD,RF                                                        
         MVC   FVMSGNO,=AL2(AE$BALNZ)                                           
         CP    ABLFRWD,BCPZERO     ALL BALANCES MUST BE ZERO TO DELETE          
         BNE   EXITL                                                            
         CP    ABLDR,BCPZERO                                                    
         BNE   EXITL                                                            
         CP    ABLCR,BCPZERO                                                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
**********************************************************************          
*   IF RECORD HAS NO BALANCE ELEMENT,WE NEED TO READ                 *          
*   THE NEXT RECORD TO SEE IF ITS IN THE SAME FAMILY                 *          
**********************************************************************          
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
RFDEL04  MVC   T.ACTKEY,ACTKEY                                                  
         GOTO1 AGETLDG                                                          
         BNE   EXITL               LEDGER MISSING                               
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4                                                       
*                                                                               
         CLI   LDGTLVA,L'ACTKACT   SINGLE LEVEL LEDGER?                         
         BE    EXITOK                                                           
         SR    R5,R5                                                            
         IC    R5,LDGTLVA                                                       
         LA    RF,T.ACTKACT(R5)    FIRST CHARACTER OF LEVEL B                   
         CLI   0(RF),C' '                                                       
         BE    RFDEL06             LEVEL B IS BLANK THIS IS LEVEL A             
*                                                                               
         IC    R5,LDGTLVB                                                       
         LA    RF,T.ACTKACT(R5)                                                 
         CLI   LDGTLVB,L'ACTKACT                                                
         BE    RFDEL06                                                          
         CLI   0(RF),C' '                                                       
         BE    RFDEL06             LEVEL C IS BLANK THIS IS LEVEL B             
*                                                                               
         IC    R5,LDGTLVC                                                       
         LA    RF,T.ACTKACT(R5)                                                 
         CLI   LDGTLVC,L'ACTKACT                                                
         BE    RFDEL06                                                          
         CLI   0(RF),C' '                                                       
         BE    RFDEL06             LEVEL D IS BLANK THIS IS LEVEL C             
         LA    R5,L'ACTKACT        LOWLEVEL ACCOUNT                             
         LA    RF,T.ACTKACT(R5)                                                 
*                                                                               
RFDEL06  SR    R0,R0                                                            
         IC    R0,0(RF)            READ NEXT UNDELETED RECORD                   
         AHI   R0,1                                                             
         STC   R0,0(RF)                                                         
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         LA    R5,2(R5)            LENGTH FOR COMPARE                           
         EX    R5,*+8                                                           
         BNE   EXITOK              OK TO DELETE                                 
         CLC   T.ACTKEY(0),IOKEYSAV                                             
*                                                                               
         MVC   FVMSGNO,=AL2(AE$NLOWA)    NOT A LOWEST LEVEL ACCOUNT             
         B     EXITL               CAN'T DELETE - LOWER LEVEL EXIST             
         DROP  R4,RF,T                                                          
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
         L     R2,SVPARMS4                                                      
         USING ACTRECD,R2                                                       
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
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(ARU#LDG),AL4(ACLEDG) LEDGER CODE                             
         DC    AL2(ARU#ACCT),AL4(ACCDTA) ACCOUNT CODE                           
         DC    AL2(ARU#GLPA),AL4(ACSDTA) ACCOUNT CODE 1 & 2                     
         DC    AL2(ARU#GLPCA),AL4(CNSDTA) CONTRA-ACCOUNT CODE 1 & 2             
         DC    AL2(ARU#GFLT),AL4(GFTDTA) GLPFLTS FROM TSAR RECORD               
         DC    AL2(ARU#ACTV),AL4(ACVDTA)  ACTION VALUE                          
         DC    AL2(ARU#BUTYP),AL4(BUCDTA) BUCKET TYPE                           
         DC    AL2(EOT)                                                         
*                                                                               
* I ADDED THIS FIELD IN ORDER TO WORKAROUND THE BUG WHEN ADDING RECS            
* WITH GIVING ACC INFO AFTER DISPLAYING AN ACCT.  RECORD GETS ADDED             
* W/O THE DETAILED INFO.  THIS FIELD FORCES USERS TO INDICATE WHETHER           
* THEY ARE ADDING A HIGH LEVEL REC OR A DETAIL REC.  THAN IN UPDLAST            
* I CHECK WHETHER THIS IS Y/N.  IF YES THAN FORCE CURSOR TO 1ST                 
* MAINTENANCE LIST FIELD AND 'MISSING INPUT FIELD' MSG.  THIS WAY THE           
* RECORD DOES NOT GET INADVERTANTLY ADDED WITHOUT PERTAINT INFO.  AFM           
* COORDINATORS DECIDED THAT THIS IS TOO MUCH WORK SINCE THE RECORDS             
* ARE NOT ADDED/CHANGED TOO OFTEN.  KEEP THIS CODE B/C I'M SURE USERS           
* WILL COMPLAIN ABOUT THIS.  IF NEED TO PUT THIS BACK ADD A NEW SCREEN          
* W/ A TEST VERSION OF 'A' AND ADD THE NEW FIELD                                
*                                                                               
*        DC    AL2(ARU#DETL),AL4(DETL)   DETAIL INDICATOR FIELD                 
*                                                                               
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL14    CSECT                                                                  
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
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                                       
***********************************************************************         
         SPACE 1                                                                
         USING RSTELD,RF                                                        
DFDDIS   XC    BOELEM,BOELEM                                                    
         GOTO1 AGETEL,BOPARM,('RSTELQ',ACTRECD),0                               
         BNE   EXITOK                                                           
         LA    RF,BOELEM                                                        
         CLI   RSTLN,RSTLN3Q       ALWAYS DEAL WITH X'30' LONGEST LEN           
         BNL   EXITOK                                                           
         MVI   RSTFILT5,C' '                                                    
         MVI   RSTLN,RSTLN3Q       NEW LENGTH                                   
         GOTO1 AREPEL,BOPARM,('RSTELQ',ACTRECD),0,BOELEM                        
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
T        USING RSTELD,BOELEM                                                    
DFDVAL   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    DFDV01                                                           
         GOTO1 AADDRST,ACTRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL                                                            
*                                                                               
DFDV01   CLI   T.RSTLN,RSTLN3Q     MAKE SURE THE STATUS ELEMENT                 
         BNL   DFDV02              IS CONSISTENT                                
         MVI   T.RSTFILT5,C' '                                                  
         MVI   T.RSTLN,RSTLN3Q     NEW LENGTH                                   
         DROP  T                                                                
         GOTO1 AREPEL,BOPARM,('RSTELQ',ACTRECD),0,BOELEM                        
*                                                                               
DFDV02   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('ASTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    DFDV04                                                           
         BAS   RE,CHKACLN          CHECK IF LOW LEVEL ACCT                      
         BNE   DFDV04                                                           
         GOTO1 AADDAST,ACTRECD     ADD AN ASTEL IF NONE THERE                   
         BNE   EXITL                                                            
*                                                                               
DFDV04   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('APOELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
*                                                                               
         BAS   RE,CHKACLN          CHECK IF LOW LEVEL ACCT                      
         BNE   EXITOK                                                           
         OI    GSRECSTA+(ACTKSTAT-ACTKSTA),ACTSABLP                             
         GOTO1 AADDBAL,ACTRECD     ADD AN APOEL & AN ABLEL                      
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEDGER                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACLEDG   LA    RF,ACLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ACLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLDG)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDLDG)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFVLDG)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTLDG)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEDGER                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISLDG   MVC   FVIFLD(L'ACTKLDG),ACTKLDG                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEDGER                                                     *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
VALLDG   MVC   SVALDG,FVADDR       SAVE ADDRESS OF LEDGER FIELD                 
         CLI   FVILEN,0                                                         
         BE    EXITNO              MISSING INPUT                                
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,BCWORK,FVIFLD                                                 
*                                                                               
         LA    R5,IOKEY                                                         
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVI   T.ACTKUNT,SUPERUNT    ALWAYS UNIT F                              
         MVC   T.ACTKLDG,BCWORK      MOVE IN LEDGER                             
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         MVC   ACTKLDG,FVIFLD                                                   
         GOTO1 AGETEL,BOPARM,('ACLELQ',AIO1),0                                  
         BNE   EXITNV              NO ACCOUNT LENGTHS ELEMENT                   
         USING ACLELD,RF                                                        
         LA    RF,BOELEM                                                        
         MVC   LNLEV1,ACLVALS                                                   
         MVC   LNLEV2,ACLVALS+(L'ACLVALS*1)                                     
         MVC   LNLEV3,ACLVALS+(L'ACLVALS*2)                                     
         MVC   LNLEV4,ACLVALS+(L'ACLVALS*3)                                     
         B     EXITOK                                                           
         DROP   RF,T                                                            
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
         MVC   FLTIFLD(L'ACTKLDG),FVIFLD   FILL INTO FILTER FIELD               
         MVC   ACTKLDG,FVIFLD              AND KEY                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LEDGER                                                       
***********************************************************************         
         SPACE 1                                                                
DOFLTLDG CLI   FLTIFLD,0                                                        
         BE    EXITOK              NOTHING TO FILTER ON                         
         CLC   ACTKLDG,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* TEST NEW OR EXISTING ACCOUNT IS LOW-LEVEL                           *         
***********************************************************************         
         SPACE 1                                                                
CHKACLN  NTR1                                                                   
         LA    R1,L'ACTKACT           12 MAX FOR THE ACCOUNT                    
         LA    RF,ACTKACT+L'ACTKACT-1 POINT TO LAST BYTE                        
         CLI   0(RF),C' '             FIND OUT HOW MANY CHARACTERS              
         BNE   *+10                   THE USER ENTERED                          
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         CLI   LNLEV1,L'ACTKACT    ONE LEVEL LEDGER?                            
         BE    EXITOK                                                           
         ZIC   RF,LNLEV3           IF 4 LEVEL LEDGER MUST CHECK                 
         CLI   LNLEV4,0            3RD LEVEL                                    
         BNE   CHKAC10                                                          
         ZIC   RF,LNLEV2           IF 3 LEVEL LEDGER MUST CHECK                 
         CLI   LNLEV3,0            2ND LEVEL                                    
         BNE   CHKAC10                                                          
         ZIC   RF,LNLEV1           CAN ASSUME A 2 LEVEL LEDGER SO               
CHKAC10  CR    R1,RF               R1 CONTAINS LENGTH USER ENTERED              
         BNH   EXITL                                                            
         B     EXITOK                                                           
*                                                                               
*&&DO                                                                           
         LA    RF,BOELEM           ACCOUNT LENGTHS ELEMENT                      
         USING ACLELD,RF                                                        
         CLI   ACLVLEN,L'ACTKACT   TEST ONE-LEVEL LEDGER                        
         BE    EXITOK                                                           
         LA    RF,ACLVALS+(3*L'ACLVALS)                                         
         USING ACLVALS,RF                                                       
         CLI   ACLVLEN,0           FIND LOWEST USED LEVEL                       
         BH    *+12                                                             
         SH    RF,=Y(L'ACLVALS)                                                 
         B     *-12                                                             
         SH    RF,=Y(L'ACLVALS)    THEN PENULTIMATE LEVEL                       
         CLC   FVILEN,ACLVLEN      L'A/C MUST BE > L'PENULTIMATE LEVEL          
         BNH   EXITL                                                            
         B     EXITOK                                                           
         DROP  RF                                                               
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE THAT THE HIGHER LEVEL EXISTS WHEN ADDING/COPYING ACCOUNT   *         
***********************************************************************         
         SPACE 1                                                                
VALHIGH  NTR1                                                                   
         LA    R1,L'ACTKACT           12 MAX FOR THE ACCOUNT                    
         LA    RF,ACTKACT+L'ACTKACT-1 POINT TO LAST BYTE                        
         CLI   0(RF),C' '             FIND OUT HOW MANY CHARACTERS              
         BNE   *+10                   THE USER ENTERED                          
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
*                                                                               
         CLI   LNLEV1,L'ACTKACT    ONE LEVEL LEDGER?                            
         BE    EXITOK              THAN OKAY TO ADD/COPY                        
         ZIC   RE,LNLEV3                                                        
         LTR   RE,RE                                                            
         BZ    VALHI2                                                           
         CR    R1,RE               DID USER ENTER THE 4TH LVL                   
         BH    VALHI10             YES THAN READ THE 3RD LVL                    
VALHI2   ZIC   RE,LNLEV2                                                        
         CR    R1,RE               DIS USER ENTER THE 3RD LVL                   
         BH    VALHI10             YES THAN READ THE 2ND LVL                    
         ZIC   RE,LNLEV1                                                        
         CR    R1,RE               DID USER ENTER THE 2ND LVL                   
         BH    VALHI10             YES THAN READ THE 1ST LVL                    
         B     EXITOK              MUST HAVE ENTERED 1ST LVL                    
*                                                                               
VALHI10  MVC   SVKEY,IOKEY                                                      
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'ACTKCULA),0(R2)                                          
         LA    R1,IOKEY+3                                                       
         AR    R1,RE               RE CONTAINS DISPL INTO ACCT TO CLEAR         
         LA    RF,L'ACTKACT                                                     
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         EXMVC RF,0(R1),BCSPACES                                                
         L     R1,=AL4(XORDD+XOACCDIR+XIO1)     READ FOR DELETES                
         GOTO1 AIO                                                              
         BE    VALHIE              HIGHER LEVEL EXISTS                          
*                                                                               
VALHIL   CLI   *,FF                                                             
         B     VALHIX                                                           
VALHIE   CR    RB,RB                                                            
VALHIX   MVC   IOKEY(L'SVKEY),SVKEY                                             
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT CODE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACCDTA   LA    RF,ACCTABL                                                       
         B     ITER                                                             
*                                                                               
ACCTABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISACC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALACC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTACC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTACC)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTACC)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISACC)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETACC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHACC)                               
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISACC)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETACC  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN ACCOUNT CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISACC   MVC   FVIFLD(L'ACTKACT),ACTKACT                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON AN ACCOUNT CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHACC  CLI   CSACT,A#LST         FOR ACTION LIST                              
         BE    EXITOK              DON'T SEARCH(MESSES UP FILTERING)            
         GOTO1 VACSRCHC,BOPARM,(3,FVADDR),ATWA,ACTKUNT,ACOM,(X'14',0)           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ACCOUNT CODE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VALACC   MVC   ACTKACT,FVIFLD                                                   
         MVC   AACCFLD,FVADDR      SAVE A(ACCOUNT FIELD)                        
         CLI   CSACT,A#ADD         ADDING THIS ACCOUNT?                         
         BE    EXITOK              THAN DON'T READ FOR IT YET                   
         CLI   CSACT,A#CPY         COPYING THIS ACCOUNT?                        
         BE    EXITOK              THAN DON'T READ FOR IT YET                   
         CLI   CSACT,A#RES         RESTORING THIS ACCOUNT?                      
         BE    EXITOK              THAN DON'T READ FOR IT YET                   
T        USING ACTRECD,RF                                                       
         LA    RF,IOKEY                                                         
         MVC   IOKEY(L'ACTKEY),0(R2)                                            
         DROP  T                                                                
         GOTO1 AGETACT,1           READ ACCOUNT/TEST SECURITY AND               
         BNE   EXITL               READ FOR DELETES                             
         GOTO1 ATSTSEC                                                          
         BNE   EXITL                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN ACCOUNT CODE FILTER FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
DFLTACC  MVC   FVIFLD(L'ACTKACT),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ACCOUNT CODE FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VFLTACC  MVC   ACTKACT,FVIFLD                                                   
         MVC   FLTIFLD(L'ACTKACT),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON ACCOUNT CODE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DOFTACC  CLC   ACTKACT,BCSPACES    IS THERE AN ACCOUNT TO COMPARE ON?           
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   ACTKACT,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* DATA OBJECT FOR DETAIL FIELD                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DETL     LA    RF,DETLABL                                                       
         B     ITER                                                             
*                                                                               
DETLABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDTL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDTL)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY YES OR NO DETAIL INDICATOR FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DISDTL   MVC   FVIFLD(L'AC@NO),AC@NO       DEFAULT TO NO                        
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('GLPELQ',ACTRECD),0               
         CLI   12(R1),0                    ANY 15 ELEMENTS?                     
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE YES OR NO DETAIL INDICATOR FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
VALDTL   NI    BIT,X'FF'-ACCDTL                                                 
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,AC@NO                                                  
         BNE   VALDTL10                                                         
         BAS   RE,CHKLIST                IF 'NO' THEN SHOULD NOT BE ANY         
         BL    EXITOK                    ACCT INFO IN LIST                      
         SR    R1,R1                                                            
         LH    R1,LS1STLIN                                                      
         A     R1,ATWA            R1 POINTS TO FIRST OFFICE FIELD               
         ST    R1,BOCURSOR                                                      
         B     EXITNV                                                           
*                                                                               
VALDTL10 EXCLC R1,FVIFLD,AC@YES                                                 
         BNE   EXITNV                                                           
         OI    BIT,ACCDTL                                                       
         CLI   CSACT,A#ADD           IF DETL MAKE SURE AT LEAST ONE             
         BE    EXITOK                ENTRY HAS BEEN ENTERED BUT DON'T           
         BAS   RE,CHKLIST            BOTHER WHEN ADDING (UPDLAST WILL           
         BE    EXITOK                HANDLE IT.)                                
         SR    R1,R1                                                            
         LH    R1,LS1STLIN                                                      
         A     R1,ATWA            R1 POINTS TO FIRST OFFICE FIELD               
         ST    R1,BOCURSOR                                                      
         B     EXITNO                                                           
         SPACE 2                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR ACCOUNT 1 & 2                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACSDTA   LA    RF,ACSTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
ACSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISACS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALACS)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDACS)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFVACS)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTACS)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCOUNT 1 & 2 CODES                                         *         
***********************************************************************         
         SPACE 1                                                                
DISACS   CLC   TLKACC1,EFFS         IS THIS THE DUMMY ENTRY?                    
         BE    EXITOK              DON'T SHOW                                   
         MVC   BOWORK1,BCSPACES                                                 
         LA    R4,BOWORK1                                                       
         MVC   0(L'TLKACC1,R4),TLKACC1                                          
         LA    R4,L'TLKACC1(R4)                                                 
         CLI   0(R4),C' '          SKIP ALL SPACES                              
         BH    *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         CLC   TLKACC2,BCSPACES                                                 
         BNH   DISACSX                                                          
         MVI   1(R4),C'-'                                                       
         MVC   2(L'TLKACC2,R4),TLKACC2                                          
                                                                                
DISACSX  MVC   FVIFLD,BOWORK1                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCOUNT 1 & 2 CODES                                        *         
***********************************************************************         
         SPACE 1                                                                
VALACS   MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   VACS02                                                           
         OI    LSLNIND1,LSLNIDEL                                                
         B     EXITOK                                                           
*                                                                               
VACS02   CLC   TLKACC1,EFFS                                                     
         BE    *+14                                                             
         OC    TLKACC1,TLKACC1     IF ACCT ALREADY THERE MUST BE                
         BNZ   VACS04              CHANGING SO DON'T TREAT AS NEW ENTRY         
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1             FIRST LIST REC #                          
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
         CHI   RE,20               20 ENTRIES MAX                               
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MAX#)                                            
         B     EXITL                                                            
*                                                                               
VACS04   MVC   TLKACC1(L'TLKACC1+L'TLKACC2),BCSPACES                            
         MVI   BOFLAG1,C'A'        ACCOUNT                                      
         BAS   RE,APGSPLIT                                                      
         CLI   SPLITERR,SPLNOSPL   INVALID INPUT FIELD                          
         BE    EXITNV                                                           
         CLI   SPLITERR,SPL2SPL    SPLIT ACCOUNT MORE THAN ONCE                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NOSA2)                                           
         B     EXITL                                                            
         CLI   SPLITERR,SPLMXSPL   SPLIT AC WITH WILD CARDS - CHAR '*'          
         BNE   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$NOSAW)                                           
         B     EXITL                                                            
*                                                                               
* NEEDED TO REMOVE THIS VALIDATION CODE BECAUSE OF THE WAY 13,14,15,16          
* ACCTS ARE ENTERED AND BECAUSE USERS CAN ENTER BILLING SOURCES W/O             
* ANY U/L                                                                       
*&&DO                                                                           
         CLC   TLKACC1,BCSPACES    ANY ACCOUNT 1                                
         BNH   EXITOK                                                           
         LA    RF,TLKACC1                                                       
         GOTO1 =A(VACCTS),BOPARM,(RF),RR=BORELO                                 
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     EXITL                                                            
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE ACCOUNT AS A FILTER                                               
***********************************************************************         
         SPACE 1                                                                
DFDACS   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FVIFLD,FLTIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE ACCOUNT AS A FILTER                                              
***********************************************************************         
         SPACE 1                                                                
DFVACS   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FLTIFLD,FVIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR ACCOUNT                                                      
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
DOFLTACS CLI   FLTIFLD,0                                                        
         BE    EXITOK              NOTHING TO FILTERON                          
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,ACTKACT,FLTIFLD                                               
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CONTRA-ACCOUNT 1 & 2                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CNSDTA   LA    RF,CNSTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
CNSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCNS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCNS)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CONTRA-ACCOUNT 1 CODE                                       *         
***********************************************************************         
         SPACE 1                                                                
DISCNS   CLC   TLKACC1,EFFS        IS THIS THE DUMMY ENTRY?                     
         BE    EXITOK              DON'T SHOW                                   
         MVC   BOWORK1,BCSPACES                                                 
         LA    R4,BOWORK1                                                       
         MVC   0(L'TLKCON1,R4),TLKCON1                                          
         LA    R4,L'TLKCON1(R4)                                                 
         CLI   0(R4),C' '          SKIP ALL SPACES                              
         BH    *+10                                                             
         BCTR  R4,0                                                             
         B     *-10                                                             
         CLC   TLKCON2,BCSPACES                                                 
         BNH   DISCNSX                                                          
         MVI   1(R4),C'-'                                                       
         MVC   2(L'TLKCON2,R4),TLKCON2                                          
                                                                                
DISCNSX  MVC   FVIFLD,BOWORK1                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CONTRA-ACCOUNT 1 CODE                                      *         
***********************************************************************         
         SPACE 1                                                                
VALCNS   MVC   TLKCON1(L'TLKCON1+L'TLKCON2),BCSPACES                            
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
* IF THIS IS THE 1ST LINE ADDED AND YOU DIDN'T ENTER A GIVING ACCT              
* THAN IGNORE THIS FIELD                                                        
         CLC   TLKACC1,EFFS                                                     
         BE    EXITOK                                                           
         MVI   BOFLAG1,C'C'        CONTRA A/C                                   
         BAS   RE,APGSPLIT                                                      
         CLI   SPLITERR,SPLNOSPL   INVALID INPUT FIELD                          
         BE    EXITNV                                                           
         CLI   SPLITERR,SPL2SPL    SPLIT ACCOUNT MORE THAN ONCE                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NOSA2)                                           
         B     EXITL                                                            
         CLI   SPLITERR,SPLMXSPL   SPLIT AC WITH WILD CARDS - CHAR '*'          
         BNE   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$NOSAW)                                           
         B     EXITL                                                            
*                                                                               
* NEEDED TO REMOVE THIS VALIDATION CODE BECAUSE OF THE WAY 13,14,15,16          
* ACCTS ARE ENTERED AND BECAUSE USERS CAN ENTER BILLING SOURCES W/O             
* ANY U/L                                                                       
*                                                                               
*&&DO                                                                           
VALCNS5  CLC   TLKACC1,BCSPACES    ANY ACCOUNT 1                                
         BNH   EXITOK                                                           
         LA    RF,TLKCON1                                                       
         GOTO1 =A(VACCTS),BOPARM,(RF),RR=BORELO                                 
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     EXITL                                                            
*&&                                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR GLPFLTS                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GFTDTA   LA    RF,GFTTABL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
GFTTABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISGFT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGFT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY GLPFLTS VALUE FROM TSAR RECORD                              *         
***********************************************************************         
         SPACE 1                                                                
DISGFT   CLC   TLKACC1,EFFS       IS THIS THE DUMMY ENTRY?                      
         BE    EXITOK              DON'T SHOW                                   
         MVC   FVIFLD(L'TLKFLTS),TLKFLTS                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE GLPFLTS VALUE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALGFT   MVC   TLKFLTS,BCSPACES                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
* IF THIS IS THE 1ST LINE ADDED AND YOU DIDN'T ENTER A GIVING ACCT              
* THAN IGNORE THIS FIELD                                                        
         CLC   TLKACC1,EFFS                                                     
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$XACCP)                                           
         CLC   TLKACC1,BCSPACES    MUST HAVE A FIRST ACCOUNT                    
         BNH   EXITL                                                            
         MVC   TLKFLTS,FVIFLD                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTION VALUE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ACVDTA   LA    RF,ACVTABL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
ACVTABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISACV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALACV)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION VALUE FROM TSAR RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
DISACV   CLC   TLKACC1,EFFS        IS THIS THE DUMMY ENTRY?                     
         BE    EXITOK              DON'T SHOW                                   
         MVC   FVIFLD(L'TLKACTN),TLKACTN                                        
         CLI   TLKACTN,0           IF NO ACTION THAN SHOW THE DEFAULT           
         BNE   EXITOK              OF +                                         
         MVI   FVIFLD,C'+'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE ACTION VALUE AND SAVE ON TSAR RECORD                       *         
***********************************************************************         
         SPACE 1                                                                
VALACV   MVI   TLKACTN,X'00'                                                    
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
* IF THIS IS THE 1ST LINE ADDED AND YOU DIDN'T ENTER A GIVING ACCT              
* THAN IGNORE THIS FIELD                                                        
         CLC   TLKACC1,EFFS                                                     
         BE    EXITOK                                                           
         CLI   FVIFLD,C'+'         + IS THE DEFAULT SO DON'T ADD TO REC         
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$XACCP)                                           
         CLC   TLKACC1,BCSPACES    MUST HAVE A FIRST ACCOUNT                    
         BNH   EXITNV                                                           
         CLI   FVIFLD,C'-'         MINUS OK                                     
         BE    VALACVX                                                          
         CLC   TLKACC2,BCSPACES    ARE THERE 2 ACCOUNTS                         
         BNH   EXITNV                                                           
         CLI   FVIFLD,C'/'         IS IT DIVISION?                              
         BE    VALACVX                                                          
         CLI   FVIFLD,C'%'         IS IT A PERCENT?                             
         BNE   EXITNV              NO SO NOT VAILD SIGN                         
*                                                                               
VALACVX  MVC   TLKACTN,FVIFLD                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BUCKET TYPE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BUCDTA   LA    RF,BUCTABL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
BUCTABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISBUC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBUC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BUCKET TYPE FROM TSAR RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
DISBUC   CLC   TLKACC1,EFFS         IS THIS THE DUMMY ENTRY?                    
         BE    EXITOK              DON'T SHOW                                   
         CLI   TLKBTYP,C'1'        SHOW 1 OR S FOR SALARY                       
         BE    DBUC10                                                           
         CLI   TLKBTYP,C'S'                                                     
         BE    DBUC10                                                           
         CLI   TLKBTYP,C'2'        SHOW 2 OR P FOR PENSION                      
         BE    DBUC20                                                           
         CLI   TLKBTYP,C'P'                                                     
         BE    DBUC20                                                           
         CLI   TLKBTYP,C'3'        SHOW 3 OR B FOR BENEFIT                      
         BE    DBUC30                                                           
         CLI   TLKBTYP,C'B'                                                     
         BE    DBUC30              FOR ANYTHING ELSE DON'T SHOW B/C             
         B     EXITOK              IT'S NOT VALID AND NOT HONORED               
*                                                                               
DBUC10   MVI   FVIFLD,C'S'                                                      
         B     EXITOK                                                           
DBUC20   MVI   FVIFLD,C'P'                                                      
         B     EXITOK                                                           
DBUC30   MVI   FVIFLD,C'B'                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BUCKET TYPE AND SAVE ON TSAR RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
VALBUC   MVI   TLKBTYP,C' '                                                     
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
* IF THIS IS THE 1ST LINE ADDED AND YOU DIDN'T ENTER A GIVING ACCT              
* THAN IGNORE THIS FIELD                                                        
         CLC   TLKACC1,EFFS                                                     
         BE    EXITOK                                                           
         CLI   FVIFLD,C'1'         VALID ENTRIES ARE 1,2,3                      
         BE    VBUC50                                                           
         CLI   FVIFLD,C'S'                                                      
         BE    VBUC50                                                           
         CLI   FVIFLD,C'2'                                                      
         BE    VBUC55                                                           
         CLI   FVIFLD,C'P'                                                      
         BE    VBUC55                                                           
         CLI   FVIFLD,C'3'                                                      
         BE    VBUC60                                                           
         CLI   FVIFLD,C'B'                                                      
         BE    VBUC60                                                           
         BNE   EXITNV                                                           
*                                                                               
VBUC50   MVI   TLKBTYP,C'1'                                                     
         B     VBUC75                                                           
VBUC55   MVI   TLKBTYP,C'2'                                                     
         B     VBUC75                                                           
VBUC60   MVI   TLKBTYP,C'3'                                                     
*                                                                               
VBUC75   CLC   =C'1C',TLKACC1      THIS FIELD IS ONLY VALID WHEN THE            
         BE    *+14                GIVING ACCOUNT IS 1C OR 1R                   
         CLC   =C'1R',TLKACC1                                                   
         BNE   EXITNV                                                           
*                                                                               
*        CLC   TLKACC1,BCSPACES    MUST HAVE A FIRST ACCOUNT                    
*        BH    *+14                                                             
*        MVC   FVMSGNO,=AL2(AE$XACCP)                                           
*        B     EXITL                                                            
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
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
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   EXITOK                                                           
         CLI   SREC,R#ARUL         APG RULES RECORD                             
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   SACT,A#LST          PAGE DISPLAY SCREEN FROM LIST SCREEN         
         BNE   EXITH                                                            
         NI    SNINDS1,FF-SNIUSECR   TURN OFF USE CURRENT RECORD BIT            
         MVI   LSLTIND1,0          TURN OFF LIST INDICATORS                     
         OI    LSSCIND1,LSSCIBLD   AND REBUILD LIST                             
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
*LIST OBJECT                                                         *          
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
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
X        USING ACTRECD,IOKEY                                                    
FLST     MVC   X.ACTKEY,THIS.ACTKEY                                             
         L     R1,=AL4(XOHID+XOACCDIR+XIO1)                                     
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         BL    EXITL               HARD I/O ERROR                               
         TM    IOERR,IOEDEL        IF ERROR IS THAT REC IS DELETED              
         BO    NLST                READ NEXT INSTEAD OF EXIT                    
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     L     R1,=AL4(XOHID+XOACCDIR+XIO1)                                     
         XR    RF,RF                                                            
         IC    RF,X.ACTKACT+L'ACTKACT-1                                         
         LA    RF,1(RF)                                                         
         STC   RF,X.ACTKACT+L'ACTKACT-1                                         
         GOTO1 AIO                                                              
*                                                                               
NLST02   CLC   X.ACTKCPY,THIS.ACTKCPY                                           
         BNE   EXITL               DIFFERENT COMPANY - IGNORE IT                
         CLI   X.ACTKUNT,SUPERUNT                                               
         BNE   EXITL               MUST BE SUPERLEDGER                          
         CLI   X.ACTKLDG,C' '                                                   
         BNH   NLST                MUST HAVE LEDGER                             
         CLC   X.ACTKACT,BCSPACES                                               
         BE    NLST                MUST BE ACCOUNT                              
*                                                                               
         GOTO1 AGETACT,0           TEST SECURITY                                
         MVC   FVXTRA,BCSPACES     CLEAR OUT ANY MSG'S FROM GETACT              
         BNE   NLST                                                             
         MVC   THIS.ACTKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  THIS,LAST,X                                                      
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT2,LSSADD                                                   
         OI    LSSTAT1,LSSTSAR                                                  
         MVC   LSCOLLIN,=AL2(240)                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,ACTRFST-ACTRECD                                               
         STH   RF,RECDSP                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    RF,RECDSP           CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         C     RF,AIOREC           MAKE SURE RECDSP INITIALISED                 
         BH    *+8                                                              
         LA    RF,ACTRFST-ACTRECD(RF) IT IS NOW.                                
         XR    RE,RE                                                            
*                                                                               
         USING GLPELD,RF                                                        
FML02    CLI   GLPEL,0             RECORD END?                                  
         BNE   FML03               YES                                          
         MVC   RECDSP,EFFS         MOVE FF'S INTO RECDSP SO WE ARE DONE         
         B     EXITOK                                                           
*                                                                               
FML03    CLI   GLPEL,GLPELQ        ACTEL?                                       
         BE    FML04                                                            
         ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     FML02                                                            
                                                                                
FML04    S     RF,AIOREC                                                        
         STH   RF,RECDSP                                                        
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
NLST1    CLC   RECDSP,EFFS         IF FF'S THAN DONE                            
         BE    EXITL                                                            
         LH    RF,RECDSP           CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         XR    RE,RE                                                            
         C     RF,AIOREC           MAKE SURE RECDSP INITIALISED                 
         BH    NML04                                                            
         LA    RF,ACTRFST-ACTRECD(RF) IT IS NOW.                                
*                                                                               
         USING GLPELD,RF                                                        
NML02    CLI   GLPEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   GLPEL,GLPELQ        ACTEL?                                       
         BE    NML06               YES                                          
                                                                                
NML04    IC    RE,GLPLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML06    S     RF,AIOREC                                                        
         STH   RF,RECDSP                                                        
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         LH    RF,RECDSP                                                        
         A     RF,AIOREC                                                        
         USING GLPELD,RF           MOVE IN DETAILS FROM ELEMENT                 
*                                                                               
         CLC   RECDSP,EFFS         IF RECDSP IS EFFS                            
         BNE   TSARF05             THEN CREATE A DUMMY TSAR ENTRY               
         MVC   TLKACC1,EFFS        SO WILL GET CALLED W/ VALIDATE RTNS          
         OI    LSLTIND1,LSLTIFVL   FORCE VAL OF LIST LINES                      
         B     EXITOK                                                           
*                                                                               
TSARF05  MVC   TLKFLTS,GLPFLTS     FILTERS                                      
         MVC   TLKBTYP,GLPBTYP     BUCKET TYPE                                  
         MVC   TLKACTN,GLPACTN     ACTION                                       
         MVC   TLKACC1,GLPACC1     ACCOUNT 1                                    
         MVC   TLKACC2,BCSPACES                                                 
         MVC   TLKCON1,BCSPACES                                                 
         MVC   TLKCON2,BCSPACES                                                 
         CLI   GLPLN,GLPLN1Q                                                    
         BNH   EXITOK                                                           
         MVC   TLKACC2,GLPACC2     ACCOUNT 2                                    
         CLI   GLPLN,GLPLN2Q                                                    
         BNH   EXITOK                                                           
         MVC   TLKCON1,GLPCON1     CONTRA 1                                     
         CLI   GLPLN,GLPLN3Q                                                    
         BNH   EXITOK                                                           
         MVC   TLKCON2,GLPCON2     CONTRA 2                                     
         B     EXITOK                                                           
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BNE   EXITOK              A MAIN ACTION OF CHANGE                      
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('GLPELQ',AIOREC),0                
         B     EXITOK              DELETE ALL PREVIOUS GLPELS                   
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
                                                                                
         LM    R2,R3,SVPARMS3                                                   
         USING GLPELD,BOELEM       MOVE IN DETAILS TO ELEMENT                   
         CLC   TLKACC1,EFFS        IS THIS THE DUMMY TSAR REC?                  
         BE    EXITOK                                                           
         MVC   BOELEM,BCSPACES                                                  
         MVI   GLPEL,GLPELQ                                                     
         MVI   GLPLN,GLPLN1Q                                                    
         MVC   GLPFLTS,TLKFLTS     FILTERS                                      
         OC    GLPFLTS,BCSPACES                                                 
         MVC   GLPBTYP,TLKBTYP     BUCKET TYPE                                  
         OC    GLPBTYP,BCSPACES                                                 
         MVC   GLPACTN,TLKACTN     ACTION                                       
         MVC   GLPACC1,TLKACC1     ACCOUNT 1                                    
         MVC   GLPACC2,BCSPACES                                                 
         MVC   GLPCON1,BCSPACES                                                 
         MVC   GLPCON2,BCSPACES                                                 
*                                                                               
         CLC   TLKACC2,BCSPACES    ACCOUNT 2 ENTERED?                           
         BE    UPD102              NO                                           
         MVI   GLPLN,GLPLN2Q                                                    
         MVC   GLPACC2,TLKACC2                                                  
*                                                                               
UPD102   CLC   TLKCON1,BCSPACES    CONTRA 1 ENTERED?                            
         BE    UPD104              NO                                           
         OC    TLKCON1,TLKCON1                                                  
         BE    UPD104                                                           
         MVI   GLPLN,GLPLN3Q                                                    
         MVC   GLPCON1,TLKCON1                                                  
*                                                                               
UPD104   CLC   TLKCON2,BCSPACES    CONTRA 2 ENTERED?                            
         BE    UPD106              NO                                           
         OC    TLKCON2,TLKCON2                                                  
         BE    UPD106                                                           
         MVI   GLPLN,GLPLN4Q                                                    
         MVC   GLPCON2,TLKCON2                                                  
*                                                                               
UPD106   GOTO1 AADDEL,BOPARM,AIOREC                                             
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
         DROP  R2,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 DS    0H                                                               
         B     EXITOK                                                           
*                                                                               
* ONLY HAD THIS ROUTINE TO CHECK FOR DETAIL TO GET AROUND THE BUG WHEN          
* ADDING RECORDS AND GIVING ACC INFO IS IGNORED.  DECIDED TO LIVE W/            
* THE ISSUE RATHER THAN HAVE A WORKAROUND.  KEEP THIS CODE B/C I'M SURE         
* USERS WILL COMPLAIN                                                           
*                                                                               
         TM    BIT,ACCDTL          DETAIL RECORD?                               
         BZ    EXITOK              NO                                           
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('GLPELQ',AIOREC),0                
         CLI   12(R1),0                    ANY 15 ELEMENTS?                     
         BE    EXITOK                                                           
         LH    R1,LS1STLIN                                                      
         A     R1,ATWA                                                          
         ST    R1,BOCURSOR                                                      
         B     EXITNO                                                           
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* MAKE SURE FOR DETAIL RECS THAT AT LEAST ONE ENTRY HAS BEEN ENTERED  *         
* ON EXIT - EQUAL MEANS AN ENTRY FOUND                                          
*           LOW MEANS NO ENTRY FOUND ON FIRST PAGE OF SCREEN                    
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R1                                                           
CHKLIST  NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         LH    R1,LS1STLIN                                                      
         A     R1,ATWA            R1 POINTS TO FIRST OFFICE FIELD               
         LA    RF,MAXOPAGE        RF CONTAINS MAX NUMBER OF OFFICES             
CHKLI10  OC    FHDA(L'GLPACC1),FHDA   ANY GIVING ACCOUNT?                       
         BNZ   CHKLIE                 YES SO EQUAL                              
         LHI   R3,5               NEED TO BUMP PAST NEXT 5 FIELDS               
CHKLI15  ZIC   RE,FHLN            TO GET TO NEXT GIVING ACCOUNT                 
         AR    R1,RE                                                            
         BCT   R3,CHKLI15                                                       
         BCT   RF,CHKLI10                                                       
*                                                                               
CHKLIL   CLI   *,FF                                                             
         B     CHKLIX                                                           
CHKLIE   CR    RB,RB                                                            
CHKLIX   XIT1                                                                   
*&&                                                                             
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* VALIDATE THE ACCOUNTS/CONTRA ACCOUNTS ENTERED                                 
* NTRY - ACCOUNT/CONTRA ACCOUNT PASSED IN R1                                    
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
VACCTS   NMOD1 0,*VACCTS*                                                       
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         L     RF,0(R1)            POINT TO ACCOUNT PASSED                      
VACC05   NI    BIT,FF-NOCOMMA                                                   
         ST    RF,SVADDR                                                        
         SR    RE,RE                                                            
         LA    R1,L'TLKACC1        MAX LENGTH TO COMPARE TO                     
VACC10   AHI   RE,1                KEEP COUNTER OF LEN OF ACCOUNT               
         CLI   0(RF),C','          CHECK FOR , TO DETERMINE LENGTH              
         BE    VACC20              TO VALIDATE                                  
         LA    RF,1(RF)                                                         
         BCT   R1,VACC10                                                        
         OI    BIT,NOCOMMA                                                      
         B     VACC30                                                           
*                                                                               
VACC20   DS    0H                                                               
         BCTR  RE,0                                                             
         LA    RF,1(RF)                                                         
         ST    RF,SVADDR2          KEEP POINTER TO NEXT ACCOUNT(IF ANY)         
*                                                                               
VACC30   L     RF,SVADDR           NOW CHECK FOR WILDCARD WITH                  
         LR    R1,RE               VALIDATION LENGTH                            
         SR    RE,RE                                                            
VACC40   AHI   RE,1                                                             
         CLI   0(RF),C'*'                                                       
         BNE   *+10                                                             
         BCTR  RE,0                IF WILDCARD DON'T COUNT THE '*' IN           
         B     VACC100             THE VALIDATION                               
         LA    RF,1(RF)                                                         
         BCT   R1,VACC40                                                        
*                                                                               
T        USING ACTRECD,R3                                                       
VACC100  LA    R3,IOKEY                                                         
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         L     RF,SVADDR                                                        
         BCTR  RE,0                                                             
         EXMVC RE,T.ACTKUNT,0(RF)                                               
         GOTO1 AGETACT,0           READ LEDGER/TEST SECURITY                    
         BNE   VACCTSL                                                          
         TM    BIT,NOCOMMA         IF NO COMMA ENTERED THAN FINISHED            
         BO    VACCTSE             VALIDATING                                   
         L     RF,SVADDR2                                                       
         B     VACC05                                                           
*                                                                               
VACCTSL  CLI   *,FF                                                             
         B     VACCTSX                                                          
VACCTSE  CR    RB,RB                                                            
VACCTSX  XIT1                                                                   
         EJECT                                                                  
         DROP  T                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
EFFS     DC    (TLLNQ)X'FF'                                                     
ACCFLDLN EQU   29                                                               
SUPERUNT EQU   C'F'                                                             
MAXOPAGE EQU   5                   NUMBER OF GIVING ACCOUNTS ON 1 PAGE          
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#YES,3,L                                                       
         DCDDL AC#NO,2,L                                                        
DCLISTX  DC    X'00'                                                            
         SPACE 2                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORKA                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
SAVEREG  DS    A                                                                
SAVERE   DS    A                                                                
SVADDR   DS    A                                                                
SVADDR2  DS    A                                                                
SVALDG   DS    A                   A(LEDGER FIELD)                              
AACCFLD  DS    A                   A(ACCOUNT FIELD)                             
RECDSP   DS    H                                                                
TOTFIELD DS    XL1                                                              
LNLEVELS DS    0XL4                LENGTHS OF EACH LEVEL                        
LNLEV1   DS    XL1                                                              
LNLEV2   DS    XL1                                                              
LNLEV3   DS    XL1                                                              
LNLEV4   DS    XL1                                                              
* DO NOT SEPERATE THESE FIELDS                                                  
BIT      DS    XL1                                                              
NOCOMMA  EQU   X'80'               NO COMMA IN ACC OR CONTRA ACC FIELD          
ACCDTL   EQU   X'40'               ACCT DETAIL INDICATOR FLD SET TO YES         
SPLITSW  DS    X                   SPLIT SWITCH                                 
SPLSPLIT EQU   X'F0'               ACCOUNT SPLIT                                
SPLWILD  EQU   X'0F'               WILD                                         
SPLITERR DS    C                   SPLIT ERROR                                  
SPLNOSPL EQU   C'1'                NO SPLITTER                                  
SPL2SPL  EQU   C'2'                SPLIT TWICE                                  
SPLMXSPL EQU   C'3'                CAN'T HAVE BOTH SPLIT AND WILD               
SVKEY    DS     CL42                                                            
*                                                                               
*                                                                               
DSLIST   DS    0D                                                               
AC@YES   DS    CL3                 YES                                          
AC@NO    DS    CL2                 NO                                           
*                                                                               
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
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT                                                                  
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLUSER                                                           
TLKSUB   DS    XL(L'GLPSUB)                                                     
         ORG   TLKSUB                                                           
TLKFLTS  DS    XL(L'GLPFLTS)                                                    
TLKACTN  DS    XL(L'GLPACTN)                                                    
TLKBTYP  DS    XL(L'GLPBTYP)                                                    
         ORG   TLKSUB+L'TLKSUB                                                  
TLKACC1  DS    0CL(L'GLPACC1)                                                   
TLKACC1U DS    CL(L'GLPACC1U)                                                   
TLKACC1L DS    CL(L'GLPACC1L)                                                   
TLKACC1A DS    CL(L'GLPACC1A)                                                   
TLKACC2  DS    0CL(L'GLPACC2)                                                   
TLKACC2U DS    CL(L'GLPACC2U)                                                   
TLKACC2L DS    CL(L'GLPACC2L)                                                   
TLKACC2A DS    CL(L'GLPACC2A)                                                   
TLKCON1  DS    0CL(L'GLPCON1)                                                   
TLKCON1U DS    CL(L'GLPCON1U)                                                   
TLKCON1L DS    CL(L'GLPCON1L)                                                   
TLKCON1A DS    CL(L'GLPCON1A)                                                   
TLKCON2  DS    0CL(L'GLPCON2)                                                   
TLKCON2U DS    CL(L'GLPCON2U)                                                   
TLKCON2L DS    CL(L'GLPCON2L)                                                   
TLKCON2A DS    CL(L'GLPCON2A)                                                   
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACFIL14   08/10/11'                                      
         END                                                                    
