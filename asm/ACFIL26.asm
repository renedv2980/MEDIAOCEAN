*          DATA SET ACFIL26    AT LEVEL 016 AS OF 08/31/20                      
*&&      SET   NOP=N                                                            
*PHASE T62326A                                                                  
         TITLE 'LEDGER RECORD - OBJECT VERSION'                                 
***********************************************************************         
* RGUP 016 20AUG2020 SPEC-49541 RELAX PASSWORD RULES FOR FQA AND CSC  *         
***********************************************************************         
         SPACE 2                                                                
FIL26    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL26**,R7,RR=RE                                              
         USING TWAD,RA                                                          
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
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     L     R2,=A(DCLIST)                                                    
         A     R2,BORELO                                                        
         GOTO1 VDICTAT,BOPARM,C'LU  ',(R2),DSLIST                               
         OI    GSINDSL1,GSIXKEY    DON'T ASK 'ENTER KEY'                        
         B     EXITOK                                                           
         EJECT                                                                  
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
         EJECT                                                                  
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
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
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
         USING LDGRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   MVC   LDGKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVC   LDGKCPY,CUABIN      SET DEFAULT                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   LDGKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVC   LDGKCPY,CUABIN      SET DEFAULT                                  
         MVI   LDGKUNT,X'41'                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                       *         
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 A(RECORD)                                                                  
* P4 HOLDS SUB-ACTION VERB                                                      
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R0,R3,SVPARMS                                                    
         USING LDGRECD,R2                                                       
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
         DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD/CHANGE                                     
***********************************************************************         
         SPACE 1                                                                
RFADD    GOTO1  =A(ADDACTEL),BODMCB,AIOREC,RR=BORELO                            
         B      EXITOK                                                          
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
RECLAST  L     R1,SVPARMS4                                                      
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RLTABL   DS    0H                                                               
         DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD/CHANGE                                      
***********************************************************************         
         SPACE 1                                                                
RLADD    GOTO1  =A(ADDACTPT),BODMCB,AIOREC,RR=BORELO                            
         B      EXITOK                                                          
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
FTFLST   DS    0H                                                               
*        XC    LGNPAS,LGNPAS       NO PASSWORD ON LIST SCREEN                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'ACTKEY),THIS.ACTRECD                                     
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   EXITL               READ HIGH UNHAPPY                            
         B     NLST02                                                           
         SPACE 2                                                                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* REMEMBER TO ADD A CALL TO AGETACT IN ORDER TO TEST LEDGER SECURITY            
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
X        USING ACTRECD,IOKEY                                                    
NLST     XR    RF,RF               CONTROLLER REESTABLISHES SEQUENCE            
         IC    RF,X.ACTKLDG                                                     
         LA    RF,1(RF)                                                         
         STC   RF,X.ACTKLDG                                                     
         L     R1,=AL4(XOHID+XOACCDIR+XIO1)                                     
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         BL    EXITL               HARD I/O ERROR                               
         TM    IOERR,IOEDEL        IF ERROR IS THAT REC IS DELETED              
         BO    NLST                READ NEXT INSTEAD OF EXIT                    
         B     EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   X.ACTKCPY,THIS.ACTKCPY                                           
         BNE   EXITL               DIFFERENT COMPANY - IGNORE IT                
         CLI   X.ACTKLDG,C' '                                                   
         BNH   NLST                                                             
         CLC   X.ACTKCPY(2),=X'FEFF'  IF COMPANY CODE IS FE -                   
         BE    EXITL                  AND WE REACHED EOF EXIT                   
*                                                                               
         MVC   THIS.ACTKEY(ACCKLEN),IOKEY   WE WANT THIS RECORD                 
         B     EXITOK                                                           
         DROP  X                                                                
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
         USING LDGRECD,R2                                                       
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
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING LDGRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
*        BR    RF                                                               
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(LDG#UL),AL4(ULC)           UNIT/LEDGER                       
         DC    AL2(LDG#PSW),AL4(PAS)          PASSWORD                          
         DC    AL2(LDG#LDTYP),AL4(LDGTY)      LEDGER TYPE                       
         DC    AL2(LDG#SECLV),AL4(SECL)       SECURITY LEVEL                    
         DC    AL2(LDG#SECHX),AL4(SECLX)      SECURITY LEVEL IN HEX             
         DC    AL2(LDG#LVALN),AL4(LAL)        LEVEL A LENGTH                    
         DC    AL2(LDG#LVBLN),AL4(LBL)        LEVEL B LENGTH                    
         DC    AL2(LDG#LVCLN),AL4(LCL)        LEVEL C LENGTH                    
         DC    AL2(LDG#LVDLN),AL4(LDL)        LEVEL D LENGTH                    
         DC    AL2(LDG#LVANM),AL4(LAN)        LEVEL A NAME                      
         DC    AL2(LDG#LVBNM),AL4(LBN)        LEVEL B NAME                      
         DC    AL2(LDG#LVCNM),AL4(LCN)        LEVEL C NAME                      
         DC    AL2(LDG#LVDNM),AL4(LDN)        LEVEL D NAME                      
         DC    AL2(LDG#GLDG1),AL4(GENL1)      GENERAL LEDGER LINE 1             
         DC    AL2(LDG#GLDG2),AL4(GENLG)      GENERAL LEDGER LINE 2             
         DC    AL2(LDG#GLDG3),AL4(GENLG)      GENERAL LEDGER LINE 3             
         DC    AL2(LDG#GLDG4),AL4(GENLG)      GENERAL LEDGER LINE 4             
         DC    AL2(LDG#GLDG5),AL4(GENLG)      GENERAL LEDGER LINE 5             
         DC    AL2(LDG#CDINC),AL4(CDA)        INCOME FROM C.D.                  
         DC    AL2(LDG#CDINCN),AL4(CDAN)      INCOME FROM C.D. ACCT NME         
         DC    AL2(LDG#NXTBIL),AL4(NXTB)      NEXT BILL NUMER                   
         DC    AL2(LDG#RSTNUM),AL4(RESB)      RESET BILL NUMER                  
         DC    AL2(LDG#EXPANAL),AL4(EXPA)     EXPENSE ANALYSIS                  
         DC    AL2(LDG#COSTP),AL4(CSTP)       MAKE COST POSTINGS?               
         DC    AL2(LDG#SRCH),AL4(NAMS)        NAME SEARCH                       
         DC    AL2(LDG#LEVLST),AL4(LVLL)      LEVELS FOR LIST SCREEN            
         DC    AL2(LDG#RUNMA),AL4(RULAN)      ACC EQUIV RULE 'A' NAME           
         DC    AL2(LDG#RUNMB),AL4(RULBN)      ACC EQUIV RULE 'B' NAME           
         DC    AL2(LDG#RUNMC),AL4(RULCN)      ACC EQUIV RULE 'C' NAME           
         DC    AL2(LDG#RUNMD),AL4(RULDN)      ACC EQUIV RULE 'D' NAME           
         DC    AL2(LDG#RUNME),AL4(RULEN)      ACC EQUIV RULE 'E' NAME           
         DC    AL2(LDG#QRULA),AL4(RULA)       ACC EQUIV RULE A                  
         DC    AL2(LDG#QRULB),AL4(RULB)       ACC EQUIV RULE B                  
         DC    AL2(LDG#QRULC),AL4(RULC)       ACC EQUIV RULE C                  
         DC    AL2(LDG#QRULD),AL4(RULD)       ACC EQUIV RULE D                  
         DC    AL2(LDG#QRULE),AL4(RULE)       ACC EQUIV RULE E                  
         DC    AL2(LDG#ASDRF),AL4(DRFD)       LDGSDRFT                          
         DC    AL2(LDG#SLSAC),AL4(SLSA)       SLUSH ACCOUNT                     
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
         SPACE 1                                                                
***********************************************************************         
* MACRO BRANCH TO DATA OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
         MACRO                                                                  
&NTRDO   NTRDO                                                                  
         DC    CL8'&NTRDO'                                                      
         DS    0H                                                               
         USING *,RF                                                             
&NTRDO   NTR1                                                                   
         DROP  RF                                                               
         LR    R7,RF                                                            
         USING &NTRDO,R7                                                        
         LA    RF,&NTRDO.TBL       TABLE OF KNOWN VERBS                         
         B     ITER                                                             
         MEND                                                                   
         SPACE 1                                                                
FIL26    CSECT                                                                  
         EJECT                                                                  
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
DFDDIS   GOTO1 AGETEL,BOPARM,('LDGELQ',LDGRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NO LEDGER ELEMENT                            
         MVC   SVLDGEL,BOELEM      SAVE LEDGER ELEMENT                          
         GOTO1 AGETEL,BOPARM,('RSTELQ',LDGRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NO STATUS ELEMENT                            
         MVC   SVRSTEL,BOELEM      SAVE STATUS ELEMENT                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   MVI   NFILIND,0           SET INDICATOR TO 0                           
*                                                                               
         GOTO1 AGETEL,BOPARM,('LDGELQ',LDGRECD),0                               
         BE    DFDV02              LDGEL FOUND OK                               
T        USING LDGELD,BOELEM       IF NO LDGEL, BUILD A NEW ONE                 
         MVI   T.LDGEL,LDGELQ                                                   
         MVI   T.LDGLN,LDGLNQ                                                   
         DROP  T                                                                
         GOTO1 AADDEL,BOPARM,LDGRECD                                            
DFDV02   MVC   SVLDGEL,BOELEM      SAVE LEDGER ELEMENT                          
*                                                                               
         GOTO1 AGETEL,BOPARM,('RSTELQ',LDGRECD),0                               
         BE    DFDV04              RSTEL FOUND OK                               
T        USING RSTELD,BOELEM       IF NO RSTEL, BUILD A NEW ONE                 
         MVI   T.RSTEL,RSTELQ                                                   
         MVI   T.RSTLN,RSTLN3Q                                                  
         DROP  T                                                                
         GOTO1 AADDEL,BOPARM,LDGRECD                                            
DFDV04   MVC   SVRSTEL,BOELEM      SAVE STATUS ELEMENT                          
*                                                                               
         GOTO1 AGETEL,BOPARM,('ACLELQ',LDGRECD),0                               
         BE    DFDV06              ACLEL FOUND OK                               
T        USING ACLELD,BOELEM                                                    
         XC    SVACLEL,SVACLEL     BUILD AN ACC LENGTH ELEMENT                  
         MVI   T.ACLEL,ACLELQ                                                   
         MVI   T.ACLLN,ACLLN1Q+(L'ACLVALS*4)                                    
         MVC   T.ACLVDESC,BCSPACES                                              
         MVC   T.ACLVDESC+(L'ACLVALS*1)(L'ACLVDESC),BCSPACES                    
         MVC   T.ACLVDESC+(L'ACLVALS*2)(L'ACLVDESC),BCSPACES                    
         MVC   T.ACLVDESC+(L'ACLVALS*3)(L'ACLVDESC),BCSPACES                    
         GOTO1 AADDEL,BOPARM,LDGRECD                                            
DFDV06   MVC   SVACLEL,BOELEM      SAVE STATUS ELEMENT                          
*                                                                               
         USING NUMELD,RF                                                        
         LA    RF,LDGRFST                                                       
         XR    R1,R1                                                            
*                                                                               
         USING NUMELD,RF                                                        
DFDV08   CLI   NUMEL,0                                                          
         BE    DFDV10                                                           
         CLI   NUMEL,NUMELQ                                                     
         BNE   *+12                                                             
         CLI   NUMTYPE,NUMTYLEQ                                                 
         BE    DFDVALX             NUMEL/NUMTYLEQ FOUND OK                      
         IC    R1,NUMLN                                                         
         AR    RF,R1                                                            
         B     DFDV08                                                           
         DROP  RF                                                               
*                                                                               
T        USING NUMELD,BOELEM       IF NO NUMEL, BUILD A NEW ONE                 
DFDV10   XC    BOELEM,BOELEM                                                    
         MVI   T.NUMEL,NUMELQ                                                   
         MVI   T.NUMLN,NUMLN2Q                                                  
         MVI   T.NUMTYPE,NUMTYLEQ                                               
         DROP  T                                                                
         GOTO1 AADDEL,BOPARM,LDGRECD                                            
*                                                                               
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
* LAST TIME FOR DISPLAY OF A DATA OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   GOTO1 AREPEL,BOPARM,('LDGELQ',LDGRECD),0,SVLDGEL                       
         GOTO1 AREPEL,BOPARM,('RSTELQ',LDGRECD),0,SVRSTEL                       
*                                                                               
         TM    NFILIND,NFILALBD    HAS ACLEL BEEN BUILT?                        
         BZ    DLDVALX                                                          
         GOTO1 AREPEL,BOPARM,('ACLELQ',LDGRECD),0,SVACLEL                       
*                                                                               
DLDVALX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR GENERAL LEDGER LINE 1                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
* AIO5 -> A(BLOCK)                                                    *         
***********************************************************************         
         SPACE 1                                                                
GENL1    LA    RF,GENL1TBL                                                      
         B     ITER                                                             
*                                                                               
GENL1TBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISGENLA)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGENL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY GENERAL LEDGER LINE 1                                       *         
***********************************************************************         
         SPACE 1                                                                
DISGENLA XC    SVREG,SVREG                                                      
         L     R5,AIO5                                                          
         LR    RE,R5               CLEAR IO5                                    
         LA    RF,IOAREALN                                                      
         XCEF                                                                   
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('GLPELQ',LDGRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)           A(GLPELQ)                                    
         USING GLPELD,RF                                                        
         SR    R4,R4                                                            
         SR    R0,R0                                                            
DGLA10   CLC   GLPSUB,BCSPACES                                                  
         BH    DGLA20                                                           
         MVC   0(L'AC@DEF,R5),AC@DEF                                            
         B     *+10                                                             
DGLA20   MVC   0(L'GLPSUB,R5),GLPSUB                                            
         MVC   L'GLPSUB(L'GLPACC1,R5),GLPACC1                                   
         LA    R5,L'GLPSUB+L'GLPACC1(R5)                                        
         LA    R4,1(R4)                                                         
         IC    R0,GLPLN                                                         
         AR    RF,R0                                                            
         CLI   GLPEL,GLPELQ                                                     
         BE    DGLA10                                                           
*                                                                               
         GOTO1 VUNSCAN,BOPARM,((R4),AIO5),(L'GLPACC1,FVIHDR)                    
         ST    R1,SVREG            NEXT BLOCK                                   
         CLI   0(R1),0             LAST BLOCK?                                  
         BNE   *+10                                                             
         XC    SVREG,SVREG         YES - CLEAR SVREG                            
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE GENERAL LEDGER ALL LINES                                   *         
***********************************************************************         
         SPACE 1                                                                
VALGENL  L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(LDG#GLDG1) TEST GENERAL LINE 1?                        
         BNE   VGENL02                                                          
         GOTO1 ADELEL,BOPARM,('GLPELQ',LDGRECD),0                               
VGENL02  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
VGENL02A GOTO1 VSCANNER,BOPARM,(L'GLPACC1,FVIHDR),(8,BLOCK),0                   
         SR    R4,R4                                                            
         ICM   R4,1,4(R1)          NUMBER OF LINES USED                         
         BZ    EXITNV                                                           
         MVC   FVMSGNO,=AL2(AE$RECNF)                                           
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,LDGKCPY                                                  
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITL               RECORD NOT FOUND                             
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
*                                                                               
         L     RF,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('CPYELQ',(RF)),0                                  
         BE    *+6                                                              
         DC    H'0'                MUST HAVE COMPANY ELEMENT                    
         SPACE 2                                                                
         USING GLPELD,BOELEM                                                    
         XC    BOELEM,BOELEM                                                    
         MVI   GLPEL,GLPELQ        BUILD GENERAL LEDGER ELEMENT                 
         MVI   GLPLN,GLPLN1Q                                                    
         LA    R5,BLOCK                                                         
         USING SCANBLKD,R5                                                      
VGENL04  CLI   SC2NDLEN,0                                                       
         BE    EXITNV                                                           
         CLC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),=C'SJ'  FOR SJ LEDGER NO            
         BNE   *+12                                 LONGER ALLOW *N OR          
         CLI   SC1STFLD,C'*'                        *NN OFFICE LVL              
         BE    EXITNV                               FILTERS AS OF 04/04         
         MVC   ACTKACT,BCSPACES                                                 
         MVC   ACTKULA,SC2NDFLD                                                 
         CLI   ACTKUNT,C'G'        MUST BE UNIT 'G'                             
         BNE   EXITNV                                                           
         LA    RE,ACTKACT        LOOK FOR AN ASTERISK                           
         LA    RF,L'ACTKACT                                                     
         CLI   0(RE),C'*'                                                       
         BE    *+16                                                             
         LA    RE,1(RE)                                                         
         BCT   RF,*-12                                                          
         B     VGENL10                                                          
*                                                                               
         TM    BCCPYST4,CPYSOFF2   NEW OFFICES?                                 
         BO    EXITNV              THAN WILDCARDS ARE NOT VALID                 
         MVI   0(RE),C' '          BLANK IT OUT                                 
         LA    RF,IOKEY                                                         
         SR    RE,RF                                                            
         STC   RE,BOBYTE1                                                       
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         ZIC   RE,BOBYTE1                                                       
         BCTR  RE,0                                                             
         EXCLC RE,IOKEY,IOKEYSAV   WAS IT THE SAME AS BEFORE?                   
         BE    VGENL12                                                          
         BNE   VGENLERR                                                         
VGENL10  LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   VGENLERR                                                         
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
         MVC   BCHALF,SC2NDFLD     HALF CONTAINS THE GEN LDGR U/L               
         BRAS  RE,LDGRD            CALL FOR READING LEDGER                      
         ICM   RE,15,ACALDG                                                     
         USING LDGTABD,RE                                                       
         ZIC   R1,SC2NDLEN         RHS INPUT LENGTH                             
         AHI   R1,-2               SUBTRACT 2 FOR U/L                           
         CLI   LDGTLVA,L'ACTKACT   ONE LEVEL LEDGER?                            
         BE    VGENL12             THAN AT THE LOWEST LEVEL                     
         ZIC   RF,LDGTLVC          IF 4 LEVEL LEDGER MUST CHECK                 
         CLI   LDGTLVD,0           3RD LEVEL                                    
         BNE   VGENL10A                                                         
         ZIC   RF,LDGTLVB          IF 3 LEVEL LEDGER MUST CHECK                 
         CLI   LDGTLVC,0           2ND LEVEL                                    
         BNE   VGENL10A                                                         
         ZIC   RF,LDGTLVA          CAN ASSUME A 2 LEVEL LEDGER                  
VGENL10A CR    R1,RF               DID USER INPUT A LOW LVL GEN ACCT?           
         BH    VGENL12                                                          
         MVC   FVMSGNO,=AL2(AE$INACP)                                           
         B     EXITL                                                            
*                                                                               
VGENL12  CLC   SC1STFLD(L'AC@GEN),AC@GEN    GENERAL= NOT VALID AS OF            
         BE    EXITNV                       MAY/2004                            
         MVC   GLPSUB,SC1STFLD     1ST FIELD                                    
         MVC   GLPACC1,SC2NDFLD    2ND FIELD                                    
         CLC   GLPSUB(L'AC@DEF),AC@DEF                                          
         BNE   *+10                                                             
         MVC   GLPSUB,BCSPACES                                                  
         SPACE 2                                                                
         GOTO1 AADDEL,BOPARM,LDGRECD                                            
         L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(LDG#GLDG2) TEST GENERAL LINE 2?                        
         BNE   *+8                                                              
         B     *+4                                                              
         LA    R5,SCBLKLQ+4(R5)    NEXT SCANBLK LINE                            
         BCT   R4,VGENL04                                                       
         B     EXITOK                                                           
*                                                                               
VGENLERR MVC   FVMSGNO,=AL2(AE$INACC)                                           
         MVC   FVXTRA,BCSPACES                                                  
         ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    EXITL                                                            
         EXMVC R1,FVXTRA,SC2NDFLD                                               
         B     EXITL                                                            
*                                                                               
         DROP  R5                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR GENERAL LEDGER LINE 2,3 ,4 AND 5                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
* AIO5 -> A(BLOCK)                                                              
***********************************************************************         
         SPACE 1                                                                
GENLG    LA    RF,GENLG                                                         
         B     ITER                                                             
*                                                                               
GENLGTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISGENL)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGENL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY GENERAL LEDGER LINE 2,3,4 AND 5                             *         
***********************************************************************         
         SPACE 1                                                                
DISGENL  OC    SVREG,SVREG                                                      
         BZ    EXITOK              NO GENERAL LEDGER ELEMENT                    
         L     R1,SVREG                                                         
         GOTO1 VUNSCAN,(R1),,(L'GLPACC1,FVIHDR)                                 
         ST    R1,SVREG            NEXT BLOCK                                   
         CLI   0(R1),0             LAST BLOCK?                                  
         BNE   *+10                                                             
         XC    SVREG,SVREG         YES - CLEAR SVREG                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR UNIT/LEDGER CODE                                    *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ULC      NTRDO                                                                  
*                                                                               
ULCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISULC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALULC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTULC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALULC)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTULC)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISULC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISULC   MVC   FVIFLD(L'LDGKUNT+L'LDGKLDG),LDGKUNT                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LEDGER FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALULC   CLI   FVILEN,2            CAN ENTER JUST THE UNIT IF ON THE            
         BE    *+12                                                             
         CLI   CSACT,A#LST                                                      
         BNE   EXITNO                                                           
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,LDGKUNT,FVIFLD   MOVE INTO KEY                                
         EXMVC R1,FLTIFLD,FVIFLD   MOVE INTO FILTER TOO                         
         CLI   CSACT,A#LST         CAN ENTER ONLY UNIT ON LIST SCREEN           
         BE    VALUL10                                                          
         CLI   FVIFLD+1,X'C1'      MAKE SURE A VALID LETTER OR #                
         BL    VALULLDG                                                         
         CLI   FVIFLD+1,X'F9'                                                   
         BH    VALULLDG                                                         
         BRAS  RE,CHKUL            MAKE SURE VALID LEDGER BASED ON UNIT         
         BNE   VALULLDG                                                         
VALUL10  MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(LDGKEND-1),0(R2)  MOVE IN UNIT                             
         CLI   CSACT,A#ADD         ADDING THIS LEDGER?                          
         BE    *+10                THAN JUST VALIDATE THE UNIT                  
         MVC   IOKEY(LDGKEND),0(R2)    MOVE IN UNIT AND LEDGER                  
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INUNT)  INVALID UNIT                             
         CLI   FVILEN,1                                                         
         BE    EXITL                                                            
VALULLDG MVC   FVMSGNO,=AL2(AE$INLDG)  INVALID LEDGER                           
         B     EXITL                                                            
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTULC  MVC   FVIFLD(L'LDGKUNT+L'LDGKLDG),FLTIFLD                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LEDGER                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTULC  CLI   LDGKLDG,C' '        IS THERE A LEDGER TO COMPARE ON?             
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLI   FLTIFLD+1,C' '      FILTER ON UNIT OR UNIT AND LEDGER?           
         BNE   DFTUL10                                                          
         CLC   LDGKUNT,FLTIFLD                                                  
         BE    FLTXE                                                            
         B     FLTXL                                                            
*                                                                               
DFTUL10  CLC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),FLTIFLD                             
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         CLC   LDGKUNT,FLTIFLD                                                  
         BE    FLTXE                                                            
         B     FLTXX                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A PASSOWRD                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PAS      NTRDO                                                                  
*                                                                               
PASTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPAS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPAS)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PASSWORD                                                 *          
***********************************************************************         
         SPACE 1                                                                
DISPAS   MVC   FVIFLD,LGNPAS                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PASSWORD                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALPAS   XC    LGNPAS,LGNPAS                                                    
         CLI   FVILEN,0            ANY PASSWORD ENTERED                         
         BE    EXITOK                                                           
         GOTO1 VGETFACT,BODMCB,(X'80',0),F#SSBD CHECK DSPACE IN SSB             
         L     R1,0(R1)                                                         
         MVC   BOBYTE1,F@DSPACE-F@SSBD(R1)    SYSTEM                            
         CLI   BOBYTE1,C'C'         CSC?                                        
         BE    VALPAS10                                                         
         CLI   BOBYTE1,C'Q'         FQA?                                        
         BNE   VALPAS20                                                         
*        TM    FACFLAG2,XICSCADV   CONNECTED TO CSC?                            
*        BNO   *+10                ANY 3 LETTER PSW WILL WORK ON CSC            
VALPAS10 MVC   FVIFLD(3),=C'$%#'                                                
VALPAS20 CLC   FVIFLD(3),=C'$%#'                                                
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVPW)  INVALID PASSWORD                         
         B     EXITL                                                            
         MVC   LGNPAS,FVIFLD       PASSWORD                                     
         MVC   APASSWRD,FVADDR                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LEDGER TYPE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LDGTY    NTRDO                                                                  
*                                                                               
LDGTYTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDGT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLDGT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TYPE OF LEDGER ATTRIBUTES                                   *         
* BUILD BLOCK IN AIO5 FOR UNSCAN.  UNSCAN USES FIXED 20 BYTE FIELDS.  *         
* 10 BYTES FOR RHS AND 10 BYTES FOR LHS UNLESS YOU SPECIFY OTHERWISE  *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
DISLDGT  L     R5,AIO5                                                          
         LR    RE,R5               CLEAR IO5                                    
         LA    RF,IOAREALN                                                      
         XCEF                                                                   
         MVI   BCBYTE1,0           KEEP TRACK OF # OF ENTRIES                   
         LA    R4,SVLDGEL          R4=SAVED 14 LEDGER ELEMENT                   
         OC    LDGTYPE,LDGTYPE     TYPE=?                                       
         BZ    DLDGT10                                                          
         MVC   0(L'AC@TYPE,R5),AC@TYPE                                          
         MVC   10(L'LDGTYPE,R5),LDGTYPE                                         
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT10  CLI   LDGLIKE,C' '        LIKE=?                                       
         BNH   DLDGT20                                                          
         MVC   0(L'AC@LIKE,R5),AC@LIKE                                          
         MVC   10(L'LDGLIKE,R5),LDGLIKE                                         
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT20  TM    LDGSTAT,LDGSCHQO   OFFCK=?                                       
         BZ    DLDGT40                                                          
         MVC   0(L'AC@NFOCK,R5),AC@NFOCK                                        
         MVI   10(R5),C'O'                                                      
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT40  TM    LDGSTAT,LDGSCANL   CANAD=?                                       
         BZ    DLDGT50                                                          
         MVC   0(L'AC@NFCAN,R5),AC@NFCAN                                        
         MVC   10(10,R5),BCSPACES                                               
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT50  TM    LDGSTAT,LDGSVENR   VEHICLE=?                                     
         BZ    DLDGT60                                                          
         MVC   0(L'AC@NFVEH,R5),AC@NFVEH                                        
         MVC   10(10,R5),BCSPACES                                               
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT60  TM    LDGSTAT,LDGSCOKE   EXPEND=?                                      
         BZ    DLDGT70                                                          
         MVC   0(L'AC@NFXPD,R5),AC@NFXPD                                        
         MVC   10(10,R5),BCSPACES                                               
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT70  TM    LDGSTAT,LDGRUPCT   100%=?                                        
         BZ    DLDGT80                                                          
         MVC   0(4,R5),=C'100%'                                                 
         MVC   10(10,R5),BCSPACES                                               
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT80  CLI   LDGOPOS,0          OFFPOS=?                                      
         BZ    DLDGT100                                                         
         MVC   0(L'AC@NFOFP,R5),AC@NFOFP                                        
         MVC   10(L'LDGOPOS,R5),LDGOPOS                                         
         CLI   LDGOPOS,C' '        NN OR 0?                                     
         BL    DLDGT85                                                          
         CLI   LDGOPOS,X'4C'       TEST NEW OFFICE IN KEY LEDGER (+NN?)         
         BNH   DLDGT85             YES                                          
         CLI   LDGOPOS,X'F0'       F1-F4?                                       
         BH    DLDGT95                                                          
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
         B     DLDGT100                                                         
*                                                                               
DLDGT85  ZIC   R0,LDGOPOS                                                       
         N     R0,=F'15'           ISOLATE DISPLACEMENT INTO KEY                
         CVD   R0,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         TM    LDGOPOS,LDGOKEY2    TEST NEW OFFICE IN KEY                       
         BZ    DLDGT90             NO                                           
         MVI   10(R5),C'+'         YES-PUT A PLUS SIGN BEFORE NUMBER            
         UNPK  11(2,R5),BODUB1                                                  
         B     *+10                                                             
*                                                                               
DLDGT90  UNPK  10(2,R5),BODUB1                                                  
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
         B     DLDGT100                                                         
*                                                                               
DLDGT95  GOTO1 VHEXOUT,BODMCB,LDGOPOS,10(R5),1,0                                
         LA    R5,20(R5)                                                        
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT100 CLI   LDGLN,X'20'                                                      
         BL    DLDGT150            OLD ELEMENT                                  
*                                                                               
         CLC   =C'SN',LDGKUNT                                                   
         BE    DLDGT110            IGNORE FOR TALENT PERFORMERS                 
         CLI   LDGDPOS,0           DEPT=?                                       
         BE    DLDGT110                                                         
         ZIC   R1,LDGDPOS                                                       
         CVD   R1,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         MVC   0(L'AC@DEPT,R5),AC@DEPT                                          
         LA    R3,10(R5)                                                        
         LA    RE,12(R5)                                                        
         UNPK  BODUB1(3),BODUB1+6(2)                                            
         MVC   0(2,R3),BODUB1+1                                                 
         CLI   0(R3),C'0'                                                       
         BNE   *+12                                                             
         BCTR  RE,0                                                             
         MVC   0(1,R3),1(R3)                                                    
         MVI   0(RE),C'/'                                                       
         ZIC   R1,LDGDLEN                                                       
         CVD   R1,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         LA    R3,1(RE)                                                         
         LA    RE,3(RE)                                                         
         UNPK  BODUB1(3),BODUB1+6(2)                                            
         MVC   0(2,R3),BODUB1+1                                                 
         CLI   0(R3),C'0'                                                       
         BNE   *+16                                                             
         BCTR  RE,0                                                             
         MVC   0(1,R3),1(R3)                                                    
         MVI   0(RE),C' '          CLEAR OUT THE BYTE                           
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT110 CLI   LDGCPOS,0           CLI=?                                        
         BE    DLDGT120                                                         
         MVC   0(L'AC@CLI,R5),AC@CLI                                            
         MVC   10(L'LDGCPOS,R5),LDGCPOS                                         
         OI    10(R5),X'F0'                                                     
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT120 CLI   LDGBPOS,0           BUDPOS=?                                     
         BE    DLDGT140                                                         
         MVC   0(L'AC@NFBDP,R5),AC@NFBDP                                        
         MVC   10(L'LDGBPOS,R5),LDGBPOS                                         
         CLI   LDGBPOS,C' '                                                     
         BL    DLDGT125                                                         
         CLI   LDGBPOS,X'F0'                                                    
         BH    DLDGT130                                                         
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
         B     DLDGT140                                                         
*                                                                               
DLDGT125 ZIC   R0,LDGBPOS                                                       
         CVD   R0,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  10(2,R5),BODUB1                                                  
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
         B     DLDGT140                                                         
*                                                                               
DLDGT130 GOTO1 VHEXOUT,BODMCB,LDGBPOS,10(R5),1,0                                
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT140 CLI   LDGOFFC,X'40'       GLOFF=?                                      
         BNH   DLDGT150                                                         
         MVC   0(L'AC@GLOFF,R5),AC@GLOFF                                        
         MVC   10(L'LDGOFFC,R5),LDGOFFC                                         
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
         DROP  R4                                                               
*                                                                               
         USING RSTELD,R4                                                        
DLDGT150 LA    R4,SVRSTEL          R4=SAVED 30 STATUS ELEMENT                   
         TM    RSTSTAT3,RSTSLAPL   P/L=?                                        
         BZ    DLDGT160                                                         
         MVC   0(L'AC@PL,R5),AC@PL                                              
         MVC   10(10,R5),BCSPACES                                               
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT160 TM    RSTSTAT3,RSTSLABS   BAL=?                                        
         BZ    DLDGT170                                                         
         MVC   0(L'AC@BAL,R5),AC@BAL                                            
         MVC   10(10,R5),BCSPACES                                               
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DLDGT170 ZIC   RF,BCBYTE1          # OF ENTRIES                                 
         GOTO1 VUNSCAN,BOPARM,((RF),AIO5),FVIHDR                                
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TYPE OF LEDGER ATTRIBUTES                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R3                                                      
VALLDGT  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         LA    R3,BLOCK                                                         
         LR    RE,R3               CLEAR SCANBLK                                
         LA    RF,SCANMAX*SCBLKLQ                                               
         XCEF                                                                   
*                                                                               
         USING LDGELD,R4                                                        
         LA    R4,SVLDGEL          SAVED 14 LEDGER ELEMENT                      
         XC    LDGOFFC,LDGOFFC     CLEAR VALUES                                 
         XC    LDGOPOS,LDGOPOS                                                  
         XC    LDGBPOS,LDGBPOS                                                  
         XC    LDGDPOS,LDGDPOS                                                  
         XC    LDGCPOS,LDGCPOS                                                  
         XC    LDGTYPE,LDGTYPE                                                  
         XC    LDGLIKE,LDGLIKE                                                  
         NI    LDGSTAT,X'FF'-(LDGSCANL+LDGSVENR+LDGSCOKE)                       
         NI    LDGSTAT,X'FF'-(LDGRUPCT+LDGSCHQO)                                
         DROP  R4                                                               
*                                                                               
         USING RSTELD,R4                                                        
         LA    R4,SVRSTEL          R4=SAVED 30 ELEMENT                          
         NI    RSTSTAT3,X'FF'-(RSTSLAPL+RSTSLABS)   CLEAR VALUES                
         MVI   LTYBYTE,0                                                        
         MVI   LTYBYTE2,0                                                       
         GOTO1 VSCANNER,BOPARM,FVIHDR,BLOCK                                     
         CLI   BOPARM+4,0                                                       
         BE    EXITNV                                                           
         ZIC   RF,BOPARM+4         RF=# OF SCANNER LINES                        
         STC   RF,SVSCLINE                                                      
         LA    R3,BLOCK                                                         
         USING SCANBLKD,R3                                                      
VLDGT02  LA    R4,SVRSTEL                                                       
         CLC   SC1STFLD(L'AC@PL),AC@PL     P/L=                                 
         BNE   *+12                                                             
         OI    RSTSTAT3,RSTSLAPL                                                
         B     VLDGTNX                                                          
         CLC   SC1STFLD(L'AC@BAL),AC@BAL   BAL=                                 
         BNE   *+12                                                             
         OI    RSTSTAT3,RSTSLABS                                                
         B     VLDGTNX                                                          
         DROP  R4                                                               
*                                                                               
         USING LDGELD,R4                                                        
         LA    R4,SVLDGEL          R4=SAVED 14 ELEMENT                          
         CLC   SC1STFLD(L'AC@NFCAN),AC@NFCAN    CANAD=                          
         BNE   *+12                                                             
         OI    LDGSTAT,LDGSCANL                                                 
         B     VLDGTNX                                                          
         CLC   SC1STFLD(L'AC@NFVEH),AC@NFVEH    VEHICLE=                        
         BNE   *+12                                                             
         OI    LDGSTAT,LDGSVENR                                                 
         B     VLDGTNX                                                          
         CLC   SC1STFLD(L'AC@NFXPD),AC@NFXPD    EXPEND=                         
         BNE   *+12                                                             
         OI    LDGSTAT,LDGSCOKE                                                 
         B     VLDGTNX                                                          
         CLC   SC1STFLD(4),=C'100%'                                             
         BNE   *+12                                                             
         OI    LDGSTAT,LDGRUPCT                                                 
         B     VLDGTNX                                                          
*                                                                               
         CLI   SC2NDLEN,0          2ND HALF OF FIELD REQUIRED FOR               
         BE    EXITNV              THE REST OF THE OPTIONS                      
         CLC   SC1STFLD(L'AC@GLOFF),AC@GLOFF    GLOFF=                          
         BE    VLDGT10                                                          
         CLC   SC1STFLD(L'AC@NFOFP),AC@NFOFP    OFFPOS=                         
         BE    VLDGT20                                                          
         CLC   SC1STFLD(L'AC@NFBDP),AC@NFBDP    BUDPOS=                         
         BE    VLDGT50                                                          
         CLC   SC1STFLD(L'AC@DEPT),AC@DEPT      DEPT=                           
         BE    VLDGT70                                                          
         CLC   SC1STFLD(L'AC@CLI),AC@CLI        CLI=                            
         BE    VLDGT80                                                          
         CLC   SC1STFLD(L'AC@TYPE),AC@TYPE      TYPE=                           
         BE    VLDGT90                                                          
         CLC   SC1STFLD(L'AC@LIKE),AC@LIKE      LIKE=                           
         BE    VLDGT100                                                         
         CLC   SC1STFLD(L'AC@NFOCK),AC@NFOCK    OFFCK=                          
         BE    VLDGT120                                                         
         B     EXITNV                                                           
*                                                                               
VLDGT10  TM    LTYBYTE,LTGLOFF     IS THIS OPTION ALREADY USED?                 
         BO    VLDGDUP                                                          
         MVC   LDGOFFC,SC2NDFLD                                                 
         CLI   SC2NDLEN,1                                                       
         BL    EXITNV              MUST BE A LEAST 1                            
         BE    VLDGT12                                                          
         CLI   SC2NDLEN,2                                                       
         BH    EXITNV              NEVER MORE THAN 2                            
         TM    BCCPYST4,CPYSOFF2   NEW OFFICES                                  
         BNO   EXITNV              IF NOT ON 2 OFFICES 2 IS ERROR               
VLDGT12  OI    LTYBYTE,LTGLOFF     SET BIT THAT THIS OPTION IS IN USE           
         B     VLDGTNX                                                          
*                                                                               
VLDGT20  TM    LTYBYTE,LTOFFPOS    IS THIS OPTION ALREADY USED?                 
         BO    VLDGDUP                                                          
         TM    SC2NDVAL,SCNUMQ     CHECK IF FIELD IS NUMERIC                    
         BZ    VLDGT25                                                          
         TM    BCCPYST4,CPYSOFF2   IF ON 2 CHAR OFFICES CAN'T ENTER             
         BO    EXITNV              JUST A NUMBER (NEED +NN)                     
         L     R0,SC2NDNUM         NUMERIC VALUE OF FIELD                       
         C     R0,=F'12'                                                        
         BH    EXITNV                                                           
         C     R0,=F'0'                                                         
         BL    EXITNV                                                           
         STC   R0,LDGOPOS                                                       
         OI    LTYBYTE,LTOFFPOS                                                 
         B     VLDGTNX                                                          
*                                                                               
VLDGT25  CLI   SC2NDLEN,1          RHS INPUT IS 1 BYTE LONG?                    
         BNE   VLDGT30                                                          
         CLI   SC2NDFLD,C'C'                                                    
         BE    VLDGT27                                                          
         CLI   SC2NDFLD,C'T'                                                    
         BE    VLDGT27                                                          
         CLI   SC2NDFLD,C'0'           LEAVE AS X'00' IN ELEM                   
         BNE   EXITNV                                                           
         OI    LTYBYTE,LTOFFPOS                                                 
         B     VLDGTNX                                                          
*                                                                               
VLDGT27  OI    LTYBYTE,LTOFFPOS                                                 
         MVC   LDGOPOS,SC2NDFLD                                                 
         B     VLDGTNX                                                          
*                                                                               
VLDGT30  CLI   SC2NDFLD,C'+'       TEST PARAMETER STARTS WITH PLUS              
         BNE   VLDGT40                                                          
         TM    BCCPYST4,CPYSOFF2   NEW OFFICES                                  
         BZ    EXITNV              NO-DON'T ALLOW IT                            
*                                                                               
         CLI   SC2NDLEN,2                                                       
         BL    EXITNV                                                           
         BE    *+12                                                             
         CLI   SC2NDLEN,3                                                       
         BH    EXITNV                                                           
         ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                CHECK THAT NUMBER FOLLOWS PLUS SIGN          
         LA    RE,SC2NDFLD+1                                                    
VLDGT35  CLI   0(RE),C'0'                                                       
         BL    EXITNV                                                           
         CLI   0(RE),C'9'                                                       
         BH    EXITNV                                                           
         LA    RE,1(RE)                                                         
         BCT   R1,VLDGT35                                                       
*                                                                               
         ZIC   R1,SC2NDLEN                                                      
         SH    R1,=H'2'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,SC2NDFLD+1(0)                                             
         CVB   R0,BODUB1                                                        
         LTR   R0,R0               TEST NUMBER BETWEEN 1-12                     
         BZ    EXITNV                                                           
         CH    R0,=H'12'                                                        
         BH    EXITNV                                                           
         STC   R0,LDGOPOS                                                       
         OI    LDGOPOS,LDGOKEY2    TURN ON SPECIAL BIT                          
         OI    LTYBYTE,LTOFFPOS                                                 
         B     VLDGTNX                                                          
*                                                                               
VLDGT40  CLC   SC2NDFLD(2),=C'F4'                                               
         BH    EXITNV                                                           
         CLC   SC2NDFLD(2),=C'F1'                                               
         BL    EXITNV                                                           
         TM    BCCPYST4,CPYSOFF2   NEW OFFICES?                                 
         BZ    *+18                THIS ENTRY ONLY VALID FOR ONE CHAR           
         MVI   FVOMTYP,C'I'        FILES                                        
         MVC   FVMSGNO,=AL2(AI$OFPFN)                                           
         B     EXITL                                                            
         CLI   LDGKUNT,C'1'        OFFPOS=FN IS INVALID FOR LEDGERS             
         BNE   VLDGT45             11-16 AS OF SEPT/04                          
         CLI   LDGKLDG,C'1'                                                     
         BL    VLDGT45                                                          
         CLI   LDGKLDG,C'6'                                                     
         BNH   EXITNV                                                           
VLDGT45  GOTO1 VHEXIN,BODMCB,SC2NDFLD,LDGOPOS,2,0                               
         OC    BODMCB+12(4),BODMCB+12                                           
         BZ    EXITNV                                                           
         OI    LTYBYTE,LTOFFPOS                                                 
         B     VLDGTNX                                                          
*                                                                               
VLDGT50  TM    LTYBYTE,LTBUDPOS                                                 
         BO    VLDGDUP             WAS INPUT ALREADY                            
         OC    SC2NDNUM,SC2NDNUM   SEE IF NUMERIC                               
         BZ    VLDGT55                                                          
         L     R0,SC2NDNUM                                                      
         C     R0,=F'12'                                                        
         BH    EXITNV                                                           
         C     R0,=F'0'                                                         
         BL    EXITNV                                                           
         STC   R0,LDGBPOS                                                       
         B     VLDGT65                                                          
*                                                                               
VLDGT55  CLI   SC2NDLEN,1                                                       
         BNE   VLDGT60                                                          
         CLI   SC2NDFLD,C'C'                                                    
         BE    VLDGT58                                                          
         CLI   SC2NDFLD,C'T'                                                    
         BE    VLDGT58                                                          
         CLI   SC2NDFLD,C'0'           LEAVE AS X'00' IN ELEM                   
         BE    VLDGTNX                                                          
         B     EXITNV                                                           
VLDGT58  MVC   LDGBPOS,SC2NDFLD                                                 
         OI    LTYBYTE,LTBUDPOS                                                 
         B     VLDGTNX                                                          
*                                                                               
VLDGT60  CLC   SC2NDFLD(2),=C'F4'                                               
         BH    EXITNV                                                           
         CLC   SC2NDFLD(2),=C'F1'                                               
         BL    EXITNV                                                           
         GOTO1 VHEXIN,BODMCB,SC2NDFLD,LDGBPOS,2,0                               
         OC    BODMCB+12(4),BODMCB+12                                           
         BZ    EXITNV                                                           
*                                                                               
VLDGT65  OI    LTYBYTE,LTBUDPOS                                                 
         B     VLDGTNX                                                          
*                                                                               
VLDGT70  TM    LTYBYTE,LTDEPPOS                                                 
         BO    VLDGDUP             ALREADY INPUT                                
         CLI   SC2NDLEN,3                                                       
         BL    EXITNV              MUST BE 3 OR 4                               
         CLI   SC2NDLEN,4                                                       
         BH    EXITNV              FORMAT IS N/N OR NN/N                        
         LA    R1,1                                                             
         CLI   SC2NDFLD+1,C'/'                                                  
         BE    *+16                                                             
         LA    R1,2                                                             
         CLI   SC2NDFLD+2,C'/'                                                  
         BNE   EXITNV                                                           
         LR    RF,R1                                                            
         LA    R5,SC2NDFLD                                                      
         CLI   0(R5),C'1'                                                       
         BL    EXITNV                                                           
         CLI   0(R5),C'9'                                                       
         BH    EXITNV                                                           
         LA    R5,1(R5)                                                         
         BCT   RF,*-20                                                          
         LA    R5,SC2NDFLD                                                      
         LR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,0(0,R5)                                                   
         CVB   R5,BODUB1                                                        
         STC   R5,LDGDPOS                                                       
         CH    R5,=H'12'                                                        
         BH    EXITNV                                                           
         ZIC   RF,SC2NDLEN                                                      
         BCTR  RF,0                                                             
         SR    RF,R1               LENGTH OF FIELD 2 IN RF                      
         CH    RF,=H'1'                                                         
         BNE   EXITNV                                                           
         LA    R5,SC2NDFLD+1(R1)   R5 TO START OF FIELD 2                       
         LR    R1,RF                                                            
         LR    R2,R5                                                            
         CLI   0(R5),C'1'                                                       
         BL    EXITNV                                                           
         CLI   0(R5),C'3'                                                       
         BH    EXITNV                                                           
         LR    R5,R2                                                            
         LR    RF,R1                                                            
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,0(0,R5)                                                   
         CVB   R5,BODUB1                                                        
         STC   R5,LDGDLEN          LENGTH OF DEPARTMENT CODE                    
         ZIC   RF,LDGDPOS                                                       
         AR    RF,R5                                                            
         CH    RF,=H'13'                                                        
         BH    EXITNV                                                           
         OI    LTYBYTE,LTDEPPOS                                                 
         B     VLDGTNX                                                          
*                                                                               
VLDGT80  TM    LTYBYTE,LTCLI       ALREADY ENTERED CLI=?                        
         BO    VLDGDUP                                                          
         TM    SC2NDVAL,SCNUMQ     NUMERIC?                                     
         BZ    EXITNV              NO-GIVE ERROR                                
         CLC   SC2NDNUM,=F'1'                                                   
         BL    EXITNV                                                           
         CLC   SC2NDNUM,=F'9'                                                   
         BH    EXITNV                                                           
         MVC   LDGCPOS,SC2NDNUM+3                                               
         OI    LTYBYTE,LTCLI                                                    
         B     VLDGTNX                                                          
*                                                                               
VLDGT90  TM    LTYBYTE,LTTYPE      ENTERED TYPE ALREADY?                        
         BO    VLDGDUP                                                          
         CLI   SC2NDFLD,C'A'           ANALYSIS                                 
         BE    VLDGT95                                                          
         CLI   SC2NDFLD,C'G'           GENERAL                                  
         BE    VLDGT95                                                          
         CLI   SC2NDFLD,C'S'           SUBSID                                   
         BNE   EXITNV                                                           
*                                                                               
VLDGT95  MVC   LDGTYPE,SC2NDFLD                                                 
         OI    LTYBYTE,LTTYPE                                                   
         B     VLDGTNX                                                          
*                                                                               
VLDGT100 TM    LTYBYTE,LTLIKE      ENTERED LIKE= ALREADY?                       
         BO    VLDGDUP                                                          
         CLI   SC2NDFLD,C'R'                                                    
         BE    VLDGT105                                                         
         BNE   EXITNV                                                           
*                                                                               
VLDGT105 MVC   LDGLIKE,SC2NDFLD                                                 
         OI    LTYBYTE,LTLIKE                                                   
         B     VLDGTNX                                                          
*                                                                               
VLDGT115 MVC   LDGSPRNT,SC2NDFLD                                                
         OI    LTYBYTE,LTPRINT                                                  
         B     VLDGTNX                                                          
*                                                                               
VLDGT120 TM    LTYBYTE2,LTOFFCK    WAS OFFCK= ALREADY ENTERED?                  
         BO    VLDGDUP                                                          
         CLI   SC2NDFLD,C'O'                                                    
         BNE   EXITNV                                                           
         OI    LDGSTAT,LDGSCHQO                                                 
         OI    LTYBYTE2,LTOFFCK                                                 
         B     VLDGTNX                                                          
*                                                                               
VLDGTNX  LA    R3,SCBLKLQ(R3)       BUMP TO NEXT SCANNER LINE                   
         ZIC   R1,SVSCLINE                                                      
         AHI   R1,-1                                                            
         STC   R1,SVSCLINE                                                      
         BNZ   VLDGT02                                                          
*                                                                               
         LA    R4,SVLDGEL                                                       
         CLI   LDGOPOS,0           OFFPOS IS NOW MANDATORY                      
         BE    VLDGERR                                                          
         CLI   LDGOPOS,C' '                                                     
         BE    VLDGERR                                                          
         CLC   =C'SR',LDGKUNT      FOR SR/1C CLI= IS MANDATORY AS OF            
         BE    *+14                SEPT/03                                      
         CLC   =C'1C',LDGKUNT                                                   
         BNE   EXITOK                                                           
         TM    LTYBYTE,LTCLI                                                    
         BZ    VLDGERR2                                                         
         B     EXITOK                                                           
*                                                                               
VLDGERR  MVI   FVOMTYP,C'I'                                                     
         MVC   FVMSGNO,=AL2(AI$OFPOS)                                           
         B     EXITL                                                            
*                                                                               
VLDGERR2 MVI   FVOMTYP,C'I'                                                     
         MVC   FVMSGNO,=AL2(AI$CLIEN)                                           
         B     EXITL                                                            
*                                                                               
VLDGDUP  MVC   FVMSGNO,=AL2(AE$DUPIF)  DUPLICATE INPUT FIELD                    
         B     EXITL                                                            
*                                                                               
         DROP  R3,R4                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SECURITY LEVEL                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SECL     NTRDO                                                                  
*                                                                               
SECLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSEC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SECURITY LEVEL                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTELD,R4                                                        
DISSEC   LA    R4,SVRSTEL          R4=SAVED 30 STATUS ELEMENT                   
         SR    RE,RE                                                            
         ICM   RE,3,RSTSECY                                                     
         LTR   RE,RE                                                            
         BNZ   *+12                                                             
         MVI   FVIFLD,C' '         IF NO SECURITY LEVEL SHOW NOTHING            
         B     EXITOK                                                           
         CURED (RE),(5,FVIFLD),0,ALIGN=LEFT,DMCB=BCPARM                         
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SECURITY LEVEL                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTELD,R4                                                        
VALSEC   LA    R4,SVRSTEL                                                       
         XC    RSTSECY,RSTSECY                                                  
         CLI   FVILEN,0            NO INPUT - DEFAULT IS 0                      
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM       FIELD MUST BE NUMERIC IF INPUT               
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL                                                            
*                                                                               
         L     RE,BCFULL                                                        
         STCM  RE,3,RSTSECY        SAVE NEW SECURITY LEVEL                      
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SECURITY LEVEL IN HEX (SO IT MATCHES WHAT'S ON THE  *         
* SYSTEM RECORD IN CON/SEC                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SECLX    NTRDO                                                                  
*                                                                               
SECLXTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSECX)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SECURITY LEVEL IN HEX                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING RSTELD,R4                                                        
DISSECX  LA    R4,SVRSTEL          R4=SAVED 30 STATUS ELEMENT                   
         SR    RE,RE                                                            
         ICM   RE,3,RSTSECY                                                     
         LTR   RE,RE                                                            
         BZ    EXITOK                                                           
         MVC   BCWORK(1),RSTSECY+1      ONLY THE 2ND BYTE IS USED               
         GOTO1 VHEXOUT,BOPARM,BCWORK,FVIFLD,1,0                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LEVEL A ACCOUNT LENGTH                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LAL      NTRDO                                                                  
*                                                                               
LALTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLAL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLAL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL A ACCOUNT LENGTH                                      *         
***********************************************************************         
         SPACE 1                                                                
DISLAL   GOTO1 AGETEL,BOPARM,('ACLELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING ACLELD,BOELEM                                                    
         SR    RE,RE                                                            
         IC    RE,ACLVLEN                                                       
         CURED (RE),(2,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM,ZERO=BLANK              
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL A ACCOUNT LENGTH                                     *         
***********************************************************************         
         SPACE 1                                                                
VALLAL   MVC   ALEVAFLD,FVADDR     SAVE ADDRESS OF THIS FIELD                   
         TM    FVIIND,FVIVAL       FIELD CHANGE?                                
         BNZ   VLAL10                                                           
         GOTO1 =A(CHECKPW),BODMCB,RR=BORELO CHECK PASSWORD                      
         BNE   EXITL                                                            
VLAL10   CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO INPUT                             
         TM    FVIIND,FVINUM       IS IT A NUMERIC                              
         BZ    EXITNOTN            ERROR - NOT A NUMBER                         
         L     R5,BCFULL                                                        
         LTR   R5,R5               ERROR - INPUT ZERO                           
         BZ    EXITNV                                                           
         CH    R5,=H'12'                                                        
         BH    EXITNV              INVALID INPUT                                
         LA    RE,SVACLEL          SAVE ACCOUNT LENGTH ELEMENT                  
         STC   R5,SACLVLEN                                                      
         STC   R5,ACLVLEN-ACLELD(RE)                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEVEL B ACCOUNT LENGTH                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LBL      NTRDO                                                                  
*                                                                               
LBLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLBL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLBL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL B ACCOUNT LENGTH                                      *         
***********************************************************************         
         SPACE 1                                                                
DISLBL   GOTO1 AGETEL,BOPARM,('ACLELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING ACLELD,BOELEM                                                    
         LA    R1,ACLVLEN+(L'ACLVALS*1)                                         
         CLI   0(R1),0                                                          
         BE    EXITOK                                                           
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         SH    R1,=Y(L'ACLVALS)                                                 
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         SR    RE,RF                                                            
         CURED (RE),(2,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM,ZERO=BLANK              
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL B ACCOUNT LENGTH                                     *         
***********************************************************************         
         SPACE 1                                                                
VALLBL   TM    FVIIND,FVIVAL       FIELD CHANGE?                                
         BNZ   VLBL10                                                           
         GOTO1 =A(CHECKPW),BODMCB,RR=BORELO CHECK PASSWORD                      
         BNE   EXITL                                                            
         PUSH  USING                                                            
         USING ACLELD,SVACLEL                                                   
VLBL10   MVI   ACLVLEN+(L'ACLVALS*1),0                                          
         CLI   FVILEN,0                                                         
         BE    VALLBLX                                                          
         TM    FVIIND,FVINUM       IS IT A NUMERIC                              
         BZ    EXITNOTN            ERROR - NOT A NUMBER                         
         L     R4,BCFULL                                                        
         LTR   R4,R4                                                            
         BZ    VALLBLX                                                          
         SR    R0,R0                                                            
         IC    R0,ACLVLEN                                                       
         AR    R4,R0                                                            
         CH    R4,=H'12'                                                        
         BNH   VALLB10                                                          
         MVC   FVADDR,ALEVAFLD                                                  
         MVC   FVMSGNO,=AL2(AE$NOLVL)   INVALID LEVEL FORMAT                    
         B     EXITL                                                            
*                                                                               
VALLB10  STC   R4,SACLVLEN                                                      
         STC   R4,ACLVLEN+(L'ACLVALS*1)                                         
         B     *+8                                                              
VALLBLX  OI    NFILIND,NFILALBD    SET ELEMENT HAS BEEN BUILT                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEVEL C ACCOUNT LENGTH                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LCL      NTRDO                                                                  
*                                                                               
LCLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLCL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL C ACCOUNT LENGTH                                      *         
***********************************************************************         
         SPACE 1                                                                
DISLCL   GOTO1 AGETEL,BOPARM,('ACLELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING ACLELD,BOELEM                                                    
         LA    R1,ACLVLEN+(L'ACLVALS*2)                                         
         CLI   0(R1),0                                                          
         BE    EXITOK                                                           
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         SH    R1,=Y(L'ACLVALS)                                                 
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         SR    RE,RF                                                            
         CURED (RE),(2,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM,ZERO=BLANK              
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL C ACCOUNT LENGTH                                     *         
***********************************************************************         
         SPACE 1                                                                
VALLCL   TM    NFILIND,NFILALBD    HAS ELEMENT BEEN BUILT?                      
         BNZ   EXITOK              YES - EXIT                                   
         TM    FVIIND,FVIVAL       FIELD CHANGE?                                
         BNZ   VLCL10                                                           
         GOTO1 =A(CHECKPW),BODMCB,RR=BORELO CHECK PASSWORD                      
         BNE   EXITL                                                            
         PUSH  USING                                                            
         USING ACLELD,SVACLEL                                                   
VLCL10   MVI   ACLVLEN+(L'ACLVALS*2),0                                          
         CLI   FVILEN,0                                                         
         BE    VALLCLX                                                          
         TM    FVIIND,FVINUM       IS IT A NUMERIC                              
         BZ    EXITNOTN            ERROR - NOT A NUMBER                         
         L     R4,BCFULL                                                        
         LTR   R4,R4               INPUT ZERO                                   
         BZ    VALLCLX                                                          
         SR    R0,R0                                                            
         IC    R0,ACLVLEN+(L'ACLVALS*1)                                         
         AR    R4,R0                                                            
         CH    R4,=H'12'                                                        
         BNH   VALLC10                                                          
         MVC   FVADDR,ALEVAFLD                                                  
         MVC   FVMSGNO,=AL2(AE$NOLVL)   INVALID LEVEL FORMAT                    
         B     EXITL                                                            
VALLC10  STC   R4,SACLVLEN                                                      
         STC   R4,ACLVLEN+(L'ACLVALS*2)                                         
         B     *+8                                                              
VALLCLX  OI    NFILIND,NFILALBD    SET ELEMENT HAS BEEN BUILT                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEVEL D ACCOUNT LENGTH                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LDL      NTRDO                                                                  
*                                                                               
LDLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLDL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL D ACCOUNT LENGTH                                      *         
***********************************************************************         
         SPACE 1                                                                
DISLDL   GOTO1 AGETEL,BOPARM,('ACLELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING ACLELD,BOELEM                                                    
         CLI   ACLLN,ACLLN1Q+(L'ACLVALS*4)                                      
         BL    EXITOK                                                           
         LA    R1,ACLVLEN+(L'ACLVALS*3)                                         
         CLI   0(R1),0                                                          
         BE    EXITOK                                                           
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         SH    R1,=Y(L'ACLVALS)                                                 
         SR    RF,RF                                                            
         IC    RF,0(R1)                                                         
         SR    RE,RF                                                            
         CURED (RE),(2,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM,ZERO=BLANK              
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL D ACCOUNT LENGTH                                     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING ACLELD,SVACLEL                                                   
VALLDL   MVI   ACLVLEN+(L'ACLVALS*3),0                                          
         CLI   FVILEN,0                                                         
         BE    VLDL04                                                           
         CLC   =C'2P',LDGKUNT      4TH LVL NOT VALID FOR 2P AS OF               
         BE    EXITNV              JAN/05                                       
         TM    FVIIND,FVIVAL       FIELD CHANGE?                                
         BNZ   VLDL02                                                           
         GOTO1 =A(CHECKPW),BODMCB,RR=BORELO CHECK PASSWORD                      
         BNE   EXITL                                                            
VLDL02   TM    FVIIND,FVINUM       IS IT A NUMERIC                              
         BZ    EXITNOTN            ERROR - NOT A NUMBER                         
         L     R4,BCFULL                                                        
         LTR   R4,R4                                                            
         BZ    VLDL04                                                           
         SR    R0,R0                                                            
         IC    R0,ACLVLEN+(L'ACLVALS*2)                                         
         AR    R4,R0                                                            
         CH    R4,=H'12'                                                        
         BH    VLDLX                                                            
         STC   R4,SACLVLEN                                                      
         STC   R4,ACLVLEN+(L'ACLVALS*3)                                         
VLDL04   SR    RE,RE               CHECK TOTAL LENGTH OF ALL LEVELS             
         IC    RE,SACLVLEN                                                      
         CH    RE,=H'12'                                                        
         BE    VLDL06                                                           
         MVC   FVADDR,ALEVAFLD                                                  
         MVC   FVMSGNO,=AL2(AE$NOLVL)   INVALID LEVEL FORMAT                    
         B     EXITL                                                            
VLDL06   OI    NFILIND,NFILALBD    SET ELEMENT HAS BEEN BUILT                   
         B     EXITOK                                                           
*                                                                               
VLDLX    MVC   FVADDR,ALEVAFLD                                                  
         MVC   FVMSGNO,=AL2(AE$NOLVL)   INVALID LEVEL FORMAT                    
         B     EXITL                                                            
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LEVEL A ACCOUNT NAME                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LAN      NTRDO                                                                  
*                                                                               
LANTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLAN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLAN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL A ACCOUNT NAME                                        *         
***********************************************************************         
         SPACE 1                                                                
DISLAN   GOTO1 AGETEL,BOPARM,('ACLELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING ACLELD,BOELEM                                                    
         MVC   FVIFLD(L'ACLVDESC),ACLVDESC                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL A ACCOUNT NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
VALLAN   CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO LEVEL A NAME                      
         LA    RE,SVACLEL          SAVE ACC LENGTH ELEMENT                      
         MVC   ACLVDESC-ACLELD(L'ACLVDESC,RE),FVIFLD                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEVEL B ACCOUNT NAME                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LBN      NTRDO                                                                  
*                                                                               
LBNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLBN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLBN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL B ACCOUNT NAME                                        *         
***********************************************************************         
         SPACE 1                                                                
DISLBN   GOTO1 AGETEL,BOPARM,('ACLELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING ACLELD,BOELEM                                                    
         MVC   FVIFLD(L'ACLVDESC),ACLVDESC+(L'ACLVALS*1)                        
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL B ACCOUNT NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
VALLBN   DS    0H                                                               
         PUSH  USING                                                            
         USING ACLELD,SVACLEL                                                   
         MVC   ACLVDESC+(L'ACLVALS*1),BCSPACES                                  
         CLI   ACLVLEN+(L'ACLVALS*1),0                                          
         BE    EXITOK              EXIT - NO LEVEL B LENGTH                     
         CLI   FVILEN,0                                                         
         BE    EXITNO              NO INPUT                                     
         MVC   ACLVDESC+(L'ACLVALS*1)(L'ACLVDESC),FVIFLD                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEVEL C ACCOUNT NAME                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LCN      NTRDO                                                                  
*                                                                               
LCNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLCN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL C ACCOUNT NAME                                        *         
***********************************************************************         
         SPACE 1                                                                
DISLCN   GOTO1 AGETEL,BOPARM,('ACLELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING ACLELD,BOELEM                                                    
         MVC   FVIFLD(L'ACLVDESC),ACLVDESC+(L'ACLVALS*2)                        
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL C ACCOUNT NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
VALLCN   DS    0H                                                               
         PUSH  USING                                                            
         USING ACLELD,SVACLEL                                                   
         MVC   ACLVDESC+(L'ACLVALS*2),BCSPACES                                  
         CLI   ACLVLEN+(L'ACLVALS*2),0                                          
         BE    EXITOK              EXIT - NO LEVEL C LENGTH                     
         CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO NAME                              
         MVC   ACLVDESC+(L'ACLVALS*2)(L'ACLVDESC),FVIFLD                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEVEL D ACCOUNT NAME                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LDN      NTRDO                                                                  
*                                                                               
LDNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLDN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL D ACCOUNT NAME                                        *         
***********************************************************************         
         SPACE 1                                                                
DISLDN   GOTO1 AGETEL,BOPARM,('ACLELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING ACLELD,BOELEM                                                    
         MVC   FVIFLD(L'ACLVDESC),ACLVDESC+(L'ACLVALS*3)                        
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL D ACCOUNT NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
VALLDN   DS    0H                                                               
         PUSH  USING                                                            
         USING ACLELD,SVACLEL                                                   
         MVC   ACLVDESC+(L'ACLVALS*3),BCSPACES                                  
         CLI   ACLVLEN+(L'ACLVALS*3),0                                          
         BE    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO NAME                              
         MVC   ACLVDESC+(L'ACLVALS*3)(L'ACLVDESC),FVIFLD                        
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INCOME CASH DISCOUNT ACCOUNT                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CDA      NTRDO                                                                  
*                                                                               
CDATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCDSAC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCDSAC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INCOME CASH DISCOUNT ACCOUNT                                *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
DISCDSAC LA    R4,SVLDGEL          A(LDGEL)                                     
         OC    LDGCDSC,LDGCDSC                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'LDGCDSC),LDGCDSCU    CASH DISCOUNT ACCOUNT              
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INCOME CASH DISCOUNT ACCOUNT                               *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
VALCDSAC LA    R4,SVLDGEL          R4=SAVED 14 LEDGER ELEMENT                   
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         XC    LDGCDSC,LDGCDSC                                                  
         B     EXITOK                                                           
*                                                                               
         MVC   FVMSGNO,=AL2(AE$NOTSI)                                           
         CLC   FVIFLD(2),=C'SI'    MUST BE SI ACCOUNT                           
         BNE   EXITL                                                            
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     CHECK IF ACCOUNT EXISTS                      
         MVC   ACTKCPY,LDGKCPY                                                  
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         MVC   ACTKUNT(0),FVIFLD                                                
         EX    RF,*-6                                                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                       READ CASH DISCOUNT ACCOUNT             
         BE    *+14                INVALID ACCOUNT                              
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     EXITL                                                            
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
         L     RE,AIO1             SEARCH FOR ACCOUNT BALANCE ELEMENT           
         GOTO1 AGETEL,BOPARM,('ABLELQ',(RE)),0                                  
         BNE   EXITNV              ERROR - CAN'T FIND ABLEL                     
         XC    LDGCDSC,LDGCDSC                                                  
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,LDGCDSC,FVIFLD                                                
         OC    LDGCDSC,BCSPACES                                                 
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INCOME CASH DISCOUNT ACCOUNT NAME                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CDAN     NTRDO                                                                  
*                                                                               
CDANTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCDNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INCOME CASH DISCOUNT ACCOUNT NAME                           *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R3                                                       
         USING LDGELD,R4                                                        
DISCDNM  LA    R3,IOKEY            READ THE COMPANY RECORD                      
         LA    R4,SVLDGEL          R4=SAVED 14 LEDGER ELEMENT                   
         OC    LDGCDSC,LDGCDSC                                                  
         BZ    EXITOK                                                           
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN    COMPANY                                        
         MVC   ACTKUNT(L'LDGCDSC),LDGCDSC    UNIT/LEDGER/SI ACCOUNT             
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             GET COMPANY NAME                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NEXT BILL NUMBER                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NXTB     NTRDO                                                                  
*                                                                               
NXTBTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISNXTB)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNXTB)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NEXT BILL NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
DISNXTB  GOTO1 AGETEL,BOPARM,('PMDELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING PMDELD,BOELEM                                                    
         MVC   FVIFLD(L'PMDLBILL),PMDLBILL LAST BILL NUMBER                     
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE NEXT BILL NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALNXTB  GOTO1 ADELEL,BOPARM,('PMDELQ',LDGRECD),0                               
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,6            MUST BE 6 NUMBERS                            
         BNE   EXITNV                                                           
         TM    FVIIND,FVINUM       IS IT A NUMBER?                              
         BZ    EXITNOTN                                                         
*                                                                               
T        USING PMDELD,BOELEM                                                    
         XC    BOELEM,BOELEM                                                    
         MVI   T.PMDEL,PMDELQ                                                   
         MVI   T.PMDLN,PMDLN1Q                                                  
         MVC   T.PMDCODE(46),BCSPACES                                           
         MVI   T.PMDANAL,C' '                                                   
         MVC   T.PMDFBILL,FVIFLD                                                
         MVC   T.PMDLBILL,FVIFLD                                                
         GOTO1 AADDEL,BOPARM,LDGRECD                                            
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RESET BILL NUMBER                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RESB     NTRDO                                                                  
*                                                                               
RESBTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRESB)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRESB)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RESET BILL NUMBER                                           *         
***********************************************************************         
         SPACE 1                                                                
DISRESB  GOTO1 AGETEL,BOPARM,('PMDELQ',LDGRECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING PMDELD,BOELEM                                                    
         MVC   FVIFLD(L'PMDRBILL),PMDRBILL RESET BILL NUMBER                    
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* VALIDATE RESET BILL NUMBER                                          *         
***********************************************************************         
         SPACE 1                                                                
VALRESB  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('PMDELQ',LDGRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         CLI   FVILEN,0            ERROR - NO RESET BILL NUMBER                 
         BE    EXITNO                                                           
         CLI   FVILEN,4            MUST BE 4 NUMBERS                            
         BNE   EXITNV                                                           
         TM    FVIIND,FVINUM       IS IT A NUMBER?                              
         BZ    EXITNOTN                                                         
         L     RF,12(,R1)                                                       
         USING PMDELD,RF                                                        
         MVC   PMDRBILL,FVIFLD     RESET BILL#                                  
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EXPENSE ANALYSIS                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
EXPA     NTRDO                                                                  
*                                                                               
EXPATBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISXANL)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALXANL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EXPENSE ANALYSIS MASK                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
DISXANL  CLC   LDGKUNT(2),=C'SE'   ONLY FOR SE                                  
         BNE   EXITOK                                                           
         LA    R4,SVLDGEL          R4=SAVED 14 LEDGER ELEMENT                   
         MVI   FVIFLD,C'N'                                                      
         MVC   FVIFLD+1(11),FVIFLD                                              
         LA    R0,12                                                            
         LA    RF,FVIFLD                                                        
         SR    R1,R1                                                            
         ICM   R1,3,LDGXMSK        12 BIT MASK                                  
         SLL   R1,15                                                            
*                                                                               
         SLL   R1,1                SHIFT TO HIGH ORDER OF R1                    
         LTR   R1,R1               IS IT ON                                     
         BNM   *+8                                                              
         MVI   0(RF),C'Y'          POSITION IS PART OF CATEGORY                 
         LA    RF,1(RF)                                                         
         BCT   R0,*-18                                                          
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE EXPENSE ANALYSIS FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
VALXANL  CLC   LDGKUNT(2),=C'SE'   ONLY FOR SE                                  
         BNE   EXITOK                                                           
         LA    R4,SVLDGEL          R4=SAVED 14 LEDGER ELEMENT                   
         XC    LDGXMSK,LDGXMSK                                                  
         CLI   FVILEN,0            TEST NO INPUT IN EITHER FIELD                
         BE    EXITOK                                                           
         LA    R0,12                                                            
         SR    R5,R5                                                            
         SR    R3,R3                                                            
         LA    RF,FVIFLD                                                        
*                                                                               
VXANL10  CLI   0(RF),C'N'           MUST BE N OR Y                              
         BE    VXANL20                                                          
         CLI   0(RF),C'Y'                                                       
         BNE   EXIT                ERROR                                        
         O     R3,=X'00000001'                                                  
         AH    R5,=H'1'            COUNT NUMBER OF POSITIONS                    
         CH    R5,=H'5'            NOT MORE THAN 5                              
         BH    EXITNV                                                           
*                                                                               
VXANL20  SLL   R3,1                                                             
         LA    RF,1(RF)                                                         
         BCT   R0,VXANL10                                                       
         SLL   R3,3                                                             
         STCM  R3,3,LDGXMSK                                                     
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COST POSTINGS                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CSTP     NTRDO                                                                  
*                                                                               
CSTPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOSP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOSP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY COST POSTINGS FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
DISCOSP  CLC   LDGKUNT(2),=C'SE'   ONLY FOR SE                                  
         BNE   EXITOK                                                           
         LA    R4,SVLDGEL          R4=SAVED 14 LEDGER ELEMENT                   
         MVI   FVIFLD,C'N'         SET POSTING INDICATOR                        
         TM    LDGSTAT,X'04'                                                    
         BNO   *+8                                                              
         MVI   FVIFLD,C'Y'                                                      
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE COST POSTINGS FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
VALCOSP  CLC   LDGKUNT(2),=C'SE'   ONLY FOR SE                                  
         BNE   EXITOK                                                           
         LA    R4,SVLDGEL          R4=SAVED 14 LEDGER ELEMENT                   
         CLI   FVILEN,0                                                         
         BNE   VALCOS5                                                          
         OC    LDGXMSK,LDGXMSK     IF THERE'S A MASK MUST ENTER                 
         BZ    EXITOK              WHETHER WANT TO MAKE COST POSTINGS           
         B     EXITNO              OR NOT                                       
*                                                                               
VALCOS5  NI    LDGSTAT,X'FF'-LDGSCOST                                           
         CLI   FVIFLD,C'N'        NO COST POSTINGS                              
         BE    EXITOK                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV              ERROR                                        
         OI    LDGSTAT,X'04'                                                    
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NAME SEARCH                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NAMS     NTRDO                                                                  
*                                                                               
NAMSTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAMSH)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNAMSH)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NAME SEARCH                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
DISNAMSH MVI   FVIFLD,C' '         DEFAULT IS BLANK                             
         LA    R4,SVLDGEL          A(LDGEL)                                     
         TM    LDGSTAT2,LDGSSRCH    USE LEDGER FOR SEARCH?                      
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@YES),AC@YES                                          
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE NAME SEARCH                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
VALNAMSH LA    R4,SVLDGEL          R4 POINTS TO SAVED 14 ELEMENT                
         NI    LDGSTAT2,X'FF'-LDGSSRCH                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK               NO INPUT - DEFAULT IS NO                    
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),AC@NO                                                  
*                                                                               
         EX    RF,*+8                                                           
         BNE   EXITNV                                                           
         CLC   FVIFLD(0),AC@YES    YES OR NO                                    
         OI    LDGSTAT2,LDGSSRCH   USE LEDGER FOR SEARCH                        
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEVELS ON LIST SCREEN                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LVLL     NTRDO                                                                  
*                                                                               
LVLLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLLEV)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVELS FOR LIST                                             *         
***********************************************************************         
         SPACE 1                                                                
DISLLEV  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE 'A' NAME                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULAN    NTRDO                                                                  
*                                                                               
RULANTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRUNMA)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRUNMA)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACC EQUIV RULE 'A' NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DISRUNMA LA    R4,LDGRECD+(LDGRFST-LDGRECD)                                     
         MVI   CHKSEQ,0            SAVE SEQ # FOR DISPLAYING RULE               
DRUNA5   CLI   0(R4),0                                                          
         BE    EXITOK                                                           
         CLI   0(R4),X'CA'         ACC EQUIVALENT RULES ELEMENT                 
         BNE   *+14                                                             
         CLC   APRSEQ,CHKSEQ       WANT SEQ NUMBER 0                            
         BE    DRUNA10                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DRUNA5                                                           
*                                                                               
DRUNA10  MVC   FVIFLD(L'APRDESC),APRDESC                                        
         OC    FVIFLD,BCSPACES                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACC EQUIV RULE 'A' NAME                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
VALRUNMA DS    0H                 DELETE ALL 'CA' ELEMENTS FIRST                
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('APRELQ',LDGRECD),0               
         XC    SVAPREL,SVAPREL     CLEAR SAVE CA ELEMENT                        
         CLI   FVILEN,0            ANY NAME?                                    
         BE    EXITOK                                                           
*                                                                               
         LA    R4,SVAPREL          BUILD ELEMENT IN SAVED STORAGE FIRST         
         MVI   APREL,APRELQ        FOR NOW FILL IN WHAT WE CAN                  
         MVI   APRSEQ,0                                                         
         ZIC   RF,FVXLEN                                                        
         EXMVC RF,APRDESC,FVIFLD   MOVE IN EQUIV ACCOUNT NAME                   
         OC    APRDESC,BCSPACES                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE 'B' NAME                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULBN    NTRDO                                                                  
*                                                                               
RULBNTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRUNMB)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRUNMB)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACC EQUIV RULE 'B' NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DISRUNMB LA    R4,LDGRECD+(LDGRFST-LDGRECD)                                     
         MVI   CHKSEQ,1            SAVE SEQ # FOR DISPLAYING RULE               
DRUNB5   CLI   0(R4),0                                                          
         BE    EXITOK                                                           
         CLI   0(R4),X'CA'         ACC EQUIVALENT RULES ELEMENT                 
         BNE   *+14                                                             
         CLC   APRSEQ,CHKSEQ       WANT SEQ NUMBER 1                            
         BE    DRUNB10                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DRUNB5                                                           
*                                                                               
DRUNB10  MVC   FVIFLD(L'APRDESC),APRDESC                                        
         OC    FVIFLD,BCSPACES                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACC EQUIV RULE 'B' NAME                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
VALRUNMB XC    SVAPREL,SVAPREL     CLEAR SAVE CA ELEMENT                        
         CLI   FVILEN,0            ANY NAME?                                    
         BE    EXITOK                                                           
*                                                                               
         LA    R4,SVAPREL          BUILD ELEMENT IN SAVED STORAGE FIRST         
         MVI   APREL,APRELQ        FOR NOW FILL IN WHAT WE CAN                  
         MVI   APRSEQ,1                                                         
         ZIC   RF,FVXLEN                                                        
         EXMVC RF,APRDESC,FVIFLD   MOVE IN EQUIV ACCOUNT NAME                   
         OC    APRDESC,BCSPACES                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE 'C' NAME                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULCN    NTRDO                                                                  
*                                                                               
RULCNTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRUNMC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRUNMC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACC EQUIV RULE 'C' NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DISRUNMC LA    R4,LDGRECD+(LDGRFST-LDGRECD)                                     
         MVI   CHKSEQ,2            SAVE SEQ # FOR DISPLAYING RULE               
DRUNC5   CLI   0(R4),0                                                          
         BE    EXITOK                                                           
         CLI   0(R4),X'CA'         ACC EQUIVALENT RULES ELEMENT                 
         BNE   *+14                                                             
         CLC   APRSEQ,CHKSEQ       WANT SEQ NUMBER 2                            
         BE    DRUNC10                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DRUNC5                                                           
*                                                                               
DRUNC10  MVC   FVIFLD(L'APRDESC),APRDESC                                        
         OC    FVIFLD,BCSPACES                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACC EQUIV RULE 'C' NAME                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
VALRUNMC XC    SVAPREL,SVAPREL     CLEAR SAVE CA ELEMENT                        
         CLI   FVILEN,0            ANY NAME?                                    
         BE    EXITOK                                                           
*                                                                               
         LA    R4,SVAPREL          BUILD ELEMENT IN SAVED STORAGE FIRST         
         MVI   APREL,APRELQ        FOR NOW FILL IN WHAT WE CAN                  
         MVI   APRSEQ,2                                                         
         ZIC   RF,FVXLEN                                                        
         EXMVC RF,APRDESC,FVIFLD   MOVE IN EQUIV ACCOUNT NAME                   
         OC    APRDESC,BCSPACES                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE 'D' NAME                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULDN    NTRDO                                                                  
*                                                                               
RULDNTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRUNMD)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRUNMD)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACC EQUIV RULE 'D' NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DISRUNMD LA    R4,LDGRECD+(LDGRFST-LDGRECD)                                     
         MVI   CHKSEQ,3            SAVE SEQ # FOR DISPLAYING RULE               
DRUND5   CLI   0(R4),0                                                          
         BE    EXITOK                                                           
         CLI   0(R4),X'CA'         ACC EQUIVALENT RULES ELEMENT                 
         BNE   *+14                                                             
         CLC   APRSEQ,CHKSEQ       WANT SEQ NUMBER 3                            
         BE    DRUND10                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DRUND5                                                           
*                                                                               
DRUND10  MVC   FVIFLD(L'APRDESC),APRDESC                                        
         OC    FVIFLD,BCSPACES                                                  
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE ACC EQUIV RULE 'D' NAME                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
VALRUNMD XC    SVAPREL,SVAPREL     CLEAR SAVE CA ELEMENT                        
         CLI   FVILEN,0            ANY NAME?                                    
         BE    EXITOK                                                           
*                                                                               
         LA    R4,SVAPREL          BUILD ELEMENT IN SAVED STORAGE FIRST         
         MVI   APREL,APRELQ        FOR NOW FILL IN WHAT WE CAN                  
         MVI   APRSEQ,3                                                         
         ZIC   RF,FVXLEN                                                        
         EXMVC RF,APRDESC,FVIFLD   MOVE IN EQUIV ACCOUNT NAME                   
         OC    APRDESC,BCSPACES                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE 'E' NAME                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULEN    NTRDO                                                                  
*                                                                               
RULENTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISRUNME)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRUNME)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACC EQUIV RULE 'E' NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DISRUNME LA    R4,LDGRECD+(LDGRFST-LDGRECD)                                     
         MVI   CHKSEQ,4            SAVE SEQ # FOR DISPLAYING RULE               
DRUNE5   CLI   0(R4),0                                                          
         BE    EXITOK                                                           
         CLI   0(R4),X'CA'         ACC EQUIVALENT RULES ELEMENT                 
         BNE   *+14                                                             
         CLC   APRSEQ,CHKSEQ       WANT SEQ NUMBER 4                            
         BE    DRUNE10                                                          
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DRUNE5                                                           
*                                                                               
DRUNE10  MVC   FVIFLD(L'APRDESC),APRDESC                                        
         OC    FVIFLD,BCSPACES                                                  
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE ACC EQUIV RULE 'E' NAME                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
VALRUNME XC    SVAPREL,SVAPREL     CLEAR SAVE CA ELEMENT                        
         CLI   FVILEN,0            ANY NAME?                                    
         BE    EXITOK                                                           
*                                                                               
         LA    R4,SVAPREL          BUILD ELEMENT IN SAVED STORAGE FIRST         
         MVI   APREL,APRELQ        FOR NOW FILL IN WHAT WE CAN                  
         MVI   APRSEQ,4                                                         
         ZIC   RF,FVXLEN                                                        
         EXMVC RF,APRDESC,FVIFLD   MOVE IN EQUIV ACCOUNT NAME                   
         OC    APRDESC,BCSPACES                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE A                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULA     NTRDO                                                                  
*                                                                               
RULATBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DQRULA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VQRULA)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCT EQUIVALENT RULE A                                      *         
***********************************************************************         
         SPACE 1                                                                
DQRULA   MVI   CHKSEQ,0                                                         
         BRAS  RE,DRULE                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACC EQUIV RULE A                                           *         
***********************************************************************         
         SPACE 1                                                                
VQRULA   BRAS  RE,VALEQRUL                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE B                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULB     NTRDO                                                                  
*                                                                               
RULBTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DQRULB)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VQRULB)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCT EQUIVALENT RULE B                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DQRULB   MVI   CHKSEQ,1                                                         
         BRAS  RE,DRULE                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACC EQUIV RULE B                                           *         
***********************************************************************         
         SPACE 1                                                                
VQRULB   BRAS  RE,VALEQRUL                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE C                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULC     NTRDO                                                                  
*                                                                               
RULCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DQRULC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VQRULC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCT EQUIVALENT RULE C                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DQRULC   MVI   CHKSEQ,2                                                         
         BRAS  RE,DRULE                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACC EQUIV RULE C                                           *         
***********************************************************************         
         SPACE 1                                                                
VQRULC   BRAS  RE,VALEQRUL                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE D                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULD     NTRDO                                                                  
*                                                                               
RULDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DQRULD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VQRULD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCT EQUIVALENT RULE D                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DQRULD   MVI   CHKSEQ,3                                                         
         BRAS  RE,DRULE                                                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACC EQUIV RULE D                                           *         
***********************************************************************         
         SPACE 1                                                                
VQRULD   BRAS  RE,VALEQRUL                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCOUNT EQUIVALENT RULE E                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULE     NTRDO                                                                  
*                                                                               
RULETBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DQRULE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VQRULE)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCT EQUIVALENT RULE E                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DQRULE   MVI   CHKSEQ,4                                                         
         BRAS  RE,DRULE                                                         
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE ACC EQUIV RULE E                                           *         
***********************************************************************         
         SPACE 1                                                                
VQRULE   BRAS  RE,VALEQRUL                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR 'ADD ACCOUNTS AS DRAFT'                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DRFD     NTRDO                                                                  
*                                                                               
DRFDTBL  DC      AL1(DDIS),AL1(0,0,0),AL4(DISDRF)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDRF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 'ADD ACCOUNTS AS DRAFT'                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
DISDRF   CLI   LDGKUNT,C'S'        UNIT S ONLY                                  
         BNE   EXITOK                                                           
         LA    R4,SVLDGEL          SAVE LEDGER ELEMENT                          
*                                                                               
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    LDGSTAT2,LDGSDRFT                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 'ADD ACCOUNTS AS DRAFT'                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGELD,R4                                                        
VALDRF   LA    R4,SVLDGEL          SAVE LEDGER ELEMENT                          
                                                                                
         NI    LDGSTAT2,FF-LDGSDRFT                                             
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - DEFAULT IS NO                     
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
                                                                                
         CLC   LDGKUNT(2),=C'SJ'   IS IT UNIT/LEDGER SJ ?                       
         BNE   EXITNA                                                           
                                                                                
         CLC   FVIFLD(1),BC@YES    YES OR NO                                    
         BNE   EXITNV1                                                          
         OI    LDGSTAT2,LDGSDRFT                                                
         B     EXITOK                                                           
*                                                                               
EXITNV1  MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
*                                                                               
EXITNA   MVC   FVMSGNO,=AL2(AE$INPNA)                                           
         B     EXITL               EXIT WITH INPUT NOT ALLOWED SET              
         DROP  R4                                                               
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR SLUSH ACCOUNT (FOR PAYABLE LEDGERS)                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SLSA     NTRDO                                                                  
*                                                                               
SLSATBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSLSAC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSLSAC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SLUSH ACCOUNT                                               *         
***********************************************************************         
         SPACE 1                                                                
DISSLSAC GOTO1 AGETFFT,BOPARM,LDGRECD,FFTTSLAC                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SLUSH ACCOUNT                                              *         
***********************************************************************         
         SPACE 1                                                                
VALSLSAC CLI   FVILEN,0                                                         
         BE    VSLS10                                                           
*                                                                               
         USING ACTRECD,R3                                                       
         LA    R3,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     CHECK IF ACCOUNT EXISTS                      
         MVC   ACTKCPY,LDGKCPY                                                  
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         MVC   ACTKUNT(0),FVIFLD                                                
         EX    RF,*-6                                                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                       READ CASH DISCOUNT ACCOUNT             
         BE    *+14                INVALID ACCOUNT                              
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     EXITL                                                            
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST RECORD                            
         L     RE,AIO1             SEARCH FOR ACCOUNT BALANCE ELEMENT           
         GOTO1 AGETEL,BOPARM,('ABLELQ',(RE)),0                                  
         BNE   EXITNV              ERROR - CAN'T FIND ABLEL                     
*                                                                               
VSLS10   GOTO1 ABLDFFT,BOPARM,LDGRECD,FFTTSLAC                                  
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CALL BASE FOR AGETLDG                                              
* ON ENTRY - BCHALF CONTAINS THE GB OR GP U/L FROM SCANNER                      
***********************************************************************         
         SPACE 1                                                                
X        USING LDGRECD,RF                                                       
LDGRD    NTR1  BASE=*,LABEL=*                                                   
         LA    RF,IOKEY                                                         
         MVC   X.LDGKEY,BCSPACES                                                
         MVC   X.LDGKCPY,CUABIN                                                 
         MVC   X.LDGKUNT(L'LDGKUNT+L'LDGKLDG),BCHALF                            
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOREAD+XOACCDIR+XIO1    RE-READ THE LEDGER REC                
         GOTO1 AIO                        TO RE-ESTABLISH READ SEQ              
         XIT1                                                                   
         DROP  X                                                                
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO MOVE THE ACCOUNT EQUIVALENT RULE INTO FVIFLD FOR                   
* DISPLAYING                                                                    
* EXIT - FVIFLD CONTAINS THE ACCT EQU RULE                                      
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
DRULE    NTR1  BASE=*,LABEL=*                                                   
         LA    R4,LDGRECD+(LDGRFST-LDGRECD)                                     
DRUL05   CLI   0(R4),0                                                          
         BE    DRULX                                                            
         CLI   0(R4),X'CA'         ACC EQUIVALENT RULES ELEMENT                 
         BNE   *+14                                                             
         CLC   APRSEQ,CHKSEQ      MAKE SURE IT'S THE RIGHT ELEM                 
         BE    DRUL10                                                           
         ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DRUL05                                                           
*                                                                               
* PUT CODE HERE TO OUTPUT THE RULES MYSELF BECAUSE UNSCAN SUCKS                 
*                                                                               
DRUL10   LA    R3,FVIFLD                                                        
         ZIC   RF,APRNLEVS         NUMBER OF RULES                              
         LA    RE,APRMLEN          POINT RE TO FIRST MINI ELEM                  
DRUL20   ZIC   R1,0(RE)            LENGTH OF RULE                               
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),1(RE)      MOVE TO OUTPUT FIELD                         
         LA    R1,1(R1)            BUMP BACK TO REAL LENGTH                     
         AR    RE,R1               BUMP TO                                      
         LA    RE,1(RE)            NEXT MINI ELEMENT                            
         AR    R3,R1               BUMP TO NEXT OUTPUT SLOT                     
         LR    R1,RF               SEE IF ANY MORE RULES TO DISPLAY             
         BCTR  R1,0                                                             
         LTR   R1,R1               ANY MORE RULES?                              
         BZ    DRULX                                                            
         MVI   0(R3),C','          INSERT COMMA                                 
         LA    R3,1(R3)                                                         
         BCT   RF,DRUL20                                                        
*                                                                               
DRULX    XIT1                                                                   
         DROP  R4                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO VALIDATE THE ACCOUNT EQUIVALENT RULE                               
* EXIT - 'CA' ELEMENT IS ADDED TO RECORD OR FVMSGNO CONTAINS ERROR              
*         MESSAGE                                                               
***********************************************************************         
         SPACE 1                                                                
         USING APRELD,R4                                                        
VALEQRUL NTR1  BASE=*,LABEL=*     R4 POINTS TO SAVED STORAGE WHERE              
         LA    R4,SVAPREL         CA ELEMENT IS BEING BUILT                     
         OC    APRDESC,APRDESC     ANY DESCRIPTION?                             
         BZ    VEQUE               NO-THAN CAN'T HAVE ANY RULE                  
*                                                                               
         TM    FVIIND,FVIVAL       FIELD CHANGE?                                
         BNZ   VEQRU05                                                          
         GOTO1 =A(CHECKPW),BODMCB,RR=BORELO  CHECK PASSWORD                     
         BNE   VEQUL                                                            
VEQRU05  CLI   FVILEN,0            ANYTHING ENTERED?                            
         BNE   VEQRU10                                                          
         OC    APRDESC,APRDESC     IF ENTERED AN ACCT NAME                      
         BZ    VEQUE                                                            
         MVC   FVMSGNO,=AL2(AE$MISIF)  MUST ENTER A RULE                        
         B     VEQUL                                                            
*                                                                               
VEQRU10  BRAS  RE,CHKEQUL         MAKE SURE EQU'S VALID FOR UL                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$EQUUL)                                           
         B     VEQUL                                                            
*                                                                               
         TM    FVIIND,FVIVAL       FIELD CHANGE?                                
         BNZ   VEQRU12                                                          
         GOTO1 =A(CHECKPW),BODMCB,RR=BORELO  CHECK PASSWORD                     
         BNE   VEQUL                                                            
VEQRU12  LA    R3,BLOCK                                                         
         LR    RE,R3               CLEAR SCANNER BLOCK                          
         LA    RF,SCANMAX*SCBLKLQ                                               
         XCEF                                                                   
         GOTO1 VSCANNER,BOPARM,FVIHDR,BLOCK                                     
         CLI   BOPARM+4,0                                                       
         BE    VEQUERR2                                                         
         ZIC   RF,BOPARM+4         RF=# OF SCANNER LINES                        
         CHI   RF,9                CANNOT BE MORE THAN 9 LEVELS                 
         BH    VEQUERR1                                                         
         STC   RF,SVSCLINE                                                      
         STC   RF,APRNLEVS         FILL IN # OF LEVELS TO ELEMENT               
*                                                                               
         LA    R5,APRMLEN          R5 POINTS TO MINI ELEMENT                    
         LA    R3,BLOCK                                                         
         USING SCANBLKD,R3                                                      
VEQRU15  CLI   SC2NDLEN,0          SHOULD NEVER BE A RHS ENTRY                  
         BNE   VEQUERR                                                          
         MVI   FIRST,C'Y'          SET FIRST TIME THROUGH                       
         ZIC   R1,SC1STLEN         R1=LENGTH OF FIRST LEVEL                     
         STC   R1,0(R5)            STORE LENGTH OF LEVEL IN ELEMENT             
         LA    RE,SC1STFLD         RE POINTS TO TO THE FIELD                    
VEQRU20  CLI   FIRST,C'Y'          FIRST TIME THROUGH?                          
         BNE   VEQRU30                                                          
         CLI   0(RE),C' '          THE FIRST CHAR CANNOT BE A SPACE             
         BE    VEQUERR                                                          
         B     *+12                                                             
VEQRU30  CLI   0(RE),C' '          BUT OTHER CHARS CAN BE A SPACE               
         BE    VEQRUNX                                                          
         CLI   0(RE),C'#'          # MEANS A NUMERIC VALUE                      
         BE    VEQRUNX                                                          
         CLI   0(RE),X'81'         LOWERCASE 'A' MEANS ALPHA ONLY               
         BE    VEQRUNX                                                          
         CLI   0(RE),X'83'         LOWERCASE 'C' MEANS ALPHANUMERIC             
         BE    VEQRUNX                                                          
         CLI   0(RE),X'C1'         VALID CONSTANT VALUES ARE A-Z                
         BL    VEQUERR                                                          
         CLI   0(RE),X'F9'         AND 0-9                                      
         BH    VEQUERR                                                          
VEQRUNX  MVI   FIRST,C'N'          FIRST TIME IS OVER                           
         LA    RE,1(RE)            BUMP TO NEXT POSITION IN RULE                
         BCT   R1,VEQRU20                                                       
*                                                                               
         ZIC   R1,SC1STLEN         R1=LENGTH OF FIRST LEVEL                     
         BCTR  R1,0                                                             
         EXMVC R1,1(R5),SC1STFLD   MOVE IN MASK TO ELEMENT                      
         LA    R1,1(R1)            BUMP BACK LENGTH                             
         AR    R5,R1               POINT TO NEXT MINI MASK ELEMENT              
         LA    R5,1(R5)                                                         
         ZIC   R1,SC1STLEN         R1=LENGTH OF FIRST LEVEL                     
         ZIC   RE,APRTLEN          ADD TO TOTAL LENGTH OF RULES                 
         AR    R1,RE                                                            
         STC   R1,APRTLEN                                                       
         CHI   R1,20               20 BYTE RULE MAX                             
         BH    VEQUERR2                                                         
*                                                                               
         LA    R3,SCBLKLQ(R3)       BUMP TO NEXT SCANNER LINE                   
         ZIC   R1,SVSCLINE         ANY MORE RULES TO PROCESS?                   
         AHI   R1,-1                                                            
         STC   R1,SVSCLINE                                                      
         BNZ   VEQRU15             YES                                          
*                                                                               
* NO MORE RULES SO COPY ELEMENT TO BOELEM.  FILL IN TOTAL LENGTH OF             
* ELEM AND ADD ELEM                                                             
*                                                                               
         ZIC   R1,APRTLEN          TOTAL LENGTH OF RULES                        
         ZIC   RE,APRNLEVS         NUMBER OF LEVELS                             
         AR    R1,RE                                                            
         AHI   R1,APRLN1Q          ADD OVERHEAD                                 
         STC   R1,APRLN            STORE ELEMENT LENGTH                         
         XC    BOELEM,BOELEM                                                    
         MVC   BOELEM(L'SVAPREL),SVAPREL                                        
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),LDGRECD,BOELEM,0                   
         B     VEQUE                                                            
*                                                                               
VEQUERR  MVC   FVMSGNO,=AL2(AE$IRULE) INVALID RULE                              
         B     VEQUL                                                            
*                                                                               
VEQUERR1 MVC   FVMSGNO,=AL2(AE$9MAX)  MAX OF 9 LEVELS ALLOWED                   
         B     VEQUL                                                            
*                                                                               
VEQUERR2 MVC   FVMSGNO,=AL2(AE$20MAX) FIELD LEN CANNOT EXCEED 20                
         B     VEQUL                  CHARACTERS                                
*                                                                               
VEQUL    CLI   *,FF                                                             
         B     VEQUX                                                            
VEQUE    CR    RB,RB                                                            
VEQUX    XIT1                                                                   
         DROP  R3,R4                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* MAKE SURE ACCT EQU CODES ARE VALID FOR LEDGER                                 
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
CHKEQUL  NTR1  BASE=*,LABEL=*                                                   
         LA    RF,EQUUL                                                         
CEQU10   CLI   0(RF),X'FF'                                                      
         BE    CEQU20                                                           
         CLC   0(L'ACTKUNT+L'ACTKLDG,RF),ACTKUNT                                
         BE    CEQUE                                                            
         LA    RF,2(RF)                                                         
         B     CEQU10                                                           
*                                                                               
CEQU20   CLI   ACTKUNT,C'S'        UNIT S?                                      
         BNE   CEQUL                                                            
         CLI   ACTKLDG,C'9'        ALL LEDGERS FOR UNIT S VALID EXCEPT          
         BNE   CEQUE               S9                                           
*                                                                               
CEQUL    CLI   *,FF                                                             
         B     CEQUX                                                            
CEQUE    CR    RB,RB                                                            
CEQUX    XIT1                                                                   
*                                                                               
EQUUL    DC    C'GBGP1C1N1R2D28'                                                
         DC    X'FF'                                                            
         DROP  R2                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE LEDGER ENTERED BASED ON UNIT CODE                                    
* FVIFLD CONTAINS THE UL ENTERED                                                
***********************************************************************         
         SPACE 1                                                                
CHKUL    NTR1  BASE=*,LABEL=*                                                   
         LA    RF,UNTSXT                                                        
         CLI   FVIFLD,C'S'         UNIT S?                                      
         BNE   CHKUL20                                                          
CHKUL10  CLI   0(RF),X'FF'                                                      
         BE    CHKULE                                                           
         CLC   0(1,RF),FVIFLD+1    IF FIND MATCH THAN ENTERED AN                
         BE    CHKULL              INVALID LEDGER FOR UNIT S                    
         LA    RF,1(RF)                                                         
         B     CHKUL10                                                          
*                                                                               
CHKUL20  LA    RF,UNTGT                                                         
         CLI   FVIFLD,C'G'         UNIT G?                                      
         BE    CHKUL30                                                          
         LA    RF,UNT1T                                                         
         CLI   FVIFLD,C'1'         UNIT 1?                                      
         BE    CHKUL30                                                          
         LA    RF,UNT2T                                                         
         CLI   FVIFLD,C'2'         UNIT 2?                                      
         BNE   CHKULE              ANYTHING GOES FOR OTHER LEDGERS              
*                                                                               
CHKUL30  CLI   0(RF),X'FF'         IF DON'T FIND MATCH THAN ENTERED AN          
         BE    CHKULL              INVALID LEDGER FOR UNIT G,1 OR 2             
         CLC   0(1,RF),FVIFLD+1                                                 
         BE    CHKULE                                                           
         LA    RF,1(RF)                                                         
         B     CHKUL30                                                          
*                                                                               
CHKULL   CLI   *,FF                                                             
         B     CHKULX                                                           
CHKULE   CR    RB,RB                                                            
CHKULX   XIT1                                                                   
*                                                                               
UNTSXT   DC    C'D,H,M,N,O,0,1,2,3,4,5,6,7,8,' INVALID LDGRS FOR UNT S          
         DC    X'FF'                                                            
*                                                                               
UNTGT    DC    C'B,P'                          VALID LDGRS FOR UNT G            
         DC    X'FF'                                                            
*                                                                               
UNT1T    DC    C'C,F,J,N,P,R,1,2,3,4,5,6'      VALID LDGRS FOR UNIT 1           
         DC    X'FF'                                                            
*                                                                               
UNT2T    DC    C'C,D,P,7,8,9'                  VALID LDGRS FOR UNIT 2           
         DC    X'FF'                                                            
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GENERATE ACTIVITY POINTER ELEMENTS                                 
* R1 POINTS TO RECORD                                                           
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R5                                                       
ADDACTEL NMOD1 0,*ACTEL*                                                        
         L      RC,0(R1)                                                        
*                                                                               
         TM    BCCPYST6,CPYSRAPP   TEST FOR RECORD ACTIVITY POINTERS            
         BZ    AACTELX             NO                                           
         L     R2,0(R1)                                                         
         L     R5,AIO3                                                          
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM    BUILD RAP PTR ELEM                           
         MVC   RAPCPY,CUABIN                                                    
         MVI   RAPRTYP,RAPKRLED                                                 
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOM                                                     
         ST    R2,RAPAREC          ADDRESS OF RECORD                            
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
AACTELX  XIT1                                                                   
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GENERATE ACTIVITY POINTER                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R5                                                       
ADDACTPT NMOD1 0,*ACTPT*                                                        
         L      RC,0(R1)                                                        
*                                                                               
         TM    BCCPYST6,CPYSRAPP   TEST FOR RECORD ACTIVITY POINTERS            
         BZ    AACTPTX             NO                                           
         L     R5,AIO3                                                          
         MVI   RAPACTN,RAPAPTR     BUILD RAP PTR RECORD                         
         CLI   CSACT,A#CPY         FOR ACTN COPY CLEAR OUT THE OLD PTR          
         BNE   *+10                OR ELSE WON'T BUILD THE ACTIVITY             
         XC    RAPOLDP,RAPOLDP     REC                                          
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
AACTPTX  XIT1                                                                   
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* CHECK PASSWORD                                                      *         
***********************************************************************         
         SPACE 1                                                                
CHECKPW  NMOD1 0,*CHKPW**                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLI   CSACT,A#ADD                                                      
         BE    CHKPWE                                                           
         CLC   LGNPAS,=CL3'$%#'    CHECK PASSWORD                               
         BE    CHKPWE                                                           
         MVC   FVMSGNO,=AL2(AE$NOPAS)  MUST ENTER PASSWORD                      
         MVC   FVADDR,APASSWRD      PUT CURSOR INTO PASSWORD FIELD              
         B     CHKPWL                                                           
*                                                                               
CHKPWL   CLI   *,FF                                                             
         B     CHKPWX                                                           
CHKPWE   CR    RB,RB                                                            
CHKPWX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SCANMAX  EQU   10                                                               
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#PL,L'AC@PL,L       P/L                                        
         DCDDL AC#BAL,L'AC@BAL,L     BAL                                        
         DCDDL AC#NFCAN,L'AC@NFCAN,L CANAD                                      
         DCDDL AC#NFVEH,L'AC@NFVEH,L VEHICLE                                    
         DCDDL AC#NFXPD,L'AC@NFXPD,L EXPEND                                     
         DCDDL AC#GLOFF,L'AC@GLOFF,L GLOFF                                      
         DCDDL AC#NFOFP,L'AC@NFOFP,L OFFPOS                                     
         DCDDL AC#NFBDP,L'AC@NFBDP,L BUDPOS                                     
         DCDDL AC#DPT,L'AC@DEPT,L    DEPT                                       
         DCDDL AC#CLI,L'AC@CLI,L     CLI                                        
         DCDDL AC#TYPE,L'AC@TYPE,L   TYPE                                       
         DCDDL AC#LIKE,L'AC@LIKE,L   LIKE                                       
         DCDDL AC#PRINT,L'AC@PRINT,L PRINT                                      
         DCDDL AC#NFOCK,L'AC@NFOCK,L OFFCK                                      
         DCDDL AC#NO,L'AC@NO,L       NO                                         
         DCDDL AC#YES,L'AC@YES,L     YES                                        
         DCDDL AC#DEF,L'AC@DEF,L     DEFAULT                                    
         DCDDL AC#SEC,L'AC@SEC,L     SECURITY                                   
         DCDDL AC#GEN,L'AC@GEN,L     GENERAL                                    
DCLISTX  DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
CONBLKD  DSECT                                                                  
       ++INCLUDE DDCONBLK                                                       
RAPPERD  DSECT                                                                  
       ++INCLUDE ACRAPPERD                                                      
       ++INCLUDE DDSCANBLKD                                                     
*FAXTRAINF                                                                      
         PRINT OFF                                                              
       ++INCLUDE FAXTRAINF                                                      
         PRINT ON                                                               
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
APASSWRD DS    A                   A(PASSWORD FIELD)                            
ALEVAFLD DS    A                   A (LEVEL A FIELD)                            
SVREG    DS    A                   SAVE REGISTER                                
LGNPAS   DS    CL3                 PASSWORD FROM USER                           
SACLVLEN DS    CL(L'ACLVLEN)                                                    
FIRST    DS    CL1                                                              
SVSCLINE DS    XL1                 SAVED # OF SCANNER LINES                     
CHKSEQ   DS    XL1                 SAVED SEQ # FOR DISPLAYING RULES             
NFILIND  DS    XL1                 NFIL INDICATOR                               
NFILALBD EQU   X'80'               ACCOUNT LEVEL ELEMENT HAS BEEN BUILT         
*                                                                               
LTYBYTE  DS    XL1                 FLAG FOR LEDGER TYPE                         
LTGLOFF  EQU   X'80'               GLOFF=                                       
LTOFFPOS EQU   X'40'               OFFPOS=                                      
LTBUDPOS EQU   X'20'               BUDPOS=                                      
LTDEPPOS EQU   X'10'               DEPT=                                        
LTCLI    EQU   X'08'               CLI=                                         
LTTYPE   EQU   X'04'               TYPE=                                        
LTLIKE   EQU   X'02'               LIKE=                                        
LTPRINT  EQU   X'01'               PRINT=                                       
*                                                                               
LTYBYTE2 DS    XL1                 2ND FLAG FOR LEDGER TYPE                     
LTOFFCK  EQU   X'80'               OFFCK=                                       
*                                                                               
SVACLEL  DS    CL70                SAVE ACCOUNT LENGTH ELEMENT                  
SVAPREL  DS    CL50                SAVED ACC EQUIV RULE ELEMENT (CA)            
SVLDGEL  DS    CL(LDGLNQ)          SAVED LEDGER ELEM (14)                       
SVRSTEL  DS    CL(RSTLN3Q)         SAVED RECORD STATUS ELEM (30)                
BLOCK    DS    (SCANMAX)CL(SCBLKLQ)                                             
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
DSLIST   DS    0D                  DICTIONARY EQUATES USED                      
AC@PL    DS    CL3                 P/L                                          
AC@BAL   DS    CL3                 BAL                                          
AC@NFCAN DS    CL5                 CANAD                                        
AC@NFVEH DS    CL7                 VEHICLE                                      
AC@NFXPD DS    CL6                 EXPEND                                       
AC@GLOFF DS    CL5                 GLOFF                                        
AC@NFOFP DS    CL6                 OFFPOS                                       
AC@NFBDP DS    CL6                 BUDPOS                                       
AC@DEPT  DS    CL4                 DEPT                                         
AC@CLI   DS    CL3                 CLI                                          
AC@TYPE  DS    CL4                 TYPE                                         
AC@LIKE  DS    CL4                 LIKE                                         
AC@PRINT DS    CL5                 PRINT                                        
AC@NFOCK DS    CL5                 OFFCK                                        
AC@NO    DS    CL4                 NO                                           
AC@YES   DS    CL4                 YES                                          
AC@DEF   DS    CL7                 DEFAULT                                      
AC@SEC   DS    CL8                 SECURITY                                     
AC@GEN   DS    CL7                 GENERAL                                      
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACFIL26   08/31/20'                                      
         END                                                                    
