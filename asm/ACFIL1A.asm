*          DATA SET ACFIL1A    AT LEVEL 003 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE T6231AC,*                                                                
         SPACE 1                                                                
FIL1A    TITLE 'FEE RULES RECORD'                                               
         SPACE 2                                                                
FIL1A    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL1A**,R6,R7,RR=RE                                           
         USING WORKD,R9                                                         
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
         LH    R5,=Y(TWUSER-TWAD)                                               
         AR    R5,RA                                                            
         USING TWUSER,R5                                                        
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
EXITNVRC MVC   FVMSGNO,=AL2(AE$INREC)     INVALID RECORD                        
         LH    R0,GSDSPREC                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR     SET CURSOR TO RECORD FIELDD                   
         B     EXITL                                                            
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
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
*                                                                               
         USING RULED,R3                                                         
         LA    R3,FEETAB                                                        
         CLI   RULKEYW,X'40'       ALREADY DONE                                 
         BH    INIT10                                                           
INIT5    GOTO1 VDICTAT,BOPARM,C'SU  ',RULKEYW,0                                 
         LA    R3,RULLNQ(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BNE   INIT5                                                            
         DROP  R3                                                               
*                                                                               
         USING MONTABD,R3                                                       
INIT10   LA    R3,MONTBLE                                                       
         CLI   MONCHAR,X'40'         ALREADY DONE                               
         BH    INIT20                                                           
INIT15   GOTO1 VDICTAT,BOPARM,C'SU  ',MONCHAR,0                                 
         LA    R3,MONLNQ(R3)                                                    
         CLI   0(R3),X'FF'                                                      
         BNE   INIT15                                                           
*                                                                               
INIT20   MVC   FEEUL,=C'1F'                                                     
         MVC   STFUL,=C'1R'                                                     
*                                                                               
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVC   ACTKCPY,CUABIN    CONNECTED ID                                   
         MVC   ACTKUNT(L'FEEUL),FEEUL                                           
         GOTO1 AGETLDG             GET LEDGER INFO.                             
         BNE   EXITL               LEDGER RECORD MISSING                        
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4                                                       
         MVC   LDGFLEN(4),LDGTLVA   SAVE LEDGER LENGTHS OF 1F                   
*                                                                               
         MVC   ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVC   ACTKCPY,CUABIN    CONNECTED ID                                   
         MVC   ACTKUNT(L'STFUL),STFUL                                           
         GOTO1 AGETLDG             GET LEDGER INFO.                             
         BNE   EXITL               LEDGER RECORD MISSING                        
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4                                                       
         MVC   LDG1RLEN(4),LDGTLVA   SAVE LEDGER LENGTHS OF 1R                  
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS R1 TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
*TER     TM    TWAAUTH,X'10'       PASSWORD MUST BE USED                        
*        BZ    EXITNVRC                                                         
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
*        DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
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
KEY      LM    R0,R3,SVPARMS                                                    
         USING CHDRECD,R2                                                       
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
KEYFRST  L     R1,SVPARMS4         TABLE OF KNOWN INVOKERS                      
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
KFKVAL   MVC   CHDKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVC   CHDKCPY,CUABIN      CONNECTED ID                                 
         MVC   CHDKUNT(L'CHDKUNT+L'CHDKLDG),=C'1F'                              
         MVC   CHDKCCPY,CUABIN     DO BOTH ACCOUNT AND CONTRA ACCOUNT           
         MVC   CHDKCUNT(L'CHDKCUNT+L'CHDKCLDG),=C'1R'                           
         XC    CHDKNULL,CHDKNULL   CLEAR OUT THIS PART OF KEY                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   CHDKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVC   CHDKCPY,CUABIN      CONNECTED ID                                 
         MVC   CHDKUNT(L'CHDKUNT+L'CHDKLDG),=C'1F'                              
         MVC   CHDKCCPY,CUABIN     DO BOTH ACCOUNT AND CONTRA ACCOUNT           
         MVC   CHDKCUNT(L'CHDKCUNT+L'CHDKCLDG),=C'1R'                           
         XC    CHDKNULL,CHDKNULL   CLEAR OUT THIS PART OF KEY                   
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
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING CHDRECD,R2                                                       
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
         USING CHDRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(FEE#CLI),AL4(CLIDTA)    CLIENT                               
         DC    AL2(FEE#CLINM),AL4(CLINDTA) CLIENT NAME                          
         DC    AL2(FEE#DIV),AL4(DIVDTA)    DIVISION                             
         DC    AL2(FEE#DIVNM),AL4(DIVNDTA) DIVISION NAME                        
         DC    AL2(FEE#OFF),AL4(OFFDTA)    OFFICE                               
         DC    AL2(FEE#OFFNM),AL4(OFFNDTA)  OFFICE NAME                         
         DC    AL2(FEE#DEPT),AL4(DEPDTA)    DEPARTMENT CODE                     
         DC    AL2(FEE#DEPTNM),AL4(DEPNDTA) DEPARTMENT NAME                     
         DC    AL2(FEE#SUBDPT),AL4(SUBDTA)  SUB-DEPARTMENT CODE                 
         DC    AL2(FEE#SUBDNM),AL4(SUBNDTA) SUB-DEPARMMENT NAME                 
         DC    AL2(FEE#STAFF),AL4(STFDTA)  STAFF CODE                           
         DC    AL2(FEE#STFNM),AL4(STFNDTA) STAFF NAME                           
         DC    AL2(FEE#RULES),AL4(RULDTA) FEE RULES                             
         DC    AL2(FEE#DATE),AL4(EDTDTA)   EFFECTIVE DATE                       
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL1A    CSECT                                                                  
         SPACE 2                                                                
**  TABLE OF FEE RULE PARAMETERS  **                                            
         SPACE                                                                  
FEETAB   DS    0H                                                               
START    DCDD  AC#START,7          KEYWORD                                      
         DC    AL1(5)              LENGTH OF KEYWORD                            
         DC    AL4(EDSTART)        EDIT ROUTINE FOR THIS KEYWORD                
         DC    AL1(0)              MAXIMUM INTEGERS ALLOWED                     
         DC    AL1(0)              MAXIMUM DECIMAL PLACES ALLOWED               
         DC    AL1(*-START+1)      LENGTH OF TABLE ENTRY                        
*                                                                               
HOURS    DCDD  AC#HOURS,7                                                       
         DC    AL1(5)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(4)              NNNN                                         
         DC    AL1(0)                                                           
         DC    AL1(*-HOURS+1)                                                   
*                                                                               
OVA      DCDD  AC#NFOVA,7                                                       
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(1)              N.NNN                                        
         DC    AL1(3)                                                           
         DC    AL1(*-OVA+1)                                                     
OVB      DCDD  AC#NFOVB,7                                                       
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(1)              N.NNN                                        
         DC    AL1(3)                                                           
         DC    AL1(*-OVB+1)                                                     
OVC      DCDD  AC#NFOVC,7                                                       
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(1)              N.NNN                                        
         DC    AL1(3)                                                           
         DC    AL1(*-OVC+1)                                                     
BONUS    DCDD  AC#BONUS,7                                                       
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(2)              NN.NN                                        
         DC    AL1(2)                                                           
         DC    AL1(*-BONUS+1)                                                   
TYPE     DCDD  AC#TYPE,7                                                        
         DC    AL1(4)                                                           
         DC    AL4(EDTYPE)                                                      
         DC    AL1(3)                                                           
         DC    AL1(0)                                                           
         DC    AL1(*-TYPE+1)                                                    
MAX      DCDD  AC#MAX,7                                                         
         DC    AL1(3)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(3)              NNN                                          
         DC    AL1(0)                                                           
         DC    AL1(*-MAX+1)                                                     
WMAX     DCDD  AC#NFWMX,7                                                       
         DC    AL1(4)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(1)              N                                            
         DC    AL1(0)                                                           
         DC    AL1(*-WMAX+1)                                                    
ADJUST   DCDD  AC#ADJ,7                                                         
         DC    AL1(6)                                                           
         DC    AL4(EDADJUST)                                                    
         DC    AL1(0)                                                           
         DC    AL1(0)                                                           
         DC    AL1(*-ADJUST+1)                                                  
RATE     DCDD  AC#RATE,7                                                        
         DC    AL1(4)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(3)              NNN.NNN                                      
         DC    AL1(3)                                                           
         DC    AL1(*-RATE+1)                                                    
PART     DCDD  AC#NFPAR,7                                                       
         DC    AL1(4)                                                           
         DC    AL4(HILO)                                                        
         DC    AL1(2)              NN                                           
         DC    AL1(0)                                                           
         DC    AL1(*-PART+1)                                                    
         DC    X'FF'                                                            
*                                                                               
TYPETBLE DS    0H                                                               
         DC    CL4'ZERO'                                                        
         DC    CL4'NON '                                                        
         DC    CL4'100 '                                                        
         DC    CL4'90  '                                                        
         DC    CL4'66  '                                                        
         DC    CL4'50  '                                                        
         DC    CL4'33  ',X'FF'                                                  
*                                                                               
MONTBLE  DS    0H                                                               
         DCDD  AC#JAN,3                                                         
         DCDD  AC#FEB,3                                                         
         DCDD  AC#MAR,3                                                         
         DCDD  AC#APR,3                                                         
         DCDD  AC#MAY,3                                                         
         DCDD  AC#JUN,3                                                         
         DCDD  AC#JUL,3                                                         
         DCDD  AC#AUG,3                                                         
         DCDD  AC#SEP,3                                                         
         DCDD  AC#OCT,3                                                         
         DCDD  AC#NOV,3                                                         
         DCDD  AC#DEC,3                                                         
         DC    X'FF'                                                            
*                                                                               
NUMTBLE  DS    0CL256              TRT TABLES                                   
         DC    240X'FF',10X'00',6X'FF'                                          
         SPACE                                                                  
RULED    DSECT                                                                  
RULKEYW  DS    CL7                 FEE RULE (KEYWORD)                           
RULKEYLN DS    AL1                 LENGTH OF KEYWORD                            
RULEDRTN DS    AL4                 A(EDIT ROUTINE FOR THAT RULE)                
RULMXINT DS    AL1                 MAX WHOLE NUMBERS ALLOWED                    
RULDECPT DS    AL1                 MAXIMUM DECIMAL PLACES ALLOWED               
RULLEN   DS    AL1                 LENGTH OF TABLE ENTRY                        
RULLNQ   EQU   *-RULED                                                          
         SPACE 2                                                                
MONTABD  DSECT                                                                  
MONCHAR  DS    CL3                 MONTH                                        
MONLNQ   EQU   *-MONTABD                                                        
*                                                                               
FIL1A    CSECT                                                                  
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
*        MVC   SVNAME,BCSPACES                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   DS    0H                                                               
*        MVC   SVNAME,BCSPACES                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CLIENT CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CLIDTA   LA    RF,CLITBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CLITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLI)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCLI)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCLI)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETCLI)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCLI)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALCLI)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCLI)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCLI)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETCLI  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCLI   ZIC   R1,LDGFLNA                                                       
         BCTR  R1,0                                                             
         EXMVC R1,FVIFLD,CHDKACT   MOVE IN CLIENT                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLIENT CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCLI   XC    SVCLI,SVCLI                                                      
         MVC   SVNAME,BCSPACES                                                  
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
*                                                                               
VCLI04   CLC   FVILEN,LDGFLNA    INPUT LENGTH SHORT ENOUGH?                     
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         SR    RF,RF                                                            
         IC    RF,LDGFLNA          LENGTH OF THE CLIENT CODE                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   CHDKACT(0),FVIFLD   MOVE IN CLIENT CODE                          
         EX    RF,*+4                                                           
         MVC   FLTIFLD(0),FVIFLD   MOVE IN CLIENT CODE TO FILTER FIELD          
         EX    RF,*+4                                                           
         MVC   SVCLI(0),FVIFLD     MOVE INTO SAVED CLIENT FIELD                 
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   IOKEY(L'CHDKCULA),CHDKCPY                                        
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTCLI  SR    RF,RF                                                            
         IC    RF,LDGFLNA          LENGTH OF THE CLIENT CODE                    
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),FLTIFLD   MOVE IN CLIENT CODE FROM KEY                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A CLIENT CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
SRCHCLI  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP THE FILTERING                      
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,FEEUL,ACOM,    C        
               (X'11',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR CLIENT CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTCLI  CLI   FLTIFLD,0           ANY FILTER?                                  
         BE    FLTXE               NO                                           
         SR    RF,RF                                                            
         IC    RF,LDGFLNA          LENGTH OF THE CLIENT CODE                    
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   CHDKACT(0),FLTIFLD                                               
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A CLIENT NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CLINDTA  LA    RF,CLINTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CLINTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
DISCLIN  MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         LA    R3,IOKEY                                                         
         MVC   0(CHDKACT-CHDKEY,R3),CHDKCPY                                     
         LA    R3,CHDKACT-CHDKEY(R3)                                            
         ZIC   R1,LDGFLNA                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),CHDKACT                                                 
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
         GOTO1 AGETNAM             GET CLIENT NAME                              
         MVC   SVNAME,FVIFLD                                                    
         MVC   SVCLINM,FVIFLD      SAVE CLIENT NAME                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DIVISION                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DIVDTA   LA    RF,DIVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DIVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDIV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDIV)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISDIV)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETDIV)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTDIV)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALDIV)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTDIV)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHDIV)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETDIV  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DIVISTION                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISDIV   LA    R3,CHDKACT                                                       
         ZIC   R1,LDGFLNB          R1=L'CLIENT+DIVISION                         
         ZIC   RE,LDGFLNA          RE=L'CLIENT                                  
         AR    R3,RE               POINT TO DIVISION IN KEY                     
         SR    R1,RE               R1=L'DIVISION                                
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF *'S IN DIVISION CODE                      
         BE    EXITOK              THAN DON'T DISPLAY                           
         EXMVC R1,FVIFLD,0(R3)                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DIVISION FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTDIV  ZIC   R1,LDGFLNB          R1=L'CLIENT+DIVISION                         
         ZIC   RE,LDGFLNA          RE=L'CLIENT                                  
         SR    R1,RE               R1=L'DIVISION                                
         BCTR  R1,0                                                             
         EXMVC R1,FVIFLD,FLTIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A DIVISION                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALDIV   XC    SVDIV,SVDIV                                                      
         ZIC   R1,LDGFLNB          R1=L'CLIENT+DIVISION                         
         ZIC   RE,LDGFLNA          RE=L'CLIENT                                  
         SR    R1,RE               R1=L'DIVISION                                
         LA    R3,CHDKACT                                                       
         AR    R3,RE               R3 POINTS TO DIVISION                        
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   VDIV04                                                           
         BCTR  R1,0                NO-THAN FILL DIVISION IN WITH *'S            
         EX    R1,MVCSTAR                                                       
         EXMVC R1,FLTIFLD,BCSPACES AND FILL SPACES IN FILTER FIELD              
         B     EXITOK                                                           
*                                                                               
VDIV04   CLM   R1,1,FVILEN                                                      
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),FVIFLD     MOVE THE DIVISION INTO THE KEY               
         EXMVC R1,FLTIFLD,FVIFLD   AND INTO THE FILTER                          
         EXMVC R1,SVDIV,FVIFLD     AND MOVE INTO SAVED DIV FIELD                
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   IOKEY(L'CHDKCULA),CHDKCPY                                        
         GOTO1 AGETACT,0             GET ACCOUNT/TEST SECURITY                  
         BNE   EXITL                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A DIVISION                                                *         
***********************************************************************         
         SPACE 1                                                                
SRCHDIV  CLI   CSACT,A#LST         DON'T SEARCH FOR ACTION LIST                 
         BE    EXITOK              MESSES UP THE FILTERING                      
         MVC   BOWORK1,BCSPACES                                                 
         ZIC   R1,LDGFLNB          R1=L'CLIENT+DIVISION                         
         ZIC   RE,LDGFLNA          RE=L'CLIENT                                  
         LA    R3,CHDKACT                                                       
         AR    R3,RE               R3 POINTS TO DIVISION IN KEY                 
         SR    R1,RE               R1=L'DIVISION                                
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF *'S IN DIVISION                           
         BE    EXITOK              THAN DON'T FILTER                            
         EXMVC R1,BOWORK1,0(R3)                                                 
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,FEEUL,ACOM,    C        
               (X'22',BOWORK1)                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PRODUCT CODE                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTDIV  CLI   FLTIFLD,0           ANY FILTER                                   
         BE    FLTXE               NO                                           
         ZIC   R1,LDGFLNB          R1=L'CLIENT+DIVISION                         
         ZIC   RE,LDGFLNA          RE=L'CLIENT                                  
         LA    R3,CHDKACT                                                       
         AR    R3,RE               R3 POINTS TO DIVISION IN KEY                 
         SR    R1,RE               R1=L'DIVISION                                
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FLTIFLD    COMPARE DIVISION & FILTER                    
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A DIVISION NAME                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DIVNDTA  LA    RF,DIVNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DIVNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDIVN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DIVISION NAME                                             *         
***********************************************************************         
         SPACE 1                                                                
DISDIVN  ZIC   R1,LDGFLNB          R1=L'CLIENT+DIVISION                         
         ZIC   RE,LDGFLNA          RE=L'CLIENT                                  
         LA    R3,CHDKACT                                                       
         AR    R3,RE               R3 POINTS TO DIVISION IN KEY                 
         SR    R1,RE               R1=L'DIVISION                                
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR      IF *'S IN DIVISION CODE                   
         BE    EXITOK              THAN NO NAME TO DISPLAY                      
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   IOKEY(L'CHDKCULA),CHDKCPY                                        
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
         GOTO1 AGETNAM             GET DIVISION NAME                            
         MVC   SVNAME,FVIFLD                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFDTA   LA    RF,OFFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFF)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOFF)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOFF)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOFF)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHOFF)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
DSETOFF  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
DISOFF   LR    R3,R2                                                            
         LA    R3,T.CHDKCACT         R3 POINT TO OFFICE CODE IN C/A             
         ZIC   R1,LDGRLNA                                                       
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF *'S IN OFFICE DON'T DISPLAY               
         BE    EXITOK                                                           
         EXMVC R1,FVIFLD,0(R3)                                                  
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A OFFICE CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
VALOFF   LR    R3,R2                                                            
         LA    R3,T.CHDKCACT         R3 POINTS TO OFFICE IN C/A                 
         ZIC   R1,LDGRLNA          R1=L'OFFICE                                  
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   VOFF02                                                           
         BCTR  R1,0                                                             
         EX    R1,MVCSTAR          NO-THAN FILL IN OFFICE WITH *'S              
         EXMVC R1,FLTIFLD,BCSPACES AND FILL SPACES IN FILTER FIELD              
         B     EXITOK                                                           
*                                                                               
VOFF02   CLM   R1,1,FVILEN           INPUT LENGTH SHORT ENOUGH?                 
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),FVIFLD                                                  
         EXMVC R1,FLTIFLD,FVIFLD   MOVE IN OFFICE TO FILTER FIELD               
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'CHDKCULC),CHDKCCPY                                       
         GOTO1 AGETACT,0                GET ACCOUNT/TEST SECURITY               
         BNE   EXITL                                                            
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  ZIC   R1,LDGRLNA                                                       
         BCTR  R1,0                                                             
         EXMVC R1,FVIFLD,FLTIFLD   MOVE IN OFFICE FROM KEY                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A OFFICE CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
SRCHOFF  CLI   CSACT,A#LST         FOR ACTION LIST                              
         BE    EXITOK              DON'T SEARCH(MESSES UP FILTERING)            
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,STFUL,ACOM,    C        
               (X'11',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
DOFTOFF  CLI   FLTIFLD,0           ANY FILTER?                                  
         BE    FLTXE                                                            
*                                                                               
         LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLNA          LENGTH OF THE OFFICE                         
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFFICE NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFNDTA  LA    RF,OFFNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFFN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
DISOFFN  LA    R3,CHDKCACT-CHDRECD(R2)                                          
         ZIC   R1,LDGRLNA          LENGTH OF THE OFFICE                         
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF OFFICE FILLED WITH *'S DON'T              
         BE    EXITOK              DISPLAY NAME                                 
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   IOKEY(CHDKCACT-CHDKCCPY),CHDKCCPY                                
         EXMVC R1,IOKEY+(CHDKCACT-CHDKCCPY),CHDKCACT                            
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
         GOTO1 AGETNAM             GET OFFICE NAME                              
         MVC   SVNAME,FVIFLD                                                    
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEPARTMENT CODE                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DEPDTA   LA    RF,DEPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DEPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDEP)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISDEP)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETDEP)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTDEP)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALDEP)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHDEP)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTDEP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
DSETDEP  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DEPARTMENT CODE                                           *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
DISDEP   LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLNB          R1=L'OFFICE+L'DEPT                           
         ZIC   RE,LDGRLNA          RE=L'OFFICE                                  
         AR    R3,RE               R3 POINTS TO DEPT FIELD IN C/A               
         SR    R1,RE               R1=L'DEPT                                    
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF *'S IN DEPT DON'T DISPLAY                 
         BE    EXITOK                                                           
         EXMVC R1,FVIFLD,0(R3)                                                  
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A DEPARTMENT FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
VALDEP   LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLNB          R1=L'OFFICE+L'DEPT                           
         ZIC   RE,LDGRLNA          RE=L'OFFICE                                  
         AR    R3,RE               R3 POINTS TO DEPT FIELD IN C/A               
         SR    R1,RE               R1=L'DEPT                                    
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   VDEP02                                                           
         BCTR  R1,0                                                             
         EX    R1,MVCSTAR          NO-THAN FILL IN DEPT WITH *'S                
         EXMVC R1,FLTIFLD,BCSPACES AND FILL SPACES IN FILTER FIELD              
         B     EXITOK                                                           
*                                                                               
VDEP02   CLM   R1,1,FVILEN           INPUT LENGTH SHORT ENOUGH?                 
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),FVIFLD                                                  
         EXMVC R1,FLTIFLD,FVIFLD   MOVE IN DEPT TO FILTER FIELD                 
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'CHDKCULC),CHDKCCPY                                       
         GOTO1 AGETACT,0                GET ACCOUNT/TEST SECURITY               
         BNE   EXITL                                                            
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DEPARTMENT CODE FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTDEP  ZIC   R1,LDGRLNB          R1=L'OFFICE+L'DEPT                           
         ZIC   RE,LDGRLNA          RE=L'OFFICE                                  
         SR    R1,RE               R1=L'DEPT                                    
         BCTR  R1,0                                                             
         EXMVC R1,FVIFLD,FLTIFLD   MOVE IN DEPT FROM KEY                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A DEPARTMENT CODE                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHDEP  CLI   CSACT,A#LST         FOR ACTION LIST                              
         BE    EXITOK              DON'T SEARCH(MESSES UP FILTERING)            
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,STFUL,ACOM,    C        
               (X'22',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR DEPARTMENT CODE                                    *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
DOFTDEP  CLI   FLTIFLD,0           ANY FILTER?                                  
         BE    FLTXE                                                            
*                                                                               
         LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLNB          R1=L'OFFICE+L'DEPT                           
         ZIC   RE,LDGRLNA          RE=L'OFFICE                                  
         SR    R1,RE               R1=L'DEPT                                    
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A DEPARTMENT NAME FIELD                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DEPNDTA  LA    RF,DEPNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DEPNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEPN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DEPARTMENT NAME FIELD FROM THE KEY                        *         
***********************************************************************         
         SPACE 1                                                                
DISDEPN  LA    R3,CHDKCACT-CHDKEY(R2)                                           
         ZIC   R1,LDGRLNB          R1=L'OFFICE+L'DEPT                           
         ZIC   RE,LDGRLNA          RE=L'OFFICE                                  
         LR    RF,R1               SAVE ACCUMULATED LENGTH IN RF                
         AR    R3,RE               R3 NOW POINTS TO DEPT IN KEY                 
         SR    R1,RE               R1=L'DEPT                                    
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF DEPT FILLED WITH *'S DON'T                
         BE    EXITOK              DISPLAY NAME                                 
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   IOKEY(CHDKCACT-CHDKCCPY),CHDKCCPY                                
         BCTR  RF,0                                                             
         EXMVC RF,IOKEY+(CHDKCACT-CHDKCCPY),CHDKCACT                            
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
         GOTO1 AGETNAM             GET DEPARTMENT NAME                          
         MVC   SVNAME,FVIFLD                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SUB DEPARTMENT CODE                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBDTA   LA    RF,SUBTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SUBTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUB)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSUB)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISSUB)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETSUB)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSUB)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALSUB)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHSUB)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSUB)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
DSETSUB  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUB DEPARTMENT CODE                                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
DISSUB   LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLNC          R1=L'DEPT+L'SUBDEPT                          
         ZIC   RE,LDGRLNB          RE=L'DEPT                                    
         AR    R3,RE               R3 POINTS TO SUBDEPT FIELD IN C/A            
         SR    R1,RE               R1=L'SUBDEPT                                 
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF *'S IN SUBDEPT DON'T DISPLAY              
         BE    EXITOK                                                           
         EXMVC R1,FVIFLD,0(R3)                                                  
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SUB DEPARTMENT FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
VALSUB   LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLNC          R1=L'DEPT+L'SUBDEPT                          
         ZIC   RE,LDGRLNB          RE=L'DEPT                                    
         AR    R3,RE               R3 POINTS TO SUBDEPT FIELD IN C/A            
         SR    R1,RE               R1=L'SUBDEPT                                 
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   VSUB02                                                           
         BCTR  R1,0                                                             
         EX    R1,MVCSTAR          NO-THAN FILL IN SUBDEPT WITH *'S             
         EXMVC R1,FLTIFLD,BCSPACES AND FILL SPACES IN FILTER FIELD              
         B     EXITOK                                                           
*                                                                               
VSUB02   CLM   R1,1,FVILEN           INPUT LENGTH SHORT ENOUGH?                 
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),FVIFLD                                                  
         EXMVC R1,FLTIFLD,FVIFLD   MOVE IN SUBDEPT TO FILTER FIELD              
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'CHDKCULC),CHDKCCPY                                       
         GOTO1 AGETACT,0                GET ACCOUNT/TEST SECURITY               
         BNE   EXITL                                                            
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUB DEPARTMENT CODE FILTER FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTSUB  ZIC   R1,LDGRLNC          R1=L'DEPT+L'SUBDEPT                          
         ZIC   RE,LDGRLNB          RE=L'DEPT                                    
         SR    R1,RE               R1=L'SUBDEPT                                 
         BCTR  R1,0                                                             
         EXMVC R1,FVIFLD,FLTIFLD   MOVE IN SUBDEPT FROM KEY                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A SUB DEPARTMENT CODE                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHSUB  CLI   CSACT,A#LST         FOR ACTION LIST                              
         BE    EXITOK              DON'T SEARCH(MESSES UP FILTERING)            
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,STFUL,ACOM,    C        
               (X'33',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR SUB DEPARTMENT CODE                                *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
DOFTSUB  CLI   FLTIFLD,0           ANY FILTER?                                  
         BE    FLTXE                                                            
*                                                                               
         LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLNC          R1=L'DEPT+L'SUBDEPT                          
         ZIC   RE,LDGRLNB          RE=L'DEPT                                    
         SR    R1,RE               R1=L'SUBDEPT                                 
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A SUB DEPARTMENT NAME FIELD              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBNDTA  LA    RF,SUBNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SUBNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUBN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUB DEPARTMENT NAME FIELD FROM THE KEY                    *         
***********************************************************************         
         SPACE 1                                                                
DISSUBN  LA    R3,CHDKCACT-CHDKEY(R2)                                           
         ZIC   R1,LDGRLNC          R1=L'DEPT+L'SUBEPT                           
         ZIC   RE,LDGRLNB          RE=L'DEPT                                    
         LR    RF,R1               SAVE ACCUM LENGTH FOR LATER                  
         AR    R3,RE               R3 NOW POINTS TO SUBDEPT IN KEY              
         SR    R1,RE               R1=L'SUBDEPT                                 
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF SUBDEPT FILLED WITH *'S DON'T             
         BE    EXITOK              DISPLAY NAME                                 
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   IOKEY(CHDKCACT-CHDKCCPY),CHDKCCPY                                
         BCTR  RF,0                                                             
         EXMVC RF,IOKEY+(CHDKCACT-CHDKCCPY),CHDKCACT                            
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
         GOTO1 AGETNAM             GET STAFF NAME                               
         MVC   SVNAME,FVIFLD                                                    
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR STAFF CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
STFDTA   LA    RF,STFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
STFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTF)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISSTF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETSTF)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSTF)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALSTF)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHSTF)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSTF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
DSETSTF  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A STAFF CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
DISSTF   LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLND          R1=L'SUBDEPT+STAFF                           
         ZIC   RE,LDGRLNC          RE=L'SUBDEPT                                 
         AR    R3,RE               R3 POINTS TO STAFF FIELD IN C/A              
         SR    R1,RE               R1=L'STAFF                                   
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF *'S IN STAFF DON'T DISPLAY                
         BE    EXITOK                                                           
         EXMVC R1,FVIFLD,0(R3)                                                  
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A STAFF FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
VALSTF   LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLND          R1=L'SUBDEPT+L'STAFF                         
         ZIC   RE,LDGRLNC          RE=L'SUBDEPT                                 
         AR    R3,RE               R3 POINTS TO STAFF FIELD IN C/A              
         SR    R1,RE               R1=L'STAFF                                   
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   VSTF02                                                           
         BCTR  R1,0                                                             
         EX    R1,MVCSTAR          NO-THAN FILL IN STAFF WITH *'S               
         EXMVC R1,FLTIFLD,BCSPACES AND FILL SPACES IN FILTER FIELD              
         B     EXITOK                                                           
*                                                                               
VSTF02   CLM   R1,1,FVILEN           INPUT LENGTH SHORT ENOUGH?                 
         BNL   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),FVIFLD                                                  
         EXMVC R1,FLTIFLD,FVIFLD   MOVE IN STAFF TO FILTER FIELD                
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(L'CHDKCULC),CHDKCCPY                                       
         GOTO1 AGETACT,0                GET ACCOUNT/TEST SECURITY               
         BNE   EXITL                                                            
         B     EXITOK                                                           
*                                                                               
MVCSTAR  MVC   0(0,R3),FILLCHAR                                                 
         SPACE 2                                                                
         DROP  T                                                                
***********************************************************************         
* DISPLAY A STAFF CODE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTSTF  ZIC   R1,LDGRLND          R1=L'SUBDEPT+L'STAFF                         
         ZIC   RE,LDGRLNC          RE=L'SUBDEPT                                 
         SR    R1,RE               R1=L'STAFF                                   
         BCTR  R1,0                                                             
         EXMVC R1,FVIFLD,FLTIFLD   MOVE IN STAFF FROM KEY                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A STAFF CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
SRCHSTF  CLI   CSACT,A#LST         FOR ACTION LIST                              
         BE    EXITOK              DON'T SEARCH(MESSES UP FILTERING)            
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,STFUL,ACOM,    C        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR SUB DEPARTMENT CODE                                *         
***********************************************************************         
         SPACE 1                                                                
T        USING CHDRECD,R3                                                       
DOFTSTF  CLI   FLTIFLD,0           ANY FILTER?                                  
         BE    FLTXE                                                            
*                                                                               
         LR    R3,R2                                                            
         LA    R3,T.CHDKCACT                                                    
         ZIC   R1,LDGRLND          R1=L'SUBDEPT+L'STAFF                         
         ZIC   RE,LDGRLNC          RE=L'SUBDEPT                                 
         SR    R1,RE               R1=L'STAFF                                   
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A STAFF NAME FIELD                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
STFNDTA  LA    RF,STFNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
STFNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTFN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A STAFF NAME FIELD FORM THE KEY                             *         
***********************************************************************         
         SPACE 1                                                                
DISSTFN  LA    R3,CHDKCACT-CHDKEY(R2)                                           
         ZIC   R1,LDGRLND          R1=L'SUBDEPT+L'STAFF                         
         ZIC   RE,LDGRLNC          RE=L'SUBDEPT                                 
         LR    RF,R1               SAVE ACCUM LENGTH IN RF FOR LATER            
         AR    R3,RE               R3 NOW POINTS TO STAFF IN KEY                
         SR    R1,RE               R1=L'STAFF                                   
         BCTR  R1,0                                                             
         EXCLC R1,0(R3),FILLCHAR   IF STAFF FILLED WITH *'S DON'T               
         BE    EXITOK              DISPLAY NAME                                 
*                                                                               
         MVC   IOKEY,BCSPACES      READ THE ACCOUNT RECORD                      
         MVC   IOKEY(CHDKCACT-CHDKCCPY),CHDKCCPY                                
         BCTR  RF,0                                                             
         EXMVC RF,IOKEY+(CHDKCACT-CHDKCCPY),CHDKCACT                            
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
         GOTO1 AGETNAM             GET STAFF NAME                               
         MVC   SVNAME,FVIFLD                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FEE RULES FIELD                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RULDTA   LA    RF,RULTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
RULTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRUL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRUL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FEE RULES                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISRUL   MVC   FVIFLD,BCSPACES                                                  
         ZIC   R1,TLKLHSL          LEN OF LEFT HAND SIDE                        
         BCTR  R1,0                                                             
         EXMVC R1,FVIFLD,TLKKEYW                                                
*                                                                               
         LA    R1,1(R1)            BUMP UP TO TRUE LENGTH                       
         LA    R3,FVIFLD                                                        
         AR    R3,R1               POINT TO NEXT OPEN SPOT IN FIELD             
         MVI   0(R3),C'='                                                       
         LA    R3,1(R3)                                                         
*                                                                               
         ZIC   R1,TLKRHSL          LEN OF RIGHT HAND SIDE                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R3),TLKDATA                                                 
*                                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FEE RULES                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALRUL   CLI   FVILEN,0                                                         
         BNE   VRUL10                                                           
         CLI   CSACT,A#ADD                                                      
         BE    EXITNO                                                           
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
VRUL10   OC    TLKKEYW,TLKKEYW IF RULE ALREADY THERE MUST BE CHANGING           
         BNZ   VRUL20           AN EXISTING RATE SO DON'T CHK FOR MAX           
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1             FIRST LIST REC #                          
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
         CHI   RE,22               22 RATES MAX                                 
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$RATMX)                                           
         B     EXITL                                                            
*                                                                               
VRUL20   OC    TLKKEYW,TLKKEYW                                                  
         MVC   TLKDATA,BCSPACES                                                 
         GOTO1 VSCANNER,BOPARM,FVIHDR,SCANBLK                                   
         CLI   4(R1),1             SHOULD ONLY BE ONE LINE IN SCANNER           
         BNE   EXITNV                                                           
         LA    R3,SCANBLK          MATCH ITEM IN BLOCK TO TABLE                 
         USING SCANBLKD,R3                                                      
         LA    R4,FEETAB                                                        
         USING RULED,R4                                                         
*                                                                               
VRUL25   ZIC   R1,RULKEYLN         L'KEYWORD FROM TABLE                         
         BCTR  R1,0                                                             
         CLI   0(R4),X'FF'                                                      
         BNE   VRUL30                                                           
         MVC   FVMSGNO,=AL2(AE$IKEYW) NOT A VALID KEYWORD                       
         B     EXITL                                                            
*                                                                               
VRUL30   EXCLC R1,SC1STFLD,RULKEYW                                              
         BNE   VRUL40                                                           
         CLI   SC1STLEN,0           GOT VALID KEYWORD. CHECK THAT FORM          
         BNE   VRUL50               IS KEYWORD=VALUE                            
         MVC   FVMSGNO,=AL2(AE$IFORM)  INVALID FORMAT                           
         B     EXITL                                                            
         SPACE                                                                  
VRUL40   ZIC   R0,RULLEN           POINT TO NEXT TABLE ENTRY                    
         AR    R4,R0                                                            
         B     VRUL25                                                           
*                                                                               
VRUL50   ST    R4,SVADDR           SAVE ADDRESS OF TABLE ENTRY                  
         L     RF,RULEDRTN                                                      
         A     RF,BORELO                                                        
         BASR  RE,RF                                                            
         BL    EXITL                                                            
*                                                                               
         ZIC   R1,SC1STLEN         LENGTH OF LHS                                
         STC   R1,TLKLHSL          STORE IN TSAR REC                            
         BCTR  R1,0                                                             
         EXMVC R1,TLKKEYW,SC1STFLD MOVE IN KEYWORD TO TSAR                      
*                                                                               
         ZIC   R1,SC2NDLEN         LENGH OF RHS                                 
         STC   R1,TLKRHSL          STORE IN TSAR REC                            
         BCTR  R1,0                                                             
         EXMVC R1,TLKDATA,SC2NDFLD MOVE IN DATA PORTION TO TSAR                 
*                                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* VALIDATE 'START' KEYWORD                                                      
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
EDSTART  NTR1                                                                   
         CLI   SC2NDLEN,3             MONTHS ALWAYS 3 BYTES                     
         BE    EDST10                                                           
         MVC   FVMSGNO,=AL2(AE$INMON)   INVALID MONTH                           
         B     EXITL                                                            
         SPACE                                                                  
EDST10   LA    R2,MONTBLE                                                       
EDST20   CLI   0(R2),X'FF'                                                      
         BNE   EDST30                                                           
         MVC   FVMSGNO,=AL2(AE$INMON)    INVALID MONTH                          
         B     EXITL                                                            
         SPACE                                                                  
EDST30   CLC   0(3,R2),SC2NDFLD                                                 
         BE    EXITOK                                                           
         LA    R2,3(R2)                                                         
         B     EDST20                                                           
         SPACE 2                                                                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* VALIDATE 'TYPE' KEYWORD                                                       
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
EDTYPE   NTR1                                                                   
         LA    R2,TYPETBLE                                                      
EDTY10   CLI   0(R2),X'FF'                                                      
         BNE   EDTY20                                                           
         MVC   FVMSGNO,=AL2(AE$IVTYP)     INVALID TYPE                          
         B     EXITL                                                            
         SPACE                                                                  
EDTY20   ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         EXCLC R1,SC2NDFLD,0(R2)                                                
         BE    EXITOK                                                           
         LA    R2,4(R2)                                                         
         B     EDTY10                                                           
         SPACE 2                                                                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* VALIDATE 'ADJUST' KEYWORD                                                     
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
EDADJUST NTR1                                                                   
         CLI   SC2NDLEN,2                                                       
         BH    EADJ10                                                           
         CLC   SC2NDFLD(2),=C'YE'     ONLY POSSIBILITY ALLOWED                  
         BE    EXITOK                                                           
EADJ10   DS    0H                                                               
         MVC   FVMSGNO,=AL2(AE$NOADJ)                                           
         MVC   FVXTRA,BCSPACES                                                  
         ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    EXITL                                                            
         EXMVC R1,FVXTRA,SC2NDFLD  MOVE IN BAD VALUE ENTERED                    
         B     EXITL                                                            
         SPACE 2                                                                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* VALIDATE THE FOLLOWING KEYWORDS:  'HOURS','OVA','OVB','OVC','BONUS',          
* 'MAX','WMAX','RATE','PART'                                                    
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
T        USING RULED,RF                                                         
HILO     NTR1                                                                   
         L     RF,SVADDR           POINT RF TO SAVED FEE TABLE ENTRY            
         MVC   BCWORK,BCSPACES                                                  
         LA    R4,SC2NDFLD         POINT R4 TO ENTRY TO VALIDATE                
         SR    R1,R1               INITIALIZE CNTR                              
         ZIC   R2,SC2NDLEN         L'ENTRY FROM BLOCK                           
HILO10   CLI   0(R4),X'4B'         IS THERE A DECIMAL POINT                     
         BE    HILO20                                                           
         TRT   0(1,R4),NUMTBLE                                                  
         BNE   EXITNOTN                                                         
         LA    R1,1(R1)                                                         
         LA    R4,1(R4)            POINT TO NEXT BYTE TO TEST                   
         BCT   R2,HILO10                                                        
         MVI   BCWORK+1,0          DEC PT NOT FOUND = INTEGERS ONLY             
         STC   R1,BCWORK           COUNT (INTEGERS ONLY)                        
         B     HILO50              OR                                           
         SPACE                                                                  
HILO20   STC   R1,BCWORK           COUNT(INTEGERS) OR 0 (DECIMAL ONLY)          
         ZIC   R2,SC2NDLEN         L'ENTRY                                      
         SR    R2,R1               - L'LEFT = L'RIGHT                           
         BCTR  R2,0                MINUS DECIMAL POINT                          
         STC   R2,BCWORK+1          COUNT OF DECIMAL PLACES                     
         LA    R4,1(R4)            POINT PAST DECIMAL POINT                     
HILO30   TRT   0(1,R4),NUMTBLE                                                  
         BNE   EXITNOTN                                                         
         LA    R4,1(R4)                                                         
         BCT   R2,HILO30                                                        
         CLI   BCWORK+1,0          ANY DECIMAL PLACES                           
         BE    HILO50                                                           
         CLI   T.RULDECPT,0          ARE DECIMAL PLACES ALLOWED                 
         BNE   HILO40                                                           
         MVC   FVMSGNO,=AL2(AE$NODEC)  DECIMAL PLACES NOT ALLOWED               
         MVC   FVXTRA,BCSPACES                                                  
         ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    EXITL                                                            
         EXMVC R1,FVXTRA,SC2NDFLD  MOVE IN BAD VALUE ENTERED                    
         B     EXITL                                                            
         SPACE                                                                  
HILO40   CLC   BCWORK+1(1),T.RULDECPT    DOES IT EXCEED MAX DEC PLS             
         BNH   HILO50                                                           
         MVC   FVMSGNO,=AL2(AE$2DEC)  TOO MANY DECIMAL PLACES ENTERED           
         MVC   FVXTRA,BCSPACES                                                  
         ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    EXITL                                                            
         EXMVC R1,FVXTRA,SC2NDFLD  MOVE IN BAD VALUE ENTERED                    
         B     EXITL                                                            
         SPACE                                                                  
HILO50   CLI   BCWORK,0            IS LEFT SIDE LENGTH ZERO                     
         BE    EXITOK                                                           
         CLC   BCWORK(1),T.RULMXINT  DOES IT EXCEED MAX. INT'S ALLOWED          
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$2INT)  TOO MANY INTEGERS ENTERED                 
         MVC   FVXTRA,BCSPACES                                                  
         ZIC   R1,SC2NDLEN                                                      
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    EXITL                                                            
         EXMVC R1,FVXTRA,SC2NDFLD  MOVE IN BAD VALUE ENTERED                    
         B     EXITL                                                            
         SPACE                                                                  
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EFFECTIVE DATE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
EDTDTA   LA    RF,EDTTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
EDTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEDT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEDT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EFFECTIVE DATE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISEDT   OC    TLKTDTE,TLKTDTE     ONLY DISPLAY IF THERE'S A DATE               
         BZ    EXITOK                                                           
         MVC   BCWORK,BCSPACES                                                  
         MVC   BCWORK(2),TLKTDTE                                                
         MVI   BCWORK+2,1                                                       
         GOTO1 VDATCON,BODMCB,(1,BCWORK),(6,FVIFLD)                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EFFECTIVE DATE FIELD (OPTIONAL)                          *         
***********************************************************************         
         SPACE 1                                                                
VALEDT   MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         XC    TLKTDTE,TLKTDTE                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         MVC   BCWORK,BCSPACES                                                  
         GOTO1 VDATVAL,BODMCB,(0,FVIFLD),BCWORK                                 
         OC    BODMCB(4),BODMCB                                                 
         BZ    *+14                M/D/Y IS INVALID                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXITL               INVAILD DATE                                 
*                                                                               
         GOTO1 VDATVAL,BODMCB,(2,FVIFLD),BCWORK                                 
         OC    BODMCB(4),BODMCB                                                 
         BNZ   *+14                NOT M/Y                                      
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXITL               INVAILD DATE                                 
*                                                                               
         MVC   BCWORK+4(2),=C'01'    ALWAYS FIRST OF MONTH                      
         GOTO1 VDATCON,BODMCB,(0,BCWORK),(1,BCWORK+6)                           
         MVC   TLKTDTE,BCWORK+6    MOVE IN JUST YYMM TO TSAR REC                
         B     EXITOK                                                           
*                                                                               
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
         CLI   SREC,R#RATE         RATES RECORD                                 
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
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING CHDRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING CHDRECD,R2                                                       
LAST     USING CHDRECD,R3                                                       
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
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'CHDKEY),THIS.CHDKEY                                      
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
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
T        USING CHDRECD,IOKEY                                                    
NLST     LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(CHDKUNT-CHDRECD),THIS.CHDRECD                              
         BNE   EXITL               CHANGE COMPANY                               
         CLC   T.CHDKUNT(L'FEEUL),FEEUL    WANT 1F ACCTS ONLY                   
         BNE   NLST                                                             
         CLC   T.CHDKCULC,BCSPACES   MUST HAVE CONTRA ACCOUNT                   
         BE    NLST                                                             
*                                                                               
NLST04   MVC   THIS.CHDKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSMULIN+LSSTSAR                                 
         OI    LSSTAT2,LSSADD+LSSNOSEQ+LSS1HEAD                                 
         MVC   LSCOLLIN,=AL2(39)   NUMBER OF COLUMNS PER LIST LINE              
         MVC   LSLINROW,=AL2(2)    NUMBER OF LIST LINES PER ROW                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,CHDRFST-CHDRECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         C     RF,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    RF,CHDRFST-CHDRECD(RF) IT IS NOW.                                
         XR    RE,RE                                                            
*                                                                               
         USING FARELD,RF                                                        
FML02    CLI   FAREL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   FAREL,FARELQ        FAREL?                                       
         BNE   NML04               NO                                           
                                                                                
FML04    S     RF,AIOREC                                                        
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
NLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         XR    RE,RE                                                            
         C     RF,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    NML04                                                            
         LA    RF,CHDRFST-CHDRECD(RF) IT IS NOW.                                
*                                                                               
         USING FARELD,RF                                                        
NML02    CLI   FAREL,0             RECORD END?                                  
         BE    EXITL               YES                                          
NML03    CLI   FAREL,FARELQ        FAREL?                                       
         BE    NML06               YES                                          
                                                                                
NML04    IC    RE,FARLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML06    S     RF,AIOREC                                                        
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
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         LH    RF,MNTDISP                                                       
         A     RF,AIOREC                                                        
         USING FARELD,RF           MOVE IN DETAILS FROM ELEMENT                 
         MVC   TLKTDTE,FAREFF      EFFECTIVE DATE                               
         ZIC   R1,FARLN                                                         
         SH    R1,=Y(FARLN1Q)      SUBTRACT OVERHEAD FROM LENGTH                
         STC   R1,SVLEN                                                         
         LA    R4,TLKKEYW                                                       
         LA    RF,FARDET                                                        
TSARF10  CLI   0(RF),C'='                                                       
         BE    TSARF20                                                          
         MVC   0(1,R4),0(RF)                                                    
         LA    R4,1(R4)                                                         
         LA    RF,1(RF)                                                         
         BCT   R1,TSARF10                                                       
*                                                                               
TSARF20  ZIC   RE,SVLEN                                                         
         SR    RE,R1                                                            
         STC   RE,TLKLHSL          STORE LENGTH OF LHS IN TSAR                  
         ZIC   R1,SVLEN            RE CONTAINS TOTAL LENGTH                     
         SR    R1,RE                                                            
         BCTR  R1,0                                                             
         STC   R1,TLKRHSL          STORE LENGTH OF RHS IN TSAR                  
         LA    RF,1(RF)                                                         
         BCTR  R1,0                                                             
         EXMVC R1,TLKDATA,0(RF)                                                 
         B     EXITOK                                                           
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 MVI   ANYELEMS,NO         RESET BIT                                    
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BNE   EXITOK              A MAIN ACTION OF CHANGE                      
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('CACELQ',AIOREC),0                
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('FARELQ',AIOREC),0                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 CLI   CSACT,A#CHA                                                      
         BE    *+12                                                             
         CLI   CSACT,A#ADD                                                      
         BNE   UPDLST10                                                         
         CLI   ANYELEMS,YES        ANY ELEMENTS IN THE RECORD                   
         BE    UPDLST10            YES - GOOD                                   
         LH    R0,LS1STLIN                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         B     EXITNO              NO INPUT                                     
*                                                                               
         USING CACELD,R3                                                        
UPDLST10 LA    R3,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   CACEL,CACELQ        C/A HEADER ELEMENT                           
T        USING CHDRECD,RF                                                       
         L     RF,AIOREC                                                        
         MVC   CACCNT,CHDKCULC-CHDKEY(RF)  MOVE IN CO/U/L/CONTRA ACCT           
         LA    RE,CACCNT                                                        
         LA    RE,CACCNTA-CACCNT(RE)                                            
         CLC   0(L'CACCNTA,RE),FILLCHAR    IF C/A FILLED WITH *'S               
         BE    UPDLST20                    THAN DON'T FILL IN A NAME            
*                                                                               
         CLC   SVNAME,BCSPACES     ANY CONTRA ACCOUNT NAME?                     
         BE    UPDLST20                                                         
         LA    RE,36                                                            
         LA    R1,SVNAME                                                        
         AR    R1,RE               POINT TO END OF SVNAME                       
         BCTR  R1,0                BUMP BACK ONE TO POINT TO END                
UPDLST15 CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RE,UPDLST15                                                      
*                                                                               
         BCTR  RE,0                                                             
         EXMVC RE,CACNAME,SVNAME                                                
         LA    RE,1(RE)            BUMP BACK TO TRUE LENGTH                     
         LA    R1,CACLN1Q          OVERHEAD LENGTH                              
         AR    R1,RE               RE=LENGTH OF NAME                            
         STC   R1,CACLN                                                         
         B     UPDLST30                                                         
*                                                                               
UPDLST20 MVI   CACNAME,C' '        IF NO NAME MOVE IN 1 SPACE                   
         LA    R1,CACLN1Q+1                                                     
         STC   R1,CACLN                                                         
*                                                                               
UPDLST30 GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIOREC,BOELEM,0                    
*                                                                               
         CLI   CSACT,A#DEL         ACTION=DELETE?                               
         BE    EXITOK              THAN DON'T BUILD CLT LEVEL STAR REC          
         CLI   CSACT,A#RES         ACTION=RESTORE?                              
         BE    EXITOK                                                           
         BAS   RE,ADDSTAR          BUILD CLIENT LEVEL STAR REC                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CHDRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  LM    R2,R3,SVPARMS3                                                   
         CLI   CSACT,A#CHA         ONLY UPDATE ELEMENT IF WE HAVE               
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
                                                                                
         MVI   ANYELEMS,YES                                                     
         USING FARELD,R4                                                        
         LA    R4,BOELEM                                                        
         XC    BOELEM,BOELEM       MOVE IN DETAILS TO ELEMENT                   
         MVI   FAREL,FARELQ                                                     
         MVC   FAREFF,TLKTDTE      EFFECTIVE DATE                               
         ZIC   R1,TLKLHSL          LEN OF LEFT HAND SIDE                        
         BCTR  R1,0                OF KEYWORD                                   
         EXMVC R1,FARDET,TLKKEYW                                                
*                                                                               
         LA    R1,1(R1)            BUMP UP TO TRUE LENGTH                       
         LA    RF,FARDET                                                        
         AR    RF,R1               POINT TO NEXT OPEN SPOT IN FIELD             
         MVI   0(RF),C'='                                                       
         LA    RF,1(RF)                                                         
*                                                                               
         ZIC   R1,TLKRHSL          LEN OF RIGHT HAND SIDE                       
         BCTR  R1,0                                                             
         EXMVC R1,0(RF),TLKDATA                                                 
         ZIC   R1,TLKLHSL          ADD LENGTHS OF LHS OF KEYWORD                
         ZIC   RE,TLKRHSL          AND RHS OF KEYWORD                           
         AR    R1,RE                                                            
         LA    R1,1(R1)            ADD ONE FOR EQUAL SIGN TOO                   
         LA    RE,FARLN1Q                                                       
         AR    R1,RE               AND ADD THAT TO OVERHEAD LENGTH              
         STC   R1,FARLN                                                         
*                                                                               
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* ADD RECORD WITH ASTERISKS IF DOESN'T ALREADY EXIST                            
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 2                                                                
ADDSTAR  NTR1                                                                   
         OC    SVDIV,SVDIV         IF THEY ENTERED A DIVISION                   
         BNZ   EXITOK              THAN DON'T NEED TO ADD                       
*                                                                               
         USING CACRECD,R4                                                       
         LA    R4,IOKEY                                                         
         MVC   IOKEY(L'CACKEY),BCSPACES                                         
         MVC   CACKCPY,CUABIN                                                   
         MVC   CACKUNT(L'FEEUL),FEEUL                                           
         LA    R4,CACKEY+3                                                      
         ZIC   R1,LDGFLNA                                                       
         BCTR  R1,0                                                             
         EXMVC R1,0(R4),SVCLI      MOVE IN CLIENT                               
         LA    R1,1(R1)            RESET LENGTH                                 
         AR    R4,R1               POINT TO DIVISION FIELD                      
         LA    R3,IOKEY                                                         
         LA    R3,CHDKOFF-CHDKEY(R3) POINT TO END OF ACCOUNT                    
         SR    R3,R4               R3=LENGTH FOR MOVE                           
         BCTR  R3,0                                                             
         EXMVC R3,0(R4),FILLCHAR   MOVE IN *'S                                  
         L     R1,=AL4(XOHIUPD+XOACCDIR+XIO1)                                   
         GOTO1 AIO                                                              
         CLC   IOKEY,IOKEYSAV                                                   
         BNE   ADDST05             GO ADD THE RECORD                            
*                                                                               
         LA    R4,IOKEY                                                         
         TM    CACKSTAT,ACTSDELT   IS IT A DELETED RECORD?                      
         BNO   EXITOK                                                           
         NI    CACKSTAT,X'FF'-ACTSDELT  UNDELETE IT                             
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO1)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD COMPANY RECORD                           
         L     RF,AIO1                                                          
         NI    CACRSTAT,X'FF'-ACTSDELT  UNDELETE IT                             
         LHI   R1,XOPUT+XOACCMST+XIO1     PUT THE RECORD BACK                   
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
         LHI   R1,XOWRITE+XOACCDIR+XIO1   WRITE DIRECTORY RECORD BACK           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXITOK                                                           
*                                                                               
ADDST05  L     R4,AIO2                                                          
         MVC   0(L'CACKEY,R4),IOKEYSAV                                          
         LH    RE,=Y(CACRFST-CACRECD+1)  ACCOUNTS LENGTH                        
         STCM  RE,3,CACRLEN                                                     
         XC    CACRSTA,CACRSTA  CLEAR STATUS FIELD                              
         MVC   CACRLMOS(L'CACRLMOS+L'CACRHMOS),BCSPACES                         
*                                                                               
         XC    BOELEM,BOELEM                                                    
         USING NAMELD,BOELEM                                                    
         MVI   NAMEL,NAMELQ       X'20' NAME FOR *** ACCOUNT                    
         CLC   SVCLINM,BCSPACES                                                 
         BE    ADDST20                                                          
*                                                                               
         LA    RE,36                                                            
         LA    R1,SVCLINM                                                       
         AR    R1,RE               POINT TO END OF SVCLINM                      
         BCTR  R1,0                BUMP BACK ONE TO POINT TO END                
ADDST10  CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   RE,ADDST10                                                       
*                                                                               
         EXMVC RE,NAMEREC,SVCLINM                                               
         LA    R1,NAMLN1Q          OVERHEAD LENGTH                              
         AR    R1,RE               RE=LENGTH OF NAME                            
         STC   R1,NAMLN                                                         
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO2,BOELEM,0,0,0                  
*                                                                               
ADDST20  XC    BOELEM,BOELEM                                                    
         GOTO1 AADDRST,CACRECD       ADD RSTEL STATUS ELEM                      
         GOTO1 AADDBAL,CACRECD       ADD 32/33 ELEMENTS                         
         OI    CACRSTAT,ACTSABLP   ACCOUNT HAS BALANCE ELEMENT                  
*                                                                               
         DS    0H                                                               
         LHI   R1,XOADDREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
EFFALL   DC    32X'FF'                                                          
*                                                                               
FILLCHAR DC    12C'*'                                                           
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
WRKCODE  DC    C'WC'                                                            
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
RATELN   EQU   5                                                                
MAXFEE   EQU   22                  MAX # OF FEE RULES ALLOWED                   
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#YES,L'UC@YES,L                                                
         DCDDL AC#NO,L'UC@NO,L                                                  
         DCDDL AC#ALL,L'UC@ALL,L                                                
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORKA                                                                    
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
SVADDR   DS    F                                                                
MNTDISP  DS    H                                                                
*                                                                               
LDGFLEN  DS    0XL4                1F LEDGER LENGTHS                            
LDGFLNA  DS    XL(L'LDGTLVA)                                                    
LDGFLNB  DS    XL(L'LDGTLVB)                                                    
LDGFLNC  DS    XL(L'LDGTLVC)                                                    
LDGFLND  DS    XL(L'LDGTLVD)                                                    
*                                                                               
LDG1RLEN  DS    0XL4               1R LEDGER LENGTHS                            
LDGRLNA  DS    XL(L'LDGTLVA)                                                    
LDGRLNB  DS    XL(L'LDGTLVB)                                                    
LDGRLNC  DS    XL(L'LDGTLVC)                                                    
LDGRLND  DS    XL(L'LDGTLVD)                                                    
*                                                                               
ANYELEMS DS    XL1                 DO WE HAVE ANY ELEMENTS                      
SVLEN    DS    XL1                 SAVED LENGTH                                 
FEEUL    DS    CL2                 SAVED 1F FEE LEDGER                          
STFUL    DS    CL2                 SAVED 1R LEDGER                              
SVNAME   DS    CL36                SAVED CONTRA ACCOUNT NAME FIELD              
SVCLINM  DS    CL36                SAVED CLIENT NAME FOR ASTERISK REC           
SVCLI    DS    CL3                 SAVED CLIENT                                 
SVDIV    DS    CL9                 SAVED DIVISION                               
SCANBLK  DS    CL17                SCANNER BLOCK                                
*                                                                               
*EELST   DS    CL(MAXRATE*17)      TABLE OF FEE RULES TO CHECK FOR DUPS         
*EEEND   DS    XL4                 END OF TABLE                                 
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
         SPACE 2                                                                
DSLISTU  DS    0D                  UPPERCASE FOR MATCHING                       
UC@YES   DS    CL(RATELN)                                                       
UC@NO    DS    CL(RATELN)                                                       
UC@ALL   DS    CL(RATELN)                                                       
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT                                                                  
***********************************************************************         
* FEE LIST DSECT (TO CHECK FOR DUPS)                                            
***********************************************************************         
         SPACE 2                                                                
FEELSTD  DSECT                                                                  
FEEKEY   DS    CL17                                                             
FEELNQ   EQU   *-FEELSTD                                                        
         SPACE 2                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKKEYW  DS    CL15                FEE RULE KEYWORD                             
         ORG   TLUSER                                                           
TLKDATA  DS    XL10                FEE RULE DATA                                
TLKLHSL  DS    XL1                 LENGTH OF LHS OF FEE RULE                    
TLKRHSL  DS    XL1                 LENGTH OF RHS OF FEE RULE                    
TLKTDTE  DS    CL(L'FAREFF)        EFFECTIVE DATE (PWOS)                        
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACFIL1A   08/10/11'                                      
         END                                                                    
