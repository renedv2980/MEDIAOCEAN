*          DATA SET ACFIL55    AT LEVEL 005 AS OF 08/10/11                      
*PHASE T62355C                                                                  
*                                                                               
*NRAK 003 12OCT09 <BR27823L>ALLOW MULTIPLE LEVELS ON ONE TEMPLATE               
*                                                                               
         SPACE 1                                                                
FIL55    TITLE 'TIMESHEET TEMPLATE RECORD (TIMETEMP)'                           
         SPACE 2                                                                
         SPACE 2                                                                
FIL55    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL55**,RA,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
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
*                                                                               
         L     RE,ATWA                                                          
         MVC   SESNL,TWASESNL-TWAD(RE)                                          
         L     R6,ATWA                                                          
         AH    R6,=Y(TWUSER-TWAD)                                               
         USING SAVED,R6                                                         
*                                                                               
         LR    RF,R2                                                            
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
         L     R1,CALLR1           RETURN PARAMETERS TO CALLER                  
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
EXIT     XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITSHRT MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     EXITL               EXIT WITH FIELD TOO SHORT SET                
EXITLONG MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               EXIT WITH FIELD TOO LONG SET                 
*                                                                               
EXITIACT MVC   FVMSGNO,=AL2(AE$IACTS)   INVALID ACTION FOR THIS SCREEN          
         LH    RF,GSDSPACT         SET CURSOR TO ACTION FIELD                   
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
         B     EXITL                                                            
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
*                                                                               
T        USING LDGRECD,IOKEY                                                    
INIT     MVC   T.LDGKEY,BCSPACES   GET SJ CLI/PRO/JOB LENGTH                    
         MVC   T.LDGKCPY,CUABIN                                                 
         MVC   T.LDGKUNT(L'PRODUL),PRODUL                                       
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                NO SJ LEDGER?                                
         ICM   RF,15,ACALDG                                                     
         MVC   CLILEN(L'CLILEN+L'PROLEN+L'JOBLEN),LDGTLVA-LDGTABD(RF)           
*                                                                               
         B     EXITOK                                                           
*                                                                               
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDING           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
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
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITERH                                                            
*                                                                               
TABLEOO  DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SCREEN OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(SCREEN)                                                        *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCREEN   LM    R0,R3,SVPARMS                                                    
         LA    RF,TABLSCR                                                       
         B     ITER                                                             
*                                                                               
TABLSCR  DC    AL1(SSET),AL1(0,0,0),AL4(SCRMSET)                                
         DC    AL1(SLSET),AL1(0,0,0),AL4(SCRLSET)                               
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* SET MAINTENANCE DATA SCREEN                                         *         
***********************************************************************         
         SPACE 1                                                                
SCRMSET  MVI   GSSMCODE,0                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET LIST SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLSET  MVI   GSSLCODE,0                                                       
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* KEY OBJECT                                                          *         
* ----------                                                          *         
* SVPARMS1 HOLDS EQUATED OBJECT                                       *         
* SVPARMS2 HOLDS EQUATED VERB                                         *         
* SVPARMS3 A(KEY)                                                     *         
* SVPARMS4 HOLDS SUB-ACTION                                           *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R1,R2,SVPARMS2                                                   
         USING TTMRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
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
KFKVAL   XC    TTMKEY,TTMKEY                                                    
         MVI   TTMKTYP,TTMKTYPQ    TIME TEMPLATE RECORD TYPE                    
         MVI   TTMKSUB,TTMKSUBQ    AND SUBTYPE                                  
         MVC   TTMKCPY,CUABIN      CONNECTED USER                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    TTMKEY,TTMKEY                                                    
         MVI   TTMKTYP,TTMKTYPQ    TIME TEMPLATE RECORD TYPE                    
         MVI   TTMKSUB,TTMKSUBQ    AND SUBTYPE                                  
         MVC   TTMKCPY,CUABIN                                                   
*                                                                               
         XC    SVFLTS(SVFLTLQ),SVFLTS                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** LAST TIME FOR KEY OBJECT ***                  
*                                 ------------------------                      
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
         CLC   CDOPTION,SDOPTION   TEST DELETE OPTION CHANGED                   
         BE    EXITOK                                                           
         MVI   LSSCIND1,LSSCIFLT   REFRESH LIST                                 
         MVC   SDOPTION(SDOPTSL),CDOPTION                                       
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
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
         USING TTMRECD,R2                                                       
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
RFTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RFRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD                                  *         
***********************************************************************         
         SPACE 1                                                                
RFADD    CLI   CSACT,A#ADD                                                      
         BNE   RFADD02                                                          
         CLI   GSSMPAGE,1          PAGE 1?                                      
         BNE   EXITIACT            NO - INVALID ACTION FOR THIS SCREEN          
RFADD02  B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    GOTO1 ADELRECS,BOPARM,TTMRECD                                          
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - CHANGE                               *         
***********************************************************************         
         SPACE 1                                                                
RFWRT    GOTO1 ADELRECS,BOPARM,TTMRECD,0                                        
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RESTORE                              *         
***********************************************************************         
         SPACE 1                                                                
RFRES    DS    0H                                                               
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE                                                                  
RECLAST  L     R1,SVPARMS4                                                      
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE                                                                  
RLADD    DS    0H                                                               
         GOTO1 AADDRECS,BOPARM,TTMRECD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE                                                                  
RLDEL    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE                               *         
***********************************************************************         
         SPACE                                                                  
RLRES    DS    0H                                                               
         GOTO1 AADDRECS,BOPARM,TTMRECD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE                                                                  
RLWRT    DS    0H                                                               
         GOTO1 AADDRECS,BOPARM,TTMRECD                                          
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                               
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
         USING TTMRECD,R2                                                       
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
         LA    RF,KNOWLQ(,RF)                                                   
         B     DATA04                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING TTMRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
*        BR    RF                                                               
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                         *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(F#TIME#1RAC),AL4(AC1R)  1R ACCOUNT CODE                      
         DC    AL2(F#TIME#1RNAM),AL4(ACN1R) 1R ACCOUNT NAME                     
         DC    AL2(F#TIME#CPJC),AL4(CPJ)   CLI/PRO/JOB CODE                     
         DC    AL2(F#TIME#CPJT),AL4(TMTY)  CLIENT TYPE OF TIME                  
         DC    AL2(F#TIME#CPJN),AL4(CPJN)  CLI/PRO/JOB NAME                     
         DC    AL2(F#TIME#WCC),AL4(WCC)    WORKCODE                             
         DC    AL2(F#TIME#NCLC),AL4(NCC)   NON-CLIENT TIME CODE                 
         DC    AL2(F#TIME#NCLN),AL4(NCN)   NON-CLIENT TIME NAME                 
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
         SPACE 2                                                                
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
FIL55    CSECT                                                                  
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
         B     EXITOK                                                           
*                                                                               
FIL55N   CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR 1R ACCOUNT CODE                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
AC1R     NTRDO                                                                  
*                                                                               
AC1RTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DIS1RA)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DIS1RA)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSET1RA)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLT1RA)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAL1RA)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLT1RA)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFT1RA)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCH1RA)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT 1R ACCOUNT CODE AFTER SELECT FROM LIST                    *         
***********************************************************************         
         SPACE 1                                                                
DSET1RA  DS    0H                                                               
         B     FLTXX               RETURN 'NOT VALID' TO FORCE UNPROT           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A 1R ACCOUNT CODE                                           *         
***********************************************************************         
         SPACE 1                                                                
DIS1RA   MVC   FVIFLD(L'TTMK1RAC),TTMK1RAC                                      
*                                                                               
D1RAX    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A 1R ACCOUNT CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
VAL1RA   MVC   A1RAFLD,FVADDR      SAVE A(FIELD)                                
         CLC   FVIFLD(L'SV1RAC),BCSPACES                                        
         BE    EXITNO                                                           
         MVC   SV1RAC,FVIFLD                                                    
*                                                                               
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         GOTO1 AVALC1R,BOPARM,(FVILEN,FVIFLD)                                   
         BE    V1RA04                                                           
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
V1RA04   DS    0H                                                               
         MVC   TTMK1RAC,C1RCODE                                                 
*                                                                               
V1RA10   MVC   FLTIFLD(L'ACTKACT),FVIFLD                                        
         B     EXITOK                                                           
*        DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A 1R ACC CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLT1RA  MVC   FVIFLD(L'ACTKACT),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A 1R ACCOUNT CODE FILTER FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VFLT1RA  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         GOTO1 AVALC1R,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VFLTR04                                                          
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
VFLTR04  MVC   FLTIFLD(L'ACTKACT),FVIFLD                                        
         MVC   SV1RAFL(L'ACTKACT),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH FOR A 1R ACCOUNT CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
SRCH1RA  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,CSTUL,         X        
               ACOM,(X'14',0)                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT ,                                                                
***********************************************************************         
* DO FILTERING FOR 1R ACCOUNT CODE                                   *          
***********************************************************************         
         SPACE 1                                                                
DOFT1RA  CLC   TTMK1RAC,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING 1R ACCOUNT NAME FIELD                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACN1R    NTRDO                                                                  
*                                                                               
ACN1RTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DIS1RAN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A 1R ACCOUNT NAME FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DIS1RAN  GOTO1 AVALC1R,BOPARM,(L'TTMK1RAC,TTMK1RAC)                             
         MVC   FVIFLD(L'C1RNAME),C1RNAME                                        
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DEFAULT INDICATOR                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TMTY     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
TMTYTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISTMTY)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTMTY)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DEFAULT INDICATOR FIELD                                    *          
***********************************************************************         
         SPACE 1                                                                
DISTMTY  MVC   FVIFLD(1),BCSPACES                                               
         CLC   TLKTIMET,BCSPACES TEST BLANK ENTRY                               
         BNH   EXITOK              YES - NOTHING TO DO                          
         MVC   FVIFLD(1),TLKTIMET                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DEFAULT INDICATOR FIELD                                   *          
***********************************************************************         
         SPACE 1                                                                
VALTMTY  CLI   FVILEN,0                                                         
         BNE   VTMTY02                                                          
         MVC   FVMSGNO,=AL2(AE$MISIF) MISSING INPUT FIELD                       
         B     EXITL                                                            
VTMTY02  XC    TLKTIMET,TLKTIMET                                                
         XC    TIMETYPE,TIMETYPE                                                
         CLI   FVIFLD,C'N'                                                      
         BE    VTMTY04                                                          
         CLI   FVIFLD,C'R'                                                      
         BE    VTMTY04                                                          
         CLI   FVIFLD,C'B'                                                      
         BNE   EXITNV                                                           
VTMTY04  MVC   TLKTIMET,FVIFLD                                                  
         MVC   TIMETYPE,FVIFLD                                                  
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CLI/PRO/JOB CODE LIST                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CPJ      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
CPJTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPJ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPJC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCPJ)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CLI/PRO/JOB CODE LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISCPJ   MVC   FVIFLD(L'TLKCPJC),TLKCPJC                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CLI/PRO/JOB CODE LIST FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VALCPJC  MVC   TLRLEN,=AL2(TLLNQ)                                               
         CLI   FVILEN,0                                                         
         BNE   VCPJC02                                                          
         XC    TLKCPJC,TLKCPJC                                                  
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
VCPJC02  LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS                                                       
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         GOTO1 AVALCPJ,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VCPJC04                                                          
         CLC   FVILEN,CLILEN                                                    
*&&UK*&& BNE   *+14                                                             
*&&US*&& BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCLI)                                           
         B     EXITL               INVALID CLIENT CODE                          
         CLC   FVILEN,PROLEN                                                    
*&&UK*&& BNE   *+14                                                             
*&&US*&& BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INPRO)                                           
         B     EXITL               INVALID PRODUCT CODE                         
         MVC   FVMSGNO,=AL2(AE$INJOB)                                           
         B     EXITL               INVALID JOB CODE                             
*                                                                               
VCPJC04  MVC   TLKCPJC,CPJCODE     SORT THE CLI/PRO/JOB CODES                   
         MVC   TLKCPJNM,CPJNAME    SAVE CLI/PRO/JOB NAME                        
         MVC   TLKTIMET,TIMETYPE   SAVE TIME TYPE                               
         B     EXITOK                                                           
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO/JOB CODE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
SRCHCPJ  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   >        
               (X'13',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A CLI/PRO/JOB CODE NAME FIELD            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CPJN     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
CPJNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPJN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN CLI/PRO/JOB CODE NAME FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISCPJN  MVC   FVIFLD(L'TLKCPJNM),TLKCPJNM                                      
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR WORKCODE LIST                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
WCC      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
WCCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWCL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWCL)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHWC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY WORKCODE LIST FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISWCL   MVC   FVIFLD(L'TLKWC),TLKWC                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE WORKCODE LIST FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALWCL   XC    TLKWC,TLKWC                                                      
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVILEN,2                                                         
         BNE   VWCC02              WORKCODE OF WRONG LENGTH                     
         XC    WCCODE,WCCODE                                                    
         GOTO1 AVALWC,BOPARM,(FVILEN,FVIFLD)                                    
         BE    VWCC04                                                           
VWCC02   MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITL               INVALID WORKCODE                             
*                                                                               
VWCC04   MVC   TLKWC,WCCODE        SAVE WORKCODE                                
         B     EXITOK                                                           
*                                                                               
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A WORKCODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
SRCHWC   GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,WC,ACOM,       >        
               (X'44',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR NON-CLIENT CODE LIST                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NCC      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
NCCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNCC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNCC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHNC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NON-CLIENT CODE LIST FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DISNCC   MVC   FVIFLD(L'TLKNCC),TLKNCC                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE NON-CLIENT CODE LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VALNCC   MVC   TLRLEN,=AL2(TLLNQ)  SET TSAR RECLEN                              
         CLI   FVILEN,0                                                         
         BNE   VNCC04                                                           
         XC    TLKNCC,TLKNCC                                                    
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
VNCC04   LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 80 ITEMS                              
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
*                                                                               
VNCC10   CLI   FVILEN,L'TLKNCC                                                  
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
         GOTO1 AVALNCL,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VNCC20                                                           
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
VNCC20   MVC   TLKNCC,NCLCODE      SORT THE NON CLIENT CODES                    
         MVC   TLKNCLNM,NCLNAME    SAVE NON CLIENT NAME                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A NON-CLIENT CODE FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
SRCHNC   GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,NCTUL,ACOM,    X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A NON-CLIENT NAME FIELD                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NCN      NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
NCNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNCN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A NON-CLIENT NAME FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISNCN   MVC   FVIFLD(L'TLKNCLNM),TLKNCLNM                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
         EJECT ,                                                                
FIL55    CSECT                                                                  
*                                                                               
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
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   NTRO02                                                           
         CLI   SREC,R#TIME         TIME TEMPLATE RECORD                         
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   NTRO02                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
*                                                                               
NTRO02   CLI   CSREC,R#TIME                                                     
         BNE   EXITOK                                                           
         CLI   CSACT,A#LST                                                      
         BNE   EXITOK                                                           
         L     R4,ATLST                                                         
         USING TLSTD,R4                                                         
         MVC   SV1RAC,TLK1RACC                                                  
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
***********************************************************************         
         SPACE 1                                                                
XITIN    B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING TTMRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING TTMRECD,R2                                                       
LAST     USING TTMRECD,R3                                                       
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
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR)                            
         DC    AL1(LLSTLAST),AL1(0,0,0),AL4(SCRLAST)                            
         DC    AL1(LSCRLAST),AL1(0,0,0),AL4(SCRLAST)                            
*                                                                               
         DC    AL1(LSCRFRST),AL1(0,0,1),AL4(SCRF1)                              
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
*                                                                               
         DC    AL1(LSCRFRST),AL1(0,0,2),AL4(SCRF1)                              
         DC    AL1(LGETFRST),AL1(0,0,2),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,2),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,2),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,2),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,2),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,2),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,2),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,2),AL4(UPDLAST1)                           
*                                                                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INIT LIST                                                           *         
***********************************************************************         
         SPACE 1                                                                
ILST     NI    LSSTAT1,X'FF'-LSSENTK                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING TTMRECD,IOKEY                                                    
FLST     TM    LSSCIND1,LSSCIFLT   TEST FILTERS CHANGED                         
         BO    FLST02              YES, IGNORE ERRIND                           
         TM    ERRIND,ERMAXIO                                                   
         BZ    FLST02                                                           
         MVC   THIS.TTMKEY,SKEYLAST                                             
FLST02   MVI   ERRIND,0            RESET ERROR INDICATOR                        
         XC    SKEYLAST,SKEYLAST   OR CLEAR SAVED LAST KEY                      
*                                                                               
         MVC   X.TTMKEY,THIS.TTMKEY                                             
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
NLST02   GOTO1 AIOCHK            CHECK FOR MAX IOS                              
         BE    NLST02A                                                          
         OI    ERRIND,ERMAXIO                                                   
         MVC   SKEYLAST,IOKEY      WHEN RESUMED, START HERE                     
         B     EXITL                                                            
*                                                                               
NLST02A  CLC   X.TTMKEY(TTMKREM-TTMRECD),THIS.TTMRECD                           
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
NLST06   MVC   THIS.TTMKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
*                                                                               
         B     EXITOK                                                           
         DROP  THIS,LAST,X                                                      
         SPACE 2                                                                
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM DIRECTORY RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TTMRECD,R2                                                       
         USING TLSTD,R3                                                         
TSARDIR  LM    R2,R3,SVPARMS3                                                   
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         MVC   TLK1RACC,SV1RAC                                                  
         LHI   R1,XOGET+XOACCMST+XIO11                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
         L     R2,AIOREC                                                        
         B     EXITOK                                                           
         DROP  R2,R3                                                            
***********************************************************************         
* LAST FOR LIST SCREEN                                                *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST  TM    ERRIND,ERMAXIO      MAX IOS SET FROM NLST?                       
         BZ    EXITOK                                                           
         MVI   FVOMTYP,C'I'                                                     
         MVC   FVMSGNO,=AL2(AI$MAXIO)                                           
         NI    LSLTIND1,FF-LSLTIEOL    NOT DONE YET                             
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1/2                                     (LINIT) *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         OI    LSSTAT2,LSSADD+LSSNOSEQ                                          
         MVC   LSCOLLIN,=AL2(78)  NUMBER OF COLUMNS PER LIST LINE               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST SCREEN 1/2                                (LSCRFRST) *         
***********************************************************************         
         SPACE 1                                                                
SCRF1    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1/2                                  (LLSTFRST) *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,TTMRFST-TTMRECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1/2                                       (LGETFRST) *         
* BUILD LSTBLK FROM TIME TEMPLATE RECORD LIDELS                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TTMRECD,R5                                                       
FLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIOREC           A(RECORD)                                    
         C     R5,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    R5,TTMRFST          IT IS NOW.                                   
         XR    RE,RE                                                            
*                                                                               
         USING LIDELD,R5                                                        
FML02    CLI   LIDEL,0             RECORD END?                                  
         BE    EXITNO              YES                                          
         CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML18               NO                                           
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   FML04               NO                                           
         CLI   LIDTYPE,LIDTCLIT    IS IT CLI/PRO/JOB LIST                       
         BE    FML10                                                            
         B     NML18                                                            
FML04    CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BE    *+6                 NO                                           
         DC    H'0'                                                             
         CLI   LIDTYPE,LIDTNCLC    IS IT NON CLIENT LIST                        
         BNE   NML18                                                            
*                                                                               
FML10    LA    R4,LIDDATA          ADDRESS OF DATA START OF ELEMENT             
         SR    R4,R5               MINUS ELEMENT START ADDRESS                  
         STH   R4,CURDISP          CURRENT DISPLACEMENT INTO ELEMENT            
         XR    R4,R4                                                            
         IC    R4,LIDITLN          GET LENGTH OF DATA                           
         STH   R4,DATALEN          SAVE THIS LENGTH                             
         IC    R4,LIDLN                                                         
         STH   R4,TOTELLN          SAVE TOTAL ELEMENT LENGTH                    
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   FML12               NO                                           
         L     RF,AVALTMT          SET TO VALIDATE TIME TYPE                    
         GOTO1 (RF),BOPARM,(L'TLKTIMET,LIDDATA)                                 
         L     RF,AVALWC           SET TO VALIDATE WORKCODE                     
         GOTO1 (RF),BOPARM,(L'TLKWC,LIDDATA+1)                                  
FML11    L     RF,AVALCPJ          SET TO VALIDATE CLIENT PROD JOB              
         XR    RE,RE                                                            
         IC    RE,LIDITLN                                                       
         AHI   RE,-3                                                            
         STC   RE,MYBYTE                                                        
         GOTO1 (RF),BOPARM,(MYBYTE,LIDDATA+L'TLKWC+1)                           
         B     FML19                                                            
FML12    CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AVALNCL          SET TO VALIDATE NON CLIENT ACCOUNT           
FML18    GOTO1 (RF),BOPARM,(LIDITLN,LIDDATA)                                    
FML19    BNE   NML20               NOT VALID                                    
FML20    S     R5,AIOREC                                                        
         STH   R5,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1/2                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TTMRECD,R5                                                       
NLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIOREC           A(RECORD)                                    
         C     R5,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    NML20                                                            
         LA    R5,TTMRFST          IT IS NOW.                                   
*                                                                               
         USING LIDELD,R5                                                        
NML02    CLI   LIDEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML18               YES                                          
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   NML04               NO                                           
         CLI   LIDTYPE,LIDTCLIT    IS IT CLI/PRO/JOB LIST                       
         BE    NML30                                                            
         B     NML18                                                            
NML04    CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LIDTYPE,LIDTNCLC    IS IT NON CLIENT ACCOUNT LIST                
         BE    NML30                                                            
*                                                                               
NML18    XR    RE,RE                                                            
         IC    RE,LIDLN            GET NEXT ELEMENT                             
         AR    R5,RE                                                            
         B     NML02                                                            
*                                                                               
NML20    LH    R4,DATALEN          LENGTH OF DATA                               
         AH    R4,CURDISP          CURRENT DISPLACEMENT TO DATA                 
         STH   R4,CURDISP          SAVE NEXT DISPLACEMENT TO DATA               
         CH    R4,TOTELLN          HAVE WE REACHED END OF ELEMENT               
         BNL   NML18               YES                                          
         AR    R4,R5                                                            
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   NML22               NO                                           
         L     RF,AVALTMT          SET TO VALIDATE TIME TYPE                    
         GOTO1 (RF),BOPARM,(L'TLKTIMET,(R4))                                    
         L     RF,AVALWC           SET TO VALIDATE WORKCODE                     
         LR    RE,R4                                                            
         LA    RE,L'TLKTIMET(RE)                                                
         GOTO1 (RF),BOPARM,(L'TLKWC,(RE))                                       
NML21    L     RF,AVALCPJ          SET TO VALIDATE CLIENT/PRODUCT/JOB           
         XR    R0,R0                                                            
         IC    R0,LIDITLN                                                       
         AHI   R0,-3                                                            
         STC   R0,MYBYTE                                                        
         LR    RE,R4                                                            
         LA    RE,L'TLKTIMET+L'TLKWC(RE)                                        
         GOTO1 (RF),BOPARM,(MYBYTE,(RE))                                        
         B     NML29                                                            
NML22    CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AVALNCL          SET TO VALIDATE NON CLIENT ACCOUNT           
NML28    GOTO1 (RF),BOPARM,(LIDITLN,(R4))                                       
NML29    BE    NML40               VALID CLI/PRO/JOB CODE                       
         B     NML20               INVALID CLI/PRO/JOB CODE                     
*                                                                               
NML30    LA    R4,LIDDATA          ADDRESS OF DATA START OF ELEMENT             
         SR    R4,R5                                                            
         STH   R4,CURDISP          CURRENT DISPLACEMENT INTO ELEMENT            
         XR    R4,R4                                                            
         IC    R4,LIDITLN          GET LENGTH OF DATA                           
         STH   R4,DATALEN          SAVE THIS LENGTH                             
         IC    R4,LIDLN                                                         
         STH   R4,TOTELLN          SAVE TOTAL ELEMENT LENGTH                    
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   NML32               NO                                           
         L     RF,AVALTMT          SET TO VALIDATE TIME TYPE                    
         GOTO1 (RF),BOPARM,(L'TLKTIMET,LIDDATA)                                 
         L     RF,AVALWC           SET TO VALIDATE WORKCODE                     
         GOTO1 (RF),BOPARM,(L'TLKWC,LIDDATA+1)                                  
NML31    L     RF,AVALCPJ          SET TO VALIDATE CLIENT PROD JOB              
         XR    RE,RE                                                            
         IC    RE,LIDITLN                                                       
         AHI   RE,-3                                                            
         STC   RE,MYBYTE                                                        
         GOTO1 (RF),BOPARM,(MYBYTE,LIDDATA+L'TLKWC+1)                           
         B     NML39                                                            
NML32    CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AVALNCL          SET TO VALIDATE NON CLIENT ACCOUNT           
NML38    GOTO1 (RF),BOPARM,(LIDITLN,LIDDATA)                                    
NML39    BNE   NML20               NOT VALID CLI/PRO/JOB CODE                   
                                                                                
NML40    S     R5,AIOREC                                                        
         STH   R5,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)                                               
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   TSFL02              NO                                           
         XC    TLKCPJC,TLKCPJC                                                  
         XC    TLKWC,TLKWC                                                      
         MVC   TLKCPJC,CPJCODE                                                  
         MVC   TLKCPJNM,CPJNAME                                                 
         MVC   TLKTIMET,TIMETYPE                                                
         MVC   TLKWC,WCCODE                                                     
         B     EXITOK                                                           
*                                                                               
TSFL02   CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON-CLIENT PAGE                    
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    TLKNCC,TLKNCC                                                    
         MVC   TLKNCC,NCLCODE                                                   
         MVC   TLKNCLNM,NCLNAME                                                 
         B     EXITOK                                                           
*                                                                               
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE PAGE 1/2                                      *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 MVI   ANYLINES,NO                                                      
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   UPDF02              NO                                           
*        MVC   LASTCODE,BCSPACES                                                
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),        X        
               (2,=AL1(L'TLKCPJC+L'TLKTIMET+L'TLKWC,LIDTCLIT))                  
         B     EXITOK                                                           
*                                                                               
UPDF02   CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),        X        
               (2,=AL1(L'TLKNCC,LIDTNCLC))                                      
         B     EXITOK                                                           
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1/2                                    *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TTMRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
         MVI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         LM    R2,R3,SVPARMS3                                                   
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         CLI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         BNE   EXITOK                                                           
*                                                                               
T        USING LIDELD,BOELEM                                                    
         USING TTMRECD,R2                                                       
         XC    BOELEM,BOELEM                                                    
         MVI   T.LIDEL,LIDELQ                                                   
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   ULST104             NO                                           
         MVI   T.LIDTYPE,LIDTCLIT                                               
         MVI   T.LIDITLN,L'TLKCPJC+L'TLKTIMET+L'TLKWC                           
         B     ULST112                                                          
ULST104  CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTNCLC                                               
         MVI   T.LIDITLN,L'TLKNCC                                               
ULST112  LA    R4,T.LIDDATA                                                     
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
ULST120  LA    R1,TSANXT           DEAL WITH ALL DELETE REQUEST                 
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    ULST200             END OF FILE                                  
         CLC   TLKSES,SESNL        CHECK NEST LEVEL                             
         BNE   ULST200             DONE ALL FOR THIS LEVEL                      
                                                                                
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE?           
         BNE   ULST172                                                          
*                                                                               
*                                                                               
*    REMOVED AS A TEMPLATE SHOULD BE ABLE TO INCLUDE MULTIPLE LEVELS            
*    OF AN ACCOUNT WITHIN ONE TEMPLATE                                          
*                                                                               
*ULST121  CLC   LASTCODE,BCSPACES                                               
*         BE    ULST168                                                         
*        XR    RE,RE                                                            
*        LA    RE,L'LASTCODE                                                    
*        LA    R5,LASTCODE+L'LASTCODE-1                                         
*ULST122  CLI   0(R5),C' '                                                      
*         BNE   ULST123                                                         
*         SHI   R5,1                                                            
*         BCT   RE,ULST122             RE=ACTUAL LENGTH OF LASTCODE             
*         DC    H'0'                                                            
*                                                                               
*ULST123  SHI   RE,1                                                            
*         EXCLC RE,LASTCODE,TLKCPJC    COMPARE LAST CLI/PRO/JOB CODE            
*         BNE   ULST168                WITH CURRENT ONE                         
*ULST164  MVC   FVMSGNO,=AL2(AE$DUPAC) DUPLICATE ENTRY                          
*         MVC   FVXTRA,LASTCODE                                                 
*         NI    LSLTIND1,FF-LSLTIBLD   REBUILD THE LIST                         
*         XC    GCLASKEY,GCLASKEY      SET KEY HAS BEEN CHANGED                 
*         NI    GSINDSL1,FF-GSIXMNT    TURN OF MAINT SCREEN LOADED FLAG         
*         B     EXITL                  ALREADY EXISTS                           
*ULST168  MVC   LASTCODE(L'TLKCPJC),TLKCPJC MOVE CPJ CODE TO LASTCODE           
*                                                                               
ULST172  XR    RE,RE                                                            
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE?           
         BNE   ULST174                                                          
         LA    RE,L'TLKTIMET                                                    
         SHI   RE,1                                                             
         MVC   0(0,R4),TLKTIMET                                                 
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         AR    R4,RE                                                            
         MVC   0(L'TLKWC,R4),TLKWC                                              
         LA    R4,L'TLKWC(R4)                                                   
ULST174  MVC   0(L'TLKCPJC,R4),TLKCPJC                                          
         AHI   R4,L'TLKCPJC                                                     
ULST176  LR    R5,R4                                                            
         LA    RF,T.LIDEL                                                       
         SR    R5,RF               TOTAL DISPLACEMENT OF ELEMENT                
         CHI   R5,240                                                           
         BL    ULST120                                                          
         STC   R5,T.LIDLN                                                       
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BE    ULST186                                                          
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
ULST186  XC    BOELEM,BOELEM                                                    
         MVI   T.LIDEL,LIDELQ                                                   
         CLI   GSSMPAGE,CPJLISTQ   ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   ULST188             NO                                           
         MVI   T.LIDTYPE,LIDTCLIT                                               
         MVI   T.LIDITLN,L'TLKCPJC+L'TLKTIMET+L'TLKWC                           
         B     ULST198                                                          
ULST188  CLI   GSSMPAGE,NCCLISTQ   ARE WE ON NON CLIENT ACCOUNT PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTNCLC                                               
         MVI   T.LIDITLN,L'TLKNCC                                               
ULST198  LA    R4,T.LIDDATA                                                     
         SR    R5,R5                                                            
         B     ULST120                                                          
*                                                                               
ULST200  LTR   R5,R5                                                            
         BZ    EXITOK                                                           
         STC   R5,T.LIDLN                                                       
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
         DROP  T,R2,R3                                                          
         EJECT ,                                                                
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
         B     IOCHK                                                            
         B     DELRECS                                                          
         B     ADDRECS                                                          
         B     VALCPJ                                                           
         B     VALWC                                                            
         B     VALNCL                                                           
         B     VALC1R                                                           
         B     VALTMT                                                           
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
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         SPACE 1                                                                
PZERO    DC    P'0'                                                             
PRODUL   DC    C'SJ'                                                            
WC       DC    C'WC'                                                            
CSTUL    DC    C'1R'                                                            
NCTUL    DC    C'1N'                                                            
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
DEF      EQU   C'D'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
BOTH     EQU   C'B'                                                             
EDITOR   EQU   C'E'                                                             
STMPSTRQ EQU   X'03'                                                            
CPJLISTQ EQU   1                                                                
NCCLISTQ EQU   2                                                                
CPJNMFLQ EQU   40                                                               
         EJECT ,                                                                
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
***********************************************************************         
* UPDATE PASSIVE TIME TEMPLATE RECORD                               *           
*                                                                     *         
* NTRY - P1  = TIME TEMPLATE RECORD                                   *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
ADDRECS  CLI   CSACT,A#RES         ACTION RESTORE                               
         BE    AREC01              YES - ADD ALL PASSIVE POINTERS               
         CLI   CSACT,A#ADD         ACTION ADD                                   
         BE    AREC01              YES - ADD ALL PASSIVE POINTERS               
         CLI   GSSMPAGE,1          ARE WE ON PID PAGE                           
         BNE   OVROU1E             NO                                           
AREC01   L     R3,0(R1)                                                         
         USING TTMRECD,R3                                                       
         LA    R2,IOKEY                                                         
         MVC   IOKEY,0(R3)                                                      
         L     R1,=AL4(XORDD+XOACCDIR+XIO4) READ DIR FOR DA                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TTEMDA,IOKEY+TTMKDA-TTMRECD                                      
*                                                                               
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'A',(R3)),CPTRBLK,TTEMDA,0,ACOM                 
         B     OVROU1E                                                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* DELETE TIME TEMPLATE PASSIVES                                       *         
*                                                                     *         
* NTRY - P1  = TIME TEMPLATE RECORD                                   *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
DELRECS  CLI   CSACT,A#DEL         ACTION DELETE                                
         BE    *+12                YES - DELETE ALL PASSIVE POINTERS            
         CLI   GSSMPAGE,1          ARE WE ON 1ST PAGE                           
         BNE   OVROU1E             NO                                           
         L     R3,0(R1)            GET RECORD ADDRESS                           
         USING TTMRECD,R3                                                       
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',CPTRBLK),0,0,ACOM               
*                                                                               
         B     OVROU1E                                                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* CHECK CLI/PRO/JOB CODE IS VALID AND RETURN NAME                     *         
*                                                                     *         
* NTRY - P1  = CLI/PRO/JOB CODE                                       *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
VALCPJ   L     R2,0(R1)            R2=CLI/PRO/JOB CODE                          
         LR    RF,R2                                                            
         SR    RE,RE                                                            
*                                                                               
         LHI   RE,L'ACTKACT                                                     
         LA    RF,L'ACTKACT-1(RF)  POINT AT LAST CHARACTER OF ACCOUNT           
VCPJ02   CLI   0(RF),C' '          IS IT A SPACE                                
         BH    VCPJ03                                                           
         BCTR  RF,0                                                             
         BCT   RE,VCPJ02                                                        
*                                                                               
*&&DO                                                                           
VCPJ02   CLI   0(RF),C' '                                                       
         BE    VCPJ03                                                           
         LA    RF,1(,RF)                                                        
         AHI   RE,1                                                             
         CHI   RE,L'ACTKACT                                                     
         BNE   VCPJ02                                                           
*&&                                                                             
*                                                                               
VCPJ03   STC   RE,MYBYTE           LENGTH OF CLI/PRO/JOB CODE                   
*&&UK                                                                           
         CLC   MYBYTE,CLILEN                                                    
         BE    VCPJ04                                                           
         CLC   MYBYTE,PROLEN                                                    
         BE    VCPJ04                                                           
         BL    OVROU1L             INVALID CLI/PRO/JOB LENGTH                   
*&&                                                                             
                                                                                
VCPJ04   MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'PRODUL),PRODUL                                       
*                                                                               
         MVI   TOTNAML,0                                                        
         MVC   BOWORK1(L'BOWORK1+L'BOWORK2),BCSPACES                            
         LA    R4,BOWORK1                                                       
*                                                                               
         IC    RE,CLILEN           READ CLIENT NAME                             
         SHI   RE,1                                                             
         EXMVC RE,T.ACTKACT,0(R2)  CLIENT CODE                                  
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VCPJ10                                                           
         L     RF,12(R1)                                                        
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EXMVC R1,0(R4),NAMEREC                                                 
         AHI   R1,1                                                             
         STC   R1,TOTNAML          LENGTH OF CLIENT NAME                        
         AR    R4,R1               BUMP TO THE END OF CLIENT NAME               
         DROP  RF                                                               
*                                                                               
VCPJ10   CLC   MYBYTE,CLILEN       TEST CLIENT CODE ONLY                        
         BNH   VCPJ30                                                           
*                                                                               
         MVC   IOKEY,IOKEYSAV      RESTORE IOKEY                                
         SR    RE,RE                                                            
         IC    RE,PROLEN                                                        
         SHI   RE,1                                                             
         EXMVC RE,T.ACTKACT,0(R2)  PRODUCT CODE                                 
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VCPJ20                                                           
         MVI   0(R4),C'/'                                                       
         L     RF,12(R1)                                                        
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EXMVC R1,1(R4),NAMEREC                                                 
         AHI   R1,2                                                             
         SR    RE,RE                                                            
         IC    RE,TOTNAML                                                       
         AR    RE,R1                                                            
         STC   RE,TOTNAML          LENGTH OF CLIENT/PRODUCT NAMES               
         AR    R4,R1               BUMP TO THE END OF PRODUCT NAME              
         DROP  RF                                                               
*                                                                               
VCPJ20   CLC   MYBYTE,PROLEN       TEST PRODUCT CODE ONLY                       
         BNH   VCPJ30                                                           
*                                                                               
         MVC   IOKEY,IOKEYSAV      RESTORE IOKEY                                
         MVC   T.ACTKACT,0(R2)     JOB CODE                                     
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VCPJ30                                                           
         MVI   0(R4),C'/'                                                       
         L     RF,12(R1)                                                        
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EXMVC R1,1(R4),NAMEREC                                                 
         SR    RE,RE                                                            
         IC    RE,TOTNAML                                                       
         LA    RE,2(R1,RE)                                                      
         STC   RE,TOTNAML          LENGTH OF CLI/PRO/JOB NAMES                  
         DROP  RF                                                               
*                                                                               
VCPJ30   MVC   CPJCODE,T.ACTKACT                                                
VCPJ32   LA    R4,BOWORK1                                                       
         SR    RE,RE                                                            
         ICM   RE,1,TOTNAML                                                     
         BZ    OVROU1E                                                          
         MVC   CPJNAME,0(R4)                                                    
         B     OVROU1E                                                          
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK WORKCODE CODE IS VALID AND RETURN NAME                        *         
*                                                                     *         
* NTRY - P1  = WORKCODE                                               *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALWC    L     R2,0(R1)                                                         
         XC    WCCODE,WCCODE                                                    
         LA    R5,IOKEY                                                         
         USING WCORECD,R5                                                       
         MVC   WCOKEY,BCSPACES   READ FOR WORKCODE RECORD                       
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN                                                   
         MVC   WCOKUNT(2),=C'SJ'                                                
         MVC   WCOKWRK(L'WCOKWRK),0(R2)                                         
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L             NOT FOUND - INVALID WORK CODE                
         MVC   WCCODE,WCOKWRK                                                   
         B     OVROU1E                                                          
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* CHECK NON CLIENT CODE IS VALID AND RETURN NAME                      *         
*                                                                     *         
* NTRY - P1  = NON CLIENT ACCOUNT CODE                                *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALNCL   L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES     READ NON CLIENT ACCOUNT RECORD               
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         MVC   ACTKUNT(L'BCCPYPRD),=C'1N'                                       
         SHI   RE,1                                                             
         MVC   ACTKACT(0),0(R2)                                                 
         EX    RE,*-6                                                           
         OI    ACTKACT,X'40'                                                    
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('NAMELQ',ACTRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NAMEL MISSING                                
         MVC   NCLNAME,BCSPACES                                                 
                                                                                
T        USING NAMELD,BOELEM                                                    
         MVC   NCLCODE,ACTKACT                                                  
         XR    RF,RF                                                            
         IC    RF,T.NAMLN                                                       
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     OVROU1E                                                          
         MVC   NCLNAME(0),T.NAMEREC                                             
         DROP  R5                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK COSTING 1R CODE IS VALID AND RETURN NAME                      *         
*                                                                     *         
* NTRY - P1  = 1R ACCOUNT CODE                                        *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALC1R   L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES     READ NON CLIENT ACCOUNT RECORD               
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         MVC   ACTKUNT(L'BCCPYPRD),=C'1R'                                       
         SHI   RE,1                                                             
         MVC   ACTKACT(0),0(R2)                                                 
         EX    RE,*-6                                                           
         OC    ACTKACT,BCSPACES                                                 
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('NAMELQ',ACTRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NAMEL MISSING                                
         MVC   C1RNAME,BCSPACES                                                 
                                                                                
T        USING NAMELD,BOELEM                                                    
         MVC   C1RCODE,ACTKACT                                                  
         XR    RF,RF                                                            
         IC    RF,T.NAMLN                                                       
         SH    RF,=Y(NAMLN1Q+1)                                                 
         EX    RF,*+8                                                           
         B     OVROU1E                                                          
         MVC   C1RNAME(0),T.NAMEREC                                             
         DROP  R5                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK TIME TYPE IS VALID AND RETURN NAME                            *         
*                                                                     *         
* NTRY - P1  = TIME TYPE                                              *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALTMT   L     R2,0(R1)            R2=TIMETYPE                                  
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
VTMT02   SHI   RE,1                                                             
         MVC   TIMETYPE(0),0(R2)                                                
         EX    RE,*-6                                                           
         OC    TIMETYPE,TIMETYPE                                                
         BZ    OVROU1L                                                          
         B     OVROU1E                                                          
         EJECT ,                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         EJECT ,                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
DUB      DS    D                                                                
DMCB     DS    6F                                                               
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
         SPACE 1                                                                
A1RAFLD  DS    A                   A(PERSON ID FIELD)                           
         SPACE 1                                                                
OVROUT1  DS    0A                                                               
AIOCHK   DS    A                                                                
ADELRECS DS    A                                                                
AADDRECS DS    A                                                                
AVALCPJ  DS    A                                                                
AVALWC   DS    A                                                                
AVALNCL  DS    A                                                                
AVALC1R  DS    A                                                                
AVALTMT  DS    A                                                                
OVROUT1N EQU   (*-OVROUT1)/L'OVROUT1                                            
         SPACE 1                                                                
WORK     DS    XL64                                                             
*                                                                               
MNTDISP  DS    H                   CURRENT DISPLACENT INTO ELEMENT              
CURDISP  DS    H                   CURRENT DISPLACENT INTO ELEMENT              
DATALEN  DS    H                   LENGTH OF DATA                               
TOTELLN  DS    H                   TOTAL LENGTH OF ELEMENT                      
*                                                                               
SESNL    DS    XL1                                                              
ANYLINES DS    CL1                                                              
SVIOKEY  DS    CL(L'IOKEY)                                                      
TTEMDA   DS    XL(L'TTMKDA)                                                     
*                                                                               
MYBYTE   DS    XL1                                                              
MAXITMS  EQU   80                                                               
CPTRWRK  DS    CL200                                                            
OVERWRKX EQU   *-OVERWRKD                                                       
         SPACE 1                                                                
***********************************************************************         
* SAVED DSECT                                                         *         
***********************************************************************         
         SPACE 2                                                                
SAVED    DSECT                                                                  
APPINDS  DS    XL1                                                              
APPIFCPJ EQU   X'80'               CLI/PRO/JOB CODE FOUND                       
APPIFNCC EQU   X'08'               NON-CLIENT CODE FOUND                        
APPIFRST EQU   X'01'               FIRST PASS                                   
*                                                                               
CLILEN   DS    XL1                 LENGTH OF CLIENT                             
PROLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT                     
JOBLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT+JOB                 
TOTNAML  DS    XL1                 LENGTH OF CLIENT NAME                        
*                                                                               
LENSA    DS    XL1                 SAVED LENGTH OF LEVEL 1                      
LENSB    DS    XL1                 SAVED LENGTH OF LEVEL 2                      
LENSC    DS    XL1                 SAVED LENGTH OF LEVEL 3                      
LENSD    DS    XL1                 SAVED LENGTH OF LEVEL 4                      
*                                                                               
SV1RAC   DS    CL(L'ACTKACT)       1R ACCOUNT                                   
SV1RNAM  DS    CL(L'NAMEREC)                                                    
*                                                                               
CPJNAME  DS    CL(CPJNMFLQ)        CLI/PRO/JOB NAME                             
CPJCODE  DS    CL(L'ACTKACT)       CLI/PRO/JOB CODE                             
WCCODE   DS    CL2                 WORKCODE                                     
NCLCODE  DS    CL(L'ACTKACT)       NON CLIENT ACCOUNT CODE                      
NCLNAME  DS    CL(L'NAMEREC)       NON CLIENT ACCOUNT NAME                      
C1RCODE  DS    CL(L'ACTKACT)       COSTING 1R ACCOUNT CODE                      
C1RNAME  DS    CL(L'NAMEREC)       COSTING 1R ACCOUNT NAME                      
TIMETYPE DS    CL1                 TIME TYPE                                    
*                                                                               
IOCOUNT  DS    H                   COUNT IO'S                                   
*                                                                               
SVFLTS   DS    0F                                                               
SV1RAFL  DS    CL(L'ACTKACT)       FILTER ON 1R ACCOUNT CODE                    
SVFCPJXL DS    XL1                 LENGTH OF INPUT CLI/PRO/JOB - 1              
SVFCPJ   DS    CL(L'TLKCPJC)       FILTER ON CLI/PRO/JOB                        
SVFNCC   DS    CL(L'TLKNCC)        FILTER ON NON-CLIENT CODE                    
SVFLTLQ  EQU   *-SVFLTS                                                         
*                                                                               
LASTCODE DS    CL14                PREVIOUS CPJ CODE                            
SKEYLAST DS    XL(L'ACTKEY)        SAVED CONTINUATION KEY FOR FLST              
ERRIND   DS    XL1                 ERROR INDICATOR                              
ERMAXIO  EQU   X'80'               MAX IOS RETURNED                             
         SPACE 1                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKELEM  DS    0CL14                                                            
TLK1RACC DS    CL(L'ACTKACT)       1R ACCOUNT CODE                              
         DS    XL2                                                              
         ORG   TLKSRT                                                           
TLKCPJC  DS    CL12                CLI/PRO/JOB CODE                             
         DS    XL2                                                              
         ORG   TLKSRT                                                           
TLKNCC   DS    CL12                NON-CLIENT CODE                              
         DS    XL2                                                              
         ORG   TLUSER                                                           
TLKCPJNM DS    CL(CPJNMFLQ)        CLI/PRO/JOB NAME                             
TLKWC    DS    CL2                 WORKCODE                                     
TLKTIMET DS    CL1                 TIME TYPE                                    
         ORG   TLKCPJNM                                                         
TLKNCLNM DS    CL(L'NAMEREC)       NON CLIENT ACCOUNT NAME                      
         ORG   TLKNCLNM                                                         
TLKC1RNM DS    CL(L'NAMEREC)       COSTING 1R NAME                              
         DS    XL80                                                             
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACFIL55   08/10/11'                                      
         END                                                                    
