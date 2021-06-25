*          DATA SET ACFIL18    AT LEVEL 014 AS OF 08/10/11                      
*PHASE T62318C,*                                                                
         SPACE 1                                                                
FIL18    TITLE 'FILTER VALUE RECORD'                                            
         SPACE 2                                                                
*YNGX 003 13APR05 BUG FIX - CURSOR IS SET TO SERVICE FIELD AFTER                
*                 DELETING/RESTORING RECORD USING SUBACTION ON LIST SCR         
         SPACE 2                                                                
FIL18    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL18**,R6,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
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
         SPACE 2                                                                
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
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
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
         USING RSFRECD,R2                                                       
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
KFKVAL   MVC   RSFKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVI   RSFKTYP,RSFKTYPQ    FILTER TYPE                                  
         MVC   RSFKCPY,CUABIN      CONNECTED ID                                 
         MVI   RSFKFLT#,0                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   RSFKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVI   RSFKTYP,RSFKTYPQ    FILTER TYPE                                  
         MVC   RSFKCPY,CUABIN      CONNECTED ID                                 
         MVI   RSFKUNT,X'41'                                                    
         MVI   RSFKLDG,X'41'       GO PAST UNIT AND LEDGER RECORDS              
         MVI   RSFKFLT#,0                                                       
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
* RECORD OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R0,R3,SVPARMS                                                    
         USING RSFRECD,R2                                                       
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
RFADD    GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',RSFRECD),0               
         CLI   12(R1),0            RSTEL ON RECORD?                             
         BE    RFADD02                                                          
         GOTO1 AADDRST,RSFRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL               SOMETHING WRONG                              
         B     EXITOK                                                           
*                                                                               
RFADD02  L     RF,12(,R1)                                                       
         USING RSTELD,RF                                                        
         MVC   RSTBDATE,BCTODAYP   SET TO TODAY                                 
         MVC   RSTTDATE,BCTODAYP                                                
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA RSFION         *         
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                  *         
* P3 BYTES 1-3 HOLDS EQUATED RSFION VERB                              *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
*                                                                     *         
* FIELD DATA IS EXTRRSFED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              RSFION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING RSFRECD,R2                                                       
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
         USING RSFRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(F#FVAL#UNTLD),AL4(LGRDTA) UNIT/LEDGER                        
         DC    AL2(F#FVAL#LDGNM),AL4(LGNDTA) LEDGER NAME                        
         DC    AL2(F#FVAL#FLTNM),AL4(FNNDTA) FILTER NAME                        
         DC    AL2(F#FVAL#FLTND),AL4(FNFDTA) FILTER NAME DISPLAY                
         DC    AL2(F#FVAL#FLTVA),AL4(VL1DTA) FILTER VALUE                       
         DC    AL2(F#FVAL#FLTC),AL4(CD1DTA)  FILTER CODE                        
         DC    AL2(F#FVAL#FLTLN),AL4(LN1DTA) FILTER LONG NAME                   
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL18    CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR UNIT/LEDGER                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LGRDTA   LA    RF,LGRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LGRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLGR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLGR)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISLGR)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETLGR)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLGR)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTLGR)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTLGR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETLGR  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A UNIT/LEDGER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISLGR   MVC   FVIFLD(L'RSFKLDG+L'RSFKUNT),RSFKUNT                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A UNIT/LEDGER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALLGR   MVC   RSFKUNT(L'RSFKUNT+L'RSFKLDG),FVIFLD                              
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ THE LEDGER RECORD FOR THIS ID           
         MVC   T.ACTKCPY,RSFKCPY   COMPANY ID                                   
         MVC   T.ACTKUNT,RSFKUNT   UNIT                                         
         MVC   T.ACTKLDG,RSFKLDG   LEDGER                                       
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$IVUNL)                                           
         B     EXITL               CAN'T READ ACCDIR FOR U/L                    
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A UNIT/LEDGER FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTLGR  MVC   FVIFLD(L'RSFKLDG+L'RSFKUNT),FLTIFLD                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A UNIT/LEDGER FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTLGR  MVC   RSFKUNT(L'RSFKUNT+L'RSFKLDG),FVIFLD                              
         MVC   FLTIFLD(L'RSFKUNT+L'RSFKLDG),FVIFLD                              
*                                                                               
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ THE LEDGER RECORD FOR THIS ID           
         MVC   T.ACTKCPY,RSFKCPY   COMPANY ID IN HERE                           
         MVC   T.ACTKUNT,RSFKUNT   UNIT HERE                                    
         MVC   T.ACTKLDG,RSFKLDG   LEDGER                                       
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$IVUNL)                                           
         B     EXITL               CAN'T READ ACCDIR FOR U/L                    
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR UNIT/LEDGER                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTLGR  CLC   RSFKUNT(L'RSFKUNT+L'RSFKLDG),BCSPACES                            
         BNH   FLTXX               NO U/L - WE DON`T WANT IT                    
*                                                                               
         CLC   RSFKUNT(L'RSFKUNT+L'RSFKLDG),FLTIFLD                             
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A LEDGER NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LGNDTA   LA    RF,LGNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LGNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLGN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISLGN   MVC   T.ACTKEY,BCSPACES   READ THE LEDGER RECORD FOR THIS ID           
         MVC   T.ACTKCPY,RSFKCPY   COMPANY ID                                   
         MVC   T.ACTKUNT,RSFKUNT   UNIT                                         
         MVC   T.ACTKLDG,RSFKLDG   LEDGER                                       
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER NAME                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FNNDTA   LA    RF,FNNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FNNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFNN)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISFNN)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETFNN)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFNN)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFNN)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFNN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFNN  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
T        USING RSFRECD,IOKEY                                                    
DISFNN   MVC   FVIFLD(L'RSFKFLT#),RSFKFLT#                                      
         OI    FVIFLD,X'F0'        DEFAULT IS RSFKFLT#                          
         MVC   XFLTNAM,BCSPACES                                                 
*                                                                               
         MVC   T.RSFKEY,BCSPACES                                                
         MVC   T.RSFKEY,RSFKEY     FILTER RECORD HERE                           
         MVI   T.RSFKFLT#,X'00'    FOR THE NAME RECORD                          
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2  READ THE NAME RECORD                    
         GOTO1 AIO                                                              
         BNE   EXITOK              CAN'T READ ACCDIR FOR RECORD                 
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSFELQ',AIO2),          X        
               (1,RSFKFLT#)                                                     
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R4,12(,R1)                                                       
         USING RSFELD,R4                                                        
         MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
*                                                                               
         CLI   RSFLN,RSFLNQ        CODE HAS A LONG NAME?                        
         BNH   EXITOK              NO                                           
         XR    RF,RF                                                            
         IC    RF,RSFLN                                                         
         SHI   RF,RSFLNQ+1                                                      
         CHI   RF,L'XFLTNAM                                                     
         BL    *+8                                                              
         LA    RF,L'XFLTNAM-1                                                   
         MVC   XFLTNAM(0),RSFLNAM  MOVE IN THE LONG NAME FOR LATER              
         EX    RF,*-6                                                           
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER NAME FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING RSFRECD,IOKEY                                                    
VALFNN   MVC   T.RSFKEY,RSFKEY     TRY TO FIND FILTER NAME RECORD               
         MVI   T.RSFKFLT#,0        SET TO 0 FOR NAME RECORD                     
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NOFNM)  FILTER NAME RECORD NOT SET UP            
         B     EXITL                                                            
         SPACE 1                                                                
         CLI   FVILEN,1            IF LENGTH IS 1 AND FIELD IS NUMERIC          
         BNE   VFNN02              TRY TO FIND A MATCHING CODE                  
         TM    FVIIND,FVINUM                                                    
         BZ    VFNN02                                                           
         SPACE 1                                                                
         MVN   RSFKFLT#,FVIFLD     TURN OFF ZONE BITS & PUT INTO KEY            
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSFELQ',AIO2),          X        
               (1,RSFKFLT#)                                                     
         CLI   12(R1),0                                                         
         BE    EXITOK              THIS NUMBER DOES EXIST AND IS VALID          
*                                                                               
         USING RSFELD,RF                                                        
VFNN02   L     RF,AIO2                                                          
         LA    RF,RSFRFST-RSFRECD(,RF)                                          
         XR    RE,RE                                                            
VFNN04   CLI   RSFEL,0                                                          
         BE    EXITNV              END OF RECORD - NOT VALID                    
         CLI   RSFEL,RSFELQ                                                     
         BNE   VFNN06              NOT A FILTER ELEMENT                         
         SPACE 1                                                                
         IC    RE,FVXLEN           COMPARE INPUT AGAINST CODE                   
         EX    RE,*+8                                                           
         BNE   VFNN06              IT DOESN`T MATCH - TRY NEXT                  
         CLC   FVIFLD(0),RSFCODE                                                
         MVC   RSFKFLT#,RSFTYPE    GET THE RSTEL TYPE NUMBER                    
         B     EXITOK                                                           
         SPACE 1                                                                
VFNN06   IC    RE,RSFLN                                                         
         AR    RF,RE                                                            
         B     VFNN04                                                           
         DROP  RF,T                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER NAME FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTFNN  MVC   FVIFLD(L'RSFCODE),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER NAME FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTFNN  MVC   FLTIFLD(L'RSFCODE),FVIFLD                                        
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER NAME FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
T        USING RSFRECD,IOKEY                                                    
DOFTFNN  MVC   BOWORK1,IOKEY       SAVE IOKEY                                   
         MVC   T.RSFKEY,RSFKEY     FILTER RECORD HERE                           
         MVI   T.RSFKFLT#,X'00'    FOR THE NAME RECORD                          
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2  READ THE NAME RECORD                    
         GOTO1 AIO                                                              
         BNE   FLTXE               DISP. RECORD IF NAME REC NOT EXIST           
*                                                                               
         MVC   IOKEY,BOWORK1       RESTORE IOKEY                                
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSFELQ',AIO2),          C        
               (1,RSFKFLT#)                                                     
         CLI   12(R1),0                                                         
         BNE   FLTXE               DISPLAY RECORD IF NO RSFEL                   
                                                                                
         L     RF,12(,R1)                                                       
         USING RSFELD,RF                                                        
         CLC   RSFCODE,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  RF                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FNFDTA   LA    RF,FNFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FNFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNF)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DISFNF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
DISFNF   MVC   FVIFLD(L'XFLTNAM),XFLTNAM                                        
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER VALUE FILTER VALUE                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
VL1DTA   LA    RF,VL1TBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
VL1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISVL1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVL1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER VALUE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISVL1   MVC   FVIFLD(L'TLKVAL),TLKVAL                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER VALUE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALVL1   XC    TLKVAL,TLKVAL                                                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   TELL LIST TO DELETE THIS ENTRY               
         B     EXITOK                                                           
*                                                                               
         GOTO1 ACHKFLD,BOPARM,('CKDATAQ',CKTAB1Q)   (0-9, A-Z)?                 
         BNE   EXITNV              INVALID CHARS IN THE FIELD                   
         MVC   TLRLEN,=AL2(TLLNQ)                                               
         MVC   TLKVAL,FVIFLD                                                    
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER VALUE CODE                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CD1DTA   LA    RF,CD1TBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
CD1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCD1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCD1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER VALUE CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISCD1   MVC   FVIFLD(L'TLKCODE),TLKCODE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER VALUE CODE FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VALCD1   OC    TLKVAL,TLKVAL                                                    
         BZ    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO INPUT                             
         MVC   TLKCODE,FVIFLD                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER VALUE LONG NAME                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
LN1DTA   LA    RF,LN1TBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
LN1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER VALUE LONG NAME                                      *         
***********************************************************************         
         SPACE 1                                                                
DISLN1   MVC   FVIFLD(L'TLKLNAM),TLKLNAM                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER VALUE LONG NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
VALLN1   OC    TLKVAL,TLKVAL                                                    
         BZ    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO INPUT                             
         MVC   TLKLNMLN,FVILEN     LENGTH OF LONG NAME                          
         MVC   TLKLNAM,FVIFLD                                                   
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
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   EXITOK                                                           
         CLI   SREC,R#FVAL         FILTER VALUE RECORD                          
         BE    *+8                                                              
         CLI   SREC,R#FNAM         FILTER NAME RECORD                           
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
         USING RSFRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING RSFRECD,R2                                                       
LAST     USING RSFRECD,R3                                                       
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
LISTABL  DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
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
X        USING RSFRECD,IOKEY                                                    
FLST     MVC   X.RSFKEY,THIS.RSFKEY                                             
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
NLST02   CLC   X.RSFKEY(RSFKUNT-RSFRECD),THIS.RSFKEY                            
         BNE   EXITL               DIFFERENT COMPANY - IGNORE IT                
         CLI   X.RSFKFLT#,0                                                     
         BE    NLST                IF '0', IT IS A FILTER NAME RECORD           
         CLC   X.RSFKACT,BCSPACES                                               
         BNE   NLST                ACCOUNT LEVEL NOT YET SUPPORTED              
*                                                                               
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
NLST06   MVC   THIS.RSFKEY(ACCKLEN),IOKEY                                       
         B     EXITOK                                                           
         DROP  LAST,X                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         OI    LSSTAT2,LSSNOSEQ+LSSADD                                          
         MVC   LSCOLLIN,=AL2(240)                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,RSFRFST-RSFRECD                                               
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
         LA    RF,RSFRFST-RSFRECD(,RF) IT IS NOW.                               
         XR    RE,RE                                                            
*                                                                               
         USING RSFELD,RF                                                        
FML02    CLI   RSFEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   RSFEL,RSFELQ        RSFEL?                                       
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
         LA    RF,RSFRFST-RSFRECD(,RF) IT IS NOW.                               
*                                                                               
         USING RSFELD,RF                                                        
NML02    CLI   RSFEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   RSFEL,RSFELQ        RSFEL?                                       
         BE    NML06               YES                                          
                                                                                
NML04    IC    RE,RSFLN                                                         
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
         USING RSFELD,RF           MOVE IN DETAILS FROM ELEMENT                 
         MVC   TLKVAL,RSFTYPE                                                   
         MVC   TLKCODE,RSFCODE                                                  
         MVC   TLKLNAM,BCSPACES                                                 
         MVI   TLKLNMLN,0                                                       
         CLI   RSFLN,RSFLNQ                                                     
         BNH   EXITOK                                                           
         SR    RE,RE                                                            
         IC    RE,RSFLN                                                         
         SHI   RE,RSFLNQ                                                        
         CHI   RE,L'RSFLNAM                                                     
         BNH   *+8                                                              
         LHI   RE,L'RSFLNAM        USE MAX. LENGTH                              
         STC   RE,TLKLNMLN                                                      
         BCTR  RE,0                                                             
         EXMVC RE,TLKLNAM,RSFLNAM                                               
         B     EXITOK                                                           
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 MVI   ANYLINES,NO                                                      
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('RSFELQ',AIOREC),0                
         B     EXITOK              DELETE ALL OLD RSFELS                        
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RSFRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
*                                                                               
         MVI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         LM    R2,R3,SVPARMS3                                                   
         USING RSFELD,BOELEM                                                    
         XC    RSFELD(RSFLNQ),RSFELD                                            
         MVI   RSFEL,RSFELQ                                                     
         MVI   RSFLN,RSFLNQ                                                     
         MVC   RSFTYPE,TLKVAL                                                   
         MVC   RSFCODE,TLKCODE                                                  
         SR    RE,RE                                                            
         IC    RE,TLKLNMLN         LENGTH OF LONG NAME                          
         BCTR  RE,0                                                             
         EXMVC RE,RSFLNAM,TLKLNAM                                               
         LA    RE,RSFLNQ+1(,RE)                                                 
         STC   RE,RSFLN                                                         
*                                                                               
UREC102  GOTO1 AADDEL,BOPARM,AIOREC                                             
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
         DROP  R2,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 CLI   CSACT,A#CHA                                                      
         BE    *+12                                                             
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
         CLI   ANYLINES,YES        EMPTY LIST?                                  
         BE    EXITOK              NO - OK                                      
         MVC   FVMSGNO,=AL2(AE$NLINE)                                           
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
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
FLTNO    EQU   5                                                                
         SPACE 2                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
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
ANYLINES DS    CL1                                                              
XNUM     DS    XL1                                                              
XFLTNAM  DS    CL12                                                             
*                                                                               
MNTDISP  DS    H - MOVE TO SAVED STORAGE                                        
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT ,                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKVAL   DS    CL(L'RSFTYPE)       CODE                                         
         ORG   TLUSER                                                           
TLKCODE  DS    CL(L'RSFCODE)       SHORT NAME                                   
TLKLNMLN DS    XL1                 LENGTH OF LONG NAME                          
TLKLNAM  DS    CL(L'RSFLNAM)       LONG NAME                                    
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014ACFIL18   08/10/11'                                      
         END                                                                    
