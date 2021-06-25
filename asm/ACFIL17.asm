*          DATA SET ACFIL17    AT LEVEL 010 AS OF 06/05/08                      
*PHASE T62317A,*                                                                
         SPACE 1                                                                
FIL17    TITLE 'FILTER NAME RECORD'                                             
         SPACE 2                                                                
FIL17    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL17**,R6,RR=RE                                              
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
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
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
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
T        USING RSFRECD,IOKEY                                                    
RFDEL    MVC   T.RSFKEY,RSFKEY                                                  
         XR    R0,R0                                                            
         IC    R0,T.RSFKFLT#       READ NEXT UNDELETED RECORD                   
         AHI   R0,1                                                             
         STC   R0,T.RSFKFLT#                                                    
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   T.RSFKEY(RSFKFLT#-RSFRECD),RSFKEY                                
         BNE   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$DFNAM)                                           
         LH    R0,LS1STKEY                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR      SET CURSOR TO KEY FIELD                      
         B     EXITL               CAN'T DELETE - FVALUE RECORDS EXIST          
         DROP  T                                                                
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
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(F#FNAM#UNTLD),AL4(LGRDTA) UNIT/LEDGER                        
         DC    AL2(F#FNAM#LDGNM),AL4(LGNDTA) LEDGER NAME                        
         DC    AL2(F#FNAM#FLNM1),AL4(LN1DTA) FILTER 1 LONG NAME                 
         DC    AL2(F#FNAM#FLT1),AL4(CD1DTA)  FILTER 1 CODE                      
         DC    AL2(F#FNAM#FLNM2),AL4(LN2DTA) FILTER 2 LONG NAME                 
         DC    AL2(F#FNAM#FLT2),AL4(CD2DTA)  FILTER 2 CODE                      
         DC    AL2(F#FNAM#FLNM3),AL4(LN3DTA) FILTER 3 LONG NAME                 
         DC    AL2(F#FNAM#FLT3),AL4(CD3DTA)  FILTER 3 CODE                      
         DC    AL2(F#FNAM#FLNM4),AL4(LN4DTA) FILTER 4 LONG NAME                 
         DC    AL2(F#FNAM#FLT4),AL4(CD4DTA)  FILTER 4 CODE                      
         DC    AL2(F#FNAM#FLNM5),AL4(LN5DTA) FILTER 5 LONG NAME                 
         DC    AL2(F#FNAM#FLT5),AL4(CD5DTA)  FILTER 5 CODE                      
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL17    CSECT                                                                  
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
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDDIS)     DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFDVAL)     VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   MVI   XNUM,1                                                           
         LA    R0,FLTNO            FIVE FILTERS                                 
         LA    R3,RSELS                                                         
SAVE     USING RSFELD,R3                                                        
*                                                                               
DFD02    XC    0(L'RSEL1,R3),0(R3) CLEAR NEXT BLOCK OF STORAGE                  
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSFELQ',RSFRECD),       *        
               (1,XNUM)            GET NEXT RSFEL                               
         CLI   12(R1),0                                                         
         BNE   DFD04               ELEMENT DOES NOT EXIST                       
*                                                                               
         L     R4,12(,R1)          SAVE ELEMENT INTO RSFEL BLOCK                
         USING RSFELD,R4                                                        
         XR    RF,RF                                                            
         IC    RF,RSFLN                                                         
         MVC   SAVE.RSFELD(0),RSFELD                                            
         EX    RF,*-6                                                           
*                                                                               
DFD04    XR    RE,RE                                                            
         IC    RE,XNUM             NEXT ELEMENT NUMBER                          
         LA    RE,1(,RE)                                                        
         STC   RE,XNUM                                                          
         LA    R3,L'RSEL1(,R3)     NEXT SAVE AREA                               
         BCT   R0,DFD02                                                         
         DROP  R4,SAVE                                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   MVI   XNUM,1                                                           
         LA    R0,FLTNO            FIVE FILTERS                                 
         LA    R3,RSELS                                                         
SAVE     USING RSFELD,R3                                                        
*                                                                               
DFV02    XC    0(L'RSEL1,R3),0(R3) CLEAR NEXT BLOCK OF STORAGE                  
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSFELQ',RSFRECD),       *        
               (1,XNUM)                                                         
         CLI   12(R1),0                                                         
         BNE   DFV04               ELEMENT DOES NOT EXIST                       
*                                                                               
         L     R4,12(,R1)          SAVE ELEMENT                                 
         USING RSFELD,R4                                                        
         XR    RF,RF                                                            
         IC    RF,RSFLN                                                         
         MVC   SAVE.RSFELD(0),RSFELD                                            
         EX    RF,*-6                                                           
*                                                                               
DFV04    XR    RE,RE                                                            
         IC    RE,XNUM             NEXT ELEMENT NUMBER                          
         LA    RE,1(,RE)                                                        
         STC   RE,XNUM                                                          
         LA    R3,L'RSEL1(,R3)     NEXT SAVE AREA                               
         BCT   R0,DFV02                                                         
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('RSFELQ',RSFRECD),0               
         B     EXITOK              DELETE ALL THE RSFELS                        
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
DLDVAL   LA    R0,FLTNO                                                         
         LA    R3,RSELS                                                         
         MVI   BOFLAG1,0           RESET FLAG                                   
SAVE     USING RSFELD,R3                                                        
*                                                                               
DLV02    CLI   SAVE.RSFLN,0        NO INPUT - GET NEXT                          
         BE    DLV04                                                            
         MVI   BOFLAG1,FF          WE HAVE AT LEAST ONE ELEMENT                 
         MVI   SAVE.RSFEL,RSFELQ   ELEMENT CODE                                 
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),RSFRECD,SAVE.RSFELD,0              
         CLI   12(R1),0                                                         
         BE    *+14                ERROR ON PUT IN HELLO                        
         MVC   FVMSGNO,=AL2(AE$RECTB)                                           
         B     EXITL                                                            
*                                                                               
DLV04    LA    R3,L'RSEL1(,R3)     NEXT SAVE AREA                               
         BCT   R0,DLV02                                                         
         CLI   BOFLAG1,FF          ANY RSFEL                                    
         BE    EXITOK              YES - OK                                     
         MVC   FVADDR,AFSTFLD      SET CURSOR TO FIRST DATA FIELD               
         MVC   FVMSGNO,=AL2(AE$NLINE)  ERROR - NO RSFEL                         
         B     EXITL                                                            
         DROP  SAVE                                                             
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
* DATA OBJECT FOR FILTER 1 LONG NAME                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL1                                                     
LN1DTA   LA    RF,LN1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LN1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 1 LONG NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
DISLN1   CLI   RSFLN,RSFLNQ        DO WE HAVE A LONG NAME?                      
         BNH   EXITOK                                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSFLN                                                         
         SHI   RF,RSFLNQ+1         LONG NAME LENGTH FOR EXECUTED MOVE           
         MVC   FVIFLD(0),RSFLNAM                                                
         EX    RF,*-6                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 1 LONG NAME                                         *         
***********************************************************************         
         SPACE 1                                                                
VALLN1   MVC   AFSTFLD,FVADDR      SAVE A(FIELD)                                
         MVC   ALNMFLD,FVADDR                                                   
         MVI   RSFLN,0                                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         MVC   RSFLNAM(0),FVIFLD                                                
         EX    RF,*-6                                                           
         LA    RF,RSFLNQ+1(,RF)    MODIFY THE ELEMENT LENGTH                    
         STC   RF,RSFLN                                                         
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER CODE 1                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL1                                                     
CD1DTA   LA    RF,CD1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CD1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCD1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCD1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 1 CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCD1   MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 1 CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCD1   CLI   RSFLN,0             DO WE HAVE A LONG NAME?                      
         BNE   VCD102                                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   FVADDR,ALNMFLD      POINT TO LONG NAME FIELD                     
         B     EXITNO              NO LONG NAME                                 
                                                                                
VCD102   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         GOTO1 ACHKFLD,BOPARM,('CKDATAQ',CKTAB1Q)   (0-9, A-Z)?                 
         BNE   EXITNV              INVALID CHARS IN THE FIELD                   
         MVI   RSFTYPE,1                                                        
         MVC   RSFCODE,FVIFLD                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER 2 LONG NAME                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL2                                                     
LN2DTA   LA    RF,LN2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LN2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 2 LONG NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
DISLN2   CLI   RSFLN,RSFLNQ        DO WE HAVE A LONG NAME?                      
         BNH   EXITOK                                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSFLN                                                         
         SHI   RF,RSFLNQ+1         LONG NAME LENGTH FOR EXECUTED MOVE           
         MVC   FVIFLD(0),RSFLNAM                                                
         EX    RF,*-6                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 2 LONG NAME                                         *         
***********************************************************************         
         SPACE 1                                                                
VALLN2   MVC   ALNMFLD,FVADDR                                                   
         MVI   RSFLN,0                                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         MVC   RSFLNAM(0),FVIFLD                                                
         EX    RF,*-6                                                           
         LA    RF,RSFLNQ+1(,RF)    MODIFY THE ELEMENT LENGTH                    
         STC   RF,RSFLN                                                         
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER CODE 2                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL2                                                     
CD2DTA   LA    RF,CD2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CD2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCD2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCD2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 2 CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCD2   MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 2 CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCD2   CLI   RSFLN,0             DO WE HAVE A LONG NAME?                      
         BNE   VCD202                                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   FVADDR,ALNMFLD      POINT TO LONG NAME FIELD                     
         B     EXITNO              NO LONG NAME                                 
                                                                                
VCD202   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         GOTO1 ACHKFLD,1           CHECK INVALID CHARS (0-9, A-Z)               
         BNE   EXITNV                                                           
         MVC   SVFCODE,FVIFLD                                                   
         MVI   SVFLTNO,1           NO OF FILTER CODES TO BE CHECKED             
         BAS   RE,CHKFCOD          CHECK NO DUPLICATE FILTER CODE               
         BNE   EXITL                                                            
         MVI   RSFTYPE,2                                                        
         MVC   RSFCODE,FVIFLD                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER 3 LONG NAME                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL3                                                     
LN3DTA   LA    RF,LN3TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LN3TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN3)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN3)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 3 LONG NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
DISLN3   CLI   RSFLN,RSFLNQ        DO WE HAVE A LONG NAME?                      
         BNH   EXITOK                                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSFLN                                                         
         SHI   RF,RSFLNQ+1         LONG NAME LENGTH FOR EXECUTED MOVE           
         MVC   FVIFLD(0),RSFLNAM                                                
         EX    RF,*-6                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 3 LONG NAME                                         *         
***********************************************************************         
         SPACE 1                                                                
VALLN3   MVC   ALNMFLD,FVADDR                                                   
         MVI   RSFLN,0                                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         MVC   RSFLNAM(0),FVIFLD                                                
         EX    RF,*-6                                                           
         LA    RF,RSFLNQ+1(,RF)    MODIFY THE ELEMENT LENGTH                    
         STC   RF,RSFLN                                                         
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER CODE 3                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL3                                                     
CD3DTA   LA    RF,CD3TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CD3TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCD3)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCD3)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 3 CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCD3   MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 3 CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCD3   CLI   RSFLN,0             DO WE HAVE A LONG NAME?                      
         BNE   VCD302                                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   FVADDR,ALNMFLD      POINT TO LONG NAME FIELD                     
         B     EXITNO              NO LONG NAME                                 
                                                                                
VCD302   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         GOTO1 ACHKFLD,1           CHECK INVALID CHARS (0-9, A-Z)               
         BNE   EXITNV                                                           
         MVC   SVFCODE,FVIFLD                                                   
         MVI   SVFLTNO,2           NO OF FILTER CODES TO BE CHECKED             
         BAS   RE,CHKFCOD          CHECK NO DUPLICATE FILTER CODE               
         BNE   EXITL                                                            
         MVI   RSFTYPE,3                                                        
         MVC   RSFCODE,FVIFLD                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER 4 LONG NAME                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL4                                                     
LN4DTA   LA    RF,LN4TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LN4TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN4)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN4)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 4 LONG NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
DISLN4   CLI   RSFLN,RSFLNQ        DO WE HAVE A LONG NAME?                      
         BNH   EXITOK                                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSFLN                                                         
         SHI   RF,RSFLNQ+1         LONG NAME LENGTH FOR EXECUTED MOVE           
         MVC   FVIFLD(0),RSFLNAM                                                
         EX    RF,*-6                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 4 LONG NAME                                         *         
***********************************************************************         
         SPACE 1                                                                
VALLN4   MVC   ALNMFLD,FVADDR                                                   
         MVI   RSFLN,0                                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         MVC   RSFLNAM(0),FVIFLD                                                
         EX    RF,*-6                                                           
         LA    RF,RSFLNQ+1(,RF)    MODIFY THE ELEMENT LENGTH                    
         STC   RF,RSFLN                                                         
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER CODE 4                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL4                                                     
CD4DTA   LA    RF,CD4TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CD4TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCD4)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCD4)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 4 CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCD4   MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 4 CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCD4   CLI   RSFLN,0             DO WE HAVE A LONG NAME?                      
         BNE   VCD402                                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   FVADDR,ALNMFLD      POINT TO LONG NAME FIELD                     
         B     EXITNO              NO LONG NAME                                 
                                                                                
VCD402   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         GOTO1 ACHKFLD,1           CHECK INVALID CHARS (0-9, A-Z)               
         BNE   EXITNV                                                           
         MVC   SVFCODE,FVIFLD                                                   
         MVI   SVFLTNO,3           NO OF FILTER CODES TO BE CHECKED             
         BAS   RE,CHKFCOD          CHECK NO DUPLICATE FILTER CODE               
         BNE   EXITL                                                            
         MVI   RSFTYPE,4                                                        
         MVC   RSFCODE,FVIFLD                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER 5 LONG NAME                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL5                                                     
LN5DTA   LA    RF,LN5TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LN5TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN5)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN5)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 5 LONG NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
DISLN5   CLI   RSFLN,RSFLNQ        DO WE HAVE A LONG NAME?                      
         BNH   EXITOK                                                           
*                                                                               
         SR    RF,RF                                                            
         IC    RF,RSFLN                                                         
         SHI   RF,RSFLNQ+1         LONG NAME LENGTH FOR EXECUTED MOVE           
         MVC   FVIFLD(0),RSFLNAM                                                
         EX    RF,*-6                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 5 LONG NAME                                         *         
***********************************************************************         
         SPACE 1                                                                
VALLN5   MVC   ALNMFLD,FVADDR                                                   
         MVI   RSFLN,0                                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         MVC   RSFLNAM(0),FVIFLD                                                
         EX    RF,*-6                                                           
         LA    RF,RSFLNQ+1(,RF)    MODIFY THE ELEMENT LENGTH                    
         STC   RF,RSFLN                                                         
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILTER CODE 5                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSFELD,RSEL5                                                     
CD5DTA   LA    RF,CD5TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CD5TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCD5)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCD5)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILTER 5 CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCD5   MVC   FVIFLD(L'RSFCODE),RSFCODE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER 5 CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCD5   CLI   RSFLN,0             DO WE HAVE A LONG NAME?                      
         BNE   VCD502                                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   FVADDR,ALNMFLD      POINT TO LONG NAME FIELD                     
         B     EXITNO              NO LONG NAME                                 
                                                                                
VCD502   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         GOTO1 ACHKFLD,1           CHECK INVALID CHARS (0-9, A-Z)               
         BNE   EXITNV                                                           
         MVC   SVFCODE,FVIFLD                                                   
         MVI   SVFLTNO,4           NO OF FILTER CODES TO BE CHECKED             
         BAS   RE,CHKFCOD          CHECK NO DUPLICATE FILTER CODE               
         BNE   EXITL                                                            
         MVI   RSFTYPE,5                                                        
         MVC   RSFCODE,FVIFLD                                                   
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
         CLI   SREC,R#FNAM         FILTER NAME RECORD                           
         BE    *+8                                                              
         CLI   SREC,R#FVAL         FILTER VALUE RECORD                          
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
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
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING RSFRECD,R2                                                       
LAST     USING RSFRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INIT LIST                                                           *         
***********************************************************************         
         SPACE 1                                                                
ILST     NI    LSSTAT1,FF-LSSENTK  DON'T ASK 'ENTER KEY'                        
         B     EXITOK                                                           
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
         BNE   NLST                MUST BE 0 FOR FILTER NAME                    
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
         DROP  THIS,LAST,X                                                      
         EJECT ,                                                                
***********************************************************************         
* CHECK NO DUPLICATE FILTER CODES                                     *         
* NTRY - SVFCODE = RSFCODE                                            *         
*      - SVFLTNO = NO OF FILTERS                                      *         
***********************************************************************         
         SPACE 1                                                                
CHKFCOD  NTR1  ,                                                                
         XR    R0,R0                                                            
         IC    R0,SVFLTNO          NO OF FILTERS TO BE CHECKED                  
         LA    R3,RSELS                                                         
T        USING RSFELD,R3                                                        
*                                                                               
CHKFC02  CLC   SVFCODE,T.RSFCODE                                                
         BNE   CHKFC04                                                          
         MVC   FVMSGNO,=AL2(AE$DUPEN)                                           
         MVC   FVXTRA(L'SVFCODE),SVFCODE                                        
         B     EXITL                                                            
*                                                                               
CHKFC04  LA    R3,L'RSEL1(,R3)     NEXT SAVE AREA                               
         BCT   R0,CHKFC02                                                       
         B     EXITOK                                                           
         DROP  R3                                                               
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
AFSTFLD  DS    A                   A(FIRST FIELD)                               
ALNMFLD  DS    A                   A(LONG NAME FIELD)                           
*                                                                               
SVFCODE  DS    CL(L'RSFCODE)                                                    
SVFLTNO  DS    XL1                                                              
XNUM     DS    XL1                                                              
RSELS    DS    0X                                                               
RSEL1    DS    XL(L'RSFLNAM+RSFLNQ)                                             
RSEL2    DS    XL(L'RSFLNAM+RSFLNQ)                                             
RSEL3    DS    XL(L'RSFLNAM+RSFLNQ)                                             
RSEL4    DS    XL(L'RSFLNAM+RSFLNQ)                                             
RSEL5    DS    XL(L'RSFLNAM+RSFLNQ)                                             
OVERWRKN EQU   *-OVERWRKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010ACFIL17   06/05/08'                                      
         END                                                                    
