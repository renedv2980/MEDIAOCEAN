*          DATA SET ACFIL45    AT LEVEL 035 AS OF 05/03/12                      
*PHASE T62345B,*                                                                
                                                                                
         TITLE 'APLIMIT RECORD (EBUYER)'                                        
*                                                                               
*YNGX 025 08JAN08 <BR11807D> BUG FIX SHOWING CORRECT APPLIMIT FOR GER           
*TKLU 026 13FEB08 <BR16052L> ALLOW INTERNAL AS TYPE FOR UK                      
*YNGX 027 11MAY09 <LO01-8868> ADD EXPENDITURE TYPE TO APLIMIT RECORD            
*MPEN 028 22SEP09 <LO01-9064> CHANGES TO INVOICES AND NEW EXPENSE APLI          
*JFOS 030 15FEB10 <LO01-9814>NEW APPS:ESTS/ORDS+UNAPP EST/ORDS+APP EST          
*MPEN     25JUN10 <PR000549> ADD INTERNAL ESTIMATES                             
*YNGX 031 31JAN11 <PR001426> RELINK TO INCLUDE NEW TLSTD                        
*MPEN 032 06JAN11 <PR001315> ALLOW HIGHER LEVEL APPROVAL YES ON EXPENSE         
*                                                                               
FIL45    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL45**,RR=RE,R7                                              
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
EXIT     L     R1,CALLR1           RETURN PARAMETERS TO CALLER                  
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITOFF  MVC   FVMSGNO,=AL2(AE$EXPNV) EXPENDITURE TYPE NOT VALID FOR            
         B     EXITL                  THIS OFFICE                               
EXITNA   MVC   FVMSGNO,=AL2(AE$CLENA) CLIENT/ETYPE INPUT NOT ALLOWED            
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
INIT     DS    0H                                                               
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLISTU,DSLISTU                           
T        USING LDGRECD,IOKEY                                                    
         MVC   T.LDGKEY,BCSPACES                                                
         MVC   T.LDGKCPY,CUABIN                                                 
         MVC   T.LDGKUNT(L'BCCPYPRD),BCCPYPRD                                   
         GOTO1 AGETLDG                                                          
         BE    *+6                                                              
         DC    H'0'                NO SJ LEDGER?                                
         ICM   RF,15,ACALDG                                                     
         USING LDGTABD,RF                                                       
         MVC   CLILEN(L'CLILEN+L'PROLEN+L'JOBLEN),LDGTLVA                       
         DROP  RF                                                               
*                                                                               
         TM    BCCPYST1,CPYSOROE   COMPANY USES OFFICES                         
         BNO   EXITOK                                                           
         MVI   GSSKCODE,C'A'                                                    
         MVI   GSSLCODE,C'A'                                                    
         MVI   GSSMCODE,C'A'                                                    
         B     EXITOK                                                           
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
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
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
         USING ALIRECD,R2                                                       
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
KFKVAL   XC    ALIKEY,ALIKEY                                                    
         MVI   ALIKTYP,ALIKTYPQ    APPROVER LIMIT RECORD TYPE                   
         MVI   ALIKSUB,ALIKSUBQ    AND SUBTYPE                                  
         MVC   ALIKCPY,CUABIN      CONNECTED USER                               
         MVI   ALIKCAT,ALIKORD     CATEGORY=ORDERS                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    ALIKEY,ALIKEY                                                    
         MVI   ALIKTYP,ALIKTYPQ    APPROVER LIMIT RECORD TYPE                   
         MVI   ALIKSUB,ALIKSUBQ    AND SUBTYPE                                  
         MVC   ALIKCPY,CUABIN                                                   
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
         USING ALIRECD,R2                                                       
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
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFRES)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD OR COPY                          *         
***********************************************************************         
         SPACE 1                                                                
RFADD    GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',ALIRECD),0               
         CLI   12(R1),0            RSTEL ON RECORD?                             
         BE    RFADD02                                                          
         GOTO1 AADDRST,ALIRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL               SOMETHING WRONG                              
         B     EXITOK                                                           
*                                                                               
RFADD02  L     RF,12(,R1)                                                       
         USING RSTELD,RF                                                        
         MVC   RSTBDATE,BCTODAYP   SET TO TODAY                                 
         MVC   RSTTDATE,BCTODAYP                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RESTORE                              *         
***********************************************************************         
         SPACE 1                                                                
RFRES    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    B     EXITOK                                                           
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
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE                                                                  
RLWRT    DS    0H                                                               
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
         USING ALIRECD,R2                                                       
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
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING ALIRECD,R2          R2 HOLDS A(RECORD)                           
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                         *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(F#APL#ALAPA),AL4(APP)  APPROVAL APPLICATION                  
         DC    AL2(F#APL#ALAPT),AL4(APT)  APPROVAL TYPE                         
         DC    AL2(F#APL#OFFCD),AL4(OFC)  OFFICE CODE                           
         DC    AL2(F#APL#OFFNM),AL4(OFN)  OFFICE NAME                           
         DC    AL2(F#APL#ETY),AL4(ETY)    EXP. TYPE CODE                        
         DC    AL2(F#APL#ETYNM),AL4(ETYN) EXP. TYEP NAME                        
         DC    AL2(F#APL#CLICD),AL4(CLI)  CLIENT CODE                           
         DC    AL2(F#APL#CLINM),AL4(CLN)  CLIENT NAME                           
         DC    AL2(F#APL#ALVAL),AL4(APV)  APPROVAL VALUE                        
         DC    AL2(F#APL#ALFAR),AL4(FAR)  FINAL APPROVAL REQUIRED               
         DC    AL2(F#APL#ALNTH),AL4(NAT)  N'APPROVERS, THIS LVL                 
         DC    AL2(F#APL#ALNPV),AL4(NAP)  N'APPROVERS, PREV LVL                 
         DC    AL2(F#APL#ALFIN),AL4(FAP)  FINAL APPROVER THIS LVL               
         DC    AL2(F#APL#ALHIA),AL4(HIA)  HIGHER APPROVERS THIS LVL             
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL45    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   DS    0H                                                               
         B     EXITOK                                                           
         EJECT ,                                                                
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
***********************************************************************         
* DATA OBJECT FOR APPLICATION ALIKCAT                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APP      LA    RF,APPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
APPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPP)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISAPP)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETAPP)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTAPP)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALAPP)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTAPP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETAPP  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A APPLICATION FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISAPP   SR    RE,RE                                                            
         IC    RE,ALIKCAT                                                       
         MHI   RE,APPTLNQ                                                       
         LA    RE,APPTAB(RE)                                                    
         LH    R1,2(RE)            DD DISP TO APPLICATION LITERAL               
         LA    R1,DSLISTU(R1)                                                   
         MVC   FVIFLD(L'UC@ORDS),0(R1)                                          
         MVC   SVAPP,ALIKCAT       SAVE APPLICATION FOR LATER                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A APPLICATION FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALAPP   SR    R0,R0                                                            
         ICM   R0,1,FVILEN                                                      
         BZ    EXITNV              NO INPUT - INVALID                           
         LA    RE,APPTAB                                                        
         SR    RF,RF                                                            
VAPP02   ICM   RF,1,0(RE)          MINIMUM L'COMPARE                            
         BZ    VALAPPE                                                          
         CR    R0,RF                                                            
         BL    VAPP04              NOT THIS ENTRY                               
         LH    R1,2(RE)            DD DISP TO APPLICATION LITERAL               
         LA    R1,DSLISTU(R1)                                                   
         IC    RF,FVXLEN                                                        
         CLC   FVIFLD(0),0(R1)                                                  
         EX    RF,*-6                                                           
         BE    *+12                                                             
VAPP04   AHI   RE,APPTLNQ                                                       
         B     VAPP02                                                           
         MVC   ALIKCAT,1(RE)       SET APPLICATION                              
*                                                                               
         MVC   FLTIFLD(L'ALIKCAT),ALIKCAT                                       
         MVC   SVAPP,ALIKCAT                                                    
         B     EXITOK              IT'S MAINTAINED IN PRESTO                    
                                                                                
VALAPPE  MVC   FVMSGNO,=AL2(AE$IVAPP)                                           
         B     EXITL               INVALID APPLICATION                          
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A APPLICATION FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTAPP  LA    RE,APPTAB                                                        
*        SR    RF,RF                                                            
DFAPP02  CLC   FLTIFLD(L'ALIKCAT),1(RE)       MATCH ON ALIKCAT VALUE            
         BE    *+12                                                             
         AHI   RE,APPTLNQ                                                       
         B     DFAPP02                                                          
         LH    R1,2(RE)            DD DISP TO APPLICATION LITERAL               
         LA    R1,DSLISTU(R1)                                                   
         MVC   FVIFLD(L'UC@ORDS),0(R1)  SET APPLICATION NAME                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR APPLICATION                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTAPP  CLC   ALIKCAT,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR APPROVAL TYPE                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APT      LA    RF,APTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
APTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPT)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETAPT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPT)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTAPT)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTAPT)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTAPT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT APPROVAL TYPE FIELD AFTER SELECT FROM LIST                *         
***********************************************************************         
         SPACE 1                                                                
DSETAPT  DS    0H                                                               
         B     FLTXX               RETURN 'NOT VALID' TO FORCE UNPROT           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPROVAL TYPE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISAPT   LA    RE,APTYTAB                                                       
DISAPT02 CLI   0(RE),0                                                          
         BE    DISAPTER            TYPE NOT IN TABLE                            
         CLC   ALIKSCAT,0(RE)                                                   
         BE    *+12                                                             
DISAPT04 LA    RE,APTYTLNQ(RE)                                                  
         B     DISAPT02                                                         
         CLI   1(RE),CTRYALL       TEST ALL COUNTRIES                           
         BE    *+14                                                             
         CLC   CUCTRY,1(RE)        ELSE MATCH ON COUNTRY                        
         BNE   DISAPT04                                                         
         LH    R1,2(RE)                                                         
         LA    R1,DSLISTU(R1)                                                   
         MVC   FVIFLD(APTYLN),0(R1)  TYPE NAME                                  
         B     EXITOK                                                           
DISAPTER DS    0H                                                               
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPROVAL TYPE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALAPT   CLI   FVILEN,0                                                         
         BE    EXITNV              NO INPUT - INVALID                           
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RE,APTYTAB                                                       
VALAPT02 CLI   0(RE),0                                                          
         BE    EXITNV              UNKNOWN TYPE - INVALID                       
         CLI   1(RE),CTRYALL       TEST ALL COUNTRIES                           
         BE    *+14                                                             
         CLC   CUCTRY,1(RE)        ELSE MATCH ON COUNTRY                        
         BNE   VALAPT04                                                         
         LH    R1,2(RE)                                                         
         LA    R1,DSLISTU(R1)                                                   
         EX    RF,*+8                                                           
         BE    VALAPT06                                                         
         CLC   FVIFLD(0),0(R1)                                                  
*                                                                               
VALAPT04 LA    RE,APTYTLNQ(RE)                                                  
         B     VALAPT02                                                         
*                                                                               
VALAPT06 CLI   4(RE),0               ANY EXCEPTION CODE?                        
         BE    VALAPT08                                                         
         CLC   SVAPP,4(RE)           IF MATCH ON EXCEPTION ERROR                
         BE    EXITNV                                                           
VALAPT08 MVC   FVIFLD(APTYLN),0(R1)  SHOW FULL TYPE NAME                        
         MVC   ALIKSCAT,0(RE)        AND SET KEY SUBCATEGORY                    
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY A ORDER TYPE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTAPT  LA    RE,APTYTAB                                                       
DFLTPT02 CLI   0(RE),0                                                          
         BE    DFLTPTER            TYPE NOT IN TABLE                            
         CLC   FLTIFLD(L'ALIKSCAT),0(RE)                                        
         BE    *+12                                                             
DFLTPT04 LA    RE,APTYTLNQ(RE)                                                  
         B     DFLTPT02                                                         
         CLI   1(RE),CTRYALL       TEST ALL COUNTRIES                           
         BE    *+14                                                             
         CLC   CUCTRY,1(RE)        ELSE MATCH ON COUNTRY                        
         BNE   DFLTPT04                                                         
         LH    R1,2(RE)                                                         
         LA    R1,DSLISTU(R1)                                                   
         MVC   FVIFLD(APTYLN),0(R1)  TYPE NAME                                  
         B     EXITOK                                                           
DFLTPTER DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ORDER TYPE FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTAPT  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         LA    RE,APTYTAB                                                       
VFLTPT02 CLI   0(RE),0                                                          
         BE    EXITNV              UNKNOWN TYPE - INVALID                       
         CLI   1(RE),CTRYALL       TEST ALL COUNTRIES                           
         BE    *+14                                                             
         CLC   CUCTRY,1(RE)        ELSE MATCH ON COUNTRY                        
         BNE   VFLTPT04                                                         
         LH    R1,2(RE)                                                         
         LA    R1,DSLISTU(R1)                                                   
         EX    RF,*+8                                                           
         BE    VFLTPT06                                                         
         CLC   FVIFLD(0),0(R1)                                                  
VFLTPT04 LA    RE,APTYTLNQ(RE)                                                  
         B     VFLTPT02                                                         
*                                                                               
VFLTPT06 CLI   4(RE),0             ANY EXCEPTION CODE?                          
         BE    VFLTPT08                                                         
         CLC   SVAPP,4(RE)         IF MATCH ON EXCEPTION ERROR                  
         BE    EXITNV                                                           
VFLTPT08 MVC   FVIFLD(APTYLN),0(R1)  SHOW FULL TYPE NAME                        
         MVC   FLTIFLD(L'ALIKSCAT),0(RE)                                        
         B     EXITOK                                                           
***********************************************************************         
* DO FILTERING ON A ORDER TYPE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DOFTAPT  CLC   FLTIFLD(L'ALIKSCAT),BCSPACES                                     
         BNH   FLTXE                                                            
         CLC   ALIKSCAT,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFC      LA    RF,OFFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFF)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOFF)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOFF)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOFF)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFF)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHOFF)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETOFF  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISOFF   MVC   FVIFLD(L'ALIKOFFC),ALIKOFFC                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   ALIKOFFC,FVIFLD                                                  
         MVC   FLTIFLD(L'ALIKOFFC),ALIKOFFC                                     
*                                                                               
         CLI   CSACT,A#DIS         DON'T TEST OFFICE CODE                       
         BE    EXITOK              IF DISPLAY                                   
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE CODE                             
         BNE   EXITL               INVALID OFFICE                               
         TM    BIT1,CLIINP                                                      
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$OFORC)                                           
         B     EXITL                                                            
         OI    BIT1,OFFINP                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  MVC   FVIFLD(L'ALIKOFFC),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLC   ALIKOFFC,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
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
OFN      LA    RF,OFFNTBL          TABLE OF KNOWN VERBS                         
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
DISOFFN  OC    ALIKOFFC,ALIKOFFC   TEST ALL OFFICE                              
         BZ    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         MVC   IOKEY,BCSPACES                 READ THE ACCOUNT RECORD           
         MVC   T.ACTKCPY,ALIKCPY              COMPANY                           
         MVC   T.ACTKUNT(L'OFFUL),OFFUL       UNIT/LEDGER                       
         MVC   T.ACTKACT(L'ALIKOFFC),ALIKOFFC OFFICE CODE CODE                  
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET OFFICE CODE NAME                         
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXP. TYPE CODE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ETY      LA    RF,ETYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ETYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISETY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALETY)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISETY)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETETY)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTETY)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTETY)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTETY)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHETY)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETETY  B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EXP. TYPE CODE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISETY   OC    ALIKETYP,ALIKETYP   ALL EXP. TYPE                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'ALIKETYP),ALIKETYP                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EXP. TYPE CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALETY   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   ALIKETYP,FVIFLD     MOVE IN EXP. TYPE CODE                       
T        USING ETYRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE EXP. TYPE RECORD                    
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,ALIKCPY   COMPANY                                      
         MVC   T.ETYKCODE,ALIKETYP EXP. TYPE CODE                               
         MVC   SVIOKEY,IOKEY                                                    
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   SVIOKEY(ETYKOFFC-ETYRECD),IOKEY                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INETY)                                           
         B     EXITL               INVALID EXP. TYPE                            
*                                                                               
         CLC   T.ETYKOFFC,BCSPACES NO OFFICE CODE THEN FINE                     
         BNH   VALETYX                                                          
         CLI   CUACCS,0            GLOBAL LOGON                                 
         BE    VALETYX             THEN FINE TO USE IT                          
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   VALETY04                                                         
         CLI   CUACCS,C'$'         LIMIT LIST LOGON?                            
         BNE   VALETY02                                                         
         CLI   T.ETYKOFFC,C'$'     OFFICE LIST EXPENDITURE TYPE                 
         BNE   VALETY02                                                         
         CLC   CUACCS(2),T.ETYKOFFC CHECK WHETHER OFFICE LIST MATCHES           
         BE    VALETYX                                                          
         B     EXITOFF             EXPENDITURE TYPE NOT VALID ON THIS           
*                                                                               
VALETY02 GOTO1 ATSTOFF,T.ETYKOFFC                                               
         BE    VALETYX                                                          
         B     EXITL               EXPENDITURE TYPE NOT VALID                   
*                                                                               
X        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
VALETY04 MVC   SVIOKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   X.OFFKTYP,OFFKTYPQ                                               
         MVC   X.OFFKCPY,CUABIN                                                 
         MVC   X.OFFKOFF,CUACCS+2                                               
         L     R1,=AL4(XOHI+XOACCDIR+XIO2)                                      
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    X.OFFKSTAT,OFFSLIST  OFFICE LIST?                                
         BZ    VALETY06             NO THEN VALIDATE OFFICE                     
         MVC   IOKEY,SVIOKEY                                                    
         CLC   CUACCS+2(2),T.ETYKOFFC CHECK WHETHER MATCH ON                    
         BE    VALETYX              OFFICE LIST                                 
*                                                                               
VALETY06 MVC   IOKEY,SVIOKEY                                                    
         GOTO1 ATSTOFF,T.ETYKOFFC                                               
         BE    VALETYX                                                          
         B     EXITL                EXPENDITURE TYPE NOT VALID                  
*                                                                               
VALETYX  B     EXITOK                                                           
         DROP  T,X                                                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EXP. TYPE CODE FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTETY  MVC   FVIFLD(L'ALIKETYP),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EXP. TYPE CODE FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
VFLTETY  MVC   ALIKETYP,FVIFLD     MOVE IN EXP. TYPE CODE                       
         MVC   FLTIFLD(L'ALIKETYP),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR EXP. TYPE CODE                                     *         
***********************************************************************         
         SPACE 1                                                                
DOFTETY  CLC   ALIKETYP,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A WORK CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHETY  CLI   CSACT,A#LST                                                      
         BNE   SRETY10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SRETY10  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,EXTYP,ACOM,0            
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A EXP. TYPE NAME FIELD                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ETYN     LA    RF,ETYNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ETYNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISETYN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EXP. TYPE NAME FIELD FROM THE KEY                         *         
***********************************************************************         
         SPACE 1                                                                
DISETYN  OC    ALIKETYP,ALIKETYP   TEST ALL EXP. TYPE CODE                      
         BZ    EXITOK                                                           
*                                                                               
T        USING ETYRECD,IOKEY                                                    
         XC    IOKEY,IOKEY         READ THE EXP. TYPE RECORD                    
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,ALIKCPY   COMPANY                                      
         MVC   T.ETYKCODE,ALIKETYP EXP. TYPE CODE                               
         MVC   SVIOKEY,IOKEY                                                    
         DROP  T                                                                
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         CLC   SVIOKEY(ETYKOFFC-ETYRECD),IOKEY                                  
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET EXP. TYPE NAME                           
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CLIENT CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CLI      LA    RF,CLITBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CLITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLI)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETCLI)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCLI)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCL)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCLI)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCLI)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCLI)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT CLIENT CODE FIELD AFTER SELECT FROM LIST                  *         
***********************************************************************         
         SPACE 1                                                                
DSETCLI  DS    0H                                                               
         B     FLTXX               RETURN 'NOT VALID' TO FORCE UNPROT           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN CLIENT CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DISCLI   MVC   FVIFLD(L'ALIKCLIC),ALIKCLIC                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLIENT CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCLI   CLI   ALIKCAT,ALIKEXPS    CLIENT/ETYPE ENTRY NOT ALLOWED               
         BNE   VALCLI2                                                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD,BCSPACES     NOT VALID ENTRY                              
         BNH   EXITOK                                                           
         B     EXITNA                                                           
*                                                                               
VALCLI2  ICM   RF,1,FVILEN                                                      
         BZ    EXITOK                                                           
         CLC   FVILEN,CLILEN                                                    
*&&UK*&& BE    *+14                                                             
*&&US*&& BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCLI)                                           
         B     EXITL               INVALID CLIENT CODE                          
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),BCCPYPRD                            
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),FVIFLD                                                
         OC    ACTKACT,BCSPACES                                                 
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         TM    BIT1,OFFINP                                                      
         BNO   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$OFORC)                                           
         B     EXITL                                                            
         OI    BIT1,CLIINP                                                      
         MVC   ALIKCLIC,ACTKACT                                                 
VALCLIX  B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A CLIENT CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
SRCHCL   MVC   BODUB1(L'BCCPYPRD),BCCPYPRD                                      
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,BODUB1,ACOM,   C        
               (X'13',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLIENT CODE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTCLI  MVC   FVIFLD(L'ALIKCLIC),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLIENT CODE FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTCLI  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         CLC   FVILEN,CLILEN                                                    
*&&UK*&& BE    *+14                                                             
*&&US*&& BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCLI)                                           
         B     EXITL               INVALID CLIENT CODE                          
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),BCCPYPRD                            
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EXMVC RF,ACTKACT,FVIFLD                                                
         OC    ACTKACT,BCSPACES                                                 
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         MVC   FLTIFLD(L'ALIKCLIC),FVIFLD                                       
         OC    FLTIFLD(L'ALIKCLIC),BCSPACES                                     
         B     EXITOK                                                           
***********************************************************************         
* DO FILTERING ON A CLIENT CODE FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFTCLI  CLC   FLTIFLD(L'ALIKCLIC),BCSPACES                                     
         BE    FLTXE                                                            
         CLC   ALIKCLIC,BCSPACES                                                
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
         CLC   ALIKCLIC,FLTIFLD                                                 
         BNE   FLTXL                                                            
         BE    FLTXE                                                            
*        BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR CLIENT NAME                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CLN      LA    RF,CLNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CLNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN CLIENT NAME FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DISCLN   CLC   ALIKCLIC,BCSPACES                                                
         BNH   EXITOK                                                           
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'ACTKUNT+L'ACTKLDG),BCCPYPRD                            
         MVC   ACTKACT(L'ALIKCLIC),ALIKCLIC                                     
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         SR    R0,R0                                                            
         L     R4,AIO2                                                          
         LA    R4,ACTRFST                                                       
*                                                                               
         USING NAMELD,R4                                                        
DCLIN02  CLI   NAMEL,0                                                          
         BE    EXITOK                                                           
         CLI   NAMEL,NAMELQ                                                     
         BE    DCLIN06                                                          
*                                                                               
DCLIN04  IC    R0,NAMLN                                                         
         AR    R4,R0                                                            
         B     DCLIN02                                                          
*                                                                               
DCLIN06  SR    RF,RF                                                            
         IC    RF,NAMLN                                                         
         SHI   RF,NAMLN1Q+1                                                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FVIFLD(0),NAMEREC                                                
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR APPROVAL VALUE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
APV      LA    RF,APVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
APVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPV)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPV)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPROVAL VALUE FIELD                                    *          
***********************************************************************         
         SPACE 1                                                                
DISAPV   OC    TLKAPV,TLKAPV                                                    
         BZ    EXITOK                                                           
         CURED (P6,TLKAPV),(13,FVIFLD),0,ALIGN=LEFT,COMMAS=YES,        C        
               DMCB=BODMCB                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPROVAL VALUE FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VALAPV   XC    TLKAPV,TLKAPV                                                    
         MVC   TLRLEN,=AL2(TLLNQ2)  SET LENGTH OF TSAR RECORD                   
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
         SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BOPARM,(C'0',FVIFLD),(RF),0                             
         CLI   0(R1),X'FF'                                                      
         BE    EXITNV                                                           
         ZAP   TLKAPV,6(L'TLKAPV,R1)                                            
         CP    TLKAPV,=P'0'                                                     
         BNP   EXITNV                                                           
         CP    TLKAPV,=P'10000000000'                                           
         BNL   EXITNV                                                           
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FINAL APPROVER REQUIRED STATUS                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING ALIRECD,R2                                                       
FAR      LA    RF,FARTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FARTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFAR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFAR)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFAR)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFAR)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFAR)                                
         DC    AL1(EOT)                                                         
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FINAL APPROVER REQUIRED STATUS FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING ALIRSTA,GSRECSTA                                                 
DISFAR   MVC   FVIFLD(L'BC@NO),BC@NO                                            
         TM    T.ALIRSTAT,ALISFAPP                                              
         BZ    *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FINAL APPROVER REQUIRED STATUS FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING ALIRSTA,GSRECSTA                                                 
VALFAR   NI    T.ALIRSTAT,X'FF'-ALISFAPP                                        
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         OI    FVIFLD,X'40'        ENSURE UPPER CASE                            
         CLI   SVAPP,ALIKEXPS      EXPENSES?                                    
         BE    EXITOK              ONLY NO ALLOWED                              
         CLI   SVAPP,ALIKINVN      INVOICES NO PURCHASING APPROVERS?            
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    VALFAR02                                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         LA    RF,ALIRFST          IF FINAL APPROVER REQUIRED,                  
         SR    R0,R0               MAX. PREVIOUS APPROVAL LVLS 1 LESS           
         USING APLELD,RF                                                        
VALFAR00 CLI   0(RF),0             TEST EOR                                     
         BNE   *+12                                                             
         OI    T.ALIRSTAT,ALISFAPP OK - SET KEY STATUS BIT                      
         B     VALFAR02                                                         
         CLI   0(RF),APLELQ                                                     
         BNE   *+12                                                             
         CLI   APLPREV,3                                                        
         BE    VALFAERR            TOO MANY PREVIOUS APPROVAL LVLS              
         IC    R0,APLLN                                                         
         AR    RF,R0                                                            
         B     VALFAR00                                                         
*                                                                               
VALFAR02 GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',ALIRECD),0               
         CLI   12(R1),0                                                         
         BE    VALFAR04                                                         
         GOTO1 AADDRST,ALIRECD     NOT FOUND - ADD ONE                          
         BNE   EXITL                                                            
         B     VALFAR02            AND GET ITS ADDRESS                          
VALFAR04 L     RF,12(R1)                                                        
         USING RSTELD,RF                                                        
         NI    RSTSTAT6,X'FF'-RSTSFAPP                                          
         TM    T.ALIRSTAT,ALISFAPP                                              
         BZ    *+8                                                              
         OI    RSTSTAT6,RSTSFAPP   SET RSTEL STATUS BIT                         
         B     EXITOK                                                           
VALFAERR DS    0H                                                               
         MVC   FVMSGNO,=AL2(AE$M2APF) MAX 3 APPROVER LVLS IF FINAL APPR         
         B     EXITL                                                            
         DROP  T,RF                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FINAL APPROVER REQUIRED STATUS FIELD                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTFAR  MVC   FVIFLD(L'BC@YES),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FINAL APPROVER REQUIRED STATUS FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTFAR  CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT - OK                                
         OI    FVIFLD,X'40'        ENSURE UPPER CASE                            
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BC@NO                                          
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON A FINAL APPROVER REQUIRED STATUS FIELD              *         
***********************************************************************         
         SPACE 1                                                                
T        USING ALIRSTA,GSRECSTA                                                 
DOFTFAR  LA    RF,X'70'            BNZ CC                                       
         CLC   FLTIFLD(L'BC@YES),BC@YES                                         
         BE    *+8                                                              
         LA    RF,X'80'            BZ CC                                        
         TM    T.ALIRSTAT,ALISFAPP                                              
         EX    RF,*+8                                                           
         B     FLTXH                                                            
         NOP   FLTXE                                                            
         DROP  T                                                                
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR N'APPROVERS, THIS LEVEL                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
NAT      LA    RF,NATTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
NATTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNAT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A N'APPROVERS FIELD                                        *          
***********************************************************************         
         SPACE 1                                                                
DISNAT   EDIT  (B1,TLKNAT),(3,FVIFLD),0,ALIGN=LEFT,ZERO=NOBLANK                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A N'APPROVERS FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALNAT   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         GOTO1 VNUMVAL,BOPARM,(0,FVIFLD),(01,0)                                 
         CLI   0(R1),X'FF'                                                      
         BE    EXITNV                                                           
         ICM   RF,15,4(R1)                                                      
         BZ    EXITNV              =0  INVALID                                  
         C     RF,=F'0'                                                         
         BL    EXITNV              <0  INVALID                                  
         C     RF,=F'2'                                                         
         BH    EXITNV              >02 INVALID                                  
VALNAT4  STC   RF,TLKNAT                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR N'APPROVERS, PREVIOUS LEVEL                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
NAP      LA    RF,NAPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
NAPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNAP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY N'APPROVERS, PREVIOUS LEVEL                                 *         
***********************************************************************         
         SPACE 1                                                                
DISNAP   EDIT  (B1,TLKNAP),(3,FVIFLD),0,ALIGN=LEFT,ZERO=NOBLANK                 
         LA    RF,FVIFLD+3                                                      
         EDIT  (B2,TLNUM),(3,(RF)),0                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE N'APPROVERS, PREVIOUS LEVEL(S)                             *         
***********************************************************************         
         SPACE 1                                                                
T        USING ALIRSTA,GSRECSTA                                                 
VALNAP   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         GOTO1 VNUMVAL,BOPARM,(0,FVIFLD),(01,0)                                 
         CLI   0(R1),X'FF'                                                      
         BE    EXITNV                                                           
         ICM   RF,15,4(R1)                                                      
         C     RF,=F'0'                                                         
         BL    EXITNV              <0  INVALID                                  
         C     RF,=F'3'            >3 INVALID-CHECK FINAL APP IN VALFAP         
         BH    VALNAERR                                                         
VALNAP2  LH    RE,TLNUM            TSAR REC NUMBER                              
         CR    RE,RF               N'PREV LVLS MUST BE < THAN N'RECS            
         BNH   EXITNV              ELSE INVALID                                 
         L     R3,FVADDR           GET ADDRESS IN TWA OF FIELD HDR              
         SR    R0,R0                                                            
         IC    R0,0(R3)            FIELD LENGTH                                 
         AR    R3,R0               -> FINAL APPROVAL FIELD                      
         OI    4(R3),FVITHIS       FORCE FUNCTION CALL (VALFAP)                 
         STC   RF,TLKNAP                                                        
         B     EXITOK                                                           
VALNAERR DS    0H                                                               
         MVC   FVMSGNO,=AL2(AE$M2APF) MAX 2 PREVIOUS LVLS IF FINAL APPR         
         B     EXITL                                                            
         DROP  T                                                                
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FINAL APPROVAL,THIS LEVEL                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
FAP      LA    RF,FAPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FAPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFAP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFAP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FINAL APPROVER REQUIRED STATUS FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
DISFAP   MVC   FVIFLD(L'BC@NO),BC@NO                                            
         CLI   TLKFAP,C'Y'                   THIS LVL SET TO Y                  
         BNE   *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES       YES                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FINAL APPROVER REQUIRED STATUS FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
VALFAP   MVI   TLKFAP,X'40'        SET TO NO                                    
         CLI   FVILEN,0            INPUT LENGTH                                 
         BE    EXITOK              NO ENTRY - DEFAULT TO NO                     
         OI    FVIFLD,X'40'        ENSURE UPPER CASE                            
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK              STORE AS NO                                  
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV              NOT VALID ENTRY                              
*                                  YES - CHECK NO. OF PREV LVLS                 
         CLI   TLKNAP,3            CHECK NO. PREV LVL APPS < 3                  
         BNL   VALFAPER            TOO MANY APPROVERS AT PREV LEVELS            
         MVI   TLKFAP,C'Y'         STORE AS YES                                 
         B     EXITOK                                                           
VALFAPER DS    0H                                                               
         MVC   FVMSGNO,=AL2(AE$M2APF) MAX 3 APPROVER LVLS IF FINAL APPR         
         B     EXITL                                                            
         SPACE 2                                                                
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR RETURN HIGHER LEVEL APPROVERS,THIS LVL              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
HIA      LA    RF,HIATBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
HIATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISHIA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHIA)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RETURN HIGHER APPROVAL LEVELS STATUS FIELD                  *         
***********************************************************************         
         SPACE 1                                                                
DISHIA   MVC   FVIFLD(L'BC@NO),BC@NO                                            
         CLI   TLKHIA,C'Y'                                                      
         BNE   *+10                                                             
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FINAL APPROVER REQUIRED STATUS FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
VALHIA   MVI   TLKHIA,X'40'        SET AS NO                                    
         CLI   FVILEN,0            INPUT LENGTH = 0                             
         BE    EXITOK              DEFAULT TO NO                                
         OI    FVIFLD,X'40'        ENSURE UPPER CASE                            
*                                                                               
VALHIA2  CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK              STORE AS NO                                  
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV              NOT VALID ENTRY                              
*                                  YES - DON'T VALIDATE, NO NEED TO             
         MVI   TLKHIA,C'Y'         STORE AS YES                                 
         B     EXITOK                                                           
         SPACE 2                                                                
         POP   USING                                                            
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
         CLI   SREC,R#APLIMIT      APLIMIT RECORD                               
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
THIS     USING ALIRECD,R2                                                       
LAST     USING ALIRECD,R3                                                       
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
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
*                                                                               
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
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
* FIRST TIME FOR LIST PAGE                                            *         
***********************************************************************         
         SPACE 1                                                                
X        USING ALIRECD,IOKEY                                                    
FLST     MVC   X.ALIKEY,THIS.ALIKEY                                             
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    *+12                                                             
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
                                                                                
NLST02   CLC   X.ALIKEY(ALIKREM-ALIKEY),THIS.ALIKEY                             
         BNE   EXITL               DIFFERENT RT/CPY - FINISHED                  
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
                                                                                
NLST06   MVC   THIS.ALIKEY(ACCKLEN),IOKEY   WE WANT THIS KEY                    
         B     EXITOK                                                           
         DROP  THIS,LAST,X                                                      
         SPACE 2                                                                
***********************************************************************         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         OI    LSSTAT2,LSSNOSEQ+LSSADD                                          
         MVC   LSCOLLIN,=AL2(80)                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,ALIRFST-ALIRECD                                               
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
         LA    RF,ALIRFST-ALIRECD(,RF)  IT IS NOW                               
         XR    RE,RE                                                            
*                                                                               
         USING APLELD,RF                                                        
FML02    CLI   APLEL,0             TEST EOR                                     
         BE    EXITL               YES                                          
         CLI   APLEL,APLELQ                                                     
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
         LA    RF,ALIRFST-ALIRECD(,RF)  IT IS NOW.                              
*                                                                               
         USING APLELD,RF                                                        
NML02    CLI   APLEL,0             TEST EOR                                     
         BE    EXITL               YES                                          
         CLI   APLEL,APLELQ        APLEL                                        
         BE    NML06               YES                                          
                                                                                
NML04    IC    RE,APLLN                                                         
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
         MVC   TLRLEN,=AL2(TLLNQ2)  SET LENGTH OF TSAR RECORD                   
         LH    RF,MNTDISP                                                       
         A     RF,AIOREC                                                        
         USING APLELD,RF           MOVE IN DETAILS FROM ELEMENT                 
         ZAP   TLKAPV,APLVAL       APPROVAL VALUE                               
         MVC   TLKNAT,APLNUM       N'APPROVERS, THIS LEVEL                      
         MVC   TLKNAP,APLPREV      N'APPROVERS, PREV LEVEL                      
         CLI   APLLN,APLLN3Q       NEW LENGTH ELEMENT(FAPP+HIGH LVL)            
         BE    TSARFIL2                                                         
         MVI   TLKHIA,X'40'        OLD STYLE REC - DEFAULT TO NO                
         MVI   TLKFAP,X'40'                        DEFAULT TO NO                
X        USING ALIRECD,IOKEY                                                    
         TM    X.ALIKSTAT,ALISFAPP  FINAL APPROVAL (OLD) REQUIRED               
         BZ    EXITOK                                                           
         MVI   TLKFAP,C'Y'          SET FAP AS UES                              
         B     EXITOK                                                           
TSARFIL2 MVC   TLKFAP,APLFAPP      FINAL APPROVER STATUS FIELD                  
         MVC   TLKHIA,APLHILVL     RETURN HIGHER LEVEL APPS STATUS FLD          
         B     EXITOK                                                           
         DROP  RF,R3,X                                                          
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
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('APLELQ',AIOREC),0                
         B     EXITOK              DELETE ALL PREVIOUS APLELS                   
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING ALIRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
                                                                                
         MVI   ANYLINES,YES                                                     
         LM    R2,R3,SVPARMS3                                                   
         XC    BOELEM,BOELEM                                                    
         USING APLELD,BOELEM       MOVE IN DETAILS TO ELEMENT                   
         MVI   APLEL,APLELQ                                                     
         MVI   APLLN,APLLN3Q       NEW LENGTH                                   
         ZAP   APLVAL,TLKAPV       APPROVAL VALUE                               
         MVC   APLNUM,TLKNAT       N'APPROVERS, THIS LEVEL                      
         MVC   APLPREV,TLKNAP      N'APPROVERS, PREV LEVEL                      
         MVC   APLFAPP,TLKFAP      FINAL APPROVER STATUS FIELD                  
         MVC   APLHILVL,TLKHIA     RETURN HIGHER LEVEL APPS STATUS FLD          
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
         DROP  R2,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 CLI   CSACT,A#ADD                                                      
         BE    *+12                                                             
         CLI   CSACT,A#CHA                                                      
         BNE   EXITOK                                                           
         CLI   ANYLINES,YES        EMPTY LIST?                                  
         BE    EXITOK              NO - OK                                      
         MVC   FVMSGNO,=AL2(AE$NLINE)                                           
         LH    RF,LS1STLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         CLI   CSACT,A#ADD         IF ACTION IS ADD                             
         BE    EXITL               GIVE THE POOR SOD A CHANCE                   
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
         SPACE 1                                                                
PZERO    DC    P'0'                                                             
PONE     DC    P'1'                                                             
OFFUL    DC    C'2D'                                                            
EXTYP    DC    C'ET'                                                            
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
APTYLN   EQU   15                                                               
STMPSTRQ EQU   X'03'                                                            
         SPACE 2                                                                
DCLISTU  DS    0D                                                               
         DCDDL AC#CLINT,APTYLN,L                                                
         DCDDL AC#NCLI,APTYLN,L                                                 
         DCDDL AC#EXPEN,APTYLN,L                                                
         DCDDL AC#PRD,APTYLN,L                                                  
         DCDDL AC#ARTST,APTYLN,L                                                
         DCDDL AC#INT,APTYLN,L                                                  
         DCDDL AC#DEF,APTYLN,L                                                  
         DCDDL AC#ORDS,L'UC@ORDS,L                                              
         DCDDL AC#INVS,L'UC@INVS,L                                              
         DCDDL AC#INVNO,L'UC@INVNO,L                                            
         DCDDL AC#EST,L'UC@ESTS,L                                               
*&&UK*&& DCDDL AC#IEST,L'UC@ESTI,L                                              
*&&US*&& DCDDL AC#INTES,L'UC@ESTI,L                                             
         DCDDL AC#ORSAP,L'UC@ORSAP,L                                            
         DCDDL AC#ORSUN,L'UC@ORSUN,L                                            
DCLISTX  DC    AL1(EOT)                                                         
                                                                                
         DS    0D                                                               
*              AL1(MIN L'INP,ALIKCAT),Y(DISP TO DD ENTRY) *ALIKCAT SEQ*         
APPTAB   DC    AL1(2,ALIKORD),Y(UC@ORDS-DSLISTU)  ORDERS                        
APPTLNQ  EQU   *-APPTAB                                                         
         DC    AL1(4,ALIKINV),Y(UC@INVS-DSLISTU)   INVOICES                     
         DC    AL1(4,ALIKINVN),Y(UC@INVNO-DSLISTU) INVS NO PO                   
         DC    AL1(2,ALIKEXPS),Y(UC@EXP-DSLISTU)   EXPENSES                     
         DC    AL1(2,ALIKESTS),Y(UC@ESTS-DSLISTU)  ESTIMATES                    
         DC    AL1(6,ALIKOEAP),Y(UC@ORSAP-DSLISTU) ORDS W'APPROVED ESTS         
         DC    AL1(6,ALIKOEUN),Y(UC@ORSUN-DSLISTU) ORDS W'UNAPRVED ESTS         
         DC    AL1(3,ALIKESTI),Y(UC@ESTI-DSLISTU)  INTERNAL ESTIMATES           
         DC    H'0'                                                             
         DS    0D                                                               
APTYTAB  DC    AL1(ALIKCLI,CTRYALL),YL2(UC@CLINT-DSLISTU),AL1(ALIKEXPS)         
APTYTLNQ EQU   *-APTYTAB                                                        
         DC    AL1(ALIKDFT,CTRYALL),YL2(UC@DEF-DSLISTU),AL1(0)                  
         DC    AL1(ALIKNCLI,CTRYALL),YL2(UC@NCLI-DSLISTU),AL1(ALIKEXPS)         
         DC    AL1(ALIKEXP,CTRYALL),YL2(UC@EXP-DSLISTU),AL1(0)                  
         DC    AL1(ALIKPROD,CTRYALL),YL2(UC@PRD-DSLISTU),AL1(0)                 
         DC    AL1(ALIKART,CTRYALL),YL2(UC@ARTST-DSLISTU),AL1(ALIKEXPS)         
         DC    AL1(ALIKINT,CTRYALL),YL2(UC@INT-DSLISTU),AL1(ALIKEXPS)           
*        DC    AL1(ALIKINT,CTRYGER),YL2(UC@INT-DSLISTU),AL1(ALIKEXPS)           
         DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
DUB      DS    D                                                                
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
         SPACE 1                                                                
MNTDISP  DS    H                                                                
ANYLINES DS    C                                                                
CLILEN   DS    XL1                 LENGTH OF CLIENT                             
PROLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT                     
JOBLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT+JOB                 
*                                                                               
BIT1     DS    XL1                                                              
OFFINP   EQU   X'80'                                                            
CLIINP   EQU   X'40'                                                            
         SPACE 1                                                                
WORK     DS    XL64                                                             
SVIOKEY  DS    XL(L'IOKEY)                                                      
SVAPP    DS    XL1                 APPLICATION                                  
*                                                                               
DSLISTU  DS    0D                                                               
UC@CLINT DS    CL(APTYLN)                                                       
UC@NCLI  DS    CL(APTYLN)                                                       
UC@EXP   DS    CL(APTYLN)                                                       
UC@PRD   DS    CL(APTYLN)                                                       
UC@ARTST DS    CL(APTYLN)                                                       
UC@INT   DS    CL(APTYLN)                                                       
UC@DEF   DS    CL(APTYLN)                                                       
*                                                                               
UC@ORDS  DS    CL(APTYLN)                                                       
UC@INVS  DS    CL(APTYLN)                                                       
UC@INVNO DS    CL(APTYLN)                                                       
UC@ESTS  DS    CL(APTYLN)                                                       
UC@ESTI  DS    CL(APTYLN)                                                       
UC@ORSAP DS    CL(APTYLN)                                                       
UC@ORSUN DS    CL(APTYLN)                                                       
*                                                                               
OVERWRKX EQU   *-OVERWRKD                                                       
                                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKAPV   DS    PL6                 APPROVAL VALUE                               
         ORG   TLUSER                                                           
TLKNAT   DS    XL1                 N'APPROVERS, THIS LEVEL                      
TLKNAP   DS    XL1                 N'APPROVERS, PREVIOUS LEVEL                  
TLLNQ    EQU   *-TLSTD                                                          
TLKFAP   DS    CL1                 FINAL APPROVER, THIS LEVEL                   
TLKHIA   DS    CL1                 RETURN HIGHER LVL APPROVERS,THIS LVL         
TLLNQ2   EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'035ACFIL45   05/03/12'                                      
         END                                                                    
