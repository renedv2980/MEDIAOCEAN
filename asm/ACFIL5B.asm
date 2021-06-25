*          DATA SET ACFIL5B    AT LEVEL 004 AS OF 02/26/19                      
*PHASE T6235BA,*                                                                
                                                                                
         TITLE 'DISTANCE RATE TYPE RECORD'                                      
*                                                                               
* YNGX 000 25FEB11 <PR001547> NEW VERSION                                       
* YNGX 002 05DEC11 <BR46033L> RELINK TO INCLUDE NEW DSECT FOR FFTTDIS           
* MPEN 003 06JUN13 <PR003554> RELINK FOR NEW GEFILWORK                          
* SGAV 004 05SEP18 <DSPCA-2823> FLIST-MAX LENGTH ISSUE WITH FLIST REC           
*                                                                               
FIL5B    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL5B**,R7,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
         L     RE,ATWA                                                          
         MVC   SESNL,TWASESNL-TWAD(RE)                                          
         L     R6,ATWA                                                          
         AH    R6,=Y(TWUSER-TWAD)                                               
         USING SAVED,R6                                                         
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
EXITSHRT MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     EXITL               EXIT WITH FIELD TOO SHORT SET                
EXITLONG MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               EXIT WITH FIELD TOO LONG SET                 
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
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLISTU,DSLISTU                           
         GOTO1 (RF),(R1),C'LL  ',DCLISTL,DSLISTL                                
*                                                                               
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
         BE    ITER04              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE TABLE                                
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER04              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
*                                                                               
ITER04   ICM   RF,15,OBJADR        INVOKE OBJECT                                
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
         USING DRTRECD,R2                                                       
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
KFKVAL   XC    DRTKEY,DRTKEY       INITIALIZE KEY OF RECORD                     
         MVI   DRTKTYP,DRTKTYPQ    DISRATE RECORD TYPE                          
         MVI   DRTKSUB,DRTKSUBQ    DISRATE SUB-RECORD TYPE                      
         MVC   DRTKOFF,BCSPACES                                                 
         MVC   DRTKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    DRTKEY,DRTKEY       INITIALIZE KEY OF RECORD                     
         MVI   DRTKTYP,DRTKTYPQ    DISRATE RECORD TYPE                          
         MVI   DRTKSUB,DRTKSUBQ    DISRATE SUB-RECORD TYPE                      
         MVC   DRTKCPY,CUABIN      CONNECTED ID                                 
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
         USING DRTRECD,R2                                                       
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
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD/RESTORE                          *         
***********************************************************************         
         SPACE 1                                                                
RFADD    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE                                                                  
RECLAST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLRES)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE ALL DISRATE SUB-RECORDS        *         
***********************************************************************         
         SPACE 2                                                                
T        USING DRTRECD,R4                                                       
RLDEL    MVC   IOKEY(L'DRTKEY),DRTKEY                                           
*                                                                               
RLDEL10  LA    R4,IOKEY            READ NEXT DISRATE SUB-RECORD                 
         SR    RF,RF               THEY ARE ALWAYS IN SEQUENCE                  
         IC    RF,T.DRTKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.DRTKSEQ                                                     
*                                                                               
         L     R1,=AL4(XORDUP+XOACCDIR+XIO2)                                    
         GOTO1 AIO                                                              
         BNE   EXITOK              DELETE OR NO FOUND - END                     
*                                                                               
         OI    T.DRTKSTAT,DRTSDELT DELETE DISRATE DIR                           
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         OI    T.DRTRSTAT,DRTSDELT DELETE MASTER RECORD                         
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
         B     RLDEL10                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE ALL DISRATE SUB-RECORDS       *         
***********************************************************************         
         SPACE 2                                                                
T        USING DRTRECD,R4                                                       
RLRES    MVC   IOKEY(L'DRTKEY),DRTKEY                                           
*                                                                               
RLRES10  LA    R4,IOKEY            READ NEXT DISRATE SUB-RECORD                 
         SR    RF,RF               THEY ARE ALWAYS IN SEQUENCE                  
         IC    RF,T.DRTKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.DRTKSEQ                                                     
*                                                                               
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)                                   
         GOTO1 AIO                                                              
         BE    RLRES10             RECORD FOUND!!! - GET NEXT                   
         TM    IOERR,IOERRS-(IOEDEL+IOERNF)                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOERNF        RECORD NOT FOUND - END                       
         BO    EXITOK                                                           
*                                                                               
         NI    T.DRTKSTAT,FF-DRTSDELT     RESTORE IT                            
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         NI    T.DRTRSTAT,FF-DRTSDELT                                           
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
         B     RLRES10                                                          
         DROP  T                                                                
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
         L     R2,SVPARMS+12                                                    
         USING DRTRECD,R2                                                       
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
         USING DRTRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(F#DRT#OFFC),AL4(OFFDTA)    OFFICE CODE                       
         DC    AL2(F#DRT#EFFDT),AL4(EDTDTA)   EFFECTIVE DATE                    
         DC    AL2(F#DRT#OFFNM),AL4(OFNDTA)   OFFICE NAME                       
         DC    AL2(F#DRT#VEHTY),AL4(VTYDTA)   VEHICLE TYPE                      
*&&UK*&& DC    AL2(F#DRT#FUETY),AL4(FTYDTA)   FUEL TYPE                         
*&&UK*&& DC    AL2(F#DRT#ENGSZ),AL4(ESZDTA)   ENGINE SIZE                       
         DC    AL2(F#DRT#DISBD),AL4(DBDDTA)   DISTANCE BAND                     
         DC    AL2(F#DRT#CRSRT),AL4(CRTDTA)   GROSS RATE                        
         DC    AL2(F#DRT#VATRT),AL4(VRTDTA)   VAT RATE                          
*&&UK*&& DC    AL2(F#DRT#PASRT),AL4(PRTDTA)   PASSENGER RATE                    
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL5B    CSECT                                                                  
         EJECT ,                                                                
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
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   CLM   R2,B'0111',ATLST+1  ARE WE PROCESSING MAINTENANCE LIST?          
         BE    EXITOK              YES - EXIT                                   
*                                                                               
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
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTOFF)                               
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
DISOFF   CLC   DRTKOFF,BCSPACES                                                 
         BNE   *+12                                                             
         MVI   FVIFLD,C'*'         ALL OFFICE ENTRIES                           
         B     EXITOK                                                           
         MVC   FVIFLD(L'DRTKOFF),DRTKOFF                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TWAD,R4                                                          
VALOFF   L     R4,ATWA                                                          
*                                                                               
         MVC   DRTKOFF,BCSPACES                                                 
         CLI   FVIFLD,C'*'         TEST ALL OFFICE                              
         BNE   VOFF04                                                           
         CLI   FVILEN,1            TO MAKE SURE THEY ONLY ENTER 1 *             
         BNE   EXITNV                                                           
         CLI   TWAACCS,0           TEST LIMIT ACCESS OR LIST ACCESS             
         BE    EXITOK              NO - OK                                      
         MVC   FVMSGNO,=AL2(AE$AOFNA)                                           
         B     EXITL               ALL OFFICES NOT ALLOWED                      
*                                                                               
VOFF04   MVC   DRTKOFF,FVIFLD      SEARCH FOR THE OFFICE CODE.                  
         TM    BCCPYST4,CPYSOFF2   TEST 2CO?                                    
         BZ    VOFF08                                                           
         CLI   TWAACCS,0           TEST LIMIT ACCESS OR LIST ACCESS             
         BE    VOFF08              NO - OK                                      
         CLC   FVIFLD(2),TWAACCS+2                                              
         BE    EXITOK              OK - IT'S LIMIT OR LIST ACCESS               
*                                                                               
VOFF08   GOTO1 ATSTOFF,FVIFLD      TEST OFFICE CODE                             
         BE    EXITOK                                                           
         OC    FVMSGNO,FVMSGNO     IF MESSAGE ALREADY SET USE IT                
         BNZ   EXITL                                                            
         MVC   FVMSGNO,=AL2(AE$IVOFF)                                           
         B     EXITL               INVALID OFFICE                               
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  CLC   DRTKOFF,BCSPACES                                                 
         BNE   DFOFF02                                                          
         CLI   CSACT,A#LST         LIST ACTION?                                 
         BNE   DFOFF02                                                          
         MVI   FVIFLD,C'*'         ALL OFFICE ENTRIES                           
         B     EXITOK                                                           
DFOFF02  MVC   FVIFLD(L'DRTKOFF),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE CODE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
VFLTOFF  CLI   FVILEN,1                                                         
         BNE   VFOFF01                                                          
         TM    BCCPYST4,CPYSOFF2   TEST 2CO?                                    
         BZ    VFOFF02                                                          
         MVC   FVMSGNO,=AL2(AE$FLDTS)                                           
         B     EXITL               INPUT FIELD TOO SHORT                        
*                                                                               
VFOFF01  CLI   FVILEN,2                                                         
         BNE   EXITNV                                                           
         TM    BCCPYST4,CPYSOFF2   TEST 2CO?                                    
         BO    VFOFF02                                                          
         MVC   FVMSGNO,=AL2(AE$FLDTL)                                           
         B     EXITL               INPUT FIELD TOO LONG                         
*                                                                               
VFOFF02  MVC   DRTKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'DRTKOFF),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLI   DRTKOFF,C' '        IS THERE AN OFFICE TO COMPARE?               
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   DRTKOFF,FLTIFLD     COMPARE OFFICE WITH FILTER DATA              
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON OFFICE CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHOFF  GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,OFFUL,ACOM,    C        
               (X'11',0)                                                        
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EFFECTIVE DATE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
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
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALEDT)                                
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
DISEDT   MVC   SDRTDATE,DRTKDATE   DRTKDATE = COMPLEMENT OF DATE                
         XC    SDRTDATE,EFFS                                                    
         GOTO1 VDATCON,BODMCB,(1,SDRTDATE),(17,FVIFLD)                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EFFECTIVE DATE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALEDT   LLC   RF,FVXLEN                                                        
         EXCLC RF,FVIFLD,UC@TODAY  TODAY                                        
         BNE   VEDT02                                                           
         MVC   DRTKDATE,BCTODAYP                                                
         XC    DRTKDATE,EFFS                                                    
         MVC   EFFDATE,BCTODAYP                                                 
         B     EXITOK                                                           
*                                                                               
VEDT02   GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         BZ    EXITL               INVAILD DATE                                 
*                                                                               
         GOTO1 VDATCON,BODMCB,BODUB1,(1,EFFDATE)                                
*                                                                               
         MVC   DRTKDATE,EFFDATE    DRTKDATE = COMPLEMENT OF DATE                
         XC    DRTKDATE,EFFS                                                    
         MVC   FLTIFLD(L'EFFDATE),EFFDATE  STORE IN FILTER FIELD TOO            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EFFECTIVE DATE FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTEDT  DS    0H                                                               
         GOTO1 VDATCON,BODMCB,(1,FLTIFLD),(17,FVIFLD)                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR EFFECTIVE DATE                                     *         
***********************************************************************         
         SPACE 1                                                                
DOFTEDT  DS    0H                                                               
         MVC   SDRTDATE,DRTKDATE                                                
         XC    SDRTDATE,EFFS       DRTKDATE = COMPLEMENT OF DATE                
         CLC   SDRTDATE,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFFICE NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFNDTA   LA    RF,OFNTBL                                                        
         B     ITER                                                             
*                                                                               
OFNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISOFN   CLC   DRTKOFF,BCSPACES                                                 
         BNH   EXITOK                                                           
         MVC   IOKEY,BCSPACES                                                   
         TM    BCCPYST4,CPYSOFF2                                                
         BZ    DOFN04                                                           
T        USING OFFRECD,IOKEY       FOR NEW OFFICE SYSTEM                        
         MVI   T.OFFKTYP,OFFKTYPQ  SET UP OFFICE RECORD                         
         MVC   T.OFFKCPY,DRTKCPY                                                
         MVC   T.OFFKOFF,DRTKOFF                                                
         B     DISOFNX                                                          
         DROP  T                                                                
T        USING ACTRECD,IOKEY                                                    
DOFN04   MVC   T.ACTKCPY,DRTKCPY   SET UP ACCOUNT RECORD                        
         MVC   T.ACTKUNT(L'OFFUL),OFFUL                                         
         MVC   T.ACTKACT(1),DRTKOFF                                             
         DROP  T                                                                
DISOFNX  LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2             A(OFFICE RECORD)                             
         GOTO1 AGETNAM                                                          
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR VEHICLE TYPE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
VTYDTA   LA    RF,VTYTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
VTYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISVTY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVTY)                                 
*        DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHVTY)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY VEHICLE TYPE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISVTY   MVC   FVIFLD(L'TLKVEHT),TLKVEHT                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE VEHICLE TYPE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALVTY   MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITEMS         MAX OF 500 LINES                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'TLKVEHT                                                 
         BL    EXITSHRT            FIELD TOO SHORT                              
         CLI   FVILEN,L'TLKVEHT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         XC    SVEFFDAT,SVEFFDAT                                                
         MVC   TLKVEHT,FVIFLD                                                   
*                                                                               
T        USING VEHRECD,IOKEY                                                    
         XC    T.VEHKEY,T.VEHKEY                                                
         MVI   T.VEHKTYP,VEHKTYPQ  VEHICLE RECORD TYPE                          
         MVI   T.VEHKSUB,VEHKSUBQ  VEHICLE SUB-RECORD TYPE                      
         MVC   T.VEHKCPY,CUABIN    CONNECTED ID                                 
         L     RF,AIOREC                                                        
         MVC   T.VEHKOFF,DRTKOFF-DRTRECD(RF)                                    
         MVC   T.VEHKDATE,DRTKDATE-DRTRECD(RF)                                  
         MVC   SDRTDATE,DRTKDATE-DRTRECD(RF)                                    
         XC    SDRTDATE,EFFS                                                    
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         B     *+8                                                              
VVTY04   LHI   R1,XOSEQ+XOACCDIR+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         CLC   T.VEHKEY(VEHKDATE-VEHRECD),IOKEYSAV                              
         BNE   EXITNV              VEHICLE RECORD NOT FOUND                     
         OC    SVEFFDAT,SVEFFDAT                                                
         BZ    *+14                                                             
         CLC   T.VEHKDATE,SVEFFDAT                                              
         BNE   EXITNV              DIFFERENT EFFECTIVE DATE                     
         MVC   SVEFFDAT,T.VEHKDATE                                              
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST                                   
*                                                                               
         L     RF,AIO2                                                          
         AHI   RF,VEHRFST-VEHKEY                                                
         USING FFTELD,RF                                                        
         SR    R1,R1                                                            
VVTY08   CLI   FFTEL,0             RECORD END?                                  
         BE    VVTY04                                                           
         CLI   FFTEL,FFTELQ        FFTEL?                                       
         BNE   VVTY12              NO                                           
         CLI   FFTTYPE,FFTTVEH     VEHICLE?                                     
         BNE   VVTY12                                                           
         OC    FFTVCUT,FFTVCUT     ANY CUTOFF DATE ?                            
         BZ    *+14                NO - OK                                      
         CLC   FFTVCUT,SDRTDATE    CHECK CUTOFF DATE                            
         BL    VVTY12                                                           
         CLC   FFTVEHC,TLKVEHT                                                  
         BE    EXITOK              VEHICLE CODE FOUND                           
*                                                                               
VVTY12   IC    R1,FFTLN                                                         
         AR    RF,R1                                                            
         B     VVTY08                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A VEHICLE TYPE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING DRTRECD,R4                                                       
SRCHVTY  L     R4,AIOREC           A(DRTRECD)                                   
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,VEHCODE,ACOM,  C        
               DRTKOFF,DRTKDATE                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         POP   USING                                                            
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
* DATA OBJECT FOR FUEL TYPE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
FTYDTA   LA    RF,FTYTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
FTYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFTY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFTY)                                 
*        DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFTY)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FUEL TYPE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISFTY   MVC   FVIFLD(L'TLKFUET),TLKFUET                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FUEL TYPE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALFTY   OC    TLKVEHT,TLKVEHT                                                  
         BZ    EXITOK                                                           
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   TLKFUET,BCSPACES                                                 
         B     EXITOK                                                           
*                                                                               
         CLI   FVILEN,L'TLKFUET                                                 
         BL    EXITSHRT            FIELD TOO SHORT                              
         CLI   FVILEN,L'TLKFUET                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         XC    SVEFFDAT,SVEFFDAT                                                
         MVC   TLKFUET,FVIFLD                                                   
*                                                                               
T        USING FUERECD,IOKEY                                                    
         XC    T.FUEKEY,T.FUEKEY                                                
         MVI   T.FUEKTYP,FUEKTYPQ  FUEL RECORD TYPE                             
         MVI   T.FUEKSUB,FUEKSUBQ  FUEL SUB-RECORD TYPE                         
         MVC   T.FUEKCPY,CUABIN    CONNECTED ID                                 
         L     RF,AIOREC                                                        
         MVC   T.FUEKOFF,DRTKOFF-DRTRECD(RF)                                    
         MVC   T.FUEKDATE,DRTKDATE-DRTRECD(RF)                                  
         MVC   SDRTDATE,DRTKDATE-DRTRECD(RF)                                    
         XC    SDRTDATE,EFFS                                                    
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         B     *+8                                                              
VFTY04   LHI   R1,XOSEQ+XOACCDIR+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         CLC   T.FUEKEY(FUEKDATE-FUERECD),IOKEYSAV                              
         BNE   EXITNV              FUEL RECORD NOT FOUND                        
         OC    SVEFFDAT,SVEFFDAT                                                
         BZ    *+14                                                             
         CLC   T.FUEKDATE,SVEFFDAT                                              
         BNE   EXITNV              DIFFERENT EFFECTIVE DATE                     
         MVC   SVEFFDAT,T.FUEKDATE                                              
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST                                   
*                                                                               
         L     RF,AIO2                                                          
         AHI   RF,FUERFST-FUEKEY                                                
         USING FFTELD,RF                                                        
         SR    R1,R1                                                            
VFTY08   CLI   FFTEL,0             RECORD END?                                  
         BE    VFTY04                                                           
         CLI   FFTEL,FFTELQ        FFTEL?                                       
         BNE   VFTY12              NO                                           
         CLI   FFTTYPE,FFTTFUE     FUEL?                                        
         BNE   VFTY12                                                           
         OC    FFTFCUT,FFTFCUT     ANY CUTOFF DATE ?                            
         BZ    *+14                NO - OK                                      
         CLC   FFTFCUT,SDRTDATE    CHECK CUTOFF DATE                            
         BL    VFTY12                                                           
         CLC   FFTFUEC,TLKFUET                                                  
         BE    EXITOK              FUEL CODE FOUND                              
*                                                                               
VFTY12   IC    R1,FFTLN                                                         
         AR    RF,R1                                                            
         B     VFTY08                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A FUEL TYPE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING DRTRECD,R4                                                       
SRCHFTY  L     R4,AIOREC           A(DRTRECD)                                   
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,FUECODE,ACOM,  C        
               DRTKOFF,DRTKDATE                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         POP   USING                                                            
         EJECT ,                                                                
*&&                                                                             
*&&UK                                                                           
***********************************************************************         
* DATA OBJECT FOR ENGINE SIZE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ESZDTA   LA    RF,ESZTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
ESZTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISESZ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALESZ)                                 
*        DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHESZ)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ENGINE SIZE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISESZ   MVC   FVIFLD(L'TLKENGS),TLKENGS                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ENGINE SIZE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALESZ   OC    TLKVEHT,TLKVEHT                                                  
         BZ    EXITOK                                                           
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   TLKENGS,BCSPACES                                                 
         B     EXITOK                                                           
*                                                                               
         CLI   FVILEN,L'TLKENGS                                                 
         BL    EXITSHRT            FIELD TOO SHORT                              
         CLI   FVILEN,L'TLKENGS                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         XC    SVEFFDAT,SVEFFDAT                                                
         MVC   TLKENGS,FVIFLD                                                   
*                                                                               
T        USING ENGRECD,IOKEY                                                    
         XC    T.ENGKEY,T.ENGKEY                                                
         MVI   T.ENGKTYP,ENGKTYPQ  ENGINE RECORD TYPE                           
         MVI   T.ENGKSUB,ENGKSUBQ  ENGINE SUB-RECORD TYPE                       
         MVC   T.ENGKCPY,CUABIN    CONNECTED ID                                 
         L     RF,AIOREC                                                        
         MVC   T.ENGKOFF,DRTKOFF-DRTRECD(RF)                                    
         MVC   T.ENGKDATE,DRTKDATE-DRTRECD(RF)                                  
         MVC   SDRTDATE,DRTKDATE-DRTRECD(RF)                                    
         XC    SDRTDATE,EFFS                                                    
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         B     *+8                                                              
VESZ04   LHI   R1,XOSEQ+XOACCDIR+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         CLC   T.ENGKEY(ENGKDATE-ENGRECD),IOKEYSAV                              
         BNE   EXITNV              ENGINE RECORD NOT FOUND                      
         OC    SVEFFDAT,SVEFFDAT                                                
         BZ    *+14                                                             
         CLC   T.ENGKDATE,SVEFFDAT                                              
         BNE   EXITNV              DIFFERENT EFFECTIVE DATE                     
         MVC   SVEFFDAT,T.ENGKDATE                                              
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST                                   
*                                                                               
         L     RF,AIO2                                                          
         AHI   RF,ENGRFST-ENGKEY                                                
         USING FFTELD,RF                                                        
         SR    R1,R1                                                            
VESZ08   CLI   FFTEL,0             RECORD END?                                  
         BE    VESZ04                                                           
         CLI   FFTEL,FFTELQ        FFTEL?                                       
         BNE   VESZ12              NO                                           
         CLI   FFTTYPE,FFTTENG     ENGINE?                                      
         BNE   VESZ12                                                           
         OC    FFTECUT,FFTECUT     ANY CUTOFF DATE ?                            
         BZ    *+14                NO - OK                                      
         CLC   FFTECUT,SDRTDATE    CHECK CUTOFF DATE                            
         BL    VESZ12                                                           
         CLC   FFTENGC,TLKENGS                                                  
         BE    EXITOK              ENGINE CODE FOUND                            
*                                                                               
VESZ12   IC    R1,FFTLN                                                         
         AR    RF,R1                                                            
         B     VESZ08                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A ENGINE SIZE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING DRTRECD,R4                                                       
SRCHESZ  L     R4,AIOREC           A(DRTRECD)                                   
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,ENGCODE,ACOM,  C        
               DRTKOFF,DRTKDATE                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         POP   USING                                                            
         EJECT ,                                                                
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR DISTANCE BAND                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
DBDDTA   LA    RF,DBDTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
DBDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDBD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDBD)                                 
*        DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHDBD)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISTANCE BAND FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISDBD   MVC   FVIFLD(L'TLKDISB),TLKDISB                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DISTANCE BAND FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALDBD   OC    TLKVEHT,TLKVEHT                                                  
         BZ    EXITOK                                                           
                                                                                
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   TLKDISB,BCSPACES                                                 
         B     EXITOK                                                           
*                                                                               
         CLI   FVILEN,L'TLKDISB                                                 
         BL    EXITSHRT            FIELD TOO SHORT                              
         CLI   FVILEN,L'TLKDISB                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         XC    SVEFFDAT,SVEFFDAT                                                
         MVC   TLKDISB,FVIFLD                                                   
*                                                                               
T        USING DTNRECD,IOKEY                                                    
         XC    T.DTNKEY,T.DTNKEY                                                
         MVI   T.DTNKTYP,DTNKTYPQ  DISTANCE RECORD TYPE                         
         MVI   T.DTNKSUB,DTNKSUBQ  DISTANCE SUB-RECORD TYPE                     
         MVC   T.DTNKCPY,CUABIN    CONNECTED ID                                 
         L     RF,AIOREC                                                        
         MVC   T.DTNKOFF,DRTKOFF-DRTRECD(RF)                                    
         MVC   T.DTNKDATE,DRTKDATE-DRTRECD(RF)                                  
         MVC   SDRTDATE,DRTKDATE-DRTRECD(RF)                                    
         XC    SDRTDATE,EFFS                                                    
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         B     *+8                                                              
VDBD04   LHI   R1,XOSEQ+XOACCDIR+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         CLC   T.DTNKEY(DTNKDATE-DTNRECD),IOKEYSAV                              
         BNE   EXITNV              DISTANCE RECORD NOT FOUND                    
         OC    SVEFFDAT,SVEFFDAT                                                
         BZ    *+14                                                             
         CLC   T.DTNKDATE,SVEFFDAT                                              
         BNE   EXITNV              DIFFERENT EFFECTIVE DATE                     
         MVC   SVEFFDAT,T.DTNKDATE                                              
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCMST                                   
*                                                                               
         L     RF,AIO2                                                          
         AHI   RF,DTNRFST-DTNKEY                                                
         USING FFTELD,RF                                                        
         SR    R1,R1                                                            
VDBD08   CLI   FFTEL,0             RECORD END?                                  
         BE    VDBD04                                                           
         CLI   FFTEL,FFTELQ        FFTEL?                                       
         BNE   VDBD12              NO                                           
         CLI   FFTTYPE,FFTTDIS     DISTANCE?                                    
         BNE   VDBD12                                                           
         OC    FFTDCUT,FFTDCUT     ANY CUTOFF DATE ?                            
         BZ    *+14                NO - OK                                      
         CLC   FFTDCUT,SDRTDATE    CHECK CUTOFF DATE                            
         BL    VDBD12                                                           
         CLC   FFTDISC,TLKDISB                                                  
         BE    EXITOK              DISTANCE CODE FOUND                          
*                                                                               
VDBD12   IC    R1,FFTLN                                                         
         AR    RF,R1                                                            
         B     VDBD08                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A DISTANCE BAND FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING DRTRECD,R4                                                       
SRCHDBD  L     R4,AIOREC           A(DRTRECD)                                   
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,DISCODE,ACOM,  C        
               DRTKOFF,DRTKDATE                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR GROSS RATE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CRTDTA   LA    RF,CRTTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
CRTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCRT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCRT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY GROSS RATE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISCRT   OC    TLCRORT,TLCRORT     ANY GROSS RATE?                              
         BZ    EXITOK                                                           
*&&UK*&& CURED TLCRORT,(10,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                     
*&&US*&& CURED TLCRORT,(10,FVIFLD),3,ALIGN=LEFT,DMCB=BOPARM                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE GROSS RATE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALCRT   OC    TLKVEHT,TLKVEHT                                                  
         BZ    EXITOK                                                           
*                                                                               
         ZAP   TLCRORT,BCPZERO     SET DEFAULT TO ZERO                          
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    EXITOK                                                           
*                                                                               
*&&UK*&& GOTO1 VCASHVAL,BODMCB,(X'80',FVIFLD),(X'C0',(RF))                      
*&&US*&& GOTO1 VCASHVAL,BODMCB,(X'83',FVIFLD),(X'80',(RF))                      
         CLI   0(R1),0                 VALID NUMBER?                            
         BE    *+14                                                             
*&&UK*&& MVC   FVMSGNO,=AL2(AE$IVRAT)  INVALID RATE                             
*&&US*&& MVC   FVMSGNO,=AL2(AE$MAXNM)                                           
         B     EXITL                                                            
                                                                                
         ZAP   TLCRORT,BODMCB+6(6)                                              
         BNL   EXITOK                                                           
*&&UK*&& MVC   FVMSGNO,=AL2(AE$IVRAT)  INVALID RATE                             
*&&US*&& MVC   FVMSGNO,=AL2(AE$MAXNM)                                           
         B     EXITL                                                            
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR VAT RATE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
VRTDTA   LA    RF,VRTTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
VRTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISVRT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVRT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY VAT RATE                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISVRT   OC    TLVATRT,TLVATRT     ANY VAT RATE?                                
         BZ    EXITOK                                                           
         CURED TLVATRT,(10,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE VAT RATE                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALVRT   OC    TLKVEHT,TLKVEHT                                                  
         BZ    EXITOK                                                           
*                                                                               
         ZAP   TLVATRT,BCPZERO     SET DEFAULT TO ZERO                          
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    EXITOK                                                           
*                                                                               
         GOTO1 VCASHVAL,BODMCB,(X'80',FVIFLD),(X'C0',(RF))                      
         CLI   0(R1),0                 VALID NUMBER?                            
         BE    *+14                                                             
*&&UK*&& MVC   FVMSGNO,=AL2(AE$IVRAT)  INVALID RATE                             
*&&US*&& MVC   FVMSGNO,=AL2(AE$MAXNM)                                           
         B     EXITL                                                            
                                                                                
         ZAP   TLVATRT,BODMCB+6(6)                                              
         BNL   EXITOK                                                           
*&&UK*&& MVC   FVMSGNO,=AL2(AE$IVRAT)  INVALID RATE                             
*&&US*&& MVC   FVMSGNO,=AL2(AE$MAXNM)                                           
         B     EXITL                                                            
         POP   USING                                                            
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
* DATA OBJECT FOR PASSENGER RATE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PRTDTA   LA    RF,PRTTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
PRTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PASSENGER RATE                                              *         
***********************************************************************         
         SPACE 1                                                                
DISPRT   OC    TLPASRT,TLPASRT     ANY PASSENGER RATE?                          
         BZ    EXITOK                                                           
         CURED TLPASRT,(10,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE VAT RATE                                                   *         
***********************************************************************         
         SPACE 1                                                                
VALPRT   OC    TLKVEHT,TLKVEHT                                                  
         BZ    EXITOK                                                           
*                                                                               
         ZAP   TLPASRT,BCPZERO     SET DEFAULT TO ZERO                          
         SR    RF,RF                                                            
         ICM   RF,1,FVILEN                                                      
         BZ    EXITOK                                                           
*                                                                               
         GOTO1 VCASHVAL,BODMCB,(X'80',FVIFLD),(X'C0',(RF))                      
         CLI   0(R1),0                 VALID NUMBER?                            
         BE    *+14                                                             
*&&UK*&& MVC   FVMSGNO,=AL2(AE$IVRAT)  INVALID RATE                             
*&&US*&& MVC   FVMSGNO,=AL2(AE$MAXNM)                                           
         B     EXITL                                                            
                                                                                
         ZAP   TLPASRT,BODMCB+6(6)                                              
         BNL   EXITOK                                                           
*&&UK*&& MVC   FVMSGNO,=AL2(AE$IVRAT)  INVALID RATE                             
*&&US*&& MVC   FVMSGNO,=AL2(AE$MAXNM)                                           
         B     EXITL                                                            
         POP   USING                                                            
         EJECT ,                                                                
*&&                                                                             
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
         CLI   SREC,R#DRT          DISRATE TYPE RECORD                          
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
         NI    SNINDS1,FF-SNIUSECR TURN OFF USE CURRENT RECORD BIT              
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
         USING DRTRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING DRTRECD,R2                                                       
LAST     USING DRTRECD,R3                                                       
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
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING DRTRECD,IOKEY                                                    
FLST     MVC   X.DRTKEY,THIS.DRTKEY                                             
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
NLST02   CLC   X.DRTKEY(DRTKREM-DRTRECD),THIS.DRTKEY                            
         BNE   EXITL                                                            
*                                                                               
         CLI   X.DRTKSEQ,0         FIRST DISRATE RECORD                         
         BNE   NLST                NO - GET THE NEXT ONE                        
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
NLST06   L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    NLST10              NO - LIST ALL RECORDS                        
         CLC   X.DRTKOFF,BCSPACES  GLOBAL RECORD?                               
         BE    NLST                YES, DON'T LIST IT                           
         GOTO1 ATSTOFF,X.DRTKOFF   TEST OFFICE SECURITY                         
         BE    NLST08                                                           
*                                                                               
         MVC   FVXTRA,BCSPACES                                                  
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                 KEEP THE SEQUENCE                            
         B     NLST                                                             
*                                                                               
NLST08   L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                 KEEP THE SEQUENCE                            
*                                                                               
NLST10   MVC   THIS.DRTKEY(ACCKLEN),IOKEY                                       
         B     EXITOK                                                           
         DROP  LAST,X                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSTSAR                                                  
         OI    LSSTAT2,LSSNOSEQ+LSSADD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,DRTRFST-DRTRECD                                               
         STH   RF,MNTDISP                                                       
         MVI   READSEQ#,0                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
* AIO5 -> NEXT DISRATE RECORD IF THE FIRST ONE IS NOT BIG ENOUGH      *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST DISRATE RECORD?                        
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    R5,R1               A(RECORD)                                    
         CR    R5,R1               MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    R5,DRTRFST-DRTRECD(,R5) IT IS NOW.                               
*                                                                               
         B     NML02                                                            
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
NLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST DISRATE RECORD?                        
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    R5,R1               A(RECORD)                                    
         CR    R5,R1               MAKE SURE MNTDISP INITIALISED                
         BH    NML10                                                            
         LA    R5,DRTRFST-DRTRECD(,R5) IT IS NOW.                               
*                                                                               
         USING LIDELD,R5                                                        
NML02    CLI   LIDEL,0             RECORD END?                                  
         BNE   NML04               NO                                           
         BAS   RE,READNXT          READ NEXT DISRATE RECORD                     
         BNE   EXITL               NO MORE RECORD                               
         LHI   RF,VEHRFST-VEHRECD  CURRENT DISPLACEMENT INTO RECORD             
         STH   RF,MNTDISP                                                       
         L     R5,AIO5                                                          
         AR    R5,RF               NEW RECORD IN AIO5                           
*                                                                               
NML04    CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML06               NO                                           
         CLI   LIDTYPE,LIDTDRTL    DISRATE LIST?                                
         BE    NML08                                                            
                                                                                
NML06    LLC   RE,LIDLN                                                         
         AR    R5,RE                                                            
         B     NML02                                                            
*                                                                               
NML08    LA    R4,LIDDATA          ADDRESS OF DATA START OF ELEMENT             
         SR    R4,R5                                                            
         STH   R4,CURDISP          CURRENT DISPLACEMENT INTO ELEMENT            
         XR    R4,R4                                                            
         IC    R4,LIDITLN          GET LENGTH OF DATA                           
         STH   R4,DATALEN          SAVE THIS LENGTH                             
         IC    R4,LIDLN                                                         
         STH   R4,TOTELLN          SAVE TOTAL ELEMENT LENGTH                    
         B     NML12                                                            
*                                                                               
NML10    LH    R4,DATALEN          LENGTH OF DATA                               
         AH    R4,CURDISP          CURRENT DISPLACEMENT TO DATA                 
         STH   R4,CURDISP          SAVE NEXT DISPLACEMENT TO DATA               
         CH    R4,TOTELLN          HAVE WE REACHED END OF ELEMENT               
         BNL   NML06               YES                                          
*                                                                               
NML12    L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST DISRATE RECORD?                        
         BE    *+8                                                              
         L     R1,AIO5                                                          
                                                                                
         SR    R5,R1                                                            
         STH   R5,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         LH    RF,MNTDISP                                                       
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST DISRATE RECORD?                        
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    RF,R1               A(ELEMENT)                                   
         AH    RF,CURDISP          CURRENT DISPLACEMENT INTO THE ELEMEN         
         USING LIDDATA,RF          MOVE IN DETAILS FROM ELEMENT                 
         MVC   TLKVEHT,LIDRVEHT                                                 
         MVC   TLKFUET,LIDRFUET                                                 
         MVC   TLKENGS,LIDRENGS                                                 
         MVC   TLKDISB,LIDRDISB                                                 
         MVC   TLCRORT,LIDRGRSR                                                 
         MVC   TLVATRT,LIDRVATR                                                 
         MVC   TLPASRT,LIDRPASR                                                 
         B     EXITOK                                                           
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 MVI   ANYLINES,NO                                                      
         MVI   ADDSEQ#,0                                                        
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),0                
*                                                                               
         L     RF,AIOREC                                                        
         MVC   IOKEY(L'DRTKEY),0(RF)                                            
*                                                                               
T        USING DRTRECD,R4                                                       
UPDF104  LA    R4,IOKEY            READ NEXT DISRATE SUB-RECORD                 
         SR    RF,RF               THEY ARE ALWAYS IN SEQUENCE                  
         IC    RF,T.DRTKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.DRTKSEQ                                                     
*                                                                               
         L     R1,=AL4(XOHIUPD+XOACCDIR+XIO2)                                   
         GOTO1 AIO                                                              
         CLC   T.DRTKEY(DRTKSEQ-DRTRECD),IOKEYSAV                               
         BNE   EXITOK              EXIT - NOT SUB-RECORD                        
         TM    IOERR,FF-IOEDEL                                                  
         BZ    *+6                                                              
         DC    H'0'                ERROR READING THE RECORD                     
*                                                                               
         OI    T.DRTKSTAT,DRTSDELT DELETE DISRATE DIR                           
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         TM    T.DRTRSTAT,DRTSDELT RECORD DELETED ?                             
         BO    UPDF108                                                          
         OI    T.DRTRSTAT,DRTSDELT DELETE MASTER RECORD                         
*                                                                               
UPDF108  GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIO2),0                  
*                                                                               
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
         B     UPDF104                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
* AIO6 = A(DISRATE RECORD) IF ADDSEQ# IS NOT 0                        *         
***********************************************************************         
         SPACE 1                                                                
         USING DRTRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
*                                                                               
         MVI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         LM    R2,R3,SVPARMS3                                                   
         B     EXITOK                                                           
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
         BNE   ULST1ER1            NO - OK                                      
*                                                                               
T        USING LIDELD,BOELEM       BUILD NEW ELEMENT                            
         USING DRTRECD,R2                                                       
         L     R2,AIOREC                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   T.LIDEL,LIDELQ                                                   
         MVI   T.LIDTYPE,LIDTDRTL                                               
         MVI   T.LIDITLN,LIDRLNQ                                                
*                                                                               
         LA    R4,T.LIDDATA                                                     
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
ULST110  LA    R1,TSANXT           DEAL WITH ALL DELETE REQUEST                 
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    ULST120             END OF FILE                                  
         CLC   TLKSES,SESNL        CHECK NEST LEVEL                             
         BNE   ULST120             DONE ALL FOR THIS LEVEL                      
*                                                                               
         USING LIDDATA,R4                                                       
         MVC   LIDRVEHT,TLKVEHT                                                 
         MVC   LIDRFUET,TLKFUET                                                 
         MVC   LIDRENGS,TLKENGS                                                 
         MVC   LIDRDISB,TLKDISB                                                 
         MVC   LIDRGRSR,TLCRORT                                                 
         MVC   LIDRVATR,TLVATRT                                                 
         MVC   LIDRPASR,TLPASRT                                                 
         DROP  R4                                                               
*                                                                               
         LLC   RE,T.LIDITLN                                                     
         AR    R4,RE               R4=NEXT DATA BLOCK ON ELEMENT                
         LA    RF,T.LIDELD                                                      
         LR    R1,R4                                                            
         SR    R1,RF               TOTAL DISPLACEMENT OF ELEMENT                
*                                                                               
         LHI   RF,L'BOELEM-1                                                    
         SR    RF,RE                                                            
         CR    R1,RF               ENOUGH SPACE TO STORE THE NEXT ELEM          
         BL    ULST110             YES - OK                                     
*                                                                               
         STC   R1,T.LIDLN                                                       
         GOTO1 ADDREC,BOPARM,DRTRECD                                            
         BNE   ULST1ER2                                                         
         CLI   ADDSEQ#,0                                                        
         BE    *+8                                                              
         L     R2,AIO6             NEW DRTREC                                   
*                                                                               
         XC    BOELEM,BOELEM                                                    
         MVI   T.LIDEL,LIDELQ                                                   
         MVI   T.LIDTYPE,LIDTDRTL                                               
         MVI   T.LIDITLN,LIDRLNQ                                                
         LA    R4,T.LIDDATA                                                     
         B     ULST110                                                          
*                                                                               
ULST120  LA    RF,T.LIDELD                                                      
         SR    R4,RF               LENGTH OF ELEMENT                            
         CHI   R4,LIDLNDQ                                                       
         BNH   ULST124             NO ITEM TO UPDATE                            
*                                                                               
         STC   R4,T.LIDLN                                                       
         GOTO1 ADDREC,BOPARM,DRTRECD                                            
         BNE   ULST1ER2                                                         
ULST124  CLI   ADDSEQ#,0           TEST MAIN RECORD                             
         BE    EXITOK                                                           
         GOTO1 UPDREC              UPDATE DRTREC SUB-RECORD                     
         B     EXITOK                                                           
*                                                                               
ULST1ER1 MVC   FVMSGNO,=AL2(AE$NLINE)                                           
         LH    RF,LS1STLIN                                                      
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         CLI   CSACT,A#ADD                                                      
         BE    EXITL                                                            
         NI    LSLTIND1,FF-LSLTIBLD REBUILD THE LIST                            
         XC    GCLASKEY,GCLASKEY    SET KEY HAS BEEN CHANGED                    
         NI    GSINDSL1,FF-GSIXMNT  TURN OF MAINT SCREEN LOADED FLAG            
         B     EXITL                                                            
*                                                                               
ULST1ER2 OI    CSINDSG1,CSINDUNW   SET TO UNWIND VIA $ABEND                     
         B     EXITL                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO READ THE NEXT DISRATE RECORD                             *         
* EXIT - READSEQ# : NEXT RECORD SEQUENCE NUMBER                       *         
*      - MNTDISP: DISPLACEMENT TO THE FIRST ELEMENT                   *         
***********************************************************************         
         SPACE 1                                                                
T        USING DRTRECD,IOKEY                                                    
READNXT  NTR1  ,                                                                
         L     RF,AIOREC           A(CURRENCT DISRATE RECORD)                   
         MVC   T.DRTKEY,0(RF)                                                   
         SR    RF,RF                                                            
         IC    RF,READSEQ#                                                      
         AHI   RF,1                                                             
         STC   RF,READSEQ#                                                      
         STC   RF,T.DRTKSEQ                                                     
*                                                                               
         LHI   R1,XOREAD+XOACCMST+XIO5                                          
         GOTO1 AIO                                                              
         BNE   EXITL               NO MORE DISRATE RECORD                       
         LA    RF,DRTRFST-DRTRECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* ADD DRTRECD RECORD                                                  *         
*                                                                     *         
* NTRY - P1     = A(CURRENT DRTRECD RECORD)                           *         
*      - BOELEM = CURRENT DISRATE ELEMENT                             *         
* EXIT - ADDSEQ# : NEXT RECORD SEQUENCE NUMBER                        *         
***********************************************************************         
         SPACE 1                                                                
         USING DRTRECD,R2                                                       
ADDREC   NTR1  ,                                                                
         L     R2,0(R1)            A(CURRENCT DRTRECD RECORD)                   
*                                                                               
ADDREC04 SR    RF,RF                                                            
         ICM   RF,3,DRTRLEN                                                     
*                                                                               
         LLC   R0,BOELEM+LIDLN-LIDELD CHECK IF RECORD+ELM LENGTH > 2K           
         AR    RF,R0                                                            
*                                                                               
         CHI   RF,IOMAXLNQ         GREATER MAX RECORD ALLOWED ?                 
         BNH   ADDREC16            NO - ADD IT INTO CURRENT RECORD              
*                                                                               
         CLI   ADDSEQ#,0           MAIN DRTRECD RECORD ?                        
         BE    ADDREC08                                                         
         GOTO1 UPDREC              NO - UPDATE SAVED DRTREC IN AIO6             
*                                                                               
ADDREC08 L     R2,AIO6             R2=A(NEW DRTRECD SUB-RECORD)                 
         L     RF,AIOREC                                                        
         MVC   DRTKEY(DRTRLNK-DRTRECD),0(RF)                                    
         LLC   RF,ADDSEQ#                                                       
         AHI   RF,1                                                             
         STC   RF,ADDSEQ#          NEXT SEQUENCE NUMBER                         
         STC   RF,DRTKSEQ                                                       
         CLI   DRTKSEQ,MAXRECSQ    TEST MAX DRTRECD RECORDS REACH               
         BNL   ADDRECL             YES - TOO MANY LINES                         
                                                                                
         MVC   IOKEY(L'DRTKEY),DRTKEY                                           
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO6)                                   
         GOTO1 AIO                                                              
         BE    ADDREC12                                                         
         TM    IOERR,IOEDEL                                                     
         BNZ   ADDREC12                                                         
         TM    IOERR,IOERNF                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RECFLAG,RECFADD     ADD NEW DRTRECD RECORD                       
         LA    RF,DRTRFST                                                       
         MVI   0(RF),0                                                          
         SR    RF,R2                                                            
         AHI   RF,1                                                             
         STCM  RF,3,DRTRLEN        LENGTH OF DRTRECD RECORD                     
         MVC   DRTKSTA,GSRECSTA    COPY STATUS FROM MAIN RECORD                 
         MVI   RECFLAG,1                                                        
         MVC   IOKEY(L'DRTKEY),DRTKEY                                           
         B     ADDREC16                                                         
*                                                                               
ADDREC12 MVI   RECFLAG,RECFPUT     PUT DRTRECD RECORD                           
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO6)                                  
         GOTO1 AIO                                                              
         BE    ADDREC04                                                         
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
ADDREC16 MVC   DRTRSTA,GSRECSTA    COPY STATUS FROM MAIN RECORD                 
         GOTO1 VHELLO,BCPARM,(C'P',GCFILNAM),DRTRECD,BOELEM                     
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
ADDRECL  MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               SOMETHING WRONG!!!                           
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO UPDATE DRTRECD RECORD                                    *         
* ETRY - AIO6: A(CURRENT RECORD) TO BE UPDATED                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING DRTRECD,R4                                                       
UPDREC   NTR1  ,                                                                
         LA    R4,IOKEY                                                         
         L     RF,AIO6                                                          
         CLI   RECFLAG,0           DO WE WANT TO UPDATE DRTRECD RECORD          
         BE    EXITOK              NO - EXIT                                    
         MVC   T.DRTKEY,0(RF)                                                   
         CLI   RECFLAG,RECFPUT     PUT RECORD?                                  
         BNE   UPDREC10                                                         
*                                                                               
         LHI   R1,XOPUTREC+XOACCMST+XIO6 CHANGE DRTRECD RECORD                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         NI    T.DRTKSTAT,FF-DRTSDELT    CHANGE NON DELETED DRTREC DIR          
         LHI   R1,XOWRITE+XOACCDIR+XIO6                                         
         GOTO1 AIO                                                              
         BE    UPDREC20                                                         
         DC    H'0'                                                             
*                                                                               
UPDREC10 LHI   R1,XOADDREC+XOACCMST+XIO6 ADD DIR + FILE RECORDS                 
         GOTO1 AIO                                                              
         BE    UPDREC20                                                         
         DC    H'0'                ERROR - CAN'T PUT RECORD BACK                
*                                                                               
UPDREC20 MVI   RECFLAG,0                                                        
         L     RF,AIOREC           RESTORE IOADDR FOR CONTROLLER                
         ST    RF,IOADDR                                                        
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
VEHCODE  DC    C'VEH'                                                           
FUECODE  DC    C'FUE'                                                           
ENGCODE  DC    C'ENG'                                                           
DISCODE  DC    C'DIS'                                                           
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
*                                                                               
OFFUL    DC    C'2D'                                                            
EFFS     DC    8XL1'FF'                                                         
*                                                                               
DCLISTU  DS    0D                                                               
         DCDDL AC#TODAY,L'UC@TODAY,L                                            
DCLISTUX DC    AL1(EOT)                                                         
*                                                                               
DCLISTL  DS    0D                                                               
DCLISTLX DC    AL1(EOT)                                                         
*                                                                               
MAXRECSQ EQU   255                                                              
MAXITEMS EQU   500                                                              
IOMAXLNQ EQU   2000                                                             
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
SVEFFDAT DS    PL3                                                              
SDRTDATE DS    PL3                                                              
*                                                                               
READSEQ# DS    XL(L'DRTKSEQ)                                                    
ADDSEQ#  DS    XL(L'DRTKSEQ)                                                    
RECFLAG  DS    XL1                                                              
RECFADD  EQU   1                                                                
RECFPUT  EQU   2                                                                
*                                                                               
SESNL    DS    XL1                                                              
ANYLINES DS    CL1                                                              
DSLISTU  DS    0C                                                               
UC@TODAY DS    CL10                                                             
*                                                                               
DSLISTL  DS    0C                                                               
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT ,                                                                
***********************************************************************         
* SAVED DSECT                                                         *         
***********************************************************************         
         SPACE 2                                                                
SAVED    DSECT                                                                  
MNTDISP  DS    H                   MOVE TO SAVED STORAGE                        
CURDISP  DS    H                   CURRENT DISPLACENT INTO ELEMENT              
DATALEN  DS    H                   LENGTH OF DATA                               
TOTELLN  DS    H                   TOTAL LENGTH OF ELEMENT                      
*                                                                               
EFFDATE  DS    PL3                 SAVE EFFECTIVE DATE (PWOS)                   
SAVEDX   EQU   *-SAVED                                                          
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKVEHT  DS    CL(L'LIDRVEHT)      VEHICLE TYPE                                 
TLKFUET  DS    CL(L'LIDRFUET)      FUEL TYPE                                    
TLKENGS  DS    CL(L'LIDRENGS)      ENGINE SIZE                                  
TLKDISB  DS    CL(L'LIDRDISB)      DISTANCE BAND                                
         ORG   TLUSER                                                           
TLCRORT  DS    PL6                 GROSS RATE                                   
TLVATRT  DS    PL6                 VAT RATE                                     
TLPASRT  DS    PL6                 PASSENGER RATE                               
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACFIL5B   02/26/19'                                      
         END                                                                    
