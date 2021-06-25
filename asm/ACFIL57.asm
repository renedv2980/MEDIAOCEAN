*          DATA SET ACFIL57    AT LEVEL 003 AS OF 09/26/12                      
*PHASE T62357B,*                                                                
                                                                                
         TITLE 'VEHICLE TYPE RECORD'                                            
*                                                                               
* YNGX 000 16FEB11 <PR001547> NEW VERSION                                       
*                                                                               
FIL57    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL57**,R7,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
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
         USING VEHRECD,R2                                                       
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
KFKVAL   XC    VEHKEY,VEHKEY       INITIALIZE KEY OF RECORD                     
         MVI   VEHKTYP,VEHKTYPQ    VEHICLE RECORD TYPE                          
         MVI   VEHKSUB,VEHKSUBQ    VEHICLE SUB-RECORD TYPE                      
         MVC   VEHKOFF,BCSPACES                                                 
         MVC   VEHKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    VEHKEY,VEHKEY       INITIALIZE KEY OF RECORD                     
         MVI   VEHKTYP,VEHKTYPQ    VEHICLE RECORD TYPE                          
         MVI   VEHKSUB,VEHKSUBQ    VEHICLE SUB-RECORD TYPE                      
         MVC   VEHKCPY,CUABIN      CONNECTED ID                                 
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
         USING VEHRECD,R2                                                       
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
RFTABL   DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    OC    GCRACADD,GCRACADD                                                
         BZ    RFDEL10             NO - ACTIVITY ADD DATE!!                     
         LA    RF,GCRACADD                                                      
         CLC   BCTODAYP,RACDATE-RACELD(RF)                                      
         BE    EXITOK              OK - IF ADDED TODAY                          
*                                                                               
RFDEL10  LH    RF,GSDSPACT         DISPLACEMENT TO ACTION FIELD                 
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO ACTION FIELD                   
         MVC   FVMSGNO,=AL2(AE$CDREC)                                           
         B     EXITL               CAN'T DELETE RECORD                          
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE                                                                  
RECLAST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE                                                                  
RLADD    DS    0H                                                               
         GOTO1 ADDPAS,BOPARM,VEHRECD                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE ALL VEHICLE SUB-RECORDS        *         
***********************************************************************         
         SPACE 2                                                                
T        USING VEHRECD,R4                                                       
RLDEL    DS    0H                                                               
         GOTO1 DELPAS,BOPARM,VEHRECD                                            
         MVC   IOKEY(L'VEHKEY),VEHKEY                                           
*                                                                               
RLDEL10  LA    R4,IOKEY            READ NEXT VEHICLE SUB-RECORD                 
         SR    RF,RF               THEY ARE ALWAYS IN SEQUENCE                  
         IC    RF,T.VEHKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.VEHKSEQ                                                     
*                                                                               
         L     R1,=AL4(XORDUP+XOACCDIR+XIO2)                                    
         GOTO1 AIO                                                              
         BNE   EXITOK              DELETE OR NO FOUND - END                     
*                                                                               
         OI    T.VEHKSTAT,VEHSDELT DELETE VEHICLE DIR                           
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
         OI    T.VEHRSTAT,VEHSDELT DELETE MASTER RECORD                         
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
*                                                                               
         GOTO1 DELPAS,BOPARM,AIO2                                               
         B     RLDEL10                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE ALL VEHICLE SUB-RECORDS       *         
***********************************************************************         
         SPACE 2                                                                
T        USING VEHRECD,R4                                                       
RLRES    DS    0H                                                               
         GOTO1 ADDPAS,BOPARM,VEHRECD                                            
         MVC   IOKEY(L'VEHKEY),VEHKEY                                           
*                                                                               
RLRES10  LA    R4,IOKEY            READ NEXT VEHICLE SUB-RECORD                 
         SR    RF,RF               THEY ARE ALWAYS IN SEQUENCE                  
         IC    RF,T.VEHKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.VEHKSEQ                                                     
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
         NI    T.VEHKSTAT,FF-VEHSDELT     RESTORE IT                            
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
         NI    T.VEHRSTAT,FF-VEHSDELT                                           
         LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                                                              
*                                                                               
         GOTO1 ADDPAS,BOPARM,AIO2                                               
         B     RLRES10                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE                                                                  
T        USING VEHRECD,R4                                                       
RLWRT    DS    0H                                                               
         GOTO1 ADDPAS,BOPARM,VEHRECD                                            
         MVC   IOKEY(L'VEHKEY),VEHKEY                                           
*                                                                               
RLWRT04  LA    R4,IOKEY            READ NEXT VEHICLE SUB-RECORD                 
         LLC   RF,T.VEHKSEQ        THEY ARE ALWAYS IN SEQUENCE                  
         AHI   RF,1                                                             
         STC   RF,T.VEHKSEQ                                                     
*                                                                               
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK              DELETE OR NO FOUND - END                     
*                                                                               
         GOTO1 ADDPAS,BOPARM,AIO2                                               
         B     RLWRT04                                                          
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
         USING VEHRECD,R2                                                       
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
         USING VEHRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(F#VEH#OFFC),AL4(OFFDTA)    OFFICE CODE                       
         DC    AL2(F#VEH#EFFDT),AL4(EDTDTA)   EFFECTIVE DATE                    
         DC    AL2(F#VEH#OFFNM),AL4(OFNDTA)   OFFICE NAME                       
         DC    AL2(F#VEH#VEHST),AL4(STADTA)   VEHICLE STATUS                    
         DC    AL2(F#VEH#VEHCD),AL4(CODDTA)   VEHICLE CODE                      
         DC    AL2(F#VEH#VEHNM),AL4(NAMDTA)   VEHICLE NAME                      
         DC    AL2(F#VEH#CUTOF),AL4(CDTDTA)   CUTOFF DATE                       
         DC    AL2(F#VEH#ACCUM),AL4(ACUDTA)   ACCUMULATOR                       
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL57    CSECT                                                                  
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
         MVI   VEHINDS,0           CLEAR INDICATOR                              
         CLI   CSACT,A#CHA                                                      
         BNE   EXITOK                                                           
                                                                                
         OI    VEHINDS,VEHLAXD     SET LIMITED AMENDMENTS ALLOWED               
         OC    GCRACADD,GCRACADD                                                
         BZ    EXITOK              NO - ACTIVITY ADD DATE!!                     
         LA    RF,GCRACADD                                                      
         CLC   BCTODAYP,RACDATE-RACELD(RF)                                      
         BNE   EXITOK                                                           
         NI    VEHINDS,FF-VEHLAXD  NO RESTRICTION IF ADDED TODAY                
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
DISOFF   CLC   VEHKOFF,BCSPACES                                                 
         BNE   *+12                                                             
         MVI   FVIFLD,C'*'         ALL OFFICE ENTRIES                           
         B     EXITOK                                                           
         MVC   FVIFLD(L'VEHKOFF),VEHKOFF                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TWAD,R4                                                          
VALOFF   L     R4,ATWA                                                          
*                                                                               
         MVC   VEHKOFF,BCSPACES                                                 
         CLI   FVIFLD,C'*'         TEST ALL OFFICE                              
         BNE   VOFF04                                                           
         CLI   FVILEN,1            TO MAKE SURE THEY ONLY ENTER 1 *             
         BNE   EXITNV                                                           
         CLI   TWAACCS,0           TEST LIMIT ACCESS OR LIST ACCESS             
         BE    EXITOK              NO - OK                                      
         MVC   FVMSGNO,=AL2(AE$AOFNA)                                           
         B     EXITL               ALL OFFICES NOT ALLOWED                      
*                                                                               
VOFF04   MVC   VEHKOFF,FVIFLD      SEARCH FOR THE OFFICE CODE.                  
         TM    BCCPYST4,CPYSOFF2   TEST 2CO?                                    
         BZ    VOFF08                                                           
         CLI   TWAACCS,0           TEST LIMIT ACCESS OR LIST ACCESS             
         BE    VOFF08              NO - OK                                      
         CLC   FVIFLD(2),TWAACCS+2                                              
         BE    EXITOK              OK - IT'S LIMIT OR LIST ACCESS               
*                                                                               
VOFF08   GOTO1 ATSTOFF,FVIFLD      TEST OFFICE CODE                             
         BE    EXITOK                                                           
         OC    FVMSGNO,FVMSGNO     IF MESSAGE ALREADY SET THEN USE IT           
         BNZ   EXITL                                                            
         MVC   FVMSGNO,=AL2(AE$IVOFF)                                           
         B     EXITL               INVALID OFFICE                               
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  CLC   VEHKOFF,BCSPACES                                                 
         BNE   DFOFF02                                                          
         CLI   CSACT,A#LST         LIST ACTION?                                 
         BNE   DFOFF02                                                          
         MVI   FVIFLD,C'*'         ALL OFFICE ENTRIES                           
         B     EXITOK                                                           
DFOFF02  MVC   FVIFLD(L'VEHKOFF),FLTIFLD                                        
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
VFOFF02  MVC   VEHKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'VEHKOFF),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLI   VEHKOFF,C' '        IS THERE AN OFFICE TO COMPARE?               
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   VEHKOFF,FLTIFLD     COMPARE OFFICE WITH FILTER DATA              
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON OFFICE CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHOFF  GOTO1 VACSRCHC,BOPARM,(3,FVADDR),ATWA,OFFUL,ACOM,(X'11',0)             
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
DISEDT   MVC   SVEHDATE,VEHKDATE   VEHKDATE = COMPLEMENT OF DATE                
         XC    SVEHDATE,EFFS                                                    
         GOTO1 VDATCON,BODMCB,(1,SVEHDATE),(17,FVIFLD)                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EFFECTIVE DATE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALEDT   LLC   RF,FVXLEN                                                        
         EXCLC RF,FVIFLD,UC@TODAY  TODAY                                        
         BNE   VEDT02                                                           
         MVC   VEHKDATE,BCTODAYP                                                
         XC    VEHKDATE,EFFS                                                    
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
         MVC   VEHKDATE,EFFDATE    VEHKDATE = COMPLEMENT OF DATE                
         XC    VEHKDATE,EFFS                                                    
         MVC   FLTIFLD(L'EFFDATE),EFFDATE   MOVE INTO FILTER FIELD TOO          
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
         MVC   SVEHDATE,VEHKDATE                                                
         XC    SVEHDATE,EFFS       VEHKDATE = COMPLEMENT OF DATE                
         CLC   SVEHDATE,FLTIFLD                                                 
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
DISOFN   CLC   VEHKOFF,BCSPACES                                                 
         BNH   EXITOK                                                           
         MVC   IOKEY,BCSPACES                                                   
         TM    BCCPYST4,CPYSOFF2                                                
         BZ    DOFN04                                                           
T        USING OFFRECD,IOKEY       FOR NEW OFFICE SYSTEM                        
         MVI   T.OFFKTYP,OFFKTYPQ  SET UP OFFICE RECORD                         
         MVC   T.OFFKCPY,VEHKCPY                                                
         MVC   T.OFFKOFF,VEHKOFF                                                
         B     DISOFNX                                                          
         DROP  T                                                                
T        USING ACTRECD,IOKEY                                                    
DOFN04   MVC   T.ACTKCPY,VEHKCPY   SET UP ACCOUNT RECORD                        
         MVC   T.ACTKUNT(L'OFFUL),OFFUL                                         
         MVC   T.ACTKACT(1),VEHKOFF                                             
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
* DATA OBJECT FOR STATUS                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
STADTA   LA    RF,STATBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
STATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTA)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY STATUS FIELD                                               *          
***********************************************************************         
         SPACE 1                                                                
DISSTA   MVC   FVIFLD(L'NOS),NOS   SET FIELD TO NNNN                            
         CLI   TLVSTAT,0                                                        
         BE    EXITOK              YES - NOTHING TO DO                          
*                                                                               
         TM    TLVSTAT,FFTVSAPA    ALLOWS PASSENGERS ?                          
         BZ    *+10                                                             
         MVC   FVIFLD(1),BC@YES                                                 
         TM    TLVSTAT,FFTVSRFT    REQUIRES FUEL TYPE ?                         
         BZ    *+10                                                             
         MVC   FVIFLD+1(1),BC@YES                                               
         TM    TLVSTAT,FFTVSRES    REQUIRES ENGINE SIZE ?                       
         BZ    *+10                                                             
         MVC   FVIFLD+2(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE STATUS FIELD                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSTA   MVI   TLVSTAT,0                                                        
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
                                                                                
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   *+12                                                             
         OI    TLVSTAT,FFTVSAPA    SET ALLOWS PASSENGERS                        
         B     VSTA02                                                           
         CLI   FVIFLD,C' '                                                      
         BE    VSTA02                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BNE   EXITNV                                                           
*                                                                               
VSTA02   CLC   FVIFLD+1(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLVSTAT,FFTVSRFT    SET REQUIRES FUEL TYPE                       
         B     VSTA04                                                           
         CLI   FVIFLD+1,C' '                                                    
         BE    VSTA04                                                           
         CLC   FVIFLD+1(1),BC@NO                                                
         BE    VSTA04                                                           
         MVI   FVERRNDX,1          SET CURSOR TO STATUS 2                       
         B     EXITNV                                                           
*                                                                               
VSTA04   CLC   FVIFLD+2(1),BC@YES                                               
         BNE   *+12                                                             
         OI    TLVSTAT,FFTVSRES    SET REQUIRES ENGINE SIZE                     
         B     VALSTAX                                                          
         CLI   FVIFLD+2,C' '                                                    
         BE    VALSTAX                                                          
         CLC   FVIFLD+2(1),BC@NO                                                
         BE    VALSTAX                                                          
         MVI   FVERRNDX,2          SET CURSOR TO STATUS 3                       
         B     EXITNV                                                           
*                                                                               
VALSTAX  DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR VEHICLE CODE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CODDTA   LA    RF,CODTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
CODTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY VEHICLE CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISCOD   MVC   FVIFLD(L'TLKCODE),TLKCODE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE VEHICLE CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALCOD   OI    TLINDS,TLNEWEL                                                   
         CLC   TLKCODE,BCSPACES                                                 
         BNH   *+8                                                              
         NI    TLINDS,FF-TLNEWEL   EXISTING ELEMENT                             
*                                                                               
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   VCOD10                                                           
         XC    TLKCODE,TLKCODE                                                  
         TM    TLINDS,TLNEWEL      NEW ELEMENT ?                                
         BO    *+12                YES - OK                                     
         TM    VEHINDS,VEHLAXD     ELEMENT ADDED TODAY ?                        
         BO    *+12                NO - SHOW ERROR                              
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CDLIN)                                           
         B     EXITL               CAN'T DELETE THIS LINE                       
*                                                                               
VCOD10   LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITEMS         MAX OF 500 LINES                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,2                                                         
         BNE   EXITNV                                                           
         GOTO1 ACHKFLD,BOPARM,('CKDATAQ',CKTAB1Q)                               
         BNE   EXITNV              INVALID CHARS IN THE FIELD                   
         TM    TLINDS,TLNEWEL      NEW ELEMENT ?                                
         BO    VCOD20              YES - OK                                     
         TM    VEHINDS,VEHLAXD     ELEMENT ADDED TODAY ?                        
         BZ    VCOD20              YES - OK                                     
         CLC   TLKCODE,FVIFLD                                                   
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CCFLD)                                           
         B     EXITL               CAN'T CHANGE THIS FIELD                      
VCOD20   MVC   TLKCODE,FVIFLD                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR VEHICLE NAME                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
NAMDTA   LA    RF,NAMTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
NAMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNAM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY VEHICLE NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   MVC   FVIFLD(L'TLNAME),TLNAME                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE VEHICLE NAME                                               *         
***********************************************************************         
         SPACE 1                                                                
VALNAM   OC    TLKCODE,TLKCODE                                                  
         BZ    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO INPUT                             
         MVC   TLNAMEL,FVILEN      LENGTH OF NAME                               
         MVC   TLNAME,FVIFLD                                                    
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CUTOFF DATE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CDTDTA   LA    RF,CDTTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
CDTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCDT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCDT)                                 
*&&UK*&& DC    AL1(DHED),AL1(0,0,0),AL4(DHDCDT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CUTOFF DATE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISCDT   OC    TLCUTOFF,TLCUTOFF                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(1,TLCUTOFF),(17,FVIFLD),0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CUTOFF DATE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALCDT   OC    TLKCODE,TLKCODE                                                  
         BZ    EXITOK                                                           
*                                                                               
         MVC   BODUB2(L'TLCUTOFF),TLCUTOFF                                      
         XC    TLCUTOFF,TLCUTOFF                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         BZ    EXITL               INVAILD DATE                                 
*                                                                               
         GOTO1 VDATCON,BODMCB,BODUB1,(1,TLCUTOFF)                               
         CLC   TLCUTOFF,EFFDATE                                                 
         BNL   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CDAED)                                           
         B     EXITL               CUTOFF DATE MUST BE ON/AFTER EFFDATE         
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR ACCUMULATOR                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ACUDTA   LA    RF,ACUTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
ACUTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISACU)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALACU)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCUMULATOR                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISACU   OC    TLACCUM,TLACCUM                                                  
         BZ    EXITOK                                                           
         CLI   TLACCUM,0                                                        
         BE    EXITOK                                                           
         MVC   FVIFLD(L'TLACCUM),TLACCUM                                        
         OI    FVIFLD,X'F0'                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCUMULATOR                                                *         
***********************************************************************         
         SPACE 1                                                                
VALACU   OC    TLKCODE,TLKCODE                                                  
         BZ    EXITOK                                                           
*                                                                               
         XC    TLACCUM,TLACCUM                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM       IS IT A NUMERIC                              
         BZ    EXITNOTN            ERROR - NOT A NUMBER                         
         MVC   TLACCUM,FVIFLD                                                   
         NI    TLACCUM,X'0F'                                                    
         B     EXITOK                                                           
         POP   USING                                                            
*&&UK                                                                           
***********************************************************************         
* SET COLUMN HEADINGS TO NORMAL INTENSITY                             *         
***********************************************************************         
         SPACE 1                                                                
DHDCDT   L     R1,CALLR1                                                        
         ICM   RE,15,24(R1)        RE=A(HEADLINE 1 FIELD HEADER)                
         ICM   RF,15,28(R1)        RF=A(HEADLINE 2 FIELD HEADER)                
         NI    1(RE),FF-X'08'      SET TO NORMAL INTENSITY                      
         NI    1(RF),FF-X'08'      SAME FOR HEADLINE 2                          
         OI    6(RE),X'80'         AND TRANSMIT                                 
         OI    6(RF),X'80'                                                      
         B     EXITOK                                                           
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
         CLI   SREC,R#VEH          VEHICLE TYPE RECORD                          
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
         USING VEHRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING VEHRECD,R2                                                       
LAST     USING VEHRECD,R3                                                       
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
X        USING VEHRECD,IOKEY                                                    
FLST     MVC   X.VEHKEY,THIS.VEHKEY                                             
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
NLST02   CLC   X.VEHKEY(VEHKREM-VEHRECD),THIS.VEHKEY                            
         BNE   EXITL                                                            
*                                                                               
         CLI   X.VEHKSEQ,0         FIRST VEHICLE RECORD                         
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
         CLC   X.VEHKOFF,BCSPACES  GLOBAL RECORD?                               
         BE    NLST                YES, DON'T LIST IT                           
         GOTO1 ATSTOFF,X.VEHKOFF   TEST OFFICE SECURITY                         
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
NLST10   MVC   THIS.VEHKEY(ACCKLEN),IOKEY                                       
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
FTFLST1  LA    RF,VEHRFST-VEHRECD                                               
         STH   RF,MNTDISP                                                       
         MVI   READSEQ#,0                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
* AIO5 -> NEXT VEHICLE RECORD IF THE FIRST ONE IS NOT BIG ENOUGH      *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST VEHICLE RECORD?                        
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    RF,R1               A(RECORD)                                    
         CR    RF,R1               MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    RF,VEHRFST-VEHRECD(,RF) IT IS NOW.                               
         XR    RE,RE                                                            
*                                                                               
         USING FFTELD,RF                                                        
FML02    CLI   FFTEL,0             RECORD END?                                  
         BNE   FML04               NO                                           
         BAS   RE,READNXT          READ NEXT VEHICLE RECORD                     
         BNE   EXITL               NO MORE RECORD                               
         LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIO5             NEW RECORD IN AIO5                           
         XR    RE,RE                                                            
*                                                                               
FML04    CLI   FFTEL,FFTELQ        FFTEL?                                       
         BNE   NML06               NO                                           
         CLI   FFTTYPE,FFTTVEH     VEHICLE?                                     
         BNE   NML06                                                            
                                                                                
FML08    L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST VEHICLE RECORD?                        
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         SR    RF,R1                                                            
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
NLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST VEHICLE RECORD?                        
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    RF,R1               A(RECORD)                                    
         XR    RE,RE                                                            
         CR    RF,R1               MAKE SURE MNTDISP INITIALISED                
         BH    NML06                                                            
         LA    RF,VEHRFST-VEHRECD(,RF) IT IS NOW.                               
*                                                                               
         USING FFTELD,RF                                                        
NML02    CLI   FFTEL,0             RECORD END?                                  
         BNE   NML04               NO                                           
         BAS   RE,READNXT          READ NEXT VEHICLE RECORD                     
         BNE   EXITL               NO MORE RECORD                               
         LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIO5             NEW RECORD IN AIO5                           
         XR    RE,RE                                                            
*                                                                               
NML04    CLI   FFTEL,FFTELQ        FFTEL?                                       
         BNE   NML06               NO                                           
         CLI   FFTTYPE,FFTTVEH     VEHICLE?                                     
         BE    NML08                                                            
                                                                                
NML06    IC    RE,FFTLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML08    L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST VEHICLE RECORD?                        
         BE    *+8                                                              
         L     R1,AIO5                                                          
                                                                                
         SR    RF,R1                                                            
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
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST VEHICLE RECORD?                        
         BE    *+8                                                              
         L     R1,AIO5                                                          
                                                                                
         AR    RF,R1               A(RECORD)                                    
         USING FFTELD,RF           MOVE IN DETAILS FROM ELEMENT                 
         MVC   TLVSTAT,FFTVSTAT                                                 
         MVC   TLKCODE,FFTVEHC                                                  
         MVC   TLCUTOFF,FFTVCUT                                                 
         MVC   TLACCUM,FFTVEHAC                                                 
         CLI   FFTLN,FFTVLNQ                                                    
         BNH   EXITOK                                                           
         SR    RE,RE                                                            
         IC    RE,FFTLN                                                         
         SHI   RE,FFTVLNQ                                                       
         CHI   RE,L'TLNAME                                                      
         BNH   *+8                                                              
         LHI   RE,L'TLNAME         USE MAX. LENGTH                              
         STC   RE,TLNAMEL                                                       
         BCTR  RE,0                                                             
         EXMVC RE,TLNAME,FFTVEHN                                                
         B     EXITOK                                                           
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING VEHRECD,R2                                                       
UPDFRST1 MVI   ANYLINES,NO                                                      
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         MVI   ADDSEQ#,0                                                        
         L     R2,AIOREC                                                        
         B     UPDF10                                                           
*                                                                               
T        USING VEHRECD,IOKEY                                                    
UPDF02   CLI   VEHKSEQ,0                                                        
         BNE   UPDF04                                                           
         MVC   IOKEY(L'VEHKEY),VEHKEY                                           
         B     UPDF08                                                           
*                                                                               
UPDF04   CLI   VEHRFST,0           EMPTY RECORD                                 
         BNE   UPDF06                                                           
         OI    VEHRSTAT,VEHSDELT   DELETE MASTER RECORD                         
*                                                                               
         TM    T.VEHKSTAT,VEHSDELT                                              
         BO    UPDF06              DIR HAS ALREADY BEEN DELETED                 
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)                                   
         GOTO1 AIO                                                              
         TM    IOERR,FF-IOEDEL                                                  
         BZ    *+6                                                              
         DC    H'0'                ERROR READING THE RECORD                     
         OI    T.VEHKSTAT,VEHSDELT DELETE VEHICLE DIR                           
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
*                                                                               
UPDF06   LHI   R1,XOPUTREC+XOACCMST+XIO2                                        
         GOTO1 AIO                 UPDATE THE RECORD                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDF08   SR    RF,RF               READ NEXT XDATA SUB-RECORD                   
         IC    RF,T.VEHKSEQ        THEY ARE ALWAYS IN SEQUENCE                  
         AHI   RF,1                                                             
         STC   RF,T.VEHKSEQ                                                     
*                                                                               
         L     R1,=AL4(XOHID+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         CLC   T.VEHKEY(VEHKSEQ-VEHRECD),IOKEYSAV                               
         BNE   EXITOK              EXIT - NOT SUB-RECORD                        
         TM    IOERR,FF-IOEDEL                                                  
         BZ    *+6                                                              
         DC    H'0'                ERROR READING THE RECORD                     
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                  BAD MASTER RECORD                          
         L     R2,AIO2                                                          
*                                                                               
UPDF10   DS    0H                                                               
         GOTO1 DELPAS,BOPARM,VEHRECD DELETE PASSIVE POINTERS                    
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('FFTELQ',VEHRECD),0               
         B     UPDF02                                                           
         DROP  T,R2                                                             
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
* AIO6 = A(VEHICLE RECORD) IF ADDSEQ# IS NOT 0                        *         
***********************************************************************         
         SPACE 1                                                                
         USING VEHRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
*                                                                               
         MVI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         LM    R2,R3,SVPARMS3                                                   
         CLI   ADDSEQ#,0           FIRST VEHICLE RECORD?                        
         BE    *+8                                                              
         L     R2,AIO6             R4 = NEW VEHICLE RECORD                      
*                                                                               
T        USING FFTELD,BOELEM       BUILD NEW ELEMENT                            
         XC    BOELEM,BOELEM                                                    
         MVI   T.FFTEL,FFTELQ                                                   
         MVI   T.FFTLN,FFTVLNQ                                                  
         MVI   T.FFTDLEN,FFTVLNQ-(FFTDATA-FFTELD)                               
         MVI   T.FFTTYPE,FFTTVEH                                                
*                                                                               
         MVC   T.FFTVSTAT,TLVSTAT                                               
         MVC   T.FFTVEHC,TLKCODE                                                
         MVC   T.FFTVCUT,TLCUTOFF                                               
         MVC   T.FFTVEHAC,TLACCUM                                               
         SR    RF,RF                                                            
         ICM   RF,1,TLNAMEL          LENGTH OF VEHICLE NAME                     
         BZ    UREC108                                                          
         BCTR  RF,0                                                             
         EXMVC RF,T.FFTVEHN,TLNAME                                              
         AHI   RF,FFTVLNQ+1                                                     
         STC   RF,T.FFTLN            LENGTH OF ELEMENT                          
         SHI   RF,FFTDATA-FFTELD                                                
         STC   RF,T.FFTDLEN          LENGTH OF DATA                             
         DROP  T                                                                
*                                                                               
UREC108  SR    RF,RF                                                            
         ICM   RF,3,VEHRLEN                                                     
         CHI   RF,IOMAXLNQ         GREATER MAX RECORD ALLOWED ?                 
         BNH   UREC110             NO - ADD IT INTO CURRENT RECORD              
*                                                                               
         CLI   ADDSEQ#,0           FIRST VEHICLE RECORD ?                       
         BE    *+8                                                              
         BAS   RE,ADDREC           NO - ADD SAVED VEHICLE REC IN AIO6           
*                                                                               
         L     R2,AIO6             R2=A(NEW VEHICLE SUB-RECORD)                 
         L     RF,AIOREC                                                        
         MVC   VEHKEY(L'VEHKEY+L'VEHRLEN+L'VEHRSTA),0(RF)                       
         SR    RF,RF                                                            
         IC    RF,ADDSEQ#                                                       
         AHI   RF,1                                                             
         STC   RF,ADDSEQ#          NEXT SEQUENCE NUMBER                         
         STC   RF,VEHKSEQ                                                       
*                                                                               
         LA    RE,VEHRFST                                                       
         MVI   0(RE),0                                                          
         SR    RE,R2                                                            
         AHI   RE,1                                                             
         STCM  RE,3,VEHRLEN        LENGTH OF VEHICLE RECORD                     
*                                                                               
UREC110  GOTO1 AADDEL,BOPARM,(R2)  ADD NEW VEHICLE ELEMENT                      
         BE    EXITOK                                                           
         DC    H'0'                ERROR ADDING ELEMENT                         
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
         BNE   ULAST104            NO - OK                                      
         CLI   ADDSEQ#,0           FIRST VEHICLE RECORD ?                       
         BE    EXITOK                                                           
         BAS   RE,ADDREC           NO - ADD SAVED VEHICLE REC IN AIO6           
*                                                                               
         L     RF,AIOREC           RESTORE IOADDR FOR CONTRALLER                
         ST    RF,IOADDR                                                        
         B     EXITOK                                                           
*                                                                               
ULAST104 MVC   FVMSGNO,=AL2(AE$NLINE)                                           
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
* UPDATE VEHICLE PASSIVES                                             *         
*                                                                     *         
* NTRY - P1  = VEHICLE RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING VEHRECD,R3                                                       
ADDPAS   NTR1  ,                                                                
         L     R3,0(R1)                                                         
*                                                                               
         CLI   VEHKSEQ,0                                                        
         BNE   ADDPAS10            NO - IODA HAS ALREADY BEEN SET               
         MVC   IOKEY,0(R3)         GET IODA FOR PADDLE                          
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
ADDPAS10 XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'A',(R3)),(C'A',T.CPTRBLK),IODA,0,ACOM          
         B     EXITOK                                                           
         DROP  T,R3                                                             
         EJECT ,                                                                
***********************************************************************         
* DELETE VEHICLE PASSIVES                                             *         
*                                                                     *         
* NTRY - P1  = PID LIST ELEMENT                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING VEHRECD,R3                                                       
DELPAS   NTR1  ,                                                                
         L     R3,0(R1)            GET RECORD ADDRESS                           
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
         XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',T.CPTRBLK),0,0,ACOM             
         B     EXITOK                                                           
         DROP  T,R3                                                             
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO READ THE NEXT VEHICLE RECORD                             *         
* EXIT - READSEQ# : NEXT RECORD SEQUENCE NUMBER                       *         
*      - MNTDISP: DISPLACEMENT TO THE FIRST ELEMENT                   *         
***********************************************************************         
         SPACE 1                                                                
T        USING VEHRECD,IOKEY                                                    
READNXT  NTR1  ,                                                                
         L     RF,AIOREC           A(CURRENCT VEHICLE RECORD)                   
         MVC   T.VEHKEY,0(RF)                                                   
         SR    RF,RF                                                            
         IC    RF,READSEQ#                                                      
         AHI   RF,1                                                             
         STC   RF,READSEQ#                                                      
         STC   RF,T.VEHKSEQ                                                     
*                                                                               
         LHI   R1,XOREAD+XOACCMST+XIO5                                          
         GOTO1 AIO                                                              
         BNE   EXITL               NO MORE VEHICLE RECORD                       
         LA    RF,VEHRFST-VEHRECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD VEHICLE RECORD                                       *         
* EXIT - AIO6: A(NEW RECORD)                                          *         
***********************************************************************         
         SPACE 1                                                                
T        USING VEHRECD,R4                                                       
ADDREC   NTR1  ,                                                                
         LA    R4,IOKEY                                                         
         L     RF,AIO6                                                          
         MVC   T.VEHKEY,0(RF)                                                   
*                                                                               
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)   READ FOR UPDATE                 
         GOTO1 AIO                                                              
         BE    ADDREC10                                                         
         TM    IOERR,IOERRS-(IOEDEL+IOERNF)                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVIOERR,IOERR       SAVE IOERR                                   
         TM    IOERR,IOERNF                                                     
         BNO   ADDREC10                                                         
         MVC   IOKEY,IOKEYSAV      RESTORE KEY IF NOT FOUND                     
         B     ADDREC20                                                         
*                                                                               
ADDREC10 NI    T.VEHKSTAT,FF-VEHSDELT  SET DELETE OFF                           
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
ADDREC20 L     R0,AIO6             COPY FROM SUB-RECORD                         
         L     RE,AIO2             COPY TO SUB-RECORD                           
         LA    RF,IOAREALN         L'IOAREA                                     
         LR    R1,RF               LENGTH IS LENGTH OF RECORD                   
         MVCL  RE,R0                                                            
*                                                                               
         LHI   R1,XOADDREC+XOACCMST+XIO2   ADD DIR + FILE RECORDS               
         TM    SVIOERR,IOERNF                                                   
         BO    *+8                                                              
         LHI   R1,XOPUTREC+XOACCMST+XIO2   CHANGE VEHICLE RECORD                
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                ERROR - CAN'T PUT RECORD BACK                
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
MAXITEMS EQU   500                                                              
IOMAXLNQ EQU   1800                                                             
*                                                                               
OFFUL    DC    C'2D'                                                            
EFFS     DC    8XL1'FF'                                                         
NOS      DC    C'NNN'                                                           
DCLISTU  DS    0D                                                               
         DCDDL AC#TODAY,L'UC@TODAY,L                                            
DCLISTUX DC    AL1(EOT)                                                         
*                                                                               
DCLISTL  DS    0D                                                               
DCLISTLX DC    AL1(EOT)                                                         
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
SVEHDATE DS    PL3                                                              
VEHINDS  DS    XL1                                                              
VEHLAXD  EQU   X'80'               LIMIT AMENTMENTS ALLOWED                     
*                                                                               
READSEQ# DS    XL(L'VEHKSEQ)                                                    
ADDSEQ#  DS    XL(L'VEHKSEQ)                                                    
SVIOERR  DS    CL(L'IOERR)                                                      
*                                                                               
ANYLINES DS    CL1                                                              
MNTDISP  DS    H   MOVE TO SAVED STORAGE                                        
DSLISTU  DS    0C                                                               
UC@TODAY DS    CL10                                                             
*                                                                               
DSLISTL  DS    0C                                                               
*                                                                               
*                                                                               
CPTRWRK  DS    XL128                                                            
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT ,                                                                
***********************************************************************         
* SAVED DSECT                                                         *         
***********************************************************************         
         SPACE 2                                                                
SAVED    DSECT                                                                  
EFFDATE  DS    PL3                 SAVE EFFECTIVE DATE (PWOS)                   
SAVEDX   EQU   *-SAVED                                                          
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKCODE  DS    CL(L'FFTVEHC)       VEHICLE CODE                                 
         ORG   TLUSER                                                           
TLINDS   DS    XL1                 VEHICLE LOCAL INDICATOR                      
TLNEWEL  EQU   X'80'               NEW ELEMENT                                  
TLVSTAT  DS    XL1                 VEHICLE STATUS                               
TLCUTOFF DS    PL3                 CUTOFF DATE                                  
TLACCUM  DS    XL1                 ACCUMULATOR                                  
TLNAMEL  DS    XL1                 LENGTH OF VEHICLE NAME                       
TLNAME   DS    CL50                VEHICLE NAME                                 
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACFIL57   09/26/12'                                      
         END                                                                    
