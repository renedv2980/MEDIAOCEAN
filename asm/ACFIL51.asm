*          DATA SET ACFIL51    AT LEVEL 008 AS OF 08/10/11                      
*PHASE T62351C,*                                                                
*                                                                               
***********************************************************************         
*MPEN 005 <UKCR00021578> FIX FOR LIMIT ACCESS LOGONS                            
*MPEN 006 <LO01-9355> NEW FILTERING ON PID                                      
*MPEN 007 <LO01-9810> NEW MEDIA FIELD                                           
         SPACE 1                                                                
FIL51    TITLE 'TEAM LIST RECORD'                                               
         SPACE 2                                                                
FIL51    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL51**,RA,R7,RR=RE                                           
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         LH    R6,=Y(TWUSER-TWAD)                                               
         A     R6,ATWA                                                          
         USING SAVED,R6                                                         
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
         LA    R1,OVROUT1                                                       
         LA    R0,OVROUT1N                                                      
         XR    RE,RE                                                            
         LR    R2,RF                                                            
         L     RF,=A(OVROU1)                                                    
         A     RF,BORELO                                                        
INIT01   STCM  RE,1,0(R1)                                                       
         STCM  RF,7,1(R1)                                                       
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,INIT01                                                        
         LR    RF,R2                                                            
*                                                                               
         L     RE,ATWA                                                          
         MVC   SESNL,TWASESNL-TWAD(RE)                                          
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
FLTXX    MVI   SVPARMS,DFLTX       EXIT DEFINITELY NOT VALID                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     MVI   TEAINDS,0                                                        
*                                                                               
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
         DROP  RF,T                                                             
*                                                                               
         USING CPYRECD,R2                                                       
         LA    R2,IOKEY                                                         
         MVC   CPYKEY,BCSPACES                                                  
         MVC   CPYKCPY,CUABIN                                                   
         LHI   R1,XOREAD+XOACCMST+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                NO COMPANY RECORD?                           
         L     R2,AIO1             LOCATE COMPANY ELEMENT                       
         LA    RF,CPYRFST                                                       
         USING CPYEL,RF                                                         
         XR    R0,R0                                                            
INIT02   CLI   CPYEL,0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   CPYEL,CPYELQ        TEST COMPANY ELEMENT                         
         BE    *+14                                                             
         IC    R0,CPYLN                                                         
         AR    RF,R0                                                            
         B     INIT02                                                           
         MVC   SCPYALP,CPYALPHA    AGENCY ALPHA CODE                            
*                                                                               
         MVC   SAVOFF,BCSPACES                                                  
         TM    BCCPYSTC,CPYSROFF   COMPANY ENFORCE OFFICES                      
         BNO   EXITOK                                                           
         CLI   CUACCS,0                                                         
         BE    INIT10                                                           
         TM    BCCPYST4,CPYSOFF2                                                
         BO    INIT04                                                           
         CLI   CUACCS,C'*'                                                      
         BNE   INIT10                                                           
         MVC   SAVOFF(1),CUACCS+1     CONNECTED USER-ID                         
         B     INIT10                                                           
*                                                                               
INIT04   MVC   SAVOFF,CUACCS+2                                                  
*                                                                               
INIT10   MVI   GSSKCODE,C'A'                                                    
         MVI   GSSLCODE,C'A'                                                    
         MVI   GSSMCODE,C'A'                                                    
         B     EXITOK                                                           
         DROP  RF,R2                                                            
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
         LA    R1,SVPARMS                                                       
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
         USING TEARECD,R2                                                       
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
KEYFRST  L     R1,SVPARMS4         TABLE OF KNOWN INVOKERS                      
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
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
KFKVAL   XC    TEAKEY,TEAKEY       INITIALIZE KEY OF RECORD                     
         MVI   TEAKTYP,TEAKTYPQ                                                 
         MVI   TEAKSUB,TEAKSUBQ                                                 
         MVC   TEAKCPY,CUABIN      CONNECTED ID                                 
         MVC   TEAKOFF,BCSPACES                                                 
         MVC   TEAKOFF,SAVOFF                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    TEAKEY,TEAKEY       INITIALIZE KEY OF RECORD                     
         MVI   TEAKTYP,TEAKTYPQ                                                 
         MVI   TEAKSUB,TEAKSUBQ                                                 
         MVC   TEAKCPY,CUABIN      CONNECTED ID                                 
*                                                                               
         XC    SVFLTS(SVFLTLQ),SVFLTS                                           
         B     EXITOK                                                           
         EJECT ,                                                                
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
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  XC    CDOPTION,CDOPTION                                                
         GOTO1 AVALDOPT,0                                                       
         BL    EXITOK                                                           
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
         USING TEARECD,R2                                                       
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
* FIRST TIME FOR RECORD OBJECT - ADD OR COPY                          *         
***********************************************************************         
         SPACE 1                                                                
RFADD    CLI   CSACT,A#ADD                                                      
         BNE   *+12                                                             
         CLI   GSSMPAGE,1          ARE WE ON PID PAGE?                          
         BNE   EXITIACT            NO - INVALID ACTION FOR THIS SCREEN          
*                                                                               
         CLI   CSACT,A#CPY         COPY?                                        
         BNE   EXITOK                                                           
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),0                
         ORG   *-2                                                              
*                                  INCLUDE SEARCH ARG TO HELLO PARMLIST         
*                                  SO CORRECT LIDEL IS DELETED                  
         LA    R2,=AL1(L'SAPWDNUM,LIDTPID)                                      
         ST    R2,8(R1)            SET A(ARG)                                   
         MVI   8(R1),2             AND L'ARG                                    
         BASR  RE,RF               CALL HELLO                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    MVC   FVADDR,APIDFLD      SET CURSOR TO PID FIELD                      
         GOTO1 ADELRECS,BOPARM,TEARECD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RESTORE                              *         
***********************************************************************         
         SPACE 1                                                                
RFRES    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - WRITE                                *         
***********************************************************************         
         SPACE 1                                                                
RFWRT    GOTO1 ADELRECS,BOPARM,TEARECD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
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
         SPACE 1                                                                
RLADD    GOTO1 AADDRECS,BOPARM,TEARECD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE 1                                                                
RLDEL    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE                               *         
***********************************************************************         
         SPACE 1                                                                
RLRES    GOTO1 AADDRECS,BOPARM,TEARECD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE 1                                                                
RLWRT    GOTO1 AADDRECS,BOPARM,TEARECD                                          
         B     EXITOK                                                           
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
         USING TEARECD,R2                                                       
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
         USING TEARECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(F#TEAM#OFF),AL1(0),AL3(TOFDTA)   OFFICE/OFFICE LIST          
         DC    AL2(F#TEAM#OFNM),AL1(0),AL3(OFNDTA)  OFFICE NAME                 
         DC    AL2(F#TEAM#NUM),AL1(0),AL3(TMCDTA)   TEAM NUMBER                 
         DC    AL2(F#TEAM#PID),AL1(0),AL3(PIDDTA)   PID CODE                    
         DC    AL2(F#TEAM#FPID),AL1(0),AL3(FPIDDTA) PID FILTER                  
         DC    AL2(F#TEAM#FNAM),AL1(0),AL3(FNMDTA)  FIRST NAME                  
         DC    AL2(F#TEAM#LNAM),AL1(0),AL3(LNMDTA)  LAST NAME                   
         DC    AL2(F#TEAM#POFC),AL1(0),AL3(OFCDTA)  OFFICE CODE                 
         DC    AL2(F#TEAM#CPJC),AL1(0),AL3(CPJCDTA) CLI/PRO CODE                
         DC    AL2(F#TEAM#CPJN),AL1(0),AL3(CPJNDTA) CLI/PRO NAME                
         DC    AL2(F#TEAM#FCPJ),AL1(0),AL3(FCPJDTA) CLI/PRO FILTER              
         DC    AL2(F#TEAM#MEDL),AL1(0),AL3(MEDL)    MEDIA LIST                  
         DC    AL2(F#TEAM#MEDN),AL1(0),AL3(MEDND)   MEDIA NAME                  
         DC    AL2(F#TEAM#FMED),AL1(0),AL3(FMED)    MEDIA FILTER                
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL51    CSECT                                                                  
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
         MVC   SVTEA,TEAKNUM       SAVE TEAM NUMBER                             
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3                                                      
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   CLM   R2,B'0111',ATLST+1  ARE WE PROCESSING MAINTENANCE LIST?          
         BE    EXITOK              YES - DON'T UPDATE ACCOUNT RECORD            
*                                                                               
         B     EXITOK              NO - OK                                      
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR OFFICE/OFFICE LIST                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TOFDTA   LA    RF,TOFTBL                                                        
         B     ITER                                                             
*                                                                               
TOFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTOFF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTOFF)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISTOFF)                                
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETFOFF)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFOFF)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALTOFF)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFF)                                
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTOFF)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHOFF)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFOFF DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE/OFFICE LIST                                          *         
***********************************************************************         
         SPACE 1                                                                
DISTOFF  MVC   FVIFLD(L'TEAKOFF),TEAKOFF   OFFICE                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE/OFFICE LIST                                         *         
***********************************************************************         
         SPACE 1                                                                
VALTOFF  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         MVC   TEAKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'TEAKOFF),TEAKOFF                                       
                                                                                
         CLI   CSACT,A#DIS         DON'T TEST OFFICE CODE                       
         BE    EXITOK              IF DISPLAY                                   
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE CODE                             
         BNE   EXITL               INVALID OFFICE                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER ON OFFICE FIELD                                   *          
***********************************************************************         
         SPACE 1                                                                
DFLTFOFF MVC   FVIFLD(L'TEAKOFF),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR OFFICE                                              *         
***********************************************************************         
         SPACE 1                                                                
DDFTOFF  CLI   CUACCS,0                                                         
         BE    EXITNO                                                           
         TM    BCCPYST4,CPYSOFF2                                                
         BNO   DDFTOFF2                                                         
         MVC   FVIFLD(L'TEAKOFF),CUACCS+2                                       
         B     EXITOK                                                           
*                                                                               
DDFTOFF2 CLI   CUACCS,C'*'                                                      
         BNE   EXITOK                                                           
         MVC   FVIFLD(1),CUACCS+1                                               
         MVI   FVIFLD+1,X'40'                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON OFFICE                                   *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLC   TEAKOFF,FLTIFLD                                                  
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
* DATA OBJECT FOR DISPLAYING A OFFICE CODE NAME FIELD                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFNDTA   LA    RF,OFFNTBL          TABLE OF KNOWN VERBS                         
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
DISOFFN  OC    TEAKOFF,TEAKOFF     TEST ALL OFFICE                              
         BZ    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         TM    BCCPYST4,CPYSOFF2                                                
         BO    DISOFN02                                                         
         MVC   IOKEY,BCSPACES                 READ THE ACCOUNT RECORD           
         MVC   T.ACTKCPY,TEAKCPY              COMPANY                           
         MVC   T.ACTKUNT(L'OFFUL),OFFUL       UNIT/LEDGER                       
         MVC   T.ACTKACT(L'TEAKOFF),TEAKOFF   OFFICE CODE CODE                  
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET OFFICE CODE NAME                         
         B     EXITOK                                                           
*                                                                               
T        USING OFFRECD,IOKEY                                                    
DISOFN02 MVC   IOKEY,BCSPACES                 READ THE ACCOUNT RECORD           
         MVC   T.OFFKCPY,TEAKCPY              COMPANY                           
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKOFF,TEAKOFF           OFFICE CODE CODE                     
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
* DATA OBJECT FOR TEAM NUMBER                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TMCDTA   LA    RF,TMCTBL                                                        
         B     ITER                                                             
*                                                                               
TMCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTEAC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTEAC)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISTEAC)                                
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETTEAC)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTTEAC)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTTEAC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTEAC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETTEAC DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TEAM NUMBER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISTEAC  CURED TEAKNUM,(5,FVIFLD),0,ALIGN=LEFT,DMCB=BODMCB,ZERO=YES             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TEAM NUMBER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALTEAC  CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         GOTO1 ACHKFLD,BOPARM,('CKKEYQ',CKTAB1Q)                                
         BNE   EXITNV              INVALID CHARS IN THE FIELD                   
         TM    FVIIND,FVINUM       CHECK FVIFLD IS NUMERIC                      
         BNO   EXITNV                                                           
*                                                                               
         ZIC   RE,FVILEN                                                        
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  MYDUB,FVIFLD(0)                                                  
         CVB   RF,MYDUB                                                         
         STCM  RF,B'0011',TEAKNUM                                               
         STCM  RF,B'0011',SVTEA                                                 
         CLI   CSACT,A#ADD         SKIP CHECKING, IF ADDING OR COPYING          
         BE    EXITOK                                                           
         CLI   CSACT,A#RES         AND RESTORE                                  
         BE    EXITOK                                                           
         CLI   CSACT,A#CPY                                                      
         BE    EXITOK                                                           
         LA    R4,IOKEY                                                         
X        USING TEARECD,R4                                                       
         XC    X.TEAKEY,X.TEAKEY   READ ROLE RECORD                             
         MVI   X.TEAKTYP,TEAKTYPQ                                               
         MVI   X.TEAKSUB,TEAKSUBQ                                               
         MVC   X.TEAKCPY,CUABIN                                                 
         MVC   X.TEAKOFF,TEAKOFF                                                
         MVC   X.TEAKNUM,TEAKNUM                                                
         MVC   SVIOKEY,X.TEAKEY                                                 
         L     R1,=AL4(XORDD+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         BE    VTEAC02                                                          
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITNV                                                           
VTEAC02  MVC   FLTIFLD,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  X                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TEAM NUMBER FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTTEAC MVC   FVIFLD,FLTIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TEAM NUMBER FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTTEAC TM    FVIIND,FVINUM   CHECK FVIFLD IS NUMERIC                          
         BNO   EXITNV                                                           
         ZIC   RE,FVILEN                                                        
         AHI   RE,-1                                                            
         EXMVC RE,FLTIFLD,FVIFLD                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  MYDUB,FVIFLD(0)                                                  
         CVB   RF,MYDUB                                                         
         STCM  RF,B'0011',TEAKNUM                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON TEAM NUMBER                                         *         
***********************************************************************         
         SPACE 1                                                                
DOFTEAC  OC    TEAKNUM,TEAKNUM     HAVE WE A PERSONAL ID TO FILTER ON           
         BZ    FLTXX               NO - WE DON`T WANT IT THEN                   
         CVB   RF,MYDUB                                                         
         STCM  RF,B'0011',MYBINA                                                
         CLC   TEAKNUM,MYBINA                                                   
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR PID (PERSONAL ID)                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PIDDTA   LA    RF,PIDTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
PIDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPID)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAAPID)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHPID)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PID FIELD                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISPID   MVC   FVIFLD(L'TLKPID),TLKPID                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PERSONAL ID FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VAAPID   MVC   APIDFLD,FVADDR      SAVE A(FIELD)                                
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'TLKPID                                                  
         BH    EXITLONG            FIELD TOO LONG                               
         GOTOX ('VALPID',AGROUTS),FVIFLD                                        
         BNE   VPID04                                                           
*&&UK                                                                           
         GOTO1 ACHKLPID,BOPARM,BCWORK CHECK PID IS VALID FOR LOGON              
         BL    VPID04                                                           
*&&                                                                             
         MVC   TLKPID,FVIFLD                                                    
         MVC   TLKPIDB,BCWORK                                                   
         MVC   TLKPIDFN,BCWORK+2                                                
         MVC   TLKPIDLN,BCWORK+22                                               
         GOTOX ('GETPID',AGROUTS),TLKPIDB      CHECK BINARY MATCHES             
         CLC   BCWORK(L'SAPALPID),=C'????????'                                  
         BE    VPID04              DIDN'T FIND A RECORD                         
         CLC   TLKPID,BCWORK       DOES THE RECORD MATCH                        
         BE    VPID08              YES                                          
VPID04   MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
         USING PERRECD,R4          RETRIEVE OFFICE CODE                         
VPID08   MVC   IOKEY,BCSPACES                                                   
         LA    R4,IOKEY                                                         
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUABIN                                                   
         MVC   PERKCODE,TLKPID                                                  
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BE    VPID10                                                           
         MVC   TLKPIDOC,BCSPACES   LEAVE OFFICE BLANK                           
         B     VPID20                                                           
                                                                                
VPID10   L     R1,AIO2                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LOCELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VPID20                                                           
         L     R5,12(R1)                                                        
         USING LOCELD,R5                                                        
VPID18   MVC   TLKPIDOC,LOCOFF                                                  
         XR    RE,RE                                                            
         IC    RE,LOCLN                                                         
         AR    R5,RE                                                            
         CLI   LOCEL,LOCELQ   NEED OFFICE CODE OF LATEST LOCELD                 
         BE    VPID18                                                           
                                                                                
VPID20   MVC   FLTIFLD(L'TLKPID),FVIFLD                                         
         B     EXITOK                                                           
         DROP  R4,R5                                                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A PERSONAL ID FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
SRCHPID  MVC   BODUB1,BCSPACES                                                  
         L     R0,FVADDR                                                        
         S     R0,ATWA                                                          
         GOTO1 VSRCHCAL,BOPARM,('STMPSTRQ',(R0)),ATWA,ACOM,0,          X        
               (1,=CL8'PERSON'),0                                               
         B     EXITOK                                                           
         DROP  RF                                                               
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON PID FIELD                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FPIDDTA  LA    RF,FPIDTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FPIDTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFPID)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFPID)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFPID)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFPID)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFPID)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFPID DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER ON PID FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTFPID MVC   FVIFLD(L'SVPID),FLTIFLD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON PID FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTFPID CLI   FVILEN,L'SVPID                                                   
         BH    EXITLONG            FIELD TOO LONG                               
         GOTOX ('VALPID',AGROUTS),FVIFLD                                        
         BNE   VFLTF1                                                           
*&&UK                                                                           
         GOTO1 ACHKLPID,BOPARM,BCWORK                                           
         BNL   VFLTF2                                                           
*&&                                                                             
VFLTF1   MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
VFLTF2   MVC   SVPIDB,BCWORK                                                    
         MVC   SVPID,FVIFLD                                                     
         MVC   FLTIFLD(L'SVPID),SVPID                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A PERSONAL ID FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
SRCHFPID MVC   BODUB1,BCSPACES                                                  
         L     R0,FVADDR                                                        
         S     R0,ATWA                                                          
         GOTO1 VSRCHCAL,BOPARM,('STMPSTRQ',(R0)),ATWA,ACOM,0,          X        
               (1,=CL8'PERSON'),0                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON PID                                      *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFPID B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FIRST NAME                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
FNMDTA   LA    RF,FNMTBL                                                        
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
FNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIRST NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISFNM   MVC   FVIFLD(L'TLKPIDFN),TLKPIDFN                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LAST NAME                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
LNMDTA   LA    RF,LNMTBL                                                        
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
LNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNM)                                 
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY LAST NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISLNM   MVC   FVIFLD(L'TLKPIDLN),TLKPIDLN                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE FIELD                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
OFCDTA   LA    RF,OFCTBL                                                        
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
OFCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFC)                                 
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY OFFICE CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISOFC   MVC   FVIFLD(L'TLKPIDOC),TLKPIDOC                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR CLI/PRO CODE FIELD                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CPJCDTA  LA    RF,CPJCTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
CPJCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPJC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPJC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCPJC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLI/PRO CODE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DISCPJC  LLC   RF,PROLEN                                                        
         SHI   RF,1                                                             
         MVC   FVIFLD(0),TLKCPJC   UNKNOWN TYPE                                 
         EX    RF,*-6                                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLI/PRO CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
VALCPJC  MVC   ACPJFLD,FVADDR      SAVE A(FIELD)                                
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         MVC   TLKCPJC,BCSPACES    CLEAR OUT CPJ CODE                           
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLC   FVILEN,PROLEN                                                    
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ECOPO) CLIENT OR PRODUCT CODES ONLY              
         B     EXITL                                                            
         GOTO1 AVALCPJ,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VCPJC02                                                          
         MVC   FVADDR,ACPJFLD                                                   
         BH    EXITL                                                            
         CLC   FVILEN,CLILEN                                                    
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCLI)                                           
         B     EXITL               INVALID CLIENT CODE                          
         CLC   FVILEN,PROLEN                                                    
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INPRO)                                           
         B     EXITL               INVALID PRODUCT CODE                         
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
VCPJC02  MVC   TLKCPJC,CPJCODE     SORT THE CLI/PRO CODES                       
         MVC   TLKCPJNM,CPJNAME    SAVE CLI/PRO NAME                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO CODE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
SRCHCPJC GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   X        
               (X'12',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CLI/PRO NAME FIELD                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CPJNDTA  LA    RF,CPJNTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
CPJNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPJN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CLI/PRO NAME FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISCPJN  MVC   FVIFLD(L'TLKCPJNM),TLKCPJNM                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON CLI/PRO FIELD                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FCPJDTA  LA    RF,FCPJTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FCPJTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFCPJ)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFCPJ)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFCPJ)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFCPJ)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFCPJ)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFCPJ DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER ON CLI/PRO FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTFCPJ MVC   FVIFLD(L'SVCPJ),FLTIFLD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON CLI/PRO FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VFLTFCPJ CLC   FVILEN,PROLEN                                                    
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ECOPO) CLIENT OR PRODUCT CODES ONLY              
         B     EXITL                                                            
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ FOR CLI/PRO ACCOUNT RECORD              
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'PRODUL),PRODUL                                       
         MVC   T.ACTKACT,FVIFLD                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACC) INVALID ACCOUNT                           
         B     EXITL                                                            
*                                                                               
         MVC   SVCPJ,FVIFLD                                                     
         MVC   SVCPJXLN,FVXLEN                                                  
         MVC   FLTIFLD(L'SVCPJ),SVCPJ                                           
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO CODE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
SRCHFCPJ GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   X        
               (X'12',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON CLI/PRO                                  *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFCPJ B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR MEDIA FIELD                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
MEDL     LA    RF,MEDLTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
MEDLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEDC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMEDC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHMEDC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER ON CLI/PRO FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DISMEDC  LA    RF,TLKCPJC                                                       
         LLC   R0,PROLEN                                                        
         AR    RF,R0                                                            
         CLI   0(RF),C' '          EXTRACT MEDIA CODE                           
         BNH   EXITOK                                                           
         MVC   FVIFLD(L'PMDCODE),0(RF)                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A MEDIA CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALMEDC  MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         LA    R5,TLKCPJC                                                       
         LLC   RE,CLILEN                                                        
         SHI   RE,1                                                             
         CLC   0(0,R5),BCSPACES    ANY CLIENT CODE EXISTS?                      
         EX    RE,*-6                                                           
         BH    VALMED2             YES                                          
         CLI   FVILEN,0            DELETING THE MEDIA CODE?                     
         BNE   VALMED3                                                          
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
VALMED2  CLI   FVILEN,0            DELETING MEDIA CODE?                         
         BNE   VALMED3                                                          
         LLC   R0,PROLEN                                                        
         AR    R5,R0                                                            
         MVC   0(L'PMDCODE,R5),BCSPACES                                         
         B     EXITOK                                                           
*                                                                               
VALMED3  GOTO1 AVALMED,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VALMED4                                                          
         MVC   FVMSGNO,=AL2(AE$INVCD)                                           
         B     EXITL               INVALID CODE                                 
*                                                                               
VALMED4  LA    R5,TLKCPJC                                                       
         LLC   R0,PROLEN                                                        
         AR    R5,R0                                                            
         MVC   0(L'MEDCODE,R5),MEDCODE PUT MEDIA CODE WHERE JOB IS              
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A MEDIA CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
SRCHMEDC GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MED,ACOM,      X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR MEDIA NAME FIELD                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
MEDND    LA    RF,MEDNDTBL                                                      
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
MEDNDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEDN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY MEDIA NAME FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING PMDRECD,R5                                                       
DISMEDN  LA    RF,TLKCPJC                                                       
         LLC   R0,PROLEN                                                        
         AR    RF,R0                                                            
         LA    R5,IOKEY                                                         
         CLI   0(RF),C' '                                                       
         BNH   EXITOK                                                           
         MVC   PMDKEY,BCSPACES     READ MEDIA CODE RECORD                       
         MVC   PMDKCPY,CUABIN      CONNECTED ID                                 
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKMED,0(RF)                                                    
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   EXITOK                                                           
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('PMDELQ',PMDRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NAMEL MISSING                                
T        USING PMDELD,BOELEM                                                    
         MVC   FVIFLD(L'T.PMDDESC),T.PMDDESC                                    
         B     EXITOK                                                           
         DROP  T,R5                                                             
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON MEDIA CODE                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FMED     LA    RF,FMEDTBL                                                       
         B     ITER                                                             
*                                                                               
FMEDTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFMED)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFMED)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFMED)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFMED)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFMED)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFMED)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFMED DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON MEDIA CODE FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTFMED MVC   FVIFLD(L'SVMED),FLTIFLD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON MEDIA CODE FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
VFLTFMED CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'PMDKMED                                                 
         BH    EXITLONG            FIELD TOO LONG                               
T        USING PMDRECD,IOKEY                                                    
         MVC   T.PMDKEY,BCSPACES   READ FOR MEDIA CODE RECORD                   
         MVC   T.PMDKCPY,CUABIN                                                 
         MVI   T.PMDKTYP,PMDKTYPQ                                               
         MVC   T.PMDKMED,FVIFLD                                                 
         L     R1,=AL4(XOREAD+XOACCDIR+XIO2)                                    
         GOTOR AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVCD) INVALID CODE                              
         B     EXITL                                                            
*                                                                               
         MVC   SVMED,FVIFLD                                                     
         MVC   FLTIFLD(L'SVMED),SVMED                                           
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON MEDIA CODE NAME                                           *         
***********************************************************************         
         SPACE 1                                                                
SRCHFMED GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MED,ACOM,      X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON MEDIA CODE                               *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFMED B     FLTXE                                                            
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
         CLI   SREC,R#TEAM         TEAM RECORD                                  
         BE    NTRO02                                                           
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
NTRO02   OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   SACT,A#LST          PAGE DISPLAY SCREEN FROM LIST SCREEN         
*        BNE   EXITH                                                            
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
THIS     USING TEARECD,R2                                                       
LAST     USING TEARECD,R3                                                       
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
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LSCRLAST),AL1(0,0,0),AL4(SCRLAST)                            
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLST1)                            
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,2),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,2),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,2),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,2),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,2),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,2),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,2),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,2),AL4(UPDLST1)                            
*                                                                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING TEARECD,IOKEY                                                    
FLST     TM    LSSCIND1,LSSCIFLT   TEST FILTERS CHANGED                         
         BO    FLST02              YES, IGNORE ERRIND                           
         TM    ERRIND,ERMAXIO                                                   
         BZ    FLST02                                                           
         MVC   THIS.TEAKEY,SKEYLAST                                             
FLST02   MVI   ERRIND,0            RESET ERROR INDICATOR                        
         XC    SKEYLAST,SKEYLAST   OR CLEAR SAVED LAST KEY                      
*                                                                               
         MVC   X.TEAKEY,THIS.TEAKEY                                             
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
NLST02   GOTO1 AIOCHK              CHECK FOR MAX IOS                            
         BE    NLST02A                                                          
         OI    ERRIND,ERMAXIO                                                   
         MVC   SKEYLAST,IOKEY      WHEN RESUMED, START HERE                     
         B     EXITL                                                            
*                                                                               
NLST02A  CLC   X.TEAKEY(TEAKREM-TEARECD),THIS.TEAKEY                            
         BNE   EXITL               CHANGE COMPANY OR UNIT/LEDGER                
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
         BO    NLST                GET NEXT                                     
*                                                                               
NLST06   TM    BCCPYSTC,CPYSROFF   COMPANY ENFORCE OFFICES                      
         BNO   NLST08                                                           
         CLI   CUACCS,0                                                         
         BE    NLST08                                                           
         MVC   SKEYLAST,IOKEY                                                   
         GOTO1 ATSTOFF,X.TEAKOFF   TEST OFFICE CODE                             
         BE    NLST07                                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   IOKEY,SKEYLAST                                                   
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         B     NLST                                                             
*                                                                               
NLST07   MVC   IOKEY,SKEYLAST                                                   
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
NLST08   MVC   THIS.TEAKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
         GOTO1 ADOFLT                 FILTER UNWANT RECORDS                     
         BNE   NLST                                                             
         B     EXITOK                                                           
         DROP  THIS,LAST,X                                                      
         SPACE 2                                                                
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
* INITIALISE FOR LIST 1/2                                             *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSTSAR+LSSBALL                                          
         OI    LSSTAT2,LSSADD+LSSNOSEQ                                          
         MVC   LSCOLLIN,=AL2(240)  NUMBER OF COLUMNS PER LIST LINE              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1/2                                             *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,TEARFST-TEARECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1/2                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TEARECD,R5                                                       
FLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIOREC           A(RECORD)                                    
         C     R5,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    R5,TEARFST          IT IS NOW.                                   
         XR    RE,RE                                                            
*                                                                               
         USING LIDELD,R5                                                        
FML01    CLI   LIDEL,0             RECORD END?                                  
         BE    EXITNO              YES                                          
         CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML18               NO                                           
         CLI   GSSMPAGE,1          ARE WE ON PID PAGE                           
         BNE   FML02               NO                                           
         CLI   LIDTYPE,LIDTPID     IS IT PID LIST                               
         BE    FML10                                                            
         B     NML18                                                            
FML02    CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LIDTYPE,LIDTCPJ     IS IT CLI/PRO LIST                           
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
         CLI   GSSMPAGE,1          ARE WE ON PID PAGE                           
         BNE   FML12               NO                                           
         L     RF,AVALPIDC         SET TO VALIDATE PID CODE                     
         B     FML18                                                            
FML12    CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AVALCPJ          SET TO VALIDATE CLIENT PROD JOB              
FML18    GOTO1 (RF),BOPARM,(LIDITLN,LIDDATA)                                    
         BNE   NML20               NOT VALID CLI/PRO CODE                       
FML20    S     R5,AIOREC                                                        
         STH   R5,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1/2                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TEARECD,R5                                                       
NLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIOREC           A(RECORD)                                    
         C     R5,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    NML20                                                            
         LA    R5,TEARFST          IT IS NOW.                                   
*                                                                               
         USING LIDELD,R5                                                        
NML02    CLI   LIDEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML18               YES                                          
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   NML03               NO                                           
         CLI   LIDTYPE,LIDTPID     IS IT PID LIST                               
         BE    NML30                                                            
         B     NML18                                                            
NML03    CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         CLI   LIDTYPE,LIDTCPJ     IS IT CLI/PRO LIST                           
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
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   NML22               NO                                           
         L     RF,AVALPIDC         SET TO VALIDATE PID CODE                     
         B     NML28                                                            
NML22    CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         L     RF,AVALCPJ          SET TO VALIDATE CLIENT PROD JOB              
NML28    GOTO1 (RF),BOPARM,(LIDITLN,(R4))                                       
         BE    NML40               VALID                                        
         XC    FVXTRA,FVXTRA                                                    
         B     NML20               INVALID                                      
*                                                                               
NML30    LA    R4,LIDDATA          ADDRESS OF DATA START OF ELEMENT             
         SR    R4,R5                                                            
         STH   R4,CURDISP          CURRENT DISPLACEMENT INTO ELEMENT            
         XR    R4,R4                                                            
         IC    R4,LIDITLN          GET LENGTH OF DATA                           
         STH   R4,DATALEN          SAVE THIS LENGTH                             
         IC    R4,LIDLN                                                         
         STH   R4,TOTELLN          SAVE TOTAL ELEMENT LENGTH                    
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   NML32               NO                                           
         L     RF,AVALPIDC         SET TO VALIDATE PID CODE                     
         B     NML38                                                            
NML32    CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         L     RF,AVALCPJ          SET TO VALIDATE CLIENT PROD JOB              
NML38    GOTO1 (RF),BOPARM,(LIDITLN,LIDDATA)                                    
         BE    NML40                                                            
         XC    FVXTRA,FVXTRA                                                    
         B     NML20               NOT VALID CLI/PRO CODE                       
                                                                                
NML40    S     R5,AIOREC                                                        
         STH   R5,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1/2                                           *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)                                               
                                                                                
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   TSFL01              NO                                           
         XC    TLKPID,TLKPID                                                    
         MVC   TLKPID,PIDCODE                                                   
         MVC   TLKPIDB,PIDBINY                                                  
         MVC   TLKPIDFN,PIDFSTNM                                                
         MVC   TLKPIDLN,PIDLSTNM                                                
         MVC   TLKPIDOC,PIDOFFC                                                 
         B     EXITOK                                                           
                                                                                
TSFL01   CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    TLKCPJC,TLKCPJC                                                  
         MVC   TLKCPJC,CPJCODE                                                  
         MVC   TLKCPJNM,CPJNAME                                                 
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1/2                                           *         
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
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   UPDF01              NO                                           
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),        X        
               (2,=AL1(L'SAPWDNUM,LIDTPID))                                     
         B     EXITOK                                                           
*                                                                               
UPDF01   CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    LASTCODE,LASTCODE                                                
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),        X        
               (2,=AL1(L'TLKCPJC,LIDTCPJ))                                      
         B     EXITOK                                                           
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1/2                                    *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TEARECD,R2                                                       
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
* LAST TIME FOR UPDATE 1/2                                            *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
UPDLST1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         CLI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         BE    ULST101                                                          
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   EXITOK              NO                                           
         LH    RF,LS1STLIN         MUST ENTER A PID                             
         A     RF,ATWA                                                          
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         MVC   FVMSGNO,=AL2(AE$NLINE)                                           
         B     EXITL                                                            
*                                                                               
ULST101  XC    BOELEM,BOELEM                                                    
T        USING LIDELD,BOELEM                                                    
         USING TEARECD,R2                                                       
         MVI   T.LIDEL,LIDELQ                                                   
         LA    R4,T.LIDDATA                                                     
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   ULST102             NO                                           
         MVI   T.LIDTYPE,LIDTPID                                                
         MVI   T.LIDITLN,L'SAPWDNUM                                             
         B     ULST114                                                          
ULST102  CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTCPJ                                                
         MVI   T.LIDITLN,L'TLKCPJC                                              
*                                                                               
ULST114  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
ULST116  LA    R1,TSANXT           DEAL WITH ALL DELETE REQUEST                 
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    ULST140             END OF FILE                                  
         CLC   TLKSES,SESNL        CHECK NEST LEVEL                             
         BNE   ULST140             DONE ALL FOR THIS LEVEL                      
                                                                                
         CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE?           
         BNE   ULST124                                                          
*                                                                               
ULST117  OC    LASTCODE,LASTCODE   ANYTHING IN LASTCODE YET?                    
         BZ    ULST122             NO                                           
*                                                                               
         XR    RE,RE               FIND ACTUAL LENGTH OF LASTCODE               
         LA    RE,L'LASTCODE                                                    
         LA    R5,LASTCODE+L'LASTCODE-1                                         
ULST118  CLI   0(R5),C' '                                                       
         BNE   ULST119                                                          
         SHI   R5,1                                                             
         BCT   RE,ULST118                                                       
                                                                                
ULST119  SHI   RE,1                                                             
         EXCLC RE,LASTCODE,TLKCPJC    COMPARE LAST CLI/PRO CODE                 
         BNE   ULST122                WITH CURRENT ONE                          
ULST121  MVC   FVMSGNO,=AL2(AE$HLEXS) HIGHER OR LOWER LEVEL ACCOUNT             
         NI    LSLTIND1,FF-LSLTIBLD   REBUILD THE LIST                          
         XC    GCLASKEY,GCLASKEY      SET KEY HAS BEEN CHANGED                  
         NI    GSINDSL1,FF-GSIXMNT    TURN OF MAINT SCREEN LOADED FLAG          
         B     EXITL                  ALREADY EXISTS                            
ULST122  MVC   LASTCODE,TLKCPJC       MOVE CLI/PRO CODE TO LASTCODE             
*                                                                               
ULST124  XR    RE,RE                                                            
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   ULST126             NO                                           
         LA    RE,L'SAPWDNUM-1                                                  
         MVC   0(0,R4),TLKPIDB                                                  
         EX    RE,*-6                                                           
         B     ULST128                                                          
*                                                                               
ULST126  IC    RE,T.LIDITLN                                                     
         SHI   RE,1                                                             
         MVC   0(0,R4),TLKCPJC                                                  
         EX    RE,*-6                                                           
*                                                                               
ULST128  AHI   RE,1                                                             
         AR    R4,RE                                                            
         LR    R5,R4                                                            
         LA    RF,T.LIDEL                                                       
         SR    R5,RF               TOTAL DISPLACEMENT OF ELEMENT                
         CHI   R5,240                                                           
         BL    ULST116                                                          
         STC   R5,T.LIDLN                                                       
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BE    ULST130                                                          
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
ULST130  XC    BOELEM,BOELEM                                                    
         MVI   T.LIDEL,LIDELQ                                                   
         LA    R4,T.LIDDATA                                                     
         SR    R5,R5                                                            
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   ULST131             NO                                           
         MVI   T.LIDITLN,L'SAPWDNUM                                             
         MVI   T.LIDTYPE,LIDTPID                                                
         B     ULST116                                                          
ULST131  CLI   GSSMPAGE,2          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTCPJ                                                
         MVI   T.LIDITLN,L'TLKCPJC                                              
         B     ULST116                                                          
*                                                                               
ULST140  LTR   R5,R5                                                            
         BZ    EXITOK                                                           
         STC   R5,T.LIDLN                                                       
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
         DROP  T,R2,R3                                                          
         EJECT ,                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
ADDQ     EQU   C'A'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
NAMFLDLQ EQU   36                                                               
MAXITMS  EQU   100                                                              
PRODUL   DC    C'SJ'                                                            
OFFUL    DC    C'2D'                                                            
MED      DC    C'MEDIA'                                                         
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
         USING SAVED,R6                                                         
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     GUSERI                                                           
         B     VALPIDC                                                          
         B     VALCPJ                                                           
         B     ADDRECS                                                          
         B     DELRECS                                                          
         B     DOFLT                                                            
         B     IOCHK                                                            
         B     VALMED                                                           
*                                                                               
OVROU1H  CLI   *,0                                                              
         B     OVROU1X                                                          
OVROU1L  CLI   *,FF                                                             
         B     OVROU1X                                                          
OVROU1E  CR    RB,RB                                                            
OVROU1X  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK BINARY USER-ID AND RETURN NAME                                *         
*                                                                     *         
* NTRY - P1  = BINARY USER-ID                                         *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = NAME FOUND                                    *         
***********************************************************************         
         SPACE 1                                                                
GUSERI   L     R2,0(R1)                                                         
                                                                                
         USING CTIREC,R4                                                        
         LA    R4,IOKEY                                                         
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKTYP,CTIKTYPQ                                                 
         MVC   CTIKNUM,0(R2)                                                    
         LHI   R1,XOREAD+XOCONFIL+XIO2                                          
         GOTO1 AIO                                                              
         BE    GUSE02                                                           
         MVC   FVIFLD(4),=C'????'  CAN'T FIND THE USER ID                       
         B     OVROU1E                                                          
GUSE02   L     R4,AIO2             CONTROL RECORD                               
         LA    R1,CTIDATA                                                       
         XR    R0,R0                                                            
         USING CTDSCD,R1                                                        
GUSE04   CLI   CTDSCEL,0           TEST E-O-R                                   
         BNE   *+6                                                              
         DC    H'0'                CTDSCEL MISSING                              
         CLI   CTDSCEL,CTDSCELQ    TEST DESCRIPTION ELEMENT                     
         BE    *+14                                                             
         IC    R0,CTDSCLEN                                                      
         AR    R1,R0                                                            
         B     GUSE04                                                           
         MVC   FVIFLD(L'SAPALPID),CTDSC     USER-ID                             
         B     OVROU1E                                                          
         DROP  R1,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* CHECK PID BINARY CODE FROM LIST ELEMENT AND RETURN NAME             *         
*                                                                     *         
* NTRY - P1  = PID BINARY CODE                                        *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALPIDC  L     R2,0(R1)                                                         
         MVC   PIDBINY,0(R2)                                                    
         GOTOX ('GETPID',AGROUTS),PIDBINY                                       
         CLC   =C'????????',BCWORK                                              
         BE    OVROU1L             INVALID BINARY PID                           
         MVC   PIDCODE,BCWORK                                                   
         GOTOX ('VALPID',AGROUTS),PIDCODE                                       
         BNE   OVROU1L                                                          
*&&UK                                                                           
         GOTO1 ACHKLPID,BOPARM,BCWORK                                           
         BL    OVROU1L                                                          
*&&                                                                             
         MVC   PIDFSTNM,BCWORK+2                                                
         MVC   PIDLSTNM,BCWORK+22                                               
                                                                                
         USING PERRECD,R4          RETRIEVE OFFICE CODE                         
         MVC   IOKEY,BCSPACES                                                   
         LA    R4,IOKEY                                                         
         MVI   PERKTYP,PERKTYPQ                                                 
         MVC   PERKCPY,CUABIN                                                   
         MVC   PERKCODE,PIDCODE                                                 
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BE    VPIDC02                                                          
         MVC   PIDOFFC,BCSPACES    LEAVE OFFICE BLANK                           
         B     OVROU1E                                                          
                                                                                
VPIDC02  L     R1,AIO2                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LOCELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   OVROU1E                                                          
         L     R5,12(R1)                                                        
         USING LOCELD,R5                                                        
VPIDC10  MVC   PIDOFFC,LOCOFF                                                   
         XR    RE,RE                                                            
         IC    RE,LOCLN                                                         
         AR    R5,RE                                                            
         CLI   LOCEL,LOCELQ   NEED OFFICE CODE OF LATEST LOCELD                 
         BE    VPIDC10                                                          
         B     OVROU1E                                                          
         DROP  R4,R5                                                            
         EJECT ,                                                                
***********************************************************************         
* CHECK CLI/PRO CODE IS VALID AND RETURN NAME                         *         
*                                                                     *         
* NTRY - P1  = CLI/PRO CODE                                           *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
VALCPJ   L     R2,0(R1)            R2=CLI/PRO CODE                              
         MVC   CPJCODE,0(R2)       EXTRACT CLI/PRO CODE + MEDIA                 
         MVC   CPJNAME,BCSPACES                                                 
         LLC   RE,CLILEN                                                        
         SHI   RE,1                                                             
         CLC   0(0,R2),BCSPACES    ANY CLI/PRO/JOB?                             
         EX    RE,*-6                                                           
         BNH   OVROU1E                                                          
         LR    RF,R2               IF FROM LIDDATA THEN ALSO CONTAINS           
         XR    R0,R0               MEDIA CODE                                   
         IC    R0,PROLEN                                                        
         SR    RE,RE                                                            
VCPJ02   CLI   0(RF),C' '                                                       
         BE    VCPJ03                                                           
         LA    RF,1(,RF)                                                        
         AHI   RE,1                                                             
         CR    RE,R0                                                            
         BNE   VCPJ02                                                           
*                                                                               
VCPJ03   STC   RE,MYBYTE           LENGTH OF CLI/PRO CODE                       
         CLC   MYBYTE,CLILEN                                                    
*&&UK*&& BE    VCPJ04                                                           
*&&US*&& BNH   VCPJ04                                                           
         CLC   MYBYTE,PROLEN                                                    
*&&UK*&& BE    VCPJ04                                                           
*&&US*&& BNH   VCPJ04                                                           
         B     OVROU1L             INVALID CLI/PRO LENGTH                       
                                                                                
VCPJ04   MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(2),=C'SJ'                                              
*                                                                               
         MVI   TOTNAML,0                                                        
         MVC   BOWORK1(L'BOWORK1+L'BOWORK2),BCSPACES                            
         LA    R4,BOWORK1                                                       
*                                                                               
         IC    RE,CLILEN           READ CLIENT NAME                             
         SHI   RE,1                                                             
         EXMVC RE,T.ACTKACT,0(R2)  CLIENT CODE                                  
         GOTO1 AGETACT,BOPARM,0           READ ACCOUNT/TEST SECURITY            
         BNE   OVROU1H                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO1),0                  
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
         BE    VCPJ20                                                           
*                                                                               
         MVC   IOKEY,IOKEYSAV      RESTORE IOKEY                                
         SR    RE,RE                                                            
         IC    RE,PROLEN                                                        
         SHI   RE,1                                                             
         EXMVC RE,T.ACTKACT,0(R2)  PRODUCT CODE                                 
         GOTO1 AGETACT,BOPARM,0           READ ACCOUNT/TEST SECURITY            
         BNE   OVROU1H                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO1),0                  
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
         DROP  RF                                                               
*                                                                               
VCPJ20   LA    R4,BOWORK1                                                       
         SR    RE,RE                                                            
         ICM   RE,1,TOTNAML                                                     
         BZ    OVROU1E                                                          
         CHI   RE,NAMFLDLQ         ENOUGH SPACE TO STORE NAMES                  
         BNH   VCPJ22                                                           
         SHI   RE,NAMFLDLQ                                                      
         AR    R4,RE               TRUNCATE THE NAMES                           
*                                                                               
VCPJ22   MVC   CPJNAME,0(R4)                                                    
         B     OVROU1E                                                          
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* UPDATE PASSIVE TEAM RECORDS                                         *         
*                                                                     *         
* NTRY - P1  = TEAM RECORD                                            *         
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
         USING TEARECD,R3                                                       
         LA    R2,IOKEY                                                         
         MVC   IOKEY,0(R3)                                                      
         L     R1,=AL4(XORDD+XOACCDIR+XIO4) READ DIR FOR DA                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEAMDA,IOKEY+TEAKDA-TEARECD                                      
*                                                                               
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'A',(R3)),CPTRBLK,TEAMDA,0,ACOM                 
         B     OVROU1E                                                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* DELETE TEAM PASSIVES                                                *         
*                                                                     *         
* NTRY - P1  = PID LIST ELEMENT                                       *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
DELRECS  CLI   CSACT,A#DEL         ACTION DELETE                                
         BE    *+12                YES - DELETE ALL PASSIVE POINTERS            
         CLI   GSSMPAGE,1          ARE WE ON PID PAGE                           
         BNE   OVROU1E             NO                                           
         L     R3,0(R1)            GET RECORD ADDRESS                           
         USING TEARECD,R3                                                       
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',CPTRBLK),0,0,ACOM               
*                                                                               
         B     OVROU1E                                                          
         DROP  R3                                                               
         EJECT ,                                                                
*********************************************************************           
* FILTER TEAM RECORDS                                               *           
* ENTY - IOKEY = TEAM RECORD KEY                                    *           
*********************************************************************           
         SPACE 2                                                                
DOFLT    OC    SVFLTS(SVFLTLQ),SVFLTS                                           
         BZ    OVROU1E             OK - NO FILTER                               
*                                                                               
         LHI   R1,XOGET+XOACCMST+XIO11                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
*                                                                               
         NI    TEAINDS,FF-(TEAIFCPJ+TEAIFPID+TEAIFMED)                          
         OC    SVPID,SVPID         ANY PID FILTER?                              
         BNZ   *+8                                                              
         OI    TEAINDS,TEAIFPID    DON'T COMPARE PID                            
         OC    SVCPJ,SVCPJ         ANY CLI/PRO FILTER?                          
         BNZ   *+8                                                              
         OI    TEAINDS,TEAIFCPJ    DON'T COMPARE CLI/PRO                        
         OC    SVMED,SVMED         ANY MEDIA CODE FILTER?                       
         BNZ   *+8                                                              
         OI    TEAINDS,TEAIFMED    DON'T COMPARE MEDIA CODES                    
*                                                                               
         L     R4,AIOREC           A(TEAM RECORD)                               
         LA    R4,TEARFST-TEARECD(R4)                                           
DOFLT10  TM    TEAINDS,TEAIFCPJ+TEAIFPID+TEAIFMED                               
         BO    OVROU1E                                                          
         CLI   0(R4),0                                                          
         BE    OVROU1L             END OF RECORD                                
*                                                                               
DOFLT12  CLI   0(R4),LIDELQ                                                     
         BE    DOFLT20                                                          
*                                                                               
DOFLT14  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DOFLT10                                                          
*                                                                               
         USING LIDELD,R4                                                        
DOFLT20  MVC   BOWORK1,BCSPACES                                                 
         CLI   LIDTYPE,LIDTCPJ                                                  
         BNE   DOFLT24                                                          
         TM    TEAINDS,TEAIFMED    COMPARE MEDIA CODE?                          
         BNO   *+12                                                             
         TM    TEAINDS,TEAIFCPJ    COMPARE CLI/PRO?                             
         BO    DOFLT14             NO - CLI/PRO FOUND                           
         MVC   BOWORK1(L'SVCPJ),SVCPJ                                           
         B     DOFLT30                                                          
DOFLT24  CLI   LIDTYPE,LIDTPID                                                  
         BNE   DOFLT14                                                          
         TM    TEAINDS,TEAIFPID    COMPARE PID?                                 
         BO    DOFLT14             NO - PID FOUND                               
         MVC   BOWORK1(L'SVPIDB),SVPIDB                                         
*                                                                               
DOFLT30  SR    R5,R5                                                            
         IC    R5,LIDITLN          LENGTH OF ITEMS                              
         SR    R0,R0                                                            
         IC    R0,LIDLN            LENGTH OF ELEMENT                            
         SHI   R0,LIDDATA-LIDELD                                                
         SRDL  R0,32                                                            
         DR    R0,R5               R1=NUMBER OF ITEMS                           
*                                                                               
         LR    RE,R5               R5=LENGTH OF ITEMS                           
         BCTR  RE,0                RE=LENGTH OF ITEMS - 1                       
         LA    RF,LIDDATA                                                       
DOFLT34  CLI   LIDTYPE,LIDTCPJ                                                  
         BNE   DOFLT35                                                          
         IC    RE,SVCPJXLN         INPUT CLI/PRO LENGTH-1                       
         TM    TEAINDS,TEAIFCPJ    ANY CLI/PRO FILTER?                          
         BNO   DOFLT35                                                          
         B     DOFLT35A                                                         
DOFLT35  CLI   LIDTYPE,LIDTPID                                                  
         BNE   *+8                                                              
         LA    RE,L'TLKPIDB-1                                                   
         EXCLC RE,0(RF),BOWORK1                                                 
         BNE   DOFLT38                                                          
DOFLT35A TM    TEAINDS,TEAIFMED    DOING MEDIA COMPARE                          
         BNZ   DOFLT36                                                          
         LR    R3,RF                                                            
         LLC   R0,PROLEN                                                        
         AR    R3,R0                                                            
         CLC   0(L'SVMED,R3),SVMED                                              
         BNE   DOFLT38                                                          
*                                                                               
DOFLT36  CLI   LIDTYPE,LIDTCPJ                                                  
         BNE   *+12                                                             
         OI    TEAINDS,TEAIFCPJ+TEAIFMED FOUND CLI/PRO/MEDIA                    
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTPID                                                  
         BNE   DOFLT14                                                          
         OI    TEAINDS,TEAIFPID    FOUND PID CODE                               
         B     DOFLT14                                                          
*                                                                               
DOFLT38  AR    RF,R5               CHECK NEXT ITEM                              
         BCT   R1,DOFLT34                                                       
         B     DOFLT14             GET NEXT ELEMENT                             
         DROP  R4                                                               
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
         AHI   R0,200              WITHIN 200 OF GRPIT?                         
         CLM   R0,3,FATMAXIO-FACTSD(R1)                                         
         BNH   OVROU1E                                                          
*                                                                               
         B     OVROU1L             MAX I/O COUNT REACHED                        
***********************************************************************         
* CHECK MEDIA CODE IS VALID AND RETURN NAME                           *         
*                                                                     *         
* NTRY - P1  = MEDIA CODE                                             *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALMED   L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    R5,IOKEY                                                         
         USING PMDRECD,R5                                                       
         MVC   PMDKEY,BCSPACES     READ MEDIA CODE RECORD                       
         MVC   PMDKCPY,CUABIN      CONNECTED ID                                 
         MVI   PMDKTYP,PMDKTYPQ                                                 
         SHI   RE,1                                                             
         MVC   PMDKMED(0),0(R2)                                                 
         EX    RE,*-6                                                           
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L                                                          
         MVC   MEDCODE,PMDKMED                                                  
         B     OVROU1E                                                          
         DROP  R5                                                               
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
         SPACE 2                                                                
* ACSCRDSECT                   - FOR STYELD IN SCRIBE RECORDS                   
         PRINT OFF                                                              
       ++INCLUDE ACSCRDSECT                                                     
         PRINT ON                                                               
         SPACE 2                                                                
* CTGENFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
         EJECT ,                                                                
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
APIDFLD  DS    A                   A(PID CODE FIELD)                            
ACPJFLD  DS    A                   A(CLI/PRO FIELD)                             
*                                                                               
OVROUT1  DS    0A                                                               
AGUSERI  DS    A                                GUSERI                          
AVALPIDC DS    A                                VALPIDC                         
AVALCPJ  DS    A                                VALCPJ                          
AADDRECS DS    A                                ADDRECS                         
ADELRECS DS    A                                DELRECS                         
ADOFLT   DS    A                                DOFLT                           
AIOCHK   DS    A                                IOCHK                           
AVALMED  DS    A                                VALMED                          
OVROUT1N EQU   (*-OVROUT1)/L'OVROUT1                                            
*                                                                               
MNTDISP  DS    H                   MOVE TO SAVED STORAGE                        
CURDISP  DS    H                   CURRENT DISPLACENT INTO ELEMENT              
DATALEN  DS    H                   LENGTH OF DATA                               
TOTELLN  DS    H                   TOTAL LENGTH OF ELEMENT                      
*                                                                               
TEAINDS  DS    XL1                                                              
TEAIFCPJ EQU   X'80'               CLI/PRO CODE FOUND                           
TEAIFPID EQU   X'40'               PID FOUND                                    
TEAIFMED EQU   X'20'               MEDIA FOUND                                  
TEADFMED EQU   X'10'               DOING MEDIA FILTER                           
*                                                                               
CLILEN   DS    XL1                 LENGTH OF CLIENT                             
PROLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT                     
JOBLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT+JOB                 
TOTNAML  DS    XL1                 LENGTH OF CLIENT NAME                        
*                                                                               
SCPYALP  DS    CL(L'CPYALPHA)      AGENCY ALPHA CODE                            
*                                                                               
SVIOKEY  DS    XL42                                                             
TEAMDA   DS    XL(L'TEAKDA)                                                     
SESNL    DS    XL1                                                              
MYBYTE   DS    XL1                                                              
MYDUB    DS    D                                                                
MYBINA   DS    XL2                                                              
ANYLINES DS    CL1                                                              
CPTRWRK  DS    XL128                                                            
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT ,                                                                
***********************************************************************         
* DSECT                                                               *         
***********************************************************************         
         SPACE 2                                                                
SAVED    DSECT                                                                  
SAVOFF   DS    XL(L'TEAKOFF)       OFFICE                                       
SVTEA    DS    CL(L'TEAKNUM)       2 CHARACTER TEAM NUMBER                      
PIDCODE  DS    CL(L'SAPALPID)      8 CHARACTER PERSONAL ID                      
PIDBINY  DS    XL(L'SAPWDNUM)      2 BYTE BINARY PERSONAL ID                    
PIDFSTNM DS    CL(L'SANAME)        PERSONAL ID FIRST NAME                       
PIDLSTNM DS    CL(L'SANAME)        PERSONAL ID LAST NAME                        
PIDOFFC  DS    CL(L'LOCOFF)        OFFICE CODE                                  
CPJCODE  DS    CL(L'ACTKACT)       CLI/PRO CODE                                 
CPJNAME  DS    CL(NAMFLDLQ)        CLI/PRO NAME                                 
*                                                                               
IOCOUNT  DS    H                   COUNT IO'S                                   
*                                                                               
SVFLTS   DS    0F                                                               
SVCPJXLN DS    XL1                 LENGTH OF INPUT CLI/PRO-1                    
SVCPJ    DS    CL(L'ACTKACT)       FILTER ON CLI/PRO                            
SVMED    DS    CL(L'PMDKMED)       FILTER ON MEDIA CODE                         
SVPIDB   DS    CL(L'SAPWDNUM)      FILTER ON BINARY PID                         
SVPID    DS    CL(L'SAPALPID)      FILTER ON PID                                
SVFLTLQ  EQU   *-SVFLTS                                                         
*                                                                               
LASTCODE DS    CL12                PREVIOUS CLI/PRO OR 1R CODE                  
SKEYLAST DS    XL(L'ACTKEY)        SAVED CONTINUATION KEY FOR FLST              
MEDCODE  DS    CL(L'PMDKMED)       MEDIA CODE                                   
ERRIND   DS    XL1                 ERROR INDICATOR                              
ERMAXIO  EQU   X'80'               MAX IOS RETURNED FROM MEFILT                 
         EJECT ,                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKELEM  DS    0CL12                                                            
TLKPID   DS    CL(L'SAPALPID)      PERSONAL ID CODE CHARACTER                   
         DS    XL4                                                              
         ORG   TLKPID                                                           
TLKCPJC  DS    CL12                CLI/PRO CODE(+MEDIA)                         
         ORG   TLUSER                                                           
TLKCPJNM DS    CL(NAMFLDLQ)        CLI/PRO NAME                                 
         ORG   TLKCPJNM                                                         
TLKPIDB  DS    XL(L'SAPWDNUM)      BINARY PERSONAL ID                           
TLKPIDLN DS    CL(L'SANAME)        LAST NAME                                    
TLKPIDFN DS    CL(L'SANAME)        FIRST NAME                                   
TLKPIDOC DS    CL(L'LOCOFF)        OFFICE CODE                                  
         DS    XL20                                                             
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACFIL51   08/10/11'                                      
         END                                                                    
