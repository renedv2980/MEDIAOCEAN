*          DATA SET ACFIL52    AT LEVEL 005 AS OF 08/10/11                      
*PHASE T62352C,*                                                                
         SPACE 1                                                                
FIL52    TITLE 'ROLE RECORD'                                                    
         SPACE 2                                                                
*SMAN 003 13NOV08 <LO01-7385> STATUS REPORT FOR KEY ROLES                       
*SMAN     13NOV08 <LO01-8081> RECEIVE JOB EMAIL ALERTS                          
*MPEN 004 26MAY09 <UKCR00021578> FIX FOR LIMIT ACCESS LOGONS                    
         SPACE 2                                                                
FIL52    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL52**,RA,R7,RR=RE                                           
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
INIT     MVI   ROLINDS,0                                                        
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
         MVC   SAVOFF,CUACCS+1     CONNECTED USER-ID                            
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
         USING ROLRECD,R2                                                       
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
KFKVAL   XC    ROLKEY,ROLKEY       INITIALIZE KEY OF RECORD                     
         MVI   ROLKTYP,ROLKTYPQ                                                 
         MVI   ROLKSUB,ROLKSUBQ                                                 
         MVC   ROLKCPY,CUABIN      CONNECTED ID                                 
         MVC   ROLKOFF,SAVOFF                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    ROLKEY,ROLKEY       INITIALIZE KEY OF RECORD                     
         MVI   ROLKTYP,ROLKTYPQ                                                 
         MVI   ROLKSUB,ROLKSUBQ                                                 
         MVC   ROLKCPY,CUABIN      CONNECTED ID                                 
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
         USING ROLRECD,R2                                                       
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
         GOTO1 ADELRECS,BOPARM,ROLRECD                                          
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
RFWRT    GOTO1 ADELRECS,BOPARM,ROLRECD                                          
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
RLADD    GOTO1 AADDRECS,BOPARM,ROLRECD                                          
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
RLRES    GOTO1 AADDRECS,BOPARM,ROLRECD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE 1                                                                
RLWRT    GOTO1 AADDRECS,BOPARM,ROLRECD                                          
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
         USING ROLRECD,R2                                                       
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
         USING ROLRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(F#ROLE#OFF),AL1(0),AL3(ROFDTA)   OFFICE/OFFICE LIST          
         DC    AL2(F#ROLE#OFNM),AL1(0),AL3(OFNDTA)  NAME                        
         DC    AL2(F#ROLE#NUM),AL1(0),AL3(RLCDTA)   ROLE NUMBER                 
         DC    AL2(F#ROLE#PID),AL1(0),AL3(PIDDTA)   PID CODE                    
         DC    AL2(F#ROLE#FPID),AL1(0),AL3(FPIDDTA) PID FILTER                  
         DC    AL2(F#ROLE#FNAM),AL1(0),AL3(FNMDTA)  FIRST NAME                  
         DC    AL2(F#ROLE#LNAM),AL1(0),AL3(LNMDTA)  LAST NAME                   
         DC    AL2(F#ROLE#POFC),AL1(0),AL3(OFCDTA)  OFFICE CODE                 
         DC    AL2(F#ROLE#MEDL),AL1(0),AL3(MEDCDTA) MEDIA CODE                  
         DC    AL2(F#ROLE#MEDN),AL1(0),AL3(MEDNDTA) MEDIA NAME                  
         DC    AL2(F#ROLE#FMED),AL1(0),AL3(FMEDDTA) MEDIA FILTER                
         DC    AL2(F#ROLE#SRKR),AL1(0),AL3(SRKRDTA) STATUS REPORT KEY R         
         DC    AL2(F#ROLE#JOEM),AL1(0),AL3(JOEMDTA) JOB EMAIL ALERTS            
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL52    CSECT                                                                  
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
         MVC   SVROL,ROLKNUM       SAVE ROLE NUMBER                             
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
ROFDTA   LA    RF,ROFTBL                                                        
         B     ITER                                                             
*                                                                               
ROFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTOFF)                                
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
DISTOFF  MVC   FVIFLD(L'ROLKOFF),ROLKOFF   OFFICE                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE/OFFICE LIST                                         *         
***********************************************************************         
         SPACE 1                                                                
VALTOFF  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         MVC   ROLKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'ROLKOFF),ROLKOFF                                       
                                                                                
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
DFLTFOFF MVC   FVIFLD(L'ROLKOFF),FLTIFLD                                        
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
DOFTOFF  CLC   ROLKOFF,FLTIFLD                                                  
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
DISOFFN  OC    ROLKOFF,ROLKOFF     TEST ALL OFFICE                              
         BZ    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         TM    BCCPYST4,CPYSOFF2                                                
         BO    DISOFN02                                                         
         MVC   IOKEY,BCSPACES                 READ THE ACCOUNT RECORD           
         MVC   T.ACTKCPY,ROLKCPY              COMPANY                           
         MVC   T.ACTKUNT(L'OFFUL),OFFUL       UNIT/LEDGER                       
         MVC   T.ACTKACT(L'ROLKOFF),ROLKOFF   OFFICE CODE CODE                  
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
         MVC   T.OFFKCPY,ROLKCPY              COMPANY                           
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKOFF,ROLKOFF           OFFICE CODE CODE                     
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
* DATA OBJECT FOR ROLE NUMBER                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RLCDTA   LA    RF,RLCTBL                                                        
         B     ITER                                                             
*                                                                               
RLCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISROLC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALROLC)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISROLC)                                
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETROLC)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTROLC)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTROLC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFROLC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETROLC DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A ROLE NUMBER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISROLC  OC    ROLKNUM,ROLKNUM                                                  
         BZ    EXITOK                                                           
         CURED ROLKNUM,(5,FVIFLD),0,ALIGN=LEFT,DMCB=BODMCB,ZERO=BLANK           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A ROLE NUMBER FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALROLC  CLI   FVILEN,0                                                         
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
         STCM  RF,B'0011',ROLKNUM                                               
         STCM  RF,B'0011',SVROL                                                 
*                                                                               
         CLI   CSACT,A#ADD         SKIP CHECKING, IF ADDING OR COPYING          
         BE    EXITOK                                                           
         CLI   CSACT,A#CPY                                                      
         BE    EXITOK                                                           
         CLI   CSACT,A#RES                                                      
         BNE   VROLC02                                                          
         GOTO1 AVALKRO                                                          
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$M3RKR)  MAX OF 3 KEY ROLES ALLOWED               
         B     EXITL                                                            
VROLC02  LA    R4,IOKEY                                                         
X        USING ROLRECD,R4                                                       
         XC    X.ROLKEY,X.ROLKEY   READ ROLE RECORD                             
         MVI   X.ROLKTYP,ROLKTYPQ                                               
         MVI   X.ROLKSUB,ROLKSUBQ                                               
         MVC   X.ROLKCPY,CUABIN                                                 
         MVC   X.ROLKOFF,ROLKOFF                                                
         MVC   X.ROLKNUM,ROLKNUM                                                
         MVC   SVIOKEY,X.ROLKEY                                                 
         L     R1,=AL4(XORDD+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         BE    VROLC04                                                          
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITNV                                                           
VROLC04  MVC   FLTIFLD,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  X                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A ROLE NUMBER FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTROLC MVC   FVIFLD,FLTIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A ROLE NUMBER FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTROLC TM    FVIIND,FVINUM   CHECK FVIFLD IS NUMERIC                          
         BNO   EXITNV                                                           
         ZIC   RE,FVILEN                                                        
         AHI   RE,-1                                                            
         EXMVC RE,FLTIFLD,FVIFLD                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  MYDUB,FVIFLD(0)                                                  
         CVB   RF,MYDUB                                                         
         STCM  RF,B'0011',ROLKNUM                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON ROLE NUMBER                                         *         
***********************************************************************         
         SPACE 1                                                                
DOFROLC  OC    ROLKNUM,ROLKNUM     HAVE WE A PERSONAL ID TO FILTER ON           
         BZ    FLTXX               NO - WE DON`T WANT IT THEN                   
         CVB   RF,MYDUB                                                         
         STCM  RF,B'0011',MYBINA                                                
         CLC   ROLKNUM,MYBINA                                                   
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
*                                                                               
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
*                                                                               
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
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
         MVC   SVPIDB,BCWORK                                                    
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
* DATA OBJECT FOR MEDIA CODE FIELD                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
MEDCDTA  LA    RF,MEDCTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
MEDCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEDC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMEDC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHMEDC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A MEDIA CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISMEDC  MVC   FVIFLD(L'TLKMEDC),TLKMEDC   UNKNOWN TYPE                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A MEDIA CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALMEDC  L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
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
         CLI   FVILEN,L'PMDKMED                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         GOTO1 AVALMED,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VMEDC02                                                          
         MVC   FVMSGNO,=AL2(AE$INVCD)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
VMEDC02  MVC   TLKMEDC,MEDCODE     SORT THE MEDIA CODES                         
         MVC   TLKMEDNM,MEDNAME    SAVE MEDIA NAME                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A MEDIA CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
SRCHMEDC GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MED,ACOM,      X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR MEDIA NAME FIELD                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
MEDNDTA  LA    RF,MEDNTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
MEDNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEDN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY MEDIA NAME FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISMEDN  MVC   FVIFLD(L'TLKMEDNM),TLKMEDNM                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON MEDIA CODE                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FMEDDTA  LA    RF,FMEDTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FMEDTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFMED)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFMED)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFMED)                              
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
* DISPLAY A FILTER ON MEDIA CODE FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
DFLTFMED MVC   FVIFLD(L'SVMED),FLTIFLD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON MEDIA CODE FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VFLTFMED CLI   FVILEN,L'PMDKMED                                                 
         BH    EXITLONG            FIELD TOO LONG                               
T        USING PMDRECD,IOKEY                                                    
         MVC   T.PMDKEY,BCSPACES   READ FOR NON CLIENT ACCOUNT RECORD           
         MVC   T.PMDKCPY,CUABIN                                                 
         MVI   T.PMDKTYP,PMDKTYPQ                                               
         MVC   T.PMDKMED,FVIFLD                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
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
* DATA OBJECT FOR DISPLAYING A STATUS REPORT KEY ROLE FIELD           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
SRKRDTA  LA    RF,SRKRTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
SRKRTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETSRK)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISSRK)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSRK)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSRK)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSRK)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSRK)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETSRK  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A STATUS REPORT KEY ROLE FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
T        USING ROLKSTA,GSRECSTA                                                 
DISSRK   MVC   FVIFLD(L'BC@NO),BC@NO     NO IS DEFAULT                          
         TM    T.ROLKSTAT,ROLSSRKR                                              
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A STATUS REPORT KEY ROLE FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
T        USING ROLKSTA,GSRECSTA                                                 
VALSRK   DS    0H                                                               
         NI    T.ROLKSTAT,FF-ROLSSRKR                                           
         CLI   FVILEN,0                  IS A Y/N FIELD                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
*                               ALLOW UP TO 3 KEY ROLE RECORDS ONLY             
         GOTO1 AVALKRO                                                          
         BE    VALSRK02                                                         
         MVC   FVMSGNO,=AL2(AE$M3RKR)  MAX OF 3 KEY ROLES ALLOWED               
         B     EXITL                                                            
                                                                                
VALSRK02 OI    T.ROLKSTAT,ROLSSRKR                                              
         B     EXITOK                                                           
         DROP  T                                                                
                                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A STATUS REPORT KEY ROLE FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFLTSRK  MVC   FVIFLD(L'BC@YES),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A STATUS REPORT KEY ROLE FILTER                            *         
***********************************************************************         
         SPACE 1                                                                
VFLTSRK  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVI   SVSRKR,NO                                                        
         OI    FVIFLD,X'40'                                                     
         MVC   FLTIFLD(L'BC@NO),BC@NO                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVSRKR,YES                                                       
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON A STATUS REPORT KEY ROLE FIELD           *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTSRK  B     FLTXE                                                            
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A RECEIVE JOB EMAIL ALERTS FIELD         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
JOEMDTA  LA    RF,JOEMTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
JOEMTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETJOE)                                
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISJOE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALJOE)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTJOE)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTJOE)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTJOE)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETJOE  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECEIVE JOB EMAIL ALERTS FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
T        USING ROLKSTA,GSRECSTA                                                 
DISJOE   MVC   FVIFLD(L'BC@NO),BC@NO     NO IS DEFAULT                          
         TM    T.ROLKSTAT,ROLSJOEM                                              
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECEIVE JOB EMAIL ALERTS FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
T        USING ROLKSTA,GSRECSTA                                                 
VALJOE   DS    0H                                                               
         NI    T.ROLKSTAT,FF-ROLSJOEM                                           
         CLI   FVILEN,0                  IS A Y/N FIELD                         
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         OI    T.ROLKSTAT,ROLSJOEM                                              
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECEIVE JOB EMAIL ALERTS FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DFLTJOE  MVC   FVIFLD(L'BC@YES),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECEIVE JOB EMAIL ALERTS FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
VFLTJOE  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVI   SVJOEM,NO                                                        
         OI    FVIFLD,X'40'                                                     
         MVC   FLTIFLD(L'BC@NO),BC@NO                                           
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         MVI   SVJOEM,YES                                                       
         MVC   FLTIFLD(L'BC@YES),BC@YES                                         
         CLC   FVIFLD(1),BC@YES                                                 
         BE    EXITOK                                                           
         MVC   FLTIFLD(L'BC@YES),BCSPACES                                       
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON A RECEIVE JOB EMAIL ALERTS FIELD         *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTJOE  B     FLTXE                                                            
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
         CLI   SREC,R#ROLE         ROLE RECORD                                  
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
THIS     USING ROLRECD,R2                                                       
LAST     USING ROLRECD,R3                                                       
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
X        USING ROLRECD,IOKEY                                                    
FLST     TM    LSSCIND1,LSSCIFLT   TEST FILTERS CHANGED                         
         BO    FLST02              YES, IGNORE ERRIND                           
         TM    ERRIND,ERMAXIO                                                   
         BZ    FLST02                                                           
         MVC   THIS.ROLKEY,SKEYLAST                                             
FLST02   MVI   ERRIND,0            RESET ERROR INDICATOR                        
         XC    SKEYLAST,SKEYLAST   OR CLEAR SAVED LAST KEY                      
*                                                                               
         MVC   X.ROLKEY,THIS.ROLKEY                                             
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
NLST02A  CLC   X.ROLKEY(ROLKREM-ROLRECD),THIS.ROLKEY                            
         BNE   EXITL               CHANGE COMPANY OR UNIT/LEDGER                
         OC    X.ROLKNUM,X.ROLKNUM                                              
         BZ    NLST                MUST HAVE A PID                              
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
         MVC   SKEYLAST,IOKEY      SAVE OFF KEY 'COS TESTOFF DESTROYS           
         GOTO1 ATSTOFF,X.ROLKOFF   IT                                           
         BE    NLST07                                                           
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
NLST08   MVC   THIS.ROLKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
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
FTFLST1  LA    RF,ROLRFST-ROLRECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1/2                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING ROLRECD,R5                                                       
FLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIOREC           A(RECORD)                                    
         C     R5,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    R5,ROLRFST          IT IS NOW.                                   
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
FML02    CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LIDTYPE,LIDTMED     IS IT MEDIA LIST                             
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
FML12    CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AVALMED          SET TO VALIDATE MEDIA                        
FML18    GOTO1 (RF),BOPARM,(LIDITLN,LIDDATA)                                    
         BNE   NML20               NOT VALID CLI/PRO/JOB CODE                   
FML20    S     R5,AIOREC                                                        
         STH   R5,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1/2                                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING ROLRECD,R5                                                       
NLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIOREC           A(RECORD)                                    
         C     R5,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    NML20                                                            
         LA    R5,ROLRFST          IT IS NOW.                                   
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
NML03    CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         CLI   LIDTYPE,LIDTMED     IS IT MEDIA LIST                             
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
NML22    CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         L     RF,AVALMED          SET TO VALIDATE MEDIA                        
NML28    GOTO1 (RF),BOPARM,(LIDITLN,(R4))                                       
         BE    NML40               VALID                                        
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
NML32    CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         L     RF,AVALMED          SET TO VALIDATE MEDIA                        
NML38    GOTO1 (RF),BOPARM,(LIDITLN,LIDDATA)                                    
         BNE   NML20               NOT VALID CLI/PRO/JOB CODE                   
*                                                                               
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
*                                                                               
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   TSFL01              NO                                           
         XC    TLKPID,TLKPID                                                    
         MVC   TLKPID,PIDCODE                                                   
         MVC   TLKPIDB,PIDBINY                                                  
         MVC   TLKPIDFN,PIDFSTNM                                                
         MVC   TLKPIDLN,PIDLSTNM                                                
         MVC   TLKPIDOC,PIDOFFC                                                 
         B     EXITOK                                                           
*                                                                               
TSFL01   CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    TLKMEDC,TLKMEDC                                                  
         MVC   TLKMEDC,MEDCODE                                                  
         MVC   TLKMEDNM,MEDNAME                                                 
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
UPDF01   CLI   GSSMPAGE,2          ARE WE ON MEDIA CODE PAGE                    
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),        X        
               (2,=AL1(L'TLKMEDC,LIDTMED))                                      
         B     EXITOK                                                           
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1/2                                    *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING ROLRECD,R2                                                       
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
         USING ROLRECD,R2                                                       
         MVI   T.LIDEL,LIDELQ                                                   
         LA    R4,T.LIDDATA                                                     
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   ULST102             NO                                           
         MVI   T.LIDTYPE,LIDTPID                                                
         MVI   T.LIDITLN,L'SAPWDNUM                                             
         B     ULST114                                                          
ULST102  CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTMED                                                
         MVI   T.LIDITLN,L'TLKMEDC                                              
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
*                                                                               
ULST124  XR    RE,RE                                                            
         CLI   GSSMPAGE,1          ARE WE ON PID CODE PAGE                      
         BNE   ULST126             NO                                           
         LA    RE,L'SAPWDNUM-1                                                  
         EX    RE,*+4                                                           
         MVC   0(0,R4),TLKPIDB                                                  
         B     ULST128                                                          
*                                                                               
ULST126  IC    RE,T.LIDITLN                                                     
         SHI   RE,1                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R4),TLKMEDC                                                  
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
ULST131  CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTMED                                                
         MVI   T.LIDITLN,L'TLKMEDC                                              
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
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
MAXITMS  EQU   100                                                              
MED      DC    C'MEDIA'                                                         
OFFUL    DC    C'2D'                                                            
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
         B     VALPIDC                                                          
         B     VALMED                                                           
         B     VALKRO                                                           
         B     ADDRECS                                                          
         B     DELRECS                                                          
         B     DOFLT                                                            
         B     IOCHK                                                            
*                                                                               
OVROU1H  CLI   *,0                                                              
         B     OVROU1X                                                          
OVROU1L  CLI   *,FF                                                             
         B     OVROU1X                                                          
OVROU1E  CR    RB,RB                                                            
OVROU1X  XIT1  ,                                                                
         EJECT                                                                  
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
         MVC   PIDFSTNM,BCWORK+2                                                
         MVC   PIDLSTNM,BCWORK+22                                               
*                                                                               
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
*                                                                               
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
         MVC   PMDKEY,BCSPACES     READ MEDIA RECORD                            
         MVC   PMDKCPY,CUABIN      CONNECTED ID                                 
         MVI   PMDKTYP,PMDKTYPQ                                                 
         SHI   RE,1                                                             
         EX    RE,*+4                                                           
         MVC   PMDKMED(0),0(R2)                                                 
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('PMDELQ',PMDRECD),0                               
         BE    *+6                                                              
         DC    H'0'                PMDEL MISSING                                
         MVC   MEDNAME,BCSPACES                                                 
                                                                                
T        USING PMDELD,BOELEM                                                    
         MVC   MEDCODE,PMDKMED                                                  
         MVC   MEDNAME,T.PMDDESC                                                
         B     OVROU1E                                                          
         DROP  R5,T                                                             
         EJECT ,                                                                
***********************************************************************         
* CHECK STATUS KEY ROLE VALID                                         *         
*                                                                     *         
* EXIT - CC EQUAL = VALID                                             *         
*      - CC NOT EQUAL = INVALID                                       *         
***********************************************************************         
         SPACE 1                                                                
VALKRO   XC    IOKEY,IOKEY                                                      
         MVI   BOBYTE1,0                                                        
         USING ROLRECD,R4                                                       
         LA    R4,IOKEY                                                         
         MVI   ROLKTYP,ROLKTYPQ                                                 
         MVI   ROLKSUB,ROLKSUBQ                                                 
         MVC   ROLKCPY,CUABIN                                                   
         MVC   ROLKOFF,SAVOFF                                                   
         MVC   BOWORK1(ROLKNUM-ROLKEY),IOKEY                                    
         L     R1,=AL4(XOHI+XOACCDIR+XIO1)                                      
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
VALKR02  CLC   BOWORK1(ROLKNUM-ROLKEY),IOKEY                                    
         BNE   VALKR06                                                          
         CLI   CSACT,A#RES                IGNORE CURRENT RECORD UNLESS          
         BE    *+14                       RESTORING                             
         CLC   GSRECKEY(L'ROLKEY),IOKEY                                         
         BE    VALKR04                                                          
         TM    ROLKSTAT,ROLSSRKR                                                
         BNO   VALKR04                                                          
         CLI   BOBYTE1,2                                                        
         BNL   OVROU1L                                                          
         XR    R0,R0                                                            
         IC    R0,BOBYTE1                                                       
         AHI   R0,1                                                             
         STC   R0,BOBYTE1                                                       
VALKR04  L     R1,=AL4(XOSQ+XOACCDIR+XIO1)                                      
         GOTO1 AIO                                                              
         BE    VALKR02                                                          
         DC    H'0'                                                             
VALKR06  B     OVROU1E                                                          
         DROP  R4                                                               
         EJECT ,                                                                
***********************************************************************         
* UPDATE PASSIVE ROLE RECORDS                                         *         
*                                                                     *         
* NTRY - P1  = ROLE RECORD                                            *         
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
         USING ROLRECD,R3                                                       
         LA    R2,IOKEY                                                         
         MVC   IOKEY,0(R3)                                                      
         L     R1,=AL4(XORDD+XOACCDIR+XIO4) READ DIR FOR DA                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ROLEDA,IOKEY+ROLKDA-ROLRECD                                      
*                                                                               
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'A',(R3)),CPTRBLK,ROLEDA,0,ACOM                 
         B     OVROU1E                                                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* DELETE ROLE PASSIVES                                                *         
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
         USING ROLRECD,R3                                                       
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',CPTRBLK),0,0,ACOM               
*                                                                               
         B     OVROU1E                                                          
         DROP  R3                                                               
         EJECT ,                                                                
*********************************************************************           
* FILTER ROLE RECORDS                                               *           
* ENTRY - IOKEY = ROLE RECORD KEY                                   *           
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
         USING ROLRECD,R4                                                       
         L     R4,AIOREC           A(ROLE RECORD)                               
         CLI   SVSRKR,0            ANY STATUS REPORT KEY ROLE FILTER?           
         BE    DOFLT04                                                          
         CLI   SVSRKR,YES          STATUS REPORT KEY ROLE                       
         BNE   DOFLT02                                                          
         TM    ROLRSTAT,ROLSSRKR                                                
         BNO   OVROU1L                                                          
         B     DOFLT04                                                          
                                                                                
DOFLT02  TM    ROLRSTAT,ROLSSRKR                                                
         BNZ   OVROU1L                                                          
*                                                                               
DOFLT04  CLI   SVJOEM,0            ANY STATUS REPORT KEY ROLE FILTER?           
         BE    DOFLT08                                                          
         CLI   SVJOEM,YES          STATUS REPORT KEY ROLE                       
         BNE   DOFLT06                                                          
         TM    ROLRSTAT,ROLSJOEM                                                
         BNO   OVROU1L                                                          
         B     DOFLT08                                                          
                                                                                
DOFLT06  TM    ROLRSTAT,ROLSJOEM                                                
         BNZ   OVROU1L                                                          
*                                                                               
DOFLT08  NI    ROLINDS,FF-(ROLIFMED+ROLIFPID)                                   
         OC    SVMED,SVMED         ANY MEDIA FILTER?                            
         BNZ   *+8                                                              
         OI    ROLINDS,ROLIFMED    DON'T COMPARE MEDIA                          
         OC    SVPID,SVPID         ANY PID FILTER?                              
         BNZ   *+8                                                              
         OI    ROLINDS,ROLIFPID    DON'T COMPARE PID                            
*                                                                               
         LA    R4,ROLRFST-ROLRECD(R4)                                           
DOFLT10  TM    ROLINDS,ROLIFMED+ROLIFPID                                        
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
         CLI   LIDTYPE,LIDTMED                                                  
         BNE   DOFLT24                                                          
         TM    ROLINDS,ROLIFMED    COMPARE MEDIA?                               
         BO    DOFLT14             NO - MEDIA FOUND                             
         MVC   BOWORK1(L'SVMED),SVMED                                           
         B     DOFLT30                                                          
DOFLT24  CLI   LIDTYPE,LIDTPID                                                  
         BNE   DOFLT14                                                          
         TM    ROLINDS,ROLIFPID    COMPARE PID?                                 
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
DOFLT34  CLI   LIDTYPE,LIDTPID                                                  
         BNE   *+8                                                              
         LA    RE,L'TLKPIDB-1                                                   
         EXCLC RE,0(RF),BOWORK1                                                 
         BNE   DOFLT38                                                          
         CLI   LIDTYPE,LIDTMED                                                  
         BNE   *+12                                                             
         OI    ROLINDS,ROLIFMED    FOUND CLI/PRO/JOB                            
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTPID                                                  
         BNE   DOFLT14                                                          
         OI    ROLINDS,ROLIFPID    FOUND PID CODE                               
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
*                                                                               
OVROUT1  DS    0A                                                               
AVALPIDC DS    A                                VALPIDC                         
AVALMED  DS    A                                VALMED                          
AVALKRO  DS    A                                VALKRO                          
AADDRECS DS    A                                ADDRECS                         
ADELRECS DS    A                                DELRECS                         
ADOFLT   DS    A                                DOFLT                           
AIOCHK   DS    A                                IOCHK                           
OVROUT1N EQU   (*-OVROUT1)/L'OVROUT1                                            
*                                                                               
MNTDISP  DS    H                   MOVE TO SAVED STORAGE                        
CURDISP  DS    H                   CURRENT DISPLACENT INTO ELEMENT              
DATALEN  DS    H                   LENGTH OF DATA                               
TOTELLN  DS    H                   TOTAL LENGTH OF ELEMENT                      
*                                                                               
ROLINDS  DS    XL1                                                              
ROLIFMED EQU   X'80'               MEDIA CODE FOUND                             
ROLIFPID EQU   X'40'               PID FOUND                                    
*                                                                               
SCPYALP  DS    CL(L'CPYALPHA)      AGENCY ALPHA CODE                            
*                                                                               
SVIOKEY  DS    XL42                                                             
ROLEDA   DS    XL(L'ROLKDA)                                                     
SESNL    DS    XL1                                                              
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
SAVOFF   DS    XL(L'ROLKOFF)       OFFICE                                       
SVROL    DS    CL(L'ROLKNUM)       2 BYTE BINARY GROUP CODE                     
PIDCODE  DS    CL(L'SAPALPID)      8 CHARACTER PERSONAL ID                      
PIDBINY  DS    XL(L'SAPWDNUM)      2 BYTE BINARY PERSONAL ID                    
PIDFSTNM DS    CL(L'SANAME)        PERSONAL ID FIRST NAME                       
PIDLSTNM DS    CL(L'SANAME)        PERSONAL ID LAST NAME                        
PIDOFFC  DS    CL(L'LOCOFF)        OFFICE CODE                                  
MEDCODE  DS    CL(L'PMDKMED)       MEDIA CODE                                   
MEDNAME  DS    CL(L'PMDDESC)       MEDIA NAME                                   
*                                                                               
IOCOUNT  DS    H                   COUNT IO'S                                   
*                                                                               
SVFLTS   DS    0F                                                               
SVMED    DS    CL(L'PMDKMED)       FILTER ON MEDIA CODE                         
SVPIDB   DS    CL(L'SAPWDNUM)      FILTER ON BINARY PID                         
SVPID    DS    CL(L'SAPALPID)      FILTER ON PID                                
SVSRKR   DS    CL1                 FILTER ON STATUS REPORT KEY ROLE Y/N         
SVJOEM   DS    CL1                 FILTER ON RECEIVE JOB EMAIL ALERTS           
SVFLTLQ  EQU   *-SVFLTS                                                         
*                                                                               
SKEYLAST DS    XL(L'ACTKEY)        SAVED CONTINUATION KEY FOR FLST              
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
TLKMEDC  DS    CL1                 MEDIA CODE                                   
         DS    CL11                                                             
         ORG   TLUSER                                                           
TLKMEDNM DS    CL(L'PMDDESC)       MEDIA NAME                                   
         ORG   TLKMEDNM                                                         
TLKPIDB  DS    XL(L'SAPWDNUM)      BINARY PERSONAL ID                           
TLKPIDLN DS    CL(L'SANAME)        LAST NAME                                    
TLKPIDFN DS    CL(L'SANAME)        FIRST NAME                                   
TLKPIDOC DS    CL(L'LOCOFF)        OFFICE CODE                                  
         DS    XL20                                                             
         ORG                                                                    
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACFIL52   08/10/11'                                      
         END                                                                    
