*          DATA SET ACFIL54    AT LEVEL 004 AS OF 08/10/11                      
*PHASE T62354C,*                                                                
         SPACE 1                                                                
FIL54    TITLE 'TEMPLATE RECORD'                                                
         SPACE 2                                                                
FIL54    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL54**,RA,R7,RR=RE                                           
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
INIT     MVI   TEMINDS,0                                                        
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
         MVC   SAVOFF,CUACCS+1                                                  
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
         USING TPLRECD,R2                                                       
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
KFKVAL   XC    TPLKEY,TPLKEY       INITIALIZE KEY OF RECORD                     
         MVI   TPLKTYP,TPLKTYPQ                                                 
         MVI   TPLKSUB,TPLKSUBQ                                                 
         MVC   TPLKCPY,CUABIN      CONNECTED ID                                 
         MVC   TPLKOFF,SAVOFF                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    TPLKEY,TPLKEY       INITIALIZE KEY OF RECORD                     
         MVI   TPLKTYP,TPLKTYPQ                                                 
         MVI   TPLKSUB,TPLKSUBQ                                                 
         MVC   TPLKCPY,CUABIN      CONNECTED ID                                 
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
         USING TPLRECD,R2                                                       
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
         CLI   GSSMPAGE,1          ARE WE ON THE KEY STAGE PAGE?                
         BNE   EXITIACT            NO - INVALID ACTION FOR THIS SCREEN          
*                                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    MVC   FVADDR,AKSTFLD      SET CURSOR TO KEY STAGE FIELD                
         GOTO1 ADELRECS,BOPARM,TPLRECD                                          
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
RFWRT    GOTO1 ADELRECS,BOPARM,TPLRECD                                          
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
RLADD    GOTO1 AADDRECS,BOPARM,TPLRECD                                          
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
RLRES    GOTO1 AADDRECS,BOPARM,TPLRECD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE 1                                                                
RLWRT    GOTO1 AADDRECS,BOPARM,TPLRECD                                          
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
         USING TPLRECD,R2                                                       
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
         USING TPLRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(F#TEMP#OFF),AL1(0),AL3(TOFDTA)   USER-ID                     
         DC    AL2(F#TEMP#OFNM),AL1(0),AL3(OFNDTA)  NAME                        
         DC    AL2(F#TEMP#NUM),AL1(0),AL3(TMNDTA)   TEMPLATE NUMBER             
         DC    AL2(F#TEMP#KCD),AL1(0),AL3(KSTDTA)   KEY STAGE NUMBER            
         DC    AL2(F#TEMP#KNM),AL1(0),AL3(KSNDTA)   KEY STAGE NAME              
         DC    AL2(F#TEMP#FKCD),AL1(0),AL3(FKSTDTA) KEY STAGE FILTER            
         DC    AL2(F#TEMP#MEDL),AL1(0),AL3(MEDCDTA) MEDIA CODE                  
         DC    AL2(F#TEMP#MEDN),AL1(0),AL3(MEDNDTA) MEDIA NAME                  
         DC    AL2(F#TEMP#FMED),AL1(0),AL3(FMEDDTA) MEDIA FILTER                
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL54    CSECT                                                                  
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
         MVC   SVNUM,TPLKNUM       SAVE TEMPLATE/WORKFLOW NUMBER                
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
* DATA OBJECT FOR USER-ID                                             *         
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
DISTOFF  MVC   FVIFLD(L'TPLKOFF),TPLKOFF   OFFICE                               
         MVC   TPLOFF,TPLKOFF                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE/OFFICE LIST                                         *         
***********************************************************************         
         SPACE 1                                                                
VALTOFF  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         MVC   TPLKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'TPLKOFF),TPLKOFF                                       
                                                                                
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
DFLTFOFF MVC   FVIFLD(L'TPLKOFF),FLTIFLD                                        
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
         MVC   FVIFLD(L'TPLKOFF),CUACCS+2                                       
         B     DDFTOFF4                                                         
*                                                                               
DDFTOFF2 CLI   CUACCS,C'*'                                                      
         BNE   EXITOK                                                           
         MVC   FVIFLD(1),CUACCS+1                                               
         MVI   FVIFLD+1,X'40'                                                   
DDFTOFF4 MVC   TPLOFF,FVIFLD                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON OFFICE                                   *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLC   TPLKOFF,FLTIFLD                                                  
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
DISOFFN  OC    TPLKOFF,TPLKOFF     TEST ALL OFFICE                              
         BZ    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         TM    BCCPYST4,CPYSOFF2                                                
         BO    DISOFN02                                                         
         MVC   IOKEY,BCSPACES                 READ THE ACCOUNT RECORD           
         MVC   T.ACTKCPY,TPLKCPY              COMPANY                           
         MVC   T.ACTKUNT(L'OFFUL),OFFUL       UNIT/LEDGER                       
         MVC   T.ACTKACT(L'TPLKOFF),TPLKOFF   OFFICE CODE CODE                  
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
         MVC   T.OFFKCPY,TPLKCPY              COMPANY                           
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKOFF,TPLKOFF           OFFICE CODE CODE                     
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET OFFICE CODE NAME                         
         B     EXITOK                                                           
         EJECT ,                                                                
*                                                                               
***********************************************************************         
* DATA OBJECT FOR TEMPLATE NUMBER                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TMNDTA   LA    RF,TMNTBL                                                        
         B     ITER                                                             
*                                                                               
TMNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTMPN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTMPN)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISTMPN)                                
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETTMPN)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTTMPN)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTTMPN)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTMPN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETTMPN DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TEMPLATE NUMBER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISTMPN  OC    TPLKNUM,TPLKNUM                                                  
         BZ    EXITOK                                                           
         CURED TPLKNUM,(5,FVIFLD),0,ALIGN=LEFT,DMCB=BODMCB,ZERO=BLANK           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TEMPLATE NUMBER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VALTMPN  CLI   FVILEN,0                                                         
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
         STCM  RF,B'0011',TPLKNUM                                               
         STCM  RF,B'0011',SVNUM                                                 
*                                                                               
         CLI   CSACT,A#ADD         SKIP CHECKING, IF ADDING OR COPYING          
         BE    EXITOK                                                           
         CLI   CSACT,A#RES         AND RESTORE                                  
         BE    EXITOK                                                           
         CLI   CSACT,A#CPY                                                      
         BE    EXITOK                                                           
         LA    R4,IOKEY                                                         
X        USING TPLRECD,R4                                                       
         XC    X.TPLKEY,X.TPLKEY   READ ROLE RECORD                             
         MVI   X.TPLKTYP,TPLKTYPQ                                               
         MVI   X.TPLKSUB,TPLKSUBQ                                               
         MVC   X.TPLKCPY,CUABIN                                                 
         MVC   X.TPLKOFF,TPLKOFF                                                
         MVC   X.TPLKNUM,TPLKNUM                                                
         MVC   SVIOKEY,X.TPLKEY                                                 
         L     R1,=AL4(XORDD+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         BE    VTMPN02                                                          
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITNV                                                           
VTMPN02  MVC   FLTIFLD,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  X                                                                
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TEMPLATE NUMBER FILTER FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTTMPN MVC   FVIFLD,FLTIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TEMPLATE NUMBER FILTER FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VFLTTMPN TM    FVIIND,FVINUM   CHECK FVIFLD IS NUMERIC                          
         BNO   EXITNV                                                           
         ZIC   RE,FVILEN                                                        
         AHI   RE,-1                                                            
         EXMVC RE,FLTIFLD,FVIFLD                                                
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  MYDUB,FVIFLD(0)                                                  
         CVB   RF,MYDUB                                                         
         STCM  RF,B'0011',TPLKNUM                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON TEMPLATE NUMBER                                     *         
***********************************************************************         
         SPACE 1                                                                
DOFTMPN  OC    TPLKNUM,TPLKNUM     HAVE WE A PERSONAL ID TO FILTER ON           
         BZ    FLTXX               NO - WE DON`T WANT IT THEN                   
         CVB   RF,MYDUB                                                         
         STCM  RF,B'0011',MYBINA                                                
         CLC   TPLKNUM,MYBINA                                                   
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR KEY STAGE NUMBER                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
KSTDTA   LA    RF,KSTTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
KSTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISKST)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAAKST)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A KEY STAGE NUMBER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISKST   CURED TLKKST,(5,FVIFLD),0,ALIGN=LEFT,DMCB=BODMCB,ZERO=BLANK            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A KEY STAGE NUMBER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
VAAKST   MVC   AKSTFLD,FVADDR      SAVE A(FIELD)                                
         L     RF,FVADDR                                                        
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
         TM    FVIIND,FVINUM                                                    
         BNO   EXITNV                                                           
         OI    HEXIND,HEXKST                                                    
         GOTO1 AVALKSTG,BOPARM,(FVILEN,FVIFLD)                                  
         BE    VKST04                                                           
         MVC   FVMSGNO,=AL2(AE$INVKS) INVALID KEY STAGE                         
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
VKST04   MVC   TLKKST,KSTGNUM      SORT THE KEY STAGE NUMBERS                   
         MVC   TLKKSTNM,KSTGNAM    SAVE KEY STAGE NAME                          
*                                                                               
         B     EXITOK                                                           
         DROP  RF                                                               
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR KEY STAGE NAME                                     *          
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
KSNDTA   LA    RF,KSNTBL                                                        
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
KSNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISKSN)                                 
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY A KEYSTAGE NAME FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISKSN   MVC   FVIFLD(L'TLKKSTNM),TLKKSTNM                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON KEY STAGE FIELD              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FKSTDTA  LA    RF,FKSTTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FKSTTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFKST)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFKST)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFKST)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFKST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFKST DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER ON KEY STAGE FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DFLTFKST MVC   FVIFLD,FLTIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON KEY STAGE FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VFLTFKST CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM                                                    
         BNO   EXITNV                                                           
         OI    HEXIND,HEXKST                                                    
         GOTO1 AVALKSTG,BOPARM,(FVILEN,FVIFLD)                                  
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(AE$INVKS) INVALID KEY STAGE                         
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
         ZIC   RE,FVILEN                                                        
         AHI   RE,-1                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  MYDUB,FVIFLD(0)                                                  
         CVB   RF,MYDUB                                                         
         STCM  RF,B'0011',SVKST                                                 
         MVC   FLTIFLD,FVIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON KEYSTAGE                                 *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFKST B     FLTXE                                                            
         EJECT ,                                                                
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
         CLI   SREC,R#TEMP         TEMPLATE RECORD                              
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
THIS     USING TPLRECD,R2                                                       
LAST     USING TPLRECD,R3                                                       
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
X        USING TPLRECD,IOKEY                                                    
FLST     TM    LSSCIND1,LSSCIFLT   TEST FILTERS CHANGED                         
         BO    FLST02              YES, IGNORE ERRIND                           
         TM    ERRIND,ERMAXIO                                                   
         BZ    FLST02                                                           
         MVC   THIS.TPLKEY,SKEYLAST                                             
FLST02   MVI   ERRIND,0            RESET ERROR INDICATOR                        
         XC    SKEYLAST,SKEYLAST   OR CLEAR SAVED LAST KEY                      
*                                                                               
         MVC   X.TPLKEY,THIS.TPLKEY                                             
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
NLST02A  CLC   X.TPLKEY(TPLKREM-TPLRECD),THIS.TPLKEY                            
         BNE   EXITL               CHANGE COMPANY OR UNIT/LEDGER                
         OC    X.TPLKNUM,X.TPLKNUM                                              
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
         GOTO1 ATSTOFF,X.TPLKOFF                                                
         BE    NLST08                                                           
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         B     NLST                                                             
NLST08   MVC   THIS.TPLKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
         GOTO1 ADOFLT                 FILTER UNWANT RECORDS                     
         BNE   NLST                                                             
*                                                                               
*        MVC   THIS.ROLKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
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
FTFLST1  LA    RF,TPLRFST-TPLRECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1/2                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING TPLRECD,R5                                                       
FLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIOREC           A(RECORD)                                    
         C     R5,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    R5,TPLRFST          IT IS NOW.                                   
         XR    RE,RE                                                            
*                                                                               
         USING LIDELD,R5                                                        
FML01    CLI   LIDEL,0             RECORD END?                                  
         BE    EXITNO              YES                                          
         CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML18               NO                                           
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   FML02               NO                                           
         CLI   LIDTYPE,LIDTKYST    IS IT KEY STAGE LIST                         
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
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   FML12               NO                                           
         L     RF,AVALKSTG         SET TO VALIDATE KEY STAGE NUMBER             
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
         USING TPLRECD,R5                                                       
NLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIOREC           A(RECORD)                                    
         C     R5,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    NML20                                                            
         LA    R5,TPLRFST          IT IS NOW.                                   
*                                                                               
         USING LIDELD,R5                                                        
NML02    CLI   LIDEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML18               YES                                          
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   NML03               NO                                           
         CLI   LIDTYPE,LIDTKYST    IS IT KEY STAGE LIST                         
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
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   NML22               NO                                           
         L     RF,AVALKSTG         SET TO VALIDATE KEY STAGE                    
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
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   NML32               NO                                           
         L     RF,AVALKSTG         SET TO VALIDATE KEY STAGE                    
         B     NML38                                                            
NML32    CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         L     RF,AVALMED          SET TO VALIDATE MEDIA                        
NML38    GOTO1 (RF),BOPARM,(LIDITLN,LIDDATA)                                    
         BNE   NML20               NOT VALID MEDIA/KEY STAGE                    
                                                                                
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
                                                                                
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   TSFL01              NO                                           
         XC    TLKKST,TLKKST                                                    
         MVC   TLKKST,KSTGNUM                                                   
         MVC   TLKKSTNM,KSTGNAM                                                 
         B     EXITOK                                                           
                                                                                
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
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE NUMBER PAGE              
         BNE   UPDF01              NO                                           
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',AIOREC),        X        
               (2,=AL1(L'TLKKST,LIDTKYST))                                      
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
         USING TPLRECD,R2                                                       
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
         BNE   EXITOK              NO                                           
*                                                                               
ULST101  XC    BOELEM,BOELEM                                                    
T        USING LIDELD,BOELEM                                                    
         USING TPLRECD,R2                                                       
         MVI   T.LIDEL,LIDELQ                                                   
         LA    R4,T.LIDDATA                                                     
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   ULST102             NO                                           
         MVI   T.LIDITLN,L'TLKKST                                               
         MVI   T.LIDTYPE,LIDTKYST                                               
         B     ULST114                                                          
ULST102  CLI   GSSMPAGE,2          ARE WE ON MEDIA PAGE                         
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVI   T.LIDITLN,L'TLKMEDC                                              
         MVI   T.LIDTYPE,LIDTMED                                                
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
         IC    RE,T.LIDITLN                                                     
         SHI   RE,1                                                             
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   ULST126             NO                                           
         MVC   0(0,R4),TLKKST                                                   
         EX    RE,*-6                                                           
         B     ULST128                                                          
*                                                                               
ULST126  MVC   0(0,R4),TLKMEDC                                                  
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
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   ULST131             NO                                           
         MVI   T.LIDITLN,2                                                      
         MVI   T.LIDTYPE,LIDTKYST                                               
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
         B     VALKSTG                                                          
         B     VALMED                                                           
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
* VALIDATE KEY STAGE NUMBER AND RETURN NAME                           *         
*                                                                     *         
* NTRY - P1  = KEYSTAGE NUMBER                                        *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALKSTG  L     R2,0(R1)                                                         
         ZIC   RE,0(R1)                                                         
         AHI   RE,-1                                                            
         TM    HEXIND,HEXKST                                                    
         BNO   VKSTG02                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  BODUB1,0(0,R2)                                                   
         CVB   RF,BODUB1                                                        
         STCM  RF,B'0011',KSTGNUM                                               
         STCM  RF,B'0011',BOHALF1                                               
         NI    HEXIND,X'FF'-HEXKST                                              
         B     VKSTG04                                                          
*                                                                               
VKSTG02  EXMVC RE,KSTGNUM,0(R2)                                                 
         MVC   BOHALF1,KSTGNUM                                                  
*                                                                               
VKSTG04  LA    R5,IOKEY                                                         
         USING KSTRECD,R5                                                       
         XC    KSTKEY,KSTKEY       READ KEY STAGE RECORD                        
         MVI   KSTKTYP,KSTKTYPQ                                                 
         MVI   KSTKSUB,KSTKSUBQ                                                 
         MVC   KSTKCPY,CUABIN      CONNECTED ID                                 
         MVC   KSTKOFF,SAVOFF                                                   
         TM    BCCPYSTC,CPYSROFF   COMPANY ENFORCE OFFICES                      
         BNO   *+10                                                             
         MVC   KSTKOFF,TPLOFF                                                   
         MVC   KSTKNUM,KSTGNUM                                                  
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   OVROU1L                                                          
         L     R5,AIO2                                                          
         LA    R3,KSTRFST                                                       
         USING KSTELD,R3                                                        
         SR    R1,R1                                                            
VALKST02 CLI   KSTEL,0                                                          
         BE    OVROU1L                                                          
         CLI   KSTEL,KSTELQ                                                     
         BE    VALKST06                                                         
VALKST04 IC    R1,KSTLN                                                         
         AR    R3,R1                                                            
         B     VALKST02                                                         
*                                                                               
VALKST06 CLC   KSTCODE,BOHALF1                                                  
         BNE   VALKST04                                                         
         MVC   KSTGNAM,BCSPACES                                                 
         IC    R1,KSTLN                                                         
         SHI   R1,KSTLN1Q+1                                                     
         MVC   KSTGNAM(0),KSTNAME                                               
         EX    R1,*-6                                                           
         B     OVROU1E                                                          
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
         MVC   PMDKEY,BCSPACES     READ NON CLIENT ACCOUNT RECORD               
         MVC   PMDKCPY,CUABIN      CONNECTED ID                                 
         MVI   PMDKTYP,PMDKTYPQ                                                 
         SHI   RE,1                                                             
         MVC   PMDKMED(0),0(R2)                                                 
         EX    RE,*-6                                                           
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
         DROP  R5                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* UPDATE PASSIVE TEMPLATE RECORDS                                     *         
*                                                                     *         
* NTRY - P1  = TEMPLATE RECORD                                        *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
ADDRECS  CLI   CSACT,A#RES         ACTION RESTORE                               
         BE    AREC01              YES - ADD ALL PASSIVE POINTERS               
         CLI   CSACT,A#ADD         ACTION ADD                                   
         BE    AREC01              YES - ADD ALL PASSIVE POINTERS               
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   OVROU1E             NO                                           
AREC01   L     R3,0(R1)                                                         
         USING TPLRECD,R3                                                       
         LA    R2,IOKEY                                                         
         MVC   IOKEY,0(R3)                                                      
         L     R1,=AL4(XORDD+XOACCDIR+XIO4) READ DIR FOR DA                     
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TEMPDA,IOKEY+TPLKDA-TPLRECD                                      
*                                                                               
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'A',(R3)),CPTRBLK,TEMPDA,0,ACOM                 
         B     OVROU1E                                                          
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* DELETE TEMPLATE PASSIVES                                            *         
*                                                                     *         
* NTRY - P1  = PID LIST ELEMENT                                       *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
DELRECS  CLI   CSACT,A#DEL         ACTION DELETE                                
         BE    *+12                YES - DELETE ALL PASSIVE POINTERS            
         CLI   GSSMPAGE,1          ARE WE ON KEY STAGE PAGE                     
         BNE   OVROU1E             NO                                           
         L     R3,0(R1)            GET RECORD ADDRESS                           
         USING TPLRECD,R3                                                       
         USING CPTRBLK,CPTRWRK                                                  
         XC    CPTRBLK(CPTRBLKL),CPTRBLK                                        
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',CPTRBLK),0,0,ACOM               
*                                                                               
         B     OVROU1E                                                          
         DROP  R3                                                               
         EJECT ,                                                                
*********************************************************************           
* FILTER TEMPLATE RECORDS                                           *           
* ENTY - IOKEY = TEMPLATE RECORD KEY                                *           
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
         NI    TEMINDS,FF-(TEMIFMED+TEMIFKST)                                   
         OC    SVMED,SVMED         ANY MEDIA FILTER?                            
         BNZ   *+8                                                              
         OI    TEMINDS,TEMIFMED    DON'T COMPARE MEDIA                          
         OC    SVKST,SVKST         ANY KEY STAGE FILTER                         
         BNZ   *+8                                                              
         OI    TEMINDS,TEMIFKST    DON'T COMPARE KEY STAGE                      
*                                                                               
         L     R4,AIOREC           A(TEMPLATE RECORD)                           
         LA    R4,TPLRFST-TPLRECD(R4)                                           
DOFLT10  TM    TEMINDS,TEMIFMED+TEMIFKST                                        
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
         TM    TEMINDS,TEMIFMED    COMPARE MEDIA?                               
         BO    DOFLT14             NO - MEDIA FOUND                             
         MVC   BOWORK1(L'SVMED),SVMED                                           
         B     DOFLT30                                                          
DOFLT24  CLI   LIDTYPE,LIDTKYST                                                 
         BNE   DOFLT14                                                          
         TM    TEMINDS,TEMIFKST    COMPARE KEY STAGE?                           
         BO    DOFLT14             NO - KEY STAGE FOUND                         
         MVC   BOWORK1(L'SVKST),SVKST                                           
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
DOFLT34  CLI   LIDTYPE,LIDTKYST                                                 
         BNE   *+8                                                              
         LA    RE,L'TLKKST-1                                                    
         EXCLC RE,0(RF),BOWORK1                                                 
         BNE   DOFLT38                                                          
         CLI   LIDTYPE,LIDTMED                                                  
         BNE   *+12                                                             
         OI    TEMINDS,TEMIFMED    FOUND MEDIA                                  
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTKYST                                                 
         BNE   DOFLT14                                                          
         OI    TEMINDS,TEMIFKST    FOUND KEY STAGE CODE                         
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
AKSTFLD  DS    A                   A(KEY STAGE NUMBER FIELD)                    
*                                                                               
OVROUT1  DS    0A                                                               
AVALKSTG DS    A                                VALKSTG                         
AVALMED  DS    A                                VALMED                          
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
TEMINDS  DS    XL1                                                              
TEMIFMED EQU   X'80'               MEDIA CODE FOUND                             
TEMIFKST EQU   X'20'               KEY STAGE FOUND                              
*                                                                               
HEXIND   DS    XL1                                                              
HEXKST   EQU   X'80'               FIELD IS NOT IN HEX                          
*                                                                               
SCPYALP  DS    CL(L'CPYALPHA)      AGENCY ALPHA CODE                            
TPLOFF   DS    CL(L'TRNOFFC)       OFFICE                                       
*                                                                               
SVIOKEY  DS    XL42                                                             
TEMPDA   DS    XL(L'ROLKDA)                                                     
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
SAVOFF   DS    XL(L'TPLKOFF)       OFFICE/OFFICE LIST                           
SVNUM    DS    CL(L'TPLKNUM)       2 CHARACTER TEMPLATE NUMBER                  
KSTGNUM  DS    CL2                 2 CHARACTER KEY STAGE NUMBER                 
KSTGNAM  DS    CL(L'NAMEREC)       KEY STAGE NAME                               
MEDCODE  DS    CL(L'PMDKMED)       MEDIA CODE                                   
MEDNAME  DS    CL(L'PMDDESC)       MEDIA NAME                                   
*                                                                               
IOCOUNT  DS    H                   COUNT IO'S                                   
*                                                                               
SVFLTS   DS    0F                                                               
SVMED    DS    CL(L'PMDKMED)       FILTER ON MEDIA CODE                         
SVKST    DS    CL2                 FILTER ON KEY STAGE NUMBER                   
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
TLKKST   DS    CL2                 KEY STAGE NUMBER                             
         DS    XL4                                                              
         ORG   TLKKST                                                           
TLKMEDC  DS    CL1                 MEDIA CODE                                   
         DS    CL11                                                             
         ORG   TLUSER                                                           
TLKMEDNM DS    CL(L'PMDDESC)       MEDIA NAME                                   
         ORG   TLKMEDNM                                                         
TLKKSTNM DS    CL(L'NAMEREC)       KEY STAGE NAME                               
         DS    XL22                                                             
         ORG                                                                    
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACFIL54   08/10/11'                                      
         END                                                                    
