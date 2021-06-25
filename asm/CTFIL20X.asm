*          DATA SET CTFIL20X   AT LEVEL 036 AS OF 08/22/00                      
*&&      SET   NOP=N                                                            
*PHASE TA1320A                                                                  
*INCLUDE GETBOOK                                                                
*INCLUDE SQUASHER                                                               
*                                                                               
         PRINT NOGEN                                                            
         TITLE 'NEW FILE BOOK RECORDS'                                          
FIL20    START                                                                  
         NMOD1 0,CTFIL20*,R6,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         LH    R7,=Y(TWSAVE-TWAD)                                               
         A     R7,ATWA                                                          
         USING MYSAVED,R7                                                       
         ST    RE,BORELO                                                        
         ST    RB,BOBASE1                                                       
         ST    R6,BOBASE2                                                       
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
GSFRP    USING FRPELD,GSFRPEL                                                   
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T                                        
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
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
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(SUB)                                 
         DC    AL1(OREP),AL1(0,0,0),AL4(REP)                                    
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFK)                                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
*                                                                               
         L     RF,=V(GETBOOK)                                                   
         A     RF,BORELO                                                        
         ST    RF,AGETBOOK                                                      
*                                                                               
         L     RF,=A(SCRAMBLE)                                                  
         A     RF,BORELO                                                        
         ST    RF,ASCRMBLE                                                      
*                                                                               
         L     RF,=V(SQUASHER)                                                  
         A     RF,BORELO                                                        
         ST    RF,ASQUASH                                                       
*                                                                               
         MVI   SCRFLAG1,0          RESET SCROLL FLAG                            
         MVI   SCRFLAG2,0          RESET REDISPLAY FLAG                         
*                                                                               
         CLI   CSACT,A#LST                                                      
         BE    EXITOK                                                           
*                                                                               
         LH    RF,GSDSPSCR                                                      
         A     RF,ATWA                                                          
         USING FHD,RF                                                           
         OI    FHII,FHIIVA                                                      
         MVI   GSSCRNUM,GSKIHALF                                                
         MVC   FHDA(L'FL@HALF),FL@HALF                                          
         OI    FHOI,FHOITR                                                      
         DROP  RF                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING FDRRECD,R2                                                       
         LA    RF,TABLKEY                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
TABLKEY  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KEYVAL)                                 
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KEYFVAL)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY VALIDATE                                         *         
***********************************************************************         
         SPACE 1                                                                
KEYVAL   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY FILTER VALIDATE                                  *         
***********************************************************************         
         SPACE 1                                                                
KEYFVAL  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
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
         USING FDRRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT                                        *         
***********************************************************************         
         SPACE 1                                                                
RECFRST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RFTABL   DC    AL1(RDIS),AL1(0,0,0),AL4(RFDISS)                                 
         DC    AL1(RVAL),AL1(0,0,0),AL4(RFVALS)                                 
         DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RFRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFWRT)                                  
         DC    AL1(RCPY),AL1(0,0,0),AL4(RFCPY)                                  
         DC    AL1(RREN),AL1(0,0,0),AL4(RFREN)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DISPLAY                              *         
***********************************************************************         
         SPACE 1                                                                
RFDISS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - VALIDATE                             *         
***********************************************************************         
         SPACE 1                                                                
RFVALS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD                                  *         
***********************************************************************         
         SPACE 1                                                                
RFADD    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RESTORE                              *         
***********************************************************************         
         SPACE 1                                                                
RFRES    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - WRITE TO FILE                        *         
***********************************************************************         
         SPACE 1                                                                
RFWRT    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - COPY RECORD                          *         
***********************************************************************         
         SPACE 1                                                                
RFCPY    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RENAME RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
RFREN    DS    0H                                                               
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
RLTABL   DC    AL1(RDIS),AL1(0,0,0),AL4(RLDIS)                                  
         DC    AL1(RVAL),AL1(0,0,0),AL4(RLVAL)                                  
         DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RCPY),AL1(0,0,0),AL4(RLCPY)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DISPLAY                               *         
***********************************************************************         
         SPACE 1                                                                
RLDIS    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - VALIDATE                              *         
***********************************************************************         
         SPACE 1                                                                
RLVAL    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE 1                                                                
RLADD    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - COPY                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING CTINDD,BOELEM                                                    
TOKY     USING CTJREC,GSRECKEY                                                  
FRKY     USING CTJREC,GSCPYKEY                                                  
TEMP     USING CTJREC,IOKEY                                                     
*                                                                               
RLCPY    GOTOX AGETEL,BOPARM,('CTINDELQ',AIOREC)                                
         BNE   EXITL                                                            
         XR    R2,R2                                                            
*                                                                               
RLCP02   LA    R1,CTINDEX(R2)                                                   
         CLI   0(R1),0             REACHED END OF THOSE ACTIVE?                 
         BE    EXITOK              YES                                          
*                                                                               
         MVC   TEMP.CTJKEY,FRKY.CTJKEY                                          
         IC    RF,0(R1)            THIS INDEX NUMBER                            
         STCM  RF,1,TEMP.CTJKSUB                                                
         L     R1,=AL4(XORDD+XOCONFIL+XIO2)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                WHERE IS THE COPY FROM RECORD?               
*                                                                               
         MVC   TEMP.CTJKID,TOKY.CTJKID                                          
         L     R1,=AL4(XORDUPD+XOCONFIL+XIO1)                                   
         GOTOX ('XIO',AGROUTS)                                                  
*                                                                               
         L     R0,AIO1             COPY TO SUB-RECORD                           
         L     RE,AIO2             COPY FROM SUB-RECORD                         
         XR    RF,RF                                                            
         ICM   RF,3,CTJLEN-CTJKEY(RE)                                           
         LR    R1,RF               LENGTH IS LENGTH OF RECORD                   
         MVCL  R0,RE               COPY COPY FROM SUB-RECORD                    
*                                                                               
         L     RF,AIO1                                                          
NEW      USING CTJREC,RF                                                        
         MVC   NEW.CTJKID,TOKY.CTJKID                                           
         NI    NEW.CTJSTAT,FF-X'80'                                             
         DROP  NEW                                                              
*                                                                               
         LA    R1,XOWRITE                                                       
         TM    IOERR,IOERNF        RECORD ON FILE?                              
         BZ    *+8                 YES                                          
         LA    R1,XOADD                                                         
*                                                                               
         AH    R1,=Y(XOCONFIL+XIO1)                                             
         GOTOX ('XIO',AGROUTS)     PUT RECORD TO FILE                           
*                                                                               
         LA    R2,1(R2)                                                         
         B     RLCP02                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE 1                                                                
RLDEL    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE                               *         
***********************************************************************         
         SPACE 1                                                                
RLRES    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE TO FILE                         *         
***********************************************************************         
         SPACE 1                                                                
RLWRT    DS    0H                                                               
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
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING FDRRECD,R2                                                       
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
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING CTJREC,R2                                                        
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
KNOWTAB  DC    AL2(01000),AL4(RTYDTA)    RECORD TYPE                            
         DC    AL2(01001),AL4(NAMDTA)    BOOK NAME                              
         DC    AL2(01002),AL4(DSCDTA)    DESCRIPTION                            
         DC    AL2(01004),AL4(LINDTA)    DATA LINE                              
         DC    AL2(01005),AL4(NUMDTA)    LINE NUMBER                            
         DC    AL2(01006),AL4(SEQDTA)    START SEQUENCE NUMBER                  
         DC    AL2(01007),AL4(COLDTA)    START COLUMN NUMBER                    
         DC    AL2(01008),AL4(SHWDTA)    SHOW INCLUDE EXPANSION STATE           
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL20    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDFDIS)    DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFDFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFDFDIS  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA FILTER                            *         
***********************************************************************         
         SPACE 1                                                                
DFDFVAL  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3         VERB IN R1                                   
         LA    RF,DLTABL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DLDFDIS)    DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DLDFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
DLDDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT (IN CASE OF COPY)           *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
DLDFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DLDFVAL  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD TYPE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RTYDTA   LA    RF,RTYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RTYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRTY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRTY)                                 
         DC    AL1(DRDIS),AL1(0,0,0),AL4(DISRTY)                                
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALRTY)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTRTY)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTRTY)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTRTY)                                
         DC    AL1(EOT)                                                         
*                                                                               
RTYVTAB  DC    C'J',AL1(CTJKTYPQ,0)                                             
         DC    C'L',AL1(CTLKTYPQ,0)                                             
         DC    C'Q',AL1(CTLKTYPQ,CTLKXTRQ)                                      
         DC    C'S',AL1(CTLKTYPQ,CTLKSCRQ)                                      
         DC    AL1(EOT)                                                         
*                                                                               
RTYVTABD DSECT                                                                  
RTYVTYPE DS    CL1                                                              
RTYVCODE DS    AL2                                                              
RTYVLQ   EQU   *-RTYVTABD                                                       
*                                                                               
FIL20    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD TYPE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISRTY   LA    RF,RTYVTAB                                                       
         USING RTYVTABD,RF                                                      
*                                                                               
DRTY02   CLI   RTYVTYPE,EOT        FOUND IT?                                    
         BE    EXITNV                                                           
         CLC   RTYVCODE,CTJKEY                                                  
         BE    *+12                                                             
         LA    RF,RTYVLQ(RF)                                                    
         B     DRTY02                                                           
*                                                                               
         MVC   FVIFLD(L'RTYVTYPE),RTYVTYPE                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD TYPE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALRTY   LA    RF,RTYVTAB                                                       
         USING RTYVTABD,RF                                                      
*                                                                               
VRTY02   CLI   RTYVTYPE,EOT                                                     
         BE    EXITNV                                                           
         CLC   RTYVTYPE,FVIFLD                                                  
         BE    *+12                                                             
         LA    RF,RTYVLQ(RF)                                                    
         B     VRTY02                                                           
*                                                                               
         MVC   CTJKEY(L'RTYVCODE),RTYVCODE                                      
         CLI   CTJKEY,CTJKTYPQ     JCL BOOKS HAVE EXTRA VALIDATION              
         BNE   EXITOK                                                           
                                                                                
*&&UK*&& B     EXITOK              EXCEPT THEY DON'T IN THE UK                  
*                                                                               
*&&US                                                                           
         CLI   CSACT,A#DIS         ALLOWED TO DISPLAY                           
         BE    EXITOK                                                           
         CLI   CSACT,A#REPT        ALLOWED TO REPORT                            
         BE    EXITOK                                                           
*                                                                               
         GOTOX VGETFACT,BOPARM,0                                                
         L     R1,0(R1)                                                         
         CLI   FASYSID-FACTSD(R1),1                                             
         BE    EXITOK                                                           
         TM    CUSTAT,CUSPER       MUST HAVE PERSONAL PASSWORD                  
         BZ    EXITNV                                                           
         CLC   CUAALF,=C'**'       FOR AGENCY **                                
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD TYPE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTRTY  CLC   FLTIFLD(L'RTYVCODE),=AL1(CTLKTYPQ,CTLKSCRQ)                      
         BNE   *+12                                                             
         MVI   FVIFLD,C'S'         SPECIAL FOR SCRIPT SOURCE                    
         B     EXITOK                                                           
*                                                                               
         CLC   FLTIFLD(L'RTYVCODE),=AL1(CTLKTYPQ,CTLKXTRQ)                      
         BNE   *+12                                                             
         MVI   FVIFLD,C'Q'         SPECIAL FOR XTRACT BOOKS                     
         B     EXITOK                                                           
*                                                                               
         MVC   FVIFLD(1),FLTIFLD   ALL OTHERS ARE AS KEY                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD TYPE FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTRTY  LA    RF,RTYVTAB                                                       
         USING RTYVTABD,RF                                                      
*                                                                               
VFRTY02  CLI   RTYVTYPE,EOT                                                     
         BE    EXITNV                                                           
         CLC   RTYVTYPE,FVIFLD                                                  
         BE    *+12                                                             
         LA    RF,RTYVLQ(RF)                                                    
         B     VFRTY02                                                          
*                                                                               
         MVC   CTJKEY(L'RTYVCODE),RTYVCODE                                      
         MVC   FLTIFLD(L'RTYVCODE),RTYVCODE                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO RECORD TYPE FILTERING                                            *         
***********************************************************************         
         SPACE 1                                                                
DOFTRTY  CLC   CTJKEY(L'RTYVCODE),FLTIFLD                                       
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR RECORD NAME                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NAMDTA   LA    RF,NAMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NAMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISNAMN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNAM)                                 
         DC    AL1(DRDIS),AL1(0,0,0),AL4(DISNAM)                                
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALNAM)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTNAM)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTNAM)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTNAM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   MVC   FVIFLD(L'CTJKID),CTJKID                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD NAME FIELD FOR NTRSES                              *         
***********************************************************************         
         SPACE 1                                                                
DISNAMN  LA    RF,SCRPNAME                                                      
         CLC   0(L'SCRPNAME,RF),BCSPACES                                        
         BH    *+8                                                              
         LA    RF,CTJKID                                                        
         MVC   FVIFLD(L'CTJKID),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD NAME                                              *         
***********************************************************************         
         SPACE 1                                                                
VALNAM   MVC   CTJKID,FVIFLD                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD NAME FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTNAM  MVC   FVIFLD(L'CTJKID),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD NAME FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTNAM  MVC   CTJKID,FVIFLD                                                    
         MVC   FLTIFLD(L'CTJKID),FVIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO RECORD NAME FILTERING                                            *         
***********************************************************************         
         SPACE 1                                                                
DOFTNAM  CLC   CTJKID,FLTIFLD                                                   
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR RECORD DESCRIPTION                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DSCDTA   LA    RF,DSCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDSC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD DESCRIPTION FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DISDSC   GOTOX AGETEL,BOPARM,('CTDSCELQ',AIOREC),0                              
         BNE   EXITOK                                                           
DESC     USING CTDSCD,BOELEM                                                    
         XR    RF,RF                                                            
         IC    RF,DESC.CTDSCLEN                                                 
         SH    RF,=Y(CTDSC-CTDSCD+1)                                            
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),DESC.CTDSC                                             
         B     EXITOK                                                           
         DROP  DESC                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD DESCRIPTION FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VALDSC   GOTOX ADELEL,BOPARM,('CTDSCELQ',AIOREC),0                              
         CLI   FVILEN,0            ANY DESCRIPTION?                             
         BE    EXITOK              NO                                           
*                                                                               
DESC     USING CTDSCD,BOELEM       BUILD NEW DESCRIPTION ELEMENT                
         MVI   DESC.CTDSCEL,CTDSCELQ                                            
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   DESC.CTDSC(0),FVIFLD                                             
         AH    RF,=Y(CTDSC-CTDSCD+1)                                            
         STC   RF,DESC.CTDSCLEN                                                 
*                                                                               
         GOTOX AADDEL,BOPARM,AIOREC                                             
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(CE#RECTB)  RECORD TOO BIG                           
         B     EXITL                                                            
         DROP  DESC                                                             
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR ONE LINE OF RECORD DATA                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LINDTA   LA    RF,LINTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LINTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLIN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLIN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LINE OF RECORD DATA                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
DISLIN   LH    RF,COLNUM           START COLUMN                                 
         BCTR  RF,0                ZERO BASE START COLUMN                       
         LA    RF,TLLINE(RF)       DISPLACEMENT INTO LINE                       
         LA    RE,TLLINE+(L'TLLINE-1)                                           
         SR    RE,RF                                                            
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),0(RF)     MOVE DATA OUT TO LINE                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD DESCRIPTION FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VALLIN   LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CH    RF,=H'250'          MAX OF 250 LINES IN A BOOK                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         B     EXITL                                                            
*                                                                               
         LH    RF,COLNUM           START COLUMN                                 
         BCTR  RF,0                ZERO BASE START COLUMN                       
         LA    RF,TLLINE(RF)       DISPLACEMENT INTO LINE                       
         LA    RE,TLLINE+(L'TLLINE-1)                                           
         SR    RE,RF                                                            
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      MOVE DATA BACK INTO TSAR RECORD              
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LINE NUMBER IN RECORD DATA                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NUMDTA   LA    RF,NUMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NUMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNUM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LINE NUMBER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
DISNUM   MVC   FVIFLD(L'TLLNUM),TLLNUM                                          
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR START COLUMN IN RECORD                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
COLDTA   LA    RF,COLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
COLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOL)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFTCOL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY START COLUMN IN DATA LINE                                   *         
***********************************************************************         
         SPACE 1                                                                
DISCOL   XR    RF,RF                                                            
         ICM   RF,3,COLNUM         GET COLUMN NUMBER                            
         BNZ   *+8                                                              
         LA    RF,1                                                             
         STH   RF,COLNUM                                                        
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE START COLUMN IN DATA LINE                                  *         
***********************************************************************         
         SPACE 1                                                                
VALCOL   CLI   BCPFKEY,PFK09       SCROLL LEFT?                                 
         BNE   VCOL02                                                           
         LH    RF,COLNUM                                                        
         SH    RF,=Y(HSCRAMT)      HORIZONTAL SCROLL AMOUNT                     
         BP    *+8                                                              
         LA    RF,1                MINIMUM IS 1                                 
         STH   RF,COLNUM                                                        
         NI    BCINDS1,FF-BCIANYPF ALLOW LIST TO BE REDISPLAYED                 
         OI    SCRFLAG1,LSSCIINP   KEEP CURRENT SCREEN                          
         OI    SCRFLAG2,LSSCIPAG   SET REDISPLAY                                
         B     EXITOK                                                           
*                                                                               
VCOL02   CLI   BCPFKEY,PFK10       SCROLL RIGHT?                                
         BNE   VCOL04                                                           
         LH    RF,COLNUM                                                        
         AH    RF,=Y(HSCRAMT)      HORIZONTAL SCROLL AMOUNT                     
         CH    RF,=Y(LENLINE)                                                   
         BL    *+8                                                              
         LA    RF,LENLINE          MAXIMUM IS LENGTH OF A LINE                  
         STH   RF,COLNUM                                                        
         NI    BCINDS1,FF-BCIANYPF ALLOW LIST TO BE REDISPLAYED                 
         OI    SCRFLAG1,LSSCIINP   KEEP CURRENT SCREEN                          
         OI    SCRFLAG2,LSSCIPAG   SET REDISPLAY                                
         B     EXITOK                                                           
*                                                                               
VCOL04   CLI   FVILEN,0            IF INPUT, MUST BE A NUMBER                   
         BNE   *+14                                                             
         MVC   COLNUM,=AL2(1)      DEFAULT IS COLUMN 1                          
         B     EXITOK                                                           
*                                                                               
         TM    FVIIND,FVINUM       IF INPUT, MUST BE A NUMBER                   
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL                                                     
         BZ    EXITNV              MUST BE RANGE 1-72                           
         CH    RF,=Y(LENLINE)                                                   
         BH    EXITNV                                                           
         CH    RF,COLNUM                                                        
         BE    *+12                                                             
         OI    SCRFLAG1,LSSCIINP   KEEP CURRENT SCREEN                          
         OI    SCRFLAG2,LSSCIPAG   SET REDISPLAY                                
         STH   RF,COLNUM           SAVE COLUMN NUMBER                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT START COLUMN IN DATA LINE                               *         
***********************************************************************         
         SPACE 1                                                                
DFTCOL   MVI   FVIFLD,C'1'         SET DEFAULT TO COLUMN 1                      
         CLC   COLNUM,=AL2(1)                                                   
         BE    EXITOK              UNCHANGED                                    
         OI    SCRFLAG1,LSSCIINP   KEEP CURRENT SCREEN                          
         OI    SCRFLAG2,LSSCIPAG   SET REDISPLAY                                
         MVC   COLNUM,=AL2(1)                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR START SEQUENCE NUMBER                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SEQDTA   LA    RF,SEQTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SEQTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSEQ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEQ)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFTSEQ)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY START SEQUENCE NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSEQ   XR    RF,RF                                                            
         ICM   RF,3,SEQNUM         TEST SEQUENCE NUMBER SET                     
         BZ    EXITOK                                                           
*                                                                               
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE START SEQUENCE NUMBER IN LINE                              *         
***********************************************************************         
         SPACE 1                                                                
VALSEQ   MVC   ASEQFLD,FVADDR      SAVE A(SEQUENCE NUMBER FIELD)                
*                                                                               
         CLC   GSRECKEY,GCLASKEY   RESET SEQUENCE IF KEY CHANGES                
         BE    *+10                                                             
         XC    SEQLAST,SEQLAST                                                  
*                                                                               
         CLI   FVILEN,0            IF INPUT, MUST BE A NUMBER                   
         BNE   *+14                                                             
         XC    SEQNUM,SEQNUM                                                    
         B     EXITOK                                                           
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         ICM   RF,15,BCFULL                                                     
         BZ    EXITNV                                                           
         STH   RF,SEQNUM                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY START SEQUENCE NUMBER DEFAULT                               *         
***********************************************************************         
         SPACE 1                                                                
DFTSEQ   MVI   FVIFLD,C'1'                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SHOW EXPANDED INCLUDE BOOKS                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SHWDTA   LA    RF,SHWTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SHWTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSHW)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSHW)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EXPANDED SHOW STATUS                                        *         
***********************************************************************         
         SPACE 1                                                                
DISSHW   LA    RF,NESTSHOW                                                      
         CLI   NESTSHOW,NESTERR                                                 
         BNE   *+8                                                              
         LA    RF,NESTLAST                                                      
         MVC   FVIFLD(L'FL@NO),FL@NO                                            
         CLI   0(RF),NESTNO                                                     
         BE    EXITOK                                                           
         MVC   FVIFLD(L'FL@YES),FL@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE EXPANDED SHOW STATUS                                       *         
***********************************************************************         
         SPACE 1                                                                
VALSHW   CLI   NESTSHOW,NESTERR    ERROR LAST TIME IN?                          
         BE    *+10                                                             
         MVC   NESTLAST,NESTSHOW   SAVE LAST GOOD STATUS                        
         MVI   NESTSHOW,NESTNO                                                  
         CLI   FVILEN,0            DEFAULT IS TO NOT SHOW NESTED                
         BE    VSHW04                                                           
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
*                                                                               
         EX    RF,*+8                                                           
         BE    VSHW04                                                           
         CLC   FVIFLD(0),FL@NO                                                  
*                                                                               
         EX    RF,*+8                                                           
         BE    VSHW04                                                           
         CLC   FVIFLD(0),FU@NO                                                  
*                                                                               
         EX    RF,*+8                                                           
         BE    VSHW02                                                           
         CLC   FVIFLD(0),FL@YES                                                 
*                                                                               
         EX    RF,*+8                                                           
         BE    VSHW02                                                           
         CLC   FVIFLD(0),FU@YES                                                 
*                                                                               
         MVI   NESTSHOW,NESTERR                                                 
         B     EXITNV                                                           
*                                                                               
VSHW02   CLI   CSACT,A#DIS         CAN ONLY EXPAND INCLUDES FOR DISPLAY         
         BE    *+12                                                             
         MVI   NESTSHOW,NESTERR    SET ERROR                                    
         B     EXITNV                                                           
*                                                                               
         MVI   NESTSHOW,NESTYES    SET TO EXPAND INCLUDES                       
*                                                                               
VSHW04   CLC   NESTSHOW,NESTLAST                                                
         BE    *+12                                                             
         NI    LSLTIND1,FF-(LSLTIBLD) REBUILD LIST                              
         OI    SCRFLAG2,LSSCIRED   REDISPLAY IF STATUS CHANGED                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* NTRSES OBJECT                                                       *         
* -------------                                                       *         
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
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(XITOUT)                              
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER (OUT)                      *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SREC,R#SCRIPT                                                    
         BNE   *+8                                                              
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER (BACK)                     *         
***********************************************************************         
         SPACE 1                                                                
XITOUT   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   NSACT,A#LST                                                      
         BNE   EXITOK                                                           
         NI    SNINDS1,FF-SNIUSECR                                              
         B     EXITOK                                                           
         SPACE 2                                                                
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
THIS     USING CTJREC,R2                                                        
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
         EJECT                                                                  
*                                                                               
LISTABL  DS    0A                                                               
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
*                                                                               
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
*        DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LSCRFRST),AL1(0,0,1),AL4(FSCR1)                              
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LTSARDIR),AL1(0,0,1),AL4(TSARDIR1)                           
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(LUPDDIR),AL1(0,0,1),AL4(UPDDIR1)                             
         DC    AL1(LNEWITEM),AL1(0,0,1),AL4(NEWITEM1)                           
         DC    AL1(LSCRLAST),AL1(0,0,1),AL4(SCRLAST1)                           
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'CTJKEY),THIS.CTJREC                                      
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5         EQUATED I/O AREA TO USE                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH UNHAPPY                            
         B     NLST04                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     MVC   IOKEY(L'CTJKEY),THIS.CTJREC                                      
*                                                                               
NLST02   XR    RF,RF               INCREMENT NAME ID                            
         ICM   RF,1,IOKEY+((CTJKID+L'CTJKID-1)-CTJREC)                          
         LA    RF,1(RF)                                                         
         STCM  RF,1,IOKEY+((CTJKID+L'CTJKID-1)-CTJREC)                          
         MVI   IOKEY+(CTJKSUB-CTJREC),0                                         
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5         EQUATED I/O AREA TO USE                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH UNHAPPY                            
*                                                                               
NLST04   L     RF,SVPARMS5         CONTROL FILE RECORDS ARE RETURNED IN         
         SRL   RF,4-2              THE I/O AREA                                 
         L     RF,AIO1-L'AIO1(RF)                                               
*                                                                               
         CLC   0(CTJKID-CTJREC,RF),THIS.CTJREC                                  
         BNE   EXITL                                                            
         CLI   CTJKSUB-CTJREC(RF),0                                             
         BE    NLST06                                                           
         MVC   IOKEY(L'CTJKEY),0(RF)  PUT CURRENT KEY INTO IOKEY                
         B     NLST02                                                           
*                                                                               
NLST06   MVC   THIS.CTJREC(L'CTJKEY),0(RF)   WE WANT THIS RECORD                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT2,(LSSNOSEQ+LSSADD+LSSIUPD)                                
         OI    LSSTAT1,(LSSTSAR+LSSBALL)                                        
         MVI   LSSUBLEN,SBLEN                                                   
         XC    CCOUNT,CCOUNT       RESET COUNT OF ITEMS IN LIST                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR SCREEN 1                                                  *         
***********************************************************************         
         SPACE 1                                                                
FSCR1    OC    LSSCIND1,SCRFLAG1   KEEP CURRENT SCREEN                          
         OC    LSSCIND2,SCRFLAG2   REDISPLAY REQUEST                            
         LH    R0,LSLINPAG                                                      
         LH    R1,LS1STLIN         FIRST LIST LINE                              
         A     R1,ATWA                                                          
         USING FHD,R1                                                           
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    R1,0(RE,R1)         GO PAST SUB-ACTION FIELD                     
*                                                                               
FSCR102  NI    FHAT,FF-(FHATLC)    SET ALL UPPER-CASE                           
         OI    FHOI,FHOITR                                                      
         AH    R1,LSLINLEN                                                      
         BCT   R0,FSCR102                                                       
*                                                                               
         L     R2,AIOREC                                                        
         USING CTLREC,R2                                                        
         CLI   CTLKTYP,CTLKTYPQ                                                 
         BNE   EXITOK                                                           
         CLI   CTLKSCR,CTLKXTRQ    SQL BOOKS CAN BE LOWER CASE                  
         BNE   EXITOK                                                           
*                                                                               
         LH    R0,LSLINPAG                                                      
         LH    R1,LS1STLIN         FIRST LIST LINE                              
         A     R1,ATWA                                                          
         USING FHD,R1                                                           
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    R1,0(RE,R1)         GO PAST SUB-ACTION FIELD                     
*                                                                               
FSCR104  OI    FHAT,FHATLC         SET INPUT FIELD AS LOWERCASE                 
         AH    R1,LSLINLEN                                                      
         BCT   R0,FSCR104                                                       
         B     EXITOK                                                           
         DROP  R1,R2                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST PAGE 1                                          *         
***********************************************************************         
         SPACE 1                                                                
FLST1    MVC   IOKEY,THIS.CTJREC                                                
         XC    BOOKPARM,BOOKPARM   CLEAR PARAMETER LIST                         
         LA    R1,BOOKPARM                                                      
*                                                                               
         L     RF,AIO5             A(IO AREA)                                   
         MVC   0(L'CTJKEY,RF),GSRECKEY   SET INITIAL KEY                        
         ST    RF,0(R1)            SET A(IO AREA)                               
         CLI   NESTSHOW,NESTYES    RESOLVE NESTED BOOKS?                        
         BE    *+8                 YES                                          
         MVI   0(R1),X'FF'                                                      
*                                                                               
         LA    RE,BOWORK1          80 CHARACTER CARD AREA                       
         ST    RE,4(R1)                                                         
*                                                                               
         MVC   8(4,R1),VDMGR       A(DATA MANAGER)                              
*                                                                               
         L     RF,BCAUTL                                                        
         MVC   12(1,R1),TNUM-UTLD(RF)  SET TERMINAL NUMBER                      
         B     NLST1                                                            
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST PAGE 1                                                   *         
***********************************************************************         
         SPACE 1                                                                
NLST1    MVC   BOWORK1,BCSPACES                                                 
         GOTOX AGETBOOK,BOOKPARM                                                
         TM    8(R1),X'80'         END OF BOOK                                  
         BO    EXITL                                                            
         CLI   8(R1),0             OK?                                          
         BE    NLST102             NO                                           
         TM    8(R1),X'02'         DELETED - IGNORE IT                          
         BO    NLST102                                                          
         MVC   FVMSGNO,=AL2(FVFIOER)                                            
         B     EXITL                                                            
*                                                                               
NLST102  L     RF,AIO5                                                          
         MVC   THIS.CTJKEY,0(RF)   SAVE THE KEY                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM DIRECTORY 1                                        *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR1 B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,SVPARMS4         A(TSAR BUFFER)                               
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET NEW RECORD LENGTH                        
*                                                                               
         LH    RF,CCOUNT           CURRENT LINE INDEX                           
         LA    RF,1(RF)                                                         
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM                                                     
*                                                                               
         MVC   TLLNUM,BOWORK1+72   LINE NUMBER                                  
         MVC   TLLINE,BOWORK1      ACTUAL DATA LINE                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 CLI   GSFRP.FRPTYPE,FRPTUPDT      UPDATING RECORD?                     
         BE    UPF02                                                            
         CLI   BCPFKEY,PFK04               COMPILING RECORD                     
         BE    *+12                                                             
         CLI   BCPFKEY,PFK16               COMPILING RECORD                     
         BNE   EXITOK                                                           
*                                                                               
         CLC   GSRECKEY(2),=AL1(CTLKTYPQ,CTLKSCRQ)                              
         BNE   EXITOK                      ONLY COMPILE SCRIPTS                 
*                                                                               
UPF02    GOTOX ADELEL,BOPARM,('CTINDELQ',AIOREC),0                              
*                                                                               
         XC    SCOUNT,SCOUNT      RESET INDEX COUNT                             
         LA    RF,BOELEM          SET UP BLANK INDEX ELEMENT                    
         USING CTINDD,RF                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   CTINDEL,CTINDELQ                                                 
         MVI   CTINDLEN,254       HARD CODED LENGTH - UGG                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 CLI   GSFRP.FRPTYPE,FRPTUPDT      UPDATING RECORD?                     
         BE    UPL02                                                            
         CLI   BCPFKEY,PFK04               COMPILING RECORD                     
         BE    *+12                                                             
         CLI   BCPFKEY,PFK16               COMPILING RECORD                     
         BNE   EXITOK                                                           
*                                                                               
         CLC   GSRECKEY(2),=AL1(CTLKTYPQ,CTLKSCRQ)                              
         BNE   EXITOK                      ONLY COMPILE SCRIPTS                 
*                                                                               
UPL02    GOTO1 AADDEL,BOPARM,AIOREC                                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP A NEW LINE IN THE LIST 1                                     *         
***********************************************************************         
         SPACE 1                                                                
NEWITEM1 L     R3,SVPARMS3         A(TSAR BUFFER)                               
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET NEW RECORD LENGTH                        
*                                                                               
         LH    RF,CCOUNT           CURRENT LINE INDEX                           
         LA    RF,1(RF)                                                         
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        SET LINE INDEX NUMBER                        
*                                                                               
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  TLLNUM,BODUB1       SET LINE NUMBER IN TSAR RECORD               
         MVC   TLLINE+72(L'TLLNUM),TLLNUM                                       
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* UPDATE DIRECTORY FROM TSAR RECORD 1                                 *         
* P3 = A (DIRECTORY RECORD)                                           *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING CTINDD,BOELEM                                                    
UPDDIR1  CLI   GSFRP.FRPTYPE,FRPTUPDT      UPDATING RECORD?                     
         BE    UPDR02                                                           
         CLI   BCPFKEY,PFK04               COMPILING RECORD                     
         BE    *+12                                                             
         CLI   BCPFKEY,PFK16               COMPILING RECORD                     
         BNE   EXITOK                                                           
*                                                                               
         CLC   GSRECKEY(2),=AL1(CTLKTYPQ,CTLKSCRQ)                              
         BNE   EXITOK                      ONLY COMPILE SCRIPTS                 
*                                                                               
UPDR02   LH    RF,LSLST#X                                                       
         SH    RF,LSLST#1                                                       
         CH    RF,=H'250'          MAX OF 250 LINES IN A BOOK                   
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(CE#RECTB)                                           
         B     EXITL                                                            
*                                                                               
         LH    R3,SCOUNT                                                        
         LA    R3,1(R3)                                                         
         STH   R3,SCOUNT           INCREMENT CURRENT COUNT                      
*                                                                               
         STC   R3,CTINDHI          SET CURRENT HIGHEST USED                     
         LA    R1,CTINDEX-1(R3)    INDEX INTO THE INDEX                         
         STC   R3,0(R1)            STORE CURRENT SEQUENCE NUMBER                
*                                                                               
         USING CTJREC,IOKEY                                                     
         L     R1,AIOREC                                                        
         MVC   CTJKEY,0(R1)                                                     
         STC   R3,CTJKSUB          SET SUB RECORD NUMBER                        
*                                                                               
         L     R1,=AL4(XORDD+XOLOCK+XOCONFIL+XIO1)                              
         GOTOX ('XIO',AGROUTS)                                                  
*                                                                               
         L     R1,AIOREC                                                        
         L     R2,AIO1             BUILD EMPTY RECORD IN IO1                    
S        USING CTJKEY,R2                                                        
         MVC   S.CTJKEY,0(R1)      COPY ORIGINAL KEY                            
         STC   R3,S.CTJKSUB        SET CURRENT SUB-NUMBER                       
         XC    S.CTJSTAT,S.CTJSTAT RESET STATUS AREA                            
         LA    R1,CTJDATA-CTJKEY+1                                              
         STCM  R1,3,S.CTJLEN       SET LENGTH OF AN EMPTY RECORD                
         XC    S.CTJDATA(100),S.CTJDATA                                         
*                                                                               
         L     R3,SVPARMS4         A(CURRENT TSAR RECORD)                       
         USING TLSTD,R3                                                         
         CLC   =CL10'++INCLUDE ',TLLINE                                         
         BE    UPDD02              INCLUDING ANOTHER BOOK                       
*                                                                               
         LA    R1,LENLINE                                                       
         LA    RF,TLLINE+LENLINE-1                                              
         CLI   0(RF),C' '          LOOK FOR LAST BLANK ON LINE                  
         BH    *+14                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         LA    R1,1                                                             
*                                                                               
         USING CTDATD,S.CTJDATA                                                 
         MVI   CTDATEL,CTDATELQ    BUILD A DATA ELEMENT                         
         LA    RF,CTDATA-CTDATEL(R1)                                            
         STC   RF,CTDATLEN         SET NEW LENGTH                               
         XR    RE,RE                                                            
         ICM   RE,3,S.CTJLEN       GET LENGTH OF AN EMPTY RECORD                
         LA    RE,0(RF,RE)                                                      
         STCM  RE,3,S.CTJLEN       SET NEW RECORD LENGTH                        
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   CTDATA(0),TLLINE                                                 
         B     UPDD06                                                           
*                                                                               
         USING CTINCD,S.CTJDATA                                                 
INC      USING CTJKEY,CTINCLUD                                                  
UPDD02   MVI   CTINCEL,CTINCELQ                                                 
         LA    RF,CTINCLUD+L'CTINCLUD-CTINCD                                    
         STC   RF,CTINCLEN                                                      
         XR    RE,RE                                                            
         ICM   RE,3,S.CTJLEN       GET LENGTH OF AN EMPTY RECORD                
         LA    RE,0(RF,RE)                                                      
         STCM  RE,3,S.CTJLEN       SET NEW RECORD LENGTH                        
*                                                                               
         XC    CTINCLUD,CTINCLUD                                                
         LA    RF,RTYVTAB                                                       
         USING RTYVTABD,RF                                                      
*                                                                               
UPDD04   CLI   RTYVTYPE,EOT        WHAT TYPE IS THIS?                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   RTYVTYPE,TLLINE+10  X,XXXXXX                                     
         BE    *+12                                                             
         LA    RF,RTYVLQ(RF)                                                    
         B     UPDD04                                                           
*                                                                               
         MVC   INC.CTJKEY(L'RTYVCODE),RTYVCODE                                  
         MVC   INC.CTJKID,TLLINE+12                                             
         B     UPDD06                                                           
*                                                                               
UPDD06   L     R1,=AL4(XOWRITE+XOCONFIL+XIO1)                                   
         TM    IOERR,IOERNF        RECORD EXISTS?                               
         BZ    *+8                 NO - NEED TO ADD IT THEN                     
         L     R1,=AL4(XOADD+XOCONFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    EXITOK                                                           
         DC    H'0'                WHAT'S UP DOC?                               
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST FOR SCREEN PAGE 1 (SORTS & REDISPLAYS TSAR RECORDS)            *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST1 TM    DOFLAG,DOFROM+DOTO                                               
         BO    SLAST02             FROM & TO ACTIONS BOTH SET                   
         BNZ   SLAST04             EITHER FROM OR TO ACTION SET                 
         TM    DOFLAG,DODEL+DOREP+DOINS                                         
         BZ    SLAST06             NO ACTIONS SET                               
*                                                                               
SLAST02  BAS   RE,SORT             ACTIONS NEED RESOLVING                       
         BAS   RE,REDRAW                                                        
         OI    GCINDS2,GCIANYCH    SET CHANGES THIS TIME                        
         B     SLAST06                                                          
*                                                                               
SLAST04  BAS   RE,REACT            ACTIONS NEED REDRAWING                       
*                                                                               
         CLC   DOFRCNT,=H'1'       TEST DUPLICATE FROM FIELDS                   
         BNH   *+20                NO - GOOD                                    
         MVC   FVADDR,AFRSTFR                                                   
         MVC   FVMSGNO,=AL2(CE#CMDCF)                                           
         B     EXITL                                                            
*                                                                               
         CLC   DOTOCNT,=H'1'       TEST DUPLICATE TO FIELDS                     
         BNH   SLAST06             NO - GOOD                                    
         MVC   FVADDR,AFRSTTO                                                   
         MVC   FVMSGNO,=AL2(CE#CMDCF)                                           
         B     EXITL                                                            
*                                                                               
SLAST06  CLC   SEQNUM,SEQLAST      SEQUENCE NUMBER CHANGED?                     
         BE    SLAST08             NO                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,SEQNUM         REQUESTED SEQUENCE NUMBER                    
         BNP   *+6                                                              
         BCTR  RF,0                ZERO BASE IT                                 
*                                                                               
         AH    RF,LSLST#1          FIRST NUMBER IN LIST                         
         CH    RF,LSLST#X          STILL IN LIST?                               
         BNH   *+8                                                              
         LH    RF,LSLST#X                                                       
         STH   RF,LSPAG#1          FIRST IN PAGE NUMBER                         
*                                                                               
         L     R3,ASEQFLD          EDIT OUT CORRECT NUMBER                      
         USING FHD,R3                                                           
         OI    FHOI,FHOITR                                                      
         CURED (RF),(3,FHDA),0,DMCB=BOPARM,ALIGN=LEFT                           
         DROP  R3                                                               
*                                                                               
         MVC   SEQLAST,SEQNUM                                                   
         BAS   RE,REDRAW           REDISPLAY A SCREEN FULL OF DATA              
*                                                                               
SLAST08  CLI   BCPFKEY,PFK04       'COMPILE KEY'?                               
         BE    SLAST10                                                          
         CLI   BCPFKEY,PFK16       'COMPILE KEY'?                               
         BE    SLAST10                                                          
*                                                                               
         OC    AINSFLD,AINSFLD      WANT TO BUNG CURSOR ON INSERT LINE?         
         BZ    EXITOK               NO                                          
         MVC   BOCURSOR,AINSFLD     DO IT BEAVIS...                             
         B     EXITOK                                                           
*                                                                               
SLAST10  BAS   RE,STUMBLE           TRY TO COMPILE THE DAMN THING               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SUB ACTION OBJECT                                                   *         
* -----------------                                                   *         
* P3 = A(SUB-ACTION FIELD)                                            *         
***********************************************************************         
         SPACE 1                                                                
SUB      OC    GSSMPAGE,GSSMPAGE   VALIDATION ONLY FOR MAINT LIST               
         BZ    EXITH                                                            
*                                                                               
         TM    LSSCIND2,LSSCIDIS   IGNORE IT IF WE ARE DISPLAYING               
         BO    EXITOK                                                           
*                                                                               
         LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,SUBTABL                                                       
         B     ITER                                                             
*                                                                               
SUBTABL  DC    AL1(SAVAL),AL1(0,0,0),AL4(SUBVAL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUB ACTION FOR LIST                                        *         
***********************************************************************         
         SPACE 1                                                                
SNGL     USING SUBACTSD,LSNTRY                                                  
MLTI     USING SUBACTSD,LMNTRY                                                  
SUBVAL   XC    LSNTRY,LSNTRY       CLEAR CURRENT SUB-ACTION ELEMENT             
         OI    LSSCIND1,LSSCIINP   KEEP CURRENT SCREEN                          
         L     R2,SVPARMS3         R2=A(SUB-ACTION FIELD)                       
         USING FHD,R2                                                           
         L     R4,ATLST            R4=A(TSAR RECORD)                            
         USING TLSTD,R4                                                         
*                                                                               
         OC    TLUSTAT,TLUSTAT     ANY ACTIONS FOR THIS RECORD                  
         BZ    SVAL04              NO                                           
         TM    FHII,FHIITH         FIELD INPUT BY USER?                         
         BO    SVAL04              YES - OVERRIDE CURRENT SETTING               
*                                                                               
         LA    RF,TLUACTS          ACTION NAME DISPLAY TABLE                    
         USING TLUACTSD,RF                                                      
SVAL02   CLI   0(RF),EOT           UNKNOWN FLAG IN TSAR RECORD                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BOBYTE1,TLUSTAT     COPY STATUS FLAG                             
         NC    BOBYTE1,TLUFLAG     IS THIS ACTION REQUEST SET?                  
         BNZ   *+12                YES                                          
         LA    RF,TLULQ(RF)                                                     
         B     SVAL02              NEXT KNOWN ACTION                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,TLUNAME                                                     
         A     RE,AOVERWRK         RE=A(NAME OF ACTION)                         
         MVC   FHDA(SBLEN),0(RE)   MOVE OUT NAME INTO FIELD                     
         OI    FHOI,FHOITR         TRANSMIT IT                                  
         OI    FHII,FHIIVA         SET FIELD VALID                              
         DROP  RF                                                               
*                                                                               
SVAL04   TM    FHII,FHIIVA         FIELD VALIDATED?                             
         BO    *+8                 NO                                           
         OI    LSSCIND1,LSSCIINP   YES - KEEP CURRENT SCREEN                    
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         CLI   FVILEN,0            SUB-ACTION ENTERED?                          
         BE    SVAL18              NO                                           
         CLI   FVIFLD,C'*'         IGNORE THIS FIELD?                           
         BE    SVAL18              YES                                          
         CLI   CSACT,A#CHA                                                      
         BNE   EXITNV              INPUT INVALID UNLESS ACTION CHANGE           
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         BZ    SVAL12              ONLY 1 CHARACTER INPUT                       
         LA    RF,FVIFLD(RE)       TEST SUFFIX CHARACTER                        
*                                                                               
         CLI   0(RF),C'0'          NUMERICAL ENDING?                            
         BL    SVAL12              NO                                           
         CLI   0(RF),C'9'                                                       
         BH    SVAL12              NO                                           
*                                                                               
         XR    R1,R1               R1 HOLDS LENGTH                              
SVAL06   CLI   0(RF),C'0'          STILL NUMERIC?                               
         BL    SVAL08              NO                                           
         CLI   0(RF),C'9'                                                       
         BH    SVAL08              NO                                           
         LA    R1,1(R1)                                                         
         BCTR  RF,0                                                             
         BCT   RE,SVAL06                                                        
*                                                                               
SVAL08   BCTR  R1,0                                                             
         EX    R1,SVALPAK          OBTAIN PACKED NUMBER                         
         CVB   R0,GCDUB1                                                        
         EX    R1,SVALMVE          CLEAR NUMBER FROM INPUT FIELD                
         B     SVAL10                                                           
*                                                                               
SVALPAK  PACK  GCDUB1,1(0,RF)      PACK NUMBER INTO GCDUB1                      
SVALMVE  MVC   1(0,RF),BCSPACES    CLEAR NUMERIC PORTION OF FIELD               
*                                                                               
SVAL10   STH   R0,LMCOUNT          SAVE MULTILINE ACTION REPEAT NUMBER          
         XR    R0,R0                                                            
         IC    R0,FVILEN           REVALIDATE FVIFLD                            
         GOTOX ('FLDVAL',AGROUTS),0                                             
*                                                                               
SVAL12   LA    R3,SUBACTS          TRY TO MATCH SUB-ACTION                      
         USING SUBACTSD,R3                                                      
         XR    R1,R1                                                            
         IC    R1,FVXLEN           LENGTH OF INPUT                              
*                                                                               
SVAL14   CLC   SUBUPR,=AL2(EOT)    REACHED END OF TABLE?                        
         BE    SVALL               YES - INVALID SUB-ACTION                     
         XR    RF,RF                                                            
         ICM   RF,3,SUBUPR         TRY TO MATCH UPPERCASE NAME                  
         A     RF,AOVERWRK                                                      
         EX    R1,SUBMCH                                                        
         BE    SVAL16                                                           
         XR    RF,RF                                                            
         ICM   RF,3,SUBLWR         TRY TO MATCH LOWERCASE NAME                  
         A     RF,AOVERWRK                                                      
         EX    R1,SUBMCH                                                        
         BE    SVAL16                                                           
         LA    R3,SUBACTLQ(R3)                                                  
         B     SVAL14                                                           
*                                                                               
SUBMCH   CLC   FVIFLD(0),0(RF)                                                  
*                                                                               
SVAL16   MVC   LSNTRY,0(R3)        SAVE THIS SINGLE ENTRY                       
         OI    FHII,FHIIVA         SET FIELD VALID                              
*                                                                               
         ICM   RF,15,SNGL.SUBRTN   PROCESSING ROUTINE                           
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  R3                                                               
*                                                                               
SVAL18   OC    TLUSTAT,TLUSTAT     ACTION FLAG ON THIS FIELD BEFORE?            
         BZ    EXITOK              NO - SAFE TO IGNORE IT                       
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    R1,FHD(RE)          R1=A(FIRST DATA FIELD)                       
FLD      USING FHD,R1                                                           
*                                                                               
         LA    RF,FHD              START OF THIS LINE                           
         AH    RF,LSLINLEN         LENGTH OF A LINE                             
         BCTR  RF,0                RF=A(END OF THIS LINE-1)                     
*                                                                               
SVAL20   IC    RE,FLD.FHLN         SET INDEX IN RE                              
         OI    FLD.FHOI,FHOITR     TRANSMIT FIELD                               
         NI    FLD.FHAT,FF-FHATHI  TURN OFF HIGHLIGHT                           
         BXLE  R1,RE,SVAL20        REPEAT FOR ALL FIELDS ON LINE                
         DROP  FLD                                                              
*                                                                               
         TM    TLUSTAT,TLUSCPY+TLUSMVE   'FROM' FLAG ON THIS ONE                
         BZ    SVAL22                    NO                                     
         LH    RF,DOFRCNT                DECREMENT 'FROM' COUNT                 
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOFROM          TURN OFF FLAG IF NO 'FROM'             
         STH   RF,DOFRCNT                SAVE NEW 'FROM' COUNT                  
*                                                                               
SVAL22   TM    TLUSTAT,TLUSBEF+TLUSAFT   'TO' FLAG ON THIS ONE                  
         BZ    SVAL24                    NO                                     
         LH    RF,DOTOCNT                DECREMENT 'TO' COUNT                   
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOTO            TURN OFF FLAG IF NO 'TO'               
         STH   RF,DOTOCNT                SAVE NEW 'TO' COUNT                    
*                                                                               
SVAL24   TM    TLUSTAT,TLUSDEL           'DELETE' FLAG ON THIS ONE              
         BZ    SVAL26                    NO                                     
         LH    RF,DODLCNT                DECREMENT 'DELETE' COUNT               
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DODEL           TURN OFF FLAG IF NO 'DELETE'           
         STH   RF,DODLCNT                                                       
*                                                                               
SVAL26   TM    TLUSTAT,TLUSREP     'REPLICATE' FLAG ON THIS ONE                 
         BZ    SVAL28              NO                                           
         LH    RF,DORPCNT          DECREMENT 'REPLICATE' COUNT                  
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOREP                                                  
         STH   RF,DORPCNT                                                       
*                                                                               
SVAL28   TM    TLUSTAT,TLUSINS     'INSERT' FLAG ON THIS ONE                    
         BZ    SVAL30              NO                                           
         LH    RF,DOINCNT          DECREMENT 'INSERT' COUNT                     
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOINS                                                  
         STH   RF,DORPCNT                                                       
*                                                                               
SVAL30   XC    TLUSTAT,TLUSTAT     RESET ACTION FLAG ON THIS ONE                
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
         OI    FHII,FHIIVA                                                      
         B     EXITOK                                                           
*                                                                               
SVALL    B     EXITL                                                            
         DROP  R2,R4                                                            
*                                                                               
SUBACTS  DS    0H              *** TABLE OF VALID SUB-ACTIONS ***               
         DC    AL2(FU@COPY-OVERWRKD,FL@COPY-OVERWRKD),AL4(SUB_CPY)              
         DC    AL2(FU@MOVE-OVERWRKD,FL@MOVE-OVERWRKD),AL4(SUB_MVE)              
         DC    AL2(FU@DEL-OVERWRKD,FL@DEL-OVERWRKD),AL4(SUB_DEL)                
         DC    AL2(FU@BFR-OVERWRKD,FL@BFR-OVERWRKD),AL4(SUB_BFR)                
         DC    AL2(FU@AFTER-OVERWRKD,FL@AFTER-OVERWRKD),AL4(SUB_AFT)            
         DC    AL2(FU@REPL-OVERWRKD,FL@REPL-OVERWRKD),AL4(SUB_REP)              
         DC    AL2(FU@INSRT-OVERWRKD,FL@INSRT-OVERWRKD),AL4(SUB_INS)            
         DC    AL2(EOT)                                                         
*                                                                               
SUBACTSD DSECT                                                                  
SUBUPR   DS    AL2                 DISPLACEMENT TO UPPERCASE NAME               
SUBLWR   DS    AL2                 DISPLACEMENT TO LOWER CASE NAME              
SUBRTN   DS    AL4                 DISPLACEMENT TO VALIDATION ROUTINE           
SUBACTLQ EQU   *-SUBACTSD                                                       
*                                                                               
FIL20    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DELETE A LINE FROM THE LIST                              *         
***********************************************************************         
         SPACE 1                                                                
SUB_DEL  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSDEL     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSDEL     SET TO DELETE THIS LINE                      
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SDEL02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SDEL02   LH    RF,DODLCNT          INCREMENT COUNT OF DELETE FIELDS             
         LA    RF,1(RF)                                                         
         STH   RF,DODLCNT                                                       
         OI    DOFLAG,DODEL        SET A DELETE ACTION REQUIRED                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO REPLICATE A LINE IN THE LIST                             *         
***********************************************************************         
         SPACE 1                                                                
SUB_REP  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSREP     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSREP     SET TO REPLICATE THIS LINE                   
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SREP02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SREP02   LH    RF,DORPCNT          INCREMENT COUNT OF REPLICATE FIELDS          
         LA    RF,1(RF)                                                         
         STH   RF,DORPCNT                                                       
         OI    DOFLAG,DOREP        SET A REPLICATE ACTION REQUIRED              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO INSERT A LINE IN THE LIST                                *         
***********************************************************************         
         SPACE 1                                                                
SUB_INS  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSINS     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSINS     SET TO REPLICATE THIS LINE                   
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SINS02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SINS02   LH    RF,DOINCNT          INCREMENT COUNT OF REPLICATE FIELDS          
         LA    RF,1(RF)                                                         
         STH   RF,DOINCNT                                                       
         OI    DOFLAG,DOINS        SET A INSERT ACTION REQUIRED                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO COPY A LINE IN A LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
SUB_CPY  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSCPY     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSCPY     SET TO COPY THIS FIELD                       
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SCPY02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SCPY02   LH    RF,DOFRCNT          INCREMENT COUNT OF 'FROM' FIELDS             
         LA    RF,1(RF)                                                         
         STH   RF,DOFRCNT                                                       
         OI    DOFLAG,DOFROM       SET A 'FROM' ACTION REQUIRED                 
*                                                                               
         CLC   DOFRCNT,=H'1'       TEST ANOTHER FROM FIELD SET UP               
         BNH   EXITOK              NO - GOOD                                    
         MVC   FVMSGNO,=AL2(CE#CMDCF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO MOVE A LINE IN A LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
SUB_MVE  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSMVE     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSMVE                                                  
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SMVE02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SMVE02   LH    RF,DOFRCNT          INCREMENT COUNT OF 'FROM' FIELDS             
         LA    RF,1(RF)                                                         
         STH   RF,DOFRCNT                                                       
         OI    DOFLAG,DOFROM                                                    
         CLC   DOFRCNT,=H'1'       TEST ANOTHER FROM FIELD SET UP               
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(CE#CMDCF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO COPY BEFORE THIS LINE IN A LIST                          *         
***********************************************************************         
         SPACE 1                                                                
SUB_BFR  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSBEF     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSBEF                                                  
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SBEF02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SBEF02   LH    RF,DOTOCNT          INCREMENT COUNT OF 'TO' FIELDS               
         LA    RF,1(RF)                                                         
         STH   RF,DOTOCNT                                                       
         OI    DOFLAG,DOTO                                                      
         CLC   DOTOCNT,=H'1'       TEST ANOTHER COPY TO FIELD SET UP            
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(CE#CMDCF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO COPY AFTER THIS LINE IN A LIST                           *         
***********************************************************************         
         SPACE 1                                                                
SUB_AFT  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSAFT     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSAFT                                                  
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SAFT02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SAFT02   LH    RF,DOTOCNT          INCREMENT COUNT OF 'TO' FIELDS               
         LA    RF,1(RF)                                                         
         STH   RF,DOTOCNT                                                       
         OI    DOFLAG,DOTO                                                      
         CLC   DOTOCNT,=H'1'       TEST ANOTHER COPY TO FIELD SET UP            
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(CE#CMDCF)                                           
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* REPORT OBJECT                                                       *         
* -------------                                                       *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
REP      LM    R0,R3,SVPARMS                                                    
         LA    RF,REPTABL                                                       
         B     ITER                                                             
*                                                                               
REPTABL  DC    AL1(RPFIRST),AL1(0,0,0),AL4(REPFRST)                             
         DC    AL1(RPQINIT),AL1(0,0,0),AL4(PQINT)                               
         DC    AL1(RDO),AL1(0,0,0),AL4(REPDO)                                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE PRINT QUEUE FOR SCRIPT COMPILATION                       *         
***********************************************************************         
         SPACE 1                                                                
PQINT    L     R5,AREP                                                          
         USING REPD,R5                                                          
         CLI   CSACT,A#REPT        REPORT SETS IT'S OWN STUFF                   
         BE    EXITOK                                                           
         MVI   INWIDTH,REPWIDEQ    SET PRINTING ON A WIDE LINE                  
         MVC   REPSUBID,=C'SCP'    SET A DEFAULT SUB-ID                         
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR REPORT OBJECT                                        *         
***********************************************************************         
         SPACE 1                                                                
REPFRST  L     R5,AREP                                                          
         USING REPD,R5                                                          
         MVC   INSYSID,=CL2'CT'                                                 
         MVC   INPRGID,=CL2'NF'                                                 
         MVC   INJCLID,=CL2'BO'                                                 
         MVI   INPRTY1,C' '                                                     
         MVI   INPRTY2,C' '                                                     
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* DO REPORT FOR BOOK RECORDS                                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
REPDO    MVC   IOKEY,GSRECKEY      INITIAL KEY BUILT HERE                       
         L     R5,AREP                                                          
         USING REPD,R5                                                          
K        USING CTJREC,IOKEY                                                     
         MVC   REPCMP,K.CTJKID                                                  
         LA    R0,L'CTJKID                                                      
         LA    RF,K.CTJKID+L'CTJKID-1                                           
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         SH    R0,=H'1'                                                         
         BNM   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         STC   R0,REPLEN                                                        
         B     PRNT04                                                           
*                                                                               
PRNT02   XR    RF,RF                                                            
         IC    RF,K.CTJKID+L'CTJKID-1                                           
         LA    RF,1(RF)                                                         
         STC   RF,K.CTJKID+L'CTJKID-1                                           
         MVI   K.CTJKSUB,0          REQUEST MASTER RECORD                       
*                                                                               
PRNT04   ICM   R1,15,=AL4(XIO1+XOHIGH+XOCONFIL)                                 
         GOTOX ('XIO',AGROUTS)     NEXT FILE RECORD                             
*                                                                               
         L     R2,AIO1                                                          
         USING CTJREC,R2                                                        
         CLC   IOKEYSAV(2),CTJKEY  SAME BOOK TYPE?                              
         BNE   PRNT12              NO - DONE THEN                               
*                                                                               
         XR    RF,RF                                                            
         IC    RF,REPLEN           COMPARE FOR INPUT LENGTH                     
         EX    RF,*+8                                                           
         BNE   PRNT12              END OF COMPARISON                            
         CLC   REPCMP(0),CTJKID                                                 
*                                                                               
         MVC   K.CTJKEY,CTJKEY     SAVE CURRENT KEY                             
         TM    IOERR,IOEDEL        DELETED BOOK?                                
         BO    PRNT02              YES                                          
         CLI   CTJKSUB,0           MASTER RECORD?                               
         BNE   PRNT02              NO                                           
*                                                                               
         MVC   REPP1(L'FL@TITLE),FL@TITLE                                       
         MVC   REPP1+15(L'CTJKID),CTJKID                                        
         MVI   REPPRNSA,1                                                       
         OI    REPPRNTI,REPPSPAC   SET 1 SPACE LINE AFTER PRINTING              
         GOTOX VREPORT,REPD                                                     
*                                                                               
         USING CTDSCD,BOELEM                                                    
         GOTOX AGETEL,BOPARM,('CTDSCELQ',CTJKEY),0                              
         BNE   PRNT06              NO DESCRIPTION                               
         MVC   REPP1(L'FL@DESC),FL@DESC                                         
         XR    RF,RF                                                            
         IC    RF,CTDSCLEN                                                      
         SH    RF,=H'3'            CODEºLENGTHºMVC                              
         EX    RF,*+4                                                           
         MVC   REPP1+16(0),CTDSC   MOVE IN DESCRIPTION                          
*                                                                               
         MVI   REPPRNSA,1                                                       
         OI    REPPRNTI,REPPSPAC   SET 1 SPACE LINE AFTER PRINTING              
         GOTOX VREPORT,REPD                                                     
*                                                                               
PRNT06   L     R4,AIO2                                                          
         MVC   0(L'CTJKEY,R4),CTJKEY KEY FOR 1ST TIME IN                        
         XC    BOPARM(24),BOPARM   CLEAR MY PARM LIST                           
*                                                                               
         L     RF,BCAUTL           A(UTL)                                       
         MVC   BOPARM+12(1),TNUM-UTLD(R1) TERMINAL # REQUIRED FOR UK            
*                                                                               
         ST    R4,BOPARM           A(SAVE AREA FOR GETBOOK)                     
         MVI   BOPARM,0            EXPAND ++INCLUDES                            
         LA    R1,BOPARM                                                        
         LA    RF,BOWORK1          A(CARD AREA)                                 
         ST    RF,4(R1)                                                         
         MVI   4(R1),0             START SEQ # IS 0                             
         L     RF,VDMGR            A(DATA MANAGER)                              
         ST    RF,8(R1)                                                         
         XC    BOWORK1,BOWORK1                                                  
*                                                                               
PRNT08   GOTOX AGETBOOK,BOPARM     INDIVIDUAL BOOK PRINT LOOP                   
         LA    R1,BOPARM                                                        
         TM    8(R1),X'80'         END OF BOOK                                  
         BO    PRNT02                                                           
         CLI   8(R1),0             OK                                           
         BE    PRNT10                                                           
         TM    8(R1),X'02'         DELETED - CAN IGNORE IT                      
         BZ    PRNT02                                                           
*                                                                               
PRNT10   MVC   REPP1(80),BOWORK1                                                
         GOTOX VREPORT,REPD                                                     
         XC    BOWORK1,BOWORK1     CLEAR W/S                                    
         B     PRNT08              GET NEXT BOOK                                
*                                                                               
PRNT12   B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* PFKEY OBJECT                                                        *         
* ------------                                                        *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
PFK      LM    R0,R3,SVPARMS                                                    
         LA    RF,PFKTABL                                                       
         B     ITER                                                             
*                                                                               
PFKTABL  DC    AL1(PFREC),AL1(0,0,0),AL4(PPFREC)                                
         DC    AL1(PFACT),AL1(0,0,0),AL4(PPFACT)                                
         DC    AL1(PFUSER),AL1(0,0,0),AL4(PPFUSR)                               
         DC    AL1(PFLST),AL1(0,0,0),AL4(PPFLST)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DETERMINE WHETHER PFKEY REQUIRED BASED ON RECORD & SET DISPLAY      *         
***********************************************************************         
         SPACE 1                                                                
PPFREC   L     RF,SVPARMS3                                                      
         CLI   0(RF),PFK04                                                      
         BNE   PREC02              SUPPRESS RECORD FIELD IF COMPILE             
         OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
*                                                                               
PREC02   CLI   0(RF),PFK16                                                      
         BNE   EXITOK              SUPPRESS RECORD FIELD FOR PFK16              
         OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DETERMINE WHETHER PFKEY REQUIRED BASED ON ACTION & SET DISPLAY      *         
***********************************************************************         
         SPACE 1                                                                
PPFACT   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DETERMINE WHETHER PFKEY REQUIRED BASED ON USER & SET DISPLAY        *         
***********************************************************************         
         SPACE 1                                                                
PPFUSR   L     RF,SVPARMS3                                                      
         CLI   0(RF),PFK04                                                      
         BNE   PUSR02              SET TEXT IF COMPILE PFKEY                    
         MVC   FVIFLD(8),=CL8'Compile'                                          
         B     EXITOK                                                           
*                                                                               
PUSR02   CLI   0(RF),PFK16                                                      
         BNE   EXITOK              SUPPRESS RECORD FIELD FOR PFK16              
         OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DETERMINE WHETHER PFKEY VALID FOR INCLUSION                         *         
***********************************************************************         
         SPACE 1                                                                
PPFLST   L     RF,SVPARMS3                                                      
         USING FRPELD,RF                                                        
         CLI   FRPPFK#,PFK04       COMPILE PFKEY                                
         BE    PPFLST02                                                         
         CLI   FRPPFK#,PFK16       COMPILE PFKEY                                
         BE    PPFLST02                                                         
         CLI   FRPPFK#,PFK05       SCRIPT NAME KEY                              
         BNE   EXITOK                                                           
*                                                                               
PPFLST02 CLC   GSRECKEY(2),=AL1(CTLKTYPQ,CTLKSCRQ)                              
         BNE   EXITL                                                            
         B     EXITOK                      ONLY COMPILE SCRIPTS                 
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* SORT OUT TSAR RECORDS BASED ON FLAGS SET IN THEM                    *         
***********************************************************************         
         SPACE 1                                                                
SORT     NTR1  ,                                                                
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,TWASESNL     SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
*                                                                               
SORT02   LA    R1,TSANXT           DEAL WITH ALL DELETE REQUESTS                
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SORT04              END OF FILE                                  
         CLC   TLKSES,TWASESNL     CHECK NEST LEVEL                             
         BNE   SORT04              DONE ALL FOR THIS LEVEL                      
*                                                                               
         TM    TLUSTAT,TLUSCPY+TLUSMVE                                          
         BZ    SORT02              NOT A COPY OR A MOVE RECORD                  
*                                                                               
         MVI   BOBYTE1,FF          FLAG RECORD TO COPY/MOVE                     
         LA    R0,SVLST            TSAR RECORD SAVE AREA                        
         LA    R1,L'SVLST                                                       
         LA    RE,TLSTD                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE A COPY OF THIS RECORD                   
         B     SORT02              NEXT IN LIST                                 
*                                                                               
SORT04   XC    CCOUNT,CCOUNT       RESET CURRENT LIST COUNT                     
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,TWASESNL     SET NEST LEVEL                               
         LA    R1,TSARDH                                                        
         B     *+8                                                              
*                                                                               
SORT06   LA    R1,TSANXT                                                        
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SORT14              END OF FILE                                  
         CLC   TLKSES,TWASESNL     CHECK NEST LEVEL                             
         BNE   SORT14              DONE ALL FOR THIS LEVEL                      
*                                                                               
         TM    TLUSTAT,TLUSDEL     WANT TO DELETE THIS LINE?                    
         BO    SORT06              YES - DON'T ADD A COPY OF IT THEN            
*                                                                               
         TM    TLUSTAT,TLUSMVE     WANT TO MOVE THIS LINE?                      
         BO    SORT06              YES - DON'T ADD A COPY OF IT THEN            
*                                                                               
         MVI   BOBYTE2,0           RESET ADD INTO LIST FLAG                     
         NI    TLSTAT,FF-TLSKEYC   TURN OFF KEY CHANGED FLAG                    
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
         LH    R0,TLNUM            SAVE CURRENT LIST NUMBER                     
*                                                                               
         CLI   BOBYTE1,FF          CPY/MVE RECORD TO ADD IN?                    
         BNE   SORT08              NO                                           
*                                                                               
         TM    TLUSTAT,TLUSAFT+TLUSBEF                                          
         BZ    SORT08              NO ADD BEFORE OR AFTER THIS RECORD           
*                                                                               
         TM    TLUSTAT,TLUSAFT     ADDING AFTER THIS RECORD?                    
         BZ    *+12                NO                                           
         MVI   BOBYTE2,FF          SET ADD AFTER FLAG                           
         B     SORT08                                                           
*                                                                               
         BAS   RE,ADDIN            ADD RECORD BEFORE THIS RECORD                
*                                                                               
SORT08   TM    TLUSTAT,TLUSREP+TLUSINS REPLICATING OR INSERTING                 
         BZ    SORT10              NO                                           
*                                                                               
         MVC   BOWORK1,TLLINE      SAVE THIS LINE                               
         TM    TLUSTAT,TLUSINS     INSERTING A LINE?                            
         BZ    *+14                NO                                           
         MVC   TLLINE,BCSPACES     CLEAR LINE                                   
         OI    TLUSTAT,TLUTINS     FLAG THIS LINE INSERTED                      
*                                                                               
         LH    RF,CCOUNT           YES - ADD RECORD TWICE                       
         LA    RF,1(RF)                                                         
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        RESET KEY NUMBER                             
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  TLLNUM,BODUB1       RESET LINE NUMBER                            
         NI    TLUSTAT,TLUTINS     RESET STATUS BYTE                            
         GOTOX ('TSARIO',AGROUTS),TSAADD                                        
*                                                                               
         MVC   TLLINE,BOWORK1      RESTORE LINE                                 
*                                                                               
SORT10   LH    RF,CCOUNT           SET NEW ORDERING FOR LINE COUNT              
         LA    RF,1(RF)                                                         
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        RESET KEY NUMBER                             
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  TLLNUM,BODUB1       SET LINE NUMBER FROM KEY NUMBER              
         XC    TLUSTAT,TLUSTAT     RESET STATUS BYTE                            
*                                                                               
         GOTOX ('TSARIO',AGROUTS),TSAADD  ADD TEMPORARY COPY OF RECORD          
*                                                                               
         CLI   BOBYTE2,FF          ADD AFTER FLAG SET?                          
         BNE   SORT12              NO                                           
         BAS   RE,ADDIN            ADD RECORD AFTER THIS RECORD                 
*                                                                               
SORT12   STH   R0,TLNUM            RE-GET ORIGINAL FOR READ SEQUENCE            
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
         B     SORT06              NEXT IN LIST                                 
*                                                                               
SORT14   XC    TLKEY,TLKEY         DELETE ALL CURRENT RECORDS                   
         MVC   TLKSES,TWASESNL                                                  
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SORT16              END OF FILE                                  
         CLC   TLKSES,TWASESNL     SAME NEST LEVEL?                             
         BNE   SORT16              NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         B     SORT14                                                           
*                                                                               
SORT16   XC    TLKEY,TLKEY         MOVE SHUFFLED TO CURRENT SESSION             
         MVI   TLKSES,TLKSKEYC     TEMPORARY SHUFFLED NEST LEVEL                
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SORTX               END OF FILE                                  
         CLI   TLKSES,TLKSKEYC     STILL TEMPORARY LEVEL?                       
         BNE   SORTX               NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         MVC   TLKSES,TWASESNL     RESTORE TO CURRENT NEST LEVEL                
         GOTOX (RF),TSAADD                                                      
         B     SORT16                                                           
*                                                                               
SORTX    MVC   LSLST#X,LSLST#1     SET START                                    
         NI    LSLTIND1,FF-LSLTISOL                                             
         XR    RF,RF                                                            
         ICM   RF,3,CCOUNT         COUNT OF ITEMS ADDED TO LIST                 
         BZ    EXITOK              NOTHING                                      
         AH    RF,LSLST#X                                                       
         BCTR  RF,0                                                             
         STH   RF,LSLST#X          SET CORRECT VALUE FOR END                    
         OI    LSLTIND1,LSLTISOL                                                
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO ADD A RECORD SAVED IN SVLST TO CURRENT POINTER IN LIST   *         
***********************************************************************         
         SPACE 1                                                                
IN       USING TLSTD,SVLST                                                      
ADDIN    NTR1  ,                                                                
         MVI   IN.TLKSES,TLKSKEYC  SET TEMPORARY SHUFFLE KEY                    
         LH    RF,CCOUNT           SET NEW ORDERING FOR LINE COUNT              
         LA    RF,1(RF)                                                         
         STH   RF,CCOUNT                                                        
         STCM  RF,3,IN.TLKSNUM     RESET KEY NUMBER                             
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  IN.TLLNUM,BODUB1    SET LINE NUMBER FROM KEY NUMBER              
         XC    IN.TLUSTAT,IN.TLUSTAT                                            
         GOTOX ('TSARIO',AGROUTS),BOPARM,('TSAADD',IN.TLSTD)                    
         B     EXITOK                                                           
         DROP  IN                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO REDISPLAY A SCREENS WORTH OF DATA BASED ON NEW RECORDS   *         
***********************************************************************         
         SPACE 1                                                                
REDRAW   NTR1  ,                                                                
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         LH    R1,LSPAG#1          SET LIST NO. OF END OF PAGE                  
         AH    R1,LSLINPAG         MAXIMUM NUMBER OF LINES ON A PAGE            
         BCTR  R1,0                                                             
         STH   R1,LSPAG#X          SET LAST NUMBER                              
*                                                                               
         CLC   LSPAG#X,LSLST#X     WILL LIST ALL FIT ON 1 SCREEN?               
         BL    *+10                NO                                           
         MVC   LSPAG#X,LSLST#X     SET END OF PAGE                              
         LH    R1,LSPAG#X                                                       
         SH    R1,LSPAG#1                                                       
         STCM  R1,3,LSNUMPAG       SET ACTUAL NUMBER OF LINES ON PAGE           
*                                                                               
         LH    R1,LS1STLIN         DISPLACEMENT TO FIRST LIST LINE              
         A     R1,ATWA                                                          
         USING FHD,R1                                                           
*                                                                               
         XR    RE,RE                                                            
         LH    RF,LSLINLEN         LENGTH OF A LINE                             
         MH    RF,LSLINPAG         NUMBER OF LINES ON A PAGE                    
         LA    RF,FHD(RF)                                                       
         BCTR  RF,0                RF=A(END OF LIST PORTION-1)                  
*                                                                               
RDRW02   IC    RE,FHLN             SET INDEX IN RE                              
         OI    FHII,FHIIVA         SET FIELD VALIDATED                          
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         NI    FHAT,FF-FHATHI      TURN OFF HIGHLIGHT                           
         LR    R2,RE                                                            
         SH    R2,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER                        
         BO    *+8                                                              
         LA    R2,FHDAD(R2)                                                     
         EX    R2,*+4                                                           
         MVC   FHDA(0),BCSPACES    SPACE FILL TEXT PORTION                      
*                                                                               
         BXLE  R1,RE,RDRW02        REPEAT FOR ALL FIELDS                        
         DROP  R1                                                               
*                                                                               
         TM    LSLTIND1,LSLTISOL   LIST HAS DATA IN IT?                         
         BZ    RDRWX               NO                                           
*                                                                               
         MVC   LSLINE#,LSPAG#1     SET CURRENT LINE TO FIRST ON SCREEN          
*                                                                               
RDRW04   LH    R2,LSLINE#          CURRENT LINE OF SCREEN                       
         CH    R2,LSPAG#X          FINISHED DRAWING SCREEN YET?                 
         BH    RDRWX               YES                                          
         SH    R2,LSPAG#1                                                       
         MH    R2,LSLINLEN                                                      
         AH    R2,LS1STLIN                                                      
         STH   R2,LSCURLIN         SET DISPLACEMENT TO CURRENT LINE             
         A     R2,ATWA                                                          
S        USING FHD,R2              R2=A(SUB-ACTION FIELD)                       
*                                                                               
         CLC   TLNUM,LSLINE#       ALREADY HAVE RECORD?                         
         BE    RDRW05              YES                                          
         MVC   TLNUM,LSLINE#                                                    
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
*                                                                               
RDRW05   TM    TLUSTAT,TLUTINS     THIS LINE INSERTED?                          
         BZ    RDRW06              NO                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,S.FHLN                                                        
         LA    RF,S.FHD(RF)                                                     
         ST    RF,AINSFLD                                                       
*                                                                               
         NI    TLUSTAT,FF-TLUTINS                                               
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
RDRW06   LA    R5,LSLIN            CURRENT DISPLAY COLUMNS                      
         USING LINTABD,R5                                                       
*                                                                               
RDRW08   CLI   LINTABD,LINTEOT     DISPLAYED ALL FIELDS?                        
         BE    RDRW16              YES                                          
*                                                                               
         LH    RE,LINHDR           DISPLACEMENT TO HEADER FIELD IN LINE         
         LA    R4,S.FHD(RE)                                                     
FLD      USING FHD,R4              R4=A(FIELD HEADER FOR OUTPUT)                
         TM    LININDS,LINIOPEN    ENSURE OPEN INPUT FIELD IS OPEN              
         BZ    *+8                                                              
         NI    FLD.FHAT,FF-FHATPR                                               
         MVC   FVIHDR,FLD.FHD      SAVE OFF FIELD HEADER                        
*                                                                               
         TM    LININDS,LINIOPEN    OPEN FIELD?                                  
         BZ    RDRW12              NO                                           
*                                                                               
         GOTOX AGEN,BOPARM,ODATA,DDIS,FLD.FHD,AIOREC                            
         B     RDRW14                                                           
*                                                                               
RDRW12   GOTOX AGEN,BOPARM,ODATA,DNDIS,LINFLD#,AIOREC                           
*                                                                               
         LH    R1,LINFLD#          FIELD NUMBER                                 
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AFDRADDR         INDEX INTO FORMATTED FIELD ELEMENTS          
         L     R1,0(R1)                                                         
         USING FDRELD,R1                                                        
         LH    RE,LINDSP           DISPLALECMENT INTO FIELD                     
         LA    RF,FLD.FHDA(RE)                                                  
         IC    RE,FDRLCLEN         COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      COPY FIELD INTO CLOSED FIELD                 
         DROP  R1                                                               
*                                                                               
RDRW14   LA    R5,LINTABL(R5)      NEXT FIELD                                   
         B     RDRW08                                                           
         DROP  R5,FLD                                                           
*                                                                               
RDRW16   LH    RF,LSLINE#          NEXT LINE ON SCREEN                          
         LA    RF,1(RF)                                                         
         STH   RF,LSLINE#                                                       
         B     RDRW04              DO NEXT LINE                                 
*                                                                               
RDRWX    XC    DOFLAG,DOFLAG                                                    
         XC    DOFRCNT,DOFRCNT                                                  
         XC    DOTOCNT,DOTOCNT                                                  
         XC    DODLCNT,DODLCNT                                                  
         XC    DORPCNT,DORPCNT                                                  
         XC    DOINCNT,DOINCNT                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO REDISPLAY A SCREENS WORTH OF SUB-ACTION FIELDS           *         
***********************************************************************         
         SPACE 1                                                                
REACT    NTR1  ,                                                                
         TM    LSLTIND1,LSLTISOL   LIST HAS DATA IN IT?                         
         BZ    REACX               NO                                           
*                                                                               
         XC    AFRSTFR,AFRSTFR     RESET FIELD ADDRESSES                        
         XC    AFRSTTO,AFRSTTO                                                  
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   LSLINE#,LSPAG#1     SET CURRENT LINE TO FIRST ON SCREEN          
*                                                                               
REAC02   LH    R2,LSLINE#          CURRENT LINE OF SCREEN                       
         CH    R2,LSPAG#X          FINISHED DRAWING SCREEN YET?                 
         BH    REACX               YES                                          
         SH    R2,LSPAG#1                                                       
         MH    R2,LSLINLEN                                                      
         AH    R2,LS1STLIN                                                      
         STH   R2,LSCURLIN         SET DISPLACEMENT TO CURRENT LINE             
         A     R2,ATWA                                                          
S        USING FHD,R2              R2=A(SUB-ACTION FIELD)                       
*                                                                               
         CLC   TLNUM,LSLINE#       ALREADY HAVE RECORD?                         
         BE    REAC04              YES                                          
         MVC   TLNUM,LSLINE#                                                    
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
*                                                                               
REAC04   OC    TLUSTAT,TLUSTAT     ANY ACTIONS FOR THIS RECORD                  
         BZ    REAC10              NO                                           
*                                                                               
         LA    RF,TLUACTS          ACTION NAME DISPLAY TABLE                    
         USING TLUACTSD,RF                                                      
REAC06   CLI   0(RF),EOT           UNKNOWN FLAG IN TSAR RECORD                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BOBYTE1,TLUSTAT     COPY STATUS FLAG                             
         NC    BOBYTE1,TLUFLAG     IS THIS ACTION REQUEST SET?                  
         BNZ   *+12                YES                                          
         LA    RF,TLULQ(RF)                                                     
         B     REAC06              NEXT KNOWN ACTION                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,TLUNAME                                                     
         A     RE,AOVERWRK         RE=A(NAME OF ACTION)                         
         MVC   S.FHDA(SBLEN),0(RE) MOVE OUT NAME INTO FIELD                     
         OI    S.FHOI,FHOITR       TRANSMIT IT                                  
         OI    S.FHII,FHIIVA       SET FIELD VALID                              
*                                                                               
         TM    TLUSTAT,TLUSCPY+TLUSMVE                                          
         BZ    *+18                                                             
         OC    AFRSTFR,AFRSTFR    SET ADDRESS OF FIRST 'FROM' FIELD             
         BNZ   *+8                                                              
         ST    R2,AFRSTFR                                                       
*                                                                               
         TM    TLUSTAT,TLUSBEF+TLUSAFT                                          
         BZ    *+18                                                             
         OC    AFRSTTO,AFRSTTO     SET ADDRESS OF FIRST 'TO' FIELD              
         BNZ   *+8                                                              
         ST    R2,AFRSTTO                                                       
*                                                                               
         XR    RE,RE                                                            
         IC    RE,S.FHLN                                                        
         LA    R1,S.FHD(RE)        R1=A(FIRST DATA FIELD)                       
FLD      USING FHD,R1                                                           
*                                                                               
         LA    RF,S.FHD            START OF THIS LINE                           
         AH    RF,LSLINLEN         LENGTH OF A LINE                             
         BCTR  RF,0                RF=A(END OF THIS LINE-1)                     
*                                                                               
REAC08   IC    RE,FLD.FHLN         SET INDEX IN RE                              
         OI    FLD.FHII,FHIIVA     SET FIELD VALIDATED                          
         OI    FLD.FHOI,FHOITR     TRANSMIT FIELD                               
         OI    FLD.FHAT,FHATHI     TURN ON HIGHLIGHT                            
         BXLE  R1,RE,REAC08        REPEAT FOR ALL FIELDS ON LINE                
         DROP  RF,S,FLD                                                         
*                                                                               
REAC10   LH    RF,LSLINE#          NEXT LINE ON SCREEN                          
         LA    RF,1(RF)                                                         
         STH   RF,LSLINE#                                                       
         B     REAC02              DO NEXT LINE                                 
*                                                                               
REACX    B     EXITOK              FINISHED                                     
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* SCRIPT COMPILATION INVOKER - GOOD NAME EH?                          *         
***********************************************************************         
         SPACE 1                                                                
STUMBLE  GOTOX ('PUTRAC',AGROUTS),BOPARM,('RACTCHA',AIOREC)                     
         MVC   GSRECACT,GCRACCHA+(RACDATE-RACELD)                               
*                                                                               
         GOTOX AGENLST,BOPARM,OLIST,LMNTUPD                                     
         BL    EXITL                                                            
*                                                                               
         ICM   R1,15,=AL4(XIO11+XOWRITE+XOCONFIL)                               
         GOTOX ('XIO',AGROUTS)     WRITE FILE RECORD BACK                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    GSINDSL1,FF-(GSIDIRTY) FLAG CLEAN RECORD                         
*                                                                               
         L     R2,AIO1             GET THIS SESSION UPDATIVE RECORD             
         USING TLSTD,R2                                                         
         XC    TLKEY,TLKEY                                                      
         MVC   TLKSES,TWASESNL                                                  
         OI    TLKSES,TLKSUPDT                                                  
*                                                                               
         GOTOX ('TSARIO',AGROUTS),BOPARM,('TSARDH',TLSTD)                       
         BNE   STM02                                                            
         GOTOX ('TSARIO',AGROUTS),BOPARM,('TSADEL',TLSTD)                       
*                                                                               
STM02    LA    R0,TLSTD            RESET IO AREA                                
         LH    R1,=Y(IOAREALN-L'IODA-L'IOWORK)                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   TLKSES,TWASESNL     SET THIS IS AN UPDATIVE TSAR RECORD          
         OI    TLKSES,TLKSUPDT                                                  
         L     R4,AIOREC           GET LATEST COPY OF RECORD                    
         XR    R5,R5                                                            
         ICM   R5,3,CTJLEN-CTJREC(R4) LENGTH OF RECORD ON FILE RECORD           
         LA    R1,TLMINLNQ(R5)                                                  
         STCM  R1,3,TLRLEN         R1=TSAR RECORD LENGTH                        
*                                                                               
         LA    R0,TLUSER           MOVE INTO TSAR RECORD FROM AIOREC            
         LR    R1,R5                                                            
         MVCL  R0,R4                                                            
         GOTOX ('TSARIO',AGROUTS),BOPARM,('TSAADD',TLSTD)                       
*                                                                               
         GOTOX AGENRPT,BOPARM,OREP,RPQINIT                                      
*                                                                               
         GOTOX ('SAVVAL',AGROUTS)  SAVE ALL VALUES (TIA GETS CORRUPTED)         
*                                                                               
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         GOTOX VREPORT,REPBLK      SHOULD INITIALISE REPORT                     
*                                                                               
         MVI   REPACTN,REPAOPN     OPEN THE REPORT                              
         GOTOX VREPORT,REPBLK                                                   
         CLI   REPERRS,0           TEST FOR OPEN ERRORS                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   REPHW1(L'OLSCOM),OLSCOM                                          
         GOTOX AGETEL,BOPARM,('CTDSCELQ',AIOREC),0                              
         BNE   STM04                                                            
*                                                                               
DSC      USING CTDSCD,BOELEM                                                    
         MVC   REPPW1(L'FL@DESC),FL@DESC                                        
         XR    RF,RF                                                            
         IC    RF,DSC.CTDSCLEN                                                  
         SH    RF,=H'3'            CODEºLENGTHºMVC                              
         EX    RF,*+4                                                           
         MVC   REPPW1+L'FL@DESC+1(0),DSC.CTDSC                                  
*                                                                               
STM04    MVC   DATANUM,FL@LINE                                                  
         MVC   ASMDISP,FL@DSP                                                   
         MVI   REPACTN,REPAPUT     START THE REPORT                             
         GOTOX VREPORT,REPD                                                     
         GOTOX VREPORT,REPD        PUT A SPACE LINE                             
         DROP  DSC                                                              
*                                                                               
         L     R0,AIO1             RESET AIO1 TO USE AS BUFFER                  
         LH    R1,=Y(IOAREALN-L'IODA-L'IOWORK)                                  
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R1,AIO1             SET FIRST TIME KEY                           
         MVC   0(L'CTJKEY,R1),GSRECKEY                                          
         MVC   SCRPNAME,GSRECKEY+(CTJKID-CTJKEY)                                
*                                                                               
         XC    BOOKPARM,BOOKPARM   RESET PARAMETER LIST FOR GETBOOK             
         LA    R1,BOOKPARM                                                      
         MVC   0(4,R1),AIO1        SET A(IO AREA)                               
         MVI   0(R1),0             SET TO RESOLVE ALL NESTED BOOKS              
         XC    BOWORK1,BOWORK1                                                  
         LA    RE,BOWORK1          80 CHARACTER CARD AREA                       
         ST    RE,4(R1)                                                         
         MVC   8(4,R1),VDMGR       A(DATA MANAGER)                              
         L     RF,BCAUTL                                                        
         MVC   12(1,R1),TNUM-UTLD(RF)  SET TERMINAL NUMBER                      
*                                                                               
         GOTOX ASCRMBLE            INVOKE THE SCRIPT COMPILER                   
*                                                                               
         MVI   REPACTN,REPACLO     CLOSE THE REPORT                             
         GOTO1 VREPORT,REPBLK                                                   
         CLI   REPERRS,0                                                        
         BE    *+6                                                              
         DC    H'0'                CLOSE ERROR                                  
*                                                                               
         GOTOX ('RESVAL',AGROUTS)  RESTORE VALUES                               
         MVC   TWASRV,BCSPACES                                                  
         MVC   TWASRV(4),=CL4'=DQU'                                             
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         DROP  R7                  GET RID OF TWSAVE                            
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SBLEN    EQU   4                   LENGTH OF A SUB-ACT FIELD                    
LENLINE  EQU   72                  LENGTH OF TEXT PORTION OF TLLINE             
HSCRAMT  EQU   10                  HORIZONTAL SCROLL AMOUNT EQUATE              
MAXEQFR  EQU   8                   MAX LENGTH OF AN EQUATE NAME                 
MAXEQTO  EQU   32                  MAX LENGTH OF AN EQUATE VALUE                
*                                                                               
OLSCOM   DC    CL40'Online SCRIPT compilation output'                           
*                                                                               
TLUACTS  DS    0H              *** TABLE OF TSAR SUB-ACTION NAMES ***           
         DC    AL1(TLUSDEL),AL2(FL@DEL-OVERWRKD)                                
         DC    AL1(TLUSREP),AL2(FL@REPL-OVERWRKD)                               
         DC    AL1(TLUSINS),AL2(FL@INSRT-OVERWRKD)                              
         DC    AL1(TLUSCPY),AL2(FL@COPY-OVERWRKD)                               
         DC    AL1(TLUSMVE),AL2(FL@MOVE-OVERWRKD)                               
         DC    AL1(TLUSAFT),AL2(FL@AFTER-OVERWRKD)                              
         DC    AL1(TLUSBEF),AL2(FL@BFR-OVERWRKD)                                
         DC    AL1(EOT)                                                         
*                                                                               
TLUACTSD DSECT                                                                  
TLUFLAG  DS    XL1                                                              
TLUNAME  DS    AL2                                                              
TLULQ    EQU   *-TLUACTSD                                                       
*                                                                               
FIL20    CSECT                                                                  
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL CT#COPY,SBLEN,L     COPY                                         
         DCDDL CT#MOVE,SBLEN,L     MOVE                                         
         DCDDL CT#DEL,SBLEN,L      DELETE                                       
         DCDDL CT#BFR,SBLEN,L      BEFORE                                       
         DCDDL CT#AFTER,SBLEN,L    AFTER                                        
         DCDDL CT#REPL,SBLEN,L     REPLACE (CHANGE THIS TO REPLICATE)           
         DCDDL CT#INSRT,SBLEN,L    INSERT                                       
         DCDDL CT#YES,4,L          YES                                          
         DCDDL CT#NO,4,L           NO                                           
         DCDDL CT#DESC,15,L        DESCRIPTION                                  
         DCDDL CT#LINE,5,L         LINE                                         
         DCDDL GE#HALF,8,L         HALF                                         
         DCDDL CT#TITLE,10,L       TITLE                                        
         DCDDL CT#DSP,8,L          DISPLAY (USED FOR DISPLACEMENT)              
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
**********************************************************************          
* SCRIPT SOURCE ONE PASS COMPILER                                    *          
**********************************************************************          
         SPACE 1                                                                
SCRAMBLE CSECT                                                                  
         NMOD1 0,SCRAMBLE,R6,RR=RE                                              
         L     RC,AOVERWRK                                                      
         L     R5,AREP                                                          
         USING REPD,R5                                                          
SCRM02   XC    NUMERRS,NUMERRS     RESET ERROR COUNT FOR THIS LINE              
         XC    ASMLINE,ASMLINE     RESET COMPILED LINE                          
         BAS   RE,LINEIN           GET NEXT LINE                                
         BL    SCRM12              NO MORE LINES                                
         CLC   =C'@@',BOWORK1      CONDITIONAL ASSEMBLY - NOTHING ELSE          
         BE    SCRM12                                                           
         LH    RF,LINENUM          INCREMENT LINE COUNT                         
         LA    RF,1(RF)                                                         
         STH   RF,LINENUM                                                       
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  DATANUM,BODUB1      PUT LINE NUMBER ON PRINTLINE                 
         MVC   DATA1,BOWORK1       SAVE A COPY OF THIS LINE                     
*                                                                               
         TM    SCPFLAG1,SF1NAME    GOT NAME & BUILT DUMMY RECORD?               
         BO    SCRM08              YES                                          
*                                                                               
         CLI   BOWORK1,C'*'        COMMENT BEFORE NAME                          
         BNE   SCRM04              NO                                           
         BAS   RE,PRNTLINE         YES - IGNORE IT                              
         B     SCRM02                                                           
*                                                                               
SCRM04   CLC   =C'#SCRIPT',BOWORK1 SCRIPT NAME CARD?                            
         BNE   SCRM06              NO                                           
         BAS   RE,GETNAME          BUILD SCRIPT NAME                            
         XC    ASM1,ASM1                                                        
         BAS   RE,PRNTLINE                                                      
         B     SCRM02                                                           
*                                                                               
SCRM06   BAS   RE,BLDFRST                                                       
         OI    SCPFLAG1,SF1NAME    SET RECORD BUILT FLAG                        
*                                                                               
SCRM08   CLC   =CL4'#DEF',BOWORK1  LINE DEFINES EQUATES                         
         BNE   SCRM10              NO                                           
         BAS   RE,EQUATES          SET UP EQUATE TABLE                          
         B     SCRM02                                                           
*                                                                               
SCRM10   BAS   RE,SUBEQUS          SUBSTITUTE EQUATES                           
         BAS   RE,COMPILE          COMPILE SCRIPT CODE + PRINT LINE             
*                                                                               
         TM    SCPFLAG1,SF1ERRS    ANY COMPILE ERRORS                           
         BO    SCRM02              YES - DON'T BOTHER WITH RECORD               
         BAS   RE,BLDELS           BUILD ELEMENTS                               
         B     SCRM02                                                           
*                                                                               
SCRM12   OI    SCPFLAG1,SF1LAST    FLAG FOR LAST TIME                           
         BAS   RE,SETLBL           SORT OUT LABELS                              
         TM    SCPFLAG1,SF1ERRS    ANY ERRORS?                                  
         BZ    SCRM14              NO                                           
*                                                                               
         MVC   REPPW1,BCSPACES                                                  
         XC    ASM1,ASM1                                                        
         MVC   REPPW1+30(L'ERCNTMSG),ERCNTMSG                                   
         LH    R1,TOTERRS                                                       
         CVD   R1,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  REPPW1+34(4),BODUB1                                              
         BAS   RE,PRNTLINE                                                      
         B     XITOK                                                            
*                                                                               
SCRM14   BAS   RE,BLDELS           BUILD LAST ELEMENT                           
         TM    SCPFLAG1,SF1ERRS    ANY ERRORS?                                  
         BO    XITOK               NO                                           
*                                                                               
         MVC   REPPW1+30(L'RECUPMSG),RECUPMSG                                   
         BAS   RE,PRNTLINE                                                      
*                                                                               
         ICM   R1,15,=AL4(XOWRITE+XOCONFIL+XIO2)                                
         TM    SCPFLAG1,SF1ADD                                                  
         BZ    *+8                                                              
         ICM   R1,15,=AL4(XOADD+XOCONFIL+XIO2)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BE    XITOK                                                            
         DC    H'0'                                                             
*                                                                               
XITH     CLI   *,0                 SET CC HIGH                                  
         B     XIT                                                              
XITL     CLI   *,FF                SET CC LOW                                   
         B     XIT                                                              
XITOK    CR    RB,RB               SET CC EQUAL                                 
*                                                                               
XIT      XIT1  ,                   EXIT WITH CC SET                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SET UP EQUATE SUBSTITUTION TABLE                         *         
***********************************************************************         
         SPACE 1                                                                
EQUATES  NTR1                                                                   
         GOTOX ASQUASH,BOPARM,BOWORK1,L'BOWORK1                                 
         L     R0,4(R1)            L'INPUT AFTER SQUASHING                      
         LA    RF,BOWORK1+4        START OF INPUT STRING + #DEF                 
         LR    R3,RF               R3 = POINTER TO CURRENT POSN.                
*                                                                               
EQU02    LA    RE,BOWORK1          START OF LINE                                
         SR    RF,RE               CURRENT DISPLACEMENT INTO LINE               
         CR    RF,R0               REACHED END OF LINE?                         
         BNL   EQU24               YES                                          
*                                                                               
         CLI   0(R3),C' '          SPACE PRESENT?                               
         BH    *+8                 NO                                           
         LA    R3,1(R3)            IGNORE IT                                    
         LR    R1,R3               SAVE A(START OF EQUATED FROM)                
*                                                                               
EQU04    CLI   0(R3),C' '          END OF EQUATED FROM VALUE?                   
         BE    EQU06               YES                                          
         LA    R3,1(R3)            NEXT CHARACTER ON LINE                       
         LR    RF,R3                                                            
         SR    RF,RE               RF = CURRENT DISPLACEMENT INTO LINE          
         CR    RF,R0               REACHED END OF LINE?                         
         BL    EQU04               NO                                           
*                                                                               
         MVI   THISERR,5                                                        
         MVI   ASMERR,FF                                                        
         LR    RF,R3               CURRENT POSITION                             
         SR    RF,R1               GIVES LENGTH OF EQUATED FROM VALUE           
         GOTOX ERRHNDL,BOPARM,((RF),(R1))                                       
         B     EQU24                                                            
*                                                                               
         USING EQUTABD,R2                                                       
EQU06    LA    R2,EQUTABLE         TABLE OF EQUATES                             
         LR    RF,R3                                                            
         SR    RF,R1               RF NOW HOLDS L'EQUATED FROM VALUE            
         CH    RF,=Y(MAXEQFR)      MAXIMUM LABEL LENGTH                         
         BNH   EQU14                                                            
*                                                                               
EQU08    MVI   THISERR,9           LABEL IS TOO BIG                             
         MVI   ASMERR,FF                                                        
         GOTOX ERRHNDL,BOPARM,((RF),(R1))                                       
*                                                                               
EQU10    CLI   0(R3),C';'          LOOK FOR DELIMETER                           
         BE    EQU12                                                            
         LA    R3,1(R3)            BUMP ON                                      
         LR    RF,R3                                                            
         SR    RF,RE               RF = CURRENT DISPLACEMENT INTO LINE          
         CR    RF,R0               REACHED END OF LINE?                         
         BL    EQU10               NO                                           
*                                                                               
         MVI   THISERR,5                                                        
         MVI   ASMERR,FF                                                        
         LR    RF,R3               CURRENT POSITION                             
         SR    RF,R1               RF = L'EQUATED FROM VALUE                    
         GOTOX ERRHNDL,BOPARM,((RF),(R1))                                       
         B     EQU24                                                            
*                                                                               
EQU12    LA    R3,1(R3)            BUMP PAST ;                                  
         LR    RF,R3                                                            
         B     EQU02                                                            
*                                                                               
EQU14    CLI   EQULEN,0            FIND NEXT FREE IN EQUATE TABLE               
         BE    EQU16                                                            
         XR    RE,RE                                                            
         IC    RE,EQULEN                                                        
         LA    R2,0(RE,R2)                                                      
         B     EQU14                                                            
*                                                                               
EQU16    MVC   EQUNAME,BCSPACES    SPACE FILL EQUATED NAME AREA                 
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   EQUNAME(0),0(R1)    MOVE IN THE EQUATED NAME VALUE               
*                                                                               
         LA    R3,1(R3)            BUMP PAST SPACE AT END OF NAME               
         LR    R1,R3               SAVE A(START OF EQUATED TO VALUE)            
         LA    RE,BOWORK1                                                       
*                                                                               
EQU18    CLI   0(R3),C';'          END OF EQUATED TO DELIMITER?                 
         BE    EQU20               YES                                          
         LA    R3,1(R3)                                                         
         LR    RF,R3                                                            
         SR    RF,RE               RF = CURRENT DISPLACEMENT INTO LINE          
         CR    RF,R0                                                            
         BL    EQU18               STILL WITHIN BOUNDARIES                      
*                                                                               
         MVI   THISERR,5                                                        
         MVI   ASMERR,FF                                                        
         GOTOX ERRHNDL,BOPARM,((RF),(R1))                                       
         B     EQU24                                                            
*                                                                               
EQU20    LR    RF,R3                                                            
         SR    RF,R1               RF = L'EQUATED VALUE                         
         CH    RF,=Y(MAXEQTO)      MAXIMUM OPERAND LENGTH                       
         BL    EQU22               LENGTH IS OK                                 
*                                                                               
         MVI   THISERR,9           LABEL TOO LONG                               
         MVI   ASMERR,FF                                                        
         GOTOX ERRHNDL,BOPARM,((RF),(R1))                                       
         LA    R3,1(R3)            BUMP PAST THE ;                              
         LR    RF,R3                                                            
         B     EQU02                                                            
*                                                                               
EQU22    BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   EQUDATA,0(R1)       MOVE IN THE EQUATED VALUE                    
         LA    RF,EQLNQ+1(RF)      SET TOTAL LENGTH                             
         STC   RF,EQULEN           SAVE THE TOTAL LENGTH OF ELEMENT             
         LA    R2,0(RF,R2)                                                      
         MVI   EQULEN,0            ENSURE EOT DELIMITED                         
         LA    R3,1(R3)            FOR ;                                        
         LR    RF,R3                                                            
         B     EQU02                                                            
*                                                                               
EQU24    XC    ASM1,ASM1                                                        
         BAS   RE,PRNTLINE                                                      
         B     XITOK                                                            
         DROP  R2                                                               
*                                                                               
EQUTABD  DSECT                                                                  
EQULEN   DS    CL1                 L'INPUT                                      
EQUNAME  DS    CL(MAXEQFR)         EQUATE NAME                                  
EQUDATA  DS    0C                  EQUATE VALUE                                 
EQLNQ    EQU   *-EQUTABD                                                        
*                                                                               
SCRAMBLE CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SUSTITUTE EQUATED VALUES ON THE ASSEMBLED LINE           *         
***********************************************************************         
         SPACE 1                                                                
SUBEQUS  NTR1                                                                   
         CLI   BOWORK1,C'*'        IGNORE THIS LINE                             
         BE    XITOK               YES                                          
         GOTOX ASQUASH,BOPARM,BOWORK1,L'BOWORK1                                 
         L     R0,4(R1)            L'INPUT AFTER SQUASHING                      
         LA    R3,BOWORK1          R3 = CURRENT CHARACTER POINTER               
*                                                                               
SUB02    LR    RF,R3                                                            
         LA    RE,BOWORK1                                                       
         SR    RF,RE                                                            
         CR    RF,R0                                                            
         BH    XITOK               REACHED END OF LINE                          
*                                                                               
         CLI   0(R3),SUBCHAR       SUBSTITUTION CHARACTER?                      
         BE    SUB04               YES                                          
         LA    R3,1(R3)                                                         
         B     SUB02               NEXT ON LINE                                 
*                                                                               
SUB04    CLI   1(R3),SUBCHAR       TWO SUBCHARS MEANS LEAVE IN 1                
         BE    SUB18                                                            
         LR    RF,R3               START OF STRING TO REPLACE                   
         LA    R3,1(R3)            EQUATE TO REPLACE                            
*                                                                               
SUB06    CLI   0(R3),C';'          END                                          
         BE    SUB10                                                            
         CLI   0(R3),C' '          MISSING DELIMETER                            
         BE    SUB08                                                            
         LA    R3,1(R3)                                                         
         B     SUB06               NEXT ON LINE                                 
*                                                                               
SUB08    MVI   THISERR,5                                                        
         MVI   ASMERR,0                                                         
         LR    R1,RF               A(START OF TEXT)                             
         LR    RF,R3                                                            
         SR    RF,R1               L'TEXT (PRESERVES R3)                        
         GOTOX ERRHNDL,BOPARM,((RF),(R1))                                       
         LA    R3,1(R3)            BUMP PAST SPACE                              
         B     SUB02                                                            
*                                                                               
SUB10    LR    R1,R3               END OF EQUATE NAME                           
         SR    R1,RF               START OF EQUATE NAME INCLUDING %             
         SH    R1,=H'2'            1 FOR % 1FOR THE EX                          
         LA    R2,EQUTABLE                                                      
         USING EQUTABD,R2                                                       
         XR    RE,RE                                                            
*                                                                               
SUB12    CLI   EQULEN,0                                                         
         BE    SUB14               EQUATE NOT FOUND                             
         EX    R1,*+8                                                           
         BE    SUB16                                                            
         CLC   EQUNAME(0),1(RF)    RF POINTS TO THE % REMEMBER                  
         IC    RE,EQULEN                                                        
         LA    R2,0(RE,R2)                                                      
         B     SUB12                                                            
*                                                                               
SUB14    MVI   THISERR,8                                                        
         MVI   ASMERR,0                                                         
         LA    R1,1(R1)            L' OF EQUATE                                 
         GOTOX ERRHNDL,BOPARM,((R1),(RF))                                       
         LA    R3,1(R3)            FOR THE ;                                    
         B     SUB02                                                            
*                                                                               
SUB16    IC    RE,0(R2)            L' WHOLE THING EQUATE ELEMENT                
         SH    RE,=Y(EQLNQ)        L' WHAT MUST BE MOVED IN                     
*                                                                               
         LA    R4,BOWORK1                                                       
         AR    R4,R0                                                            
         SR    R4,R3               R4=L' WHAT'S LEFT ON LINE                    
         AR    R0,RE                                                            
         SR    R0,R1               NEW LENGTH OF THIS LINE IN R0                
         BCTR  R4,0                                                             
         EX    R4,*+4                                                           
         MVC   ASM1(0),0(R3)       SAVE WHAT'S LEFT OF LINE                     
*                                                                               
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),EQUDATA     MOVE IN EQUATE VALUE                         
         LA    R3,1(RE,RF)         SET R3 TO END OF THIS VALUE                  
*                                                                               
         LA    RF,BOWORK1+L'BOWORK1-1                                           
         SR    RF,R3                                                            
         EX    RF,*+4                                                           
         MVC   0(0,R3),BCSPACES    CLEAR DOWN LINE                              
*                                                                               
         EX    R4,*+4                                                           
         MVC   0(0,R3),ASM1        MOVE BACK WHAT`S LEFT                        
         XC    ASM1,ASM1                                                        
         B     SUB02                                                            
*                                                                               
SUB18    LA    R3,1(R3)            LEAVE IN FIRST %                             
         LR    RF,R3                                                            
         LA    RE,BOWORK1                                                       
         SR    RF,RE               CURRENT TEXT LENGTH PROCESSED                
         LR    R1,R0               TOTAL TEXT LENGTH                            
         SR    R1,RF                                                            
         BCTR  R1,0                L' LEFT TO MOVE -1 FOR MVC                   
         EX    R1,*+4                                                           
         MVC   0(0,R3),1(R3)       CLOSE UP 1 CHARACTER                         
         SH    R0,=H'1'            REDUCE LINE LENGTH                           
         B     SUB02                                                            
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO COMPILE A LINE FROM BOWORK1 TO ASMLINE                   *         
***********************************************************************         
         SPACE 1                                                                
SAVE     USING ASMTABD,SVASM                                                    
COMPILE  NTR1                                                                   
*                                                                               
         L     RF,DISPTOT                                                       
         CURED (RF),(5,ASMDISP),0,DMCB=BOPARM,ZERO=NOBLANK                      
*                                                                               
         GOTOX ASQUASH,BOPARM,BOWORK1,L'BOWORK1                                 
         ICM   R0,15,4(R1)         L'INPUT AFTER SQUASHING                      
         BZ    CMPLX               NO INPUT                                     
         ST    R0,ALINLN                                                        
         LA    R3,BOWORK1          SOURCE LINE                                  
         LA    R4,ASMLINE                                                       
*                                                                               
         LR    RF,R0               SUBSTITUTE ' ' FOR 'º'                       
         LA    RE,BOWORK1          TO ALLOW CHECKING FOR SPACES                 
         CLI   0(RE),C'º'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C' '                                                       
         LA    RE,1(RE)                                                         
         BCT   RF,*-16                                                          
*                                                                               
CMPL02   XC    SVASM,SVASM         CLEAR LAST ASMTAB ENTRY                      
         CLI   0(R3),C' '          SPACE BEFORE INSTRUCTION?                    
         BNE   *+8                                                              
         LA    R3,1(R3)            BUMP PAST SPACE                              
*                                                                               
         TM    SCPFLAG1,SF1CMT     INSIDE /*....*/ PAIR?                        
         BO    CMPL04              YES                                          
*                                                                               
         TM    SCPFLAG1,SF1LCMT    LAST PROCESSED WAS COMMENT CLOSE             
         BO    *+12                                                             
         CLI   BOWORK1,C'*'        WHOLE LINE IS A COMMENT?                     
         BE    CMPLX               YES                                          
*                                                                               
         NI    SCPFLAG1,FF-SF1LCMT RESET COMMENT CLOSE FLAG                     
*                                                                               
         CLC   =C'//',0(R3)        COMMENT TO END OF LINE?                      
         BE    CMPLX               YES - NOTHING LEFT TO PROCESS                
*                                                                               
         CLC   =C'/*',0(R3)        START OF /*....*/ PAIR?                      
         BNE   CMPL08              NO                                           
         OI    SCPFLAG1,SF1CMT     TURN ON INDICATOR BYTE                       
*                                                                               
CMPL04   CLC   =C'*/',0(R3)        LOOK FOR */ TO CLOSE COMMENT                 
         BE    CMPL06              GOT ONE!                                     
         LA    R3,1(R3)            NEED TO GET DISPLACEMENT INTO LINE           
         LR    RE,R3                                                            
         LA    RF,BOWORK1          START OF INPUT LINE                          
         SR    RE,RF                                                            
         CR    RE,R0               END OF LINE?                                 
         BH    CMPLX               YES                                          
         B     CMPL04              NOT YET - STILL WITHIN COMMENT               
*                                                                               
CMPL06   NI    SCPFLAG1,FF-SF1CMT TURN OFF COMMENT FLAG                         
         LA    R3,2(R3)            GO PAST THE */                               
         LR    RE,R3                                                            
         LA    RF,BOWORK1          START OF INPUT LINE                          
         SR    RE,RF               DISPLACEMENT INTO LINE                       
         CR    RE,R0               END OF LINE?                                 
         BNL   CMPLX               YES                                          
         OI    SCPFLAG1,SF1LCMT    SET COMMENT CLOSE FLAG                       
         B     CMPL02              CARRY ON PROCESSING                          
*                                                                               
CMPL08   LA    R2,ASMTAB           OPCODE TABLE                                 
         USING ASMTABD,R2                                                       
         LA    RE,ASMTABLQ         L'ASMTAB                                     
         L     RF,=A(ASMTABX)      END OF TABLE                                 
         A     RF,BORELO           RELOCATE IT                                  
         XR    R1,R1                                                            
*                                                                               
CMPL10   IC    R1,MTCHLEN          LENGTH OF MNEMONIC-1                         
         EX    R1,CMPLCMP                                                       
         BE    CMPL12              MATCH ON OP-CODE                             
         BXLE  R2,RE,CMPL10                                                     
         BAS   RE,INVOPCDE         ROUTINE TO DEAL WITH INVALID OPCODE          
         BE    CMPL16              PROCESS NEXT INSTRUCTION                     
         B     CMPLX               NOTHING LEFT ON LINE                         
*                                                                               
CMPLCMP  CLC   MNEMONIC(0),0(R3)                                                
*                                                                               
CMPL12   MVC   SVASM,ASMTABD       SAVE ASMTAB ENTRY                            
         MVC   0(2,R4),SAVE.OPCODE PUT OPCODE ONTO ASMLINE                      
         LA    R4,2(R4)            NEXT FREE ON ASMLINE                         
         LA    R3,1(R1,R3)         NEXT AFTER MNEMONIC                          
         CLI   SAVE.MXOPERS,0      ANY OPERANDS?                                
         BE    CMPL16              NO                                           
*                                                                               
         CLI   0(R3),C' '          FOR SPACE TWIXT OPCODEºOPERAND               
         BNE   *+8                 (IF OPCODE HAS OPERANDS)                     
         LA    R3,1(R3)                                                         
*                                                                               
         TM    SAVE.MXOPERS,MXLBL+MXBRNCH                                       
         BZ    *+8                                                              
         BAS   RE,SETLBL           SAVE BRANCH EQUATES                          
         LA    RF,DATATAB                                                       
*                                                                               
CMPL14   CLI   0(RF),EOT                                                        
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN DATA TYPE - DIE AT ONCE              
*                                                                               
         CLC   SAVE.DATATYPE,0(RF) MATCH ON PROCESSING TYPE?                    
         BE    *+12                YES                                          
         LA    RF,DATATABL(RF)     BUMP TO NEXT                                 
         B     CMPL14                                                           
*                                                                               
         ICM   RF,15,1(RF)         GO VALIDATE THIS INPUT                       
         A     RF,BORELO                                                        
         BASR  RE,RF                                                            
         BNE   CMPLX                                                            
*                                                                               
CMPL16   CLI   0(R3),C' '          NEXT CHARACTER IS A SPACE?                   
         BNE   *+8                 NO                                           
         LA    R3,1(R3)            BUMP PAST SPACE                              
         CLI   0(R3),C';'          NEXT CHARACTER MUST BE A DELIMETER           
         BE    CMPL18              ELSE SOME COCK-UP HAS OCCURRED               
         BAS   RE,INVDLMTR                                                      
         BNE   CMPLX               END OF LINE REACHED                          
*                                                                               
CMPL18   LA    R3,1(R3)            FOR ; DELIMITER                              
         LR    RF,R3               SAVE THIS ADDRESS                            
         LA    RE,BOWORK1          START OF LINE                                
         LA    RF,1(RF)            FOR SPACE AT END                             
         SR    RF,RE               GIVES DISPLACEMENT INTO INPUT LINE           
         CR    RF,R0               REACHED END OF LINE?                         
         BNH   CMPL02              NO                                           
*                                                                               
CMPLX    L     R5,AREP                                                          
         USING REPD,R5                                                          
         MVC   ASM1,ASMLINE                                                     
         GOTOX PRNTLINE                                                         
         B     XITOK                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GET A LINE OF SOURCE                                     *         
***********************************************************************         
         SPACE 1                                                                
LINEIN   NTR1                                                                   
         MVC   BOWORK1,BCSPACES                                                 
         GOTOX AGETBOOK,BOOKPARM   GET NEXT LINE                                
         MVC   BOWORK1+72(6),BCSPACES  GET RID OF THE FUCKING NUMBERS           
         TM    8(R1),X'80'         END OF BOOKS?                                
         BO    XITL                YES                                          
         CLI   8(R1),0             ERRORS?                                      
         BE    XITOK               NO                                           
         TM    8(R1),X'02'         DELETED - CAN IGNORE IT                      
         BE    XITOK                                                            
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO PRINT OUT A LINE OF COMPILED SCRIPT & OBJECT CODE        *         
***********************************************************************         
         SPACE 1                                                                
PRNTLINE NTR1                                                                   
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         TR    ASM1,TRTAB          FORMAT THIS FOR OUTPUT                       
         GOTOX VREPORT,REPD        PRINT LINE                                   
         XC    ASM1,ASM1           RESET THIS                                   
         B     XITOK                                                            
         SPACE 2                                                                
***********************************************************************         
* BUILD NAME FOR SCRIPT FROM #SCRIPT LINE                             *         
***********************************************************************         
         SPACE 1                                                                
GETNAME  NTR1                                                                   
         LA    R1,BOWORK1+8        FIRST PAST THE #SCRIPT MARKER                
*                                                                               
NAM02    CLI   0(R1),C' '          LOOK FOR FIRST NON-SPACE AFTER               
         BH    *+12                #SCRIPT CARD                                 
         LA    R1,1(R1)                                                         
         B     NAM02                                                            
*                                                                               
         MVC   SCRPNAME,0(R1)      SAVE NAME OF SCRIPT                          
         LA    R0,L'SCRPNAME                                                    
         LA    RF,SCRPNAME+L'SCRPNAME-1                                         
*                                                                               
NAM04    CLI   0(RF),C' '          CHANGE ALL FUNNIES TO BCSPACES               
         BH    *+8                                                              
         MVI   0(RF),C' '                                                       
         BCTR  RF,0                                                             
         BCT   R0,NAM04                                                         
         B     XITOK                                                            
         SPACE 2                                                                
***********************************************************************         
* BUILD CT7KEY AND FORMAT FIRST RECORD                                *         
***********************************************************************         
         SPACE 1                                                                
BLDFRST  NTR1                                                                   
         LA    R1,SCRPNAME+L'CT7KCODE                                           
         CLI   0(R1),C' '                                                       
         BE    BDFS02                                                           
         MVI   THISERR,11          NAME IS TOO LONG                             
         BAS   RE,ERRHNDL                                                       
         BAS   RE,PRNTLINE                                                      
         B     XITL                                                             
*                                                                               
BDFS02   LA    R2,IOKEY                                                         
         USING CT7REC,R2                                                        
         XC    CT7KEY,CT7KEY                                                    
         MVI   CT7KTYP,CT7KTYPQ                                                 
         MVC   CT7KCODE,SCRPNAME                                                
         L     R1,=A(XORDD+XOLOCK+XOCONFIL+XIO2)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BE    BDFS04              FOUND NORMALLY                               
*                                                                               
         TM    IOERR,IOERNF        RECORD FOUND?                                
         BZ    *+12                NO - MUST BE ADDED                           
         OI    SCPFLAG1,SF1ADD                                                  
         B     BDFS04                                                           
*                                                                               
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BO    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
BDFS04   L     R2,AIO2             SET UP AN EMPTY RECORD                       
         MVC   CT7KEY,IOKEY                                                     
         XC    CT7STAT,CT7STAT     RESET RECORD STATUS                          
         MVC   CT7LEN,=AL2(CT7KEYL+1)                                           
         MVI   CT7DATA,0                                                        
         B     XITOK                                                            
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE EBCDIC INPUT                                    *         
***********************************************************************         
         SPACE 1                                                                
EBCDIC   NTR1                                                                   
         XR    R1,R1                                                            
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AH    R0,=H'1'            TO ENSURE LOOP EXECUTES CORRECTLY            
*                                                                               
         LR    R2,R3               R3=A(START OF OPERAND)                       
         CLI   0(R2),C'"'          INPUT DELIMITED BY "..."                     
         BNE   *+12                                                             
         OI    SCPFLAG1,SF1DINK    SET DELIMITED BY "..."                       
         B     EBCD06                                                           
*                                                                               
EBCD02   CLI   0(R2),C';'          ; DELIMITS INSTRUCTION                       
         BE    EBCD14                                                           
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    EBCD04                                                           
         BAS   RE,INVDLMTR         MISSING DELIMITER DEALT WITH HERE            
         B     XIT                                                              
*                                                                               
EBCD04   IC    R1,LENNOW           INCREMENT OPERAND LENGTH                     
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW           AND SAVE IT                                  
         LA    R2,1(R2)            NEXT CHARACTER                               
         BCT   R0,EBCD02           TRY AGAIN                                    
         BAS   RE,INVOPRND         MISSING DELIMITER DEALT WITH HERE            
         B     XIT                                                              
*                                                                               
EBCD06   IC    R1,LENNOW           INCREMENT OPERAND LENGTH                     
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW           AND SAVE IT                                  
         LA    R2,1(R2)            GO PAST FIRST " IN PAIR                      
*                                                                               
EBCD08   CLI   0(R2),C'"'          LOOK FOR SECOND DINK                         
         BNE   EBCD10                                                           
         IC    R1,LENNOW           INCREMENT LENGTH FOR SECOND DINK             
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW           AND SAVE IT                                  
         LA    R2,1(R2)            NEXT CHARACTER                               
         CLI   0(R2),C';'          ; DELIMITS INSTRUCTION                       
         BE    EBCD14                                                           
         BAS   RE,INVDLMTR         MISSING DELIMITER                            
         B     XIT                                                              
*                                                                               
EBCD10   ZIC   R1,LENNOW           BUMP CURRENT LENGTH                          
         LA    R1,1(R1)            ..                                           
         STC   R1,LENNOW           AND SAVE IT                                  
         CLI   0(R2),C';'          ; DELIMITS INSTRUCTION                       
         BNE   EBCD12                                                           
         BAS   RE,INVDLMTR         INVALID OPERAND                              
         B     XIT                                                              
*                                                                               
EBCD12   LA    R2,1(R2)            NEXT CHARACTER                               
         BCT   R0,EBCD08                                                        
         BAS   RE,INVDLMTR         MISSING DELIMITER                            
         B     XIT                                                              
*                                                                               
EBCD14   ST    R2,AR3              POINT TO DELIMITER                           
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    EBCD16                                                           
         BAS   RE,INVOPRND         MISSING OPERAND                              
         B     XIT                                                              
*                                                                               
EBCD16   XR    R1,R1                                                            
         IC    R1,LENNOW           LENGTH OF INPUT OPERAND                      
         TM    SCPFLAG1,SF1DINK    INPUT SURROUNDED BY "..." ?                  
         BZ    *+8                 NO                                           
         SH    R1,=H'2'            NEED TO REDUCE LENGTH BY 2                   
*                                                                               
         CVD   R1,BODUB1           PACK LENGTH OF OPERAND                       
         OI    BODUB1+L'BODUB1-1,X'0F' ZONE IT                                  
         UNPK  0(2,R4),BODUB1      GET THE LENGTH IN EBCDIC - 2 CHARS           
         LA    R4,2(R4)            NEXT FREE ON ASMLINE                         
*                                                                               
         BCTR  R1,0                                                             
         LR    RF,R3                                                            
         TM    SCPFLAG1,SF1DINK    IN 'DINK' MODE                               
         BZ    *+8                 NO                                           
         LA    RF,1(RF)            BUMP PAST THE FIRST "                        
         EX    R1,*+4              MOVE IN OPERAND - DINKS                      
         MVC   0(0,R4),0(RF)                                                    
*                                                                               
         LA    R4,1(R1,R4)                                                      
         L     R3,AR3                                                           
         NI    SCPFLAG1,FF-SF1DINK TURN OFF DINK MODE                           
*                                                                               
         CR    RB,RB               SET CONDITION CODE EQUAL                     
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE LABELS                                          *         
***********************************************************************         
         SPACE 1                                                                
LABEL    NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AH    R0,=H'1'            TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
LBL02    CLI   0(R2),C';'          DELIMITER?                                   
         BE    LBL06               YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    LBL04                                                            
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
LBL04    IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,LBL02            CONTINUE                                     
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
LBL06    ST    R2,AR3              SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    LBL08                                                            
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
LBL08    IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         EX    R1,*+4              MOVE IN OPERAND                              
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)         NEXT FREE ON ASMLINE                         
         L     R3,AR3              A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE GAPPED INPUT                                    *         
***********************************************************************         
         SPACE 1                                                                
GAPPED   NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AH    R0,=H'1'            TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
GAP02    CLI   0(R2),C';'          DELIMITER?                                   
         BE    GAP06               YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    GAP03                                                            
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP03    CLI   0(R2),C'0'          TEST VALID NUMBER                            
         BL    *+12                NO                                           
         CLI   0(R2),C'9'          TEST VALID NUMBER                            
         BNH   GAP04               NO                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP04    IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,GAP02            CONTINUE                                     
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP06    ST    R2,AR3              SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    GAP08                                                            
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP08    IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         EX    R1,*+8              PACK OPERAND                                 
         B     *+10                                                             
         PACK  BODUB1,0(0,R3)                                                   
         OI    BODUB1+L'BODUB1-1,X'0F' ZONE IT                                  
*                                                                               
         CVB   R1,BODUB1           ENSURE WITHIN RANGE 1-99                     
         CH    R1,=H'99'                                                        
         BNH   GAP12               INVALID OPERAND                              
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
GAP12    UNPK  0(2,R4),BODUB1      SET LENGTH                                   
         LA    R4,2(R4)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+4              MOVE IN OPERAND                              
         MVC   0(0,R4),BCSPACES                                                 
         LA    R4,1(R1,R4)         Next free on ASMLINE                         
         L     R3,AR3              A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE FIXED INPUT                                     *         
***********************************************************************         
         SPACE 1                                                                
FIXED    NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AH    R0,=H'1'            TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
FIX02    CLI   0(R2),C';'          DELIMITER?                                   
         BE    FIX06               YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    FIX03                                                            
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX03    CLI   0(R2),C'0'          TEST VALID NUMBER                            
         BL    *+12                NO                                           
         CLI   0(R2),C'9'          TEST VALID NUMBER                            
         BNH   FIX04               NO                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX04    IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,FIX02            CONTINUE                                     
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX06    ST    R2,AR3              SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    FIX08                                                            
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX08    IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         EX    R1,*+8              PACK OPERAND                                 
         B     *+10                                                             
         PACK  BODUB1,0(0,R3)                                                   
         OI    BODUB1+L'BODUB1-1,X'0F' ZONE IT                                  
*                                                                               
         CVB   R1,BODUB1           ENSURE WITHIN RANGE 1-99                     
         CH    R1,=H'99'                                                        
         BNH   FIX12               INVALID OPERAND                              
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
FIX12    IC    R1,SAVE.DATALEN     FIXED LENGTH INPUT FIELD                     
         BCTR  R1,0                                                             
         SLL   R1,4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         UNPK  0(0,R4),BODUB1      UNPACK DATA ONTO OUTPUT LINE                 
*                                                                               
         IC    R1,SAVE.DATALEN                                                  
         LA    R4,0(R1,R4)         NEXT FREE ON PRINT LINE                      
         L     R3,AR3              A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE HEXADECIMAL INPUT                               *         
***********************************************************************         
         SPACE 1                                                                
HEXED    NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AH    R0,=H'1'            TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
HEX02    CLI   0(R2),C';'          DELIMITER?                                   
         BE    HEX12               YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    HEX04                                                            
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
HEX04    CLI   0(R2),C'0'          TEST VALID NUMBER                            
         BL    HEX06               NO                                           
         CLI   0(R2),C'9'          TEST VALID NUMBER                            
         BH    HEX08               NO                                           
         B     HEX10                                                            
*                                                                               
HEX06    CLI   0(R2),C'A'          TEST VALID LETTER                            
         BL    HEX08               NO                                           
         CLI   0(R2),C'F'          TEST VALID LETTER                            
         BH    HEX08               NO                                           
         B     HEX10                                                            
*                                                                               
HEX08    BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
HEX10    IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,HEX02            CONTINUE                                     
         BAS   RE,INVOPRND         ERROR HANDLER OPERAND TOO LONG               
         B     XIT                 SETS OWN CC                                  
*                                                                               
HEX12    ST    R2,AR3              SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    HEX14                                                            
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
HEX14    IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         EX    R1,*+4              MOVE IN OPERAND                              
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)         NEXT FREE IN ASMLINE                         
         L     R3,AR3              A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE NUMERIC INPUT                                   *         
***********************************************************************         
         SPACE 1                                                                
NUMERIC  NTR1                                                                   
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AH    R0,=H'1'            TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
NUMB02   CLI   0(R2),C';'          DELIMITER?                                   
         BE    NUMB08              YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BH    NUMB04                                                           
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB04   CLI   0(R2),C'0'          TEST VALID NUMBER                            
         BL    *+12                NO                                           
         CLI   0(R2),C'9'          TEST VALID NUMBER                            
         BNH   NUMB06              NO                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB06   IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,NUMB02           CONTINUE                                     
         BAS   RE,INVDLMTR         ERROR HANDLER FOR MISSING DELIMITER          
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB08   ST    R2,AR3              SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BH    NUMB10                                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB10   IC    R1,LENNOW           LENGTH OF OPERAND                            
         CVD   R1,BODUB1           PACK OPERAND LENGTH                          
         OI    BODUB1+L'BODUB1-1,X'0F' ZONE IT                                  
         UNPK  0(2,R4),BODUB1      SET LENGTH IN COMPILE LINE                   
         LA    R4,2(R4)            BUMP TO NEXT FREE ON LINE                    
*                                                                               
         LA    R0,FF               MAX VALUE OF BYTE                            
         CLI   SAVE.DATALEN,5                                                   
         BNE   *+10                                                             
         XR    R0,R0                                                            
         ICM   R0,3,=XL2'7FFF'     MAX VALUE OF HALFWORD                        
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8              PACK OPERAND                                 
         B     *+10                                                             
         PACK  BODUB1,0(0,R3)                                                   
*                                                                               
         CVB   RF,BODUB1                                                        
         CR    R0,RF               ENSURE OPERAND IS WITHIN LIMITS              
         BH    NUMB12                                                           
         BAS   RE,INVOPRND         ERROR HANDLER FOR INVALID OPERAND            
         B     XIT                 SETS OWN CC                                  
*                                                                               
NUMB12   EX    R1,*+4              MOVE IN OPERAND                              
         MVC   0(0,R4),0(R3)                                                    
         LA    R4,1(R1,R4)         NEXT FREE IN ASMLINE                         
         L     R3,AR3              A(DELIMITER)                                 
         CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* INVALID OPCODE ERROR PROCESSED HERE                                 *         
***********************************************************************         
         SPACE 1                                                                
INVOPCDE NTR1  ,                                                                
         L     R0,ALINLN           LENGTH OF A LINE                             
         LA    RE,BOWORK1          A(START OF LINE)                             
         AR    RE,R0               A(END OF LINE)                               
         LR    RF,R3               SAVE CURRENT POINT                           
*                                                                               
XOPC02   CLI   0(RF),C';'          DELIMITER FOR NEXT INSTRUCTION               
         BE    XOPC04                                                           
         CLI   0(RF),C' '          SPACE BEFORE OPERAND                         
         BE    XOPC04                                                           
         LA    RF,1(RF)            NEXT POSITION IN LINE                        
         CR    RF,R3               RE = LAST POSITION ON LINE                   
         BH    XOPC02              NOT REACHED END OF LINE YET                  
         LR    RF,RE                                                            
*                                                                               
XOPC04   ST    RF,ARSPR                                                         
         SR    RF,R3               GET L'OPCODE                                 
         MVI   THISERR,3           ERROR #                                      
         MVI   ASMERR,FF           FLAG FOR ASSEMBLY ERROR                      
         GOTOX ERRHNDL,BOPARM,((RF),(R3))                                       
*                                                                               
         LA    RE,BOWORK1                                                       
         AR    RE,R0               RE = END OF INPUT LINE                       
         L     R3,ARSPR                                                         
         LR    RF,R3                                                            
XOPC06   CLI   0(R3),C';'          DELIMITER FOUND?                             
         BE    XOPC08              YES                                          
         LA    R3,1(R3)            BUMP ALONG 1                                 
         CR    RE,R3               END OF LINE?                                 
         BNL   XOPC06              NO                                           
*                                                                               
         SR    R3,RF                                                            
         MVI   THISERR,5           MISSING DELIMITER                            
         MVI   ASMERR,FF           SET ASSEMBLY ERROR                           
         GOTOX ERRHNDL,BOPARM,((R3),(RF))                                       
         B     XITL                                                             
*                                                                               
XOPC08   CR    RB,RB               SET CC EQUAL                                 
         XIT1  REGS=(R3,R4)        KEEP CURRENT POINTERS                        
         SPACE 2                                                                
***********************************************************************         
* MISSING DELIMITER DEALT WITH HERE                                   *         
***********************************************************************         
         SPACE 1                                                                
INVDLMTR NTR1                                                                   
         L     R0,ALINLN           LENGTH OF A LINE                             
         LA    RE,BOWORK1          RE=A(START OF LINE)                          
         LR    RF,R3               R3=A(CURRENT POINTER)                        
*                                                                               
XDLM02   BCTR  RF,0                                                             
         CLI   0(RF),C';'          PREVIOUS DELIMITER?                          
         BNE   *+12                                                             
         LA    RF,1(RF)            FIRST AFTER DELIMITER                        
         B     XDLM04                                                           
         CR    RF,RE               REACHED START OF LINE?                       
         BH    XDLM02                                                           
*                                                                               
XDLM04   AR    RE,R0               RE=A(END OF LINE)                            
*                                                                               
XDLM06   CLI   0(R3),C';'          NEXT DELIMITER?                              
         BE    XDLM08                                                           
         CLI   0(R3),C' '          NEXT OPERAND                                 
         BE    XDLM08                                                           
         LA    R3,1(R3)                                                         
         CR    R3,RE               END OF LINE REACHED?                         
         BNH   XDLM06              NOT YET                                      
*                                                                               
XDLM08   ST    R3,ARSPR            COPY R3                                      
         SR    R3,RF               GET LENGTH OF ERROR STRING                   
         MVI   THISERR,5           ERROR MISSING DELIMITER                      
         MVI   ASMERR,FF                                                        
         GOTOX ERRHNDL,BOPARM,((R3),(RF))                                       
*                                                                               
         L     R3,ARSPR            RESTORE CURRENT LINE POINTER                 
         LA    R1,BOWORK1                                                       
         AR    R1,R0                                                            
         CR    R3,R1                                                            
         BNL   XITL                THIS WAS LAST INSTRUCTION                    
*                                                                               
         LA    RE,BOWORK1          RE = START OF LINE                           
         LR    RF,R3                                                            
*                                                                               
XDLM10   BCTR  RF,0                                                             
         CLI   0(RF),C';'          PREVIOUS DELIMITER?                          
         BNE   *+12                                                             
         LA    RF,1(RF)            FIRST AFTER DELIMITER                        
         B     XDLM12                                                           
*                                                                               
         CR    RF,RE               REACHED START OF LINE?                       
         BH    XDLM10                                                           
*                                                                               
XDLM12   AR    RE,R0               RE = A(END OF LINE)                          
         CLI   0(R3),C';'          NEXT INSTRUCTION                             
         BE    XDLM14                                                           
         LA    R3,1(R3)                                                         
         CR    RE,R3               END OF LINE?                                 
         BH    XDLM12              NO                                           
*                                                                               
         SR    R3,RF                                                            
         GOTOX ERRHNDL,BOPARM,((R3),(RF))                                       
         B     XITL                END OF LINE AND NO DELIMITER                 
*                                                                               
XDLM14   CR    RB,RB                                                            
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* INVALID OPERAND DEALT WITH HERE                                     *         
***********************************************************************         
         SPACE 1                                                                
INVOPRND NTR1  ,                                                                
         L     R0,ALINLN           LENGTH OF A LINE                             
         LA    RE,BOWORK1          A(START OF LINE)                             
         LR    RF,R3                                                            
*                                                                               
XOPD02   BCTR  RF,0                                                             
         CLI   0(RF),C';'          PREVIOUS DELIMITER?                          
         BNE   *+12                                                             
         LA    RF,1(RF)            FIRST AFTER DELIMITER                        
         B     XOPD04                                                           
*                                                                               
         CR    RF,RE               REACHED START OF LINE?                       
         BH    XOPD02                                                           
*                                                                               
XOPD04   AR    RE,R0               RE = A(END OF LINE)                          
*                                                                               
XOPD06   CLI   0(R3),C';'          DELIMITER?                                   
         BE    XOPD08                                                           
         CLI   0(R3),C' '          EMBEDDED SPACE OR MISSING DELIMITER          
         BE    XOPD08                                                           
         LA    R3,1(R3)                                                         
         CR    RE,R3               END OF LINE?                                 
         BH    XOPD06              NO                                           
*                                                                               
XOPD08   ST    R3,ARSPR                                                         
         SR    R3,RF               GET LENGTH OF INVALID CODE                   
         MVI   THISERR,6           ERROR INVALID OPERAND                        
         MVI   ASMERR,FF                                                        
         GOTOX ERRHNDL,BOPARM,((R3),(RF))                                       
*                                                                               
         LA    RE,BOWORK1                                                       
         L     R3,ARSPR                                                         
         LR    RF,R3                                                            
*                                                                               
XOPD10   BCTR  RF,0                                                             
         CLI   0(RF),C';'             PREVIOUS DELIMITER?                       
         BNE   *+12                                                             
         LA    RF,1(RF)            FIRST AFTER DELIMITER                        
         B     XOPD12                                                           
         CR    RF,RE               REACHED START OF LINE?                       
         BH    XOPD10                                                           
*                                                                               
XOPD12   AR    RE,R0               RE = A(END OF LINE)                          
*                                                                               
XOPD14   CLI   0(R3),C';'          DELIMITER?                                   
         BE    XOPD16              YES                                          
         LA    R3,1(R3)                                                         
         CR    RE,R3               END OF LINE?                                 
         BH    XOPD14              NO                                           
*                                                                               
         MVI   THISERR,5           ERROR MISSING DELIMITER                      
         MVI   ASMERR,FF                                                        
         SR    R3,RF                                                            
         GOTOX ERRHNDL,BOPARM,((R3),(RF))                                       
         B     XITL                                                             
*                                                                               
XOPD16   CR    RB,RB                                                            
         XIT1  REGS=(R3,R4)                                                     
         SPACE 2                                                                
***********************************************************************         
* ERROR HANDLING ROUTINE                                              *         
***********************************************************************         
         SPACE 1                                                                
ERRHNDL  NTR1                                                                   
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         LH    RF,TOTERRS          INCREMENT TOTAL ERROR COUNT                  
         LA    RF,1(RF)                                                         
         STH   RF,TOTERRS                                                       
*                                                                               
         CLI   NUMERRS,9           MAX ERRORS SUPPORTED PER LINE                
         BNH   HERR02                                                           
         MVI   REPPWA,C' '                                                      
         MVC   REPPWA+1(L'REPPWA-1),REPPWA  CLEAR IT DOWN                       
         MVC   REPPWA+30(L'ERTOOMNY),ERTOOMNY                                   
         MVC   0(4,R4),=C'@@@@'    Indicate error on O/P line                   
         LA    R4,4(R4)                                                         
         B     HERR06                                                           
*                                                                               
HERR02   XR    R2,R2                                                            
         IC    R2,NUMERRS          CURRENT ERROR COUNT - 1                      
         MH    R2,=Y(L'REPPW1)     LENGTH OF ERROR LINE IN MEMORY               
         LA    R2,ASMERRS(R2)      R2 = A(FIRST FREE ERROR LINE)                
*                                                                               
         LA    RF,ERRTAB           TABLE OF ERROR MESSAGES                      
HERR04   CLI   0(RF),EOT           END OF TABLE?                                
         BNE   *+6                                                              
         DC    H'0'                UNKNOWN ERROR                                
*                                                                               
         CLC   THISERR,0(RF)       MATCH ON ERROR CODE?                         
         BE    *+12                YES                                          
         LA    RF,L'ERRTAB(RF)                                                  
         B     HERR04                                                           
*                                                                               
         MVC   40(31,R2),1(RF)     MOVE IN ERROR MESSAGE                        
         MVC   30(9,R2),=C'**ERROR**' IDENTIFIER                                
*                                                                               
         XR    RF,RF                                                            
         IC    RF,NUMERRS          BUMP NUMBER OF ERRORS ON THIS LINE           
         LA    RF,1(RF)            ..                                           
         STC   RF,NUMERRS          AND SAVE IT                                  
*                                                                               
         CLI   ASMERR,0            TEXT TO OUTPUT?                              
         BE    HERR06              NO                                           
*                                                                               
         XR    RF,RF                                                            
         IC    RF,0(R1)            L' FAULTY CODE SEGMENT                       
         L     RE,0(R1)            A(START OF ERROR)                            
*                                                                               
         BCTR  RF,0                L'BAD DATA ON BOWORK1                        
         CH    RF,=H'28'           MAX L' SUPPORTED FOR PRINTING                
         BL    *+8                 IS 29 CHARACTERS                             
         LA    RF,28                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RE)       MOVE IN BAD DATA TO ERROR LINE               
*                                                                               
         CLI   ASMERR,FF           ASSEMBLY ERROR?                              
         BNE   HERR06                                                           
         MVC   0(4,R4),=C'@@@@'    INDICATE ERROR ON O/P LINE                   
         LA    R4,4(R4)                                                         
*                                                                               
HERR06   XC    ASMERR,ASMERR       CLEAR ASMERR                                 
         OI    SCPFLAG1,SF1ERRS    ERROR FLAG ON                                
         XIT1  REGS=(R4)                                                        
         DROP  R5                                                               
*                                                                               
*                           1234567890123456789012345678901                     
ERRTAB   DS    0CL32                                                            
         DC    AL1(01),CL31'?????????????AVAILABLE?????????'                    
         DC    AL1(02),CL31'Compile error - No record write'                    
         DC    AL1(03),CL31'Command not found              '                    
         DC    AL1(04),CL31'?????????????AVAILABLE?????????'                    
         DC    AL1(05),CL31'Unable to find delimiting ;    '                    
         DC    AL1(06),CL31'Invalid operand value          '                    
         DC    AL1(07),CL31'Script has not been named      '                    
         DC    AL1(08),CL31'Equate name not found          '                    
         DC    AL1(09),CL31'Equate name not valid          '                    
         DC    AL1(10),CL31'Script requires END statement  '                    
         DC    AL1(11),CL31'Script name is invalid         '                    
         DC    AL1(12),CL31'Label is duplicated            '                    
         DC    AL1(13),CL31'Too many labels - contact DDS  '                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SAVE LABEL EQUATES                                       *         
***********************************************************************         
         SPACE 1                                                                
SETLBL   NTR1                                                                   
         TM    SCPFLAG1,SF1LAST    WANT TO RESOLVE ALL EQUATES?                 
         BO    SLBL20              YES                                          
*                                                                               
         XC    LENNOW,LENNOW       RESET LENGTH OF OPERAND                      
         XR    R0,R0                                                            
         IC    R0,SAVE.DATALEN     MAXIMUM LENGTH OF DATA                       
         AH    R0,=H'1'            TO ENSURE LOOP EXECUTES CORRECTLY            
         XR    R1,R1                                                            
         LR    R2,R3               R3=A(START OF OPERAND)                       
*                                                                               
SLBL02   CLI   0(R2),C';'          DELIMITER?                                   
         BE    SLBL04              YES                                          
         CLI   0(R2),C' '          SPACE MEANS MISSING DELIMITER                
         BNH   XIT                 LET ERROR HANDLING TAKE OVER                 
*                                                                               
         IC    R1,LENNOW           INCREMENT OPERAND INPUT LENGTH               
         LA    R1,1(R1)                                                         
         STC   R1,LENNOW                                                        
         LA    R2,1(R2)            NEXT CHARACTER ON INPUT LINE                 
         BCT   R0,SLBL02           CONTINUE                                     
         B     XIT                 LET ERROR HANDLING TAKE OVER                 
*                                                                               
SLBL04   ST    R2,AR3              SAVE A(DELIMITER)                            
         CLI   LENNOW,0            NO OPERAND ENTERED                           
         BNH   XIT                 LET ERROR HANDLER COPE                       
*                                                                               
         TM    SAVE.MXOPERS,MXLBL  IS THIS A LABEL                              
         BO    SLBL10              YES                                          
*                                                                               
         LA    R4,BCHTAB           BRANCH TABLE                                 
         XR    R0,R0                                                            
         ICM   R0,3,0(R4)          TABLE HAS A VALUE IN IT?                     
         BZ    SLBL18              NO                                           
         CLM   R0,3,=AL2(LBLALLOW)                                              
         BNH   SLBL06                                                           
         MVI   THISERR,13                                                       
         BAS   RE,ERRHNDL                                                       
         B     XIT                                                              
*                                                                               
SLBL06   LA    RF,2(R4)                                                         
         IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
*                                                                               
SLBL08   EX    R1,SLBMCH           SEE IF OPERAND IN TABLE ALREADY              
         BE    XIT                 YES - GOOD                                   
         LA    RF,LBLLEN(RF)                                                    
         BCT   R0,SLBL08                                                        
         B     SLBL18                                                           
*                                                                               
SLBL10   LA    R4,LBLTAB           BRANCH TABLE                                 
         XR    R0,R0                                                            
         ICM   R0,3,0(R4)          TABLE HAS A VALUE IN IT?                     
         BZ    SLBL18              NO                                           
         CLM   R0,3,=AL2(LBLALLOW)                                              
         BNH   SLBL12                                                           
         MVI   THISERR,13                                                       
         BAS   RE,ERRHNDL                                                       
         B     XIT                                                              
*                                                                               
SLBL12   LA    RF,2(R4)                                                         
         IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
*                                                                               
SLBL14   EX    R1,SLBMCH           SEE IF OPERAND IN TABLE ALREADY              
         BE    SLBL16              YES - DUPLICATE LABEL                        
         LA    RF,LBLLEN(RF)                                                    
         BCT   R0,SLBL08                                                        
         B     SLBL18                                                           
*                                                                               
SLBL16   MVI   THISERR,12          DUPLICATE LABEL                              
         MVI   ASMERR,1                                                         
         GOTOX ERRHNDL,BOPARM,(2,(R3))                                          
         B     XIT                                                              
*                                                                               
SLBMCH   CLC   2(0,RF),0(R3)                                                    
*                                                                               
SLBL18   XR    RF,RF                                                            
         ICM   RF,3,0(R4)          NUMBER OF ENTRIES IN TABLE                   
         LR    RE,RF               COPY IT                                      
         LA    RE,1(RE)            INCREMENT AND SAVE                           
         STCM  RE,3,0(R4)                                                       
         MH    RF,=Y(LBLLEN)       LENGTH OF 1 ENTRY                            
         LA    RF,2(RF,R4)         INDEX INTO TABLE                             
*                                                                               
         MVC   0(2,RF),LINENUM     SAVE LINE NUMBER                             
         IC    R1,LENNOW           LENGTH OF OPERAND                            
         BCTR  R1,0                                                             
         EX    R1,*+4              MOVE IN OPERAND                              
         MVC   2(0,RF),0(R3)                                                    
         B     XIT                                                              
*                                                                               
SLBL20   L     R5,AREP                                                          
         USING REPD,R5                                                          
         XR    R0,R0                                                            
         LA    R4,BCHTAB           ONLY NEED RESOLVE BRANCHES                   
         ICM   R0,3,0(R4)          TABLE HAS A VALUE IN IT?                     
         BZ    XITOK               NO                                           
*                                                                               
         LA    R2,2(R4)            FIRST BRANCH INSTRUCTION                     
*                                                                               
SLBL22   LA    R3,LBLTAB                                                        
         ICM   RF,3,0(R3)          COUNT OF BRANCHES                            
         BZ    SLBL26                                                           
         LA    R3,2(R3)                                                         
*                                                                               
SLBL24   CLC   2(2,R2),2(R3)       SEE IF OPERAND IN BOTH TABLES                
         BE    SLBL28              YES - GOOD                                   
         LA    R3,LBLLEN(R3)                                                    
         BCT   RF,SLBL24                                                        
*                                                                               
SLBL26   MVC   REPPW1+30(L'ERINVBCH),ERINVBCH                                   
         XR    RF,RF                                                            
         ICM   RF,3,0(R2)                                                       
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  REPPW1+61(5),BODUB1                                              
         MVC   REPPW1+50(2),2(R2)                                               
         BAS   RE,PRNTLINE                                                      
         LH    RF,TOTERRS          INCREMENT ERROR COUNT                        
         LA    RF,1(RF)                                                         
         STH   RF,TOTERRS                                                       
         OI    SCPFLAG1,SF1ERRS    SET ERROR ON BUILD                           
*                                                                               
SLBL28   LA    R2,LBLLEN(R2)                                                    
         BCT   R0,SLBL22                                                        
         B     XIT                                                              
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* BUILD AND ADD ELEMENTS                                              *         
***********************************************************************         
         SPACE  1                                                               
         PUSH  USING                                                            
         USING CTSCRD,BOELEM                                                    
BLDELS   NTR1                                                                   
         LA    RF,L'ASMLINE        GET LENGTH OF DATA ON ASMLINE                
         LA    R1,ASMLINE+L'ASMLINE-1                                           
         CLI   0(R1),0                                                          
         BH    BLDL02              FOUND LAST CHARACTER                         
         BCTR  R1,0                                                             
         BCT   RF,*-10                                                          
*                                                                               
         TM    SCPFLAG1,SF1LAST    TEST LAST TIME                               
         BO    BLDL12              YES                                          
         B     XITOK               NO DATA FOUND                                
*                                                                               
BLDL02   L     RE,DISPTOT          INCREMENT CUMULATIVE DISPLACEMENT            
         AR    RE,RF                                                            
         ST    RE,DISPTOT                                                       
*                                                                               
         TM    SCPFLAG1,SF1FRST    FIRST TIME IN?                               
         BO    BLDL04              NO                                           
*                                                                               
         XC    BOELEM,BOELEM       FIRST TIME IN - CLEAR ELEMENT                
         MVI   CTSCREL,CTSCRELQ    ELEMENT CODE                                 
         XC    SEQUENCE,SEQUENCE   CLEAR SEQUENCE NUMBER                        
         MVC   CTSCRSEQ,SEQUENCE                                                
         BCTR  RF,0                OK TO MOVE IN ALL THIS LINE                  
         EX    RF,*+4                                                           
         MVC   CTSCRDTA(0),ASMLINE MOVE IN LINE                                 
         LA    RF,4(RF)            ADD FIXED TO LENGTH                          
         STC   RF,CTSCRLEN         STORE LENGTH IN ELEMENT                      
         OI    SCPFLAG1,SF1FRST    SET FLAG ON SO DON`T DO AGAIN                
         B     XITOK                                                            
*                                                                               
BLDL04   XR    R0,R0                                                            
         IC    R0,CTSCRLEN         LENGTH OF ELEMENT AT PRESENT                 
         XC    ELNOWLQ,ELNOWLQ     CLEAR TEMP LENGTH HOLDER                     
         LA    R2,BOELEM                                                        
         AR    R2,R0               FIRST FREE DATA SLOT                         
         LA    R1,FF               AS MUCH AS CAN BE FITTED IN                  
         SR    R1,R0                                                            
         STC   R1,ELNOWLQ          HOW MUCH MORE CAN BE ADDED                   
         CR    R1,RF               CAN ALL THIS STUFF MOVE IN OK?               
         BL    BLDL06              NO                                           
*                                                                               
         BCTR  RF,0                OK TO MOVE IN ALL THIS LINE                  
         EX    RF,*+4                                                           
         MVC   0(0,R2),ASMLINE     MOVE IN LINE                                 
         LR    RE,R0               R0 HOLDS CURRENT LENGTH REMEMBER             
         LA    RF,1(RE,RF)         ADD LENGTH TO TOTAL                          
         STC   RF,CTSCRLEN         STORE LENGTH IN ELEMENT                      
         CLI   CTSCRLEN,FF         EXACTLY FILLED ELEMENT?                      
         BNE   XITOK               NO                                           
         XC    ELNOWLQ,ELNOWLQ     RESET AMOUNT TO MOVE INTO NEXT EL.           
         B     BLDL08                                                           
*                                                                               
BLDL06   SR    RF,R1               RF HOLDS WHAT MUST BE MOVED                  
         STC   RF,ELNOWLQ          STORE L'LEFT AFTER THIS MOVE                 
         LR    R4,R1               SAVE AMOUNT MOVED THIS TIME                  
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R2),ASMLINE     FILL THIS ELEMENT                            
*                                                                               
BLDL08   MVI   CTSCRLEN,FF         SET ELEMENT LENGTH TO MAXIMUM                
         XR    R1,R1                                                            
         IC    R1,SEQUENCE         BUMP SEQUENCE NO.                            
         LA    R1,1(R1)                                                         
         STC   R1,SEQUENCE                                                      
         MVC   CTSCRSEQ,SEQUENCE   SET SEQUENCE NUMBER                          
*                                                                               
         L     R2,AIO2                                                          
         USING CT7REC,R2                                                        
         XR    RF,RF               HAVE TO DO OWN SIZE CHECKS                   
         ICM   RF,1,CTSCRLEN       AS HELLO ADDS THEN CHECKS...                 
         XR    RE,RE                                                            
         ICM   RE,3,CT7LEN                                                      
         LA    RE,0(RF,RE)         NEW LENGTH IF WITH THIS ELEMENT              
         CLM   RE,3,MAXLEN                                                      
         BL    *+8                 ELEMENT FITS                                 
         BAS   RE,RECPUT           WRITE THIS RECORD & GET NEXT                 
*                                                                               
         GOTOX VHELLO,BOPARM,(C'P',CTFBIG),AIO2,BOELEM,ADDEND                   
         CLI   12(R1),0                                                         
         BE    BLDL10                                                           
         DC    H'0'                                                             
*                                                                               
BLDL10   XC    BOELEM,BOELEM       MOVE IN REMAINDER AND SET LENGTHS            
         MVI   CTSCREL,CTSCRELQ                                                 
         MVI   CTSCRLEN,3                                                       
         XR    RF,RF                                                            
         ICM   RF,1,ELNOWLQ        LENGTH LEFT TO MOVE IN                       
         BZ    XITOK                                                            
*                                                                               
         BCTR  RF,0                                                             
         LA    RE,ASMLINE(R4)      START OF NEXT PIECE OF DATA                  
         EX    RF,*+4                                                           
         MVC   CTSCRDTA(0),0(RE)                                                
         LA    RF,4(RF)            2 FOR FIXED, 1 FOR MVC                       
         STC   RF,CTSCRLEN         SAVE LENGTH                                  
         B     XITOK                                                            
*                                                                               
BLDL12   XR    R1,R1                                                            
         ICM   R1,1,SEQUENCE       SEQUENCE NUMBER IS 0 OR 1...N                
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,CTSCRSEQ                                                      
*                                                                               
         IC    R1,CTSCRLEN                                                      
         LA    R1,BOELEM(R1)                                                    
         SH    R1,=H'2'                                                         
         CLC   =C'**',0(R1)        DOES SCRIPT END WITH AN END?                 
         BE    BLDL16              YES                                          
*                                                                               
         CLI   CTSCRLEN,(FF-2)     WILL IT FIT IN THIS ELEMENT?                 
         BH    BLDL14              NO                                           
         LA    R1,2(R1)                                                         
         MVC   0(2,R1),=C'**'                                                   
         XR    R1,R1                                                            
         IC    R1,CTSCRLEN                                                      
         LA    R1,2(R1)                                                         
         STC   R1,CTSCRLEN                                                      
         B     BLDL16                                                           
*                                                                               
BLDL14   MVI   THISERR,10                                                       
         BAS   RE,ERRHNDL                                                       
         B     XITOK               NO WRITE IF ERRORS                           
*                                                                               
BLDL16   L     R2,AIO2                                                          
         USING CT7REC,R2                                                        
         XR    RF,RF               HAVE TO DO OWN SIZE CHECKS                   
         ICM   RF,1,CTSCRLEN       AS HELLO ADDS THEN CHECKS...                 
         XR    RE,RE                                                            
         ICM   RE,3,CT7LEN                                                      
         LA    RE,0(RF,RE)         NEW LENGTH IF WITH THIS ELEMENT              
         CLM   RE,3,MAXLEN                                                      
         BL    *+8                 ELEMENT FITS                                 
         BAS   RE,RECPUT           WRITE THIS SEQUENCE AND GET NEXT             
*                                                                               
         GOTOX VHELLO,BOPARM,(C'P',CTFBIG),AIO2,BOELEM,ADDEND                   
         CLI   12(R1),0                                                         
         BE    XITOK                                                            
         DC    H'0'                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A CONTROL FILE RECORD AND INITIALISE NEXT RECORD     *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
RECPUT   NTR1  ,                                                                
         LH    R1,RECSEQ           INCREMENT SEQUENCE # COUNTER                 
         LA    R1,1(R1)                                                         
         STH   R1,RECSEQ                                                        
*                                                                               
         L     R2,AIO2                                                          
         USING CT7REC,R2                                                        
         OC    CT7SEQNO,CT7SEQNO   FIRST RECORD (SEQUENCE #0)                   
         BZ    RPUT02              YES - SEQUENCE IS 0 OR 1..N                  
*                                                                               
         L     R2,AIO2             WRITE BACK RECORD                            
         L     R1,=AL4(XOWRITE+XOCONFIL+XIO2)                                   
         TM    SCPFLAG1,SF1ADD                                                  
         BZ    *+8                                                              
         L     R1,=AL4(XOADD+XOCONFIL+XIO2)                                     
         GOTOX ('XIO',AGROUTS) WRITE THIS SEQUENCE # TO FILE                    
         BE    RPUT08              INITIALISE NEXT SEQUENCE                     
         DC    H'0'                                                             
*                                                                               
RPUT02   L     R0,AIO1             SAVE COPY OF SEQUENCE #0 RECORD              
         L     RE,AIO2                                                          
         LH    R1,MAXLEN                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         TM    SCPFLAG1,SF1ADD     ADDING SEQUENCE #0 RECORD?                   
         BO    RPUT04              YES                                          
*                                                                               
         L     R2,AIO2             SET SEQUENCE #0 DELETE FLAG                  
         OI    CT7STAT,X'80'                                                    
         L     R1,=AL4(XOWRITE+XOCONFIL+XIO2)                                   
         GOTOX ('XIO',AGROUTS)     DELETE SEQUENCE #0                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
K7       USING CT7REC,IOKEY                                                     
RPUT04   NI    SCPFLAG1,FF-SF1ADD  RESET ADD FLAG                               
*                                                                               
         L     R2,AIO1             COPY OF RECORD 0                             
         MVC   K7.CT7KEY,CT7KEY    MOVE KEY COPY INTO IOKEY                     
         MVC   K7.CT7SEQNO,RECSEQ  SET SEQUENCE #1                              
         L     R1,=A(XORDD+XOLOCK+XOCONFIL+XIO2)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BE    RPUT06              FOUND SEQUENCE #1 RECORD                     
*                                                                               
         TM    IOERR,IOERNF        RECORD FOUND?                                
         BZ    *+12                NO - MUST BE ADDED                           
         OI    SCPFLAG1,SF1ADD                                                  
         B     RPUT06                                                           
*                                                                               
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BO    RPUT06              YES - THAT'S OK                              
         DC    H'0'                                                             
*                                                                               
RPUT06   L     R0,AIO1             RESTORE COPY OF SEQUENCE #0 RECORD           
         L     RE,AIO2                                                          
         LH    R1,MAXLEN                                                        
         LR    RF,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R2,AIO2                                                          
         MVC   CT7SEQNO,RECSEQ     SET SEQUENCE # TO 1                          
         L     R1,=AL4(XOWRITE+XOCONFIL+XIO2)                                   
         TM    SCPFLAG1,SF1ADD                                                  
         BZ    *+8                                                              
         L     R1,=AL4(XOADD+XOCONFIL+XIO2)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                 SEQUENCE #1 WRITTEN TO FILE                  
         DC    H'0'                                                             
*                                                                               
         LH    R1,RECSEQ           NEXT ONE TO BE DEALT WITH IS #2              
         LA    R1,1(R1)                                                         
         STH   R1,RECSEQ                                                        
*                                                                               
RPUT08   NI    SCPFLAG1,FF-SF1ADD  RESET ADD FLAG                               
*                                                                               
         L     R2,AIO2                                                          
         MVC   K7.CT7KEY,CT7KEY    MOVE KEY INTO IOKEY                          
         MVC   K7.CT7SEQNO,RECSEQ  SET NEXT SEQUENCE #                          
         L     R1,=A(XORDD+XOLOCK+XOCONFIL+XIO2)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BE    RPUT10              FOUND RECORD                                 
*                                                                               
         TM    IOERR,IOERNF        RECORD FOUND?                                
         BZ    *+12                NO - MUST BE ADDED                           
         OI    SCPFLAG1,SF1ADD                                                  
         B     RPUT10                                                           
*                                                                               
         TM    IOERR,IOEDEL        RECORD DELETED?                              
         BO    RPUT10              YES - THAT'S OK                              
         DC    H'0'                                                             
*                                                                               
RPUT10   L     R2,AIO2             SET UP AN EMPTY RECORD                       
         MVC   CT7KEY,K7.CT7KEY                                                 
         XC    CT7STAT,CT7STAT     RESET RECORD STATUS                          
         MVC   CT7LEN,=AL2(CT7KEYL+1)                                           
         MVI   CT7DATA,0                                                        
*                                                                               
         XC    SEQUENCE,SEQUENCE   RESET SEQUENCE NUMBER                        
         B     XITOK               RETURN                                       
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS AND CONSTANTS                                              *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
LBLLEN   EQU   4                   LENGTH OF A TABLE ENTRY                      
LBLALLOW EQU   64                  MAX NUMBER OF LABELS                         
SUBCHAR  EQU   C'%'                SCRAMBLE SUBSTITUTION CHARACTER              
*                                                                               
CTFBIG   DC    C'CTFBIG'                                                        
ADDEND   DC    C'ADD=END'                                                       
MAXLEN   DC    H'2000'             MAX LENGTH OF CONTROL FILE RECORD            
*                                                                               
TRTAB    DC    XL16'40404040404040404040404040404040'  00-0F                    
         DC    XL16'40404040404040404040404040404040'  10-1F                    
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'6A4040404040404040404A4B4C4D4E4F'  40-4F 40-->6A            
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'40818283848586878889404040404040'  80-8F                    
         DC    XL16'40919293949596979899404040404040'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404040404040'  A0-AF                    
         DC    XL16'40404040404040404040404040404040'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404040404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-DF                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404040404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040'  F0-FF                    
*                                                                               
*                   12345678901234567890123456789012345678901234567890          
ERCNTMSG DC    CL30'***      Errors on compile ***'                             
RECUPMSG DC    CL34'***Record updated without error***'                         
ERTOOMNY DC    CL39'**ERROR** Too many errors in this line'                     
ERINVBCH DC    CL45'**ERROR** Branch to XX on line XXXXX unknown'               
*                                                                               
DATATAB  DS    0X              *** TABLE OF SCRIPT PARAMETERS***                
         DC    C'E',AL4(EBCDIC)    E - CHARACTER INPUT                          
DATATABL EQU   *-DATATAB                                                        
         DC    C'N',AL4(NUMERIC)   N - NUMERIC (0-9)                            
         DC    C'F',AL4(FIXED)     F - NUMERIC FIXED LENGTH OF 2                
         DC    C'P',AL4(GAPPED)    P - PAIN IN THE ARSE - BCSPACES IN           
         DC    C'X',AL4(HEXED)     X - HEX (NOT VALIDATED)                      
         DC    C'L',AL4(LABEL)     L - 2 CHARACTER LABEL VALIDATION             
         DC    AL1(EOT)                                                         
*                                                                               
* DDSCRAMTAB                                                                    
       ++INCLUDE DDSCRAMTAB                                                     
         SPACE 2                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
MYSAVED  DSECT                                                                  
CCOUNT   DS    H                   CURRENT BUILD COUNT                          
SCOUNT   DS    H                   CURRENT SAVE COUNT                           
COLNUM   DS    H                   START COLUMN NUMBER                          
SEQNUM   DS    H                   START SEQUENCE NUMBER                        
SEQLAST  DS    H                   PREVIOUS SEQUENCE NUMBER                     
NESTSHOW DS    CL1                 RESOLVE NESTED BOOKS IF N.Z.                 
NESTLAST DS    CL1                 LAST TIME BOOK NEST STATUS                   
NESTYES  EQU   FF                  SHOW NESTED BOOKS                            
NESTNO   EQU   0                   DO NOT SHOW NESTED BOOKS                     
NESTERR  EQU   C'E'                ERROR DURING VALIDATE                        
LSNTRY   DS    XL(SUBACTLQ)        SINGLE ENTRY ACTION                          
LMNTRY   DS    XL(SUBACTLQ)        MULTIPLE ENTRY ACTION                        
LMCOUNT  DS    XL(SUBACTLQ)        REPEAT COUNT FOR MULTIPLES                   
SCRFLAG1 DS    XL1                 HORIZONTAL SCROLL FLAG                       
SCRFLAG2 DS    XL1                 REDISPLAY FLAG                               
DOFLAG   DS    XL1                 FLAG SUB-ACTIONS                             
DOFROM   EQU   X'80'               MOVE FROM ACTION REQUESTED                   
DOTO     EQU   X'40'               MOVE TO ACTION REQUESTED                     
DODEL    EQU   X'20'               DELETE ACTION REQUESTED                      
DOREP    EQU   X'10'               REPLICATE ACTION REQUESTED                   
DOINS    EQU   X'08'               INSERT ACTION REQUESTED                      
DOTOCNT  DS    H                   COUNT OF 'DO FROM' ACTIONS                   
DOFRCNT  DS    H                   COUNT OF 'DO TO' ACTIONS                     
DODLCNT  DS    H                   COUNT OF 'DELETE' ACTIONS                    
DORPCNT  DS    H                   COUNT OF 'REPLICATE' ACTIONS                 
DOINCNT  DS    H                   COUNT OF 'INSERT' ACTIONS                    
*                                                                               
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
BOOKPARM DS    XL24                PARAMETER LIST RESERVED FOR GETBOOK          
*                                                                               
AFRSTFR  DS    A                   A(FIRST 'FROM' FIELD)                        
AFRSTTO  DS    A                   A(FIRST 'TO' FIELD)                          
AGETBOOK DS    A                                                                
ASCRMBLE DS    A                                                                
ASQUASH  DS    V                                                                
*                                                                               
SCPFLAG1 DS    XL1                                                              
SF1NAME  EQU   X'80'               SCRIPT HAS A NAME                            
SF1ERRS  EQU   X'40'               ERROR DURING COMPILE                         
SF1ADD   EQU   X'20'               NEED TO ADD SCRIPT RECORD                    
SF1FRST  EQU   X'10'               FIRST TIME FOR ACTION                        
SF1LAST  EQU   X'08'               LAST TIME FOR ACTION                         
SF1CMT   EQU   X'04'               INSIDE /*...*/ PAIR                          
SF1LCMT  EQU   X'02'               LAST PROCESSED WAS COMMENT CLOSE             
SF1DINK  EQU   X'01'               INSIDE "..." PAIR                            
*                                                                               
AR3      DS    A                   R3 USED IN CALLBACK FROM SUBROUTINE          
ARSPR    DS    A                   HANDY REGISTER SAVE AREA                     
ALINLN   DS    A                   HANDY REGISTER SAVE AREA                     
ASEQFLD  DS    A                   A(SEQUENCE NUMBER FIELD)                     
AINSFLD  DS    A                   A(CURSOR) FOR INSERTED FIELD                 
DISPTOT  DS    F                   CUMULATIVE SCRIPT LENGTH                     
*                                                                               
SVFRP    DS    XL(L'GSFRPEL)       SAVED PFKEY ELEMENT FOR SCRAMBLE             
SCRPNAME DS    XL(L'CTJKID)        SAVED SCRIPT NAME FOR SCRAMBLE               
*                                                                               
LINENUM  DS    H                   CURRENT LINE NUMBER                          
RECSEQ   DS    H                   RECORD SEQUENCE #                            
TOTERRS  DS    H                   TOTAL ERRORS ON COMPILE                      
SEQUENCE DS    X                   ELEMENT SEQUENCE #                           
ELNOWLQ  DS    X                   LENGTH OF THIS ELEMENT                       
LASM     DS    X                   LENGTH OF AN ASSEMBLED LINE                  
LDATA    DS    X                   LENGTH OF INPUT LINE                         
LENNOW   DS    X                   CURRENT LENGTH OF I/P PARAMETER              
THISERR  DS    X                   NUMBER OF THIS ERROR                         
NUMERRS  DS    X                   NUMBER OF ERRORS ON THIS LINE                
ASMERR   DS    X                   IF ERROR ON ASMLINE = FF                     
*                                                                               
REPCMP   DS    XL10                SCRIPT COMPARATOR                            
REPLEN   DS    X                   LENGTH TO COMPARE                            
*                                                                               
ASMLINE  DS    XL132               ASSEMBLED SCRIPT OUTPUT LINE                 
ERROROUT DS    9XL80               TABLE OF OUTPUT ERRORS                       
*                                                                               
DSLISTU  DS    0D                                                               
FU@COPY  DS    CL(SBLEN)                                                        
FU@MOVE  DS    CL(SBLEN)                                                        
FU@DEL   DS    CL(SBLEN)                                                        
FU@BFR   DS    CL(SBLEN)                                                        
FU@AFTER DS    CL(SBLEN)                                                        
FU@REPL  DS    CL(SBLEN)                                                        
FU@INSRT DS    CL(SBLEN)                                                        
FU@YES   DS    CL4                                                              
FU@NO    DS    CL4                                                              
FU@DESC  DS    CL15                                                             
FU@LINE  DS    CL5                                                              
FU@HALF  DS    CL8                                                              
FU@TITLE DS    CL10                                                             
FU@DSP   DS    CL8                                                              
*                                                                               
DSLISTL  DS    0D                                                               
FL@COPY  DS    CL(SBLEN)                                                        
FL@MOVE  DS    CL(SBLEN)                                                        
FL@DEL   DS    CL(SBLEN)                                                        
FL@BFR   DS    CL(SBLEN)                                                        
FL@AFTER DS    CL(SBLEN)                                                        
FL@REPL  DS    CL(SBLEN)                                                        
FL@INSRT DS    CL(SBLEN)                                                        
FL@YES   DS    CL4                                                              
FL@NO    DS    CL4                                                              
FL@DESC  DS    CL15                                                             
FL@LINE  DS    CL5                                                              
FL@HALF  DS    CL8                                                              
FL@TITLE DS    CL10                                                             
FL@DSP   DS    CL8                                                              
*                                                                               
SVASM    DS    XL(ASMTABLQ)        SAVED ASMTAB ENTRY                           
SVLST    DS    XL(L'TLST)          SAVED TSAR RECORD                            
LBLTAB   DS    XL256               LABEL TABLE  (LBLALLOW*LBLLEN)               
BCHTAB   DS    XL256               BRANCH TABLE (LBLALLOW*LBLLEN)               
EQUTABLE DS    1024C               TABLE OF EQUS BUILT FOR THIS SCRIPT          
*                                                                               
ASMTABD  DSECT                                                                  
MNEMONIC DS    CL6                 MNEMONIC CORRESPONDING TO OP-CODE            
MTCHLEN  DS    AL1                 LENGTH-1 FOR EXECUTED COMPARE                
OPCODE   DS    CL2                 2 CHARACTER OP-CODE                          
MXOPERS  DS    AL1                 MAX # OF OPERANDS FOR THIS OP-CODE           
MXLBL    EQU   X'80'               INSTRUCTION IS A LABEL                       
MXBRNCH  EQU   X'40'               INSTRUCTION IS A BRANCH                      
DATATYPE DS    CL1                 WHICH CLASS OF DATA TO VALIDATE              
DATALEN  DS    AL1                 MAX LENGTH OF DATA TO FOLLOW                 
ASMTABLQ EQU   *-ASMTABD                                                        
*                                                                               
*** CTFILWORK ***                                                               
*        PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKSNUM  DS    XL2                 SEQUENCE NUMBER                              
         ORG   TLUSER                                                           
TLUSTAT  DS    XL1                 STATUS BYTE                                  
TLUSDEL  EQU   X'80'               DELETE THIS LINE                             
TLUSCPY  EQU   X'40'               COPY THIS LINE                               
TLUSMVE  EQU   X'20'               MOVE THIS LINE                               
TLUSAFT  EQU   X'10'               CPY/MVE AFTER THIS LINE                      
TLUSBEF  EQU   X'08'               CPY/MVE BEFORE THIS LINE                     
TLUSREP  EQU   X'04'               REPLICATE THIS LINE                          
TLUSINS  EQU   X'02'               INSERT A LINE                                
TLUTINS  EQU   X'01'               THIS LINE INSERTED                           
TLLNUM   DS    XL4                 LINE NUMBER                                  
TLLINE   DS    XL80                DATA LINE                                    
TLLNQ    EQU   *-TLSTD                                                          
*                                                                               
REPD     DSECT                                                                  
         ORG   REPPW1                                                           
PLINE    DS    0XL(L'REPPW1)       165 CHAR LINE FOR OUTPUT ROUTINE             
DATA1    DS    XL72                ORIGINAL I/P BEFORE SQUASHING                
DATANUM  DS    XL5                 LINE NUMBER                                  
         DS    XL2                                                              
ASMDISP  DS    XL5                 CUMULATIVE DISPLACEMENT                      
         DS    XL2                                                              
ASM1     DS    XL79                DUP OF ASMLINE FOR PRINT ROUTINE             
         ORG   REPPW2                                                           
ASMERRS  DS    0X                  ERROR LINES COME OUT HERE                    
*                                                                               
*** CTGENFILE ***                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
**** FAFACTS ***                                                                
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*** CTMSGEQUS ***                                                               
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
*** FAUTL ***                                                                   
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
*** DDFLDHDR ***                                                                
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036CTFIL20X  08/22/00'                                      
         END                                                                    
