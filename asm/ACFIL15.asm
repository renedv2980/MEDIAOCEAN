*          DATA SET ACFIL15    AT LEVEL 061 AS OF 02/06/03                      
*PHASE T62315A,*                                                                
         TITLE 'NEW FILE CREDITOR RECORD'                                       
         SPACE 2                                                                
* YNGX 059 16AUG99 USE LABLES TO REPRESENT FIELD NUMBERS                        
***********************************************************************         
* DCUR - THIS BOOK WAS SUPPOSED TO BE THE US VENDOR RECORD.  NOT SURE           
*        IF WE WILL NEED THIS BOOK OR NOT.                                      
***********************************************************************         
         SPACE 2                                                                
FIL15    START                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL15**,R6,RR=RE                                              
         USING TWAD,RA                                                          
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
INIT     GOTOX VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTOX (RF),(R1),C'LL  ',,DSLISTL                                       
         B     EXIT                                                             
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
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE TABLE                                
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
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
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
         USING ACTRECD,R2                                                       
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
* FIRST TIME FOR DISPLAY OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KFKDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   MVC   ACTKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KFKFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   ACTKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4                                                      
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** LAST TIME FOR KEY OBJECT ***                  
*                                 ------------------------                      
KLTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KLKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(KFDIS),AL1(0,0,0),AL4(KLKFDIS)    DISPLAY FILTER             
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KLKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A KEY OBJECT                               *         
***********************************************************************         
         SPACE 1                                                                
KLKDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KLKVAL   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A KEY FILTER                               *         
***********************************************************************         
         SPACE 1                                                                
KLKFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  DS    0H                                                               
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
         USING ACTRECD,R2                                                       
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
X        USING ACTRECD,IOKEY                                                    
RFADD    MVC   X.ACTKEY,ACTKEY                                                  
         GOTOX AGETLDG                                                          
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4                                                       
         MVI   ADDED,0                                                          
         CLI   LDGTLVA,L'ACTKACT   SINGLE LEVEL LEDGER?                         
         BE    EXITOK              YES - OK                                     
         XR    RF,RF                                                            
         XR    RE,RE                                                            
*                                                                               
         CLI   LDGTLVB,L'ACTKACT   2 LEVEL ACCOUNT                              
         BNE   RFADD02             ---------------                              
         CLC   ARTIST,ACTKUNT      PENULTIMATE LEVEL FOR ST                     
         BE    EXITOK                                                           
         IC    RF,LDGTLVA          GET LENGTH OF PRIOR LEVEL                    
         IC    RE,L'ACTKACT                                                     
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   X.ACTKEY,ACTKEY                                                  
         LA    R1,X.ACTKACT(RF)    STUFF AFTER FIRST LEVEL ACCOUNT              
         EX    RE,*+4              CLEAR TO SPACES                              
         MVC   0(0,R1),BCSPACES                                                 
         MVC   TEMPKEY,X.ACTKEY    SAVE THIS FIRST LEVEL ACCOUNT                
         GOTOX AIO,IORD+IOACCDIR+IO1                                            
         BE    RFADD12             FIRST LEVEL ACCOUNT EXISTS                   
*                                                                               
         MVC   X.ACTKEY,TEMPKEY    GET BACK SAVED FIRST LEVEL ACCOUNT           
         BAS   RE,ADDACC           ADD THIS ACCOUNT                             
         MVI   ADDED,C'Y'                                                       
         B     RFADD12                                                          
*                                                                               
RFADD02  CLI   LDGTLVC,L'ACTKACT   3 LEVEL ACCOUNT                              
         BNE   RFADD06             ---------------                              
         IC    RF,LDGTLVA          GET LENGTH OF FIRST LEVEL                    
         IC    RE,L'ACTKACT                                                     
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   X.ACTKEY,ACTKEY                                                  
         LA    R1,X.ACTKACT(RF)    STUFF AFTER FIRST LEVEL ACCOUNT              
         EX    RE,*+4              CLEAR TO SPACES                              
         MVC   0(0,R1),BCSPACES                                                 
         MVC   TEMPKEY,X.ACTKEY    SAVE THIS FIRST LEVEL ACCOUNT                
         GOTOX AIO,IORD+IOACCDIR+IO1                                            
         BE    RFADD04             FIRST LEVEL ACCOUNT EXISTS                   
*                                                                               
         MVC   X.ACTKEY,TEMPKEY    GET BACK SAVED FIRST LEVEL ACCOUNT           
         BAS   RE,ADDACC           ADD THIS ACCOUNT                             
         MVI   ADDED,C'Y'                                                       
*                                                                               
RFADD04  CLC   ARTIST,ACTKUNT      NEED PENULTIMATE LEVEL FOR ST                
         BE    RFADD12                                                          
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         IC    RF,LDGTLVA          GET COMBINED LENGTHS OF PRIOR LEVELS         
         IC    RE,LDGTLVB                                                       
         LA    RF,0(RE,RF)                                                      
         IC    RE,L'ACTKACT                                                     
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   X.ACTKEY,ACTKEY                                                  
         LA    R1,X.ACTKACT(RF)    STUFF AFTER SECOND LEVEL ACCOUNT             
         EX    RE,*+4              CLEAR TO SPACES                              
         MVC   0(0,R1),BCSPACES                                                 
         MVC   TEMPKEY,X.ACTKEY    SAVE THIS SECOND LEVEL ACCOUNT               
         GOTOX AIO,IORD+IOACCDIR+IO1                                            
         BE    RFADD12             SECOND LEVEL ACCOUNT EXISTS                  
*                                                                               
         MVC   X.ACTKEY,TEMPKEY    GET BACK SAVED SECOND LEVEL ACCOUNT          
         BAS   RE,ADDACC           ADD THIS ACCOUNT                             
         MVI   ADDED,C'Y'                                                       
         B     RFADD12                                                          
*                                                                               
RFADD06  CLI   LDGTLVD,L'ACTKACT   4 LEVEL ACCOUNT                              
         BE    *+6                 ---------------                              
         DC    H'0'                ACCOUNT LEVELS ALL WRONG                     
         IC    RF,LDGTLVA          GET LENGTH OF FIRST LEVEL                    
         IC    RE,L'ACTKACT                                                     
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   X.ACTKEY,ACTKEY                                                  
         LA    R1,X.ACTKACT(RF)    STUFF AFTER FIRST LEVEL ACCOUNT              
         EX    RE,*+4              CLEAR TO SPACES                              
         MVC   0(0,R1),BCSPACES                                                 
         MVC   TEMPKEY,X.ACTKEY    SAVE THIS FIRST LEVEL ACCOUNT                
         GOTOX AIO,IORD+IOACCDIR+IO1                                            
         BE    RFADD08             FIRST LEVEL ACCOUNT EXISTS                   
*                                                                               
         MVC   X.ACTKEY,TEMPKEY    GET BACK SAVED FIRST LEVEL ACCOUNT           
         BAS   RE,ADDACC           ADD THIS ACCOUNT                             
         MVI   ADDED,C'Y'                                                       
*                                                                               
RFADD08  XR    RF,RF                                                            
         XR    RE,RE                                                            
         IC    RF,LDGTLVA          GET COMBINED LENGTHS OF PRIOR LEVELS         
         IC    RE,LDGTLVB                                                       
         LA    RF,0(RE,RF)                                                      
         IC    RE,L'ACTKACT                                                     
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   X.ACTKEY,ACTKEY                                                  
         LA    R1,X.ACTKACT(RF)    STUFF AFTER SECOND LEVEL ACCOUNT             
         EX    RE,*+4              CLEAR TO SPACES                              
         MVC   0(0,R1),BCSPACES                                                 
         MVC   TEMPKEY,X.ACTKEY    SAVE THIS SECOND LEVEL ACCOUNT               
         GOTOX AIO,IORD+IOACCDIR+IO1                                            
         BE    RFADD10             SECOND LEVEL ACCOUNT EXISTS                  
*                                                                               
         MVC   X.ACTKEY,TEMPKEY    GET BACK SAVED SECOND LEVEL ACCOUNT          
         BAS   RE,ADDACC           ADD THIS ACCOUNT                             
         MVI   ADDED,C'Y'                                                       
*                                                                               
RFADD10  CLC   ARTIST,ACTKUNT      NEED PENULTIMATE LEVEL FOR ST                
         BE    RFADD12                                                          
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         IC    RF,LDGTLVA          GET COMBINED LENGTHS OF PRIOR LEVELS         
         IC    RE,LDGTLVB                                                       
         LA    RF,0(RE,RF)                                                      
         IC    RE,LDGTLVC                                                       
         LA    RF,0(RE,RF)                                                      
         IC    RE,L'ACTKACT                                                     
         SR    RE,RF                                                            
         BCTR  RE,0                                                             
         MVC   X.ACTKEY,ACTKEY                                                  
         LA    R1,X.ACTKACT(RF)    STUFF AFTER THIRD LEVEL ACCOUNT              
         EX    RE,*+4              CLEAR TO SPACES                              
         MVC   0(0,R1),BCSPACES                                                 
         MVC   TEMPKEY,X.ACTKEY    SAVE THIS THIRD LEVEL ACCOUNT                
         GOTOX AIO,IORD+IOACCDIR+IO1                                            
         BE    RFADD12             THIRD LEVEL ACCOUNT EXISTS                   
*                                                                               
         MVC   X.ACTKEY,TEMPKEY    GET BACK SAVED THIRD LEVEL ACCOUNT           
         BAS   RE,ADDACC           ADD THIS ACCOUNT                             
         MVI   ADDED,C'Y'                                                       
         B     RFADD12                                                          
*                                                                               
RFADD12  CLI   ADDED,0                                                          
         BE    EXITOK                                                           
         MVI   FVOMTYP,GTMINF                                                   
         MVC   FVMSGNO,=AL2(AI$HLADD)                                           
         B     EXITL                                                            
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING ABLELD,BOELEM                                                    
RFDEL    GOTOX AGETEL,BOPARM,('ABLELQ',ACTRECD),0                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NLOWA)                                           
         B     EXITL                                                            
*                                                                               
         GOTOX VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                NO RSTEL - SOMETHING V. WRONG                
*                                                                               
         L     RF,12(R1)                                                        
         USING RSTELD,RF                                                        
         OC    RSTBDATE,RSTBDATE                                                
         BZ    RFDEL02                                                          
         CLC   RSTTDATE,RSTBDATE                                                
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACNVD)                                           
         B     EXITL                                                            
         DROP  RF                                                               
*                                                                               
RFDEL02  CP    ABLFRWD,=P'0'       ALL BALANCES MUST BE ZERO TO DELETE          
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACNVD)                                           
         B     EXITL                                                            
*                                                                               
         CP    ABLDR,=P'0'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACNVD)                                           
         B     EXITL                                                            
*                                                                               
         CP    ABLCR,=P'0'                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ACNVD)                                           
         B     EXITL                                                            
*                                                                               
         USING ASTELD,BOELEM                                                    
         GOTOX AGETEL,BOPARM,('ASTELQ',ACTRECD),0                               
         BNE   EXITOK              NO ASTEL ON ACCOUNT                          
*                                                                               
         OC    ASTDRAFT,ASTDRAFT   DRAFT TRANSACTIONS NOT ALLOWED               
         BZ    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$DRFD)                                            
         B     EXITL                                                            
         POP   USING                                                            
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
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
RECLAST  L     R1,SVPARMS4                                                      
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RLTABL   DC    AL1(RDIS),AL1(0,0,0),AL4(RLDIS)                                  
         DC    AL1(RVAL),AL1(0,0,0),AL4(RLVAL)                                  
         DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
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
         SPACE 2                                                                
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
         USING ACTRECD,R2                                                       
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
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(F#CROD#UNTLD),AL4(LGRDTA) UNIT/LEDGER                        
         DC    AL2(F#CROD#ACC),AL4(ACCDTA)   ACCOUNT                            
         DC    AL2(F#CROD#CURRC),AL4(CCDDTA) CURRENCY CODE                      
         DC    AL2(F#CROD#CURCN),AL4(CCNDTA) CURRENCY CODE NAME                 
         DC    AL2(F#CROD#BNKSC),AL4(BSODTA) BANK SORT CODE                     
         DC    AL2(F#CROD#BNKAC),AL4(BACDTA) BANK ACCOUNT CODE                  
         DC    AL2(F#CROD#BNKNM),AL4(BANDTA) BANK ACCOUNT NAME                  
         DC    AL2(F#CROD#BNKNA),AL4(BBNDTA) BANK NAME                          
         DC    AL2(F#CROD#PAYME),AL4(PAYDTA) PAYMENT METHOD                     
         DC    AL2(F#CROD#SUNCR),AL4(SCRDTA) SUNDRY CREDITOR                    
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL15    CSECT                                                                  
         SPACE 2                                                                
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
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDFDIS)    DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFDFVAL)    VALIDATE FILTER            
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
DFDVAL   GOTOX ADELEL,BOPARM,('DTSELQ',ACTRECD),0                               
         XC    BSORT,BSORT                                                      
         XC    BCOUNT,BCOUNT                                                    
         XC    BSTAT,BSTAT                                                      
         XC    BBNAME,BBNAME                                                    
         XC    BACNAM,BACNAM                                                    
*                                                                               
         PUSH  USING                                                            
         USING BACELD,BOELEM                                                    
         GOTOX AGETEL,BOPARM,('BACELQ',ACTRECD),0                               
         BNE   DFDV01A                                                          
*                                                                               
         MVC   BSTAT,BACSTAT                                                    
         MVC   BSORT,BACSORT                                                    
         MVC   BCOUNT,BACCOUNT                                                  
*                                                                               
         CLI   BACLN,BACLNQ                                                     
         BNH   DFDV01                                                           
         MVC   BBNAME,BACBNAME                                                  
*                                                                               
         CLI   BACLN,BACLNQ2                                                    
         BNH   DFDV01                                                           
         XR    RF,RF                                                            
         IC    RF,BACLN                                                         
         SH    RF,=Y(BACLNQ2+1)                                                 
         EX    RF,*+4                                                           
         MVC   BACNAM(0),BACACNAM                                               
*                                                                               
DFDV01   GOTOX ADELEL,BOPARM,('BACELQ',ACTRECD),0                               
         POP   USING                                                            
*                                                                               
DFDV01A  GOTOX VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',ACTRECD)                 
         CLI   12(R1),6            RSTEL ON RECORD?                             
         BNE   DFDV02                                                           
         GOTOX AADDRST,ACTRECD     ADD RSTEL TO RECORD                          
         BNE   EXITL                                                            
*                                                                               
DFDV02   GOTOX VHELLO,BOPARM,(C'G',GCFILNAM),('ASTELQ',ACTRECD),0               
         CLI   12(R1),6            ASTEL ON RECORD?                             
         BNE   DFDV04                                                           
         GOTOX AADDAST,ACTRECD     ADD ASTEL TO RECORD                          
         BNE   EXITL                                                            
*                                                                               
DFDV04   GOTOX VHELLO,BOPARM,(C'G',GCFILNAM),('APOELQ',ACTRECD),0               
         CLI   12(R1),6            APOEL/ABLEL ON RECORD                        
         BNE   DFDV06                                                           
         GOTOX AADDBAL,ACTRECD     ADD APOEL & ABLEL TO RECORD                  
         BNE   EXITL                                                            
*                                                                               
DFDV06   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFDFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA FILTER                            *         
***********************************************************************         
         SPACE 1                                                                
DFDFVAL  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
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
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
DLDVAL   XC    BOELEM,BOELEM       BUILD BANK ACCOUNT ELEMENT                   
         USING BACELD,BOELEM                                                    
         MVI   BACEL,BACELQ                                                     
         MVI   BACLN,BACLNQ                                                     
         MVC   BACSTAT,BSTAT                                                    
         MVC   BACSORT,BSORT                                                    
         MVC   BACCOUNT,BCOUNT                                                  
         MVC   BACBNAME,BBNAME                                                  
         MVC   BACACNAM,BACNAM                                                  
         OC    BACSTAT,BACSTAT     CHECK FOR NULL ELEMENT                       
         BNZ   VRRL02                                                           
         OC    BACSORT,BACSORT                                                  
         BNZ   VRRL02                                                           
         OC    BACCOUNT,BACCOUNT                                                
         BNZ   VRRL02                                                           
         OC    BACBNAME,BACBNAME                                                
         BNZ   VRRL02                                                           
         OC    BACACNAM,BACACNAM                                                
         BNZ   VRRL02                                                           
         B     EXITOK              THIS IS A NULL ELEMENT - DO NOT ADD          
*                                                                               
VRRL02   MVC   BCBYTE1,BACSTAT                                                  
         NI    BCBYTE1,X'F0'                                                    
         CLI   BCBYTE1,BACSTBAC    TEST IF BACS PAYMENT METHOD                  
         BNE   VRRL04              NO                                           
         MVC   FVADDR,ABSORT                                                    
         OC    BACSORT,BACSORT     IF SO ALL FIELDS MUST BE FILLED IN           
         BZ    EXITNO                                                           
         MVC   FVADDR,ABCOUNT                                                   
         OC    BACCOUNT,BACCOUNT                                                
         BZ    EXITNO                                                           
         MVC   FVADDR,ABBNAME                                                   
         OC    BACBNAME,BACBNAME                                                
         BZ    EXITNO                                                           
         MVC   FVADDR,ABACNAM                                                   
         OC    BACACNAM,BACACNAM                                                
         BZ    EXITNO                                                           
*                                                                               
VRRL04   XC    FVADDR,FVADDR                                                    
         OC    BACBNAME,BACBNAME   BANK NAME FIELD FILLED IN?                   
         BZ    *+8                                                              
         MVI   BACLN,BACLNQ2       ADJUST LENGTH                                
*                                                                               
         OC    BACACNAM,BACACNAM                                                
         BZ    VRRL06                                                           
         LA    RF,BACACNAM+L'BACACNAM-1                                         
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   RF,*-10                                                          
         LA    RE,BACACNAM                                                      
         SR    RF,RE                                                            
         LA    RF,BACLNQ2+2(RF)                                                 
         STC   RF,BACLN                                                         
         OC    BACBNAME,BACBNAME   WAS THIS SET?                                
         BNZ   *+10                                                             
         MVC   BACBNAME,BCSPACES   MAKE IT SPACES                               
*                                                                               
VRRL06   GOTOX AADDEL,BOPARM,ACTRECD                                            
         B     EXITOK                                                           
         POP   USING                                                            
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
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LEDGER                                              *         
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
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLGR)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTLGR)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTLGR)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISLGR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISLGR   MVC   FVIFLD(L'ACTKUNT+L'ACTKLDG),ACTKUNT                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LEDGER FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALLGR   MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),FVIFLD                              
         CLC   =C'SF',FVIFLD                                                    
         BE    VFLGR02                                                          
         CLC   =C'SX',FVIFLD                                                    
         BE    VFLGR02                                                          
         CLC   =C'ST',FVIFLD                                                    
         BE    VFLGR02                                                          
         CLC   =C'SV',FVIFLD                                                    
         BE    VFLGR02                                                          
         B     EXITNV                                                           
*                                                                               
TEMP     USING ACTRECD,IOKEY                                                    
VLGR02   MVC   IOKEY,BCSPACES       READ THE LEDGER RECORD FOR THIS ID          
         MVC   TEMP.ACTKCPY,ACTKCPY COMPANY ID IN HERE                          
         MVC   TEMP.ACTKUNT,ACTKUNT UNIT HERE                                   
         MVC   TEMP.ACTKLDG,ACTKLDG LEDGER                                      
         DROP  TEMP                                                             
         GOTOX AIO,IOREAD+IOACCDIR+IO1                                          
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     EXITL               CAN'T READ ACCDIR FOR LEDGER RECORD          
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTLGR  MVC   FVIFLD(L'ACTKUNT+L'ACTKLDG),FLTIFLD                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LEDGER FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTLGR  MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),FVIFLD                              
         MVC   FLTIFLD(L'ACTKUNT+L'ACTKLDG),FVIFLD                              
         CLC   =C'SF',FVIFLD                                                    
         BE    VFLGR02                                                          
         CLC   =C'SX',FVIFLD                                                    
         BE    VFLGR02                                                          
         CLC   =C'ST',FVIFLD                                                    
         BE    VFLGR02                                                          
         CLC   =C'SV',FVIFLD                                                    
         BE    VFLGR02                                                          
         B     EXITNV                                                           
*                                                                               
TEMP     USING ACTRECD,IOKEY                                                    
VFLGR02  MVC   IOKEY,BCSPACES       READ THE LEDGER RECORD FOR THIS ID          
         MVC   TEMP.ACTKCPY,ACTKCPY COMPANY ID IN HERE                          
         MVC   TEMP.ACTKUNT,ACTKUNT UNIT HERE                                   
         MVC   TEMP.ACTKLDG,ACTKLDG LEDGER                                      
         DROP  TEMP                                                             
         GOTOX AIO,IOREAD+IOACCDIR+IO1                                          
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         B     EXITL               CAN'T READ ACCDIR FOR LEDGER RECORD          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LEDGER                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTLGR  CLC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),BCSPACES                            
         BNH   FLTXX               NOT ACCOUNT - RSPACE 2 IT                    
*                                                                               
         CLC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),FLTIFLD                             
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR ACCOUNT                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ACCDTA   LA    RF,ACCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ACCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISACC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALACC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTACC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTACC)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTACC)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISACC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHACC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN ACCOUNT FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISACC   MVC   FVIFLD(L'ACTKACT),ACTKACT                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO SEARCH CALL FOR ACCOUNT FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
SRCHACC  GOTOX VACSRCHC,BOPARM,(3,SVPARMS5),ATWA,ACTKUNT,              *        
               ACOM,0                                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ACCOUNT FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALACC   MVC   ACTKACT,FVIFLD                                                   
         MVC   IOKEY(L'ACTKEY),ACTKEY                                           
         GOTOX AGETLDG                                                          
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4                                                       
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         CLI   LDGTLVA,L'ACTKACT   SINGLE LEVEL LEDGER?                         
         BE    EXITOK              YES - OK                                     
*                                                                               
         CLI   LDGTLVB,L'ACTKACT   2 LEVEL ACCOUNT                              
         BNE   VACC02                                                           
         CLC   ARTIST,ACTKUNT      PENULTIMATE LEVEL FOR ST                     
         BE    EXITOK                                                           
         IC    RF,LDGTLVA          GET LENGTH OF PRIOR LEVEL                    
         B     VACC06                                                           
*                                                                               
VACC02   CLI   LDGTLVC,L'ACTKACT   3 LEVEL ACCOUNT                              
         BNE   VACC04                                                           
         IC    RF,LDGTLVA          GET COMBINED LENGTHS OF PRIOR LEVELS         
         CLC   ARTIST,ACTKUNT      PENULTIMATE LEVEL FOR ST                     
         BE    *+8                                                              
         IC    RE,LDGTLVB                                                       
         LA    RF,0(RE,RF)                                                      
         B     VACC06                                                           
*                                                                               
VACC04   CLI   LDGTLVD,L'ACTKACT   4 LEVEL ACCOUNT                              
         BE    *+6                                                              
         DC    H'0'                ACCOUNT LEVELS ALL WRONG                     
         IC    RF,LDGTLVA          GET COMBINED LENGTHS OF PRIOR LEVELS         
         IC    RE,LDGTLVB                                                       
         LA    RF,0(RE,RF)                                                      
         CLC   ARTIST,ACTKUNT      PENULTIMATE LEVEL FOR ST                     
         BE    *+12                                                             
         IC    RE,LDGTLVC                                                       
         LA    RF,0(RE,RF)                                                      
*                                                                               
VACC06   CLM   RF,1,FVILEN                                                      
         BL    EXITOK                                                           
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     EXITL                                                            
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN ACCOUNT FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTACC  MVC   FVIFLD(L'ACTKACT),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ACCOUNT FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTACC  MVC   ACTKACT,FVIFLD                                                   
         MVC   FLTIFLD(L'ACTKACT),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON ACCOUNT FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTACC  CLC   ACTKACT,BCSPACES    IS THERE AN ACCOUNT TO COMPARE ON?           
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   ACTKACT,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR CURRENCY CODE                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CCDDTA   LA    RF,CCDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CCDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCCD)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCCD)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCCD)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCCD)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCCD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CURRENCY CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING ASTELD,BOELEM                                                    
DISCCD   GOTOX AGETEL,BOPARM,('ASTELQ',ACTRECD),0                               
         BNE   EXITOK              NO ASTEL                                     
         CLI   ASTCUR,ASTCANY      TEST THIS TRUE                               
         BNE   *+12                                                             
         MVI   FVIFLD,C'*'                                                      
         B     EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'ASTCUR),ASTCUR                                          
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CURRENCY CODE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VALCCD   GOTOX VHELLO,BOPARM,(C'G',GCFILNAM),('ASTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                SOMETHING HAPPENED TO ASTEL                  
         L     R4,12(R1)                                                        
         USING ASTELD,R4                                                        
*                                                                               
         CLI   FVILEN,0            IF NO INPUT USE DEFAULT CURRENCY             
         BNE   VCCD02                                                           
         MVI   FVIFLD,C'*'         DEFAULT = ALL                                
         CLC   ASTCUR,EFFS         SET TO ALL CURRENCIES PREVIOUSLY?            
         BE    VCCD02                                                           
         MVC   FVIFLD(L'CSCPYCUR),CSCPYCUR                                      
         B     VCCD06              DEFAULT = COMPANY CURRENCY CODE              
*                                                                               
VCCD02   CLC   CSCPYCUR,FVIFLD     COMPANY CURRENCY SET?                        
         BE    VCCD06                                                           
         CLI   FVIFLD,C'*'         ALL CURRENCIES SET?                          
         BE    VCCD06                                                           
*                                                                               
         CLI   ACTKLDG,C'F'        CHECK ALLOWED FOREIGN CURRENCY               
         BNE   VCCD04                                                           
         TM    BCCPYST6,CPYSFMCR                                                
         BNZ   VCCD06                                                           
         MVC   FVMSGNO,=AL2(AE$SECLK) SECURITY LOCKOUT                          
         B     EXITL                                                            
*                                                                               
VCCD04   TM    BCCPYST6,CPYSFOCR                                                
         BNZ   VCCD06                                                           
         MVC   FVMSGNO,=AL2(AE$SECLK) SECURITY LOCKOUT                          
         B     EXITL                                                            
*                                                                               
VCCD06   CLC   ASTCUR,EFFS         SET TO ALL CURRENCIES?                       
         BNE   VCCD08                                                           
         CLI   FVIFLD,C'*'                                                      
         BNE   EXITNV              CAN`T BE ALTERED                             
         B     VCCD14                                                           
*                                                                               
VCCD08   OC    ASTCUR,ASTCUR       NOT SET YET                                  
         BZ    VCCD10                                                           
         CLC   ASTCUR,BCCPYCUR     SAME AS COMPANY CURRENCY                     
         BE    VCCD10                                                           
         CLC   ASTCUR,FVIFLD       NOT CHANGED                                  
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CCCWS) CAN`T CHANGE CURRENCY CODE                
         B     EXITL                                                            
*                                                                               
VCCD10   CLI   FVIFLD,C'*'         SET TO ALL CURRENCIES?                       
         BNE   VCCD12              NO                                           
         MVC   ASTCUR,EFFS         ALL CURRENCIES                               
         B     VCCD14                                                           
*                                                                               
VCCD12   GOTOX VBLDCUR,BOPARM,FVIFLD,(X'40',CURTENT),ACOM                       
         CLI   0(R1),0             CODE IS VALID?                               
         BNE   EXITNV              NO                                           
         MVC   ASTCUR,FVIFLD       MOVE IN CURRENCY CODE                        
*                                                                               
VCCD14   B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CURRENCY CODE FILTER FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
DFLTCCD  MVC   FVIFLD(L'ASTCUR),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CURRENCY CODE FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VFLTCCD  CLI   FVIFLD,C'*'         SET TO ALL CURRENCIES?                       
         BNE   *+14                NO                                           
         MVC   FLTIFLD(L'ASTCUR),EFFS                                           
         B     EXITOK                                                           
*                                                                               
         GOTOX VBLDCUR,BOPARM,FVIFLD,(X'40',CURTENT),ACOM                       
         CLI   0(R1),0             CODE IS VALID?                               
         BNE   EXITNV              NO                                           
         MVC   FLTIFLD(L'ASTCUR),FVIFLD SAVE CURRENCY CODE                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON CURRENCY CODE                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING ASTELD,BOELEM                                                    
DOFTCCD  GOTOX AGETEL,BOPARM,('ASTELQ',ACTRECD),0                               
         BNE   FLTXX               NO ASTEL - NOT WANTED                        
*                                                                               
         CLC   ASTCUR,FLTIFLD      COMPARE CURRENCY CODES                       
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A CURRENCY CODE NAME                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CCNDTA   LA    RF,CCNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CCNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CURRENCY NAME                                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING ASTELD,BOELEM                                                    
DISCCN   GOTOX AGETEL,BOPARM,('ASTELQ',ACTRECD),0                               
         BNE   EXITOK              NO ASTEL                                     
         CLI   ASTCUR,ASTCANY      TEST THIS TRUE                               
         BNE   *+14                                                             
         MVC   FVIFLD(30),LC@ALLC  ALL CURRENCIES                               
         B     EXITOK                                                           
*                                                                               
         XC    CURTENT,CURTENT                                                  
         OC    ASTCUR,ASTCUR       CURRENCY CODE SET?                           
         BZ    EXITOK              NO                                           
*                                                                               
         GOTOX VBLDCUR,BOPARM,ASTCUR,(X'40',CURTENT),ACOM                       
         CLI   0(R1),0                                                          
         BNE   EXITOK              INVALID CURRENCY TABLE ENTRY                 
*                                                                               
         LA    RF,CURTENT                                                       
         USING CURTABD,RF                                                       
         TM    CURTPIND,CURTLNAM   ANY LONG NAME?                               
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'CURTLONG),CURTLONG                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BANK SORT CODE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BSODTA   LA    RF,BSOTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
BSOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBSO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBSO)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BANK SORT CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BACELD,BOELEM                                                    
DISBSO   GOTOX AGETEL,BOPARM,('BACELQ',ACTRECD),0                               
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'BACSORT),BACSORT                                        
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BANK SORT CODE                                             *         
***********************************************************************         
         SPACE 1                                                                
VALBSO   MVC   ABSORT,FVADDR       A(SORT CODE FIELD)                           
         XC    BSORT,BSORT                                                      
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         LA    RF,FVIFLD           VALIDATE XX-XX-XX                            
         CLI   2(RF),C'-'                                                       
         BE    *+12                                                             
         MVI   FVERRNDX,2          SET CURSOR                                   
         B     EXITNV                                                           
*                                                                               
         CLI   5(RF),C'-'                                                       
         BE    *+12                                                             
         MVI   FVERRNDX,5          SET CURSOR POSITION                          
         B     EXITNV                                                           
*                                                                               
         LA    R0,3                3 GROUPS OF 2                                
         XR    RE,RE               DISP INTO FIELD                              
VBSO02   LA    R1,2                A GROUP OF 2                                 
         SPACE 1                                                                
VBSO04   CLI   0(RF),C'9'          CHECK IF NUMERIC                             
         BNH   *+12                                                             
         STC   RE,FVERRNDX         DISPLACEMENT INTO FIELD                      
         B     EXITNOTN                                                         
         CLI   0(RF),C'0'          CHECK IF NUMERIC                             
         BNL   *+12                                                             
         STC   RE,FVERRNDX         DISPLACEMENT INTO FIELD                      
         B     EXITNOTN                                                         
*                                                                               
         LA    RF,1(RF)            NEXT NUMBER IN GROUP OF 2                    
         LA    RE,1(RE)                                                         
         BCT   R1,VBSO04           CHECK IT                                     
*                                                                               
         LA    RF,1(RF)            GO PAST THE `-`                              
         LA    RE,1(RE)                                                         
         BCT   R0,VBSO02           NEXT GROUP                                   
*                                                                               
         MVC   BSORT,FVIFLD        ALL OK                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BANK ACCOUNT CODE                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BACDTA   LA    RF,BACTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
BACTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBAC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBAC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BANK ACCOUNT CODE                                           *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BACELD,BOELEM                                                    
DISBAC   GOTOX AGETEL,BOPARM,('BACELQ',ACTRECD),0                               
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'BACCOUNT),BACCOUNT                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BANK ACCOUNT CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
VALBAC   MVC   ABCOUNT,FVADDR      A(SORT CODE FIELD)                           
         XC    BCOUNT,BCOUNT                                                    
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   BCOUNT,FVIFLD                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BANK ACCOUNT NAME                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BANDTA   LA    RF,BANTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
BANTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBAN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBAN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BANK ACCOUNT NAME                                           *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BACELD,BOELEM                                                    
DISBAN   GOTOX AGETEL,BOPARM,('BACELQ',ACTRECD),0                               
         BNE   EXITOK                                                           
         CLI   BACLN,BACLNQ2       LONG ENOUGH?                                 
         BNH   EXITOK              NO                                           
         XR    RE,RE                                                            
         IC    RE,BACLN                                                         
         SH    RE,=Y(BACLNQ2+1)                                                 
         EX    RE,*+4                                                           
         MVC   FVIFLD(0),BACACNAM                                               
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BANK ACCOUNT NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
VALBAN   MVC   ABACNAM,FVADDR      A(SORT CODE FIELD)                           
         XC    BACNAM,BACNAM                                                    
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   BACNAM,FVIFLD                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BANK NAME                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BBNDTA   LA    RF,BBNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
BBNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBBN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBBN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY BANK NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BACELD,BOELEM                                                    
DISBBN   GOTOX AGETEL,BOPARM,('BACELQ',ACTRECD),0                               
         BNE   EXITOK                                                           
         CLI   BACLN,BACLNQ        LONG ENOUGH?                                 
         BNH   EXITOK              NO                                           
         MVC   FVIFLD(L'BACBNAME),BACBNAME                                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE BANK NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALBBN   MVC   ABBNAME,FVADDR      A(BANK NAME FIELD)                           
         XC    BBNAME,BBNAME                                                    
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   BBNAME,FVIFLD                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PAYMENT METHOD                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PAYDTA   LA    RF,PAYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
PAYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPAY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPAY)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PAYMENT METHOD                                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING BACELD,BOELEM                                                    
DISPAY   GOTOX AGETEL,BOPARM,('BACELQ',ACTRECD),0                               
         BNE   EXITOK                                                           
         TM    BACSTAT,BACSTCHQ                                                 
         BNO   *+10                                                             
         MVC   FVIFLD(PYLN),LC@CHK CHEQUE                                       
*                                                                               
         TM    BACSTAT,BACSTTRF                                                 
         BNO   *+10                                                             
         MVC   FVIFLD(PYLN),LC@XFR TRANSFER                                     
*                                                                               
         TM    BACSTAT,BACSTPAY                                                 
         BNO   *+10                                                             
         MVC   FVIFLD(PYLN),LC@PYMNT PAYMENT                                    
*                                                                               
         TM    BACSTAT,BACSTBAC                                                 
         BNO   *+10                                                             
         MVC   FVIFLD(PYLN),LC@BACS  BACS                                       
*                                                                               
         TM    BACSTAT,BACSTMCH                                                 
         BNO   *+10                                                             
         MVC   FVIFLD(PYLN),LC@MANCH MANUAL CHEQUE                              
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PAYMENT METHOD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALPAY   GOTOX VHELLO,BOPARM,(C'G',GCFILNAM),('ASTELQ',ACTRECD),0               
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ELNOF) ELEMENT NOT ON FILE                       
         B     EXITL                                                            
*                                                                               
         L     RF,12(R1)                                                        
         USING ASTELD,RF                                                        
         XC    BSTAT,BSTAT      ??                                              
*        NI    BSTAT,X'0F'         TURN OFF LAST PAY METHOD                     
         CLC   CSCPYCUR,ASTCUR     CURRENCY = AGENCY CURRENCY?                  
         BNE   VPAY02              NO                                           
         CLI   FVILEN,0            PAY METHOD IS OPTIONAL IF USING              
         BE    EXITOK              AGENCY CURRENCY                              
         DROP  RF                                                               
*                                                                               
VPAY02   CLI   FVILEN,0            DEFAULT IS MANUAL CHEQUE                     
         BNE   VPAY04                                                           
         MVC   FVIFLD(PYLN),LC@MANCH                                            
         OI    BSTAT,BACSTMCH                                                   
         B     EXITOK                                                           
*                                                                               
VPAY04   XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BE    VPAY06                                                           
         CLC   FVIFLD(0),UC@CHK                                                 
         EX    RF,*+8                                                           
         BE    VPAY06                                                           
         CLC   FVIFLD(0),LC@CHK                                                 
         B     VPAY08                                                           
*                                                                               
VPAY06   OI    BSTAT,BACSTCHQ                                                   
         MVC   FVIFLD(PYLN),LC@CHK                                              
         B     EXITOK              CHEQUE                                       
*                                                                               
VPAY08   EX    RF,*+8                                                           
         BE    VPAY10                                                           
         CLC   FVIFLD(0),UC@PYMNT                                               
         EX    RF,*+8                                                           
         BE    VPAY10                                                           
         CLC   FVIFLD(0),LC@PYMNT                                               
         B     VPAY12                                                           
*                                                                               
VPAY10   OI    BSTAT,BACSTPAY                                                   
         MVC   FVIFLD(PYLN),LC@PYMNT                                            
         B     EXITOK              PAYMENT                                      
*                                                                               
VPAY12   EX    RF,*+8                                                           
         BE    VPAY14                                                           
         CLC   FVIFLD(0),UC@BACS                                                
         EX    RF,*+8                                                           
         BE    VPAY14                                                           
         CLC   FVIFLD(0),LC@BACS                                                
         B     VPAY16                                                           
*                                                                               
VPAY14   OI    BSTAT,BACSTBAC                                                   
         MVC   FVIFLD(PYLN),LC@BACS                                             
         B     EXITOK              BACS                                         
*                                                                               
VPAY16   EX    RF,*+8                                                           
         BE    VPAY18                                                           
         CLC   FVIFLD(0),UC@MANCH                                               
         EX    RF,*+8                                                           
         BE    VPAY18                                                           
         CLC   FVIFLD(0),LC@MANCH                                               
         B     VPAY20                                                           
*                                                                               
VPAY18   OI    BSTAT,BACSTMCH                                                   
         MVC   FVIFLD(PYLN),LC@MANCH                                            
         B     EXITOK              MANUAL CHEQUE                                
*                                                                               
VPAY20   CLI   CUCTRY,CTRYGER                                                   
         BNE   VPAY24                                                           
         EX    RF,*+8                                                           
         BE    VPAY22                                                           
         CLC   FVIFLD(0),UC@XFR                                                 
         EX    RF,*+8                                                           
         BE    VPAY22                                                           
         CLC   FVIFLD(0),LC@XFR                                                 
         B     VPAY24                                                           
*                                                                               
VPAY22   OI    BSTAT,BACSTTRF                                                   
         MVC   FVIFLD(PYLN),LC@XFR                                              
         B     EXITOK              TRANSFER - ONLY FOR THE GERMANS              
*                                                                               
VPAY24   B     EXITNV              NOTHING ELSE ALLOWED                         
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SUNDRY CREDITOR                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SCRDTA   LA    RF,SCRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SCRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSCR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUNDRY CREDITOR                                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSTELD,BOELEM                                                    
DISSCR   GOTOX AGETEL,BOPARM,('RSTELQ',ACTRECD),0                               
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'LC@NOK),LC@NOK                                          
         TM    RSTSTAT5,RSTSSUND   SUNDRY CREDITOR                              
         BZ    EXITOK              NO                                           
         MVC   FVIFLD(L'LC@AOK),LC@AOK                                          
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUNDRY CREDITOR                                            *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING RSTELD,BOELEM                                                    
VALSCR   XC    BOELEM,BOELEM                                                    
         GOTOX AGETEL,BOPARM,('RSTELQ',ACTRECD),0                               
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ELNOF)                                           
         B     EXITL                                                            
*                                                                               
         GOTOX ADELEL,BOPARM,('RSTELQ',ACTRECD),0                               
         MVI   RSTLN,RSTLN3Q       ADJUST LENGTH                                
         NI    RSTSTAT5,FF-(RSTSSUND)                                           
         CLI   FVILEN,0                                                         
         BE    VSCROK                                                           
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8                                                           
         BE    VSCROK                                                           
         CLC   FVIFLD(0),UC@NOK                                                 
         EX    RE,*+8                                                           
         BE    VSCROK                                                           
         CLC   FVIFLD(0),LC@NOK                                                 
*                                                                               
         EX    RE,*+8                                                           
         BE    VSCR02                                                           
         CLC   FVIFLD(0),UC@AOK                                                 
         EX    RE,*+8                                                           
         BE    VSCR02                                                           
         CLC   FVIFLD(0),LC@AOK                                                 
         B     VSCRX                                                            
*                                                                               
VSCR02   OI    RSTSTAT5,RSTSSUND                                                
         B     VSCROK                                                           
                                                                                
VSCROK   GOTOX AADDEL,BOPARM,ACTRECD                                            
         B     EXITOK                                                           
*                                                                               
VSCRX    GOTOX AADDEL,BOPARM,ACTRECD                                            
         B     EXITNV                                                           
         POP   USING                                                            
         SPACE 2                                                                
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
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SREC,R#CRORD                                                     
         BNE   EXITOK                                                           
         CLI   SACT,A#LST                                                       
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (OUT)                    *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (BACK)                   *         
***********************************************************************         
         SPACE 1                                                                
XITIN    DS    0H                                                               
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
THIS     USING ACTRECD,R2                                                       
LAST     USING ACTRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
FTFLST   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'ACTKEY),THIS.ACTRECD                                     
         GOTOX AGETLDG                                                          
         ICM   R4,15,ACALDG                                                     
         USING LDGTABD,R4                                                       
         XC    LGRLEN,LGRLEN                                                    
         XR    RF,RF                                                            
         XR    RE,RE                                                            
         CLI   LDGTLVA,L'ACTKACT   SINGLE LEVEL LEDGER?                         
         BE    EXITOK              YES - OK                                     
*                                                                               
         CLI   LDGTLVB,L'ACTKACT   2 LEVEL ACCOUNT                              
         BNE   FTFL02                                                           
         CLC   ARTIST,THIS.ACTKUNT PENULTIMATE LEVEL FOR ST                     
         BE    EXITOK                                                           
         IC    RF,LDGTLVA          GET LENGTH OF PRIOR LEVEL                    
         B     FTFL06                                                           
*                                                                               
FTFL02   CLI   LDGTLVC,L'ACTKACT   3 LEVEL ACCOUNT                              
         BNE   FTFL04                                                           
         IC    RF,LDGTLVA          GET COMBINED LENGTHS OF PRIOR LEVELS         
         CLC   ARTIST,THIS.ACTKUNT PENULTIMATE LEVEL FOR ST                     
         BE    *+8                                                              
         IC    RE,LDGTLVB                                                       
         LA    RF,0(RE,RF)                                                      
         B     FTFL06                                                           
*                                                                               
FTFL04   CLI   LDGTLVD,L'ACTKACT   4 LEVEL ACCOUNT                              
         BE    *+6                                                              
         DC    H'0'                ACCOUNT LEVELS ALL WRONG                     
         IC    RF,LDGTLVA          GET COMBINED LENGTHS OF PRIOR LEVELS         
         IC    RE,LDGTLVB                                                       
         LA    RF,0(RE,RF)                                                      
         CLC   ARTIST,THIS.ACTKUNT PENULTIMATE LEVEL FOR ST                     
         BE    *+12                                                             
         IC    RE,LDGTLVC                                                       
         LA    RF,0(RE,RF)                                                      
         DROP  R4                                                               
*                                                                               
FTFL06   STCM  RF,1,LGRLEN                                                      
         MVC   IOKEY(L'ACTKEY),THIS.ACTRECD                                     
         ICM   R1,15,=AL4(IOREC)                                                
         LA    R1,IOACCDIR+IOHI(R1)                                             
         GOTOX AIO                                                              
         BNE   EXITL               READ HIGH UNHAPPY                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     XR    RF,RF               CONTROLLER REESTABLISHES SEQUENCE            
         LA    RE,IOKEY                                                         
         IC    RF,ACTKACT+L'ACTKACT-1-ACTRECD(RE)                               
         LA    RF,1(RF)                                                         
         STC   RF,ACTKACT+L'ACTKACT-1-ACTRECD(RE)                               
         ICM   R1,15,=AL4(IOREC)                                                
         LA    R1,IOACCDIR+IOHI(R1)                                             
         GOTOX AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
         CLC   IOKEY(L'ACTKCPY),THIS.ACTRECD                                    
         BNE   EXITL               NOTHING ELSE OF INTEREST ON FILE             
*                                                                               
X        USING ACTRECD,IOKEY                                                    
NLST02   CLI   X.ACTKUNT,C' '                                                   
         BNH   NLST                MUST HAVE A UNIT                             
         CLI   X.ACTKLDG,C' '                                                   
         BNH   NLST                MUST HAVE A LEDGER                           
         CLC   X.ACTKACT,BCSPACES                                               
         BNH   NLST                MUST HAVE AN ACCOUNT                         
         XR    RE,RE                                                            
         ICM   RE,1,LGRLEN         LENGTH OF NEXT LOWEST LEVEL                  
         BZ    NLST04                                                           
         LA    R1,X.ACTKACT(RE)    FIRST CHARACTER OF THIS LEVEL                
         LA    RF,L'ACTKACT        LENGTH OF ACCOUNT                            
         SR    RF,RE               RF=(L'LEFT FOR THIS LEVEL)                   
         SH    RF,=H'1'                                                         
         BM    NLST04                                                           
         EX    RF,*+8              SEE IF THIS LEVEL CONTAINS SPACES            
         BNH   NLST                YES - WE DON`T WANT IT                       
         CLC   0(0,R1),BCSPACES                                                 
*                                                                               
NLST04   MVC   THIS.ACTKEY(ACCKLEN),IOKEY   WE WANT THIS KEY                    
         B     EXITOK                                                           
         DROP  X                                                                
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO ADD HIGH LEVEL ACCOUNT IF IT DOES NOT ALREADY EXIST      *         
* ENTRY: KEY OF ACCOUNT TO BE ADDED IS IN IOKEY                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
NEW      USING ACTRECD,R3                                                       
ADDACC   NTR1                                                                   
         L     R3,AIO2                                                          
         MVC   NEW.ACTKEY,IOKEY                                                 
         MVI   NEW.ACTRFST,0                                                    
         LA    RF,ACTRFST-ACTRECD+1                                             
         STCM  RF,3,NEW.ACTRLEN                                                 
         GOTOX AADDRST,NEW.ACTRECD ADD RSTEL                                    
         GOTOX AADDAST,NEW.ACTRECD ADD ASTEL                                    
         USING NAMELD,BOELEM                                                    
         XC    BOELEM,BOELEM                                                    
         MVI   NAMEL,NAMELQ        ADD NAMEL 'AUTO ACCOUNT ADD'                 
         MVI   NAMLN,NAMLN1Q+L'NAMEREC                                          
         MVC   NAMEREC,BCSPACES                                                 
         MVI   NAMEREC,DD#ESCC     CENTER                                       
         MVC   NAMEREC+1(2),=AL2(AC#AUTAD)                                      
         MVI   NAMEREC+3,L'NAMEREC                                              
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,GCOVSYS                                                     
         GOTOX VDICTAT,BOPARM,(RF),NAMEREC                                      
         GOTOX AADDEL,BOPARM,NEW.ACTRECD                                        
         GOTOX APUTRAC,BOPARM,('RACTADD+RACTCHA',NEW.ACTRECD)                   
         GOTOX AIO,IOADD+IOACCMST+IO2                                           
         B     EXITL                                                            
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
PYLN     EQU   14                                                               
ARTIST   DC    CL2'ST'                                                          
EFFS     DC    (L'ASTCUR)X'FF'                                                  
         SPACE 1                                                                
DCLIST   DS    0D                                                               
         DCDDL AC#CHK,PYLN,L                                                    
         DCDDL AC#PYMNT,PYLN,L                                                  
         DCDDL AC#BACS,PYLN,L                                                   
         DCDDL AC#MANCH,PYLN,L                                                  
         DCDDL AC#XFR,PYLN,L                                                    
         DCDDL AC#ALLC,30,L                                                     
         DCDDL AC#NO,4,L                                                        
         DCDDL AC#YES,4,L                                                       
DCLISTX  DC    X'00'                                                            
         SPACE 2                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*        ACFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
       ++INCLUDE DDDDEQUS                                                       
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
ASPARE   DS    A                                                                
ABSTAT   DS    A                                                                
ABSORT   DS    A                                                                
ABCOUNT  DS    A                                                                
ABBNAME  DS    A                                                                
ABACNAM  DS    A                                                                
LGRLEN   DS    XL1                                                              
ADDED    DS    XL1                                                              
TEMPKEY  DS    CL(L'ACTKEY)                                                     
BSTAT    DS    CL(L'BACSTAT)                                                    
BSORT    DS    CL(L'BACSORT)                                                    
BCOUNT   DS    CL(L'BACCOUNT)                                                   
BBNAME   DS    CL(L'BACBNAME)                                                   
BACNAM   DS    CL(L'BACACNAM)                                                   
*                                                                               
CURCODE  DS    CL(L'CURTSYMB)                                                   
CURTENT  DS    CL(L'CURTABL+L'CURTSHRT+L'CURTLONG)                              
*                                                                               
DSLISTU  DS    0D                  UPPERCASE FOR MATCHING                       
UC@CHK   DS    CL(PYLN)                                                         
UC@PYMNT DS    CL(PYLN)                                                         
UC@BACS  DS    CL(PYLN)                                                         
UC@MANCH DS    CL(PYLN)                                                         
UC@XFR   DS    CL(PYLN)                                                         
UC@ALLC  DS    CL30                                                             
UC@NOK   DS    CL4                                                              
UC@AOK   DS    CL4                                                              
*                                                                               
DSLISTL  DS    0D                  LOWERCASE FOR DISPLAY                        
LC@CHK   DS    CL(PYLN)                                                         
LC@PYMNT DS    CL(PYLN)                                                         
LC@BACS  DS    CL(PYLN)                                                         
LC@MANCH DS    CL(PYLN)                                                         
LC@XFR   DS    CL(PYLN)                                                         
LC@ALLC  DS    CL30                                                             
LC@NOK   DS    CL4                                                              
LC@AOK   DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'061ACFIL15   02/06/03'                                      
         END                                                                    
