*          DATA SET SPSFM31    AT LEVEL 050 AS OF 09/22/03                      
*PHASE T21731A,*                                                                
         TITLE 'T21731  STATUS RECORDING RECORDS'                               
T21731   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T21731                                                         
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    *+6                 HANDLE ALL THRU VALKEY                       
         DC    H'0'                                                             
*                                                                               
         NI    GENSTAT2,USGETTXT   USE OWN MSGS                                 
         CLI   MYOVNUM,X'31'       1ST TIME THROUGH PROG                        
         BE    MAIN4               YES                                          
*                                                                               
GOAHEAD  MVI   MYOVNUM,X'31'       SET OVERLAY NUMBER                           
         NI    STTMEDH+4,X'DF'     THEN FORCE VALIDATION OF KEY                 
         EJECT                                                                  
*                                                                               
* READ CONTROL FILE / CCUSA PROFILE TO GET CCUSA SYS NUM                        
*                                                                               
MAIN     XC    SAVPROF,SAVPROF                                                  
         CLC   TWAORIG,=AL2(797)   SIGN ON CCUSA                                
         BE    MAIN4                                                            
*                                                                               
         XC    WORK,WORK                                                        
         MVI   WORK,C'I'                                                        
         MVC   WORK+15(10),=CL10'CCUSA'                                         
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'CTFILE',WORK,IOAREA                   
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   WORK(25),IOAREA                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0                                                            
         LA    R1,IOAREA+28                                                     
*                                                                               
MAIN2    CLI   0(R1),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R1),X'21'         TEST SYSTEM ELEMENT                          
         BNE   *+12                                                             
         CLI   2(R1),X'02'         TEST SPOT                                    
         BE    MAIN3                                                            
         IC    R0,1(R1)                                                         
         AR    R1,R0                                                            
         B     MAIN2                                                            
*                                                                               
MAIN3    MVC   SAVSYS,3(R1)        SYSTEM NUMBER                                
         MVC   SAVAGY,4(R1)        AGENCY NUMBER                                
*                                                                               
MAIN4    MVI   KEYCHANG,C'N'       INIT SWITCH                                  
         BAS   RE,VK               VALIDATE KEY                                 
         CLI   KEYCHANG,C'Y'       NEW KEY                                      
         BE    MAIN20                                                           
         BAS   RE,VALWIND                                                       
*                                                                               
MAIN20   BAS   RE,DISPWIND                                                      
*                                                                               
MAINX    B     EXIT2               ACTION COMPLETE - EXIT                       
         EJECT                                                                  
*                                                                               
* VALIDATE KEY - VALIDATES KEY, SETS KEYCHANG SWITCH                            
* READ PROFILE AND VALIDATE ACCORDINGLY                                         
*                                                                               
VK       NTR1                                                                   
         XC    SVKEY,SVKEY                                                      
         LA    R3,SVKEY                                                         
         USING STATD,R3                                                         
         MVC   STKTYPE,=X'0D71'    RECORD TYPE                                  
*                                                                               
         LA    R2,STTMEDH          MEDIA                                        
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VKCLT                                                            
         GOTO1 VALIMED                                                          
         NI    STTCLIH+4,X'DF'     FORCE VALIDATION                             
         OI    4(R2),X'20'                                                      
*                                                                               
VKCLT    LA    R2,STTCLIH          CLIENT                                       
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VKCKPRO                                                          
         GOTO1 VALICLT                                                          
*                                                                               
         MVI   CLTCC,C'N'          CHECK IF CLIENT + PROF = COKE                
         CLC   8(2,R2),=C'CC'                                                   
         BNE   VKCLT1                                                           
         CLI   SVCXTRA+2,C'*'                                                   
         BNE   VKCLT1                                                           
         MVI   CLTCC,C'Y'                                                       
*                                                                               
VKCLT1   NI    STTKPRH+4,X'DF'     FORCE VALIDATION                             
         NI    STTKESH+4,X'DF'                                                  
         NI    STTSATH+4,X'DF'                                                  
         MVI   KEYCHANG,C'Y'                                                    
         OI    4(R2),X'20'                                                      
*                                                                               
VKCKPRO  XC    WORK,WORK                                                        
         MVC   WORK(4),=C'S0ST'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),TWAOFFC                                               
         GOTO1 GETPROF,DMCB,WORK,WORK+24,DATAMGR                                
         MVC   SAVPROF,WORK+24                                                  
         CLI   SAVPROF,0                                                        
*        BE    ERRPRO              NO PROFILE                                   
*                                                                               
VKPRD    LA    R2,STTKPRH          PRODUCT                                      
*        CLI   SAVPROF,C'P'        IGNORE PRODUCT BY PROFILE                    
*        BNE   VKPRD5                                                           
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VKEST                                                            
         CLI   CLTCC,C'Y'          IS THIS COKE                                 
         BNE   VKPRD1                                                           
         BAS   RE,VCCPRD           VALIDATE PRD ON COKE'S FILE                  
         B     VKPRD2                                                           
*                                                                               
VKPRD1   GOTO1 VALIPRD             ELSE VALIDATE PRODUCT                        
*                                                                               
VKPRD2   NI    STTKESH+4,X'DF'     FORCE VALIDATION                             
         MVI   KEYCHANG,C'Y'                                                    
         OI    4(R2),X'20'                                                      
         B     VKEST                                                            
         EJECT                                                                  
*                                                                               
VKPRD5   MVC   8(3,R2),SPACES                                                   
         MVI   BPRD,0                                                           
         OI    6(R2),X'80'                                                      
*                                                                               
VKEST    LA    R2,STTKESH          ESTIMATE                                     
         CLI   SAVPROF+2,C'E'      VALIDATE EST                                 
         BNE   VKE20               NO                                           
         CLI   SAVPROF,C'P'        IF VALIDATE EST ONLY (NOT PRD)               
         BNE   VKE10                  THEN EST MUST BE BETWEEN 1 - 255          
*                                                                               
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VKMKT                                                            
         NI    STTSATH+4,X'DF'     FORCE VALIDATION                             
         GOTO1 VALIEST                                                          
         MVI   KEYCHANG,C'Y'                                                    
         OI    4(R2),X'20'                                                      
         B     VKMKT                                                            
*                                                                               
VKE10    TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BO    VKMKT                                                            
         NI    STTSATH+4,X'DF'     FORCE VALIDATION                             
         GOTO1 ANY                                                              
         TM    4(R2),X'08'         VALID NUMERIC                                
         BZ    ESTERR                                                           
         ZIC   RE,5(R2)            LENGTH OF  INPUT                             
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         EXECUTED                                     
*                                                                               
         CVB   RE,DUB                                                           
         CH    RE,=H'1'            COMPARE RANGE 1 - 255                        
         BL    ESTERR                                                           
         CH    RE,=H'255'                                                       
         BH    ESTERR                                                           
         STC   RE,BEST                                                          
         MVI   KEYCHANG,C'Y'                                                    
         OI    4(R2),X'20'                                                      
         B     VKMKT                                                            
*                                                                               
VKE20    MVC   8(3,R2),SPACES                                                   
         MVI   BEST,0                                                           
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
*                                                                               
VKMKT    LA    R2,STTSATH          START AT SPECIFIC MKT                        
         TM    4(R2),X'20'                                                      
         BO    VKX                                                              
         MVI   KEYCHANG,C'Y'                                                    
         CLI   5(R2),0                                                          
         BE    VKMKT1                                                           
*                                                                               
         TM    4(R2),X'08'         NUMERIC                                      
         BZ    ERRTYPE                                                          
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BCTR  RE,0                SET FOR MOVE                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         ** EXECUTED ** GET BINARY MKT                
         CVB   R0,DUB                                                           
         STCM  R0,3,BMKT                                                        
         OI    4(R2),X'20'         VALID                                        
         B     VKX                                                              
*                                                                               
VKMKT1   MVI   8(R2),C'1'          FORCE MKT = 1                                
         MVI   BMKT+1,X'1'                                                      
         OI    4(R2),X'28'         VALID NUMERIC                                
         MVI   5(R2),1                                                          
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
VKX      MVC   STKAGMD,BAGYMD                                                   
         CLI   CLTCC,C'Y'          COKE CLIENT                                  
         BNE   *+10                USE COKE'S A/M ON CCUSA FILE                 
         MVC   STKAGMD,CCAGYMD                                                  
*                                                                               
         MVC   STKCLT,BCLT                                                      
         MVC   STKPRD,BPRD                                                      
         MVC   STKEST,BEST                                                      
         MVC   STKMKT,BMKT                                                      
*                                                                               
         MVC   MAINKEY,SVKEY                                                    
         B     EXIT                                                             
         DROP  R3                                                               
         EJECT                                                                  
**************************************************************                  
* VALIDATE WINDOW - CONFIRM THAT ALL INFO ON SCREEN IS VALID *                  
* 1ST - VALIDATE ALL MKTS AND STATIONS (IF NECCESSARY)       *                  
*       ON THE SCREEN IN SIGN ON SYSYTEM                     *                  
**************************************************************                  
         SPACE 2                                                                
VALWIND  NTR1                                                                   
         LA    R2,STTMKTH          START OF INPUT                               
         LR    R4,R2                                                            
         LA    R7,STTMK2H                                                       
         SR    R7,R2               DIFF BETWEEN 2 LINES                         
         GOTO1 DATCON,DMCB,(5,FULL),(2,SVDATE)                                  
*                                                                               
VW5      TM    4(R2),X'28'         VALID & NUMERIC                              
         BO    VW10                                                             
         GOTO1 VALIMKT             VALIDATE MARKET                              
         OI    4(R2),X'20'         VALID                                        
*                                                                               
VW10     SR    R0,R0               PT TO MKT NAME                               
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         SR    R0,R0               POSITION R2 TO STATION                       
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLI   SAVPROF+1,C'M'      KEY BY MARKET ONLY                           
         BE    VW15                                                             
*                                                                               
         TM    4(R2),X'20'         DETERMINE IF STATION VALID                   
         BO    VW15                                                             
         GOTO1 VALISTA                                                          
         OI    4(R2),X'20'                                                      
*                                                                               
VW15     AR    R4,R7               CHECK NEXT LINE                              
         LR    R2,R4               START OF NEXT LINE                           
         CLI   5(R2),0             ANY INPUT                                    
         BNE   VW5                                                              
         EJECT                                                                  
*                                                                               
* SWITCH TO CCUSA'S FILE                                                        
*                                                                               
VW20     CLC   TWAORIG,=AL2(797)   SIGN ON CCUSA                                
         BE    VW37                                                             
         CLI   CLTCC,C'Y'          IF NOT FOR COKE DON'T SWITCH                 
         BNE   VW37                                                             
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SAVSYS                                                   
*                                                                               
* TEST CODE BY SIGNING ON AS WWNY AND REALLY WRITING TO                         
* AN SJR FILE                                                                   
*                                                                               
         CLC   TWAORIG,=AL2(17)         SJR                                     
         BE    VW30                                                             
         CLC   TWAORIG,=AL2(1555)       WWNY                                    
         BNE   VW35                                                             
*                                                                               
VW30     MVI   DMCB,2                                                           
*                                                                               
VW35     GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VW37     LA    R2,STTMKTH                                                       
*                                                                               
VW40     BAS   RE,VR               VALIDATE RECORD                              
         AR    R2,R7               NEXT LINE                                    
         BAS   RE,TSTFLDS          TEST ALL FIELDS                              
         CLI   SWEMPTY,C'Y'        ANY MORE INPUT ON SCREEN                     
         BNE   VW40                                                             
*                                                                               
VWX      BAS   RE,MNAME                                                         
         B     EXIT                                                             
         EJECT                                                                  
********************************************************                        
* UPON ENTRY R2 PTS TO THE MARKET FIELD                *                        
*            SYSTEM IF CLT = CC THEN CCUSA SYSTEM      *                        
*                   ELSE     SIGN ON SYSTEM            *                        
*                                                      *                        
* ALL MKTS/STATTIONS ON SCREEN ARE VALIDATED           *                        
* CHECK IF RECORD EXISTS (CHANGE) OR IS TO BE          *                        
* ADDED                                                *                        
********************************************************                        
         SPACE 1                                                                
VR       NTR1                                                                   
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         XC    KEY,KEY                                                          
         MVC   KEY(8),MAINKEY      0D71/AM/CLT/PRD/REC TYPE/EST                 
         LA    R5,KEY                                                           
         USING STATKEY,R5                                                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BCTR  RE,0                SET FOR PACK/MOVE                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         ** EXECUTED ** GET BINARY MKT                
         CVB   R0,DUB                                                           
         STCM  R0,3,STKMKT                                                      
*                                                                               
         SR    R0,R0               PT TO MKT NAME                               
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         SR    R0,R0               POSITION R2 TO STATION                       
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLI   SAVPROF+1,C'S'      KEY BY STATION                               
         BNE   VR40                                                             
         CLI   QMED,C'T'                                                        
         BNE   VR39                                                             
         MVI   12(R2),C'T'                                                      
*                                                                               
VR39     OC    8(5,R2),SPACES                                                   
         MVI   TACT,C'P'                                                        
         MVC   TMKT,=C'0000'                                                    
         MVC   TSTA,8(R2)                                                       
         MVC   TMKST,STKMKT                                                     
         GOTO1 =A(SETMSP),DMCB,(RC)                                             
         MVC   MYMKTST,TMKST                                                    
         EJECT                                                                  
*                                                                               
VR40     GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    VR100                                                            
*                                                                               
* RECORD DOES NOT EXIST - BUILD IT IN AIO                                       
* R2 POINTS TO STATION                                                          
*                                                                               
VR50     XC    0(150,R6),0(R6)     CLEAR OUT IO AREA                            
         MVC   0(13,R6),KEYSAVE    BUILT KEY                                    
         MVI   14(R6),X'46'        REC LEN                                      
         LA    R6,24(R6)           POSITION OF 1ST ELEM                         
*                                                                               
         XC    ELEM,ELEM           SET UP TO BUILD ELEMENT                      
         LA    R3,ELEM                                                          
         USING CSELEM,R3                                                        
         MVI   CSCODE,CSCODEQ      ELEM CODE                                    
         MVI   CSLEN,CSLENQ        LENGTH                                       
         MVC   CSAGYA,AGENCY       ORIGINATING AGENCY                           
*                                                                               
         SR    R0,R0               POSITION R2 TO BUY FIELD                     
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLC   TWAORIG,=AL2(797)   IF CCUSA                                     
         BE    VR60                CANNOT CHANGE BUY STATUS                     
         CLI   5(R2),0             ANY INPUT                                    
         BE    VR60                                                             
         CLI   8(R2),C'X'          SET BUY TO TODAYS DATE                       
         BNE   VR60                                                             
         MVC   CSBUY,SVDATE                                                     
*                                                                               
VR60     SR    R0,R0               POSITION R2 TO MAT FIELD                     
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         SR    R0,R0               POSITION R2 TO PAY FIELD                     
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         SR    R0,R0               POSITION R2 TO LOCK FIELD                    
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
         EJECT                                                                  
*                                                                               
VR65     CLI   CLTCC,C'Y'          CANNOT CHANGE LOCK FIELD/CLIENT = CC         
         BE    VR70                IGNORE                                       
         CLI   5(R2),0             ANY INPUT                                    
         BE    VR70                                                             
         CLI   8(R2),C'X'          SET LOCK TO TODAYS DATE                      
         BNE   VR70                                                             
         MVC   CSLOCK,SVDATE                                                    
         DROP  R3                                                               
*                                                                               
VR70     MVC   BPLENQ(CSLENQ,R6),ELEM                                           
         XC    ELEM,ELEM           SET UP TO BUILD ELEMENT                      
         LA    R3,ELEM                                                          
         USING BPELEM,R3                                                        
         MVI   BPCODE,BPCODEQ      ELEM CODE                                    
         MVI   BPLEN,BPLENQ        LENGTH                                       
*                                                                               
         SR    R0,R0               POSITION R2 TO BUYER FIELD                   
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    VR80                                                             
         MVC   BPBUYER,8(R2)                                                    
*                                                                               
VR80     SR    R0,R0               POSITION R2 TO PAYER FIELD                   
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLI   5(R2),0                                                          
         BE    VR90                                                             
         MVC   BPPAYER,8(R2)                                                    
*                                                                               
VR90     MVC   0(BPLENQ,R6),ELEM                                                
         L     R6,AIO                                                           
         MVC   KEY(13),0(R6)       SET UP KEY                                   
         GOTO1 DATAMGR,DMCB,(0,=C'ADDREC'),=C'SPTFIL',KEY,AIO,WORK1             
         CLI   8(R1),0                                                          
         BE    VRX                 EXIT TO VALWIND                              
         DC    H'0'                                                             
         DROP  R3                                                               
         EJECT                                                                  
*                                                                               
*  THE RECORD EXISTS - TEST IF CHANGED                                          
*                                                                               
VR100    MVC   DA,KEY+14                                                        
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFIL',DA,AIO,WORK1          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ELEM,ELEM                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,CSCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR110    MVI   SWCHANGE,C'N'       DEFAULT TO NOT CHANGED                       
         LA    R4,ELEM                                                          
         USING CSELEM,R4                                                        
         MVC   ELEM(CSLENQ),0(R6)                                               
         MVC   CSAGYA,AGENCY       MAKE SURE AGENCY'S IN THE ELEMENT            
*                                                                               
VR120    SR    R0,R0               PT TO BUY FIELD                              
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLC   TWAORIG,=AL2(797)   CCUSA ID                                     
         BE    VR140               CANNOT CHANGE BUY FIELD                      
         CLI   5(R2),0             INPUT?                                       
         BE    VR140                                                            
         OC    CSBUY,CSBUY                                                      
         BZ    VR130               NOT YET SET                                  
*                                                                               
         CLI   8(R2),C'N'          UN-SET BUY FIELD                             
         BNE   VR140               ELSE IGNORE                                  
         XC    CSBUY,CSBUY                                                      
         B     VR140                                                            
*                                                                               
VR130    CLI   8(R2),C'X'          SET                                          
         BNE   VR140               IF NOT 'X' - IGNORE                          
         MVC   CSBUY,SVDATE        TODAYS DATE                                  
         EJECT                                                                  
*                                                                               
VR140    SR    R0,R0               PT TO MAT FIELD                              
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         SR    R0,R0               PT TO PAY FIELD                              
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         SR    R0,R0               PT TO LOCK FIELD                             
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLI   CLTCC,C'Y'          CLIENT = COKE CANNOT                         
         BE    VR160               LOCK FIELD                                   
         CLI   5(R2),0             INPUT                                        
         BE    VR160                                                            
         OC    CSLOCK,CSLOCK       LOCK NOT SET                                 
         BZ    VR150                                                            
*                                                                               
         CLI   8(R2),C'N'          UN-SET LOCK FIELD                            
         BNE   VR160                                                            
         XC    CSLOCK,CSLOCK                                                    
         B     VR160                                                            
*                                                                               
VR150    CLI   8(R2),C'X'          UN-SET                                       
         BNE   VR160                                                            
         MVC   CSLOCK,SVDATE       TODAYS DATE                                  
*                                                                               
VR160    CLC   0(CSLENQ,R6),ELEM   IF THERE WERE CHANGES THEN                   
         BE    VR170                                                            
         MVI   SWCHANGE,C'Y'       SET FLAG AND                                 
         MVC   0(CSLENQ,R6),ELEM   COPY THEM                                    
         DROP  R4                                                               
*                                                                               
VR170    XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         USING BPELEM,R4                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,BPCODEQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   ELEM(BPLENQ),0(R6)                                               
         EJECT                                                                  
*                                                                               
VR180    SR    R0,R0               PT TO BUYER NAME                             
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLC   BPBUYER,8(R2)       ANYTHING IS LEGAL                            
         BE    VR190                                                            
*                                                                               
         MVC   BPBUYER,8(R2)                                                    
         MVI   SWCHANGE,C'Y'                                                    
*                                                                               
VR190    SR    R0,R0               PT TO PAYER NAME                             
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLC   BPPAYER,8(R2)       ANYTHING IS LEGAL                            
         BE    VR200                                                            
         MVC   BPPAYER,8(R2)                                                    
         MVI   SWCHANGE,C'Y'                                                    
*                                                                               
VR200    CLI   SWCHANGE,C'N'       IF THERE WEREN'T ANY CHANGES                 
         BE    VRX                 THEN EXIT                                    
         MVC   0(BPLENQ,R6),ELEM   ELSE REWRITE THE RECORD                      
         GOTO1 DATAMGR,DMCB,(0,=C'PUTREC'),=C'SPTFIL',DA,AIO,WORK1              
*                                                                               
VRX      B     EXIT                                                             
         DROP  R4,R5                                                            
         EJECT                                                                  
************************************************                                
* DISPLAY WINDOW - 13 LINES PER WINDOW         *                                
************************************************                                
         SPACE 2                                                                
DISPWIND NTR1                                                                   
         GOTO1 CLEARF,DMCB,(0,STTPRDH),STTLAST                                  
         GOTO1 CLEARF,DMCB,(1,STTPRDH),STTLAST                                  
*                                                                               
         CLC   TWAORIG,=AL2(797)   SIGN ON CCUSA                                
         BE    DWE                                                              
         CLI   CLTCC,C'Y'          CLIENT = COKE                                
         BNE   DWE                                                              
*                                                                               
* SWITCH TO CCUSA'S FILE                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SAVSYS                                                   
*                                                                               
* TEST CODE BY SIGNING ON AS WWNY AND REALLY WRITING TO                         
* AN SJR FILE                                                                   
*                                                                               
         CLC   TWAORIG,=AL2(17)         SJR                                     
         BE    DWC                                                              
*                                                                               
DWB      CLC   TWAORIG,=AL2(1555)       WWNY                                    
         BNE   DWD                                                              
*                                                                               
DWC      MVI   DMCB,2                                                           
*                                                                               
DWD      GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DWE      XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING STATKEY,R5                                                       
         MVC   KEY(8),SVKEY                                                     
         LA    R2,STTSATH                                                       
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,1,5(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BCTR  RE,0                SET FOR MOVE                                 
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)         ** EXECUTED ** GET BINARY MKT                
         CVB   R0,DUB                                                           
         STCM  R0,3,STKMKT                                                      
         EJECT                                                                  
*                                                                               
         GOTO1 HIGH                READ RECORD                                  
         CLI   SAVPROF+2,C'E'      0D71/AM/CLT/PRD/00/EST                       
         BNE   DW2                                                              
*                                                                               
DW1      CLC   KEY(8),KEYSAVE      0D71/AM/CLT/PRD/00/EST                       
         BE    DW3                                                              
         B     DWX                 NO RECORDS                                   
*                                                                               
DW2      CLC   KEY(7),KEYSAVE      0D71/AM/CLT/PRD(00)/REC TYPE                 
         BNE   DWX                 NO RECORDS                                   
*                                                                               
DW3      LA    R7,STTMK2H          DIFFERENCE BET 2 LINES                       
         LA    R2,STTMKTH                                                       
         SR    R7,R2                                                            
*                                                                               
         CLI   SAVPROF,C'P'        PRD NOT REQUIRED                             
         BNE   DW4                                                              
         LA    R5,KEY                                                           
         LA    R2,STTPRDH                                                       
         LA    R3,STTKPRH                                                       
         MVC   8(3,R2),8(R3)       MOVE PRODUCT CODE                            
         OI    6(R2),X'80'         TO SCREEN                                    
*                                                                               
DW4      CLI   SAVPROF+2,C'E'      EST NOT REQUIRED                             
         BNE   DW5                                                              
         LA    R2,STTESTH                                                       
         LA    R3,STTKESH                                                       
         MVC   8(3,R2),8(R3)       MOVE ESTIMATE                                
         OI    6(R2),X'80'                                                      
*                                                                               
DW5      LA    R2,STTMKTH          CLEAR LINE COUNTER                           
         XC    LCOUNT,LCOUNT                                                    
         B     DW20                                                             
         EJECT                                                                  
*                                                                               
DW10     GOTO1 SEQ                                                              
         CLI   SAVPROF+2,C'E'      0D71/AM/CLT/00/00/EST                        
         BNE   DW12                                                             
*                                                                               
DW11     CLC   KEY(8),KEYSAVE      0D71/AM/CLT/PRD(00)/REC TYPE/EST             
         BE    DW20                                                             
         B     DW100               NO RECORDS                                   
*                                                                               
DW12     CLC   KEY(7),KEYSAVE      0D71/AM/CLT/PRD(00)/REC TYPE                 
         BNE   DW100               NO RECORDS                                   
*                                                                               
DW20     MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         XC    0(100,R6),0(R6)     CLEAR ENOUGH ROOM                            
         MVC   DA,KEY+14           DISK ADDRESS                                 
         MVC   SVKEY(13),KEY       SAVE LAST KEY READ                           
*                                                                               
         GOTO1 DATAMGR,DMCB,(X'80',=C'GETREC'),=C'SPTFIL',DA,AIO,WORK1          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,DR               DISPLAY RECORD                               
*                                                                               
         SR    R1,R1               INC LINE COUNTER                             
         ICM   R1,1,LCOUNT                                                      
         LA    R1,1(R1)                                                         
         STCM  R1,1,LCOUNT                                                      
*                                                                               
         CLI   LCOUNT,13           NUMBER OF LINES PER SCREEN - 1               
         BNL   DW100               GET NEXT RECORD                              
         AR    R2,R7               PT TO NEXT LINE                              
         B     DW10                READ NEXT REC                                
*                                                                               
DW100    BAS   RE,MNAME                                                         
*                                                                               
DWX      B     EXIT                                                             
         EJECT                                                                  
************************************************                                
* UPON ENTRY - R2 PTS TO MARKET FIELD          *                                
*              RECORD IS LOCATED IN AIO2       *                                
************************************************                                
         SPACE 1                                                                
DR       NTR1                                                                   
         SR    R0,R0                                                            
         XC    DUB,DUB                                                          
         ICM   R0,3,STKMKT         CONVERT MARKET TO EBCDIC                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'         SIGN                                         
         UNPK  8(4,R2),DUB         MARKET NUMBER                                
         OI    4(R2),X'28'         VALID & NUMERIC                              
         MVI   5(R2),4                                                          
         OI    6(R2),X'80'                                                      
         MVC   XMKT,8(R2)                                                       
*                                                                               
         SR    R0,R0               POSITION R2 TO MARKET NAME                   
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         SR    R0,R0               POSITION R2 TO STATION                       
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         CLI   SAVPROF+1,C'M'      IGNORE STATION                               
         BE    DR10                                                             
*                                                                               
         MVI   TACT,C'U'                                                        
         MVC   TMKST,STKMKT                                                     
         GOTO1 =A(SETMSP),DMCB,(RC)                                             
         MVC   XMKT,TMKT                                                        
         MVC   XSTA,TSTA                                                        
         MVC   STKSTA,MYSTA                                                     
         MVC   8(4,R2),XSTA        STATION CALL LETTERS                         
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
*                                                                               
DR10     XC    SVBUYER,SVBUYER                                                  
         XC    SVPAYER,SVPAYER                                                  
         MVI   ELCODE,BPCODEQ                                                   
         L     R6,AIO                                                           
         USING BPELEM,R6           GET BUYER PAYER NAMES                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   SVBUYER,BPBUYER     BUYER NAME                                   
         MVC   SVPAYER,BPPAYER     PAYER NAME                                   
         DROP  R6                                                               
*                                                                               
         L     R6,AIO              CAMP STATUS ELEM                             
         USING CSELEM,R6                                                        
         MVI   ELCODE,CSCODEQ                                                   
*                                                                               
DR20     BAS   RE,GETEL            ELEMENT SHOULD BE CREATED AT THIS PT         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         SR    R0,R0               PT TO BUY FIELD                              
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         MVI   8(R2),C'N'          DEFAULT BUY TO NO                            
         OC    CSBUY,CSBUY                                                      
         BZ    DR30                                                             
         MVI   8(R2),C'*'                                                       
*                                                                               
DR30     OI    6(R2),X'80'         TRANSMIT BUY                                 
         SR    R0,R0               PT TO MAT FIELD                              
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         MVI   8(R2),C' '          DEFAULT MAT TO NO                            
         OC    CSMAT,CSMAT                                                      
         BZ    DR40                                                             
         MVI   8(R2),C'*'                                                       
         EJECT                                                                  
*                                                                               
DR40     OI    6(R2),X'80'         TRANSMIT MAT                                 
         SR    R0,R0               PT TO PAY FIELD                              
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         MVI   8(R2),C' '          DEFAULT PAY TO NO                            
         OC    CSPAY,CSPAY                                                      
         BZ    DR50                                                             
         MVI   8(R2),C'*'                                                       
*                                                                               
DR50     OI    6(R2),X'80'         TRANSMIT PAY                                 
         SR    R0,R0               PT TO LOCK FIELD                             
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         MVI   8(R2),C'N'          DEFAULT LOCK TO NO                           
         OC    CSLOCK,CSLOCK                                                    
         BZ    DR60                                                             
         MVI   8(R2),C'*'                                                       
*                                                                               
DR60     OI    6(R2),X'80'         TRANSMIT LOCK                                
         SR    R0,R0               PT TO BUYER FIELD                            
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         MVC   8(12,R2),SVBUYER                                                 
         OC    8(12,R2),SPACES                                                  
         OI    6(R2),X'80'         TRANSMIT BUYER                               
*                                                                               
         SR    R0,R0               PT TO PAYER FIELD                            
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R2,R0                                                            
*                                                                               
         MVC   8(12,R2),SVPAYER                                                 
         OC    8(12,R2),SPACES                                                  
         OI    6(R2),X'80'         TRANSMIT PAYER                               
*                                                                               
DRX      B     EXIT                                                             
         DROP  R5                                                               
         EJECT                                                                  
*******************************************************                         
* UPON ENTRY - CHECK IF SYSTEM WAS SWITCHED           *                         
*              IF YES - THEN RESTORE SIGN ON SYSTEM   *                         
* READ MARKET RECORD AND RETURN MARKET NAME           *                         
*******************************************************                         
         SPACE 1                                                                
MNAME    NTR1                                                                   
         LA    R2,STTMKTH          DIFFERENCE BET 2 LINES                       
         LA    R7,STTMK2H                                                       
         SR    R7,R2                                                            
         CLI   CLTCC,C'Y'                                                       
         BNE   MKT10                                                            
*                                                                               
         L     RF,ACOMFACS         SWITCH BACK TO SPOT                          
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
*                                                                               
MKT10    L     R6,AIO                                                           
         USING MKTRECD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
*                                                                               
         ZIC   RE,5(R2)            CHANGE TO 4 DIGITS                           
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         OI    DUB+7,X'0F'                                                      
         UNPK  KEY+2(4),DUB        MKT NUM EBCDIC                               
         MVC   KEY+6(2),AGENCY                                                  
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'STATION',KEY,AIO                  
         CLC   KEY(15),0(R6)                                                    
         BNE   MKTERR                                                           
*                                                                               
         SR    R1,R1               POSITION R1 TO MKT NAME                      
         ICM   R1,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R1,R2                                                            
*                                                                               
         MVC   8(12,R1),MKTNAME    MARKET NAME                                  
         AR    R2,R7                                                            
         CLC   8(4,R2),SPACES                                                   
         BE    EXIT                                                             
         CLI   5(R2),0             LAST LINE DISPLAYED                          
         BNE   MKT10                                                            
*                                                                               
MKX      B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
* VALIDATE PRODUCT CODE IN COKE'S CLIENT HEADER                 *               
* SWITCH TO CCUSA FILE                                          *               
* READ CLIENT HEADER AND FIND PRODUCT CODE                      *               
* RETURN COKE'S AGYMED IN CCAGYMED                              *               
* RETURN PRODUCT CODE IN BPRD                                   *               
*****************************************************************               
         SPACE 1                                                                
VCCPRD   NTR1                                                                   
         XC    BPRD,BPRD                                                        
         CLC   TWAORIG,=AL2(797)        CCUSA                                   
         BE    VC10                                                             
*                                                                               
* SWITCH TO CCUSA'S FILE                                                        
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB(1),SAVSYS                                                   
*                                                                               
         GOTO1 (RF),DMCB                                                        
         CLI   4(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VC10     XC    KEY,KEY                                                          
         MVI   KEY,X'06'                                                        
         MVC   KEY+1(2),=C'CC'                                                  
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                 RECORD MUST EXIST                            
         DC    H'0'                                                             
*                                                                               
         MVC   AIO,AIO3                                                         
         L     R6,AIO                                                           
         GOTO1 GETREC              GET COKE AGENCY RECORD                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,24(R6)           LOCATION OF 1ST ELEMENT                      
*                                                                               
VC15     MVI   ELCODE,2                                                         
         BAS   RE,NEXTEL                                                        
         BNE   ERRMED                                                           
         CLC   2(1,R6),QMED                                                     
         BNE   VC15                                                             
         MVC   CCAGYMD,3(R6)                                                    
*                                                                               
VC20     XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING PRDHDR,R5                                                        
         MVC   PKEYAM,CCAGYMD                                                   
         MVC   PKEYCLT,BCLT        CLT = COKE                                   
         MVC   PKEYPRD,8(R2)       PRODUCT CODE                                 
         OC    PKEYPRD,SPACES                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   ERRPRD                                                           
         DROP  R5                                                               
*                                                                               
         MVC   AIO,AIO2                                                         
         L     R6,AIO                                                           
         USING PRDHDR,R6                                                        
         GOTO1 GETREC              GET COKE PRODUCT RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BPRD,PCODE+1        COKE'S PRD CODE                              
         MVC   QPRD,8(R2)                                                       
         OC    QPRD,SPACES                                                      
         CLC   TWAORIG,=AL2(797)        CCUSA                                   
         BE    VCX                                                              
*                                                                               
         L     RF,ACOMFACS         SWITCH BACK TO SPOT                          
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
*                                                                               
VCX      MVC   AIO,AIO1                                                         
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*****************************************************************               
*  TEST THE SCREEN TO SEE IF ANY FURTHER FIELDS HAVE BEEN INPUT *               
*  UPON ENTRY - R2 PTS TO A FLDHDR                              *               
*                                                               *               
*  UPON EXIT  - SWEMPTY = N IF NO MORE INPUT                    *               
*               SWEMPTY = Y IF THERE IS MORE INPUT              *               
*****************************************************************               
         SPACE 2                                                                
TSTFLDS  NTR1                                                                   
         MVI   SWEMPTY,C'Y'        DEFAULT TO NO INPUT                          
         LA    R3,STTLAST          LAST LOCATION ON SCREEN                      
*                                                                               
TF10     CR    R2,R3               END OF SCREEN                                
         BNL   TFX                                                              
*                                                                               
         CLI   5(R2),0             ANY INPUT                                    
         BNE   TF20                FIELD WITH INPUT                             
*                                                                               
         SR    R0,R0               NEXT FIELD                                   
         ICM   R0,1,0(R2)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AR    R2,R0                                                            
         B     TF10                                                             
*                                                                               
TF20     MVI   SWEMPTY,C'N'                                                     
*                                                                               
TFX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
MISSERR  MVI   ERROR,MISSING                                                    
         B     EXIT1                                                            
*                                                                               
MKTERR   MVC   AIO,AIO2            RESTORE IO AREA IF  ERROR                    
         MVI   ERROR,INVMKT                                                     
         B     EXIT1                                                            
*                                                                               
ERRSTA   MVI   ERROR,INVSTAT                                                    
         B     EXIT1                                                            
*                                                                               
ERRMED   MVI   ERROR,INVMED                                                     
         MVC   AIO,AIO1                                                         
         B     EXIT1                                                            
*                                                                               
ERRPRD   MVI   ERROR,INVPROD                                                    
         LA    R2,STTKPRH                                                       
         MVC   AIO,AIO1                                                         
         B     EXIT1                                                            
*                                                                               
ERRPRO   MVI   ERROR,NOPROF                                                     
         B     EXIT1                                                            
*                                                                               
ESTERR   MVI   ERROR,INVEST                                                     
         B     EXIT1                                                            
*                                                                               
ERRTYPE  MVI   ERROR,INVTYPE                                                    
         B     EXIT1                                                            
*                                                                               
EXIT2    OI    GENSTAT2,USGETTXT   USE GENCON MSGS                              
         LA    R1,GETTXTCB                                                      
         USING GETTXTD,R1                                                       
         MVI   GTMSGNO1,17                                                      
         MVI   GTMTYP,GTMINF       INFORMATION                                  
         MVI   GTMSYS,X'FF'        GENERAL MSG SYSTEM                           
         LA    R2,STTSATH                                                       
         DROP  R1                                                               
*                                                                               
EXIT1    L     RF,ACOMFACS         SWITCH BACK TO SPOT                          
         L     RF,CSWITCH-COMFACSD(RF)                                          
         XC    DMCB(8),DMCB                                                     
         GOTO1 (RF),DMCB,=C'SPOT',0                                             
         GOTO1 ERREX                                                            
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
         SPACE 2                                                                
         GETEL R6,DATADISP,ELCODE                                               
         SPACE 2                                                                
         LTORG                                                                  
*                                                                               
*        SET UP CALL FOR MSPACK/MSUNPK TO STAPACK                               
*                                                                               
SETMSP   NMOD1 0,*SETMSP*                                                       
         L     RC,0(R1)                                                         
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
         MVC   STAPACT,TACT                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,SVAPROF+7                                               
         MVC   STAPMED,QMED                                                     
         MVC   STAPQMKT,TMKT       SET MKT/STATION                              
         MVC   STAPQSTA,TSTA                                                    
         MVC   STAPQNET,TNET                                                    
         MVC   STAPMKST,TMKST                                                   
         MVC   STAPACOM,ACOMFACS                                                
         GOTO1 VSTAPACK,WORK                                                    
         MVC   TMKST,STAPMKST                                                   
         MVC   TMKT,STAPQMKT                                                    
         MVC   TSTA(8),STAPQSTA                                                 
         MVC   BYTE,STAPERR                                                     
         XIT1                                                                   
         DROP  R4                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE SPGENSTAT                                                      
         EJECT                                                                  
       ++INCLUDE SPSFMFFD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPSFMFCD                                                       
         EJECT                                                                  
*                                                                               
* DATA TO BE SAVED WITH SCREEN                                                  
*                                                                               
SVBUYER  DS    CL12                SAVE BUYER NAME                              
SVPAYER  DS    CL12                SAVE PAYER NAME                              
SVDATE   DS    XL2                 TODAY' COMPRESSED DATE                       
SAVSYS   DS    CL1                 SAVE SYSTEM TO FASWITCH                      
SAVAGY   DS    CL1                 AGENCY CODE                                  
CCAGYMD  DS    XL1                 COKE'S AGY MEDIA CODE                        
SAVPROF  DS    CL16                CLIENT PROFILE                               
KEY1     DS    XL32                                                             
*                                                                               
MYMKTST  DS    0XL5                BINARY MKT/STA                               
MYMKT    DS    XL2                 BINARY MARKET                                
MYSTA    DS    XL3                 BINARY STATION                               
XMKT     DS    CL4                 EBCDIC MKT                                   
XSTA     DS    CL5                 EBCDIC STATION                               
*                                                                               
KEYCHANG DS    CL1                 SWITCH - RECORD KEY                          
MYOVNUM  DS    XL1                 SWITCH - 1ST TIME THROUGH                    
MAINKEY  DS    XL8                 MAIN PORTION OF KEY                          
LCOUNT   DS    XL1                 LINE COUNTER                                 
SWCHANGE DC    C'N'                SWITCH - ELEMENT CHANGED                     
SWEMPTY  DC    C'N'                SWITCH - TWA INPUT STATUS                    
CLTCC    DS    CL1                 IS CLIENT = CC & CEXTRA+2 = *                
WORK1    DS    12D                 WORK AREA FOR DATAMGR                        
DA       DS    F                   DISK ADDRESS                                 
*                                                                               
TACT     DS    CL1                 ACTION                                       
TMKST    DS    XL5                 BINARY MKT/STA                               
TMKT     DS    CL4                 MARKET NUMBER                                
TSTA     DS    CL5                 STATION                                      
TNET     DS    CL3                 NETWORK                                      
*                                                                               
         ORG   STTWORK+250                                                      
IOAREA   DS    CL1000              I/O AREA TO READ CTFILE                      
*                                                                               
         EJECT                                                                  
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
PRDRECD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
         EJECT                                                                  
       ++INCLUDE SPSFMWORKD                                                     
         EJECT                                                                  
         ORG   SYSSPARE                                                         
*                                                                               
         PRINT OFF                                                              
         SPACE 1                                                                
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE FAGETTXTD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050SPSFM31   09/22/03'                                      
         END                                                                    
