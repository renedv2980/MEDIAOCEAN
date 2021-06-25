*          DATA SET NEBUY30S   AT LEVEL 009 AS OF 05/01/02                      
*          DATA SET NEBUY30    AT LEVEL 129 AS OF 09/28/99                      
*PHASE T31130C,+0                                                               
         TITLE 'NETPAK BUY PROGRAM - GENERAL UNIT DISPLAY - T31130'             
***********************************************************************         
* GENERAL UNIT DISPLAY RETURN UNIT RECORD FIELD VALUES IN DISPLAY     *         
* FORMAT - ASSUMES NETBLOCK POINTS TO AND DESCRIBES UNIT FOR DISPLAY  *         
*                                                                     *         
* P1     BYTE 0   = FIELD DATA TYPE (SEE EQUATED VALUES)              *         
*        BYTES 1-3 = A(GLOBAL WORKING STORAGE)                        *         
*                                                                     *         
* AFTER MODULE                                                        *         
*        FLDH+7 CONTAINS OUTPUT LENGTH                                *         
*        FLD CONTAINS OUTPUT DATA IN SPACE FILLED FIELD               *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
T31130   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**UDIS**,RR=R2                                                 
         USING T31130,RB,R5                                                     
         LA    R5,2048(RB)                                                      
         LA    R5,2048(R5)                                                      
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         LA    R9,0(R9)            CLEAR H.O.B.                                 
         MVC   BYTE,0(R1)          EXTRACT DATA REQUESTED BYTE                  
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
         L     R7,ABUYVALS         R7 POINTS TO GLOBAL BUY VALUES               
         USING BUYVALD,R7                                                       
         L     RA,ATWA             ADDRESS THE TWA FOR ACTION                   
         USING TWAD,RA                                                          
         XC    FLDH,FLDH           PRE-CLEAR FIELD HEADER AND FIELD             
         MVC   FLD,SPACES                                                       
         SPACE                                                                  
DIS      LA    R0,TYPES            COUNTER                                      
         LA    RE,TYPTAB                                                        
         CLC   BYTE,0(RE)          COMPARE FOR DATA TYPE                        
         BE    DIS2                                                             
         LA    RE,L'TYPTAB(RE)                                                  
         BCT   R0,*-14                                                          
         DC    H'0'                BLOW UP FOR UNRECOGNIZED DATA TYPE           
         SPACE                                                                  
DIS2     SR    RF,RF                                                            
         ICM   RF,7,1(RE)          GET ADDRESS OF DISPLAY ROUTINE               
         AR    RF,R2               RELOCATE IT                                  
         BASR  RE,RF                                                            
         LA    R1,L'FLD            FIND THE OUTPUT DATA LENGTH                  
         LA    RE,FLD-1(R1)                                                     
         CLI   0(RE),C' '          TEST FOR FIRST CHARACTER GREATER             
         BH    *+10                THAN A BLANK OR BINARY ZERO                  
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         STC   R1,FLDH+7                                                        
         B     EXXMOD                                                           
         EJECT                                                                  
* DISPLAY PRODUCT CODES                                                         
*                                                                               
DISPRD   LR    R4,RE                                                            
         CLI   ACTION,DM           TEST FOR DISPLAY MULTIPLE                    
         BE    DISPR20             NO-DO NOT FORCE OUT ZERO                     
         CLI   ACTION,CM           TEST FOR FORCED DIS MULTIPLE                 
         BE    DISPR20                                                          
*                                                                               
         MVI   ELCODE,X'14'                                                     
         BAS   R8,GETEL            TOO MANY PRODUCTS TO DISPLAY                 
         BNE   DISPR20                                                          
         MVC   FLD(8),=8C'*'                                                    
         BR    R4                                                               
DISPR20  CLI   NBPRD,0             TEST IF ANY PRODUCT ALLOCATED                
         BE    DISPRDX                                                          
         LA    R2,FLD              R2 POINTS TO OUTPUT                          
         TM    NBUNST2,X'20'       TEST FOR FROZEN PROD. ALLOCATION             
         BZ    *+12                NO                                           
         MVI   0(R2),DASH          PREFIX FIELD WITH A DASH                     
         LA    R2,1(R2)            INCREMENT OUTPUT POINTER                     
         LA    R1,NBPRD                                                         
         BAS   R8,GETPRD                                                        
         LA    R2,2(R2)                                                         
         CLI   0(R2),C' '                                                       
         BE    *+8                                                              
         LA    R2,1(R2)                                                         
         CLI   NBPRD2,0                                                         
         BE    DISPRDX                                                          
         LR    R3,R2               SAVE POSITION OF COMMA                       
         LA    R2,1(R2)            POSITION R2 FOR START OF PROD. CODE          
         LA    R1,NBPRD2                                                        
         BAS   R8,GETPRD                                                        
         MVI   0(R3),C'*'          SEPARATE PRODUCT CODES WITH A STAR           
         SPACE                                                                  
DISPRDX  BR    R4                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO GET PRODUCT CODE FROM CLIENT LIST                              
*                                                                               
GETPRD   LA    R0,220                                                           
         LA    RF,CLILIST                                                       
         CLC   0(1,R1),3(RF)       TEST FOR PRODUCT NUMBER                      
         BE    *+14                                                             
         LA    RF,4(RF)                                                         
         BCT   R0,*-14                                                          
         DC    H'0'                                                             
*                                                                               
         MVC   0(3,R2),0(RF)       EXTRACT PRODUCT CODE                         
         BR    R8                  RETURN NO CALLER                             
         SPACE 2                                                                
* DISPLAY FIRST PRODUCT SHARE                                                   
*                                                                               
DISP1SHR LR    R4,RE                                                            
         CLI   ACTION,DM           TEST FOR DISPLAY MULTIPLE                    
         BE    DSP1S20             NO-DO NOT FORCE OUT ZERO                     
         CLI   ACTION,CM           TEST FOR FORCED DIS MULTIPLE                 
         BE    DSP1S20                                                          
*                                                                               
         MVI   ELCODE,X'14'                                                     
         BAS   R8,GETEL            TOO MANY PERCENTS TO DISPLAY                 
         BNE   *+12                                                             
         MVC   FLD(6),=6C'*'                                                    
         BR    R4                                                               
*                                                                               
DSP1S20  SR    R2,R2                                                            
         ICM   R2,3,NBP1SHR        TEST FOR FIRST PRODUCT SHARE                 
         BNZ   *+10                ZERO-EXIT                                    
         CLI   NBPRD2,0                                                         
         BER   R4                                                               
         BAS   R8,EDTTWO           EDIT TO TWO DECIMAL PLACES                   
         BR    R4                                                               
         SPACE 2                                                                
* DISPLAY PROGRAM NAME                                                          
*                                                                               
DISPRGN  MVC   FLD(L'NBPROGNM),NBPROGNM                                         
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY SECONDS LENGTH                                                        
*                                                                               
DISLEN   LR    R4,RE                                                            
         SR    R2,R2                                                            
         ICM   R2,1,NBLEN1                                                      
         BNZ   *+8                                                              
         IC    R2,NBLEN                                                         
         LA    R3,FLD              R3=OUTPUT POINTER                            
         MVI   ELCODE,X'02'                                                     
         BAS   R8,GETEL            SUBSIDIARY 01 ELEMENT                        
         BNE   DISL02                                                           
         L     RE,12(R1)                                                        
         TM    2(RE),X'40'         COPY-SPLIT BIT SET                           
         BZ    DISL02                                                           
         MVI   0(R3),C'C'          YES                                          
         LA    R3,1(R3)                                                         
DISL02   TM    NBUNST2,X'10'       TEST FOR FROZEN LENGTH                       
         BZ    *+12                                                             
         MVI   0(R3),DASH          YES                                          
         LA    R3,1(R3)                                                         
         BAS   R8,EDTLEN                                                        
         SR    RF,RF                                                            
         ICM   RF,1,NBLEN1                                                      
         BZ    DISLENX                                                          
         ZIC   R2,NBLEN                                                         
         SR    R2,RF                                                            
         BM    DISLENX                                                          
         AR    R3,R0                                                            
         MVI   0(R3),C'*'                                                       
         LA    R3,1(R3)                                                         
         BAS   R8,EDTLEN                                                        
DISLENX  BR    R4                                                               
         SPACE 2                                                                
* DISPLAY DAY OF UNIT                                                           
*                                                                               
DISDAY   MVC   FLD(L'NBDAYNAM),NBDAYNAM                                         
         CLC   NBDAYNAM(3),=CL3'M-S'                                            
         BNE   *+8                                                              
         MVI   FLD+3,C'U'                                                       
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY UNIT TIME                                                             
*                                                                               
DISTIME  ST    RE,FULL                                                          
         GOTO1 VUNTIME,DMCB2,NBTIME,FLD                                         
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY AFFIDAVIT TIME                                                        
*                                                                               
DISAFFID OC    NBAFFTIM,NBAFFTIM                                                
         BZR   RE                                                               
         LR    R0,RE               SAVE RETURN POINT                            
         XC    FULL,FULL                                                        
         MVC   FULL(2),NBAFFTIM                                                 
         GOTO1 VUNTIME,DMCB2,FULL,FLD                                           
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY ASSIGNED COST                                                         
*                                                                               
DISASS   ICM   R2,15,NBASSIGN                                                   
         BNZ   DISASS2             NON-ZERO                                     
         TM    NBUNITST,X'08'      ZERO-TEST FOR ASSIGNED OVERRIDE              
         BZR   RE                  NO                                           
         MVI   FLD,C'0'            YES-FORCE OUT ZERO                           
         BR    RE                                                               
         SPACE                                                                  
DISASS2  BAS   R8,EDTMIN                                                        
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY ACTUAL COST                                                           
*                                                                               
DISACT   ICM   R2,15,NBACTUAL                                                   
         BNZ   DISACT2             NON-ZERO VALUE                               
         TM    NBUNITST,X'20'      ITS ZERO-TEST FOR COST OVERRIDE              
         BZ    DISACT3                                                          
         MVI   FLD,C'0'            FORCE A ZERO TO OUTPUT                       
         B     DISACT3                                                          
         SPACE                                                                  
DISACT2  BAS   R8,EDTMIN                                                        
DISACT3  CLI   NBRTTYPE,0                                                       
         BER   RE                                                               
         LA    R8,FLD                                                           
         MVC   DUB(8),0(R8)                                                     
         MVC   0(1,R8),NBRTTYPE    MOVE RATE TYPE                               
         LA    R8,1(R8)                                                         
         CLI   NBSDRTCV,0          IS THERE COVERAGE                            
         BE    *+14                NO                                           
         MVC   0(1,R8),NBSDRTCV                                                 
         LA    R8,1(R8)                                                         
         MVC   0(8,R8),DUB                                                      
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY INTEGRATION RATE                                                      
*                                                                               
DISINT   ICM   R2,15,NBINTEG                                                    
         BZ    DISI100                                                          
         BAS   R8,EDTMIN                                                        
         BR    RE                                                               
         SPACE                                                                  
DISI100  TM    NBUNST4,X'80'       ZERO INPUTTED                                
         BO    DISI200                                                          
*                                                                               
         L     R2,APACKREC                                                      
         USING NPRECD,R2                                                        
         TM    NPAKCNTL,X'80'                                                   
         BNO   *+10                                                             
DISI200  MVC   FLD(1),=C'0'                                                     
         BR    RE                                                               
         DROP  R2                                                               
         SPACE 2                                                                
* DISPLAY UNIT STATUS                                                           
*                                                                               
DISSTAT  MVC   FLD(3),=C'EST'                                                   
         TM    NBUNITST,X'04'                                                   
         BZ    *+10                                                             
         MVC   FLD(3),=C'PFB'                                                   
*                                                                               
         TM    NBUNST3,X'02'                                                    
         BZ    *+10                                                             
         MVC   FLD(3),=C'ADU'                                                   
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY FEED                                                                  
*                                                                               
DISFEED  LR    R3,RE                                                            
         CLI   ACTION,DM           TEST FOR DISPLAY MULTIPLE                    
         BE    DISFEED1            NO-DO NOT FORCE OUT ZERO                     
         CLI   ACTION,CM           TEST FOR FORCED DIS MULTIPLE                 
         BE    DISFEED1                                                         
*                                                                               
         MVI   ELCODE,X'14'                                                     
         BAS   R8,GETEL            TOO MANY FEEDS TO DISPLAY                    
         BNE   *+14                                                             
         MVC   FLD(7),=7C'*'                                                    
         B     DISFEEDX                                                         
*                                                                               
DISFEED1 SR    R2,R2                                                            
         ICM   R2,3,NBFEED                                                      
         BZ    DISFEEDX                                                         
         BAS   R8,EDTTWO                                                        
         MVI   ELCODE,X'02'                                                     
         BAS   R8,GETEL            SUBSIDIARY 01 ELEMENT                        
         BNE   DISFEEDX                                                         
         L     RF,12(R1)                                                        
         TM    2(RF),X'01'         FEED AFFECTS RATINGS                         
         BZ    DISFEEDX                                                         
         SPACE                                                                  
         LA    R1,FLD                                                           
DISFEED2 CLI   0(R1),X'40'         FIND LAST POSITION ON FEED LINE              
         BNH   DISFEED3                                                         
         LA    R1,1(R1)                                                         
         B     DISFEED2                                                         
DISFEED3 MVI   0(R1),C'R'          SET FEED AFFECTS RATING INDICATOR            
*                                                                               
DISFEEDX LR    RE,R3                                                            
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY IMPACT PERCENTAGE                                                     
*                                                                               
DISIMP   SR    R2,R2                                                            
         ICM   R2,3,NBIMPACT                                                    
         BZR   RE                                                               
         BAS   R8,EDTTWO                                                        
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY NTI CODE                                                              
*                                                                               
DISNTI   OC    NBNTI,NBNTI                                                      
         BZR   RE                                                               
         ST    RE,FULL                                                          
         SR    R0,R0                                                            
         ICM   R0,3,NBNTI                                                       
         EDIT  (R0),(5,FLD),ALIGN=LEFT,FILL=0                                   
         L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY NSI CODE                                                              
*                                                                               
DISNSI   OC    NBNSI,NBNSI                                                      
         BZR   RE                                                               
         LA    R3,FLD                                                           
         TM    NBNSI,X'80'                                                      
         BNO   *+12                                                             
         MVI   0(R3),C'T'          PUT OUT PREFIX OF 'T'                        
         LA    R3,1(R3)                                                         
         MVC   HALF,NBNSI                                                       
         NI    HALF,X'7F'                                                       
         LH    R0,HALF                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(4,R3),DUB                                                      
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY UNIVERSE CODE                                                         
*                                                                               
DISUCOD  OC    NBUNCODE,NBUNCODE                                                
         BZR   RE                                                               
         LA    R2,NBUNCODE                                                      
         BAS   R8,EDTPWO                                                        
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY BILLBOARD INDICATOR                                                   
*                                                                               
DISBB    LR    R2,RE                                                            
         MVI   ELCODE,X'21'                                                     
         BAS   R8,GETEL                                                         
         BNE   DISBBX              NO FEED CODE ELEMENT                         
         L     RE,12(R1)                                                        
         TM    NUCMLFLG-NUCMLEL(RE),X'04'                                       
         BZ    DISBBX                                                           
         MVC   FLD(2),=C'BB'                                                    
DISBBX   BR    R2                                                               
         SPACE 2                                                                
* DISPLAY UNIVERSE PERCENTAGE                                                   
*                                                                               
DISUNPC  SR    R2,R2                                                            
         ICM   R2,3,NBUNIV                                                      
         BZR   RE                                                               
         BAS   R8,EDTTWO                                                        
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY FEED CODE (TRAFFIC)                                                   
*                                                                               
DISFCD   LR    R2,RE               SAVE RETURN POINT                            
         MVI   ELCODE,X'22'                                                     
         BAS   R8,GETEL                                                         
         BNE   FC100               NO FEED CODE ELEMENT                         
         L     R4,12(R1)                                                        
         USING NUFEDEL,R4                                                       
         MVC   FLD(L'NUFEEDCD),NUFEEDCD                                         
*                                                                               
DISFCDX  LR    RE,R2                                                            
         BR    RE                                                               
         DROP  R4                                                               
         SPACE 2                                                                
* DISPLAY MULTILPY MEDIA FEED CODES (TRAFFIC)                                   
FC100    L     RE,NBAIO                                                         
         LA    RE,27(RE)                                                        
         MVC   FLD,SPACES                                                       
         SR    R3,R3               FIRST TIME SWITCH                            
         LA    R4,FLD                                                           
*                                                                               
         LTR   R3,R3               PUT NATIONAL IN 1ST POSTION                  
         BNZ   FC120                                                            
         TM    NBUNST3,X'40'       CHECK FOR COPY SPLIT BUY                     
         BZ    FC120                                                            
FC110    ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    DISFCDX                                                          
         CLI   0(RE),X'21'         FIND MEDIA 21 ELEM (NOT DELETED)             
         BL    FC110                                                            
         BH    FC120                                                            
         CLC   NUCMLPRD-NUCMLEL(1,RE),FULL                                      
         BNE   FC120               INPUTTED PROD TO ELEMENT PRODUCT             
         CLI   NUCMPPOS-NUCMLEL(RE),0                                           
         BE    FC115               NO POSITION NUMBER                           
         CLC   NUCMPPOS-NUCMLEL(1,RE),FULL+1                                    
         BNE   FC120               COMPARE POSITION NUMBER                      
*                                                                               
FC115    TM    NUCMLFL2-NUCMLEL(RE),X'01'   NO NATIONAL                         
         BZ    *+14                INPUTTED PROD TO ELEMENT PRODUCT             
         MVC   0(2,R4),=C'**'                                                   
         B     *+10                                                             
         MVC   0(2,R4),=C'*N'                                                   
         LA    R4,2(R4)                                                         
         LR    R3,R2                                                            
*                                                                               
FC120    ZIC   RF,1(RE)                                                         
         AR    RE,RF                                                            
         CLI   0(RE),0                                                          
         BE    DISFCDX                                                          
         CLI   0(RE),X'23'         FIND MEDIA 23 ELEM (NOT DELETED)             
         BNE   FC120                                                            
         TM    NUFDCFL2-NUFDCEL(RE),X'80'                                       
         BNZ   FC120                                                            
         TM    NBUNST3,X'40'       CHECK FOR COPY SPLIT BUY                     
         BNZ   FC130                                                            
         LTR   R3,R3               PUT NATIONAL IN 1ST POSTION                  
         BNZ   FC135                                                            
         MVC   0(2,R4),=C'*N'                                                   
         LA    R4,2(R4)                                                         
         LR    R3,R2                                                            
         B     FC135                                                            
FC130    CLC   NUFDCPRD-NUFDCEL(1,RE),FULL                                      
         BNE   FC120               INPUTTED PROD TO ELEMENT PRODUCT             
         CLI   NUFDPPOS-NUFDCEL(RE),0                                           
         BE    FC135               NO POSITION NUMBER                           
         CLC   NUFDPPOS-NUFDCEL(1,RE),FULL+1                                    
         BNE   FC120               CHECK POSITION MATCH                         
*                                                                               
FC135    LTR   R3,R3               PUT NATIONAL IN 1ST POSTION                  
         BZ    FC150                                                            
         LA    R0,4                                                             
FC140    CLI   0(R4),C' '                                                       
         BE    *+12                                                             
         LA    R4,1(R4)                                                         
         BCT   R0,FC140                                                         
         MVI   0(R4),C'/'                                                       
         LA    R4,1(R4)                                                         
FC150    MVC   0((L'NUFDCFED),R4),NUFDCFED-NUFDCEL(RE)                          
         LR    R3,R2                                                            
         B     FC120                                                            
*                                                                               
         SPACE 2                                                                
* DISPLAY FEED MARKET GROUP                                                     
*                                                                               
*ISFMG   OC    NBFEEDMG,NBFEEDMG                                                
*        BZR   RE                                                               
*        MVC   FLD(1),NBFEEDMG                                                  
*        UNPK  DUB(3),NBFEEDMG+1(2)                                             
*        MVC   FLD+1(2),DUB                                                     
*        BR    RE                                                               
*        SPACE 2                                                                
* DISPLAY ROTATION                                                              
*                                                                               
DISROT   LR    R2,RE                                                            
         MVI   ELCODE,X'02'                                                     
         BAS   R8,GETEL            SUBSIDIARY 01 ELEMENT                        
         BNE   DISROTX                                                          
         USING NUSDRD,R3                                                        
         L     R3,12(R1)                                                        
         CLI   NUSDROT,0                                                        
         BE    DISROTX                                                          
         GOTO1 VUNDAY,DMCB,NUSDROT,FLD                                          
*                                                                               
DISROTX  LR    RE,R2                                                            
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY HUT AVERAGE                                                           
*                                                                               
DISHAVE  CLI   NBHUTAVE,0                                                       
         BE    *+10                                                             
         MVC   FLD(1),NBHUTAVE                                                  
         MVC   FLD+1(1),NBHUTFLR                                                
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY HUT SCHEME                                                            
*                                                                               
DISHSC   CLI   NBHUTSCM,0                                                       
         BE    *+10                                                             
         MVC   FLD(1),NBHUTSCM                                                  
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY HUT ADJUSTMENT PERCENTAGE                                             
*                                                                               
DISHADJ  SR    R2,R2                                                            
         ICM   R2,3,NBHUTPCT                                                    
         BZR   RE                                                               
         BAS   R8,EDTTWO                                                        
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY SPECIAL REP                                                           
*                                                                               
DISREP   SR    R2,R2                                                            
         ICM   R2,3,NBSREP                                                      
         BZR   RE                  NO REP                                       
         CVD   R2,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FLD(3),DUB                                                       
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY PRE-EMPTION                                                           
*                                                                               
DISPRE   TM    NBUNITST,X'40'      TEST IF PRE-EMPTED                           
         BZ    *+8                                                              
         MVI   FLD,YES                                                          
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY SPECIAL CHARGE INDICATOR                                              
*                                                                               
DISSCHG  LR    R0,RE                                                            
         MVI   ELCODE,X'03'        GET ACTIVITY ELEMENT                         
         BAS   R8,GETEL                                                         
         BNE   *+10                                                             
         MVC   FLD(7),=CL7'CHARGES'                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY MAKE-GOOD PROGRAM CODE                                                
*                                                                               
DISMGPC  OC    NBMGFPCD,NBMGFPCD                                                
         BZ    *+10                                                             
         MVC   FLD(L'NBMGFPCD),NBMGFPCD                                         
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY MAKE-GOOD FOR UNIT DATE                                               
*                                                                               
DISMGDAT OC    NBMGFDAT,NBMGFDAT                                                
         BZR   RE                                                               
         ST    RE,FULL                                                          
         GOTO1 VDATCON,DMCB2,(2,NBMGFDAT),(4,FLD)                               
         CLI   NBMGFSUB,0          TEST FOR SUB-LINE                            
         BE    DISMGDTX            NO-EXIT                                      
         ZIC   R3,NBMGFSUB                                                      
         EDIT  (R3),(2,FLD+6),ALIGN=LEFT                                        
         MVI   FLD+5,C'-'          INSERT A DASH BEFORE SUB-LINE                
         SPACE 1                                                                
DISMGDTX L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY MADE-GOOD BY REFERENCE FOR MISSING UNIT                               
*                                                                               
DISMISS  OC    NBMGBPCD,NBMGBPCD   TEST IF MISSING DATA                         
         BZR   RE                                                               
         ST    RE,FULL                                                          
         MVC   FLD(8),=C'*MISSED*'                                              
         MVC   FLD+13(12),=C'MADE-GOOD BY'                                      
         MVC   FLD+26(L'NBMGBPCD),NBMGBPCD                                      
         GOTO1 VDATCON,DMCB,(2,NBMGBDAT),(4,FLD+33)                             
         CLI   NBMGBSUB,0                                                       
         BE    DISMISSX                                                         
         CLI   NBMGBSUB,1                                                       
         BE    DISMISSX                                                         
         ZIC   R2,NBMGBSUB                                                      
         MVI   FLD+38,DASH                                                      
         EDIT  (R2),(3,FLD+39),ALIGN=LEFT                                       
         SPACE                                                                  
DISMISSX L     RE,FULL                                                          
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY HUT VALUE                                                             
*                                                                               
DISHUT   SR    R2,R2                                                            
         ICM   R2,3,NBESTHUT                                                    
         BNZ   DISHUT2                                                          
         CLI   ACTION,DM           TEST FOR DISPLAY MULTIPLE                    
         BE    DISHUT2             NO-DO NOT FORCE OUT ZERO                     
         CLI   ACTION,CM           TEST FOR FORCED DIS MULTIPLE                 
         BNER  RE                                                               
         SPACE                                                                  
DISHUT2  LR    R3,RE               SAVE RETURN POINT                            
*                                                                               
*--CHECK PRECISION FACTOR FOR RATING                                            
         TM    DEMPREC+5,X'82'                                                  
         BO    DISHUT25                                                         
*                                                                               
         BAS   R8,EDTONE                                                        
         B     *+8                                                              
*                                                                               
DISHUT25 BAS   R8,EDTBWO           TWO DEC. PLACE EDIT                          
         XC    DUB,DUB                                                          
         MVC   DUB+2(2),HUT                                                     
         BAS   R8,DEMOVER                                                       
         LR    RE,R3               RESTORE RETURN POINT                         
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY SHARE                                                                 
*                                                                               
DISSHR   SR    R2,R2                                                            
         ICM   R2,3,NBESTSHR                                                    
         BNZ   DISSHR2                                                          
         CLI   ACTION,DM                                                        
         BE    DISSHR2                                                          
         CLI   ACTION,CM                                                        
         BNER  RE                                                               
         SPACE                                                                  
DISSHR2  LR    R3,RE               SAVE RETURN POINT                            
         BAS   R8,EDTONE                                                        
         XC    DUB,DUB                                                          
         MVC   DUB+2(2),SHARE                                                   
         BAS   R8,DEMOVER                                                       
         LR    RE,R3               RESTORE RETURN POINT                         
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY HOMES RATING                                                          
*                                                                               
DISRAT   SR    R2,R2                                                            
         ICM   R2,3,NBESTHOM+2                                                  
         BNZ   DISRAT2                                                          
         CLI   ACTION,DM                                                        
         BE    DISRAT2                                                          
         CLI   ACTION,CM                                                        
         BNER  RE                                                               
         SPACE                                                                  
DISRAT2  LR    R3,RE               SAVE RETURN POINT                            
*                                                                               
*--CHECK PRECISION FACTOR FOR RATING                                            
         TM    DEMPREC+1,X'82'                                                  
         BO    DISRAT25                                                         
*                                                                               
         BAS   R8,EDTONE                                                        
         B     *+8                                                              
*                                                                               
DISRAT25 BAS   R8,EDTBWO           TWO DEC PLACE EDIT                           
         XC    DUB,DUB                                                          
         MVC   DUB+2(2),RATING                                                  
         BAS   R8,DEMOVER                                                       
         LR    RE,R3               RESTORE RETURN POINT                         
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY PACKAGE GUARANTEE FACTOR                                              
*                                                                               
*   NEED TO GET ELEM/ NBDEMRAW=Y(IN'35') SO NBNGUFAC IS NOT SET                 
*   BY NETVALUE                                                                 
DISPG    DS    0H                                                               
         LR    R4,RE                                                            
         GOTO1 VHELLO,DMCB2,(C'G',UNTFILE),(X'B3',NBAIO),0                      
         CLI   12(R1),0            TEST IF PKG GUARANTEE FOUND                  
         BNER  R4                  NO                                           
         L     R3,12(R1)                                                        
         ICM   R2,15,2(R3)          GET GUARANTEE FACTOR                        
         EDIT  (R2),(10,FLD),4,ALIGN=LEFT                                       
         BR    R4                                                               
*                                                                               
*******  SR    R2,R2                                                            
******** ICM   R2,15,NBNGUFAC       GET GUARANTEE FACTOR                        
***      ICM   R2,3,NBGUAFAC       GET GUARANTEE FACTOR                         
******   BZR   RE                  EXIT IF ZERO                                 
*******  LR    R3,RE                                                            
*******  BAS   R8,EDTFOUR                                                       
***      BAS   R8,EDTTWO                                                        
******** LR    RE,R3                                                            
*******  BR    RE                                                               
         SPACE 2                                                                
* DISPLAY DEMO GUARANTEE FACTOR                                                 
*                                                                               
DISDG    STCM  RE,15,REGESAV       SAVE RETURN POINT                            
         GOTO1 VHELLO,DMCB2,(C'G',UNTFILE),(X'B4',NBAIO),0                      
         CLI   12(R1),0            TEST IF DEMO GUARANTEE FOUND                 
         BNE   DPXIT               NO                                           
         L     R3,12(R1)                                                        
         USING NUNDGD,R3                                                        
         XC    THREE,THREE                                                      
         MVI   THREE+1,C'T'                                                     
         MVC   THREE+2(1),NUNDGDEM+2  CATEGORY                                  
***      MVC   THREE+2(1),NUGUACAT  CATEGORY                                    
         LA    R4,DBLOCKA                                                       
         USING DBLOCKD,R4                                                       
         XC    DBLOCK,DBLOCK                                                    
         MVC   DBFILE,=C'NTI'                                                   
         MVI   DBSELMED,C'N'                                                    
         MVC   DBCOMFCS,ACOMFACS                                                
         GOTO1 VDEMOCON,DMCB2,THREE,(7,WORK),(C'S',DBLOCK),ESTUSNS              
         LA    R2,FLD                                                           
         MVC   0(7,R2),WORK        EXTRACT CATEGORY NAME                        
*                                                                               
         LA    R1,6(R2)                                                         
         LA    R0,7                                                             
         CLI   0(R1),C' '          FIND LAST SIGNIFICANT CHARACTER              
         BH    *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,*-10                                                          
         LA    R2,1(R1)            POINT PAST LAST SIGNIFICANT CHAR.            
         MVI   0(R2),C'/'                                                       
         SR    R0,R0                                                            
***      ICM   R0,3,NUGUAFAC                                                    
         ICM   R0,15,NUNDGFAC                                                   
         BZ    DPXIT                                                            
         EDIT  (R0),(8,1(R2)),4,ALIGN=LEFT                                      
DPXIT    ICM   RE,15,REGESAV                                                    
         BR    RE                                                               
         DROP  R3,R4                                                            
         SPACE 2                                                                
* DISPLAY PACKAGE FILTER                                                        
*                                                                               
DISPFIL  LR    R2,RE               SAVE RETURN POINT                            
         GOTO1 VHELLO,DMCB2,(C'G',UNTFILE),(X'08',NBAIO),(1,=C'K')              
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BNE   DISPFILX            NO PACKAGE FILTER ELEMENT                    
         L     R4,12(R1)                                                        
         USING NUFILD,R4                                                        
         MVC   FLD(L'NUFILTER),NUFILTER                                         
*                                                                               
DISPFILX LR    RE,R2                                                            
         BR    RE                                                               
         DROP  R4                                                               
         EJECT                                                                  
* DISPLAY RESULT CODE                                                           
*                                                                               
DISRES   XC    FLD,FLD             CLEAR PROTECTED FIELD                        
         CLI   NBRESULT,0                                                       
         BE    *+10                                                             
         MVC   FLD(1),NBRESULT                                                  
         BR    RE                                                               
         SPACE 2                                                                
* DISPLAY BUY CREATION DATE                                                     
*                                                                               
DISBUYD  LR    R0,RE               SAVE RETURN POINT                            
         XC    FLD,FLD             PROTECTED FIELD SHOULD BE ZEROED             
         MVI   ELCODE,X'99'        GET ACTIVITY ELEMENT                         
         BAS   R8,GETEL                                                         
         L     R4,12(R1)                                                        
         USING NUACTD,R4                                                        
         OC    NUACTADT,NUACTADT   TEST FOR BUY CREATION DATE                   
         BZ    DISBUY10            NONE                                         
         GOTO1 VDATCON,(R1),(3,NUACTADT),(5,FLD)                                
         SPACE 1                                                                
DISBUY10 TM    NBUNST3,X'10'       CHECK FOR PUP CREATED BUY                    
         BZ    *+8                                                              
         MVI   FLD+8,C'P'                                                       
         TM    NBUNST3,X'04'       CHECK FOR CABLE UPLOAD BUY                   
         BZ    *+8                                                              
         MVI   FLD+8,C'C'                                                       
         TM    NBUNST4,X'40'       CHECK FOR UTILS SPLIT BUY                    
         BZ    *+8                                                              
         MVI   FLD+8,C'S'                                                       
DISBUYDX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R4                                                               
         SPACE 2                                                                
* DISPLAY LAST ACTIVITY DATE                                                    
*                                                                               
DISLSTA  LR    R0,RE               SAVE RETURN POINT                            
         XC    FLD,FLD             PROTECTED FIELD SHOULD BE ZEROED             
         MVI   ELCODE,X'99'        GET ACTIVITY ELEMENT                         
         BAS   R8,GETEL                                                         
         L     R4,12(R1)                                                        
         USING NUACTD,R4                                                        
         OC    NUACTCDT,NUACTCDT   TEST FOR LAST ACTIVITY DATE                  
         BZ    DISLSTAX            NONE                                         
         GOTO1 VDATCON,(R1),(3,NUACTCDT),(5,FLD)                                
         SPACE 1                                                                
DISLSTAX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R4                                                               
         SPACE 2                                                                
* DISPLAY "OTHER" FIELD                                                         
*                                                                               
* NOTE R3 PASSED FROM DISPLAY MULTIPLE ROUTINE DO NOT USE                       
DISOTH   LR    R0,RE               SAVE RETURN POINT                            
         XC    FLD,FLD             PROTECTED FIELD SHOULD BE ZEROED             
         MVI   ELCODE,X'60'        GET ACTIVITY ELEMENT                         
         BAS   R8,GETEL                                                         
         BNE   DISOTHEX                                                         
         LA    R4,FLD                                                           
         L     R2,12(R1)                                                        
         USING NUOTH,R2                                                         
DISOT020 LA    R8,OTHTAB                                                        
*                                                                               
DISOT050 CLI   0(R8),X'FF'                                                      
         BE    DISOT100                                                         
         CLC   NUOTTYP,0(R8)       CHECK FOR CODE MATCH                         
         BNE   DISOT060            NOT =                                        
         CLI   ACTION,DM           TEST FOR DISPLAY MULTIPLE                    
         BE    *+12                NO-DO NOT FORCE OUT ZERO                     
         CLI   ACTION,CM           TEST FOR CHANGE MULTIPLE                     
         BNE   DISOT070            NO-DO NOT FORCE OUT ZERO                     
         CLC   0(2,R3),=C'OT'      IS OTHER REQUESTED                           
         BNE   DISOT055                                                         
         CLC   27(2,R3),1(R8)      CHECK OFF SECONDARY VALUE                    
         BE    DISOT070                                                         
         B     DISOT060                                                         
DISOT055 CLC   0(2,R3),1(R8)       IF MULTIPLE DISPLAY ONLY                     
         BE    DISOT070            SINGLE ELEMENT REQUESTED                     
DISOT060 LA    R8,10(R8)                                                        
         B     DISOT050                                                         
*                                                                               
DISOT070 ZIC   RF,9(R8)            MOVE TITLE OUT                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),1(R8)                                                    
         LA    RF,1(RF)                                                         
         LA    R4,0(RF,R4)                                                      
*--SPECIAL SETUP FOR DAYPART MOVE                                               
         CLI   0(R8),C'D'                                                       
         BNE   DISOT072                                                         
         ZIC   RF,NUOTLEN          MOVE DATA OUT                                
         LA    RE,5                                                             
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     DISOT080                                                         
         MVC   0(0,R4),NUOTHER+1                                                
*                                                                               
*--SPECIAL SETUP FOR WINDOW DATE                                                
DISOT072 CLI   0(R8),C'W'                                                       
         BNE   DISOT073                                                         
         GOTO1 VDATCON,DMCB2,(2,NUOTHER),(5,(R4))                               
         LA    RF,8                OUTPUT LENGTH                                
         B     DISOT080                                                         
*                                                                               
*--SPECIAL SETUP FOR BOOK DATE                                                  
DISOT073 CLI   0(R8),C'J'                                                       
         BE    *+12                                                             
         CLI   0(R8),C'K'                                                       
         BNE   DISOT074                                                         
         GOTO1 VDATCON,DMCB2,(2,NUOTHER+2),(5,(R4))                             
         LA    RF,8                OUTPUT LENGTH                                
         B     DISOT080                                                         
*                                                                               
*--SPECIAL SETUP FOR MULTI-RUN                                                  
DISOT074 CLI   0(R8),C'L'                                                       
         BNE   DISOT075                                                         
         LR    R8,R0                                                            
         EDIT  (1,NUOTHER),(2,(R4)),ALIGN=LEFT                                  
         LR    R0,R8                                                            
         LA    RF,1                                                             
         CLI   NUOTHER,10                                                       
         BL    *+8                                                              
         LA    RF,2                OUTPUT LENGTH                                
         B     DISOT080                                                         
*                                                                               
DISOT075 ZIC   RF,NUOTLEN          MOVE DATA OUT                                
         LA    RE,4                                                             
         SR    RF,RE                                                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),NUOTHER                                                  
DISOT080 CLI   ACTION,DM           IF MULTIPLE DISPLAY DONT                     
         BE    DISOTHEX            LOOK FOR ANY MORE ELEMENTS                   
         LA    RF,1(RF)                                                         
         LA    R4,0(RF,R4)                                                      
*                                                                               
DISOT100 ZIC   RF,NUOTLEN                                                       
         AR    R2,RF                                                            
         CLI   0(R2),X'60'                                                      
         BNE   DISOTHEX                                                         
         CLI   ACTION,DM           IF MULTIPLE DISPLAY DONT                     
         BE    DISOT020            MOVE COMMAS                                  
         CLI   ACTION,CM           IF MULTIPLE CHANGE DONT                      
         BE    DISOT020            MOVE COMMAS                                  
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
         B     DISOT020                                                         
         SPACE 1                                                                
DISOTHEX LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R2                                                               
OTHTAB   DC    CL1'A',CL8'AUTH=   ',XL1'04'                                     
         DC    CL1'B',CL8'CA=     ',XL1'02'                                     
         DC    CL1'C',CL8'CMT=    ',XL1'03'                                     
         DC    CL1'D',CL8'DP=     ',XL1'02'                                     
         DC    CL1'E',CL8'ECOST=  ',XL1'05'                                     
         DC    CL1'F',CL8'BT=     ',XL1'02'                                     
         DC    CL1'G',CL8'PB=     ',XL1'02'                                     
         DC    CL1'H',CL8'TSUPP=  ',XL1'05'                                     
         DC    CL1'J',CL8'EFT=    ',XL1'03'                                     
         DC    CL1'K',CL8'AFT=    ',XL1'03'                                     
         DC    CL1'L',CL8'MR=     ',XL1'02'                                     
         DC    CL1'M',CL8'MIRROR= ',XL1'06'                                     
         DC    CL1'O',CL8'POS=    ',XL1'03'                                     
         DC    CL1'P',CL8'PROMO=  ',XL1'05'                                     
         DC    CL1'R',CL8'REASON= ',XL1'06'                                     
         DC    CL1'S',CL8'SF=     ',XL1'02'                                     
         DC    CL1'S',CL8'SF=     ',XL1'02'                                     
         DC    CL1'T',CL8'TA=     ',XL1'02'                                     
         DC    CL1'W',CL8'WINDOW= ',XL1'06'                                     
         DC    CL1'Z',CL8'ZONE=   ',XL1'04'                                     
         DC    X'FF'                                                            
         SPACE 2                                                                
* DISPLAY DISK ADDRESS                                                          
*                                                                               
DISDA    LR    R0,RE                                                            
         XC    FLD,FLD                                                          
         CLI   DDS,YES             TEST FOR DDS TERMINAL                        
         BNE   DISDAX                                                           
         MVC   FLD(3),=C'DA='                                                   
         GOTO1 VHEXOUT,DMCB2,NBKEY+NDIRDA,FLD+3,4,0                             
         SPACE                                                                  
DISDAX   LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
* DISPLAY ACCOUNTING ACTIVITY                                                   
*                                                                               
DISACC   ST    RE,FULL2            SAVE RETURN POINT                            
         MVI   FLD,TIME            TIME DATA FIRST                              
         MVI   FLD+1,EQUAL                                                      
         LA    R4,FLD+2            INITIALIZE R4 AS OUTPUT POINTER              
         L     R2,NBBILTGR         SET UP FOR BILLING VALUES                    
         MVI   FULL,X'10'          BILLING ELEMENT CODE                         
         MVC   DUB2,=C'BILLABLE'                                                
         CLI   BYTE,UBILL          TEST FOR BILLING OUTPUT                      
         BE    DISACC2             YES                                          
         L     R2,NBPAYTGR                                                      
         MVI   FULL,X'12'          PAYMENT ELEMENT CODE                         
         MVC   DUB2,=C'PAYABLE*'                                                
         SPACE 1                                                                
DISACC2  BAS   R8,EDTACC           EDIT OUT TIME AMOUNT                         
         AR    R4,R0               UPDATE OUTPUT POINTER                        
         MVI   0(R4),LPAREN                                                     
         MVC   1(8,R4),DUB2        INITIALIZE TO NO BILL/PAY                    
         MVI   FULL+1,TIME         TIME DATA                                    
         BAS   R8,GETLAT           GET DATE OF LATEST EVENT                     
         BZ    *+8                 NONE                                         
         BAS   R8,DATOUT           EDIT THE DATE OUT                            
         MVI   9(R4),RPAREN                                                     
         MVI   10(R4),SLASH        SEPARATE TIME AND INT                        
         LA    R4,11(R4)           POINT TO CURRENT POSITION                    
         SPACE 1                                                                
DISACC4  MVI   0(R4),INT           INTEGRATION DATA                             
         MVI   1(R4),EQUAL                                                      
         LA    R4,2(R4)                                                         
         L     R2,NBBILIGR                                                      
         CLI   BYTE,UBILL                                                       
         BE    *+8                                                              
         L     R2,NBPAYIGR                                                      
         BAS   R8,EDTACC           EDIT OUT INTEGRATION DOLLARS                 
         AR    R4,R0                                                            
         MVI   0(R4),LPAREN                                                     
         MVC   1(8,R4),DUB2                                                     
         MVI   FULL+1,INT                                                       
         BAS   R8,GETLAT                                                        
         BZ    *+8                                                              
         BAS   R8,DATOUT                                                        
         MVI   9(R4),RPAREN                                                     
         SPACE 1                                                                
DISACCX  L     RE,FULL2                                                         
         BR    RE                                                               
         SPACE 2                                                                
* SUB-ROUTINE FOR DATE OUTPUT IN DISPLAY BILLING/PAYING                         
*                                                                               
DATOUT   GOTO1 VDATCON,DMCB2,(2,FULL+2),(5,1(R4))                               
         BR    R8                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO EDIT OUT AMOUNTS IN DISPLAY BILL/PAY                           
*                                                                               
EDTACC   LR    R0,R2               SAVE COST VALUE                              
         SRDA  R2,32               PREPARE DIVIDEND                             
         D     R2,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LTR   R2,R2               TEST REMAINDER (PENNIES)                     
         BNZ   EDTACC2             YES                                          
         EDIT  (R3),(11,(R4)),ALIGN=LEFT,MINUS=YES,ZERO=NOBLANK                 
         B     EDTACCX                                                          
         SPACE                                                                  
EDTACC2  LR    R2,R0               RESTORE COST VALUE W PENNIES                 
         EDIT  (R2),(12,(R4)),2,ALIGN=LEFT,MINUS=YES,ZERO=NOBLANK               
         SPACE                                                                  
EDTACCX  BR    R8                                                               
         EJECT                                                                  
* SUB-ROUTINE TO FIND LATEST ACCOUNTING EVENT                                   
*                                                                               
* AT ENTRY     FULL+0(1) CONTAINS ELEMENT CODE                                  
*              FULL+1(1) CONTAINS TYPE (T=TIME,I=INT)                           
* ON EXIT      FULL+2(2) RETURNS LATEST DATE OR ZERO                            
*              CC=NEQ FOR DATE SET, EQ FOR NO ELEMENT                           
*                                                                               
GETLAT   L     RF,NBAIO                                                         
         LA    RF,NUMAINEL-NUKEY(RF)                                            
         SR    R0,R0                                                            
         XC    FULL+2(2),FULL+2    INITIALIZE LATEST DATE                       
         SPACE 1                                                                
GETLAT2  CLI   0(RF),0             TEST FOR EOR                                 
         BE    GETLATX             YES                                          
         CLC   FULL(1),0(RF)       TEST FOR ELEMENT CODE                        
         BNE   GETLAT4             NO-GO TO NEXT ELEMENT                        
         CLC   FULL+1(1),2(RF)     TEST FOR MATCH ON TYPE                       
         BNE   GETLAT4                                                          
*                                                                               
         CLI   0(RF),X'10'         TEST FOR BILLING ELEMENT                     
         BNE   GETLAT3                                                          
         USING NUBILD,RF                                                        
         TM    NUBILST,X'20'       TEST FOR UNBILLED ELEMENT                    
         BO    GETLAT4             YES                                          
         SPACE 1                                                                
GETLAT3  CLC   FULL+2(2),3(RF)     TEST IF HIGHEST DATE                         
         BH    *+10                                                             
         MVC   FULL+2(2),3(RF)     YES-SAVE IT                                  
         SPACE 1                                                                
GETLAT4  IC    R0,1(RF)            BUMP TO NEXT ELEMENT                         
         AR    RF,R0                                                            
         B     GETLAT2                                                          
         SPACE 1                                                                
GETLATX  OC    FULL+2(2),FULL+2    SET CC ON EXIT                               
         BR    R8                                                               
         DROP  RF                                                               
         EJECT                                                                  
* EDITING SUB-ROUTINES                                                          
*                                                                               
EDTLEN   EDIT  (R2),(3,(R3)),ALIGN=LEFT                                         
         BR    R8                                                               
         SPACE 1                                                                
EDTTWO   EDIT  (R2),(10,FLD),2,ALIGN=LEFT                                       
         BR    R8                                                               
         SPACE 1                                                                
EDTONE   EDIT  (R2),(8,FLD),1,ALIGN=LEFT,ZERO=NOBLANK                           
         BR    R8                                                               
         SPACE 1                                                                
EDTBWO   EDIT  (R2),(8,FLD),2,ALIGN=LEFT,ZERO=NOBLANK                           
         BR    R8                                                               
         SPACE 1                                                                
EDTPWO   MVO   THREE,0(2,R2)                                                    
         OI    THREE+2,X'0F'                                                    
         EDIT  (P3,THREE),(4,FLD),ALIGN=LEFT                                    
         BR    R8                                                               
         SPACE 1                                                                
EDTMIN   TM    NBUNITST,X'80'      TEST FOR MINUS UNIT                          
         BZ    *+6                                                              
         LNR   R2,R2               CONVERT COST TO NEGATIVE NUMBER              
         LR    R0,R2               SAVE COST VALUE                              
         SRDA  R2,32               PREPARE DIVIDEND                             
         D     R2,=F'100'          SEPARATE DOLLARS AND PENNIES                 
         LTR   R2,R2               TEST REMAINDER (PENNIES)                     
         BNZ   EDTMIN2             YES                                          
         EDIT  (R3),(11,FLD),ALIGN=LEFT,MINUS=YES                               
         B     EDTMINX                                                          
         SPACE                                                                  
EDTMIN2  LR    R2,R0               RESTORE COST VALUE W PENNIES                 
         EDIT  (R2),(12,FLD),2,ALIGN=LEFT,MINUS=YES                             
         SPACE                                                                  
EDTMINX  BR    R8                                                               
         SPACE 2                                                                
* GET ELEMENT CALL (ELCODE CONTAINS SEARCH ELEMENT CODE)                        
* ON EXIT, CC=EQ IF ELEMENT FOUND, CC=NEQ IF ELEMENT NOT FOUND                  
*                                                                               
GETEL    GOTO1 VHELLO,DMCB2,(C'G',UNTFILE),(ELCODE,NBAIO),0                     
         CLI   12(R1),0            SET CC ON EXIT                               
         BR    R8                                                               
         SPACE 2                                                                
* SUB-ROUTINE TO MARK ESTIMATED DEMO OVERRIDES ON OUTPUT                        
*                                                                               
DEMOVER  GOTO1 VHELLO,DMCB2,(C'G',UNTFILE),(X'DD',NBAIO),(5,DUB)                
         CLI   12(R1),0                                                         
         BNER  R8                  NOT FOUND                                    
         MVC   DUB,FLD                                                          
         MVI   FLD,C'*'            INSERT A STAR BEFORE OVERRIDE VALUE          
         MVC   FLD+1(L'DUB),DUB    PUT THE EDITED VALUE BACK                    
         BR    R8                                                               
         SPACE 2                                                                
* MODULE EXIT                                                                   
*                                                                               
EXXMOD   XMOD1 1                                                                
         EJECT                                                                  
* TABLE OF FIELD DATA TYPES AND THEIR ROUTINES                                  
*                                                                               
         DS    0F                                                               
TYPTAB   DS    0XL4                                                             
         DC    AL1(UPRD),AL3(DISPRD)                                            
         DC    AL1(UP1SHR),AL3(DISP1SHR)                                        
         DC    AL1(UPRGN),AL3(DISPRGN)                                          
         DC    AL1(ULEN),AL3(DISLEN)                                            
         DC    AL1(UDAY),AL3(DISDAY)                                            
         DC    AL1(UTIME),AL3(DISTIME)                                          
         DC    AL1(UAFFID),AL3(DISAFFID)                                        
         DC    AL1(UASS),AL3(DISASS)                                            
         DC    AL1(UACT),AL3(DISACT)                                            
         DC    AL1(USTAT),AL3(DISSTAT)                                          
         DC    AL1(UINT),AL3(DISINT)                                            
         DC    AL1(UFEED),AL3(DISFEED)                                          
         DC    AL1(UIMP),AL3(DISIMP)                                            
         DC    AL1(UNTI),AL3(DISNTI)                                            
         DC    AL1(UNSI),AL3(DISNSI)                                            
         DC    AL1(UUNCD),AL3(DISUCOD)                                          
         DC    AL1(UUNPC),AL3(DISUNPC)                                          
         DC    AL1(UFEEDCD),AL3(DISFCD)                                         
         DC    AL1(UBB),AL3(DISBB)                                              
         DC    AL1(UROT),AL3(DISROT)                                            
         DC    AL1(UHAVE),AL3(DISHAVE)                                          
         DC    AL1(UHSC),AL3(DISHSC)                                            
         DC    AL1(UHUTADJ),AL3(DISHADJ)                                        
         DC    AL1(USREP),AL3(DISREP)                                           
         DC    AL1(UPRE),AL3(DISPRE)                                            
         DC    AL1(USCHG),AL3(DISSCHG)                                          
         DC    AL1(UMGPC),AL3(DISMGPC)                                          
         DC    AL1(UMGDAT),AL3(DISMGDAT)                                        
         DC    AL1(UMISS),AL3(DISMISS)                                          
         DC    AL1(UHUT),AL3(DISHUT)                                            
         DC    AL1(USHR),AL3(DISSHR)                                            
         DC    AL1(URAT),AL3(DISRAT)                                            
         DC    AL1(UPG),AL3(DISPG)                                              
         DC    AL1(UDG),AL3(DISDG)                                              
         DC    AL1(UPF),AL3(DISPFIL)                                            
         DC    AL1(URES),AL3(DISRES)                                            
         DC    AL1(UBUYD),AL3(DISBUYD)                                          
         DC    AL1(ULSTA),AL3(DISLSTA)                                          
         DC    AL1(UDA),AL3(DISDA)                                              
         DC    AL1(UBILL),AL3(DISACC)                                           
         DC    AL1(UPAY),AL3(DISACC)                                            
         DC    AL1(UOTHER),AL3(DISOTH)                                          
TYPES    EQU   (*-TYPTAB)/L'TYPTAB                                              
         SPACE 2                                                                
         LTORG                                                                  
* CONSTANTS                                                                     
*                                                                               
UNTFILE  DC    CL8'UNTFIL'                                                      
SHARE    DC    C'S',X'01'                                                       
HUT      DC    C'P',X'01'                                                       
RATING   DC    C'R',X'01'                                                       
         SPACE 2                                                                
* PATCH AREA                                                                    
*                                                                               
         DS    0H                                                               
PATCH    DC    XL32'00'                                                         
         SPACE 2                                                                
* LITERAL POOL                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* EQUATES                                                                       
*                                                                               
SLASH    EQU   C'/'                                                             
EQUAL    EQU   C'='                                                             
LPAREN   EQU   C'('                                                             
RPAREN   EQU   C')'                                                             
TIME     EQU   C'T'                TIME DOLLARS                                 
INT      EQU   C'I'                INTEGRATION DOLLARS                          
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009NEBUY30S  05/01/02'                                      
         END                                                                    
