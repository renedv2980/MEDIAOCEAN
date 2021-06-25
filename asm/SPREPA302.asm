*          DATA SET SPREPA302  AT LEVEL 088 AS OF 08/25/05                      
*PHASE SPA302C                                                                  
*INCLUDE CASHVAL                                                                
*INCLUDE COVAIL                                                                 
*INCLUDE SPB1WW                                                                 
         TITLE 'SPREPA302 - CLIENT SUMMARY REPORT'                              
         PRINT NOGEN                                                            
SPA302   CSECT                                                                  
         NMOD1 0,SPA302,RA                                                      
*                                                                               
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING SPA302+8192,RC,R7                                                
*                                                                               
         L     R8,0(R1)                                                         
         LA    R9,2048(R8)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,R8,R9                                                    
*                                                                               
         L     R1,=A(GLAREA)                                                    
         ST    R1,AGLAREA                                                       
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SP010                                                            
         CLI   MODE,REQFRST                                                     
         BE    SP100                                                            
         CLI   MODE,CLTFRST                                                     
         BE    SP200                                                            
         CLI   MODE,REQLAST                                                     
         BE    SP700                                                            
         B     EXIT                                                             
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* RUN FIRST                                                                     
*                                                                               
SP010    STM   R7,RC,BUYHKR7                                                    
*                                                                               
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         MVC   MEDNUMMO,=F'13'     13 WEEKS                                     
         MVC   MEDNUMPE,=F'1'      PERIOD ONLY                                  
         MVI   MEDEXTAC,C'Y'       ACTG DATA                                    
         MVI   MEDEXTDM,0          NO DEMOS                                     
         MVC   MEDLCHNK,=F'200'                                                 
         XC    MEDNUMWK,MEDNUMWK                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVI   FIRSTSW,C'Y'        FIRST TIME SWITCH                            
*                                                                               
         XC    ADRIVER,ADRIVER                                                  
         XC    ASYSDRV,ASYSDRV                                                  
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* REQUEST FIRST                                                                 
*                                                                               
SP100    L     R2,AGLAREA                                                       
         USING GLOBALD,R2                                                       
         MVC   PAGE,=X'0001'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   ANYDATA,C'N'        NO DATA YET                                  
         MVI   RQGETBF,C'N'                                                     
         CLI   QOPT1,C' '          SET UP REQUEST OPTIONS FROM PROFILE          
         BH    *+10                 IF NOT OVERRIDDEN                           
         MVC   QOPT1,PROGPROF+7    GROSS/NET DOLLARS (G/N)                      
         CLI   QOPT2,C' '                                                       
         BH    SP100A                                                           
         MVC   QOPT2,PROGPROF+6    REPORT FORMAT (1-7)                          
         OI    QOPT2,X'F0'                                                      
         CLI   QOPT2,C'0'                                                       
         BH    SP100A                                                           
         MVC   P(44),=C'** REQUEST CANCELLED - A3 PROFILE NOT SET **'           
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
SP100A   CLI   QOPT3,C' '                                                       
         BH    *+14                                                             
         MVC   QOPT3,PROGPROF+8    PERIOD FORMAT (1-3)                          
         OI    QOPT3,X'F0'                                                      
         CLI   QOPT5,C' '                                                       
         BH    *+14                                                             
         MVC   QOPT5,PROGPROF+10   COLUMN SUPPRESSION (0-4)                     
         OI    QOPT5,X'F0'                                                      
*                                                                               
         CLI   QOPT4,C'P'          TEST FOR ANALYSIS TYPE PAID                  
         BE    *+12                                                             
         CLI   QOPT4,C'U'                              OR UNPAID                
         BNE   *+12                                                             
         MVI   QOPT5,C'2'          YES - FORCE SUPPRESS BILLED                  
         B     SP101                                                            
         CLI   QOPT4,C'R'          TEST FOR ANALYSIS TYPE BILLED                
         BE    *+12                                                             
         CLI   QOPT4,C'B'                              OR BILLABLE              
         BNE   SP101                                                            
         MVI   QOPT5,C'1'          YES - FORCE SUPPRESS PAID                    
*                                                                               
SP101    CLC   QRECORD+49(12),SPACES TEST FOR DOLLAR ADJ OR DATES               
         BNH   SP102                                                            
         CLI   QRECORD+49,C'1'       TEST FOR DOLLAR ADJUSTMENT                 
         BNL   SP102                                                            
         MVI   QRECORD+49,C'+'       YES -                                      
         CLI   PROGPROF+11,C'N'      TEST FOR DOLLAR ADJ COMMENT                
         BNE   SP102                                                            
         MVI   QRECORD+49,C'-'       NO - INDICATE NOT TO PRINT CMT             
*                                                                               
SP102    MVC   SPOTPROF+2(1),PROGPROF+1   OVERRIDE THE SYSTEM PROFILE           
         MVC   SPOTPROF+6(3),PROGPROF+2                                         
         CLI   QRECORD+66,C'B'     MONTH OPTION                                 
         BNE   SP103                                                            
         MVI   SPOTPROF+2,0        SET TO BROADCAST MONTHS                      
         MVI   SPOTPROF+6,0                                                     
         MVI   SPOTPROF+7,0                                                     
         MVI   SPOTPROF+8,1                                                     
         B     SP104                                                            
*                                                                               
SP103    MVI   QRECORD+66,C'B'                                                  
         CLI   SPOTPROF+2,0        TEST PROFILE FOR FISCAL MONTHS               
         BE    SP104                                                            
         MVI   QRECORD+66,C' '     YES - INDICATE ON REQUEST CARD               
*                                                                               
SP104    MVC   PIGOPT,QRECORD+34   PIGGYBACK OPTION                             
         CLI   PIGOPT,C'N'                                                      
         BE    *+10                                                             
         MVC   PIGOPT,PROGPROF+12                                               
*                                                                               
         LA    RE,GLOBALD          INIT DRIVER STORAGE TO ZEROS                 
         L     RF,=F'40000'                                                     
         XCEF                                                                   
         MVC   GLSIZE,=F'40000'    GLOBAL SIZE                                  
         MVC   GLOBALD(8),=C'*GLOBAL*'                                          
*                                                                               
         ZIC   R4,QOPT2            DETERMINE WHICH DPG FILE                     
         LA    R4,4(R4)                                                         
         CLM   R4,1,=X'FA'                                                      
         BL    *+8                                                              
         SH    R4,=H'57'                                                        
         CLI   DPGFILE+5,C' '                                                   
         BE    SP105                                                            
         SR    R3,R3                                                            
         BCTR  R3,0                                                             
         GOTO1 LOADER,DMCB,DPGFILE,(R3)                                         
*                                                                               
SP105    STC   R4,DPGFILE+5                                                     
         GOTO1 LOADER,DMCB,DPGFILE,0    LOAD DPG PROGRAM                        
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,GLAPROG                                                       
*                                                                               
         OC    ADRIVER,ADRIVER                                                  
         BNZ   SP106                                                            
         GOTO1 LOADER,DMCB,DRIVER       LOAD DRIVER                             
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ADRIVER                                                       
*                                                                               
SP106    OC    ASYSDRV,ASYSDRV                                                  
         BNZ   SP107                                                            
         GOTO1 LOADER,DMCB,SPDRIVER     LOAD SYSTEM DRIVER                      
         ICM   R6,15,DMCB+4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         ST    R6,ASYSDRV                                                       
*                                                                               
SP107    MVC   GLASYSDR,ASYSDRV    SYSTEM DRIVER ADDR                           
*                                                                               
         CLI   FIRSTSW,C'Y'        TEST FIRST REQUEST                           
         BNE   SP108                                                            
         MVI   FIRSTSW,C'N'        YES - GET BUFFALO BUFFER                     
         L     R6,=A(BUFFALOC)                                                  
         GOTO1 COVAIL,DMCB,C'SETB',40000,350000,(R6)                            
         OC    DMCB+4(4),DMCB+4                                                 
         BNZ   *+6                                                              
         DC    H'0'                                                             
         MVC   BUFFBUFF,DMCB+12                                                 
         GOTO1 BUFFALO,DMCB,=C'SET',BUFFBUFF     INIT BUFFALO                   
*                                                                               
SP108    LA    RE,SPWORKD                                                       
         ST    RE,GLAWORKD         SPOT WORK ADDR                               
         LA    RE,DRHOOK                                                        
         ST    RE,GLAHOOK          OUR DRIVER HOOK                              
         MVI   GLTWORKD,GLTSPOT    SPOT WORK                                    
         MVI   GLAUTOCH,C'N'       NO AUTOCHUNKING                              
         MVI   GLDETHED,C'Y'       I GET HEADS AT DETAIL TIME                   
         MVI   GLFHEADL,10         HEADINGS ON LINES 10,11                      
         MVI   GLLHEADL,11                                                      
         OI    GLINDS,GLISDONT     SUPPRESS REJECTED PRINT LINES                
*                                                                               
*                                                                               
         MVI   ESTFILT,C'N'        DETERMINE IF ESTIMATE FILTERING              
         CLC   QEST(2),=C'NO'                                                   
         BNE   SP109                                                            
         CLC   QESTEND,SPACES                                                   
         BE    SP109                                                            
         MVI   ESTFILT,C'Y'                                                     
*                                                                               
SP109    MVC   RQEST,QEST          SAVE QEST AND QESTEND                        
         MVC   RQESTEND,QESTEND                                                 
         MVI   ESTOTS,C'Y'         DETERMINE WHETHER EST TOTALS NEEDED          
         MVI   QBESTIM,0                                                        
         CLC   QEST,=C'ALL'                                                     
         BE    SP113                                                            
         CLC   QEST(2),=C'NO'                                                   
         BE    SP110                                                            
         CLC   QESTEND,SPACES                                                   
         BE    SP111                                                            
         CLC   QEST,QESTEND                                                     
         BE    SP111                                                            
*                                                                               
SP110    CLI   PROGPROF+9,C'Y'   TEST FOR OPTION FOR SEPERATE ESTIMATES         
         BNE   SP112                                                            
         MVC   QEST,=C'ALL'        YES - FUDGE REQUEST ESTIMATES FOR            
         MVC   QESTEND,SPACES            CORRECT HEADLINES                      
         B     SP113                                                            
*                                                                               
SP111    PACK  DUB,QEST(3)         FOR ONE ESTIMATE, STORE IT                   
         CVB   RE,DUB                                                           
         STC   RE,QBESTIM                                                       
*                                                                               
SP112    MVI   ESTOTS,C'N'                                                      
*                                                                               
SP113    MVI   ALLOWBF,C'Y'        ALLOW BILL FORMULAE ADJUSTMENTS              
         CLI   QOPT1,C'G'          ONLY FOR GROSS DOLLARS                       
         BNE   SP113A                                                           
         CLC   QPRD,=C'POL'        AND PRODUCT NE POL                           
         BE    SP113A                                                           
         CLI   ESTOTS,C'Y'         EST=ALL IS OK                                
         BE    SP114                                                            
         CLI   QBESTIM,0           AND SINGLE ESTIMATE REQUEST IS OK            
         BNE   SP114                                                            
SP113A   MVI   ALLOWBF,C'N'                                                     
*                                                                               
SP114    XC    ADJ,ADJ                                                          
         MVI   ADJCMT,C'N'                                                      
         MVI   DATEFILT,0                                                       
         CLC   QRECORD+49(12),SPACES   TEST FOR DATE LIMITS                     
         BNH   SP116                    OR DOLLAR ADJUSTMENT                    
         CLI   QRECORD+49,C'1'         TEST FOR DATE LIMITS                     
         BL    SP118                                                            
         MVC   DATEFILT,QRECORD+29     YES - SET DATE LIMIT TYPE                
         CLI   DATEFILT,C'B'                 BILL DATES                         
         BE    SP115                                                            
         CLI   DATEFILT,C'P'                 PAY DATES                          
         BE    SP115                                                            
         DC    H'0'                                                             
*                                                                               
SP115    GOTO1 DATCON,DMCB,QRECORD+49,(2,DBSTART) SET THE DATE LIMITS           
         GOTO1 DATCON,DMCB,QRECORD+55,(2,DBEND)                                 
         GOTO1 DATCON,DMCB,QRECORD+49,(8,DSTART)                                
         GOTO1 DATCON,DMCB,QRECORD+55,(8,DEND)                                  
*                                                                               
SP116    XC    WORK,WORK           NO DOLLAR ADJUSTMENT IN REQUEST --           
         MVC   WORK(4),=C'S0AC'    READ AC PROFILE FOR DEFAULT ADJ              
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
         MVI   WORK+10,C'*'                                                     
         L     RE,ADCLT                                                         
         MVC   WORK+11(1),COFFICE-CLTHDRD(RE)                                   
         GOTO1 GETPROF,DMCB,WORK,WORK+24,DATAMGR,RR=RB                          
         MVC   SVACPROF,WORK+24    SAVE AC PROFILE                              
         OC    SVACPROF,SVACPROF                                                
         BZ    SP122                                                            
         CLI   SVACPROF+7,C'Y'     TEST BILL FORMULAE ADJUST                    
         BNE   SP117                                                            
         CLI   ALLOWBF,C'Y'        TEST ALLOWED                                 
         BNE   SP117                                                            
         MVI   RQGETBF,C'Y'        YES                                          
         B     SP122                                                            
*                                                                               
SP117    OC    SVACPROF+5(2),SVACPROF+5   EXTRACT DOLAR ADJUSTMENT              
         BNZ   *+10                                                             
         MVC   SVACPROF+5(2),=C'00'                                             
         PACK  DUB,SVACPROF(7)                                                  
         CVB   RE,DUB                                                           
         LTR   RE,RE               TEST FOR NO ADJUSTMENT                       
         BZ    SP122                                                            
         ST    RE,ADJ                                                           
         CLI   PROGPROF+11,C'N'                                                 
         BE    SP122                                                            
         B     SP120               HEADLINE COMMENT NEEDED                      
*                                                                               
SP118    CLC   QRECORD+50(2),=C'BF'   TEST BILL FORMULA ADJUST                  
         BNE   SP119                                                            
         CLI   ALLOWBF,C'Y'           YES-TEST ALLOW IT                         
         BNE   SP116                      NO-TRY THE PROFILE                    
         MVI   RQGETBF,C'Y'                                                     
         B     SP122                                                            
*                                                                               
SP119    PACK  DUB,QRECORD+50(10)  DOLLAR ADJUSTMENT                            
         CVB   RE,DUB                                                           
         ST    RE,ADJ                                                           
         CLI   QRECORD+49,C'-'     TEST FOR HEADLINE COMMENT NEEDED             
         BE    SP122                                                            
*                                                                               
SP120    MVI   ADJCMT,C'Y'                                                      
         MVC   ADJHEAD,ADHDINIT                                                 
*                                                                               
SP122    CLI   RQGETBF,C'Y'        TEST BILL FORMULAE ADJUSTMENTS               
         BNE   SP123                                                            
         XC    ADJ,ADJ             YES-THEN IGNORE ANY OTHER ADJUSTMENT         
         CLI   PROGPROF+11,C'N'    TEST PRINT ADJUSTMENT COMMENT                
         BE    SP124                                                            
         MVI   ADJCMT,C'Y'                                                      
         B     SP124                                                            
*                                                                               
SP123    CLI   QOPT1,C'G'          TEST GROSS DOLLARS                           
         BE    SP124                                                            
         B     SP124               ** ALLOW ADJUST FOR ALL **  10/23/89         
         XC    ADJ,ADJ             NO-THEN NO PCT ADJUSTMENT ALLOWED            
         MVI   ADJCMT,C'N'                                                      
*                                                                               
SP124    MVI   QRECORD+67,C'Y'     ALWAYS ALLOW BILL ADJUST (9/12/89)           
*                                                                               
         MVC   GLAUSW,PROGPROF     DETERMINE WHETHER AUTHORIZED DOLLARS         
         CLI   GLAUSW,C'A'         OR GOAL DOLLARS ARE NEEDED                   
         BNE   SP126                                                            
         CLI   QMGR,C' '           CANNOT HAVE MKTGRP FILTERING                 
         BE    SP126               FOR AUTHORIZED DOLLARS                       
         MVI   GLAUSW,C'N'                                                      
*                                                                               
SP126    CLC   QMKT,SPACES         DETERMINE IF ALL MKTS/STN                    
         BH    *+10                                                             
         MVC   QMKT(3),=C'ALL'                                                  
         CLC   QSTA,SPACES                                                      
         BH    SP130                                                            
         MVC   QSTA(3),=C'ALL'                                                  
*                                                                               
*        DETERMINE ROW SEQUENCES                                                
*                                                                               
SP130    MVI   ESTHED,C'Y'                                                      
         MVI   PRDHED,C'Y'                                                      
         MVI   MKTHED,C'Y'                                                      
         MVI   PRDLEV,0                                                         
         MVI   ESTLEV,0                                                         
         MVI   NHEADS,0                                                         
*                                                                               
         CLI   QOPT2,C'1'          EST - MKT/STA/PRD SEQUENCE                   
         BNE   SP140                                                            
         MVI   RCSUBPRG,1                                                       
         MVI   PRDHED,C'N'                                                      
         MVI   MKTHED,C'N'                                                      
         MVC   AESTCNTR,=A(L2CNTR)                                              
         MVC   AMKTCNTR,=A(L3CNTR)                                              
         MVC   ASTACNTR,=A(L4CNTR)                                              
         MVC   APRDCNTR,=A(L5CNTR)                                              
         MVI   ESTLEV,1                                                         
         MVI   NHEADS,1                                                         
         B     SP150                                                            
*                                                                               
SP140    CLI   QOPT2,C'2'          EST/PRD - MKT/STA SEQUENCE                   
         BNE   SP142                                                            
         MVI   MKTHED,C'N'                                                      
         MVI   RCSUBPRG,2                                                       
         MVC   AESTCNTR,=A(L2CNTR)                                              
         MVC   APRDCNTR,=A(L3CNTR)                                              
         MVC   AMKTCNTR,=A(L4CNTR)                                              
         MVC   ASTACNTR,=A(L5CNTR)                                              
         MVI   ESTLEV,1                                                         
         MVI   PRDLEV,2                                                         
         MVI   NHEADS,2                                                         
         B     SP150                                                            
*                                                                               
SP142    CLI   QOPT2,C'3'          PRD/EST - MKT/STA SEQUENCE                   
         BNE   SP144                                                            
         MVI   MKTHED,C'N'                                                      
         MVI   RCSUBPRG,3                                                       
         MVC   APRDCNTR,=A(L2CNTR)                                              
         MVC   AESTCNTR,=A(L3CNTR)                                              
         MVC   AMKTCNTR,=A(L4CNTR)                                              
         MVC   ASTACNTR,=A(L5CNTR)                                              
         MVI   PRDLEV,1                                                         
         MVI   ESTLEV,2                                                         
         MVI   NHEADS,2                                                         
         B     SP150                                                            
*                                                                               
SP144    CLI   QOPT2,C'4'          MKT - STA/EST/PRD SEQUENCE                   
         BNE   SP146                                                            
         MVI   ESTHED,C'N'                                                      
         MVI   PRDHED,C'N'                                                      
         MVI   RCSUBPRG,4                                                       
         MVC   AMKTCNTR,=A(L2CNTR)                                              
         MVC   ASTACNTR,=A(L3CNTR)                                              
         MVC   AESTCNTR,=A(L4CNTR)                                              
         MVC   APRDCNTR,=A(L5CNTR)                                              
         MVI   NHEADS,1                                                         
         B     SP150                                                            
*                                                                               
SP146    CLI   QOPT2,C'5'          MKT - STA/PRD/EST SEQUENCE                   
         BNE   SP148                                                            
         MVI   ESTHED,C'N'                                                      
         MVI   PRDHED,C'N'                                                      
         MVI   RCSUBPRG,5                                                       
         MVC   AMKTCNTR,=A(L2CNTR)                                              
         MVC   ASTACNTR,=A(L3CNTR)                                              
         MVC   APRDCNTR,=A(L4CNTR)                                              
         MVC   AESTCNTR,=A(L5CNTR)                                              
         MVI   NHEADS,1                                                         
         B     SP150                                                            
*                                                                               
SP148    CLI   QOPT2,C'6'          PRD/MKT - STA/EST SEQUENCE                   
         BNE   SP149                                                            
         MVI   ESTHED,C'N'                                                      
         MVI   RCSUBPRG,6                                                       
         MVC   APRDCNTR,=A(L2CNTR)                                              
         MVC   AMKTCNTR,=A(L3CNTR)                                              
         MVC   ASTACNTR,=A(L4CNTR)                                              
         MVC   AESTCNTR,=A(L5CNTR)                                              
         MVI   PRDLEV,1                                                         
         MVI   NHEADS,2                                                         
         B     SP150                                                            
*                                                                               
SP149    CLI   QOPT2,C'7'          MKT/EST/PRD/STA SEQUENCE                     
         BNE   SP149A                                                           
         MVI   ESTHED,C'N'                                                      
         MVI   PRDHED,C'N'                                                      
         MVI   MKTHED,C'N'                                                      
         MVI   RCSUBPRG,7                                                       
         MVC   AMKTCNTR,=A(L2CNTR)                                              
         MVC   AESTCNTR,=A(L3CNTR)                                              
         MVC   APRDCNTR,=A(L4CNTR)                                              
         MVC   ASTACNTR,=A(L5CNTR)                                              
         B     SP150                                                            
*                                                                               
SP149A   CLI   QOPT2,C'8'          EST/MKT - STA/PRD SEQUENCE                   
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   PRDHED,C'N'                                                      
         MVI   RCSUBPRG,8                                                       
         MVC   AESTCNTR,=A(L2CNTR)                                              
         MVC   AMKTCNTR,=A(L3CNTR)                                              
         MVC   ASTACNTR,=A(L4CNTR)                                              
         MVC   APRDCNTR,=A(L5CNTR)                                              
         MVI   ESTLEV,1                                                         
         MVI   NHEADS,2                                                         
*                                                                               
SP150    MVI   PRTSW,C'Y'                                                       
         MVI   SPSUPMKT,C'Y'       SUPPRESS MARKET NAME FOR MKTGRP              
         XC    SVESTIM,SVESTIM                                                  
         XC    SVPROD,SVPROD                                                    
         XC    SVPROD2,SVPROD2                                                  
         XC    SVMARKET,SVMARKET                                                
         XC    SVMGR,SVMGR                                                      
         XC    SVPER,SVPER                                                      
*                                                                               
*                                                                               
         GOTO1 DATCON,DMCB,QSTART,(2,BQSTARTP)                                  
         GOTO1 DATCON,DMCB,QEND,(2,BQENDP)                                      
         GOTO1 DATCON,DMCB,QSTART,(3,BQSTART)                                   
         GOTO1 DATCON,DMCB,QEND,(3,BQEND)                                       
*                                                                               
         GOTO1 MEDDATE,DMCB,(R8)   BUILD MEDBLOCK                               
*                                                                               
         CLI   GLAUSW,C'A'         TEST FOR AUTHORIZED DOLLARS                  
         BE    *+14                                                             
         CLC   QAGY,=C'WW'         OR AGENCY=WUNDERMAN                          
         BNE   SP165                                                            
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         LA    R4,MEDMON01         YES - BUILD TABLE OF REQUEST MONTHS          
         L     R5,MEDNUMMO                                                      
         LA    R6,REQMNTHS                                                      
*                                                                               
SP155    OC    0(4,R4),0(R4)                                                    
         BZ    SP160                                                            
         GOTO1 DATCON,DMCB,(2,2(R4)),(3,WORK)                                   
         MVC   0(2,R6),WORK                                                     
         MVC   ENDYM,WORK                                                       
*                                                                               
SP160    LA    R4,12(R4)                                                        
         LA    R6,2(R6)                                                         
         BCT   R5,SP155                                                         
*                                                                               
SP165    XC    DMCB(12),DMCB                                                    
         MVC   DMCB(3),QAGY        A-M                                          
         MVI   DMCB+3,C'0'                                                      
         GOTO1 DPTRD,DMCB,,ADDPTTAB                                             
         EJECT                                                                  
*                                                                               
* DETERMINE BILLING PERIOD NUMBERS                                              
*                                                                               
         CLI   QOPT5,C'1'          TEST FOR BILLED DOLLARS SUPPRESSED           
         BH    SP199               YES - SKIP                                   
         GOTO1 DATCON,DMCB,QSTART,WORK                                          
         MVC   QSTART(6),WORK                                                   
         GOTO1 (RF),(R1),QEND                                                   
         MVC   QEND(6),WORK                                                     
* GO BACK 30 MONTHS AND FORWARD 2                                               
         GOTO1 ADDAY,(R1),(C'M',QSTART),WORK,F'-30'                             
         GOTO1 (RF),(R1),(C'M',QEND),WORK+6,F'2'                                
*                                                                               
SP170    IC    R0,SPOTPROF+2       DATE CONTROL                                 
*                                                                               
         GOTO1 MOBILE,DMCB,(50,WORK),((R0),ADBUY)                               
*                                                                               
* NOW WORK OUT PERIOD NUMBERS - FIND PLACES WHERE YEAR CHANGES                  
*                                                                               
         LA    R5,BILLPER                                                       
         XC    0(256,R5),0(R5)                                                  
         XC    256(L'BILLPER-256,R5),256(R5)                                    
         L     R4,ADBUY                                                         
*                                                                               
SP180    BAS   RE,CKNEWYR          TEST START OF NEW YEAR                       
         BE    SP182               YES                                          
         LA    R4,4(R4)                                                         
         B     SP180                                                            
*                                                                               
SP182    ZIC   R3,2(R4)            SET INITIAL YEAR                             
         SRL   R3,1                                                             
*                                                                               
SP184    LA    R1,1                FOR PERIOD NUMBERS WITHIN YEAR               
*                                                                               
SP186    STC   R3,0(R5)            YEAR                                         
         STC   R1,1(R5)            MONTH                                        
         MVC   2(4,R5),0(R4)       START-END OF PERIOD                          
         LA    R5,6(R5)                                                         
         LA    R4,4(R4)                                                         
         CLI   0(R4),X'FF'         TEST E-O-L                                   
         BE    SP190                                                            
         LA    R1,1(R1)                                                         
         BAS   RE,CKNEWYR          TEST NEW YEAR                                
         BNE   SP186               NO - CONTINUE                                
         CH    R1,=H'2'            *** CODE TO AVOID FREAK DOUBLE               
         BE    SP184               *** NEW YEAR SITUATION                       
         LA    R3,1(R3)            ADD 1 TO YEAR IN BINARY                      
         B     SP184                                                            
*                                                                               
SP190    LA    R4,BILLPER                                                       
*                                                                               
SP192    CLC   BQSTARTP,4(R4)                                                   
         BNH   *+12                                                             
         LA    R4,6(R4)                                                         
         B     SP192                                                            
         MVC   BILLST,0(R4)        FIRST REQUESTED BILL MONTH                   
*                                                                               
SP194    MVC   BILLEND,0(R4)       LAST REQUESTED BILL MONTH                    
         CLC   BQENDP,4(R4)                                                     
         BE    SP199                                                            
         BH    *+14                                                             
         MVC   4(2,R4),BQENDP                                                   
         B     SP199                                                            
         LA    R4,6(R4)                                                         
         B     SP194                                                            
*                                                                               
SP199    B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
*                                                                               
* CLIENT FIRST                                                                  
*                                                                               
SP200    CLI   QOPT2,C'0'                                                       
         BNH   EXIT                                                             
         CLC   QAGY,=C'WW'         TEST WUNDERMAN                               
         BNE   SP201                                                            
         XC    B1XPROF,B1XPROF     YES-GET B1X PROFILE                          
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'SB1X'                                                 
         NI    WORK,X'BF'          MAKE SYS LOWER CASE FOR PAGE A               
         MVC   WORK+4(2),AGY                                                    
         MVC   WORK+6(1),MED                                                    
         MVC   WORK+7(3),CLIENT                                                 
         L     RF,ADCLT                                                         
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE-CLTHDR(RF)                                    
         GOTO1 GETPROF,DMCB,WORK,B1XPROF,DATAMGR                                
*                                                                               
SP201    L     R2,AGLAREA                                                       
         USING GLOBALD,R2                                                       
         MVI   SVEST,0                                                          
         CLI   SDNMGRPS,1          TEST MORE THAN ONE MKTGRP LEVEL              
         BNH   SP202                                                            
         LA    R1,11               YES - ADJUST FIRST/LAST HEADLINES            
         CLI   SDNMGRPS,3                FOR DRIVER                             
         BNE   *+8                                                              
         LA    R1,12                                                            
         STC   R1,GLFHEADL                                                      
         LA    R1,1(R1)                                                         
         STC   R1,GLLHEADL                                                      
*                                                                               
SP202    MVI   GLMODE,GLINIT       INITIALIZE DRIVER                            
         MVC   GLTRACE,QRECORD+35                                               
         GOTO1 ADRIVER,DMCB,AGLAREA                                             
*                                                                               
         L     RE,GLAPLIST                                                      
         MVC   ASPDRWKC,0(RE)      A(SPOT DRIVER WORK AREA)                     
*                                                                               
         ZIC   RE,SDNPGRPS         DETERMINE BREAK LEVELS                       
         ZIC   RF,SDNMGRPS                                                      
         CLI   QOPT3,C'3'          ADD PERIOD IF PERIOD HIGH                    
         BNE   *+8                                                              
         LA    RE,1(RE)                                                         
         AR    RE,RF               SUM PRD GRPS AND MKT GRPS                    
*                                                                               
         ICM   RF,1,PRDLEV         TEST PRODUCT IN HEADLINES                    
         BZ    *+10                                                             
         AR    RF,RE               YES - SET PRODUCT LEVEL                      
         STC   RF,PRDLEV                                                        
         ICM   RF,1,ESTLEV         TEST ESTIMATE IN HEADLINES                   
         BZ    *+10                                                             
         AR    RF,RE               YES - SET ESTIMATE LEVEL                     
         STC   RF,ESTLEV                                                        
*                                                                               
         MVI   PGRP1LEV,0                                                       
         CLI   SDNPGRPS,0          TEST PRODUCT GROUPS                          
         BE    SP203                                                            
         MVI   PGRP1LEV,1          YES - SET PGRP1 LEVEL                        
         CLI   QOPT3,C'3'                                                       
         BNE   SP203                                                            
         MVI   PGRP1LEV,2                                                       
         DROP  R2                                                               
*                                                                               
SP203    LTR   RF,RE                                                            
         BNZ   *+10                                                             
         ZAP   L2CNTR,=P'0'                                                     
         LA    RF,1(RF)                                                         
         STC   RF,TOTLEV           DRIVER SHOULD GENERATE ALL TOTALS            
         LA    R0,5                 FOR TOTLEV AND ABOVE                        
         LA    R2,FIRSTAB                                                       
*                                                                               
SP205    STC   RE,0(R2)                                                         
         LA    RE,1(RE)                                                         
         LA    R2,4(R2)                                                         
         BCT   R0,SP205                                                         
         EJECT                                                                  
*                                                                               
* MARKET GROUP ASSIGN TABLE                                                     
*                                                                               
         LA    R0,20000/250        CLEAR MARKET GROUP TABLE                     
         L     R1,AMGRTAB                                                       
*                                                                               
SP207    MVI   0(R1),X'99'                                                      
         MVC   1(249,R1),0(R1)                                                  
         LA    R1,250(R1)                                                       
         BCT   R0,SP207                                                         
*                                                                               
         CLI   QMGR,C' '           TEST MKTGRP REQUEST                          
         BE    SP240               NO - SKIP                                    
*                                                                               
         XC    KEY,KEY             READ MARKET GROUP DEFINITION RECORD          
         MVC   KEY(2),=X'0D02'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         CLI   QMGR,C'F'                                                        
         BH    *+10                                                             
         MVC   KEY+3(2),BCLT                                                    
         MVC   KEY+8(1),QMGR                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   MKTERR                                                           
*****    BE    *+6                                                              
****     DC    H'0'                                                             
         L     R2,ADBUY                                                         
         ST    R2,AREC                                                          
         GOTO1 GET                                                              
         LA    R3,24(R2)                                                        
         SR    R0,R0                                                            
         LA    R4,SVPGRPEX                                                      
         LA    R5,PEXMAX+1                                                      
         MVI   SVPGRID,0                                                        
*                                                                               
SP208    CLI   0(R3),0                                                          
         BE    SP212                                                            
         CLI   0(R3),1             MKTGRP BREAK DESCRIPTION                     
         BNE   SP209                                                            
         USING MKGEL01,R3                                                       
         CLI   MKGPGA,C'Y'         TEST MKTGRPS BY PRDGRP                       
         BNE   SP210                                                            
         CLI   QMGR,C'F'           YES-MUST BE CLIENT SPECIFIC                  
         BH    SP210                                                            
         CLI   QPGR,C' '           TEST FOR PRODUCT GROUPS                      
         BNH   SP210                                                            
         MVC   SVPGRID,QPGR        YES-SAVE PRDGRP ID                           
         B     SP210                                                            
         DROP  R3                                                               
*                                                                               
SP209    CLI   0(R3),2             CLIENT/PRODUCT EXCEPTION ELEMENT             
         BNE   SP210                                                            
         CLI   QMGR,C'F'                                                        
         BH    SP210                                                            
         CLC   QPGR,2(R3)                                                       
         BNE   SP210                                                            
         BCT   R5,*+6              SAVE PRDGRP EXCEPTIONS                       
         DC    H'0'                                                             
         MVC   0(2,R4),3(R3)                                                    
         LA    R4,2(R4)                                                         
*                                                                               
SP210    IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         B     SP208                                                            
*                                                                               
SP212    XC    KEY,KEY             BUILD MARKET GROUP ASSIGN TABLE              
         MVC   KEY(2),=X'0D82'                                                  
         MVC   KEY+2(3),BAGYMD     A-M/CLT                                      
         MVC   KEY+5(1),SVPGRID    PGRP ID (IF ANY)                             
         MVC   KEY+8(1),QMGR                                                    
         MVC   SVPGRKEY,KEY                                                     
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BE    SP230                                                            
         CLI   QMGR,C'F'                                                        
         BNH   MKTERR                                                           
***      BH    *+6                                                              
***      DC    H'0'                                                             
         MVC   KEY,KEYSAVE                                                      
         XC    KEY+3(2),KEY+3      REMOVE CLIENT                                
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE                                                   
         BNE   MKTERR                                                           
         BE    SP230                                                            
**NOP    DC    H'0'                NO MARKETS ASSIGNED                          
*                                                                               
SP220    GOTO1 SEQ                                                              
*                                                                               
SP230    CLC   KEY(9),KEYSAVE                                                   
         BNE   SP232                                                            
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,KEY+11         GET MKT NUM                                  
         AR    RE,RE               X 2                                          
         A     RE,AMGRTAB          + TABLE START                                
         MVC   0(2,RE),KEY+9       MOVE MKTGRP NUMBER                           
         B     SP220                                                            
*                                                                               
SP232    CLI   SVPGRID,0           TEST MKTGRPS BY PRDGRP                       
         BE    SP240                                                            
         OC    SVPGRPEX(2),SVPGRPEX     YES-TEST ANY PRDGRP EXCEPTIONS          
         BZ    SP240                                                            
         LA    R4,SVPGRPEX         YES-READ WITH PGRP EXCEPTIONS IN KEY         
         LA    R5,PEXMAX                                                        
*                                                                               
SP234    OC    0(2,R4),0(R4)                                                    
         BZ    SP240                                                            
         MVC   KEY,SVPGRKEY                                                     
         MVC   KEY+6(2),0(R4)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(9),KEYSAVE      TEST ANY RECORDS                             
         BNE   *+6                                                              
         DC    H'0'                YES-WE ONLY HAVE ONE MKTGRP TABLE !!         
         LA    R4,2(R4)                                                         
         BCT   R5,SP234                                                         
         B     SP240                                                            
         EJECT                                                                  
*                                                                               
* PRODUCT GROUP ASSIGN TABLE                                                    
*                                                                               
SP240    L     R1,APGRTAB          CLEAR PRODUCT GROUP ASSIGN TABLE             
         MVI   0(R1),X'99'                                                      
         MVC   1(255,R1),0(R1)                                                  
         MVC   256(256,R1),255(R1)                                              
*                                                                               
         CLI   QPGR,C' '           TEST PRDGRP REQUEST                          
         BE    SP300               NO - SKIP                                    
*                                                                               
         XC    KEY,KEY             BUILD PRODUCT GROUP ASSIGN TABLE             
         MVC   KEY(2),=X'0D81'                                                  
         MVC   KEY+2(3),BAGYMD                                                  
         MVC   KEY+5(1),QPGR                                                    
*                                                                               
SP250    GOTO1 HIGH                                                             
         CLC   KEY(6),KEYSAVE                                                   
         BE    SP270                                                            
         DC    H'0'                NO PRODUCTS ASSIGNED                         
*                                                                               
SP260    GOTO1 SEQ                                                              
*                                                                               
SP270    CLC   KEY(6),KEYSAVE                                                   
         BNE   SP300                                                            
         L     RF,ADCLT            FIND PRODUCT CODE FROM CLTHDR                
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
SP280    CLC   0(3,RF),KEY+8                                                    
         BE    SP290                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    SP280                                                            
         DC    H'0'                                                             
*                                                                               
SP290    ZIC   RE,3(RF)            PRODUCT CODE                                 
         AR    RE,RE               X 2                                          
         A     RE,APGRTAB          + TABLE START                                
         MVC   0(2,RE),KEY+6       MOVE PRDGRP NUMBER                           
         B     SP260                                                            
         EJECT                                                                  
*                                                                               
* DETERMINE REQUEST FILTERS - PRD,MKT/STA,MKTGRP                                
*                                                                               
SP300    MVI   QBPRD,0             PRODUCT CODE FILTER                          
         CLC   QPRD,=C'ALL'        TEST FOR ONE PRODUCT                         
         BE    SP315                                                            
         CLC   QPRD,=C'POL'                                                     
         BE    SP315                                                            
         CLI   QPRD,C'0'                                                        
         BNL   SP312                                                            
         L     RF,ADCLT            YES - FIND PRODUCT CODE FROM CLTHDR          
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
SP305    CLC   0(3,RF),QPRD                                                     
         BE    SP310                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    SP305                                                            
         DC    H'0'                NOT FOUND                                    
*                                                                               
SP310    MVC   QBPRD,3(RF)         PRODUCT CODE                                 
         B     SP315                                                            
*                                                                               
SP312    LA    R1,QPRD+2    WORK OUT NO OF DIGITS FOR PRDGRP FILTERING          
         LA    R0,2                                                             
*                                                                               
SP313    CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   SP314                                                            
         BCTR  R1,0                                                             
         BCT   R0,SP313                                                         
*                                                                               
SP314    STH   R0,QPGRLEN                                                       
*                                                                               
*                                                                               
SP315    XC    QBMKT,QBMKT         MARKET AND STATION FILTERS                   
         XC    QBSTA,QBSTA                                                      
         CLC   QMKT(3),=C'ALL'                                                  
         BE    SP320                                                            
         PACK  DUB,QMKT                                                         
         CVB   R0,DUB                                                           
         STCM  R0,3,QBMKT                                                       
*                                                                               
SP320    CLC   QSTA(3),=C'ALL'                                                  
         BE    SP331                                                            
         CLI   QSTA,C'0'                                                        
         BNL   SP325                                                            
         GOTO1 MSPACK,DMCB,QMKT,QSTA,WORK                                       
         MVC   QBSTA,WORK+2                                                     
         B     SP331                                                            
*                                                                               
SP325    LA    R1,QSTA+3    WORK OUT NO OF DIGITS FOR MKTGRP FILTERING          
         LA    R0,3                                                             
*                                                                               
SP330    CLI   0(R1),C'*'                                                       
         BE    *+12                                                             
         CLI   0(R1),C' '                                                       
         BNE   *+10                                                             
         BCTR  R1,0                                                             
         BCT   R0,SP330                                                         
         STH   R0,QMGRLEN                                                       
*                                                                               
* FIND OUT WHETHER TO PRINT BILL FORMULA PERCENT COMMENTS                       
*                                                                               
SP331    MVI   BFPCTCMT,C'N'                                                    
         CLI   RQGETBF,C'Y'        TEST BILL FORMULA ADJUST                     
         BNE   SP336                                                            
         CLI   ADJCMT,C'Y'         YES-TEST ADJUSTMENT COMMENTS                 
         BNE   SP336                                                            
         CLC   QPRD,=C'POL'            AND PRODUCT NOT POOL                     
         BE    SP336                                                            
         CLI   QOPT2,C'2'          YES                                          
         BE    SP334                                                            
         CLI   QOPT2,C'3'                                                       
         BE    SP334                                                            
         CLI   QBPRD,0                                                          
         BNE   SP332                                                            
         CLI   QOPT2,C'1'                                                       
         BE    SP336                                                            
         CLI   QOPT2,C'4'                                                       
         BE    SP336                                                            
         CLI   QOPT2,C'5'                                                       
         BE    SP336                                                            
         CLI   QOPT2,C'7'                                                       
         BE    SP336                                                            
         CLI   QOPT2,C'8'                                                       
         BE    SP336                                                            
*                                                                               
SP332    CLI   QBESTIM,0                                                        
         BNE   SP334                                                            
         CLI   QOPT2,C'4'                                                       
         BL    SP334                                                            
         CLI   QOPT2,C'7'                                                       
         BNH   SP336                                                            
*                                                                               
SP334    MVI   BFPCTCMT,C'Y'                                                    
         L     R1,=A(BILFOTAB)     CLEAR BILL FORMULA TABLE                     
         LA    RE,NBILFO                                                        
         XC    0(7,R1),0(R1)                                                    
         LA    R1,7(R1)                                                         
         BCT   RE,*-10                                                          
*                                                                               
SP336    B     SP340                                                            
         EJECT                                                                  
*                                                                               
* PRODUCT/ESTIMATE TABLE                                                        
*                                                                               
SP340    MVI   QBEST,1             DETERMINE ESTIMATE RANGE                     
         MVI   QBESTEND,255                                                     
         CLC   RQEST,=C'ALL'                                                    
         BE    SP345                                                            
         CLC   RQEST(2),=C'NO'                                                  
         BE    SP345                                                            
         PACK  DUB,RQEST                                                        
         CVB   RE,DUB                                                           
         STC   RE,QBEST                                                         
         STC   RE,QBESTEND                                                      
         CLC   RQESTEND,SPACES                                                  
         BE    SP345                                                            
         PACK  DUB,RQESTEND                                                     
         CVB   RE,DUB                                                           
         STC   RE,QBESTEND                                                      
*                                                                               
SP345    L     R1,AESTTAB          CLEAR ESTIMATE TABLE                         
         LA    R0,256                                                           
*                                                                               
SP350    XC    0(256,R1),0(R1)     CLEAR 256 BYTES FOR EACH BRAND               
         LA    R1,256(R1)                                                       
         BCT   R0,SP350                                                         
*                                                                               
         L     R6,ADCLT                                                         
         XC    KEY,KEY                                                          
         MVC   KEY(4),0(R6)                                                     
         MVI   KEY+4,C'A'          FORCE PAST CLTHDR                            
         LA    R2,3                                                             
         CLI   QBPRD,0             TEST FOR PRODUCT FILTER                      
         BE    *+14                                                             
         LA    R2,6                                                             
         MVC   KEY+4(3),QPRD                                                    
         GOTO1 HIGH                PRODUCT HEADER                               
         EX    R2,KEYCOMP                                                       
         BNE   EXIT                                                             
         B     SP362                                                            
*                                                                               
SP355    GOTO1 HIGH                                                             
         B     SP360                                                            
*                                                                               
SP357    GOTO1 SEQ                                                              
*                                                                               
SP360    EX    R2,KEYCOMP                                                       
         BNE   SP400                                                            
*                                                                               
SP362    OC    KEY+8(5),KEY+8      TEST BILL                                    
         BNZ   SP357               YES                                          
         CLI   KEY+7,0             TEST EST                                     
         BNE   SP365               YES                                          
         CLI   QPRD,C'0'           PRD HEADER - CHECK FOR PRDGRP FILTER         
         BL    SP357                                                            
         L     RF,ADCLT                         YES - FIND PRD CODE             
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
SP363    CLC   0(3,RF),KEY+4                                                    
         BE    SP364                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    SP363                                                            
         DC    H'0'                                                             
*                                                                               
SP364    ZIC   RE,3(RF)            INDEX INTO PRGGRP TABLE                      
         AR    RE,RE                                                            
         A     RE,APGRTAB                                                       
         UNPK  DUB,0(3,RE)                                                      
         LH    RE,QPGRLEN                                                       
         EX    RE,*+8              TEST FOR PRD IN REQ PRDGROUP                 
         B     *+10                                                             
         CLC   QPRD(0),DUB+3       ** EXECUTED                                  
         BNE   SP390               PRD NOT IN PRDGRP - READ NEXT PRD            
         B     SP357                                                            
*                                                                               
SP365    CLC   QBEST,KEY+7         CHECK ITS WITHIN REQ EST RANGE               
         BL    SP370                                                            
         BE    SP375                                                            
         MVC   KEY+7(1),QBEST      SKIP TO FIRST EST                            
         XC    KEY+8(5),KEY+8                                                   
         B     SP355                                                            
*                                                                               
SP370    CLC   QBESTEND,KEY+7                                                   
         BL    SP390                                                            
*                                                                               
SP375    GOTO1 GETEST                                                           
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
*                                                                               
         CLC   EEND,QSTART         EST END BEFORE REQ START                     
         BL    SP380                                                            
         CLC   ESTART,QEND         EST START AFTER REQ END                      
         BH    SP380                                                            
*                                                                               
         CLI   ESTFILT,C'Y'        TEST FOR ESTIMATE FILTERING                  
         BNE   SP379                                                            
         LA    R0,3                                                             
         LA    RE,RQESTEND                                                      
         LA    RF,EPROF                                                         
*                                                                               
SP376    CLI   0(RE),C'*'                                                       
         BE    SP378                                                            
         CLI   0(RE),C' '                                                       
         BE    SP378                                                            
         TM    0(RE),X'40'         TEST NEGATIVE FILTER                         
         BZ    SP377               YES                                          
         CLC   0(1,RE),0(RF)       POSITIVE FILTER MUST MATCH                   
         BNE   SP380                                                            
         B     SP378                                                            
*                                                                               
SP377    MVC   BYTE,0(RE)                                                       
         OI    BYTE,C' '           MAKE CHAR UPPER CASE FOR COMPARE             
         CLC   BYTE,0(RF)          NEGATIVE FILTER MUST NOT MATCH               
         BE    SP380                                                            
*                                                                               
SP378    LA    RE,1(RE)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,SP376                                                         
*                                                                               
SP379    MVC   BPRD,EPRDCD+1       EXTRACT PRODUCT NUMBER                       
         CLI   BPRD,0              TEST 0                                       
         BNE   *+10                                                             
         MVC   BPRD,EPRDCD         YES-USE FIRST BYTE (FOR WUNDERMAN)           
         ZIC   RE,BPRD             GET PRD NUM                                  
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         ZIC   R0,7(R6)            EST NUM                                      
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         A     RE,AESTTAB                                                       
         MVC   0(1,RE),7(R6)                                                    
*                                                                               
         CLI   GLAUSW,C'A'                                                      
         BNE   SP380                                                            
         BAS   RE,AUTH                                                          
         DROP  R6                                                               
*                                                                               
SP380    MVC   KEY+8(5),XFF        NEXT ESTIMATE                                
         B     SP355                                                            
*                                                                               
SP390    MVC   KEY+7(6),XFF        NEXT PRODUCT                                 
         B     SP355                                                            
*                                                                               
*                                                                               
KEYCOMP  CLC   KEY(0),KEYSAVE      ** EXECUTED                                  
         EJECT                                                                  
*                                                                               
* READ BUY RECORDS                                                              
*                                                                               
SP400    CLC   QAGY,=C'WW'         TEST FOR WUNDERMAN                           
         BNE   *+12                                                             
         CLI   B1XPROF+6,C'Y'      TEST READ WUNDERMAN FILE                     
         BE    WUNDER              YES                                          
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         XC    SVSTN,SVSTN                                                      
         LA    R0,BUYHOOK          MEDGETBY HOOK FOR PAY DATE LIMITS            
         ST    R0,SPOTHOOK                                                      
*                                                                               
         XC    KEY,KEY                                                          
         MVI   DMOUTBTS,X'FD'      DMOUTBTS - PASS DELETES                      
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)                                                     
         CLC   QPRD,=C'POL'                                                     
         BNE   SP410                                                            
         MVI   KEY+3,X'FF'                                                      
*                                                                               
SP410    GOTO1 HIGH                                                             
*                                                                               
SP411    CLC   KEY(3),KEYSAVE                                                   
         BNE   SP480                                                            
         CLI   QBPRD,0             TEST ONE PRODUCT ONLY                        
         BNE   SP413                                                            
         CLI   QPRD,C'0'           NO - TEST PRDGRP FILTERING                   
         BL    SP412                                                            
         CLI   KEY+3,X'FF'              YES - TEST PRODUCT POL                  
         BNL   SP480                          YES - FINISHED                    
         B     SP415                          NO  - OK                          
*                                                                               
SP412    CLI   KEY+10,0                 NO  - TEST ACTIVE POINTER               
         BNE   SP420                          NO - NEXT EST                     
         B     SP415                                                            
*                                                                               
SP413    CLC   QBPRD,KEY+3         FILTER ON PRODUCT                            
         BE    SP415                                                            
         BL    SP480                                                            
         MVC   KEY+3(1),QBPRD      SKIP TO REQ PRODUCT                          
         XC    KEY+4(9),KEY+4                                                   
         B     SP410                                                            
*                                                                               
SP415    OC    QBMKT,QBMKT         TEST ALL MARKETS                             
         BZ    SP417                                                            
         CLC   QBMKT,KEY+4           NO - FILTER ON MARKET                      
         BE    SP417                                                            
         BL    SP424               DONE WITH MARKET - NEXT PRODUCT              
         MVC   KEY+4(2),QBMKT      SKIP TO MARKET/STATION                       
         MVC   KEY+6(3),QBSTA                                                   
         XC    KEY+9(4),KEY+9                                                   
         B     SP410                                                            
*                                                                               
SP417    CLC   =C'ALL',QSTA        TEST FOR STATION FILTER                      
         BE    SP419                                                            
         CLI   QSTA,C'0'           TEST FOR NUMERIC STA (MKTGRP)                
         BL    SP418                                                            
         SR    RE,RE                                                            
         ICM   RE,3,KEY+4          INDEX INTO MARKET GRP ASSIGN TABLE           
         AR    RE,RE                                                            
         A     RE,AMGRTAB                                                       
         UNPK  DUB,0(3,RE)                                                      
         LH    RE,QMGRLEN                                                       
         EX    RE,*+8              TEST FOR MARKET IN REQ MARKET GROUP          
         B     *+10                                                             
         CLC   QSTA(0),DUB+3       ** EXECUTED                                  
         BNE   SP422               MARKET NOT IN MKTGRP - READ NEXT MKT         
         B     SP419                                                            
*                                                                               
SP418    CLC   QBSTA,KEY+6         TEST FOR RIGHT STATION                       
         BE    SP419                                                            
         BL    SP422               READ NEXT MARKET                             
         MVC   KEY+6(3),QBSTA      SKIP TO REQ STATION                          
         XC    KEY+9(4),KEY+9                                                   
         B     SP410                                                            
*                                                                               
SP419    ZIC   RE,KEY+3            PRODUCT                                      
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         ZIC   R0,KEY+9            ESTIMATE                                     
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         A     RE,AESTTAB                                                       
         CLI   0(RE),0             TEST ACTIVE                                  
         BNZ   SP425                                                            
*                                                                               
SP420    MVC   KEY+10(3),XFF       NEXT ESTIMATE                                
         B     SP410                                                            
*                                                                               
SP422    MVC   KEY+6(7),XFF        NEXT MARKET                                  
         B     SP410                                                            
*                                                                               
SP424    MVC   KEY+4(9),XFF        NEXT PRODUCT                                 
         B     SP410                                                            
*                                                                               
*                                                                               
SP425    MVC   AREC,ADBUY                                                       
         GOTO1 GET                                                              
         TM    DMCB+8,X'02'        TEST FOR DELETED RECORD                      
         BO    SP470                                                            
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         CLC   BUYMSTA(2),KEY+4    REJECT SPILL MARKET                          
         BNE   SP470                                                            
         CLI   RQGETBF,C'Y'        TEST BILL FORMULAE NEEDED                    
         BNE   SP432                                                            
         CLC   SVEST,BUYKEST       YES-TEST ESTIMATE CHANGE                     
         BE    SP432               NO                                           
         MVC   SVEST,BUYKEST       YES-                                         
         MVC   BEST,BUYKEST        SET THE ESTIMATE NUMBER                      
         MVI   BESTEND,0                                                        
*                                                                               
         MVC   BPRD,QBPRD          SET THE PRODUCT                              
         CLI   QBPRD,0                                                          
         BNE   SP431                                                            
         MVC   BPRD,KEY+3                                                       
         CLI   QPRD,C'0'                                                        
         BNL   SP431                                                            
         MVC   BPRD,BUYKPRD                                                     
*                                                                               
SP431    MVI   MODE,ESTFRST        CALL SGETBF TO GET THE BILL FORMULAE         
         GOTO1 GETBF                                                            
         MVI   MODE,PROCBUY                                                     
*                                                                               
SP432    MVI   NETWORK,C'N'                                                     
         OC    BUYMSTA(2),BUYMSTA  TEST NULL MARKET                             
         BNZ   SP434                                                            
         MVI   NETWORK,C'Y'                                                     
         B     SP434                                                            
******** SR    R0,R0               YES -                                        
******** LA    RE,BDELEM           LOOK FOR CANADIAN NETWORK STN ELEM           
********                                                                        
*SP433   CLI   0(RE),0                                                          
******** BNE   *+6                                                              
******** DC    H'0'                                                             
******** CLI   0(RE),X'68'                                                      
******** BNE   *+12                                                             
******** MVI   NETWORK,C'Y'        FOUND                                        
******** B     SP434                                                            
******** IC    R0,1(RE)                                                         
******** AR    RE,R0                                                            
******** B     SP433                                                            
*                                                                               
SP434    XC    APRD,APRD                                                        
         MVI   MEDSPTLN,0          INITIALIZE SPOT LENGTH                       
         CLI   PIGOPT,C'Y'         TEST REPORT PIGGYBACKS                       
         BNE   SP446                                                            
         CLI   KEY+3,X'FF'         YES - TEST POL BUY                           
         BE    SP436                                                            
         MVI   BPRD2,0             NON-POL -                                    
         CLI   BDTIME,0            TEST PIGGYBACK                               
         BE    SP446                                                            
         MVI   BDTIME,0            YES - FOOL MEDGETBY INTO SINGLE PRD          
         MVC   MEDBRAND,KEY+3            SET THE BRAND                          
         SR    R0,R0                                                            
         LA    RE,BDELEM                 FIND THE PARTNER PRODUCT               
*                                                                               
SP435    CLI   0(RE),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(RE),4                                                          
         BE    *+14                                                             
         IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     SP435                                                            
         USING PBELEM,RE                                                        
         MVC   BPRD2,PBPROD                                                     
         B     SP449                                                            
         DROP  RE                                                               
*                                                                               
SP436    MVI   MEDBRAND,X'FF'      POL BUY - LET MEDGETBY PROCESS ALL           
         L     R2,APSLIST                                                       
         XC    0(256,R2),0(R2)     CLEAR THE PRODUCT LIST                       
         SR    R0,R0                                                            
         LA    RE,BDELEM           SCAN THE BUY ELEMENTS                        
         USING REGELEM,RE                                                       
*                                                                               
SP437    CLI   0(RE),0                                                          
         BE    SP444                                                            
         CLI   0(RE),11            TEST POL BUY ELEMENT                         
         BL    SP443                                                            
         CLI   0(RE),13                                                         
         BH    SP443                                                            
         XC    HALF,HALF                                                        
         CLI   QBPRD,0             YES - TEST PRODUCT FILTER                    
         BE    SP438                                                            
         CLI   RLEN,14             YES - TEST ALLOCATED SPOT                    
         BL    SP443                                                            
         CLC   QBPRD,RPPRD         YES - MATCH THE FIRST PRODUCT                
         BE    SP440                                                            
         B     SP443                                                            
*                                                                               
SP438    MVI   HALF,X'FF'          ALL PRODUCTS -                               
         CLI   RLEN,10             TEST UNALLOCATED                             
         BE    SP441               YES                                          
*                                                                               
SP440    MVC   HALF(1),RPPRD       MOVE PRODUCT(S) TO PRODUCT LIST              
         CLI   RLEN,18             ENTRY                                        
         BNE   SP441                                                            
         MVC   HALF+1(1),RPPRD+4                                                
*                                                                               
SP441    L     R2,APSLIST          MOVE ENTRY TO PRODUCT LIST                   
*                                                                               
SP442    CLI   0(R2),0                                                          
         BNE   *+14                                                             
         MVC   0(2,R2),HALF                                                     
         B     SP443                                                            
         CLC   HALF,0(R2)                                                       
         BE    SP443                                                            
         LA    R2,2(R2)                                                         
         B     SP442                                                            
*                                                                               
SP443    IC    R0,1(RE)                                                         
         AR    RE,R0                                                            
         B     SP437                                                            
         DROP  RE                                                               
*                                                                               
SP444    L     R2,APSLIST                                                       
*                                                                               
SP445    CLI   0(R2),0             TEST ANY PRODUCTS                            
         BE    SP470                                                            
         ST    R2,APRD             YES - SET A(PRODUCT LIST)                    
         B     SP449                                                            
*                                                                               
*                                  NON-PIGGYBACK REPORTING -                    
SP446    CLI   QBPRD,0             TEST SINGLE PRODUCT                          
         BE    *+14                                                             
         MVC   MEDBRAND,QBPRD      YES - PROCESS IT ONLY                        
         B     SP448                                                            
         CLI   QPRD,C'0'           TEST PRODUCT GROUP FILTER                    
         BL    *+14                                                             
         MVC   MEDBRAND,KEY+3      YES - PROCESS THE KEYED BRAND ONLY           
         B     SP448                                                            
         L     R2,APSLIST                                                       
         XC    0(256,R2),0(R2)                                                  
         GOTO1 MEDPSL,DMCB,(R8),(R2)                                            
*                                                                               
SP447    CLI   0(R2),0             TEST EOL                                     
         BE    SP470                                                            
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
*                                                                               
SP448    CLI   MEDBRAND,X'FF'      TEST UNALLOCATED                             
         BNE   SP449                                                            
         MVI   MEDBRAND,219                                                     
*                                                                               
SP449    GOTO1 MEDGETBY,DMCB,(R8),0                                             
*                                                                               
         CLI   BFPCTCMT,C'Y'       TEST PRINTING BILL FORMULA PCTS              
         BNE   SP450                                                            
         GOTO1 SAVEBF,DMCB,MEDBRAND,BEST   YES-SAVE BILL FORMULA                
*                                                                               
SP450    SR    RE,RE               SET MARKET GROUP                             
         ICM   RE,3,BUYMSTA                                                     
         AR    RE,RE                                                            
         A     RE,AMGRTAB                                                       
         MVC   BMGR,0(RE)                                                       
*                                  SET THE PRODUCTS(S)                          
         CLI   PIGOPT,C'Y'         TEST REPORTING PIGGYBACKS                    
         BE    *+8                                                              
         MVI   BPRD2,0             NO - FORCE NO PARTNER PRODUCT                
         MVI   BPRD,X'FF'          TEST POL                                     
         CLC   QPRD,=C'POL'                                                     
         BE    SP451                                                            
         MVC   BPRD,MEDBRAND                                                    
         ICM   R1,15,APRD          TEST USING PRODUCT LIST                      
         BZ    SP451                                                            
         MVC   BPRD,0(R1)          YES - EXTRACT PRODUCT(S) FROM THERE          
         MVC   BPRD2,1(R1)                                                      
*                                                                               
SP451    L     R4,MEDAFRST                                                      
         L     R5,MEDNUMMO                                                      
*                                                                               
SP452    ICM   R1,15,4(R4)                                                      
         BZ    SP460                                                            
         USING MEDDATA,R1                                                       
         CLI   QOPT5,C'1'          TEST BILL                                    
         BH    SP454                                                            
         CLC   SVSTN,BUYMSTA+2     YES - TEST NEW STATION                       
         BE    SP454                                                            
         OC    SVSTN,SVSTN                                                      
         BZ    *+8                                                              
         BAS   RE,PUTDRIV          YES - PASS RECORDS TO DRIVER                 
         MVC   SVSTN,BUYMSTA+2                                                  
*                                                                               
SP454    XC    BFREC,BFREC         CLEAR BUFFALO REC                            
         MVC   BFMGR,BMGR          MKTGRP                                       
         MVC   BFMKTSTA,BUYMSTA    MKT/STA                                      
         CLI   NETWORK,C'Y'        TEST CANADIAN NETWORK                        
         BNE   *+10                                                             
         MVC   BFMKTSTA(2),=X'FFFE'                                             
         ZIC   RE,BPRD             PRDGRP                                       
         AR    RE,RE                                                            
         A     RE,APGRTAB                                                       
         MVC   BFPGR,0(RE)                                                      
         MVC   BFPRD,BPRD          PRODUCT                                      
         MVC   BFPRD2,BPRD2        PARTNER PRODUCT                              
         MVC   BFEST,QBESTIM                                                    
         CLI   ESTOTS,C'Y'         TEST FOR ESTIMATES SEPARATE                  
         BNE   *+10                                                             
         MVC   BFEST,BUYKEST                                                    
         MVC   BFPER,2(R4)         MONTH END DATE                               
         CLI   QOPT3,C'2'          TEST FOR PERIOD WANTED                       
         BNE   *+10                                                             
         MVC   BFPER,XFF           NO - 'ALL PERIODS'                           
         MVC   BFSPT,MEDBYSPT      SPOTS                                        
         CLI   QOPT5,C'4'          OPTION TO SUPPRESS ALL DOLLARS               
         BE    SP455                                                            
         MVC   BFORD,MEDBYGRS      ORDERED DOLLARS                              
         MVC   BFPAID,MEDBYPAY     PAID DOLLARS                                 
         CLI   QOPT1,C'N'          TEST FOR NET DOLLARS REQUESTED               
         BNE   *+16                                                             
         MVC   BFORD,MEDBYNET                                                   
         MVC   BFPAID,MEDBYNPY                                                  
         MVC   BFORDTX,MEDBYTAX    ORDERED DOLLARS TAX                          
         MVC   BFPAIDTX,MEDBYTXP   PAID DOLLARS TAX                             
*                                                                               
SP455    MVI   BFMAXTLV,0                                                       
         BAS   RE,PUTBUFF                                                       
         MVC   BFMAXTLV,TOTLEV                                                  
         BAS   RE,POSTOTS          POST TOTAL RECORDS                           
         CLI   ESTOTS,C'Y'         TEST FOR ESTIMATE TOTALS NEEDED              
         BNE   SP460                                                            
         MVI   BFEST,X'FF'         YES - POST EST TOTAL RECORDS                 
         MVC   BFPRD,BPRD                                                       
         MVC   BFMKTSTA,BUYMSTA                                                 
         CLI   NETWORK,C'Y'        TEST CANADIAN NETWORK                        
         BNE   *+10                                                             
         MVC   BFMKTSTA(2),=X'FFFE'                                             
         BAS   RE,PUTBUFF                                                       
         BAS   RE,POSTOTS                                                       
*                                                                               
*                                                                               
SP460    LA    R4,12(R4)                                                        
         BCT   R5,SP452                                                         
*                                                                               
         CLI   PIGOPT,C'Y'         TEST REPORTING PIGGYBACKS                    
         BNE   SP462                                                            
         ICM   R2,15,APRD          YES - TEST PRODUCT LIST                      
         BZ    SP470                     NO - FINISHED                          
         LA    R2,2(R2)                  YES - NEXT PRODUCT                     
         B     SP445                                                            
*                                                                               
SP462    CLI   QBPRD,0             TEST ONE PRODUCT                             
         BNE   SP470                                                            
         CLI   QPRD,C'0'           OR PRODUCT GROUP FILTER                      
         BNL   SP470                                                            
*                                                                               
SP465    LA    R2,2(R2)            NO - PSLIST ENTRY                            
         B     SP447                                                            
*                                                                               
SP470    GOTO1 SEQ                 READ NEXT BUY RECORD                         
         B     SP411                                                            
*                                                                               
SP480    MVI   MODE,CLTFRST        RE-SET THE MODE                              
         B     SP500                                                            
         EJECT                                                                  
*                                                                               
* MEDGETBY HOOK                                                                 
*                                                                               
         USING *,RF                                                             
BUYHOOK  NTR1                                                                   
         LM    R7,RC,BUYHKR7                                                    
         B     BH10                                                             
BUYHKR7  DC    6F'0'                                                            
         DROP  RF                                                               
*                                                                               
BH10     L     R6,SPOTADDR                                                      
         USING REGELEM,R6                                                       
         CLI   DATEFILT,C'P'       TEST PAY DATE FILTER                         
         BNE   BH20                                                             
         CLC   DBSTART,RPAY        TEST PAID WITHIN LIMITS                      
         BH    BHNO                                                             
         CLC   DBEND,RPAY                                                       
         BL    BHNO                                                             
*                                                                               
BH20     ICM   R1,15,APRD          TEST POL BUY, PIGGYBACK REQUEST              
         BZ    BHYES                                                            
         CLI   0(R6),11                                                         
         BL    BHNO                                                             
         CLI   0(R6),13                                                         
         BH    BHNO                                                             
         CLI   RLEN,10             TEST UNALLOCATED                             
         BH    *+16                                                             
         CLI   0(R1),X'FF'                                                      
         BE    BH30                                                             
         B     BHNO                                                             
         CLC   RPPRD,0(R1)         TEST FIRST PRODUCT MATCH                     
         BNE   BHNO                                                             
         CLI   RLEN,14             TEST SINGLE ALLOCATED PRODUCT                
         BH    *+16                                                             
         CLI   1(R1),0                                                          
         BE    BH30                                                             
         B     BHNO                                                             
         CLC   RPPRD+4(1),1(R1)    PIGGYBACKS                                   
         BNE   BHNO                                                             
*                                                                               
BH30     B     BHYES                                                            
*                                                                               
BHYES    MVI   SPOTYORN,C'Y'       ACCEPT                                       
         B     BHX                                                              
*                                                                               
BHNO     MVI   SPOTYORN,C'N'       REJECT                                       
*                                                                               
BHX      B     EXIT                                                             
         EJECT                                                                  
MKTERR   MVI   FORCEHED,C'Y'                                                    
         MVI   ANYDATA,C'N'        NO DATA YET                                  
         MVI   RQGETBF,C'N'                                                     
         XC    HEADHOOK,HEADHOOK                                                
         MVC   P(44),=C'** REQUEST CANCELLED - NO MARKETS IN GROUP**'           
         GOTO1 REPORT                                                           
         MVI   MODE,REQLAST                                                     
         B     EXIT                                                             
*                                                                               
* PROCESS WUNDERMAN BILLING RECORDS                                             
*                                                                               
WUNDER   LA    R2,WWBLOCK                                                       
         USING SPWWD,R2                                                         
         XC    WWBLOCK,WWBLOCK                                                  
         LA    R1,WWHOOK                                                        
         ST    R1,SPWWHOOK                                                      
         MVC   SPWWAIO,ADBUY                                                    
         L     R1,ADCLT                                                         
         MVC   SPWWAGMD(3),1(R1)   AM/CLT                                       
         LA    R3,254                                                           
         L     R4,AESTTAB                                                       
         LA    R5,1                R5=PRODUCT NUMBER                            
*                                                                               
WUN1     OC    0(256,R4),0(R4)                                                  
         BZ    WUN2                                                             
         MVI   BEST,0                                                           
         XC    ESTLST,ESTLST                                                    
         MVC   ESTLST+1(255),0(R4)                                              
         STC   R5,SPWWPRD                                                       
         MVC   SPWWSYM,REQMNTHS                                                 
         MVC   SPWWEYM,ENDYM                                                    
         MVC   SPWWIMKT,QBMKT                                                   
         MVC   SPWWISTA,QBSTA                                                   
         MVI   SPWWDATA,C'O'       ORDERED DOLLARS                              
         GOTO1 =V(SPB1WW),DMCB,(R8),(R2)                                        
         CLI   QOPT5,C'4'          OPTION TO SUPPRESS ALL DOLLARS               
         BE    WUN2                                                             
         MVI   SPWWDATA,C'P'       PAID DOLLARS                                 
         GOTO1 (RF),(R1),(R8),(R2)                                              
*                                                                               
WUN2     LA    R4,256(R4)          NEXT PRODUCT                                 
         LA    R5,1(R5)                                                         
         BCT   R3,WUN1             FOR ALL PRODUCTS                             
*                                                                               
         B     SP500                                                            
         SPACE 2                                                                
WWHOOK   NTR1                                                                   
         SR    RE,RE                                                            
         ICM   RE,3,SPWWMKT        INDEX INTO MARKET GRP ASSIGN TABLE           
         AR    RE,RE                                                            
         A     RE,AMGRTAB                                                       
         MVC   BMGR,0(RE)          SET THE MARKET GROUP                         
         CLC   =C'ALL',QSTA        TEST FOR STATION FILTER                      
         BE    WH2                                                              
         CLI   QSTA,C'0'           TEST FOR NUMERIC STA (MKTGRP)                
         BL    WH2                                                              
         UNPK  DUB,0(3,RE)                                                      
         LH    RF,QMGRLEN                                                       
         EX    RF,*+8              TEST FOR MARKET IN REQ MARKET GROUP          
         B     *+10                                                             
         CLC   QSTA(0),DUB+3       ** EXECUTED                                  
         BNE   WHX                 MARKET NOT IN MKTGRP - EXIT                  
*                                                                               
WH2      XC    BFREC,BFREC         CLEAR BUFFALO REC                            
         MVC   BFMGR,BMGR          MKTGRP                                       
         MVC   BFMKTSTA,SPWWMKT    MKT/STA                                      
         MVI   BPRD,X'FF'          SET THE PRODUCT                              
         CLC   QPRD,=C'POL'                                                     
         BE    *+10                                                             
         MVC   BPRD,SPWWPRD                                                     
         ZIC   RE,BPRD             PRDGRP                                       
         AR    RE,RE                                                            
         A     RE,APGRTAB                                                       
         MVC   BFPGR,0(RE)                                                      
         MVC   BFPRD,BPRD          PRODUCTS                                     
         MVI   BFPRD2,0                                                         
         MVC   BFEST,QBESTIM       ESTIMATE                                     
         CLI   ESTOTS,C'Y'         TEST FOR ESTIMATES SEPARATE                  
         BNE   *+10                                                             
         MVC   BFEST,SPWWESTO                                                   
         MVC   BFPER,XFF           'ALL PERIODS'                                
         CLI   QOPT3,C'2'          TEST FOR PERIOD WANTED                       
         BE    WH8                 NO                                           
         L     RF,MEDBUFF          YES-FIND THE MONTH END DATE                  
         L     R0,MEDNUMMO-MEDBLOCK(RF)                                         
         LA    R1,MEDMON01-MEDBLOCK(RF)                                         
         LA    RE,REQMNTHS                                                      
*                                                                               
WH4      CLC   SPWWYM,0(RE)                                                     
         BE    WH6                                                              
         LA    R1,12(R1)                                                        
         LA    RE,2(RE)                                                         
         BCT   R0,WH4                                                           
         B     WH14                                                             
*                                                                               
WH6      MVC   BFPER,2(R1)         MONTH END DATE                               
*                                                                               
WH8      CLI   SPWWDATA,C'O'       SPOTS                                        
         BNE   *+10                                                             
         MVC   BFSPT,SPWWSPTS                                                   
         CLI   QOPT5,C'4'          OPTION TO SUPPRESS ALL DOLLARS               
         BE    WH12                                                             
         CLI   SPWWDATA,C'O'       TEST ORDERED                                 
         BNE   WH10                                                             
         MVC   BFORD,SPWWGRS       ORDERED DOLLARS                              
         MVC   BFORDTX,SPWWTAX     ORDERED DOLLARS TAX                          
         CLI   QOPT1,C'N'          TEST FOR NET DOLLARS REQUESTED               
         BNE   *+10                                                             
         MVC   BFORD,SPWWNET                                                    
         B     WH12                                                             
*                                                                               
WH10     MVC   BFPAID,SPWWGRS      PAID DOLLARS                                 
         MVC   BFPAIDTX,SPWWTAX    PAID DOLLARS TAX                             
         CLI   QOPT1,C'N'          TEST FOR NET DOLLARS REQUESTED               
         BNE   *+10                                                             
         MVC   BFPAID,SPWWNET                                                   
*                                                                               
WH12     MVI   BFMAXTLV,0                                                       
         BAS   RE,PUTBUFF                                                       
         MVC   BFMAXTLV,TOTLEV                                                  
         BAS   RE,POSTOTS          POST TOTAL RECORDS                           
         CLI   ESTOTS,C'Y'         TEST FOR ESTIMATE TOTALS NEEDED              
         BNE   WH14                                                             
         MVI   BFEST,X'FF'         YES - POST EST TOTAL RECORDS                 
         MVC   BFPRD,BPRD                                                       
         MVC   BFMKTSTA,SPWWMKT                                                 
         CLI   NETWORK,C'Y'        TEST CANADIAN NETWORK                        
         BNE   *+10                                                             
         MVC   BFMKTSTA(2),=X'FFFE'                                             
         BAS   RE,PUTBUFF                                                       
         BAS   RE,POSTOTS                                                       
*                                                                               
WH14     B     WHX                                                              
*                                                                               
WHX      B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* ROUTINE TO WRITE TOTAL RECORDS TO BUFFALO                                     
*                                                                               
POSTOTS  NTR1                                                                   
         MVI   BFPRD,X'FE'         ALL PRODUCTS RECORD                          
         BAS   RE,PUTBUFF                                                       
         MVC   BFPRD,BPRD                                                       
         CLI   MODE,PROCGOAL       FOR GOALS, MARKET TOTAL ALREADY GONE         
         BE    PT010                                                            
*                                                                               
         MVC   BFMKTSTA+2(3),XFF   MARKET TOTAL RECORD                          
         BAS   RE,PUTBUFF                                                       
*                                                                               
         MVI   BFPRD,X'FE'         MARKET TOTAL / ALL PRODUCTS                  
         BAS   RE,PUTBUFF                                                       
         MVC   BFPRD,BPRD                                                       
*                                                                               
PT010    MVC   BFMKTSTA(2),XFF     ALL MARKETS RECORD                           
         BAS   RE,PUTBUFF                                                       
*                                                                               
         MVI   BFPRD,X'FE'         ALL MARKETS / ALL PRODUCTS                   
         BAS   RE,PUTBUFF                                                       
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* READ GOAL RECORDS                                                             
*                                                                               
SP500    L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         CLI   GLAUSW,C'G'         TEST GOAL DOLLARS NEEDED                     
         BNE   SP600                                                            
         MVI   MODE,PROCGOAL       SET MODE TO PROCESS GOALS                    
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVI   KEY,2                                                            
         MVC   KEY+1(3),1(R6)                                                   
*                                                                               
SP510    GOTO1 HIGH                                                             
*                                                                               
SP512    CLC   KEY(4),KEYSAVE                                                   
         BNE   SP600                                                            
         CLI   KEY+4,X'FF'         TEST FOR CPP GUIDE                           
         BE    SP600                                                            
         CLI   QBPRD,0             TEST ALL PRODUCTS                            
         BE    SP515                                                            
         CLC   QBPRD,KEY+4         NO - FILTER ON PRODUCT                       
         BE    SP515                                                            
         BL    SP600                                                            
         MVC   KEY+4(1),QBPRD      SKIP TO REQ PRODUCT                          
         XC    KEY+5(8),KEY+5                                                   
         B     SP510                                                            
*                                                                               
SP515    OC    QBMKT,QBMKT         TEST ALL MARKETS                             
         BZ    SP517                                                            
         CLC   QBMKT,KEY+5           NO - FILTER ON MARKET                      
         BE    SP517                                                            
         BL    SP530               DONE WITH MARKET - NEXT PRODUCT              
         MVC   KEY+5(2),QBMKT      SKIP TO MARKET START                         
         XC    KEY+7(6),KEY+7                                                   
         B     SP510                                                            
*                                                                               
SP517    CLI   QSTA,C'0'           TEST FOR NUMERIC STA (MKTGRP)                
         BL    SP519                                                            
         SR    RE,RE                                                            
         ICM   RE,3,KEY+5          INDEX INTO MARKET GRP ASSIGN TABLE           
         AR    RE,RE                                                            
         A     RE,AMGRTAB                                                       
         UNPK  DUB,0(3,RE)                                                      
         LH    RE,QMGRLEN                                                       
         EX    RE,*+8              TEST FOR MARKET IN REQ MARKET GROUP          
         B     *+10                                                             
         CLC   QSTA(0),DUB+3       ** EXECUTED                                  
         BNE   SP525               MARKET NOT IN MKTGRP - READ NEXT MKT         
*                                                                               
SP519    ZIC   RE,KEY+4            PRODUCT                                      
         BCTR  RE,0                                                             
         SLL   RE,8                X 256                                        
         ZIC   R0,KEY+7            ESTIMATE                                     
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         A     RE,AESTTAB                                                       
         CLI   0(RE),0             TEST ACTIVE                                  
         BNZ   SP540                                                            
*                                                                               
SP520    MVC   KEY+8(5),XFF        NEXT ESTIMATE                                
         B     SP510                                                            
*                                                                               
SP525    MVC   KEY+7(6),XFF        NEXT MARKET                                  
         B     SP510                                                            
*                                                                               
SP530    MVC   KEY+5(8),XFF        NEXT PRODUCT                                 
         B     SP510                                                            
*                                                                               
*                                                                               
SP540    MVC   AREC,ADGOAL                                                      
         GOTO1 GET                                                              
         L     R6,ADGOAL                                                        
         USING GOALRECD,R6                                                      
         MVC   MEDSPTLN,GKEYSLN                                                 
         GOTO1 MEDGETGL,DMCB,(R8)                                               
*                                                                               
         SR    RE,RE               SET THE MARKET GROUP                         
         ICM   RE,3,GKEYMKT                                                     
         AR    RE,RE                                                            
         A     RE,AMGRTAB                                                       
         MVC   BMGR,0(RE)                                                       
*                                                                               
         MVC   BPRD,GKEYPRD        SET THE PRODUCT                              
         MVI   BPRD2,0                                                          
         CLC   QPRD,=C'POL'                                                     
         BNE   *+12                                                             
         MVI   BPRD,X'FF'                                                       
         B     SP542                                                            
         CLI   PIGOPT,C'Y'         TEST REPORTING PIGGYBACKS                    
         BNE   SP542                                                            
         MVC   BPRD2,GKEYPRD2      YES - SET PARTNER PROD (IF ANY)              
*                                                                               
SP542    L     R4,MEDAFRST                                                      
         L     R5,MEDNUMMO                                                      
*                                                                               
SP550    ICM   R1,15,4(R4)                                                      
         BZ    SP560                                                            
         USING MEDDATA,R1                                                       
         OC    MEDGLD,MEDGLD                                                    
         BZ    SP560                                                            
         XC    BFREC,BFREC         CLEAR BUFFALO REC                            
         MVC   BFMGR,BMGR          MARKET GROUP                                 
         MVC   BFMKTSTA(2),GKEYMKT MARKET                                       
         MVC   BFMKTSTA+2(3),XFF   DUMMY STATION                                
         ZIC   RE,BPRD             PRDGRP                                       
         AR    RE,RE                                                            
         A     RE,APGRTAB                                                       
         MVC   BFPGR,0(RE)                                                      
         MVC   BFPRD,BPRD          PRODUCT(S)                                   
         MVC   BFPRD2,BPRD2                                                     
         MVC   BFEST,QBESTIM                                                    
         CLI   ESTOTS,C'Y'         TEST FOR ESTIMATES SEPARATE                  
         BNE   *+10                                                             
         MVC   BFEST,GKEYEST                                                    
         MVC   BFPER,2(R4)         MONTH END DATE                               
         CLI   QOPT3,C'2'          TEST FOR PERIOD WANTED                       
         BNE   *+10                                                             
         MVC   BFPER,XFF           NO - 'ALL PERIODS'                           
         MVC   BFGOALD,MEDGLD      GOAL DOLLARS                                 
         MVI   BFMAXTLV,0                                                       
         BAS   RE,PUTBUFF                                                       
         MVC   BFMAXTLV,TOTLEV                                                  
         BAS   RE,POSTOTS          POST THE TOTAL RECS                          
         CLI   ESTOTS,C'Y'         TEST FOR ESTIMATE TOTALS NEEDED              
         BNE   SP560                                                            
         MVI   BFEST,X'FF'         YES - POST EST TOTAL RECORDS                 
         MVC   BFPRD,BPRD                                                       
         MVC   BFMKTSTA(2),GKEYMKT                                              
         BAS   RE,PUTBUFF                                                       
         BAS   RE,POSTOTS                                                       
*                                                                               
SP560    LA    R4,12(R4)                                                        
         BCT   R5,SP550                                                         
*                                                                               
         GOTO1 SEQ                 READ NEXT GOAL RECORD                        
         B     SP512                                                            
         EJECT                                                                  
*                                                                               
* READ STATION BILL RECORDS                                                     
*                                                                               
SP600    CLI   QOPT5,C'1'          TEST IF BILL DOLLARS NEEDED                  
         BH    SP699                                                            
         MVI   MODE,CLTLAST        (NOT PROCESSING GOALS)                       
         MVI   SVEST,0                                                          
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(2),=X'0E01'                                                  
         MVC   KEY+2(3),1(R6)                                                   
*                                                                               
SP610    GOTO1 HIGH                                                             
*                                                                               
SP612    CLC   KEY(5),KEYSAVE                                                   
         BNE   SP699                                                            
         CLC   KEY+7(2),=X'270E'   IGNORE MARKET 9998                           
         BNL   SP625                                                            
         CLI   QBPRD,0                                                          
         BE    SP615                                                            
         CLC   QBPRD,KEY+5                                                      
         BE    SP615                                                            
         BL    SP699                                                            
         MVC   KEY+5(1),QBPRD                                                   
         XC    KEY+6(7),KEY+6                                                   
         B     SP610                                                            
*                                                                               
SP615    ZIC   RE,KEY+5                                                         
         BCTR  RE,0                                                             
         SLL   RE,8                                                             
         ZIC   R0,KEY+6                                                         
         BCTR  R0,0                                                             
         AR    RE,R0                                                            
         A     RE,AESTTAB                                                       
         CLI   0(RE),0                                                          
         BE    SP625                                                            
*                                                                               
         OC    QBMKT,QBMKT         TEST FOR ALL MARKETS                         
         BZ    SP617                                                            
         CLC   QBMKT,KEY+7         NO - FILTER ON MARKET                        
         BE    SP617                                                            
         BL    SP625               DONE WITH MARKET - NEXT ESTIMATE             
         MVC   KEY+7(2),QBMKT      SKIP TO MARKET/STATION                       
         MVC   KEY+9(3),QBSTA                                                   
         B     SP610                                                            
*                                                                               
SP617    CLC   =C'ALL',QSTA        TEST FOR STATION FILTER                      
         BE    SP640                                                            
         CLI   QSTA,C'0'           TEST FOR NUMERIC STA (MKTGRP)                
         BL    SP618                                                            
         SR    RE,RE                                                            
         ICM   RE,3,KEY+7          INDEX INTO MARKET GRP ASSIGN TABLE           
         AR    RE,RE                                                            
         A     RE,AMGRTAB                                                       
         UNPK  DUB,0(3,RE)                                                      
         LH    RE,QMGRLEN                                                       
         EX    RE,*+8              TEST FOR MARKET IN REQ MARKET GROUP          
         B     *+10                                                             
         CLC   QSTA(0),DUB+3       ** EXECUTED                                  
         BNE   SP630               MARKET NOT IN MKTGRP - READ NEXT MKT         
         B     SP640                                                            
*                                                                               
SP618    CLC   QBSTA,KEY+9         TEST FOR RIGHT STATION                       
         BE    SP640                                                            
         BL    SP630               READ NEXT MARKET                             
         MVC   KEY+9(3),QBSTA      SKIP TO REQ STATION                          
         B     SP610                                                            
*                                                                               
SP625    MVC   KEY+7(6),XFF        NEXT EST                                     
         B     SP610                                                            
*                                                                               
SP630    MVC   KEY+9(4),XFF        NEXT MKT                                     
         B     SP610                                                            
*                                                                               
SP640    L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         USING STABUCKD,R6                                                      
         GOTO1 GET                                                              
         CLI   RQGETBF,C'Y'        TEST BILL FORMULAE NEEDED                    
         BNE   SP646                                                            
         CLC   SVEST,STABKEST      YES-TEST ESTIMATE CHANGE                     
         BE    SP646               NO                                           
         MVC   SVEST,STABKEST      YES-                                         
         MVC   BEST,STABKEST       SET THE ESTIMATE NUMBER                      
         MVI   BESTEND,0                                                        
         MVC   BPRD,STABKPRD       SET THE PRODUCT                              
         CLI   QBESTIM,0           TEST SINGLE ESTIMATE REQUEST                 
         BE    SP644                                                            
         CLI   QBPRD,0             YES-TEST SINGLE PRODUCT REQUEST              
         BNE   SP644               YES                                          
         MVI   BPRD,X'FF'          NO-READ ALL PRODUCTS                         
*                                                                               
SP644    MVI   MODE,ESTFRST                                                     
         GOTO1 GETBF                                                            
         MVI   MODE,CLTLAST                                                     
*                                                                               
SP646    MVC   SVMSTA,STABKMKT     SAVE THE MKT/STA                             
         OC    SVMSTA(2),SVMSTA    TEST NULL MARKET                             
         BNZ   SP648                                                            
         GOTO1 MSUNPK,DMCB,BFMKTSTA,WORK,WORK+4                                 
         CLC   WORK+4(4),=C'CIII'        YES - TEST STATION IS CANADIAN         
******** BE    *+6                             GLOBAL NETWORK                   
         B     *+6                             GLOBAL NETWORK                   
         DC    H'0'                                                             
         MVC   SVMSTA(2),=X'FFFE'                                               
*                                                                               
SP648    SR    RE,RE               SET THE MARKET GROUP                         
         ICM   RE,3,STABKMKT                                                    
         AR    RE,RE                                                            
         A     RE,AMGRTAB                                                       
         MVC   BMGR,0(RE)                                                       
         MVC   BPRD,STABKPRD       SET THE PRODUCT                              
         CLC   QPRD,=C'POL'                                                     
         BNE   *+8                                                              
         MVI   BPRD,X'FF'                                                       
         MVC   BEST,STABKEST       SET THE ESTIMATE                             
*                                                                               
         XC    BFREC,BFREC         START TO POST BUFFALO RECORD                 
         MVC   BFMGR,BMGR          MARKET GROUP                                 
         ZIC   RE,BPRD             PRDGRP                                       
         AR    RE,RE                                                            
         A     RE,APGRTAB                                                       
         MVC   BFPGR,0(RE)                                                      
*                                                                               
         LA    R5,STABELEM         SEARCH FOR ELEMNTS IN REQUEST PERIOD         
         USING STABELEM,R5                                                      
         SR    R0,R0                                                            
*                                                                               
SP650    CLI   0(R5),0                                                          
         BE    SP695                                                            
         CLI   0(R5),X'0E'                                                      
         BE    SP670                                                            
*                                                                               
SP660    IC    R0,1(R5)                                                         
         LTR   R0,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R5,R0                                                            
         B     SP650                                                            
*                                                                               
SP670    CLC   STABPER,BILLST      CHECK PERIOD IS IN REQUEST PERIOD            
         BL    SP660                                                            
         CLC   STABPER,BILLEND                                                  
         BH    SP660                                                            
         CLI   DATEFILT,C'B'       TEST FOR BILLING DATE LIMITS                 
         BNE   SP675                                                            
         CLC   STABBDT,DBSTART     YES - CHECK BILLING DATE                     
         BL    SP660                                                            
         CLC   STABBDT,DBEND                                                    
         BH    SP660                                                            
*                                                                               
SP675    LA    R4,BILLPER                                                       
*                                                                               
SP680    CLC   STABPER,0(R4)       MATCH PERIOD NUMBERS                         
         BE    SP690                                                            
         LA    R4,6(R4)                                                         
         CLI   0(R4),0                                                          
         BE    SP660                                                            
         B     SP680                                                            
*                                                                               
SP690    MVC   BFMKTSTA,SVMSTA     MARKET/STATION                               
         MVC   BFPRD,BPRD          PRODUCT                                      
         MVC   BFEST,QBESTIM                                                    
         CLI   ESTOTS,C'Y'         TEST FOR ESTIMATES SEPARATE                  
         BNE   *+10                                                             
         MVC   BFEST,BEST                                                       
         MVC   BFPER,4(R4)         PERIOD END                                   
         CLI   QOPT3,C'2'          TEST FOR PERIOD WANTED                       
         BNE   *+10                                                             
         MVC   BFPER,XFF           NO - 'ALL PERIODS'                           
         XC    SVBFORM,SVBFORM                                                  
         CLI   RQGETBF,C'Y'        TEST BILL FORMULA ADJUST                     
         BNE   SP692                                                            
         XC    COSTAREA,COSTAREA   YES, CALL GETBF, BUT ONLY                    
         MVI   MODE,PROCBUY        TO GET FORMULA, NOT AMOUNT                   
         GOTO1 GETBF,DMCB,(STABKPRD,COSTAREA),SVBFORM                           
         MVI   MODE,CLTLAST                                                     
         CLI   BFPCTCMT,C'Y'       TEST PRINTING BILL FORMULA PCTS              
         BNE   SP692                                                            
         GOTO1 SAVEBF,DMCB,STABKPRD,STABKEST  YES-SAVE THE BILL FORMULA         
*                                                                               
SP692    DS    0H                                                               
         GOTO1 SPBVAL,DMCB,(C'E',STABELEM),SPBVALD,SVBFORM                      
*                                                                               
         MVC   BFBILL,SPBVACT      USE ACTUAL                                   
         CLI   RQGETBF,C'Y'        IF DOING BILL FORMS                          
         BE    SP692B                                                           
         MVC   BFBILL,SPBVEGRS     ELSE EFFECTIVE GROSS                         
         CLI   QOPT1,C'N'                                                       
         BNE   *+10                                                             
         MVC   BFBILL,SPBVENET     OR EFFECTIVE NET                             
*                                                                               
SP692B   DS    0H                                                               
         MVC   BFBILLTX,SPBVETAX   EFFECTIVE TAX                                
         MVI   BFMAXTLV,0                                                       
         BAS   RE,PUTBUFF                                                       
         MVC   BFMAXTLV,TOTLEV                                                  
         BAS   RE,POSTOTS          POST TOTAL RECORDS                           
         CLI   ESTOTS,C'Y'         TEST FOR ESTIMATE TOTALS NEEDED              
         BNE   SP693                                                            
         MVI   BFEST,X'FF'         YES - POST EST TOTAL RECORDS                 
         MVC   BFPRD,BPRD                                                       
         MVC   BFMKTSTA,SVMSTA                                                  
         BAS   RE,PUTBUFF                                                       
         BAS   RE,POSTOTS                                                       
*                                                                               
SP693    B     SP660               LOOK FOR MORE ELEMENTS                       
*                                                                               
SP695    GOTO1 SEQ                 READ MORE BILLS                              
         B     SP612                                                            
*                                                                               
SP699    MVI   MODE,CLTLAST        FOOL SPONSOR                                 
         B     EXIT                END EXIT                                     
         EJECT                                                                  
*                                                                               
* REQLAST - OUTPUT STAGE                                                        
*                                                                               
SP700    CLI   QOPT2,C'0'                                                       
         BNH   EXIT                                                             
         L     R2,AGLAREA                                                       
         USING GLOBALD,R2                                                       
         BAS   RE,PUTDRIV          PUT REMAINING RECORDS TO DRIVER              
         CLI   ANYDATA,C'Y'        TEST FOR ANY DATA                            
         BNE   EXIT                NO - EXIT                                    
         MVI   GLMODE,GLOUTPUT     DRIVER OUTPUT PHASE                          
         CLI   SDTAX,C'T'          TEST FOR TAX                                 
         BNE   *+8                                                              
         MVI   GLSPACE,2           YES - SET SPACING OF DETAILS                 
         CLI   QRECORD+30,C'D'     TEST FOR DOWNLOAD OPTION                     
         BNE   *+8                                                              
         OI    GLDOWNLD,X'80'                                                   
         MVI   MODE,REQFRST        FUDGE THE MODE TO PRINT HEADLINES            
         XC    ATAXLIT,ATAXLIT                                                  
         XC    ATOTLIT,ATOTLIT                                                  
         GOTO1 ADRIVER,DMCB,AGLAREA                                             
         MVI   MODE,REQLAST        RESTORE THE MODE                             
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT                                                                  
AUTH     NTR1                                                                   
*                                                                               
*        ROUTINE TO POST AUTHORIZED DOLLARS                                     
*                                                                               
         L     R6,ADEST                                                         
         USING ESTHDRD,R6                                                       
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         GOTO1 GETBROAD,DMCB,ESTART,WORK                                        
         GOTO1 DATCON,DMCB,WORK+6,(3,DUB)   GET END DATE IN BINARY              
         MVC   HALF(2),DUB                  SAVE START Y/M                      
* 11 MONTHS HENCE IS LAST Y/M                                                   
         GOTO1 ADDAY,DMCB,(C'M',WORK+6),WORK+12,F'11'                           
         GOTO1 DATCON,DMCB,WORK+12,(3,DUB)                                      
         MVC   HALF2(2),DUB                                                     
*                                                                               
         MVC   FULL(2),HALF                                                     
         MVI   FULL+2,15                                                        
         XC    BFREC,BFREC         CLEAR BUFFALO REC                            
         MVC   BFMKTSTA,XFF        DUMMY MKT/STA                                
         CLC   QPRD,=C'POL'                                                     
         BNE   AD010                                                            
         MVI   BPRD,X'FF'                                                       
*                                                                               
AD010    MVC   BFPRD,BPRD          PRODUCT                                      
         ZIC   RE,BPRD             PRDGRP                                       
         AR    RE,RE                                                            
         A     RE,APGRTAB                                                       
         MVC   BFPGR,0(RE)                                                      
         MVC   BFEST,QBESTIM       ESTIMATE                                     
         CLI   ESTOTS,C'Y'         TEST FOR SEPARATE ESTIMATES                  
         BNE   *+10                                                             
         MVC   BFEST,EKEYEST       YES - ESTIMATE FROM ESTHDR                   
         LA    R4,MEDMON01                                                      
         L     R5,MEDNUMMO                                                      
         LA    RF,REQMNTHS                                                      
*                                                                               
AD020    CLC   0(2,RF),FULL        MATCH TO MONTH IN PROCESS                    
         BE    AD030                                                            
         LA    R2,48(R2)                                                        
         LA    R4,12(R4)                                                        
         LA    RF,2(RF)                                                         
         BCT   R5,AD020                                                         
         B     AD040                                                            
*                                                                               
AD030    ZIC   R5,FULL+1           GET MONTH NUM                                
         BCTR  R5,0                                                             
         MHI   R5,6                                                             
         LA    R5,EAUTH(R5)                                                     
         ZAP   DUB,0(6,R5)                                                      
         CVB   R0,DUB                                                           
         ST    R0,BFAUTHD          POST AUTH DOLLARS                            
         MVC   BFPER,2(R4)         POST END OF PERIOD                           
         CLI   QOPT3,C'2'          TEST FOR PERIOD WANTED                       
         BNE   *+10                                                             
         MVC   BFPER,XFF           NO - 'ALL PERIODS'                           
         MVI   BFMAXTLV,0                                                       
         BAS   RE,PUTBUFF          WRITE BUFFALO                                
         MVC   BFMAXTLV,TOTLEV                                                  
         MVI   BFPRD,X'FE'                                                      
         BAS   RE,PUTBUFF          ALL PRODUCTS                                 
         CLI   ESTOTS,C'Y'         TEST FOR ALL ESTIMATES                       
         BNE   AD040                                                            
         MVI   BFEST,X'FF'         YES -                                        
         BAS   RE,PUTBUFF          ALL PRODUCTS / ALL ESTIMATES                 
         MVC   BFPRD,BPRD                                                       
         BAS   RE,PUTBUFF          PRODUCT / ALL ESTIMATES                      
*                                                                               
AD040    ZIC   RE,FULL+1                                                        
         LA    RE,1(RE)                                                         
         STC   RE,FULL+1                                                        
         CLI   FULL+1,12                                                        
         BNH   AD050                                                            
         MVI   FULL+1,1                                                         
         IC    RE,FULL                                                          
         LA    RE,1(RE)                                                         
         STC   RE,FULL                                                          
*                                                                               
AD050    CLC   FULL(2),HALF2       TEST PAST EST END                            
         BNH   AD010                                                            
         B     EXIT                                                             
         EJECT                                                                  
*        FIND START OF NEW YEAR                                                 
*        1) A PERIOD THAT SPANS YEAR CHANGE                                     
*           AND BEGINS NO FURTHER AWAY                                          
*           FROM 12/31 THAN IT ENDS                                             
*   OR   2) A PERIOD THAT STARTS BEFORE 1/14                                    
*                                                                               
CKNEWYR  DS    0H                                                               
         MVC   DUB(4),0(R4)                                                     
         NI    DUB,X'01'           STRIP YEAR                                   
         CLC   DUB(2),NEWYRLO                                                   
         BL    CKNYYES                                                          
*                                                                               
         CLC   DUB(2),PDDEC                                                     
         BNH   CKNYNO                                                           
*                                                                               
         NI    DUB+2,X'01'                                                      
         CLC   DUB+2(2),PDDEC                                                   
         BH    CKNYNO                                                           
*                                                                               
         NI    DUB+1,X'1F'         ISOLATE DAY                                  
         ZIC   RF,DUB+1                                                         
         LA    R0,30                                                            
         SR    R0,RF                                                            
         BNP   CKNYYES             STARTS ON 30TH OR 31ST                       
         STC   R0,DUB+4                                                         
*                                                                               
         NI    DUB+3,X'1F'         ISOLATE DAY                                  
         CLC   DUB+4(1),DUB+3                                                   
         BNH   CKNYYES                                                          
*                                                                               
CKNYNO   DS    0H                                                               
         LTR   RE,RE                                                            
         BR    RE                                                               
CKNYYES  DS    0H                                                               
         SR    R0,R0                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
NEWYRLO  DC    X'002E'             JAN14                                        
PDDEC    DC    X'0180'             DEC00                                        
         EJECT                                                                  
* ROUTINE TO SAVE BILL FORMULAE                                                 
*                                                                               
SAVEBF   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         L     R4,=A(BILFOTAB)                                                  
         LA    R5,NBILFO                                                        
*                                                                               
SAVEBF2  OC    0(7,R4),0(R4)                                                    
         BZ    SAVEBF6                                                          
         CLC   0(1,R4),0(R2)                                                    
         BNE   SAVEBF4                                                          
         CLC   1(1,R4),0(R3)                                                    
         BE    SAVEBFX                                                          
*                                                                               
SAVEBF4  LA    R4,7(R4)                                                         
         BCT   R5,SAVEBF2                                                       
         DC    H'0'                TABLE FULL                                   
*                                                                               
SAVEBF6  MVC   0(1,R4),0(R2)                                                    
         MVC   1(1,R4),0(R3)                                                    
         MVC   2(5,R4),SVBFORM                                                  
*                                                                               
SAVEBFX  B     EXIT                                                             
         EJECT                                                                  
DRHOOK   NTR1                                                                   
*                                                                               
*        DRIVER HOOK ROUTINE                                                    
*                                                                               
         L     R6,AGLAREA                                                       
         USING GLOBALD,R6                                                       
         CLI   GLHOOK,GLROUT       TEST TO EXECUTE USER ROUTINE                 
         BNE   DH005                                                            
         CLI   GLMODE,GLINPUT      YES - INPUT PHASE                            
         BE    DH100                                                            
         CLI   GLMODE,GLOUTPUT           OUTPUT PHASE                           
         BE    DH200                                                            
         DC    H'0'                                                             
*                                                                               
DH005    CLI   GLHOOK,GLPRINT      TEST ABOUT TO PRINT A LINE                   
         BNE   DH007                                                            
         CLI   PRTSW,C'N'          YES - TEST FOR PRINT SUPPRESS                
         BNE   EXIT                                                             
         MVI   GLHOOK,GLDONT                                                    
         MVI   PRTSW,C'Y'                                                       
         B     EXIT                                                             
*                                                                               
DH007    CLI   GLHOOK,GLRESOLV     TEST TO RESOLVE LABELS                       
         BE    DH010                                                            
         CLI   GLHOOK,GLRESLIT     TEST TO RESOLVE LITERAL                      
         BE    DH300                                                            
         CLI   GLHOOK,GLFIRST      TEST FOR FIRST TIME CONTROL                  
         BE    DH400                                                            
         CLI   GLHOOK,GLHEAD       TEST FOR HEADHOOK                            
         BE    DH500                                                            
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* RESOLVE ROUTINE LABELS                                                        
*                                                                               
DH010    LA    R1,ROUTLIST                                                      
         LA    R2,GLLABEL                                                       
*                                                                               
DH020    CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         CLC   0(8,R1),0(R2)                                                    
         BE    DH030                                                            
         LA    R1,12(R1)                                                        
         B     DH020                                                            
*                                                                               
DH030    MVC   GLAROUT,8(R1)                                                    
         B     EXIT                                                             
         SPACE 2                                                                
ROUTLIST DS    0F                                                               
         DC    C'PRODUCT ',A(PROD)                                              
         DC    C'PRODO   ',A(PRODO)                                             
         DC    C'ESTIMO  ',A(ESTIMO)                                            
         DC    C'ESTIMI  ',A(ESTIMI)                                            
         DC    C'MKTGRP1I',A(MGR1I)                                             
         DC    C'MKTGRP2I',A(MGR2I)                                             
         DC    C'MKTGRP3I',A(MGR3I)                                             
         DC    C'PRDGRP1I',A(PGR1I)                                             
         DC    C'PRDGRP2I',A(PGR2I)                                             
         DC    C'MARKETI ',A(MARKETI)                                           
         DC    C'MARKETO ',A(MARKETO)                                           
         DC    C'STATIONI',A(STATIONI)                                          
         DC    C'STATIONO',A(STATIONO)                                          
         DC    C'PERIODI ',A(PERIODI)                                           
         DC    C'PERIODO ',A(PERIODO)                                           
         DC    C'SPOTS   ',A(SPOT)                                              
         DC    C'GOALD   ',A(GOALD)                                             
         DC    C'AUTHD   ',A(AUTHD)                                             
         DC    C'ORDERD  ',A(ORDERD)                                            
         DC    C'ORDERTX ',A(ORDERTX)                                           
         DC    C'ORDERTXO',A(ORDERTXO)                                          
         DC    C'PAYD    ',A(PAYD)                                              
         DC    C'PAYTX   ',A(PAYTX)                                             
         DC    C'BILLD   ',A(BILLD)                                             
         DC    C'BILLTX  ',A(BILLTX)                                            
         DC    C'TAXOBL  ',A(TAXOBL)                                            
         DC    C'TAXOUT  ',A(TAXOUT)                                            
         DC    C'PAID    ',A(PAID)                                              
         DC    C'UNPAID  ',A(UNPAID)                                            
         DC    C'BILLMTAX',A(BILLMTAX)                                          
         DC    C'BILLED  ',A(BILLED)                                            
         DC    C'BILLABLE',A(BILLABLE)                                          
         DC    C'OTXO    ',A(OTXO)                                              
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* EXECUTE INPUT PHASE ROUTINES                                                  
*                                                                               
DH100    L     RF,GLAROUT                                                       
         L     R3,GLAIFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* EXECUTE OUTPUT PHASE ROUTINES                                                 
*                                                                               
DH200    L     RF,GLAROUT                                                       
         L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* INPUT ROUTINES                                                                
*                                                                               
PROD     XC    3(3,R3),3(R3)       CLEAR SECOND PRODUCT                         
         CLI   BFPRD,X'FE'         TEST FOR ALL PRODUCTS                        
         BNE   *+14                                                             
         MVC   0(6,R3),XFF                                                      
         B     EXIT                                                             
         MVC   0(3,R3),=C'POL'                                                  
         CLI   BFPRD,219                                                        
         BNL   EXIT                                                             
         LA    R1,BFPRD                                                         
         BAS   RE,GETPROD          SET FIRST PRODUCT                            
         CLI   BFPRD2,0            TEST SECOND PRODUCT                          
         BE    EXIT                                                             
         LA    R1,BFPRD2           YES                                          
         LA    R3,3(R3)                                                         
         BAS   RE,GETPROD                                                       
         B     EXIT                                                             
*                                                                               
GETPROD  L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
GETPROD2 CLC   0(1,R1),3(RF)                                                    
         BE    GETPRODX                                                         
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BNL   GETPROD2                                                         
         DC    H'0'                                                             
GETPRODX MVC   0(3,R3),0(RF)           ALPHA PRODUCT CODE                       
         BR    RE                                                               
*                                                                               
*                                                                               
ESTIMI   CLI   BFEST,X'FF'         ESTIMATE                                     
         BNE   *+14                                                             
         MVC   0(3,R3),XFF                                                      
         B     EXIT                                                             
         ZIC   RE,BFEST                                                         
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  0(3,R3),DUB                                                      
         B     EXIT                                                             
*                                                                               
*                                                                               
         USING SPDRVWKD,R4                                                      
MGR1I    L     R4,ASPDRWKC                                                      
         MVC   0(2,R3),BFMGR                                                    
         OC    0(2,R3),MGR1MASK                                                 
         B     EXIT                                                             
MGR2I    L     R4,ASPDRWKC                                                      
         MVC   0(2,R3),BFMGR                                                    
         OC    0(2,R3),MGR2MASK                                                 
         B     EXIT                                                             
MGR3I    MVC   0(2,R3),BFMGR                                                    
         B     EXIT                                                             
*                                                                               
         USING SPDRVWKD,R4                                                      
PGR1I    L     R4,ASPDRWKC                                                      
         MVC   0(2,R3),BFPGR                                                    
         OC    0(2,R3),PGR1MASK                                                 
         B     EXIT                                                             
PGR2I    MVC   0(2,R3),BFPGR                                                    
         B     EXIT                                                             
         DROP  R4                                                               
*                                                                               
*                                                                               
MARKETI  MVC   0(4,R3),XFF                                                      
         CLC   BFMKTSTA(2),XFF     ALL MARKETS                                  
         BE    EXIT                                                             
         CLC   BFMKTSTA(2),=X'FFFE'                                             
         BNE   *+12                                                             
         MVI   3(R3),X'FE'         CANADIAN NETWORK                             
         B     EXIT                                                             
         GOTO1 MSUNPK,DMCB,BFMKTSTA,0(R3),WORK                                  
         B     EXIT                                                             
*                                                                               
STATIONI CLC   BFMKTSTA+2(3),XFF                                                
         BNE   *+14                                                             
         MVC   0(5,R3),XFF                                                      
         B     EXIT                                                             
         GOTO1 MSUNPK,DMCB,BFMKTSTA,WORK,0(R3)                                  
         B     EXIT                                                             
*                                                                               
PERIODI  MVC   0(L'BFPER,R3),BFPER                                              
         B     EXIT                                                             
*                                                                               
SPOT     MVC   0(L'BFSPT,R3),BFSPT                                              
         B     EXIT                                                             
*                                                                               
GOALD    L     RE,BFGOALD                                                       
         B     CVD                                                              
*                                                                               
AUTHD    L     RE,BFAUTHD                                                       
         B     CVD                                                              
*                                                                               
ORDERD   L     RE,BFORD                                                         
         B     CVD                                                              
*                                                                               
ORDERTX  L     RE,BFORDTX                                                       
         B     CVD                                                              
*                                                                               
PAYD     L     RE,BFPAID                                                        
         B     CVD                                                              
*                                                                               
PAYTX    L     RE,BFPAIDTX                                                      
         B     CVD                                                              
*                                                                               
BILLD    L     RE,BFBILL           BILL DOLLARS                                 
         CLI   QRECORD+67,C'Y'     TEST FOR BILL ADJUSTMENT ALLOWED             
         BE    CVD                                                              
         B     CVD2                                                             
*                                                                               
BILLTX   SR    RE,RE               BILL TAX                                     
         ICM   RE,14,BFBILLTX+1                                                 
         SRA   RE,8                                                             
         CLI   QRECORD+67,C'Y'     TEST FOR BILL ADJUSTMENT ALLOWED             
         BE    CVD                                                              
         B     CVD2                                                             
*                                                                               
CVD      ICM   RF,15,ADJ           TEST FOR ADJUSTMENT                          
         BZ    CVD2                                                             
         MR    RE,RE                                                            
         SLDA  RE,1                                                             
         D     RE,=F'1000000'                                                   
         LTR   RE,RF                                                            
         BM    *+8                                                              
         AH    RE,=H'1'                                                         
         SRA   RE,1                                                             
*                                                                               
CVD2     CVD   RE,0(R3)                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* OUTPUT ROUTINES                                                               
*                                                                               
PRODO    MVC   0(3,R3),0(R2)                                                    
         OC    3(3,R2),3(R2)                                                    
         BZ    PRODO1                                                           
         LA    RF,2(R3)                                                         
         CLI   0(RF),C' '                                                       
         BNH   *+8                                                              
         LA    RF,1(RF)                                                         
         MVI   0(RF),C'-'                                                       
         MVC   1(3,RF),3(R2)                                                    
*                                                                               
PRODO1   CLI   PRDHED,C'Y'         TEST FOR PRODUCT IN HEADLINE                 
         BNE   PRODO4                                                           
         MVC   0(7,R3),SPACES      BLANK DRIVER O/P FIELD                       
         MVC   PRODUCT,0(R2)                                                    
         MVC   PRD2,3(R2)                                                       
         CLC   PRODUCT,SVPROD      TEST FOR CHANGE IN PRODUCT                   
         BE    PRODO2                                                           
         CLC   PRODUCT,XFF         IGNORE ALL PRODUCT RECORD                    
         BE    PRODO4                                                           
         MVC   SVPROD,PRODUCT                                                   
         LA    R4,PRODUCT                                                       
         LA    R5,SVPRDNM                                                       
         BAS   RE,GETPRDNM                                                      
*                                                                               
PRODO2   MVC   PRDNM,SVPRDNM       SET PRODUCT NAME                             
         OC    PRD2,PRD2           TEST SECOND PRODUCT                          
         BZ    PRODO4                                                           
         CLC   PRD2,XFF            YES - CHECK NOT TOTAL                        
         BE    PRODO4                                                           
         CLC   PRD2,SVPROD2        TEST CHANGE IN 2ND PROD                      
         BE    PRODO3                                                           
         MVC   SVPROD2,PRD2                                                     
         LA    R4,PRD2                                                          
         LA    R5,SVPRDNM2                                                      
         BAS   RE,GETPRDNM                                                      
*                                                                               
PRODO3   MVC   PRDNM2,SVPRDNM2     SET 2ND PROD NAME                            
*                                                                               
PRODO4   CLC   0(6,R2),XFF         TEST FOR ALL PRODUCTS RECORD                 
         BNE   EXIT                                                             
         CLI   PRDHED,C'Y'         YES - TEST FOR PRD IN HEADLINE               
         BE    PRODO6                                                           
         L     R1,APRDCNTR               NO -                                   
         CP    0(2,R1),=P'2'       TEST FOR MORE THAN ONE PRODUCT               
         BH    *+12                 (EXCLUDING THIS TOTAL RECORD)               
         MVI   PRTSW,C'N'          NO - SUPPRESS TOTAL PRINT                    
         B     EXIT                                                             
         MVC   0(5,R3),=C'*ALL*'                                                
         B     EXIT                                                             
*                                                                               
PRODO6   MVC   0(L'PRODUCT,R3),SPACES                                           
         MVC   PRODUCT,=C'ALL'                                                  
         MVC   PRDNM,=CL24'*** ALL PRODUCTS ***'                                
         XC    PRD2,PRD2                                                        
         B     EXIT                                                             
*                                                                               
GETPRDNM LR    R0,RE                                                            
         XC    KEY,KEY             GET PRODUCT NAME                             
         MVC   KEY+1(3),BAGYMD                                                  
         MVC   KEY+4(3),0(R4)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   AREC,ADPRD                                                       
         GOTO1 GET                                                              
         L     R1,AREC                                                          
         MVC   0(L'PNAME,R5),PNAME-PRDHDR(R1)                                   
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
ESTIMO   CLC   0(3,R2),XFF         TEST FOR ALL ESTIMATES RECORD                
         BE    ESTIMO5                                                          
         MVC   0(3,R3),SPACES                                                   
         CLC   0(3,R2),=C'000'     DON'T PRINT ESTIMATE 0                       
         BE    EXIT                                                             
         MVC   0(3,R3),0(R2)                                                    
         CLI   GLARGS,C'D'         TEST FOR EST IN DETAIL, BUT                  
         BE    ESTIMO1              REQUIRING ESTIMATE NAME                     
         CLI   ESTHED,C'Y'         TEST FOR EST IN HEADLINE                     
         BNE   EXIT                                                             
         MVC   0(L'ESTIMATE,R3),SPACES  ESTIMATE HANDLED BY SSPEC               
*                                                                               
ESTIMO1  MVC   ESTIMATE,0(R2)                                                   
         CLI   ESTOTS,C'Y'         TEST FOR MORE THAN ONE ESTIMATE              
         BNE   ESTIMO3             NO - ESTIMATE NAME ALREADY SET               
         CLC   ESTIMATE,SVESTIM    YES - TEST FOR CHANGE IN EST                 
         BE    ESTIMO2                   NO - ALREADY HAVE NAME                 
         MVC   SVESTIM,ESTIMATE                                                 
         XC    KEY,KEY             GET ESTIMATE NAME FOR PROD POL               
         MVC   KEY+1(3),BAGYMD                                                  
         MVC   KEY+4(3),=C'POL'                                                 
         PACK  DUB,ESTIMATE                                                     
         CVB   RE,DUB                                                           
         STC   RE,KEY+7                                                         
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     DOES ESTIMATE HDR FOR POL EXIST ?            
         BE    *+14                                                             
         MVC   SVESTNM,SPACES      NO - SET EST NAME TO SPACES                  
         B     ESTIMO2                                                          
         GOTO1 GETEST                                                           
         L     R1,ADEST                                                         
         MVC   SVESTNM(L'EDESC),EDESC-ESTHDR(R1)                                
*                                                                               
ESTIMO2  MVC   ESTNM,SVESTNM                                                    
*                                                                               
ESTIMO3  CLC   QAGY,=C'WW'         TEST FOR WUNDERMAN                           
         BNE   ESTIMO4                                                          
         CLI   B1XPROF+6,C'Y'      YES-TEST READ WUNDERMAN FILE                 
         BNE   ESTIMO4                                                          
         CLI   ESTOTS,C'Y'         TEST MULTIPLE ESTIMATES                      
         BE    *+14                                                             
         CLC   QPRD,=C'ALL'        OR MULTIPLE PRODUCTS                         
         BNE   ESTIMO4                                                          
         MVC   ESTNM,SPACES        YES-SET ESTIMATE NAME TO BLANKS              
*                                                                               
ESTIMO4  CLI   GLARGS,C'D'                                                      
         BNE   EXIT                                                             
         MVC   198(10,R3),ESTNM                                                 
         B     EXIT                                                             
*                                                                               
ESTIMO5  CLI   ESTHED,C'Y'         YES - TEST EST IN HEADLINE                   
         BE    ESTIMO6                                                          
         L     R1,AESTCNTR               NO -                                   
         CP    0(2,R1),=P'2'       TEST FOR MORE THAN ONE EST                   
         BH    *+12                 (EXCLUDING THIS TOTAL RECORD)               
         MVI   PRTSW,C'N'          NO - SUPPRESS TOTAL PRINT                    
         B     EXIT                                                             
         MVC   0(3,R3),=C'ALL'                                                  
         CLI   GLARGS,C'D'                                                      
         BNE   EXIT                                                             
         MVC   0(10,R3),=C'*ALL ESTS*'                                          
         B     EXIT                                                             
*                                                                               
ESTIMO6  MVC   0(L'ESTIMATE,R3),SPACES                                          
         MVC   ESTIMATE,=C'ALL'                                                 
         MVC   ESTNM,ALLESTS                                                    
         B     EXIT                                                             
*                                                                               
*                                                                               
PERIODO  CLI   QRECORD+66,C'B'     CHECK FOR BROADCAST MONTHS                   
         BNE   PERIODO2                                                         
         CLI   QOPT3,C'3'          YES - CHECK FOR PERIOD HIGH                  
         BNE   PERIODO1                                                         
         CLC   SVPER,0(R2)               YES - CHECK FOR CHANGE                 
         BE    EXIT                                                             
         MVC   SVPER,0(R2)                     YES - ALTER REQUEST              
         GOTO1 DATCON,DMCB,(2,0(R2)),(0,QSTART)      START AND END TO           
         MVC   QSTART+4(2),SPACES                    FUDGE PERIOD HEAD          
         MVC   QEND,QSTART                                                      
         B     EXIT                                                             
*                                                                               
PERIODO1 GOTO1 DATCON,DMCB,(2,0(R2)),(6,0(R3))                                  
         B     EXIT                                                             
*                                                                               
PERIODO2 L     R1,MEDBUFF          NOT BROADCAST MONTHS                         
         LA    R4,MEDMON01-MEDBLOCK(R1)                                         
         L     R5,MEDNUMMO-MEDBLOCK(R1)                                         
*                                                                               
PERIODO4 CLC   0(2,R2),2(R4)       COMPARE PERIOD ENDS                          
         BE    PERIODO6                                                         
         LA    R4,12(R4)                                                        
         BCT   R5,PERIODO4                                                      
         MVI   PRTSW,C'N'                                                       
         B     EXIT                                                             
*                                                                               
PERIODO6 CLI   QOPT3,C'3'          CHECK FOR PERIOD HIGH                        
         BNE   PERIODO8                                                         
         CLC   SVPER,0(R4)         YES - CHECK FOR CHANGE                       
         BE    EXIT                                                             
         MVC   SVPER,0(R4)                     YES - ALTER REQUEST              
         GOTO1 DATCON,DMCB,(2,0(R4)),(0,QSTART)      START AND END TO           
         GOTO1 DATCON,DMCB,(2,2(R4)),(0,QEND)        FUDGE PERIOD HEAD          
         B     EXIT                                                             
*                                                                               
PERIODO8 GOTO1 DATCON,DMCB,(2,0(R4)),(4,0(R3))                                  
         MVI   5(R3),C'-'                                                       
         GOTO1 DATCON,DMCB,(2,2(R4)),(4,6(R3))                                  
         B     EXIT                                                             
*                                                                               
*                                                                               
MARKETO  CLC   SVMARKET,0(R2)      TEST CHANGE IN MARKET                        
         BE    MARKETO2                                                         
         MVC   SVMARKET,0(R2)      YES                                          
         MVC   MARKET,0(R2)                                                     
         MVC   MKTNM,SPACES                                                     
         CLC   MARKET(3),XFF                                                    
         BNE   MARKETO1                                                         
         CLI   MARKET+3,X'FF'                                                   
         BNE   *+20                                                             
         MVC   MKTNM(24),=CL24'*** ALL MARKETS ***'                             
         MVC   MARKET(3),=C'ALL'                                                
         B     MARKETO2                                                         
         CLI   MARKET+3,X'FE'                                                   
         BNE   MARKETO1                                                         
         MVC   MARKET,=C'****'                                                  
         MVC   MKTNM(14),=C'GLOBAL NETWORK'                                     
         B     MARKETO2                                                         
*                                                                               
MARKETO1 XC    KEY,KEY             GET MARKET NAME                              
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         MVC   KEY+2(4),SVMARKET                                                
         MVC   KEY+6(2),QAGY                                                    
         MVI   KEY+8,C'0'                                                       
         MVC   KEY+9(8),KEY+8                                                   
         GOTO1 HIGHMKT                                                          
         MVC   MKTNM,SPACES        PRESET IN CASE NOT FOUND                     
         MVC   MKTNM(22),=C'** MKT XXXX UNKNOWN **'                             
         MVC   MKTNM+7(4),SVMARKET                                              
         L     R1,ADMARKET                                                      
         CLC   KEY(8),0(R1)                                                     
         BNE   *+10                                                             
         MVC   MKTNM(L'MKTNAME),MKTNAME-MKTREC(R1)                              
*                                                                               
MARKETO2 CLI   MKTHED,C'Y'         TEST FOR MARKET IN HEADLINE                  
         BE    MARKETO3                                                         
         CLC   SVMARKET,XFF                                                     
         BNE   *+14                                                             
         MVC   0(24,R3),MKTNM                                                   
         B     MARKETO4                                                         
         MVC   0(4,R3),MARKET                                                   
         MVC   5(L'MKTNM-5,R3),MKTNM                                            
         B     MARKETO4                                                         
*                                                                               
MARKETO3 MVC   0(6,R3),=C'MARKET'                                               
         MVC   7(4,R3),MARKET                                                   
         MVC   12(L'MKTNM,R3),MKTNM                                             
         B     EXIT                                                             
*                                                                               
MARKETO4 CLC   SVMARKET,XFF        TEST FOR ALL MARKETS REC                     
         BNE   EXIT                                                             
         L     R1,AMKTCNTR                                                      
         CP    0(2,R1),=P'2'       TEST FOR MORE THAN ONE MARKET                
         BH    EXIT                 (EXCLUDING THIS TOTAL RECORD)               
         MVI   PRTSW,C'N'          NO - SUPPRESS TOTAL PRINT                    
         B     EXIT                                                             
*                                                                               
STATIONO CLC   SVMARKET,XFF        TEST FOR FORMATTING ALL MARKETS              
         BNE   *+12                                                             
         MVI   0(R3),0             YES - LEAVE STATION BLANK                    
         B     EXIT                                                             
         CLC   2(3,R2),XFF         TEST FOR ALL STATIONS                        
         BNE   STATO2                                                           
         CLI   GLAUSW,C'G'         YES - IF GOALS                               
         BE    STATO1                    THEN ALWAYS PRINT MKT TOTAL            
         L     R1,ASTACNTR                                                      
         CP    0(2,R1),=P'2'       TEST FOR MORE THAN ONE STATION               
         BH    STATO1               (EXCLUDING THIS TOTAL RECORD)               
         MVI   PRTSW,C'N'          NO - SUPPRESS TOTAL PRINT                    
         B     EXIT                                                             
*                                                                               
STATO1   MVC   0(5,R3),=C'*ALL*'                                                
         B     EXIT                                                             
*                                                                               
STATO2   MVC   0(4,R3),0(R2)                                                    
         CLI   4(R2),C' '                                                       
         BE    EXIT                                                             
         CLI   4(R2),C'T'                                                       
         BE    EXIT                                                             
         CLI   4(R2),C'N'                                                       
         BE    EXIT                                                             
         CLI   3(R3),C' '                                                       
         BNE   *+6                                                              
         BCTR  R3,0                                                             
         MVI   4(R3),C'-'                                                       
         MVC   5(1,R3),4(R2)                                                    
         MVI   6(R3),C'M'                                                       
         B     EXIT                                                             
*                                                                               
ORDERTXO MVI   TAXSW,C'N'                                                       
         MVC   0(5,R3),SPACES                                                   
         CP    0(8,R2),=P'0'                                                    
         BE    EXIT                                                             
         MVI   TAXSW,C'Y'                                                       
         MVC   0(3,R3),=C'TAX'                                                  
         B     EXIT                                                             
*                                                                               
TAXOBL   MVC   0(11,R3),SPACES     ONLY PRINT IF THERE'S TAX                    
         CLI   TAXSW,C'Y'                                                       
         BE    BILLED                                                           
         B     EXIT                                                             
*                                                                               
TAXOUT   MVC   0(11,R3),SPACES     ONLY PRINT IF THERE'S TAX                    
         CLI   TAXSW,C'Y'                                                       
         BNE   EXIT                                                             
         MVI   GLHOOK,GLEDIT                                                    
         B     EXIT                                                             
*                                                                               
OTXO     MVI   TAXSW,C'Y'                                                       
         CP    0(8,R2),=P'0'                                                    
         BE    *+12                                                             
         MVI   GLHOOK,GLEDIT                                                    
         B     EXIT                                                             
         MVI   TAXSW,C'N'                                                       
         ICM   R1,15,ATAXLIT                                                    
         BZ    *+10                                                             
         MVC   0(3,R1),SPACES                                                   
         ICM   R1,15,ATOTLIT                                                    
         BZ    *+10                                                             
         MVC   0(5,R1),SPACES                                                   
         B     EXIT                                                             
*                                                                               
*                                                                               
PAID     MVI   BYTE,C'P'           SUPPRESS ZERO PAID                           
         B     ANALTYPE                                                         
*                                                                               
UNPAID   MVI   BYTE,C'U'           SUPPRESS ZERO UNPAID                         
         B     ANALTYPE                                                         
*                                                                               
BILLMTAX CLI   TAXSW,C'Y'                                                       
         BNE   BILLED                                                           
         MVI   GLHOOK,GLEDIT                                                    
         B     EXIT                                                             
*                                                                               
BILLED   MVI   BYTE,C'R'           SUPPRESS ZERO BILLED                         
         B     ANALTYPE                                                         
*                                                                               
BILLABLE MVI   BYTE,C'B'           SUPPRESS ZERO BILLABLE                       
*                                                                               
ANALTYPE CLC   QOPT4,BYTE                                                       
         BNE   ANALTYP2                                                         
         CP    0(8,R2),=P'0'                                                    
         BNE   ANALTYP2                                                         
         MVI   PRTSW,C'N'          SUPPRESS THE PRINTLINE                       
         B     EXIT                                                             
*                                                                               
ANALTYP2 MVI   GLHOOK,GLEDIT                                                    
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* RESOLVE LITERALS                                                              
*                                                                               
DH300    L     R2,GLAIFLD                                                       
         L     R3,GLAOFLD                                                       
         ZIC   R4,GLARGS                                                        
         BCTR  R4,0                                                             
         SR    R5,R5                                                            
         LA    R1,LITS                                                          
*                                                                               
DH310    CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         EX    R4,DH3CLC                                                        
         BE    DH320                                                            
         IC    R5,0(R1)                                                         
         AR    R1,R5                                                            
         B     DH310                                                            
*                                                                               
DH320    ICM   RF,7,1(R1)                                                       
         BASR  RE,RF               ROUTINES RETURN OUTPUT LEN IN R5             
         STC   R5,GLARGS+1                                                      
         B     EXIT                                                             
*                                                                               
DH3CLC   CLC   0(0,R2),4(R1)       * EXECUTED                                   
*                                                                               
LITS     DC    AL1(9),AL3(LITMG1),CL5'MGRP1'                                    
         DC    AL1(9),AL3(LITMG2),CL5'MGRP2'                                    
         DC    AL1(9),AL3(LITPG1),CL5'PGRP1'                                    
         DC    AL1(9),AL3(LITTOT),CL5'TOTAL'                                    
         DC    AL1(7),AL3(LITTAX),CL3'TAX'                                      
         DC    X'FF'                                                            
*                                                                               
*                                                                               
LITMG1   MVC   WORK(12),MGR1BK                                                  
         B     LITX                                                             
*                                                                               
LITMG2   MVC   WORK(12),MGR2BK                                                  
         B     LITX                                                             
*                                                                               
LITPG1   MVC   WORK(12),PGR1BK                                                  
         B     LITX                                                             
*                                                                               
*ITTOT   MVC   0(5,R3),SPACES      ONLY PRINT LINE IF THERE'S TAX               
*        CLI   TAXSW,C'Y'                                                       
*        BNER  RE                                                               
*        MVC   0(5,R3),=C'TOTAL'                                                
*        BR    RE                                                               
*                                                                               
LITTOT   MVC   0(5,R3),=C'TOTAL'                                                
         ST    R3,ATOTLIT                                                       
         BR    RE                                                               
*                                                                               
LITTAX   MVC   0(3,R3),=C'TAX'                                                  
         ST    R3,ATAXLIT                                                       
         BR    RE                                                               
*                                                                               
LITX     CLI   QOPT2,C'8'          REPORTS 4-6,8 ONLY HAVE ROOM                 
         BE    LITX1               FOR 7 CHARACTERS (UNDER STATION)             
         CLI   QOPT2,C'4'                                                       
         BL    LITX2                                                            
         CLI   QOPT2,C'6'                                                       
         BH    LITX2                                                            
*                                                                               
LITX1    MVC   0(7,R3),=C'*TOTAL*'                                              
         LA    R5,7                                                             
         BR    RE                                                               
*                                                                               
LITX2    MVI   0(R3),C'*'                                                       
         MVC   2(12,R3),WORK                                                    
         LA    R0,12                                                            
         LA    R5,13(R3)                                                        
         CLI   0(R5),C' '                                                       
         BH    *+10                                                             
         BCTR  R5,0                                                             
         BCT   R0,*-10                                                          
         MVC   2(7,R5),=C'TOTAL *'                                              
         SR    R5,R3                                                            
         AH    R5,=H'8'                                                         
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* FIRST TIME CONTROLS                                                           
*                                                                               
DH400    CLI   QRECORD+33,C'Y'     TEST COMMENTS REQUIRED                       
         BNE   DH410                                                            
         CLI   GLARGS,0            YES                                          
         BNE   DH402                                                            
         XC    SVCOMKEY,SVCOMKEY   FIRST TIME FOR FIRST RECORD                  
         XC    BCMTYPE(14),BCMTYPE                                              
         MVI   BCMTYPE,C'3'                                                     
         MVC   BCMCLT,BCLT                                                      
         B     DH407                                                            
*                                                                               
DH402    ICM   R1,15,GLADTENT      SET R4 = A(FIELD)                            
         BZ    DH410                                                            
         L     R1,DROIADD-DROD(R1)                                              
         LTR   R1,R1                                                            
         BZ    DH410                                                            
         LH    R4,DRINDISP-DRIND(R1)                                            
         A     R4,GLATHREC                                                      
*                                                                               
         CLC   PGRP1LEV,GLARGS     PRODUCT GROUP                                
         BNE   DH403                                                            
         MVC   BCMPGR(1),QPGR                                                   
         MVC   BCMPGR+1(2),0(R4)                                                
         MVI   BCMPRD,0                                                         
         MVI   BCMEST,0                                                         
         B     DH407                                                            
*                                                                               
DH403    CLC   PRDLEV,GLARGS       PRODUCT                                      
         BNE   DH406                                                            
         CLC   0(3,R4),XFF                                                      
         BE    DH410                                                            
         L     RF,ADCLT            FIND PRODUCT CODE FROM CLTHDR                
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
DH404    CLC   0(3,RF),0(R4)                                                    
         BE    DH405                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    DH404                                                            
         DC    H'0'                                                             
DH405    MVC   BCMPRD,3(RF)                                                     
         CLI   ESTLEV,0            TEST ESTIMATE IN HEADLINES                   
         BNE   DH407                                                            
         MVC   BCMEST,QBESTIM      NO - SET ESTIMATE                            
         B     DH407                                                            
*                                                                               
DH406    CLC   ESTLEV,GLARGS       ESTIMATE                                     
         BNE   DH410                                                            
         CLC   0(3,R4),XFF                                                      
         BE    DH410                                                            
         CLI   PRDLEV,0            TEST PRODUCT IN HEADLINES                    
         BNE   *+10                                                             
         MVC   BCMPRD,QBPRD        NO - SET PRODUCT                             
         PACK  DUB,0(3,R4)                                                      
         CVB   RE,DUB                                                           
         STC   RE,BCMEST                                                        
*                                                                               
DH407    L     R5,ADCOMREC         GET COMMENT RECORD                           
         XC    0(13,R5),0(R5)                                                   
         GOTO1 GETCOM                                                           
         CLC   SVCOMKEY,0(R5)      TEST CHANGE OF COMMENT                       
         BE    DH410                                                            
         OC    0(13,R5),0(R5)      YES                                          
         BZ    DH410                                                            
         MVC   SVCOMKEY,0(R5)      SAVE THE COMMENT KEY                         
         SR    R0,R0                                                            
         SR    R1,R1                                                            
         LA    R5,24(R5)                                                        
*                                                                               
DH408    CLI   0(R5),0             COUNT NUMBER OF COMMENTS                     
         BE    DH409                                                            
         CLI   0(R5),5                                                          
         BNE   *+8                                                              
         LA    R1,1(R1)                                                         
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DH408                                                            
*                                                                               
DH409    LTR   R1,R1                                                            
         BZ    DH410                                                            
         STC   R1,SVNCOM           SAVE NUMBER OF COMMENTS                      
         MVI   FORCECMT,C'Y'       FORCE COMMENT FOR HEADHOOK                   
         MVI   GLFHLOVR,13         SET OVERRIDE FIRST HEADLINE                  
*                                                                               
DH410    LA    R1,FIRSTAB                                                       
*                                                                               
DH412    CLI   0(R1),X'FF'                                                      
         BE    EXIT                                                             
         CLC   GLARGS(1),0(R1)                                                  
         BE    *+12                                                             
         LA    R1,4(R1)                                                         
         B     DH412                                                            
         L     RF,0(R1)                                                         
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
DH415    ZAP   L2CNTR,=P'0'                                                     
         BR    RE                                                               
*                                                                               
DH420    ZAP   L3CNTR,=P'0'                                                     
         AP    L2CNTR,=P'1'                                                     
         BR    RE                                                               
*                                                                               
DH430    ZAP   L4CNTR,=P'0'                                                     
         AP    L3CNTR,=P'1'                                                     
         BR    RE                                                               
*                                                                               
DH440    ZAP   L5CNTR,=P'0'                                                     
         AP    L4CNTR,=P'1'                                                     
         BR    RE                                                               
*                                                                               
DH450    AP    L5CNTR,=P'1'                                                     
         BR    RE                                                               
*                                                                               
         DS    0F                                                               
FIRSTAB  DC    X'00',AL3(DH415)                                                 
         DC    X'00',AL3(DH420)                                                 
         DC    X'00',AL3(DH430)                                                 
         DC    X'00',AL3(DH440)                                                 
         DC    X'00',AL3(DH450)                                                 
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
* HEADHOOK                                                                      
*                                                                               
DH500    CLI   FORCECMT,C'Y'       TEST A3 COMMENTS                             
         BNE   DH505                                                            
         MVI   FORCECMT,C'N'       YES                                          
         MVI   GLFHLOVR,0                                                       
         LA    R4,H9               SET START HEADLINE FOR COMMENTS              
         CLI   SDNMGRPS,3                                                       
         BE    DH502                                                            
         LA    R4,H8                                                            
         CLI   SDNMGRPS,2                                                       
         BE    DH502                                                            
         LA    R4,H7                                                            
         CLI   SDNMGRPS,1                                                       
         BE    *+12                                                             
         CLI   NHEADS,2                                                         
         BNE   *+16                                                             
         CLI   SVNCOM,5                                                         
         BL    DH501                                                            
         B     DH502                                                            
         LA    R4,H6                                                            
         CLI   NHEADS,1                                                         
         BNE   *+16                                                             
         CLI   SVNCOM,6                                                         
         BL    DH501                                                            
         B     DH502                                                            
         LA    R4,H5                                                            
         CLI   SVNCOM,7                                                         
         BNL   DH502                                                            
*                                                                               
DH501    LA    R4,132(R4)          CAN LEAVE A GAP LINE                         
*                                                                               
DH502    SR    R0,R0               SCAN RECORD FOR COMMENT ELEMENTS             
         LA    R2,132                                                           
         LA    R3,H11              H11 = LAST POSSIBLE COMMENT LINE             
         L     R5,ADCOMREC                                                      
         LA    R5,24(R5)                                                        
*                                                                               
DH503    CLI   0(R5),0                                                          
         BE    DH505                                                            
         CLI   0(R5),5             TEST COMMENT ELEMENT                         
         BNE   DH504                                                            
         ZIC   RE,1(R5)            YES - MOVE COMMENT TO HEADLINE               
         SH    RE,=H'3'                                                         
         BM    DH504                                                            
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),2(R5)                                                    
         BXLE  R4,R2,DH504                                                      
         B     DH505                                                            
*                                                                               
DH504    IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     DH503                                                            
*                                                                               
DH505    CLI   RQGETBF,C'Y'        TEST BILL FORMULAE ADJUSTMENTS               
         BNE   DH518                                                            
         CLI   ADJCMT,C'Y'         TEST COMMENT WANTED                          
         BNE   DH522                                                            
         CLI   BFPCTCMT,C'Y'       TEST PRINT PERCENT                           
         BNE   DH517                                                            
         MVC   BPRD,QBPRD          YES-SET THE PRODUCT AND ESTIMATE             
         CLI   QBPRD,0                                                          
         BNE   DH510                                                            
         CLC   PRODUCT,=C'ALL'                                                  
         BE    DH517                                                            
         CLC   PRODUCT,=C'POL'     TEST PRODUCT UNALLOCATED                     
         BE    DH522               YES-NO COMMENT                               
         L     RF,ADCLT                                                         
         LA    RF,CLIST-CLTHDRD(RF)                                             
*                                                                               
DH506    CLC   0(3,RF),PRODUCT                                                  
         BE    DH508                                                            
         LA    RF,4(RF)                                                         
         CLI   0(RF),C' '                                                       
         BH    DH506                                                            
         DC    H'0'                                                             
*                                                                               
DH508    MVC   BPRD,3(RF)                                                       
*                                                                               
DH510    MVC   BEST,QBESTIM                                                     
         CLI   QBESTIM,0                                                        
         BNE   DH512                                                            
         CLC   ESTIMATE,=C'ALL'                                                 
         BE    DH517                                                            
         PACK  DUB,ESTIMATE                                                     
         CVB   RE,DUB                                                           
         STC   RE,BEST                                                          
*                                                                               
DH512    L     R1,=A(BILFOTAB)     FIND BILL FORMULA FROM TABLE                 
         LA    RE,NBILFO                                                        
*                                                                               
DH514    OC    0(7,R1),0(R1)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   BPRD,0(R1)                                                       
         BNE   *+14                                                             
         CLC   BEST,1(R1)                                                       
         BE    DH516                                                            
         LA    R1,7(R1)                                                         
         BCT   RE,DH514                                                         
         DC    H'0'                                                             
*                                                                               
DH516    MVC   SVBFORM,2(R1)       PRINT BILL FORMULA PERCENT                   
         OC    SVBFORM,SVBFORM                                                  
         BZ    DH522                                                            
         MVC   HEAD5+35(51),BILFOCMT                                            
         LA    R1,HEAD5+83                                                      
         MVI   0(R1),C'G'                                                       
         TM    SVBFORM,X'10'                                                    
         BZ    *+8                                                              
         MVI   0(R1),C'N'                                                       
         MVI   1(R1),C'+'                                                       
         ICM   RE,15,SVBFORM+1                                                  
         LTR   RE,RE                                                            
         BNM   *+10                                                             
         MVI   1(R1),C'-'                                                       
         LPR   RE,RE                                                            
         LA    R1,2(R1)                                                         
         EDIT  (RE),(8,(R1)),4,ALIGN=LEFT                                       
         AR    R1,R0                                                            
         MVC   0(6,R1),=C'%G ***'                                               
         TM    SVBFORM,X'01'                                                    
         BZ    *+8                                                              
         MVI   1(R1),C'N'                                                       
         B     DH522                                                            
*                                                                               
DH517    MVC   HEAD5+41(51),BILFOCMT  BILL FORMULA COMMENT WITHOUT PCT          
         B     DH522                                                            
*                                                                               
DH518    CLI   ADJCMT,C'Y'         TEST FOR ADJUSTMENT COMMENT NEEDED           
         BNE   DH522                                                            
         CLI   ADJHEAD+23,C'X'     YES - SET HEADLINE IF FIRST                  
         BNE   DH520                                                            
         LA    R4,ADJHEAD+23                                                    
         EDIT  ADJ,(8,(R4)),4                                                   
         GOTO1 SQUASHER,DMCB,ADJHEAD,51                                         
*                                                                               
DH520    MVC   HEAD5+40(L'ADJHEAD),ADJHEAD                                      
*                                                                               
DH522    CLI   DATEFILT,0          TEST FOR DATE LIMITS                         
         BE    EXIT                                                             
         CLI   DATEFILT,C'P'                                                    
         BNE   DH524                                                            
         MVC   HEAD4+42(22),=C'*** PAYMENTS MADE FROM'                          
         LA    R5,HEAD4+65                                                      
         B     DH526                                                            
*                                                                               
DH524    MVC   HEAD4+45(16),=C'*** BILLING FROM'                                
         LA    R5,HEAD4+62                                                      
*                                                                               
DH526    MVC   0(8,R5),DSTART                                                   
         MVC   9(2,R5),=C'TO'                                                   
         MVC   12(8,R5),DEND                                                    
         MVC   21(3,R5),=C'***'                                                 
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
*        BUFFALO ROUTINES                                                       
*                                                                               
HIGHBUFF XC    BFKEY,BFKEY                                                      
         LA    R1,=C'HIGH'                                                      
         B     BUFFX                                                            
*                                                                               
SEQBUFF  LA    R1,=C'SEQ'                                                       
         B     BUFFX                                                            
*                                                                               
PUTBUFF  L     RF,BUFFBUFF                                                      
         USING BUFFALOD,RF                                                      
         CLC   BUFFSOFA,BUFFCRMX   TEST FOR BUFFER FULL                         
         BL    PB010                                                            
         DROP  RF                                                               
         ST    RE,FULL             YES -                                        
         MVC   BFRECSV,BFREC       SAVE CURRENT REC                             
         BAS   RE,PUTDRIV          EMPTY BUFFER TO DRIVER                       
         MVC   BFREC,BFRECSV       RESTORE CURRENT REC                          
         L     RE,FULL                                                          
*                                                                               
PB010    LA    R1,=C'PUT'                                                       
         B     BUFFX                                                            
*                                                                               
RSETBUFF LA    R1,=C'RESET'                                                     
         B     BUFFX                                                            
*                                                                               
BUFFX    NTR1                                                                   
         ST    R1,DMCB                                                          
         GOTO1 BUFFALO,DMCB,,BUFFBUFF,BFREC,1                                   
         TM    DMCB+8,X'80'                                                     
         B     EXIT                                                             
         EJECT                                                                  
PUTDRIV  NTR1                                                                   
*                                                                               
*        ROUTINE TO PUT RECORDS TO DRIVER                                       
*                                                                               
         L     R6,AGLAREA                                                       
         USING GLOBALD,R6                                                       
         LA    RE,PROGPROF                                                      
         ST    RE,GLAPPROF                                                      
         MVI   GLMODE,GLINPUT                                                   
         BAS   RE,HIGHBUFF                                                      
         B     *+8                                                              
*                                                                               
PD010    BAS   RE,SEQBUFF                                                       
         BNE   PD020                                                            
         OC    BFDATA,BFDATA                                                    
         BZ    PD010                                                            
         MVC   GLMAXTLV,BFMAXTLV                                                
         MVI   ANYDATA,C'Y'        INDICATE THERE IS DATA                       
         GOTO1 ADRIVER,DMCB,AGLAREA                                             
         B     PD010                                                            
*                                                                               
PD020    BAS   RE,RSETBUFF                                                      
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         DS    0D                                                               
COVAIL   DC    V(COVAIL)                                                        
AGLAREA  DS    A                                                                
ADRIVER  DS    A                                                                
ASYSDRV  DS    A                                                                
AESTCNTR DS    A                                                                
AMKTCNTR DS    A                                                                
ASTACNTR DS    A                                                                
APRDCNTR DS    A                                                                
ASPDRWKC DS    A                                                                
APRD     DS    A                                                                
ATOTLIT  DS    A                                                                
ATAXLIT  DS    A                                                                
AMGRTAB  DC    A(MGRTAB)                                                        
AESTTAB  DC    A(ESTTAB)                                                        
APGRTAB  DC    A(PGRTAB)                                                        
APSLIST  DC    A(PSLIST)                                                        
ADJ      DS    F                                                                
COSTAREA DS    0XL12                                                            
CGROSS   DS    F                                                                
CNET     DS    F                                                                
CTAX     DS    F                                                                
QMGRLEN  DS    H                                                                
QPGRLEN  DS    H                                                                
*                                                                               
         DS    0D                                                               
       ++INCLUDE SPBVALD                                                        
*                                                                               
DPGFILE  DC    CL8'SPA30   '                                                    
DRIVER   DC    CL8'T00A3A  '                                                    
SPDRIVER DC    CL8'SPDRIVER'                                                    
ALLESTS  DC    CL24'* ALL ESTIMATES *'                                          
ADHDINIT DC    CL53'*** DOLLAR AMOUNTS ARE XXX.XXXX PERCENT OF ACTUAL *X        
               **'                                                              
BILFOCMT DC    CL51'*** DOLLAR AMOUNTS ARE ADJUSTED BY BILL FORMULA ***X        
               '                                                                
ADJHEAD  DS    CL(L'ADHDINIT)                                                   
*                                                                               
XFF      DC    XL8'FFFFFFFFFFFFFFFF'                                            
*                                                                               
QBPRD    DS    X                                                                
QBEST    DS    X                                                                
QBESTIM  DS    X                                                                
QBESTEND DS    X                                                                
QBMKT    DS    CL2                                                              
QBSTA    DS    CL3                                                              
*                                                                               
RQEST    DS    CL3                                                              
RQESTEND DS    CL3                                                              
DBSTART  DS    XL2                                                              
DBEND    DS    XL2                                                              
DSTART   DS    CL8                                                              
DEND     DS    CL8                                                              
BILLST   DS    XL2                                                              
BILLEND  DS    XL2                                                              
REQMNTHS DS    XL26                                                             
ENDYM    DS    XL2                                                              
*                                                                               
SVMSTA   DS    CL5                                                              
SVSTN    DS    XL3                                                              
SVMARKET DS    CL4                                                              
SVESTIM  DS    CL3                                                              
SVESTNM  DS    CL24                                                             
SVPRDNM  DS    CL24                                                             
SVPRDNM2 DS    CL24                                                             
SVPROD   DS    CL3                                                              
SVPROD2  DS    CL3                                                              
SVMGR    DS    CL2                                                              
SVPER    DS    XL2                                                              
SVACPROF DS    XL16                                                             
SVCOMKEY DS    XL13                                                             
SVNCOM   DS    XL1                                                              
SVPGRID  DS    CL1                                                              
SVPGRPEX DS    (PEXMAX)XL2                                                      
PEXMAX   EQU   5                   MAXIMUM PRDGRP EXCEPTIONS                    
*                                                                               
BILLSW   DS    CL1                                                              
PRTSW    DS    CL1                                                              
GLAUSW   DS    CL1                                                              
TAXSW    DS    CL1                                                              
ESTHED   DS    CL1                                                              
PRDHED   DS    CL1                                                              
MKTHED   DS    CL1                                                              
ESTOTS   DS    CL1                                                              
ESTFILT  DS    CL1                                                              
ALLOWBF  DS    CL1                                                              
ADJCMT   DS    CL1                                                              
BFPCTCMT DS    CL1                                                              
DATEFILT DS    CL1                                                              
ANYDATA  DS    CL1                                                              
FIRSTSW  DS    CL1                                                              
NETWORK  DS    CL1                                                              
PIGOPT   DS    CL1                                                              
TOTLEV   DS    XL1                                                              
PGRP1LEV DS    XL1                                                              
PRDLEV   DS    XL1                                                              
ESTLEV   DS    XL1                                                              
NHEADS   DS    XL1                                                              
*                                                                               
B1XPROF  DS    XL16                                                             
*                                                                               
L2CNTR   DS    PL2                                                              
L3CNTR   DS    PL2                                                              
L4CNTR   DS    PL2                                                              
L5CNTR   DS    PL2                                                              
*                                                                               
         DS    0F                                                               
         DS    XL1                                                              
         DC    CL8'*BUFREC*'                                                    
BFREC    DS    0CL51                                                            
BFKEY    DS    0CL15                                                            
BFMGR    DS    CL2                                                              
BFMKTSTA DS    CL5                                                              
BFEST    DS    CL1                                                              
BFPRD    DS    CL1                                                              
BFPRD2   DS    CL1                                                              
BFPGR    DS    CL2                                                              
BFPER    DS    CL2                                                              
BFMAXTLV DS    XL1                                                              
BFDATA   DS    0CL36                                                            
BFSPT    DS    XL4                                                              
BFGOALD  DS    XL4                                                              
BFAUTHD  DS    XL4                                                              
BFORD    DS    XL4                                                              
BFORDTX  DS    XL4                                                              
BFPAID   DS    XL4                                                              
BFPAIDTX DS    XL4                                                              
BFBILL   DS    XL4                                                              
BFBILLTX DS    XL4                                                              
*                                                                               
BFRECSV  DS    CL51                                                             
*                                                                               
BILLPER  DS    CL304                                                            
*                                                                               
PSLIST   DS    XL256                                                            
         EJECT                                                                  
WWBLOCK  DS    CL(SPWWLEN)                                                      
         SPACE 1                                                                
       ++INCLUDE SPB1WWBLK                                                      
         EJECT                                                                  
SPA302   CSECT                                                                  
*                                                                               
         DS    0D                                                               
         DC    CL8'*PGRTAB*'                                                    
PGRTAB   DS    512X                                                             
*                                                                               
         DC    CL8'*ESTTAB*'                                                    
ESTTAB   DS    (256*256)X                                                       
*                                                                               
         DC    CL8'*MGRTAB*'                                                    
MGRTAB   DS    2500D                                                            
*                                                                               
         DC    CL8'BILFOTAB'                                                    
BILFOTAB DS    (NBILFO)XL7                                                      
NBILFO   EQU   100                                                              
*                                                                               
         DS    0D                                                               
         DC    CL8'DRGLOBAL'                                                    
GLAREA   DS    40000X              DRIVER GLOBAL AREA                           
*                                                                               
         BUFF  LINES=1,ROWS=1,COLUMNS=9,FLAVOR=BINARY,KEYLIST=(15,A)            
         EJECT                                                                  
       ++INCLUDE DRGLOBAL                                                       
         EJECT                                                                  
       ++INCLUDE SPDRVWRKD                                                      
         PRINT OFF                                                              
       ++INCLUDE DRIVETABLE                                                     
       ++INCLUDE DDBUFFALOD                                                     
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
PRDHDRD  DSECT                                                                  
       ++INCLUDE SPGENPRD                                                       
       ++INCLUDE SPGENSTAB                                                      
       ++INCLUDE SPGENMKG                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'088SPREPA302 08/25/05'                                      
         END                                                                    
