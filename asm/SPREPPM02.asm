*          DATA SET SPREPPM02  AT LEVEL 012 AS OF 02/25/09                      
*PHASE SPPM02B                                                                  
*INCLUDE BUFFERIN                                                               
SPPM02   TITLE 'PARAMOUNT PICTURES MEDIA SCHEDULE REPORT'                       
SPPM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPPM02                                                       
*                                                                               
         LR    RC,RB                                                            
         AHI   RC,4096                                                          
         USING SPPM02+4096,RC                                                   
*                                                                               
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
                                                                                
         CLI   MODE,PROCBUY                                                     
         BE    PROCB                                                            
         CLI   MODE,PROCGOAL                                                    
         BE    PROCG                                                            
         CLI   MODE,STAFRST                                                     
         BE    STAF                                                             
         CLI   MODE,STALAST                                                     
         BE    STAL                                                             
         CLI   MODE,MKTFRST                                                     
         BE    MKTF                                                             
         CLI   MODE,MKTLAST                                                     
         BE    MKTL                                                             
         CLI   MODE,ESTFRST                                                     
         BE    ESTF                                                             
         CLI   MODE,REQFRST                                                     
         BE    REQF                                                             
         CLI   MODE,RUNFRST                                                     
         BE    RUNF                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         EJECT                                                                  
*=====================================================================*         
* RUNFRST FIRST PROCESSING                                            *         
*=====================================================================*         
RUNF     DS    0H                                                               
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* REQUEST FIRST PROCESSING                                            *         
*=====================================================================*         
                                                                                
REQF     L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LHI   R0,1                                                             
         STCM  R0,15,MEDNUMPE                                                   
         LHI   R0,256                                                           
         STCM  R0,15,MEDLCHNK                                                   
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         MVC   MEDNUMWK,=F'14'                                                  
         MVI   MEDEXTDM,8          ALWAYS 8 DEMOS                               
         MVI   MEDDAILY,C'Y'       GET DAILY DATA                               
         MVC   MEDEXTAX,SPOTPROF+12  SET TAX EXCLUSION OPTION                   
         MVI   RQPRDEST,C'Y'                                                    
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   RQOPTS,RQOPTS_1DEC  SET FOR 1-DECIMAL ONLY                       
*                                                                               
         LA    RE,MYHDHK                                                        
         ST    RE,HEADHOOK                                                      
         STM   R9,RC,HDHKR9                                                     
*                                                                               
         LA    RE,MYMIDHK                                                       
         ST    RE,MIDHOOK                                                       
         STM   R9,RC,MIDHKR9                                                    
*                                                                               
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*=====================================================================*         
* ESTIMATE FIRST PROCESSING                                           *         
*=====================================================================*         
                                                                                
ESTF     GOTOR MEDPRDRD,DMCB,SPWORKD                                            
*                                                                               
         SR    R2,R2                                                            
         IC    R2,BPRD             PRODUCT NUMBER                               
         BCTR  R2,0                                                             
         MH    R2,PRDBUFLN                                                      
         A     R2,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,R2                                                       
         ST    R2,SVPRDBUF                                                      
* COUNT NUMBER OF REAL DEMOS                                                    
         SR    R0,R0                                                            
         LA    R1,PTDEMLST                                                      
         LA    RF,6                                                             
*                                                                               
ESTF10   OC    0(3,R1),0(R1)                                                    
         BZ    ESTF12                                                           
         LA    R1,3(R1)                                                         
         AHI   R0,1                                                             
         BCT   RF,ESTF10                                                        
*                                                                               
ESTF12   STH   R0,SVNUMDEM         SAVE ACTUAL NUMBER OF DEMOS                  
         LTR   RF,RF                                                            
         BZ    ESTF14                                                           
*                                                                               
         MVC   0(3,R1),=X'00D901'  FILL IN WITH RHOMES                          
         LA    R1,3(R1)                                                         
         BCT   RF,*-10                                                          
*                                                                               
* SET DEMOS 7 AND 8 TO RAD1849 AND RVW1234                                      
*                                                                               
ESTF14   MVC   PTDEMLST+18(6),=X'00D98E00D981' SET DEMOS 7/8                    
*                                                                               
* GET AND SAVE DEMO NAMES                                                       
*                                                                               
         MVC   SVDEMNMS,SPACES                                                  
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         MVC   DBSELMED,QMED       SET MEDIA CODE IN DBLOCK                     
         DROP  RE                                                               
* GET 6 CHARACTER DEMO NAMES                                                    
         L     R4,ADEST                                                         
         USING ESTHDRD,R4                                                       
*                                                                               
         MVC   SVDEMNMS,SPACES                                                  
         LH    R0,SVNUMDEM         UP TO 6 DEMOS                                
*                                                                               
         GOTO1 DEMOCON,DMCB,((R0),PTDEMLST),(6,SVDEMNMS),              X        
               (C'S',ADBLOCK),(SPOTPROF+9,EUSRNMS)                              
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
*                                                                               
         CLC   QBOOK1,SPACES                                                    
         BNE   *+8                                                              
         MVI   QRERATE,C' '                                                     
*                                                                               
         LHI   R0,2                SET EST ADJ                                  
         CLI   QRERATE,C' '                                                     
         BE    ESTF20                                                           
*                                                                               
         LHI   R0,3                SET FOR PURCHASED RERATED                    
         CLC   =C'NO',QHUT1                                                     
         BE    *+8                                                              
         AHI   R0,1                SET FOR ADJUSTMENT                           
         CLI   QRERATE,C'I'        RERATE BASED ON INVOICE                      
         BNE   *+8                                                              
         AHI   R0,3                                                             
                                                                                
ESTF20   ST    R0,SVRERATE                                                      
*                                                                               
         MVI   RQGETBF,C'N'                                                     
         CLI   Q2NET,C'Y'                                                       
         BNE   *+8                                                              
         MVI   RQGETBF,C'X'        REQUEST NET DOLLARS                          
         EJECT                                                                  
*====================================================================           
* FORMAT DATES FOR MIDLINES                                                     
*====================================================================           
                                                                                
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         L     R5,MEDAFRST         GET A(DAY 1)                                 
         LA    R4,MYDATES          3 SETS OF CL42 DATA                          
         MVC   0(MYDATEX-MYDATES,R4),SPACES                                     
*                                                                               
ESTF30   GOTO1 DATCON,DMCB,(2,(R5)),WORK  GET YYMMDD                            
         PACK  DUB,WORK+2(2)              PACK MONTH                            
         CVB   RE,DUB                                                           
         BCTR  RE,0                                                             
         AR    RE,RE                                                            
         LA    RE,MYMONTAB(RE)                                                  
         MVC   L'MYDATES(2,R4),0(RE)       2 CHARACTER MONTH                    
         MVC   2*L'MYDATES(2,R4),WORK+4    DAY NUMBER                           
*                                                                               
         GOTO1 GETDAY,DMCB,WORK,WORK+16                                         
         MVC   0(2,R4),WORK+16             2 BYTE DAY                           
*                                                                               
         LA    R4,3(R4)                                                         
         LA    R5,12(R5)                                                        
         C     R5,MEDALAST                                                      
         BNH   ESTF30                                                           
         B     EXIT                                                             
MYMONTAB DC    C'JAFEMRAPMYJNJLAUSEOCNODE' 2-BYTE MONTH CODES                   
         EJECT                                                                  
*=====================================================================*         
* MARKET FIRST                                                        *         
*=====================================================================*         
                                                                                
MKTF     GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
         XC    BUFFCNT,BUFFCNT     CLEAR BUFFALO COUNTER                        
MKTFX    B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* STAFRST                                                             *         
*=====================================================================*         
                                                                                
STAF     MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,1                                                       
         XC    TOTSPOTS,TOTSPOTS   CLEAR STATION TOTALS                         
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* STALAST                                                             *         
*=====================================================================*         
                                                                                
STAL     OC    TOTSPOTS,TOTSPOTS   TEST ANY SPOTS                               
         BZ    EXIT                                                             
         MVI   RCSUBPRG,1                                                       
*                                                                               
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK1Q                                                     
         MVC   BFK1STA,BSTA                                                     
*                                                                               
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
*                                                                               
         L     R0,BFSPOTS                                                       
         EDIT  (R0),(3,SKSPOTS)                                                 
         MVI   SKSPOTS+3,C'*'                                                   
*                                                                               
         L     R1,BFBDOL                                                        
         M     R0,=F'2'            X 2                                          
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(10,SKCOST-132),FLOAT=$,ALIGN=LEFT                          
*                                                                               
* PRINT SPOT TOTALS BY DAY                                                      
*                                                                               
         LA    RE,TOTSPOTS                                                      
         LA    RF,14                                                            
         LA    R2,SKGRID                                                        
*                                                                               
STAL8    LH    R0,0(RE)                                                         
         EDIT  (R0),(2,1(R2))                                                   
*                                                                               
         LA    R2,3(R2)            NEXT GRID POSITION                           
         LA    RE,2(RE)            NEXT DAY TOTAL                               
         BCT   RF,STAL8                                                         
*                                                                               
         ICM   R0,15,BFSPOTS                                                    
         EDIT  (R0),(3,SKSPOTS)                                                 
         MVI   SKSPOTS+3,C'*'                                                   
*                                                                               
         LA    R4,BFBDEM1                                                       
         LHI   R5,8                                                             
         LA    R6,SKDEMS                                                        
         L     R2,SVPRDBUF         GET PRDBUFF ADDRESS                          
         LA    R2,PTDEMLST-PTBUFFD(R2)                                          
*                                                                               
STAL10   MVC   DEMO,0(R4)                                                       
         MVC   DEMOTYPE,1(R2)                                                   
         MVC   DOLS,BFBDOL                                                      
*                                                                               
         BAS   RE,FMTDEM                                                        
         MVC   0(6,R6),PRTDEMO                                                  
         MVC   132(6,R6),PRTCPP                                                 
*                                                                               
STAL20   LA    R2,3(R2)            NEXT 3 BYTE DEMO                             
         LA    R4,8(R4)            NEXT DEMO IN MEDBUFF                         
         LA    R6,L'SKDEMS(R6)     NEXT PRINT LINE DEMO                         
         BCT   R5,STAL10                                                        
*                                                                               
         MVI   SPACING,1                                                        
         MVC   P4,P2               MOVE PRINT LINES DOWN 1                      
         MVC   P3,P1                                                            
*                                                                               
         MVC   P2,SPACES                                                        
         MVC   P2(19),=C'STATION WABC TOTALS'                                   
         MVC   P2+8(4),BIGSTA                                                   
*                                                                               
         MVI   P1,C'='                                                          
         MVC   P1+1(131),P1                                                     
*                                                                               
STALX    GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         XC    TOTSPOTS,TOTSPOTS                                                
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* BUY RECORD PROCESSING                                               *         
*=====================================================================*         
PROCB    BAS   RE,FIXCOVRD                                                      
         XC    PSLIST,PSLIST                                                    
         GOTOR MEDPSL,DMCB,SPWORKD,PSLIST                                       
                                                                                
         LA    R2,PSLIST                                                        
PROCB02  CLI   0(R2),0                                                          
         BE    EXIT                                                             
         CLC   0(1,R2),BPRD                                                     
         BNE   PROCB06                                                          
                                                                                
PROCB04  L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         GOTOR MEDGETBY,DMCB,SPWORKD,SVRERATE                                   
                                                                                
         BAS   RE,BUYSKED          PRINT SCHEDULE GRID                          
*                                                                               
         BAS   RE,BUYPUT           POST BUY VALUES TO BUFFERIN                  
                                                                                
PROCB06  LA    R2,2(R2)                                                         
         B     PROCB02                                                          
                                                                                
*====================================================================*          
* SCAN BUY RECORD FOR COST OVERRIDES THAT APPLY ONLY TO COST1/COST2  *          
* SINCE THIS PROGRAM DOES **NOT** WRITE RECORDS BACK TO THE FILE     *          
* WE WILL SIMPLY RESET THE COST OVERRIDE INDICATOR AS REQUIRED       *          
*====================================================================*          
FIXCOVRD NTR1                                                                   
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
FIXCOV2  IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    FIXCOVX                                                          
         CLI   0(R6),X'0B'                                                      
         BL    FIXCOV2                                                          
         CLI   0(R6),X'0C'                                                      
         BH    FIXCOV2                                                          
         TM    6(R6),X'20'                                                      
         BZ    FIXCOV2                                                          
*                                                                               
         LR    R7,R6                                                            
FIXCOV4  IC    R0,1(R7)                                                         
         AR    R7,R0                                                            
         CLI   0(R7),X'10'                                                      
         BL    FIXCOV2                                                          
         CLI   0(R7),X'13'                                                      
         BL    FIXCOV4                                                          
         BH    FIXCOV2                                                          
* X'13' ELEMENT FOUND                                                           
         TM    2(R7),X'40'         TEST COST2 OVERRIDE ONLY                     
         BZ    FIXCOV2             NO                                           
         CLI   QCOST2,C'Y'         TEST CLIENT VERSION                          
         BE    FIXCOV2             YES - KEEP OVERRIDE                          
         NI    6(R6),X'DF'         UNSET OVERRIDE IN ORIGINAL ELEMENT           
         XC    7(3,R6),7(R6)       AND CLEAR AMOUNT                             
         B     FIXCOV2                                                          
*                                                                               
FIXCOVX  B     EXIT                                                             
                                                                                
*=====================================================================*         
* PRINT THE BUY SCHEDULE                                              *         
*=====================================================================*         
                                                                                
BUYSKED  NTR1  ,                                                                
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)            POINT TO PERIOD TOTALS                       
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    EXIT                                                             
*                                                                               
* HAVE SPOTS OR DOLLARS, SO PRINT GRID                                          
*                                                                               
         LA    R2,SKGRID                                                        
         L     R5,MEDAFRST         GET A(DAY1)                                  
         LA    R6,TOTSPOTS                                                      
*                                                                               
BUYSK2   L     R4,4(R5)            POINT TO MEDDATA                             
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    BUYSK12                                                          
*                                                                               
BUYSK10  ICM   R0,15,MEDBYSPT      TEST FOR SPOTS                               
         BZ    BUYSK12                                                          
         EDIT  (R0),(2,1(R2))                                                   
*                                                                               
         ICM   R0,15,MEDBYSPT      TEST FOR SPOTS                               
         AH    R0,0(R6)            BUMP SPOT TOTAL                              
         STH   R0,0(R6)                                                         
*                                                                               
BUYSK12  LA    R2,3(R2)            NEXT GRID POSITION                           
         LA    R5,12(R5)           NEXT DAY                                     
         LA    R6,2(R6)                                                         
         C     R5,MEDALAST                                                      
         BNH   BUYSK2                                                           
*                                                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         ICM   R0,15,MEDBYSPT      TEST FOR SPOTS                               
         EDIT  (R0),(3,SKSPOTS)                                                 
         MVI   SKSPOTS+3,C'*'                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R0,7                                                             
         LA    R1,SKDAYS                                                        
         MVC   0(7,R1),=C'MTWTFSS'                                              
*                                                                               
         IC    RE,BDDAY                                                         
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
BUYSK14  LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,BUYSK14                                                       
*                                                                               
         GOTO1 UNTIME,DMCB,BDTIMST,SKTIME                                       
*                                                                               
         MVC   SKPROG,BDPROGRM                                                  
*                                                                               
         CLI   QCOST2,C'Y'         COS2 OPTION?                                 
         BNE   BUYSK15             NO                                           
         MVI   ELCDLO,X'71'        LOOK FOR X'71' COST OVERRIDE ELEMENT         
         MVI   ELCDHI,X'71'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BUYSK15                                                          
         ICM   R0,15,2(R6)                                                      
         L     R6,ADBUY                                                         
         B     BUYSK16                                                          
*                                                                               
BUYSK15  L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
*                                                                               
BUYSK16  CVD   R0,DUB                                                           
         EDIT  (R0),(9,SKCOST),2,FLOAT=$,ALIGN=LEFT                             
*                                                                               
         MVC   BYTE,BDCIND                                                      
         TM    BDCIND2,X'04'       BDCIND IS A CHARACTER                        
         BNZ   BUYSK17                                                          
         MVI   BYTE,C' '                                                        
         TM    BDCIND,X'80'                                                     
         BZ    *+8                                                              
         MVI   BYTE,C'F'                                                        
         TM    BDCIND,X'40'                                                     
         BZ    *+8                                                              
         MVI   BYTE,C'Q'                                                        
         TM    BDCIND,X'10'                                                     
         BZ    *+8                                                              
         MVI   BYTE,C'N'                                                        
         TM    BDCIND,X'FE'                                                     
         BNZ   *+8                                                              
         MVI   BYTE,C'P'                                                        
         TM    BDCIND,X'08'                                                     
         BZ    *+8                                                              
         MVI   BYTE,C'V'                                                        
         TM    BDCIND,X'04'                                                     
         BZ    *+8                                                              
         MVI   BYTE,C'S'                                                        
         TM    BDCIND,X'02'                                                     
         BZ    *+8                                                              
         MVI   BYTE,C'X'                                                        
         TM    BDCIND2,X'80'                                                    
         BZ    *+8                                                              
         MVI   BYTE,C'C'                                                        
BUYSK17  TM    BDCIND2,X'02'       TEST TRADE SPOT                              
         BZ    *+8                                                              
         MVI   BYTE,C'T'                                                        
         CLI   BYTE,C' '                                                        
         BE    BUYSK18                                                          
         LA    R1,SKCOST+L'SKCOST-1                                             
         CLI   0(R1),C'$'                                                       
         BE    *+8                                                              
         BCT   R1,*-8                                                           
         MVC   0(1,R1),BYTE                                                     
*                                                                               
BUYSK18  MVC   SKDPT,MEDDPART      3 CHAR DAYPART                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  SKSLN,DUB                                                        
*                                                                               
         CLI   Q2USER,X'40'        PRINT REP?                                   
         BNH   BUYSK19             NO                                           
         OC    BDREP,BDREP         HAVE REP?                                    
         BZ    BUYSK19             NO                                           
         GOTO1 VRCPACK,DMCB,(C'U',BDREP),SKREP                                  
         DROP  R6                                                               
*                                                                               
         CLI   Q2USER,C'B'         PRINT BOTH REP & REP NAME?                   
         BNE   BUYSK19             NO                                           
         LA    R6,KEY              YES - GET REP NAME                           
         USING REPREC,R6                                                        
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   REPKTYPE,C'R'                                                    
         MVC   REPKMED,QMED                                                     
         MVC   REPKREP,SKREP                                                    
         MVC   REPKAGY,AGY                                                      
         GOTO1 READREP                                                          
         L     R6,ADREP                                                         
         MVC   SKREPNAM,RNAME      REP NAME                                     
         DROP  R6                                                               
*=============================================================                  
* DEMOS AND CPPS                                                                
*=============================================================                  
BUYSK19  LA    R5,MEDBY1                                                        
         LA    R6,SKDEMS                                                        
         L     R2,SVPRDBUF         GET PRDBUFF ADDRESS                          
         LA    R2,PTDEMLST-PTBUFFD(R2)                                          
*                                                                               
BUYSK20  ICM   R1,15,0(R5)         DEMO                                         
         BZ    BUYSK22                                                          
         M     R0,=F'2'            X 2                                          
         D     R0,MEDBYSPT         DIVIDE TOTAL DEMO BY SPOTS                   
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,DEMO                                                          
         MVC   DEMOTYPE,1(R2)                                                   
         XC    DOLS,DOLS           SET DOLS TO ZERO                             
* THIS CALL GETS DEMO VALUE FOR 1 SPOT                                          
         BAS   RE,FMTDEM                                                        
         MVC   0(6,R6),PRTDEMO                                                  
* NOW GET CPP PASSING TOTAL FOR DEMO AND TOTAL DOLLARS                          
         MVC   DEMO,0(R5)                                                       
         MVC   DOLS,MEDBYD                                                      
*                                                                               
         BAS   RE,FMTDEM                                                        
         MVC   132(6,R6),PRTCPP                                                 
*                                                                               
BUYSK22  LA    R5,8(R5)            NEXT DEMO                                    
         LA    R6,L'SKDEMS(R6)                                                  
         LA    R2,3(R2)            NEXT 3-BYTE DEMO                             
         LA    R0,MEDBY9                                                        
         CR    R5,R0                                                            
         BL    BUYSK20                                                          
         DROP  R4                                                               
         EJECT                                                                  
*=====================================================================*         
* PRINT COMMENTS                                                                
* CAN SUPPRESS WITH QOPT1=N                                                     
*=====================================================================*         
         SPACE 1                                                                
         CLI   QOPT1,C'N'          TEST OPTION TO SUPPRESS                      
         BE    BUYSKX                                                           
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BUYSKX                                                           
*                                                                               
         MVI   P3,0                FORCE P3 TO PRINT                            
         LA    R7,P4               COMMENTS START ON P4                         
*                                                                               
BUYSK32  LLC   RE,1(R6)                                                         
         SHI   RE,4                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R7),3(R6)                                                    
*                                                                               
         LA    R7,132(R7)                                                       
         MVI   0(R7),0             FORCE A BLANK LINE AFTER LAST                
         BRAS  RE,NEXTEL                                                        
         BE    BUYSK32                                                          
*                                                                               
BUYSKX   GOTO1 REPORT                                                           
         B     EXIT                                                             
         DROP  R6                                                               
*=====================================================================*         
* BUY RECORD POST ROUTINES                                            *         
*=====================================================================*         
                                                                                
BUYPUT   NTR1  ,                                                                
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)            POINT TO DOLLARS                             
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    EXIT                                                             
* SET COL DATA                                                                  
         XC    BFREC(BFRECL),BFREC                                              
         MVC   BFSPOTS,MEDBYSPT                                                 
         MVC   BFBDOL,MEDBYD                                                    
         MVC   BFBEQDOL,MEDBYDEQ                                                
*                                                                               
         LH    R0,SVNUMDEM         MOVE UP TO 6 DEMOS                           
         LA    R5,BFBDEM1                                                       
         LA    R6,MEDBY1                                                        
*                                                                               
BUYPUT2  MVC   0(8,R5),0(R6)       MOVE DEMO AND EQUIV DEMO                     
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         BCT   R0,BUYPUT2                                                       
*                                                                               
         MVC   BFBDEM7(16),MEDBY7  ALWAYS MOVE DEMOS 7/8                        
*                                                                               
         MVI   BFTYPE,BFK1Q                                                     
         MVC   BFK1STA,KEY+6                                                    
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK1STA(3),=3X'FF'                                               
         GOTOR PUTBUF                                                           
*                                                                               
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK2Q                                                     
         MVC   BFK2DPT,MEDDPART                                                 
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK2DPT,=3X'FF'                                                  
         GOTOR PUTBUF                                                           
*                                                                               
         BAS   RE,GETEQSLN                                                      
*                                                                               
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK3Q                                                     
         MVC   BFK3SLN,EQSLN       SPOTLEN                                      
         GOTOR PUTBUF                                                           
*                                                                               
         MVI   BFK3SLN,X'FF'                                                    
         GOTOR PUTBUF                                                           
*                                                                               
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK4Q                                                     
         MVC   BFK4DPT,MEDDPART                                                 
         MVC   BFK4SLN,EQSLN       SPOTLEN                                      
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK4DPT,=3X'FF'                                                  
         MVI   BFK4SLN,X'FF'                                                    
         GOTOR PUTBUF                                                           
*                                                                               
         B     EXIT                                                             
         DROP  R3                                                               
*                                                                               
GETEQSLN NTR1                                                                   
         MVI   BYTE,C'R'           FIND EQTAB                                   
         CLI   MED,C'R'                                                         
         BE    GETEQ2                                                           
         CLI   MED,C'X'                                                         
         BE    GETEQ2                                                           
         MVI   BYTE,C'T'                                                        
         CLI   MED,C'T'                                                         
         BE    GETEQ2                                                           
         CLI   MED,C'N'                                                         
         BE    GETEQ2                                                           
         CLI   MED,C'C'                                                         
         BE    GETEQ2                                                           
         DC    H'0'                                                             
*                                                                               
GETEQ2   L     R1,VSLNTAB                                                       
         LH    RE,0(R1)            ENTRY LENGTH                                 
         L     RF,2(R1)            DSPL TO EOT                                  
         AR    RF,R1               RELOCATE EOT ADDRESS                         
         AHI   R1,6                POINT TO FIRST ENTRY                         
*                                                                               
GETEQ4   CLC   0(2,R1),=C'00'      TEST DEFAULT ENTRY                           
         BE    GETEQ10                                                          
         CLC   0(2,R1),AGY                                                      
         BNE   *+14                                                             
GETEQ6   CLC   2(1,R1),BYTE                                                     
         BE    GETEQ10                                                          
         BXLE  R1,RE,GETEQ4                                                     
         DC    H'0'                                                             
*                                                                               
GETEQ10  SR    RE,RE                                                            
         IC    RE,1(R2)            GET ACTUAL SLN                               
         AR    RE,RE                                                            
         LA    RE,4(R1,RE)         POINT TO TABLE ENTRY (HDR=4)                 
         MVC   EQSLN,1(RE)         AND USE THE LENGTH FOR EQUIV                 
         XIT1                                                                   
         EJECT                                                                  
*=====================================================================*         
* GOAL RECORD PROCESSING                                              *         
*=====================================================================*         
                                                                                
         USING GOALREC,KEY                                                      
PROCG    L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         L     R5,MEDAFRST                                                      
*                                                                               
PROCG2   GOTOR MEDGETGL,DMCB,SPWORKD                                            
         GOTOR GOLPUT              POST GOAL DATA                               
*                                                                               
         LA    R5,12(R5)           NEXT DAY                                     
         C     R5,MEDALAST                                                      
         BNH   PROCG2                                                           
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* GOAL RECORD POSTING ROUTINES                                        *         
*=====================================================================*         
                                                                                
GOLPUT   NTR1  ,                                                                
         L     R4,4(R5)            POINT TO DOLLARS                             
         USING MEDDATA,R4                                                       
         OC    MEDGLD(12),MEDGLD                                                
         BZ    EXIT                                                             
                                                                                
*=====================================================================*         
* REPORT 1 - DEMO TOTALS                                              *         
*=====================================================================*         
                                                                                
         XC    BFREC(BFRECL),BFREC                                              
         MVI   BFTYPE,BFK1Q        TYPE 1 - STATION ANALYSIS                    
         MVC   BFK1STA,=X'FFFFFF'  SET FOR TOTALS ONLY                          
                                                                                
         MVC   BFGDOL,MEDGLD                                                    
         MVC   BFGEQDOL,MEDGLDEQ                                                
         MVC   BFGDEM,MEDGL1                                                    
         MVC   BFGEQDEM,MEDGL1EQ                                                
         GOTOR PUTBUF              PUT RECORD                                   
*                                                                               
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK2Q        TYPE 2 - DAYPART ANALYSIS                    
         MVC   BFK2DPT,=X'FFFFFF'                                               
         GOTOR PUTBUF                                                           
*                                                                               
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK3Q        TYPE 3 - SLN ANALYSIS                        
         MVI   BFK3SLN,X'FF'                                                    
         GOTOR PUTBUF                                                           
*                                                                               
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK4Q        TYPE 4 - DPT/SLN ANALYSIS                    
         MVC   BFK4DPT,=X'FFFFFF'                                               
         MVI   BFK4SLN,X'FF'                                                    
         GOTOR PUTBUF                                                           
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* LAST FOR MARKET - PRINT STATION SUMMARY                             *         
*=====================================================================*         
                                                                                
MKTL     MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,2          SET STATION ANALYSIS FLAG                    
         OC    BUFFCNT,BUFFCNT                                                  
         BZ    EXIT                                                             
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK1Q                                                     
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         B     MKTL14                                                           
                                                                                
MKTL12   GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
*                                                                               
MKTL14   CLC   BFK1STA,=X'FFFFFF'                                               
         BE    MKTL20                                                           
         MVC   PSTA,SPACES                                                      
         GOTO1 MSUNPK,DMCB,BFK1STA-2,DUB,PSTA                                   
*                                                                               
MKTL16   BAS   RE,FORMAT                                                        
         MVI   ALLOWLIN,0                                                       
         B     MKTL12                                                           
*                                                                               
MKTL20   MVC   PSTA(5),=C'*TOTS'                                                
         MVI   TOTSW,C'Y'                                                       
         BAS   RE,FORMAT                                                        
*                                                                               
         BAS   RE,FMTGLS                                                        
                                                                                
*=====================================================================*         
* PRINT DAYPART SUMMARY                                               *         
*=====================================================================*         
                                                                                
         MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,3                                                       
         BAS   RE,CHKLIN           START WITH AT LEAST 10 LINES                 
*                                                                               
MKTL30   GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
*                                                                               
         CLC   BFK2DPT,=3X'FF'                                                  
         BE    MKTL32                                                           
         MVC   PDPT,BFK2DPT                                                     
*                                                                               
         BAS   RE,FORMAT                                                        
         B     MKTL30                                                           
*                                                                               
MKTL32   MVC   PDPT(5),=C'*TOTS'                                                
         MVI   TOTSW,C'Y'                                                       
         BAS   RE,FORMAT                                                        
*                                                                               
         BAS   RE,FMTGLS                                                        
         EJECT                                                                  
*=====================================================================*         
* PRINT SPOTLENGTH SUMMARY                                            *         
*=====================================================================*         
                                                                                
         MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,4                                                       
         BAS   RE,CHKLIN           START WITH AT LEAST 10 LINES                 
*                                                                               
MKTL40   GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
*                                                                               
         CLI   BFK3SLN,X'FF'                                                    
         BE    MKTL42                                                           
         SR    R0,R0                                                            
         IC    R0,BFK3SLN                                                       
         EDIT  (R0),(3,PSLN)                                                    
         BAS   RE,FORMAT                                                        
         B     MKTL40                                                           
*                                                                               
MKTL42   MVC   PSLN(5),=C'*TOTS'                                                
         MVI   TOTSW,C'Y'                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,FMTGLS                                                        
         EJECT                                                                  
*=====================================================================*         
* PRINT DPT/SLN SUMMARY                                               *         
*=====================================================================*         
                                                                                
         MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,5                                                       
         BAS   RE,CHKLIN           START WITH AT LEAST 10 LINES                 
*                                                                               
MKTL50   GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
*                                                                               
         CLI   BFK4DPT,X'FF'                                                    
         BE    MKTL52                                                           
         MVC   PDPT,BFK4DPT                                                     
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BFK4SLN                                                       
         EDIT  (R0),(3,PDPT+4),ALIGN=LEFT                                       
         BAS   RE,FORMAT                                                        
         B     MKTL50                                                           
*                                                                               
MKTL52   MVC   PSLN(5),=C'*TOTS'                                                
         MVI   TOTSW,C'Y'                                                       
         BAS   RE,FORMAT                                                        
         BAS   RE,FMTGLS                                                        
         B     EXIT                                                             
*                                                                               
CHKLIN   SR    R0,R0                                                            
         IC    R0,MAXLINES                                                      
         SR    RF,RF                                                            
         IC    RF,LINE             CURRENT LINE COUNT                           
         SR    R0,RF                                                            
         CHI   R0,12                                                            
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BR    RE                                                               
         EJECT                                                                  
FORMAT   NTR1                                                                   
         L     R0,BFSPOTS                                                       
         EDIT  (R0),(5,PSPOTS)                                                  
*                                                                               
         L     R1,BFBDOL                                                        
         M     R0,=F'2'            X 2                                          
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(10,PDOLS),FLOAT=$                                          
*                                                                               
         LA    R4,BFBDEM1                                                       
         LHI   R5,8                                                             
         LA    R6,PDEM1                                                         
         L     R2,SVPRDBUF         GET PRDBUFF ADDRESS                          
         LA    R2,PTDEMLST-PTBUFFD(R2)                                          
*                                                                               
FORMAT10 MVC   DEMO,0(R4)                                                       
         MVC   DEMOTYPE,1(R2)                                                   
         MVC   DOLS,BFBDOL                                                      
*                                                                               
         BAS   RE,FMTDEM                                                        
*                                                                               
         MVC   0(6,R6),PRTDEMO                                                  
         MVC   7(6,R6),PRTCPP                                                   
*                                                                               
FORMAT20 LA    R2,3(R2)            NEXT 3-BYTE DEMO                             
         LA    R4,8(R4)            NEXT DEMO                                    
         AHI   R6,PDEM2-PDEM1      NEXT PRINT LINE DEMO                         
         BCT   R5,FORMAT10                                                      
*                                                                               
         CLI   TOTSW,C'Y'          TEST DOING TOTALS                            
         BNE   FORMATX                                                          
         MVI   SPACING,1                                                        
         MVC   P3,P1               MOVE PRINT LINE DOWN                         
         MVI   P1,C'='                                                          
         MVC   P1+1(131),P1                                                     
         MVI   P2,0                FORCE P2 TO PRINT                            
*                                                                               
FORMATX  GOTO1 REPORT                                                           
         MVI   TOTSW,C'N'          RESET SWITCH                                 
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* FORMAT GOALS AND INDICES                                            *         
*=====================================================================*         
                                                                                
FMTGLS   NTR1                                                                   
         MVC   PSTA(5),=C'GOALS'                                                
         XC    GOALCPP,GOALCPP                                                  
*                                                                               
         L     R1,BFGDOL                                                        
         M     R0,=F'2'            X 2                                          
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(10,PDOLS)                                                  
*                                                                               
         ICM   R0,15,BFGDEM                                                     
         C     R0,=F'99999'        MAX WITH DEC IS 9999.9                       
         BNH   FMTGL2                                                           
         SRDL  R0,32                                                            
         AR    R1,R1               X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,PDEM1),0                                                 
         B     FMTGL4                                                           
*                                                                               
FMTGL2   EDIT (R0),(6,PDEM1),1                                                  
*                                                                               
FMTGL4   L     R1,BFGDOL           GET DOLLARS                                  
         M     R0,=F'20'           X 10 X 2                                     
         OC    BFGDEM,BFGDEM                                                    
         BZ    FMTGL10                                                          
         D     R0,BFGDEM           DIVIDE BY UNEQUIV POINTS                     
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,GOALCPP          SAVE GOALCPP                                 
         C     R1,=F'99999'        MAX CPP WITH CENTS IS 999.99                 
         BNH   FMTGL6                                                           
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'10000'                                                     
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,PCPP1)                                                   
         B     FMTGL10                                                          
*                                                                               
FMTGL6   EDIT  (R1),(7,PCPP1),2                                                 
*                                                                               
* NOW WORK OUT INDICES                                                          
*                                                                               
FMTGL10  MVC   P2(5),=C'INDEX'                                                  
*                                                                               
         L     R1,BFBDOL                                                        
         M     R0,=F'200'          X 100 X 2                                    
         OC    BFGDOL,BFGDOL                                                    
         BZ    FMTGL12                                                          
         D     R0,BFGDOL                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LA    R6,(PDOLS-P)+P2+7   POINT TO PRINT POSITION                      
         EDIT  (R1),(3,(R6)),0                                                  
*                                                                               
FMTGL12  L     R1,BFBDEM1                                                       
         M     R0,=F'200'          X 100 X 2                                    
         OC    BFGDEM,BFGDEM                                                    
         BZ    FMTGL14                                                          
         D     R0,BFGDEM                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LA    R6,(PDEM1-P)+P2+3   POINT TO PRINT POSITION                      
         EDIT  (R1),(3,(R6)),0                                                  
*                                                                               
FMTGL14  L     R1,BFBDOL           GET DOLLARS                                  
         M     R0,=F'20'           X 10 X 2                                     
         OC    BFBDEM1,BFBDEM1                                                  
         BZ    FMTGLX                                                           
         D     R0,BFBDEM1          DIVIDE BY UNEQUIV POINTS                     
         AHI   R1,1                                                             
         SRL   R1,1                THIS IS ACTUAL CPP                           
*                                                                               
         M     R0,=F'200'          X 100 X 2                                    
         OC    GOALCPP,GOALCPP                                                  
         BZ    FMTGLX                                                           
         D     R0,GOALCPP                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LA    R6,(PCPP1-P)+P2+4                                                
         EDIT  (R1),(3,(R6)),0                                                  
*                                                                               
FMTGLX   GOTO1 REPORT                                                           
         B     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* PRINT NEXT COMMENT LINE FROM COMMENT BUFFER                         *         
*=====================================================================*         
                                                                                
NXTCOM   CLC   COMLINEP,COMLINEN                                                
         BER   RE                                                               
         SR    R1,R1                                                            
         IC    R1,COMLINEP                                                      
         AHI   R1,1                                                             
         STC   R1,COMLINEP                                                      
         MHI   R1,L'COMLINES                                                    
         LA    R1,COMLINES-L'COMLINES(R1)                                       
         MVC   P1+61(L'COMLINES),0(R1)                                          
         BR    RE                                                               
         EJECT                                                                  
*=====================================================================*         
* PUT RECORD TO BUFFERIN AND DO TRACE IF NECESSARY                    *         
*=====================================================================*         
                                                                                
PUTBUF   OC    BFDATA(BFDATAL),BFDATA                                           
         BZR   RE                                                               
         ST    RE,SAVERE                                                        
         L     R0,BUFFCNT                                                       
         AHI   R0,1                                                             
         ST    R0,BUFFCNT                                                       
         CLI   QOPT5,C'Y'         TEST TRACE OPTION                             
         BNE   PUTBUF02                                                         
         GOTOR PRNTBL,DMCB,=C'PUTBUF',BFREC,C'DUMP',BFRECL,=C'1D00'             
                                                                                
PUTBUF02 GOTOR BUFFERIN,DMCB,('BUFFAPUT',BUFFET),BFREC,ACOMFACS                 
                                                                                
PUTBUFX  L     RE,SAVERE                                                        
         BR    RE                                                               
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
*===========================================================                    
* FORMAT DEMO AND CPP VALUES FOR PRINTING                                       
* INPUT FIELDS ARE DEMO(4),DEMOTYPE(1),DOLS(4)                                  
* OUTPUT FIELDS ARE PRTDEMO(6) AND PRTCPP(6)                                    
*===========================================================                    
         SPACE 1                                                                
FMTDEM   NTR1                                                                   
         MVC   PRTDEMO,SPACES                                                   
         MVC   PRTCPP,SPACES                                                    
*                                                                               
         ICM   R0,15,DEMO                                                       
         BZ    FMTDEMX                                                          
         CLI   DEMOTYPE,C'R'       TEST RATING                                  
         BNE   FMTDEM4                                                          
         C     R0,=F'99999'        MAX WITH DEC IS 9999.9                       
         BNH   FMTDEM2                                                          
* DEMO VALUE MORE THAN 5 DIGITS SO DROP 1 DECIMAL PLACE                         
         SRDL  R0,32                                                            
         AR    R1,R1               X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,PRTDEMO),0                                               
         B     FMTDEM10                                                         
*                                                                               
FMTDEM2  EDIT  (R0),(6,PRTDEMO),1                                               
         B     FMTDEM10                                                         
*                                                                               
FMTDEM4  EDIT  (R1),(6,PRTDEMO)    IMPS HAVE NO DECIMALS                        
                                                                                
*==========================================================                     
* FORMAT CPP                                                                    
*==========================================================                     
                                                                                
FMTDEM10 L     R1,DOLS             GET DOLLARS                                  
         M     R0,=F'20'           X 10 X 2                                     
         OC    DEMO,DEMO                                                        
         BZ    FMTDEMX                                                          
         D     R0,DEMO             DIVIDE BY UNEQUIV POINTS                     
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
         C     R1,=F'99999'        MAX CPP WITH CENTS IS 999.99                 
         BNH   FMTDEM12                                                         
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'10000'        /100 * 100                                   
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,PRTCPP)                                                  
         B     FMTDEMX                                                          
*                                                                               
FMTDEM12 EDIT  (R1),(6,PRTCPP),2                                                
*                                                                               
FMTDEMX  B     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
         DROP  RB,RC                                                            
         DS    0D                                                               
         USING *,RF                                                             
MYHDHK   NTR1                                                                   
         LM    R9,RC,HDHKR9                                                     
         DROP  RF                                                               
         USING SPPM02,RB,RC                                                     
         USING SPWORKD,RA,R9                                                    
*                                                                               
         MVC   H4+106(12),QUESTOR                                               
*                                                                               
         CLI   Q2NET,C'Y'          TEST REPORT NET DOLLARS                      
         BNE   *+10                                                             
         MVC   H4+55(21),=CL21'STATION NET DOLLARS'                             
*                                                                               
         CLI   QOPT2,C'F'                                                       
         BL    MYHD10                                                           
         BH    MYHD2                                                            
         MVC   H5+61(9),=C'F I N A L'                                           
         B     MYHD10                                                           
*                                                                               
MYHD2    MVC   H5+60(13),=C'REVISED FINAL'                                      
*                                                                               
MYHD10   MVC   H14+1(30),H5+7      MOVE MKTNUM/MKTNAME ONE POSN RIGHT           
         MVC   H5+7(31),H14                                                     
         MVC   H14(30),SPACES                                                   
*                                                                               
         MVI   MIDNEXT,0           CLEAR MIDHOOK ROUTINE NUMBER                 
         XIT1                                                                   
*                                                                               
HDHKR9   DS    A                                                                
HDHKRA   DS    A                                                                
HDHKRB   DS    A                                                                
HDHKRC   DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* MIDHOOK ROUTINES                                                              
*==============================================================                 
         SPACE 1                                                                
         DROP  RB,RC                                                            
         DS    0D                                                               
         USING *,RF                                                             
MYMIDHK  NTR1                                                                   
         LM    R9,RC,MIDHKR9                                                    
         DROP  RF                                                               
         USING SPPM02,RB,RC                                                     
         USING SPWORKD,RA,R9                                                    
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         CLI   RCSUBPRG,1          TEST PRINTING SCHEDULE                       
         BE    MYMIDSK                                                          
*                                                                               
         CLI   MIDNEXT,0           TEST FIRST MIDHOOK CALL                      
         BNE   MYMID10                                                          
*                                                                               
         MVC   MID1+58(16),=C'STATION ANALYSIS'                                 
         CLI   RCSUBPRG,2                                                       
         BE    MYMID2                                                           
         MVC   MID1+58(16),=C'DAYPART ANALYSIS'                                 
         CLI   RCSUBPRG,3                                                       
         BE    MYMID2                                                           
         MVC   MID1+56(19),=C'SPOTLENGTH ANALYSIS'                              
         CLI   RCSUBPRG,4                                                       
         BE    MYMID2                                                           
         MVC   MID1+56(19),=CL19'DPT/SLN ANALYSIS'                              
*                                                                               
MYMID2   MVI   MID2,0              FORCE SECOND LINE TO PRINT                   
         MVI   MIDHOOK,C'R'        REQUEST RETURN CALL                          
         MVI   MIDNEXT,1           SET NEXT ROUTINE                             
         B     MYMIDHKX                                                         
*                                                                               
MYMID10  CLI   MIDNEXT,1                                                        
         BNE   MYMIDHKX                                                         
*                                                                               
         MVC   MID1(24),MIDSTA     MOVE STATION ANALYSIS TITLES                 
         MVC   MID2(24),MIDSTA2                                                 
         CLI   RCSUBPRG,2                                                       
         BE    MYMID20                                                          
*                                                                               
         MVC   MID1(24),MIDDPT     MOVE DAYPART ANALYSIS TITLES                 
         MVC   MID2(24),MIDDPT2                                                 
         CLI   RCSUBPRG,3                                                       
         BE    MYMID20                                                          
*                                                                               
         MVC   MID1(24),MIDSLN     MOVE SLN ANALYSIS TITLES                     
         MVC   MID2(24),MIDSLN2                                                 
*                                                                               
         CLI   RCSUBPRG,4                                                       
         BE    MYMID20                                                          
*                                                                               
         MVC   MID1(24),MIDDLN     MOVE DPT/SLN ANALYSIS TITLES                 
         MVC   MID2(24),MIDDLN2                                                 
*                                                                               
MYMID20  MVC   MID1+25(MIDDEMX-MIDDEMS),MIDDEMS                                 
         MVC   MID2+25(MIDDEMX-MIDDEMS),MIDDEM2                                 
*                                                                               
* NOW FILL IN DEMO NAMES                                                        
*                                                                               
         LHI   R0,6                                                             
         LA    R1,MID1+25                                                       
         LA    RE,SVDEMNMS                                                      
         L     RF,SVPRDBUF                                                      
         LA    RF,PTDEMLST-PTBUFFD(RF)                                          
*                                                                               
MYMID22  MVC   3(6,R1),0(RE)       MOVE DEMO NAME (OR SPACES)                   
*                                                                               
* (REMEMBER DEMO LIST GOT FILLED IN WITH DUMMY RHOMES - DON'T TEST IT)          
*                                                                               
         CLC   0(3,RE),SPACES      IF NO NAME, NO DEMO                          
         BH    MYMID24                                                          
         MVC   0(13,R1),SPACES                                                  
         MVC   132(13,R1),SPACES                                                
         B     MYMID26                                                          
*                                                                               
MYMID24  CLI   1(RF),C'R'          TEST RATING                                  
         BE    MYMID26                                                          
         MVC   132(12,R1),=C'(IMP)  (CPM)'                                      
*                                                                               
MYMID26  LA    R1,13(R1)                                                        
         LA    RE,6(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,MYMID22                                                       
*                                                                               
         MVI   MIDNEXT,0           CLEAR 'NEXT' ROUTINE NUMBER                  
*                                                                               
MYMIDHKX XIT1                                                                   
         EJECT                                                                  
*=================================================================              
* SPECIAL MIDLINES FOR SCHEDULE                                                 
*=================================================================              
         SPACE 1                                                                
MYMIDSK  CLI   MIDNEXT,0           TEST FIRST TIME                              
         BNE   MYSK10                                                           
* FIRST MIDLINE                                                                 
         LA    RE,MID1+SKGRID+1-P                                               
         MVC   0(42,RE),MYDATES    PRINT MO TU WE ...                           
         MVI   MIDHOOK,C'R'                                                     
         MVI   MIDNEXT,1                                                        
         B     MYMIDHKX                                                         
*                                                                               
MYSK10   CLI   MIDNEXT,1                                                        
         BNE   MYSK20                                                           
         MVC   MID1(41),MIDSKD     DAYS/TIMES ...                               
         MVC   MID2(41),MIDSKD2    COST/PROG                                    
*                                                                               
         LA    RE,MID1+SKGRID+1-P                                               
         MVC   0(42,RE),MYDATES+L'MYDATES                                       
         MVC   132(42,RE),MYDATES+2*L'MYDATES                                   
* NOW FILL IN DEMO NAMES AND TYPES                                              
         LHI   R0,6                                                             
         LA    R1,MID1+SKDEMS-P                                                 
         LA    RE,SVDEMNMS                                                      
         L     RF,SVPRDBUF                                                      
         LA    RF,PTDEMLST-PTBUFFD(RF)                                          
*                                                                               
* (REMEMBER DEMO LIST GOT FILLED IN WITH DUMMY RHOMES - DON'T TEST IT)          
*                                                                               
MYSK12   CLC   0(3,RE),SPACES      IF NO NAME, NO DEMO                          
         BNH   MYSK16                                                           
         MVC   0(6,R1),0(RE)       MOVE DEMO NAME (OR SPACES)                   
         MVC   132(6,R1),=C' (RTG)'                                             
         CLI   1(RF),C'R'          TEST RATING                                  
         BE    *+10                                                             
         MVC   132(6,R1),=C' (IMP)'                                             
*                                                                               
MYSK16   LA    R1,7(R1)                                                         
         LA    RE,6(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,MYSK12                                                        
*                                                                               
         MVC   0(6,R1),=C'RA1849'                                               
         MVC   7(6,R1),=C'RV1234'                                               
         MVC   132(6,R1),=C' (RTG)'                                             
         MVC   139(6,R1),=C' (RTG)'                                             
*                                                                               
         MVI   MIDNEXT,2                                                        
         MVI   MIDHOOK,C'R'        REQUEST RETURN                               
         B     MYMIDHKX                                                         
*                                                                               
MYSK20   CLI   MIDNEXT,2                                                        
         BNE   MYSK30                                                           
         MVC   MID1(8),=C'STATION:'                                             
         MVC   MID1+9(9),BIGSTA                                                 
         L     RE,ADSTAT                                                        
         USING STARECD,RE                                                       
         MVC   MID1+22(3),SNETWRK                                               
         DROP  RE                                                               
*                                                                               
         MVI   MIDNEXT,0                                                        
         B     MYMIDHKX                                                         
*                                                                               
MYSK30   DC    H'0'                                                             
*                                                                               
MIDHKR9  DS    A                                                                
MIDHKRA  DS    A                                                                
MIDHKRB  DS    A                                                                
MIDHKRC  DS    A                                                                
MIDNEXT  DS    X                                                                
*                                                                               
MIDSKD   DC    CL35'-DAYS-  ---TIME---  DPT LEN TOT    '                        
MIDSKD2  DC    CL35'      --COST--  ---PROGRAMMING--  '                         
MIDSTA   DC    C'STAT   SPOTS  -- COST --'                                      
MIDSTA2  DC    C'----   -----  ----------'                                      
MIDDPT   DC    C'DPT    SPOTS  -- COST --'                                      
MIDDPT2  DC    C'---    -----  ----------'                                      
MIDSLN   DC    C'SLN    SPOTS  -- COST --'                                      
MIDSLN2  DC    C'---    -----  ----------'                                      
MIDDLN   DC    C'DPT/LN SPOTS  -- COST --'                                      
MIDDLN2  DC    C'------ -----  ----------'                                      
*                                                                               
MIDDEMS  DC    C'---XXXXXX--- ---XXXXXX--- ---XXXXXX--- '                       
         DC    C'---XXXXXX--- ---XXXXXX--- ---XXXXXX--- '                       
         DC    C'---RA1849--- ---RV1234---'                                     
MIDDEMX  EQU   *                                                                
*                                                                               
MIDDEM2  DC    C'(RTG)  (CPP) (RTG)  (CPP) (RTG)  (CPP) '                       
         DC    C'(RTG)  (CPP) (RTG)  (CPP) (RTG)  (CPP) '                       
         DC    C'(RTG)  (CPP) (RTG)  (CPP)'                                     
MIDDEM2X EQU   *                                                                
*                                                                               
MIDDEM3  DC    C'------------ ------------ ------------ '                       
         DC    C'------------ ------------ ------------ '                       
         DC    C'------------ ------------ '                                    
MIDDEM3X EQU   *                                                                
         LTORG                                                                  
         EJECT                                                                  
                                                                                
BUFFERIN DC    V(BUFFERIN)                                                      
                                                                                
BUFFET   BUFFD TYPE=B,KEYLEN=L'BFKEY,COLUMNS=BFDATAN,FILE=BUFFWK,      *        
               BUFFERS=10                                                       
         EJECT                                                                  
SAVERE   DS    A                   SAVED RE VALUE                               
SVPRDBUF DS    A                   PRDBUFF ENTRY FOR THIS PRD                   
SVRERATE DS    A                                                                
BUFFCNT  DS    F                                                                
GOALCPP  DS    F                                                                
DEMO     DS    F                                                                
DOLS     DS    F                                                                
DEMOTYPE DS    C                                                                
TOTSW    DS    C                                                                
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
EQSLN    DC    X'00'                                                            
PRTDEMO  DS    CL6                                                              
PRTCPP   DS    CL6                                                              
SVNUMDEM DS    H                                                                
PSLIST   DS    XL64                                                             
SVDEMNMS DS    8CL6                DEMO NAMES                                   
TOTSPOTS DS    0XL28               14 2-BYTE ACCUMS                             
         ORG   TOTSPOTS                                                         
         DS    14XL2                                                            
*                                                                               
         DS    0D                                                               
MYDATES  DS    0XL42                                                            
         DS    14CL3               2 BYTE DAY + SPACE                           
         DS    14CL3               2 BYTE MONTH + SPACE                         
         DS    14CL3               2 BYTE DATE + SPACE                          
MYDATEX  EQU   *                                                                
                                                                                
BFREC    DS    0D                  ** BUFFERIN RECORD **                        
BFKEY    DS    0XL8                ** BUFFERIN RECORD KEY **                    
                                                                                
BFTYPE   DS    XL1                 ** REPORT TYPE **                            
BFKEOFQ  EQU   X'FF'               END OF FILE FLAG                             
                                                                                
BFKDATA  DS    0X                  ** KEY DATA **                               
                                                                                
BFK1Q    EQU   1                   STATION ANALYSIS REPORT                      
BFK1STA  DS    XL3                 STATION (X'FFFFFFF'=TOT)                     
                                                                                
         ORG   BFKDATA                                                          
BFK2Q    EQU   2                   DAYPART ANALYSIS                             
BFK2DPT  DS    CL3                 DAYPART CODE (X'FFFFFF'=TOT)                 
                                                                                
         ORG   BFKDATA                                                          
BFK3Q    EQU   3                   SPOTLENGTH ANALYSIS                          
BFK3SLN  DS    XL1                 SPOTLEN (X'FF'=TOT)                          
                                                                                
         ORG   BFKDATA                                                          
BFK4Q    EQU   4                   DPT/SLN ANALYSIS                             
BFK4DPT  DS    CL3                 DAYPART (X'FFFFFF'=TOT)                      
BFK4SLN  DS    XL1                 SPOTLEN (X'FF'=TOT)                          
                                                                                
         ORG   BFREC+L'BFKEY                                                    
BFDATA   DS    0XL4                ** BUFFERIN RECORD DATA **                   
BFSPOTS  DS    XL4                 SPOTS                                        
BFBDOL   DS    XL4                 BUY DOLLARS (PENNIES)                        
BFBEQDOL DS    XL4                 BUY EQ DOLLARS                               
BFGDOL   DS    XL4                 GOAL DOLLARS (PENNIES)                       
BFGEQDOL DS    XL4                 GOAL EQ DOLS                                 
BFBDEM1  DS    XL4                 BUY DEMO                                     
BFBEQDM1 DS    XL4                 BUY EQ DEMO                                  
BFBDEM2  DS    XL4                 BUY DEMO                                     
BFBEQDM2 DS    XL4                 BUY EQ DEMO                                  
BFBDEM3  DS    XL4                 BUY DEMO                                     
BFBEQDM3 DS    XL4                 BUY EQ DEMO                                  
BFBDEM4  DS    XL4                 BUY DEMO                                     
BFBEQDM4 DS    XL4                 BUY EQ DEMO                                  
BFBDEM5  DS    XL4                 BUY DEMO                                     
BFBEQDM5 DS    XL4                 BUY EQ DEMO                                  
BFBDEM6  DS    XL4                 BUY DEMO                                     
BFBEQDM6 DS    XL4                 BUY EQ DEMO                                  
BFBDEM7  DS    XL4                 BUY DEMO                                     
BFBEQDM7 DS    XL4                 BUY EQ DEMO                                  
BFBDEM8  DS    XL4                 BUY DEMO                                     
BFBEQDM8 DS    XL4                 BUY EQ DEMO                                  
BFGDEM   DS    XL4                 GOAL DEMO                                    
BFGEQDEM DS    XL4                 GOAL EQ DEMO                                 
BFDATAL  EQU   *-BFDATA            L'DATA                                       
BFDATAN  EQU   BFDATAL/4                                                        
                                                                                
BFRECL   EQU   *-BFREC             L'BUFFERIN RECORD                            
                                                                                
COMLINEN DS    XL1                 N'COMMENT LINES TO PRINT                     
COMLINEP DS    XL1                 N'COMMENT LINES PRINTED                      
COMLINEM EQU   14                  MAXIMUM N'COMMENT LINES                      
COMLINES DS    (COMLINEM)CL70      COMMENTS                                     
COMLINEL EQU   *-COMLINES          L'COMMENT BLOCK                              
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPREPPTBUF                                                     
COMRECD  DSECT                                                                  
       ++INCLUDE SPGENCOM                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
PSTA     DS    CL4                                                              
         ORG   PSTA                                                             
PDPT     DS    CL3                                                              
         ORG   PSTA                                                             
PSLN     DS    CL3                                                              
         ORG   PSTA+7                                                           
PSPOTS   DS    CL5                                                              
         DS    CL1                                                              
PDOLS    DS    CL10                                                             
         DS    CL1                                                              
PDEM1    DS    CL5                                                              
         DS    CL1                                                              
PCPP1    DS    CL6                                                              
         DS    CL1                                                              
PDEM2    DS    CL5                                                              
         DS    CL1                                                              
PCPP2    DS    CL6                                                              
         DS    CL1                                                              
PDEM3    DS    CL5                                                              
         DS    CL1                                                              
PCPP3    DS    CL6                                                              
         DS    CL1                                                              
PDEM4    DS    CL5                                                              
         DS    CL1                                                              
PCPP4    DS    CL6                                                              
         DS    CL1                                                              
PDEM5    DS    CL5                                                              
         DS    CL1                                                              
PCPP5    DS    CL6                                                              
         DS    CL1                                                              
PDEM6    DS    CL5                                                              
         DS    CL1                                                              
PCPP6    DS    CL6                                                              
         DS    CL1                                                              
PDEM7    DS    CL5                                                              
         DS    CL1                                                              
PCPP7    DS    CL6                                                              
         DS    CL1                                                              
PDEM8    DS    CL5                                                              
         DS    CL1                                                              
PCPP8    DS    CL6                                                              
         DS    CL1                                                              
*=====================                                                          
         ORG   P                                                                
SKDAYS   DS    CL7                                                              
         DS    CL1                                                              
SKTIME   DS    CL11                                                             
         DS    CL1                                                              
SKDPT    DS    CL3                                                              
         DS    CL1                                                              
SKSLN    DS    CL3                                                              
         DS    CL1                                                              
SKSPOTS  DS    CL3                                                              
         DS    CL2                                                              
SKGRID   DS    14CL3                                                            
         DS    CL1                                                              
SKDEMS   DS    8CL7                                                             
*                                                                               
         ORG   P2+6                                                             
SKCOST   DS    CL9                                                              
         DS    CL1                                                              
SKPROG   DS    CL16                                                             
*                                                                               
         ORG   P3+6                                                             
SKREP    DS    CL3                                                              
         DS    CL1                                                              
SKREPNAM DS    CL22                                                             
         ORG   QAREA2+36                                                        
Q2NET    DS    CL1                 REPORT NET DOLLARS                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPPM02 02/25/09'                                      
         END                                                                    
