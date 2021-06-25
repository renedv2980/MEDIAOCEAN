*          DATA SET ACREPC402  AT LEVEL 021 AS OF 05/01/02                      
*PHASE ACC402A,+0                                                               
*INCLUDE SQUASHER                                                               
*INCLUDE UNDERLIN                                                               
         TITLE 'ACC402 - PROJECT TIME CONFIRMATION'                             
ACC402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACPT**                                                       
         LA    R9,2048(RB)                                                      
         LA    R9,2048(R9)                                                      
         USING ACC402+4096,R9                                                   
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING ACPT02D,RC                                                       
         EJECT                                                                  
PT1      CLI   MODE,RUNFRST                                                     
         BNE   PT10                                                             
         RELOC RELO                                                             
         LA    RE,RELOTAB          RELOCATE A-TYPES                             
         LA    R1,ATYPES                                                        
RELOOP   L     RF,0(RE)                                                         
         LA    RF,0(RF)                                                         
         A     RF,RELO                                                          
         ST    RF,0(R1)                                                         
         LA    RE,4(RE)                                                         
         LA    R1,4(R1)                                                         
         CLI   0(RE),X'FF'                                                      
         BNE   RELOOP                                                           
         SPACE 1                                                                
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         SPACE 1                                                                
PTXIT    XIT1                                                                   
         EJECT                                                                  
PT10     CLI   MODE,REQFRST                                                     
         BNE   PT20                                                             
         L     R4,ANARRA                                                        
         XC    0(42,R4),0(R4)      CLEAR NARRATIVE                              
         CLC   QSELECT,SPACES                                                   
         BE    *+8                                                              
         BAS   RE,GETNAR                                                        
         SPACE 1                                                                
         GOTO1 DATCON,DMCB,(0,QSTART),(1,START)                                 
         GOTO1 DATCON,DMCB,(0,QEND),(1,END)                                     
         MVC   PERIOD,SPACES                                                    
         GOTO1 DATCON,DMCB,(0,QSTART),(5,PERIOD)                                
         MVI   PERIOD+8,C'-'                                                    
         GOTO1 DATCON,DMCB,(0,QEND),(5,PERIOD+9)                                
         SPACE 1                                                                
         MVI   RCSUBPRG,0                                                       
         L     R4,ADCMPNAM                                                      
         LA    R6,COMPNAM                                                       
         BAS   RE,NAMOUT                                                        
         B     PTXIT                                                            
         EJECT                                                                  
PT20     CLI   MODE,LEDGFRST                                                    
         BNE   PT30                                                             
         L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4                                                       
         SR    R2,R2                                                            
         LA    R3,ACHRLEVA                                                      
         L     R5,LEVEL                                                         
         MVI   LONGDES,7                                                        
         SPACE 1                                                                
         USING LEVD,R5                                                          
PT22     CLI   0(R3),0                                                          
         BE    PT24                                                             
         LA    R2,1(R2)                                                         
         STC   R2,NUMLEV                                                        
         MVC   LEVLN,0(R3)                                                      
         MVC   LEVDES,1(R3)                                                     
         OC    LEVDES,SPACES                                                    
         GOTO1 SQUASHER,DMCB,LEVDES,15                                          
         ZIC   R6,DMCB+7                                                        
         ZIC   R7,LONGDES                                                       
         CR    R6,R7                                                            
         BL    *+8                                                              
         STC   R6,LONGDES                                                       
         CH    R2,=H'4'                                                         
         BE    PT24                                                             
         LA    R5,LEVLEN(R5)                                                    
         LA    R3,16(R3)                                                        
         B     PT22                                                             
         SPACE 1                                                                
PT24     DC    0H'0'                                                            
         B     PTXIT                                                            
         EJECT                                                                  
PT30     CLI   MODE,LEVAFRST                                                    
         BNE   PT40                                                             
         L     R5,LEVEL                                                         
         USING LEVD,R5                                                          
         L     R2,ADHEIRA                                                       
         MVC   LEVACC,3(R2)                                                     
         L     R4,ADLVANAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         B     PTXIT                                                            
         EJECT                                                                  
PT40     CLI   MODE,LEVBFRST                                                    
         BNE   PT50                                                             
         L     R5,LEVEL                                                         
         USING LEVD,R5                                                          
         ZIC   R1,LEVLN            LENGTH OF LEVEL A                            
         LA    R5,LEVLEN(R5)                                                    
         MVC   LEVACC,SPACES                                                    
         SPACE 1                                                                
         L     R2,ADHEIRB                                                       
         LA    R2,3(R1,R2)        ACCOUNT AT LEVEL B                            
         ZIC   R3,LEVLN                                                         
         SR    R3,R1                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LEVACC(0),0(R2)                                                  
         SPACE 1                                                                
         L     R4,ADLVBNAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         B     PTXIT                                                            
         EJECT                                                                  
PT50     CLI   MODE,LEVCFRST                                                    
         BNE   PT60                                                             
         L     R5,LEVEL                                                         
         USING LEVD,R5                                                          
         LA    R5,LEVLEN(R5)                                                    
         ZIC   R1,LEVLN            LENGTH OF LEVEL B                            
         LA    R5,LEVLEN(R5)                                                    
         MVC   LEVACC,SPACES                                                    
         SPACE 1                                                                
         L     R2,ADHEIRC                                                       
         LA    R2,3(R1,R2)         ACCOUNT AT LEVEL C                           
         ZIC   R3,LEVLN                                                         
         SR    R3,R1                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LEVACC(0),0(R2)                                                  
         SPACE 1                                                                
         L     R4,ADLVCNAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         B     PTXIT                                                            
         EJECT                                                                  
PT60     CLI   MODE,PROCACC                                                     
         BNE   PT70                                                             
         L     R5,LEVEL                                                         
         USING LEVD,R5                                                          
         LA    R5,LEVLEN(R5)       LENGTH OF LEVEL B                            
         CLI   NUMLEV,4                                                         
         BNE   *+8                                                              
         LA    R5,LEVLEN(R5)       OR C IF THERE ARE 4                          
         ZIC   R1,LEVLN                                                         
         LA    R5,LEVLEN(R5)                                                    
         MVC   LEVACC,SPACES                                                    
         ST    R5,LOWACC                                                        
         SPACE 1                                                                
         L     R2,ADHEIRC                                                       
         CLI   NUMLEV,4                                                         
         BNE   *+8                                                              
         L     R2,ADHEIRD                                                       
         LA    R2,3(R1,R2)                                                      
         ZIC   R3,LEVLN                                                         
         SR    R3,R1                                                            
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   LEVACC(0),0(R2)                                                  
         SPACE 1                                                                
         L     R4,ADLVCNAM                                                      
         CLI   NUMLEV,4                                                         
         BNE   *+8                                                              
         L     R4,ADLVDNAM                                                      
         LA    R6,LEVNAM                                                        
         BAS   RE,NAMOUT                                                        
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         ZAP   TOTHRS,=P'0'                                                     
         B     PTXIT                                                            
         EJECT                                                                  
PT70     CLI   MODE,PROCTRNS                                                    
         BNE   PT80                                                             
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   TRNSEL,X'44'                                                     
         BNE   PTXIT                                                            
         CLC   TRNSDATE,START                                                   
         BL    PTXIT                                                            
         CLC   TRNSDATE,END                                                     
         BH    PTXIT                                                            
         SPACE 1                                                                
         USING TASKD,R6                                                         
         LA    R6,WRKA                                                          
         XC    WRKA,WRKA                                                        
         MVC   TSKDTE,TRNSDATE                                                  
         MVI   ELCODE,X'50'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   PTXIT                                                            
         SPACE 1                                                                
         USING TRCASHD,R4                                                       
         CLI   TRCSTYPE,C'H'                                                    
         BNE   PTXIT                                                            
         ZAP   TSKHRS,TRCSAMNT                                                  
         SPACE 1                                                                
PT72     L     R4,ADTRANS                                                       
         MVI   ELCODE,X'51'                                                     
         BAS   RE,NEXTEL           NO PROJECT ELEMENT                           
         BNE   PTXIT                                                            
         SPACE 1                                                                
         USING ACPCD,R4                                                         
         CLI   ACPCLEN,X'22'       X22 SIGNIFIES 1J --X11 FOR SJ OR 1N          
         BE    PT74                THERE IS A PROJECT OR TASK                   
         MVC   TSKUL(14),ACPCCLI+1 MOVE U/L 1N OR SJ AND ITS ACCT CODE          
         B     *+10                                                             
         SPACE 1                                                                
PT74     MVC   TSKUL(16),ACPCPRJT+1 CLI/DIV/PROJECT/TASK                        
         AP    TOTHRS,TSKHRS                                                    
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,(R6)                                  
         B     PTXIT                                                            
         EJECT                                                                  
PT80     CLI   MODE,ACCLAST                                                     
         BNE   PTXIT                                                            
         USING TASKD,R6                                                         
PT80B    LA    R6,WRKA                                                          
         XC    WRKA,WRKA                                                        
         GOTO1 BUFFALO,DMCB,=C'HIGH',ABUFF,(R6),1                               
         TM    DMCB+8,X'80'                                                     
         BO    PTXIT                                                            
         B     PT82                                                             
         SPACE 1                                                                
PT81     GOTO1 BUFFALO,DMCB,=C'SEQ',ABUFF,(R6),1                                
         TM    DMCB+8,X'80'                                                     
         BO    PT85                                                             
         SPACE 1                                                                
PT82     MVC   P+1(3),TSKCLI                                                    
         MVC   P+5(3),TSKDIV                                                    
         MVC   P+9(6),TSKPRJ                                                    
         BAS   RE,PRJNME           GET PROJECT NAME                             
         MVC   P+16(36),WORK       TO PRINT                                     
         CLC   TSKUL,=C'1J'        IS IT A 1J ACCOUNT                           
         BNE   PT83                NO - ITS FROM SJ OR 1N                       
         MVC   P+54(2),TSKCDE      TASK CODE TO PRINT LINE                      
         BAS   RE,CDENME           GO GET TASK NAME                             
         MVC   P+57(15),WORK       TASK NAME TO PRINT LINE                      
PT83     GOTO1 DATCON,DMCB,(1,TSKDTE),(5,P+78)                                  
         LA    R2,TSKHRS                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         B     PT81                                                             
         SPACE 1                                                                
PT85     MVC   P+79(5),=C'TOTAL'                                                
         LA    R2,TOTHRS                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,PRNTIT                                                        
         SPACE 1                                                                
         L     R4,ANARRA                                                        
         CLI   0(R4),0                                                          
         BE    PT89                                                             
         BAS   RE,PRNTIT           SKIP A LINE                                  
         BAS   RE,PNARR                                                         
         SPACE 1                                                                
PT89     GOTO1 BUFFALO,DMCB,=C'CLEAR',ABUFF,(X'80',1)                           
         L     R3,ADACC                                                         
         USING ACKEYD,R4                                                        
         L     R4,RECORD                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC,0(R3)                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         CLI   DMCB+8,0                                                         
         BE    PTXIT                                                            
         DC    H'0'                CAN'T READ ACCOUNT                           
         EJECT                                                                  
PRNTIT   NTR1                                                                   
         MVC   HEAD3+93(L'PERIOD),PERIOD                                        
         ZIC   R3,LONGDES                                                       
         LA    R2,HEAD3+1                                                       
         LA    R2,2(R3,R2)                                                      
         MVC   0(36,R2),COMPNAM    COMPANY NAME                                 
         SPACE 1                                                                
         LA    R4,HEAD4                                                         
         LA    R2,132(R2)                                                       
         L     R5,LEVEL            LEVEL A  ACCOUNT AND NAME                    
         USING LEVD,R5                                                          
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),LEVDES                                                   
         MVC   0(48,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         SPACE 1                                                                
         LA    R4,132(R4)                                                       
         LA    R2,132(R2)          LEVEL B ACCOUNT AND NAME                     
         LA    R5,LEVLEN(R5)                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),LEVDES                                                   
         MVC   0(48,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         SPACE 1                                                                
         LA    R4,132(R4)                                                       
         LA    R2,132(R2)          LEVEL C ACCOUNT AND NAME                     
         LA    R5,LEVLEN(R5)                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),LEVDES                                                   
         MVC   0(48,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         SPACE 1                                                                
         CLI   NUMLEV,4                                                         
         BNE   *+16                                                             
         LA    R4,132(R4)                                                       
         LA    R2,132(R2)           C ACCOUNT AND NAME                          
         LA    R5,LEVLEN(R5)                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R4),LEVDES                                                   
         MVC   0(48,R2),LEVACC                                                  
         GOTO1 SQUASHER,DMCB,(R2),48                                            
         SPACE 1                                                                
         GOTO1 ACREPORT                                                         
         B     PTXIT                                                            
         EJECT                                                                  
*              ROUTINE TO GET NARRATIVE                                         
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
GETNAR   NTR1                                                                   
         L     R4,ANARRA                                                        
         XC    ACKEYACC(42),ACKEYACC                                            
         MVI   ACKEYACC,X'0C'                                                   
         MVC   ACKEYACC+1(1),RCCOMPFL                                           
         PACK  DUB1,QSELECT(2)                                                  
         LA    R2,ACKEYACC+2                                                    
         EDIT  (P8,DUB1),(6,0(R2))                                              
         GOTO1 DATAMGR,DMCB,DMREAD,=C'ACCOUNT',(R4),(R4)                        
         CLI   DMCB+8,0                                                         
         BE    PTXIT               FOUND IT                                     
         XC    ACKEYACC(42),ACKEYACC     NOT FOUND CLEAR AREA                   
         B     PTXIT                                                            
         EJECT                                                                  
*              ROUTINE TO PRINT NARRATIVE                                       
         SPACE 1                                                                
PNARR    NTR1                                                                   
         L     R4,ANARRA                                                        
         MVI   ELCODE,X'3E'                                                     
         BAS   RE,GETEL                                                         
         BNE   PTXIT                                                            
         USING ACOMMD,R4                                                        
PNARR2   ZIC   R1,ACOMLEN                                                       
         SH    R1,=H'5'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   P+1(0),ACOMMENT                                                  
         BAS   RE,PRNTIT                                                        
         BAS   RE,NEXTEL                                                        
         BE    PNARR2                                                           
         B     PTXIT                                                            
         EJECT                                                                  
*              ROUTINE TO GET PROJECT NAMES                                     
         SPACE 1                                                                
         USING TASKD,R6                                                         
         USING PRJD,R5                                                          
PRJNME   NTR1                                                                   
         MVC   WRKB,SPACES                                                      
         LA    R5,WRKB                                                          
         MVC   PRJK(1),QCOMPANY    COMPANY CODE INTO KEY                        
         SPACE 1                                                                
PRJNM1   MVC   PRJK+1(14),TSKUL    U/L AND EITHER 1J 1N OR SJ ACCT              
         L     R3,PRJLIST                                                       
         SPACE 1                                                                
         USING BIND,R3                                                          
         OC    BININ,BININ                                                      
         BZ    PRJRD               NOTHING IN TABLE YET                         
         L     R1,BININ                                                         
         LA    R2,BINTABLE                                                      
PRJNM2   CLC   PRJK,0(R2)          LOOK FOR PROJECT NAME IN TABLE               
         BE    PRJNM3                                                           
         LA    R2,PRJLEN(R2)                                                    
         BCT   R1,PRJNM2                                                        
         B     PRJRD               IF NOT FOUND READ IT                         
PRJNM3   LR    R5,R2               AND ADD TO TABLE                             
         MVC   WORK,SPACES                                                      
         MVC   WORK(36),PRJNAME                                                 
         B     PTXIT                                                            
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
PRJRD    L     R4,ADACC            READ 1J TO GET PROJECT NAME                  
         MVC   SAVEKEY,ACKEYACC                                                 
         L     R4,RECORD                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVC   ACKEYACC,PRJK                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',(R4),(R4)                    
         CLC   ACKEYACC,PRJK                                                    
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND PROJECT RECORD                    
         SPACE 1                                                                
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND NAME ELEMENT                      
         LA    R6,PRJNAME                                                       
         BAS   RE,NAMOUT                                                        
         SPACE 1                                                                
         L     R3,PRJLIST                                                       
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,PRJK),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         MVC   WORK(36),PRJNAME                                                 
         SPACE 1                                                                
PRJXIT   DC    0H'0'                                                            
         B     PTXIT                                                            
         EJECT                                                                  
*              ROUTINE TO GET TASK NAME                                         
         SPACE 1                                                                
         USING TASKD,R6                                                         
         USING CODED,R5                                                         
CDENME   NTR1                                                                   
         MVC   WRKB,SPACES                                                      
         LA    R5,WRKB                                                          
         MVC   CODE,TSKCDE                                                      
         L     R3,CDELIST                                                       
         SPACE 1                                                                
         USING BIND,R3                                                          
         OC    BININ,BININ                                                      
         BZ    CDERD               NOTHING IN TABLE YET                         
         L     R1,BININ                                                         
         LA    R2,BINTABLE                                                      
CDENM1   CLC   CODE,0(R2)          LOOK FOR PROJECT NAME IN TABLE               
         BE    CDENM3                                                           
         LA    R2,CDELEN(R2)                                                    
         BCT   R1,CDENM1                                                        
         B     CDERD               IF NOT FOUND READ IT                         
CDENM3   LR    R5,R2               AND ADD TO TABLE                             
         MVC   WORK,SPACES                                                      
         MVC   WORK(15),CDENAME                                                 
         B     PTXIT                                                            
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
CDERD    L     R4,RECORD                                                        
         MVC   ACKEYACC(42),SPACES                                              
         MVI   ACKEYACC,X'0A'                                                   
         MVC   ACKEYACC+1(1),QCOMPANY                                           
         MVC   ACKEYACC+2(2),=C'1J'                                             
         MVC   ACKEYACC+4(2),CODE                                               
         MVC   SAVEKEY,ACKEYACC                                                 
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',(R4),(R4)                    
         CLC   SAVEKEY,ACKEYACC                                                 
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND PROJECT RECORD                    
         SPACE 1                                                                
         MVI   ELCODE,X'12'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                CAN'T FIND NAME ELEMENT                      
         USING ACANALD,R4                                                       
         MVC   CDENAME,ACANDESC                                                 
         SPACE 1                                                                
         L     R3,CDELIST                                                       
         MVC   DMCB+8(16),BININ                                                 
         LA    R2,BINTABLE                                                      
         GOTO1 BINSRCH,DMCB,(1,CODE),(R2)                                       
         OC    DMCB(4),DMCB                                                     
         BNZ   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         MVC   BININ,DMCB+8        UPDATE COUNT                                 
         MVC   WORK(15),CDENAME                                                 
         SPACE 1                                                                
CDEXIT   DC    0H'0'                                                            
         B     PTXIT                                                            
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
NAMOUT   LTR   R4,R4                                                            
         BZR   RE                                                               
         MVC   0(36,R6),SPACES                                                  
         ZIC   R3,ACNMLEN                                                       
         SH    R3,=H'3'                                                         
         EX    R3,*+6                                                           
         BR    RE                                                               
         MVC   0(0,R6),ACNMNAME                                                 
         SPACE 1                                                                
FORMAT   EDIT  (P8,0(R2)),(9,P+91),2,MINUS=YES                                  
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              CONSTANTS                                                        
         SPACE 1                                                                
RELOTAB  DS    0A                                                               
         DC    V(SQUASHER)                                                      
         DC    A(ALEVEL)                                                        
         DC    A(APRJLIST)                                                      
         DC    A(ARECORD)                                                       
         DC    V(UNDERLIN)                                                      
         DC    V(BUFFALOC)                                                      
         DC    A(NARR)                                                          
         DC    A(ACDELIST)                                                      
         DC    X'FF'                                                            
         EJECT                                                                  
*              DSECT FOR WORKING STORAGE                                        
ACPT02D  DSECT                                                                  
RELO     DS    F                                                                
ATYPES   DS    0A                                                               
SQUASHER DS    V                                                                
LEVEL    DS    A                                                                
PRJLIST  DS    A                                                                
RECORD   DS    A                                                                
UNDERLIN DS    V                                                                
ABUFF    DS    V                                                                
ANARRA   DS    A                                                                
CDELIST  DS    A                                                                
         SPACE 1                                                                
TOTHRS   DS    PL8                                                              
SAVEKEY  DS    CL15                                                             
         SPACE 1                                                                
START    DS    CL3                                                              
END      DS    CL3                                                              
PERIOD   DS    CL19                                                             
         SPACE 1                                                                
DUB1     DS    D                                                                
ELCODE   DS    CL1                                                              
COMPNAM  DS    CL36                                                             
LONGDES  DS    CL1                 LENGTH OF LONGEST DESCRIPTION                
NUMLEV   DS    CL1                 NUMBER OF LEVELS                             
LOWACC   DS    A                   ADDRESS OF LOW ACCOUNT                       
LOWDESC  DS    CL31                DESCRIPTION OF TWO LOWEST LEVELS             
WRKA     DS    CL100                                                            
WRKB     DS    CL100                                                            
         EJECT                                                                  
*              DSECT FOR LEVEL DESCRIPTION AND NAMES                            
         SPACE 1                                                                
LEVD     DSECT                                                                  
LEVLN    DS    CL1                 LENGTH OF LEVEL                              
LEVDES   DS    CL15                DESCRIPTION                                  
LEVACC   DS    CL12                ACCOUNT CODE                                 
LEVNAM   DS    CL36                ACCOUNT NAME                                 
LEVLEN   EQU   *-LEVLN                                                          
         SPACE 1                                                                
*              DSECT FOR PROJECT/TASK TABLE                                     
         SPACE 1                                                                
TASKD    DSECT                                                                  
TSKDTE   DS    CL3                 TASK TRANSACTION DATE                        
TSKUL    DS    CL2                 UNIT / LEDGER                                
TSKCLI   DS    CL3                 CLIENT                                       
TSKDIV   DS    CL3                 DIVISION                                     
TSKPRJ   DS    CL6                 PROJECT                                      
TSKCDE   DS    CL2                 TASK CODE                                    
TSKHRS   DS    PL8                 HOURS                                        
TSKLEN   EQU   *-TSKUL                                                          
         SPACE 1                                                                
*              DSECT FOR THE PROJECT NAME RECORD                                
         SPACE 1                                                                
PRJD     DSECT                                                                  
PRJK     DS    CL15                KEY                                          
PRJNAME  DS    CL36                NAME                                         
PRJLEN   EQU   *-PRJK                                                           
         SPACE 1                                                                
*              DSECT FOR CODE LIST TABLE                                        
         SPACE 1                                                                
CODED    DSECT                                                                  
CODE     DS    CL2                 TASK CODE                                    
CDENAME  DS    CL15                TASK DESCRIPTION                             
CDELEN   EQU   *-CODE                                                           
         SPACE 1                                                                
*              DSECT FOR THE BINSRCH LIST                                       
         SPACE 1                                                                
BIND     DSECT                                                                  
BININ    DS    F                   NUMBER IN TABLE                              
BINLEN   DS    F                   RECORD LENGTH                                
BINDISP  DS    CL1                 DISPLACEMENT IN RECORD                       
BINKEY   DS    CL3                 KEY LENGTH                                   
BINMAX   DS    F                   MAXIMUM NUMBER IN TABLE                      
BINTABLE DS    0CL1                                                             
         EJECT                                                                  
         BUFF  LINES=100,ROWS=1,COLUMNS=1,FLAVOR=P,KEYLIST=(19,A)               
ACC402   CSECT                                                                  
         ENTRY ALEVEL                                                           
ALEVEL   DS    0D                                                               
         DS    (4*LEVLEN)C                                                      
         SPACE 1                                                                
         ENTRY APRJLIST                                                         
APRJLIST DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(PRJLEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'PRJK)         KEY LENGTH                                   
         DC    F'2000'             MAX IN TABLE                                 
         DS    (2000*PRJLEN)C       THE TABLE                                   
         SPACE 1                                                                
         ENTRY ACDELIST                                                         
ACDELIST DS    0D                                                               
         DC    F'0'                NUMBER IN TABLE                              
         DC    AL3(0)                                                           
         DC    AL1(CDELEN)         RECORD LENGTH                                
         DC    AL3(0)              DISP TO KEY                                  
         DC    AL1(L'CODE)         KEY LENGTH                                   
         DC    F'500'              MAX IN TABLE                                 
         DS    (500*CDELEN)C       THE TABLE                                    
         SPACE 1                                                                
         ENTRY ARECORD                                                          
ARECORD  DS    0D                                                               
         DS    CL42                                                             
         DS    CL2000                                                           
         SPACE 1                                                                
         ENTRY NARR                                                             
NARR     DS    0D                                                               
         DS    CL42                                                             
         DS    CL2000                                                           
         SPACE 2                                                                
*ACGENBOTH                                                                      
*ACREPWORKD                                                                     
*ACGENMODES                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021ACREPC402 05/01/02'                                      
         END                                                                    
