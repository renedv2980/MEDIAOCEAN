*          DATA SET ACREP8402  AT LEVEL 031 AS OF 05/01/02                      
*PHASE AC8402A,+0                                                               
         TITLE 'OPEN ITEM TRIAL BALANCE'                                        
AC8402   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC84**,R9                                                    
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING PROGD,RC                                                         
         EJECT                                                                  
TB10     CLI   MODE,REQFRST                                                     
         BNE   TB30                                                             
         GOTO1 PROLLER,DMCB,0,(8,TAB),5,6                                       
         MVI   RCSUBPRG,0                                                       
         CLC   QUNIT(2),=C'SR'                                                  
         BNE   *+8                                                              
         MVI   RCSUBPRG,1                                                       
         MVC   QEND+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,QEND),(6,HEADMON)                                 
         GOTO1 (RF),(R1),,(1,RCMOS)                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         B     XIT                                                              
         SPACE 2                                                                
XIT      XMOD1                                                                  
         EJECT                                                                  
TB30     CLI   MODE,LEDGFRST                                                    
         BNE   TB40                                                             
         L     R4,ADLDGHIR                                                      
         USING ACHEIRD,R4                                                       
         MVI   HILEV,1                                                          
         CLI   ACHRLEVB,0                                                       
         BE    XIT                                                              
         MVI   HILEV,2                                                          
         CLI   ACHRLEVC,0                                                       
         BE    XIT                                                              
         MVI   HILEV,3                                                          
         CLI   ACHRLEVD,0                                                       
         BE    XIT                                                              
         MVI   HILEV,4                                                          
         B     XIT                                                              
         EJECT                                                                  
TB40     CLI   MODE,PROCTRNS                                                    
         BNE   TB150                                                            
         L     R4,ADTRANS                                                       
         USING TRANSD,R4                                                        
         CLI   0(R4),X'44'                                                      
         BNE   XIT                                                              
         LR    R3,R4                                                            
         SH    R3,DATADISP                                                      
         USING ACKEYD,R3                                                        
         OC    ACDTPEEL,ACDTPEEL                                                
         BNZ   XIT                 IGNORE PEELED ITEMS                          
         CLI   PROGPROF,C'Y'       TREAT NEGATIVE DEBITS AS CREDITS             
         BNE   TB63                                                             
         TM    TRNSSTAT,X'20'      IF REVERSAL IGNORE PROFILE                   
         BO    TB63                                                             
         TM    TRNSSTAT,X'80'      IF NOT DEBIT IGNORE PROFILE                  
         BZ    TB63                                                             
         CP    TRNSAMNT,=P'0'      IF NOT NEGATIVE IGNORE PROFILE               
         BH    TB63                                                             
         NI    TRNSSTAT,X'FF'-X'80' MAKE IT A CREDIT                            
         ZAP   DUB,TRNSAMNT                                                     
         MP    DUB,=P'-1'          REVERSE SIGN                                 
         ZAP   TRNSAMNT,DUB                                                     
         SPACE 1                                                                
TB63     DS    0H                  GET MOS DATE FROM MONACC                     
         USING ACMD,R2                                                          
         L     R2,AMONACC                                                       
         MVC   TRMOS,ACMMDTE                                                    
         DROP  R2                                                               
         SPACE 1                                                                
         TM    TRNSSTAT,X'80'                                                   
         BO    RY1                                                              
         ZAP   DOUBLE,TRNSAMNT     TRNSAMNT MAY BE TOO SMALL, USE DOUBL         
         MP    DOUBLE,=P'-1'     MAKE CREDITS NEGATIVE                          
         ZAP   TRNSAMNT,DOUBLE                                                  
RY1      MVI   BYTE,1              OPENING BALANCE                              
         CLC   TRMOS,RCMOS                                                      
         BL    TB120               GO AND ADD TO OPENING BAL IF OLD             
         BH    XIT                 EXIT IF TOO RECENT                           
         TM    TRNSSTAT,X'80'                                                   
         BO    RY2                                                              
         ZAP   DOUBLE,TRNSAMNT                                                  
         MP    DOUBLE,=P'-1'     PUT CREDITS BACK TO POSITIVE                   
         ZAP   TRNSAMNT,DOUBLE                                                  
RY2      CLC   QUNIT(2),=C'SR'                                                  
         BE    TB70                                                             
         MVI   BYTE,2              DEBIT                                        
         TM    TRNSSTAT,X'80'                                                   
         BO    TB120                                                            
         MVI   BYTE,3              CREDIT                                       
         B     TB120                                                            
         SPACE 2                                                                
TB70     MVI   BYTE,2              RECEIVABLE CODE                              
         TM    TRNSSTAT,X'80'                                                   
         BZ    TB72                                                             
         CLI   TRNSTYPE,3          TALENT TYPE BILLING                          
         BE    TB120                                                            
         CLI   TRNSTYPE,6          BILLING INPUT TYPES                          
         BE    TB120                                                            
         CLI   TRNSTYPE,7                                                       
         BE    TB120                                                            
         CLI   TRNSTYPE,9                                                       
         BE    TB120                                                            
         CLI   TRNSTYPE,26                                                      
         BE    TB120                                                            
         MVI   BYTE,4              NOT BILLING-PUT IN ADJUSTMENT COL            
         B     TB120                                                            
         SPACE 1                                                                
TB72     MVI   BYTE,3              CREDITS                                      
         CLI   TRNSTYPE,16         CASH RECEIPTS                                
         BE    TB120                                                            
         CLI   TRNSTYPE,30                                                      
         BNE   TB74                                                             
         CLC   TRNSNARR(9),=C'WRITE-OFF'                                        
         BNE   TB120                                                            
TB74     ZAP   DUB,TRNSAMNT                                                     
         MP    DUB,=P'-1'          OTHER CRS ARE NEGATIVE ADJS                  
         ZAP   TRNSAMNT,DUB                                                     
         MVI   BYTE,4                                                           
         B     TB120                                                            
         SPACE 2                                                                
TB120    ZIC   R0,BYTE                                                          
         ZAP   PLEIGHT,TRNSAMNT                                                 
         GOTO1 PROLLER,DMCB,3,(8,TAB),PLEIGHT,1,(R0)                            
         B     XIT                                                              
         EJECT                                                                  
TB150    CLI   MODE,ACCLAST                                                     
         BNE   TB170                                                            
         L     R4,ADACCBAL                                                      
         LTR   R4,R4                                                            
         BZ    TB151                                                            
         USING ACBALD,R4                                                        
         ZAP   PLEIGHT,ACBLFRWD                                                 
         GOTO1 PROLLER,DMCB,3,(8,TAB),PLEIGHT,1,1                               
TB151    GOTO1 PROLLER,DMCB,1,(8,TAB),1                                         
         L     R3,DMCB             COMPLETE THE LINE                            
         LA    R4,24(R3)                                                        
         CLC   QUNIT(2),=C'SR'                                                  
         BNE   *+8                                                              
         LA    R4,8(R4)            POINT TO CLOSING BALANCE                     
         ZAP   0(8,R4),0(8,R3)     OPENING BAL                                  
         AP    0(8,R4),8(8,R3)     PLUS DEBITS (BILLING)                        
         SP    0(8,R4),16(8,R3)    LESS CREDITS (CASH RECEIPTS)                 
         CLC   QUNIT(2),=C'SR'                                                  
         BNE   *+10                                                             
         AP    0(8,R4),24(8,R3)    PLUS ADJUSTMENTS (IF ANY)                    
*                                  GIVES CLOSING BALANCE                        
         SPACE 2                                                                
         GOTO1 PROLLER,DMCB,6,(8,TAB)                                           
         MVI   BYTE,1              DO ACCOUNT LEVEL TOTAL                       
         L     R2,ADACC                                                         
         L     R3,ADACCNAM                                                      
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         EJECT                                                                  
TB170    CLI   MODE,LEVCLAST                                                    
         BNE   TB180                                                            
         MVI   BYTE,2                                                           
         L     R2,ADHEIRC                                                       
         L     R3,ADLVCNAM                                                      
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         EJECT                                                                  
TB180    CLI   MODE,LEVBLAST                                                    
         BNE   TB190                                                            
         L     R3,ADLVBNAM                                                      
         LTR   R3,R3                                                            
         BZ    XIT                                                              
         ZIC   RF,HILEV                                                         
         BCTR  RF,0                                                             
         STC   RF,BYTE                                                          
         L     R2,ADHEIRB                                                       
         L     R3,ADLVBNAM                                                      
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         EJECT                                                                  
TB190    CLI   MODE,LEVALAST                                                    
         BNE   TB200                                                            
         ZIC   RF,HILEV                                                         
         STC   RF,BYTE                                                          
         L     R2,ADHEIRA                                                       
         L     R3,ADLVANAM                                                      
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         EJECT                                                                  
TB200    CLI   MODE,REQLAST                                                     
         BNE   XIT                                                              
         ZIC   RF,HILEV                                                         
         LA    RF,1(RF)                                                         
         STC   RF,BYTE                                                          
         LA    R2,SPACES                                                        
         LA    R3,REQTOT                                                        
         BAS   RE,FORMAT                                                        
         B     XIT                                                              
         SPACE 2                                                                
REQTOT   DC    CL30'TOTAL FOR REQUEST'                                          
         EJECT                                                                  
FORMAT   NTR1                                                                   
         CLI   MODE,ACCLAST                                                     
         BNE   FMT1                                                             
         MVC   P+1(12),3(R2)                                                    
         B     FMT1A                                                            
FMT1     MVC   P+5(9),=C'TOTAL FOR'                                             
         MVI   SPACING,2                                                        
FMT1A    CLI   MODE,REQLAST                                                     
         BE    FMT2                                                             
         MVC   WORK,SPACES                                                      
         USING ACNAMED,R3                                                       
         ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),ACNMNAME                                                 
         GOTO1 CHOPPER,DMCB,(36,WORK),(23,P+15),(C'P',2)                        
         B     FMT4                                                             
         SPACE 1                                                                
FMT2     MVC   P+5(30),0(R3)                                                    
FMT4     ZIC   R0,BYTE                                                          
         GOTO1 PROLLER,DMCB,1,(8,TAB),(R0)                                      
         L     R3,DMCB                                                          
         LA    R6,4                                                             
         CLC   QUNIT(2),=C'SR'                                                  
         BNE   *+8                                                              
         LA    R6,5                                                             
         LA    R5,P+40                                                          
FMT10    CP    0(8,R3),=P'0'                                                    
         BNE   FMT11                                                            
         CLI   MODE,REQLAST                                                     
         BNE   FMT12                                                            
         MVC   9(3,R5),=C'NIL'                                                  
         B     FMT12                                                            
FMT11    DS    0H                                                               
         ZAP   DUB,0(8,R3)                                                      
         CURED DUB,(14,0(R5)),2,MINUS=YES                                       
         ZAP   0(8,R3),=P'0'                                                    
FMT12    LA    R5,14(R5)                                                        
         LA    R3,8(R3)                                                         
         BCT   R6,FMT10                                                         
         SPACE 1                                                                
         CLC   P+40(70),SPACES                                                  
         BE    FMT20                                                            
         BAS   RE,MYPRT                                                         
         B     XIT                                                              
         SPACE 1                                                                
FMT20    MVC   P(50),SPACES                                                     
         MVC   PSECOND(50),SPACES                                               
         B     XIT                                                              
         EJECT                                                                  
MYPRT    NTR1                                                                   
         MVC   HEAD5+10(1),QUNIT                                                
         MVC   HEAD6+10(1),QLEDGER                                              
         MVC   HEAD5+94(L'HEADMON),HEADMON                                      
         L     R3,ADUNTNAM                                                      
         LA    R2,HEAD5+13                                                      
         BAS   RE,NAMOUT                                                        
         L     R3,ADLDGNAM                                                      
         LA    R2,HEAD6+13                                                      
         BAS   RE,NAMOUT                                                        
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         SPACE 2                                                                
NAMOUT   ZIC   RF,ACNMLEN                                                       
         SH    RF,=H'3'                                                         
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),ACNMNAME                                                 
         BR    RE                                                               
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*              DSECT FOR AC84                                                   
         SPACE 2                                                                
PROGD    DSECT                                                                  
*TAB      DS    CL188               5*6*6+8  (L*C*6)+8 BYTES                    
TAB      DS    CL248               5*6*8+8  (L*C*8)+8 BYTES                     
HEADMON  DS    CL6                                                              
TRMOS    DS    CL2                                                              
RCMOS    DS    CL3                                                              
HILEV    DS    CL1                                                              
*PLSIX    DS    PL6                                                             
PLEIGHT  DS    PL8                                                              
         SPACE 2                                                                
* ACGENBOTH                                                                     
* ACGENMODES                                                                    
* ACREPWORKD                                                                    
* ACMASTD                                                                       
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACREP8402 05/01/02'                                      
         END                                                                    
