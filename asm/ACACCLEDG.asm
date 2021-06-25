*          DATA SET ACACCLEDG  AT LEVEL 009 AS OF 05/01/02                      
*PHASE ACCLEDG,+0                                                               
*INCLUDE SQUASHER                                                               
         TITLE 'EXTERNAL TO PRINT COMPANY/UNIT/LEDGER/ INFO'                    
ACCLEDG  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACLEDG                                                       
         LR    RA,R1                                                            
         USING ACCWORKD,RA                                                      
         L     R8,VCPRINT                                                       
         USING DPRINT,R8                                                        
         CLI   OVSWITCH,X'FF'                                                   
         BE    ACCLAST                                                          
         CLI   OVSWITCH,0                                                       
         BNE   ACLST                                                            
         MVI   OVSWITCH,1                                                       
         MVC   TITLE,SPACES                                                     
         MVC   TITLE+5(26),=C'COMPANY/UNIT/LEDGER REPORT'                       
         LA    R3,MID1                                                          
         LA    R5,SUB3-1                                                        
         LA    R4,132                                                           
         MVC   0(132,R3),SPACES                                                 
         BXLE  R3,R4,*-6                                                        
         MVC   OLDKEY,SPACES                                                    
         ZAP   CNTS,=P'0'                                                       
         SPACE 1                                                                
ACEXT    XIT1                                                                   
         SPACE 2                                                                
ACCLAST  BAS   R7,PCNTS                                                         
         B     ACEXT                                                            
         EJECT                                                                  
ACLST    L     R4,AIOAREA                                                       
         LA    R4,4(R4)                                                         
         USING ACKEYD,R4                                                        
         CLI   ACKEYACC,C' '                                                    
         BL    ACEXT                                                            
         CLI   ACKEYACC,C'9'                                                    
         BH    ACEXT                                                            
         SPACE 1                                                                
         CLC   ACKEYCON,SPACES                                                  
         BNE   ACEXT                                                            
         CLC   ACKEYACC+1(14),SPACES                                            
         BE    ACLCMP                                                           
         CLC   ACKEYACC+2(13),SPACES                                            
         BE    ACLUNT                                                           
         CLC   ACKEYACC+3(12),SPACES                                            
         BE    ACLEDG                                                           
         AP    CNTS,=P'1'                                                       
         B     ACEXT                                                            
         SPACE 3                                                                
ACLCMP   CLC   OLDKEY,SPACES                                                    
         BE    *+8                                                              
         BAS   R7,PCNTS                                                         
         ZAP   LINE,=P'99'                                                      
         MVC   OLDKEY,ACKEYACC                                                  
         SPACE 1                                                                
         MVC   P+1(7),=C'COMPANY'                                               
         GOTO1 VHEXOUT,DMCB,OLDKEY,P+9,1,=C'TOG'                                
         BAS   R6,PRT                                                           
         MVI   P+1,C'-'                                                         
         MVC   P+2(9),P+1                                                       
         BAS   R6,PRT2                                                          
         SPACE 1                                                                
         MVC   P+1(4),=C'NAME'                                                  
         LA    R2,P+10                                                          
         BAS   R7,NAMOUT                                                        
         BAS   R6,PRT2                                                          
         SPACE 1                                                                
ACLADDR  MVI   ELCODE,X'22'                                                     
         L     R4,AIOAREA                                                       
         LA    R4,4(R4)                                                         
         BAS   RE,GETEL                                                         
         BNE   ACLCSAT                                                          
         USING ACADDD,R4                                                        
         MVC   P+1(7),=C'ADDRESS'                                               
         XR    R3,R3                                                            
         IC    R3,ACADLNES                                                      
         LA    R4,ACADADD                                                       
         MVC   P+10(26),0(R4)                                                   
         BAS   R6,PRT                                                           
         LA    R4,26(R4)                                                        
         BCT   R3,*-14                                                          
         BAS   R6,PRT                                                           
         SPACE 1                                                                
ACLCSAT  L     R4,AIOAREA                                                       
         LA    R4,4(R4)                                                         
         MVI   ELCODE,X'10'                                                     
         BAS   RE,GETEL                                                         
         BNE   ACEXT                                                            
         USING ACCOMPD,R4                                                       
         MVC   P+1(4),=C'LOGO'                                                  
         MVC   P+10(7),ACMPABBR                                                 
         BAS   R6,PRT2                                                          
         SPACE 1                                                                
         CLI   ACMPSTAT,0                                                       
         BE    ACEXT                                                            
         MVC   P+1(7),=C'OPTIONS'                                               
         TM    ACMPSTAT,X'80'                                                   
         BNO   *+14                                                             
         MVC   P+10(28),=C'INVOICE/ORDER MATCH REQUIRED'                        
         BAS   R6,PRT                                                           
         TM    ACMPSTAT,X'40'                                                   
         BNO   *+14                                                             
         MVC   P+10(30),=C'CHECK INVOICE CASH VS ESTIMATE'                      
         BAS   R6,PRT                                                           
         TM    ACMPSTAT,X'20'                                                   
         BNO   *+14                                                             
         MVC   P+10(26),=C'OFFICE REQUIRED ON EXPENSE'                          
         BAS   R6,PRT                                                           
         TM    ACMPSTAT,X'10'                                                   
         BNO   *+14                                                             
         MVC   P+10(10),=C'ON COSTING'                                          
         BAS   R6,PRT                                                           
         TM    ACMPSTAT,X'08'                                                   
         BNO   *+14                                                             
         MVC   P+10(28),=C'TAKE CD ON EXPENSE AS INCOME'                        
         BAS   R6,PRT                                                           
         TM    ACMPSTAT,X'04'                                                   
         BNO   *+14                                                             
         MVC   P+10(21),=C'ON GENERAL ACCOUNTING'                               
         BAS   R6,PRT                                                           
         MVC   P,SPACES                                                         
         BAS   R6,PRT                                                           
         B     ACEXT                                                            
         EJECT                                                                  
         SPACE 3                                                                
ACLUNT   CLC   OLDKEY+1(2),SPACES                                               
         BE    *+8                                                              
         BAS   R7,PCNTS                                                         
         USING ACKEYD,R4                                                        
         MVC   OLDKEY,ACKEYACC                                                  
         SPACE 1                                                                
         BAS   R6,PRT2                                                          
         MVC   P+1(7),=C'COMPANY'                                               
         GOTO1 VHEXOUT,DMCB,OLDKEY,P+9,1,=C'TOG'                                
         MVC   P+12(4),=C'UNIT'                                                 
         MVC   P+17(1),OLDKEY+1                                                 
         LA    R2,P+20                                                          
         BAS   R7,NAMOUT                                                        
         SPACE 1                                                                
         BAS   R6,PRT                                                           
         MVI   P+1,C'-'                                                         
         MVC   P+2(16),P+1                                                      
         BAS   R6,PRT2                                                          
         B     ACEXT                                                            
         EJECT                                                                  
         SPACE 3                                                                
ACLEDG   CLC   OLDKEY+2(1),SPACES                                               
         BE    *+8                                                              
         BAS   R7,PCNTS                                                         
         USING ACKEYD,R4                                                        
         MVC   OLDKEY,ACKEYACC                                                  
         SPACE 1                                                                
         MVC   P+1(6),=C'LEDGER'                                                
         MVC   P+8(1),OLDKEY+2                                                  
         LA    R2,P+11                                                          
         BAS   R7,NAMOUT                                                        
         SPACE 1                                                                
         LA    R2,1(R2)                                                         
         L     R4,AIOAREA                                                       
         LA    R4,4(R4)                                                         
         MVI   ELCODE,X'16'                                                     
         BAS   RE,GETEL                                                         
         BNE   ACEXT                                                            
         SPACE 1                                                                
         USING ACHEIRD,R4                                                       
         LA    R5,1                     GET LEVEL LENGTHS IN WORK               
         LA    R1,WORK                                                          
         LA    R3,ACHRLEVA                                                      
         XR    R6,R6                                                            
         XR    R7,R7                                                            
         LA    RE,4                                                             
         SPACE 1                                                                
ACLED1   IC    R6,0(R3)                                                         
         SR    R6,R7                                                            
         STC   R6,0(R1)                                                         
         AR    R7,R6                                                            
         CH    R7,=H'12'                                                        
         BE    ACLED2                                                           
         LA    R5,1(R5)                                                         
         LA    R1,1(R1)                                                         
         LA    R3,16(R3)                                                        
         BCT   RE,ACLED1                                                        
         SPACE 1                                                                
ACLED2   CH    R5,=H'1'                                                         
         BNE   *+14                                                             
         MVC   0(11,R2),=C'ONE LEVEL -'                                         
         LA    R2,12(R2)                                                        
         CH    R5,=H'2'                                                         
         BNE   *+14                                                             
         MVC   0(12,R2),=C'TWO LEVELS -'                                        
         LA    R2,13(R2)                                                        
         CH    R5,=H'3'                                                         
         BNE   *+14                                                             
         MVC   0(14,R2),=C'THREE LEVELS -'                                      
         LA    R2,15(R2)                                                        
         CH    R5,=H'4'                                                         
         BNE   *+14                                                             
         MVC   0(13,R2),=C'FOUR LEVELS -'                                       
         LA    R2,14(R2)                                                        
         SPACE 1                                                                
         LA    R3,ACHRDESA                                                      
         LA    R6,WORK                                                          
ACLED4   MVC   0(15,R2),0(R3)                                                   
         LA    R2,15(R2)                                                        
         CLI   0(R2),X'40'                                                      
         BNE   *+8                                                              
         BCT   R2,*-8                                                           
         LA    R2,1(R2)                                                         
         SPACE 1                                                                
         MVI   0(R2),C'('                                                       
         LA    R2,1(R2)                                                         
         XR    R7,R7                                                            
         IC    R7,0(R6)                                                         
         CVD   R7,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(3),DUB+6(2)                                                  
         MVC   0(1,R2),DUB+2                                                    
         LA    R2,1(R2)                                                         
         CLI   DUB+1,C'0'                                                       
         BE    ACLED6                                                           
         BCTR  R2,0                                                             
         MVC   0(2,R2),DUB+1                                                    
         LA    R2,2(R2)                                                         
         SPACE 1                                                                
ACLED6   MVC   0(2,R2),=C'),'                                                   
         LA    R2,3(R2)                                                         
         LA    R3,16(R3)                                                        
         LA    R6,1(R6)                                                         
         BCT   R5,ACLED4                                                        
         SH    R2,=H'2'                                                         
         MVI   0(R2),C' '                                                       
         BAS   R6,PRT2                                                          
         SPACE 1                                                                
         L     R4,AIOAREA                                                       
         LA    R4,4(R4)                                                         
         MVI   ELCODE,X'15'                                                     
         BAS   RE,GETEL                                                         
         BNE   ACEXT                                                            
         SPACE 1                                                                
         USING ACGENLD,R4                                                       
         MVC   P+10(24),=C'G/L POSTING INSTRUCTIONS'                            
ACLED7   LA    R2,P+37                                                          
ACLED7A  LR    R3,R2                                                            
         MVC   0(3,R2),=C'ALL'                                                  
         CLC   ACGLSUB,SPACES                                                   
         BE    *+10                                                             
         MVC   0(10,R2),ACGLSUB                                                 
         MVC   11(2,R2),=C'TO'                                                  
         MVC   14(10,R2),ACGLACC                                                
         SPACE 1                                                                
         GOTO1 =V(SQUASHER),DMCB,(R2),(0,25),RR=RB                              
         A     R3,DMCB+4                                                        
         LA    R6,P+92                                                          
         CR    R3,R6                                                            
         BNL   ACLED8                                                           
         BAS   RE,NEXTEL                                                        
         BNE   ACLEND                                                           
         MVI   0(R3),C','                                                       
         LA    R2,2(R3)                                                         
         B     ACLED7A                                                          
         SPACE 1                                                                
ACLED8   L     R6,DMCB+4                                                        
         BCTR  R6,0                                                             
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),SPACES                                                   
         BAS   R6,PRT                                                           
         B     ACLED7                                                           
         SPACE 1                                                                
ACLEND   BAS   R6,PRT2                                                          
         B     ACEXT                                                            
         EJECT                                                                  
PRT2     MVI   SPACING+3,C'2'                                                   
PRT      GOTO1 VPRINTER                                                         
         MVI   SPACING+3,C'1'                                                   
         BR    R6                                                               
         SPACE 2                                                                
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 1                                                                
         USING ACNAMED,R4                                                       
NAMOUT   MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNER  R7                                                               
         XR    R6,R6                                                            
         IC    R6,ACNMLEN                                                       
         SH    R6,=H'3'                                                         
         EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),ACNMNAME                                                 
         LA    R2,2(R6,R2)                                                      
         BR    R7                                                               
         SPACE 2                                                                
PCNTS    CP    CNTS,=P'0'                                                       
         BE    PCNT1                                                            
         EDIT  (P3,CNTS),(6,P+6)                                                
         MVC   P+13(8),=C'ACCOUNTS'                                             
         CP    CNTS,=P'1'                                                       
         BNE   PCNT1                                                            
         MVI   P+20,C' '                                                        
PCNT1    BAS   R6,PRT2                                                          
         ZAP   CNTS,=P'0'                                                       
         BR    R7                                                               
         EJECT                                                                  
OLDKEY   DS    CL3                                                              
CNTS     DS    PL3                                                              
DATADISP DC    H'49'                                                            
ELCODE   DS    CL1                                                              
         LTORG                                                                  
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACACCWORKD                                                     
       ++INCLUDE DDDPRINT                                                       
