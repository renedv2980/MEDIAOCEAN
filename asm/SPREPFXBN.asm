*          DATA SET SPREPFXBN  AT LEVEL 010 AS OF 01/22/99                      
*PHASE SPFX02P                                                                  
         TITLE 'SPFX02 - FIX BAD GOAL RECORDS '                                 
SPFX02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPFX02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RC,RR=R2                                                
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
*                                                                               
         EJECT                                                                  
*                                                                               
FX10     XC    KEY,KEY                                                          
         MVI   KEY,2                                                            
         L     R6,ADCLT                                                         
         MVC   KEY+1(3),1(R6)      A-M/CLT                                      
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(4),KEYSAVE                                                   
         BNE   FX100                                                            
         CLI   KEY+7,35            TEST RIGHT ESTIMATE                          
         BNE   FX12                                                             
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         USING GOALRECD,R6                                                      
*                                                                               
FX20     XC    DELETES,DELETES     CLEAR DELETED DATE LIST                      
         LA    R5,DELETES                                                       
         MVI   ELCDLO,X'21'                                                     
         MVI   ELCDHI,X'21'                                                     
         LA    R6,GDELEM                                                        
*                                                                               
FX22     BAS   RE,NEXTEL                                                        
         BNE   FX30                                                             
*                                                                               
FX24     DS    0H                                                               
         GOTO1 DATCON,DMCB,(2,2(R6)),WORK                                       
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
*                                                                               
         CLI   0(R1),7             TEST SUNDAY                                  
         BNE   FX22                NO                                           
*                                                                               
FX26     BAS   RE,NEXTEL                                                        
         BNE   FX30                                                             
*                                                                               
         GOTO1 DATCON,DMCB,(2,2(R6)),WORK                                       
         GOTO1 GETDAY,DMCB,WORK,WORK+6                                          
*                                                                               
         CLI   0(R1),7             TEST ANOTHER SUNDAY                          
         BE    FX26                YES -TRY AGAIN                               
*                                                                               
         CLI   0(R1),1             TEST MONDAY                                  
         BNE   FX22                                                             
*                                                                               
         MVC   0(2,R5),2(R6)       ADD DATE TO LIST                             
         LA    R5,2(R5)                                                         
         GOTO1 RECUP,DMCB,ADBUY,(R6)                                            
         BAS   RE,NEXTEL2                                                       
         BE    FX24                                                             
*                                                                               
FX30     OC    DELETES,DELETES                                                  
         BZ    FX12                                                             
         BAS   RE,PRTBUY                                                        
         GOTO1 PUTBUY                                                           
         B     FX12                                                             
*                                                                               
FX100    DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
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
*                                                                               
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING GOALRECD,R6                                                      
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   BYTE,GKEYAM                                                      
         NI    BYTE,X'0F'                                                       
         MVI   PMED,C'T'                                                        
         CLI   BYTE,X'01'                                                       
         BE    PB10                                                             
         MVI   PMED,C'R'                                                        
         CLI   BYTE,X'02'                                                       
         BE    PB10                                                             
         MVI   PMED,C'N'                                                        
         CLI   BYTE,X'03'                                                       
         BE    PB10                                                             
         MVI   PMED,C'C'                                                        
         CLI   BYTE,X'08'                                                       
         BE    PB10                                                             
         MVI   PMED,C' '                                                        
*                                                                               
PB10     DS    0H                                                               
         MVC   PAGY(2),GAGYALPH                                                 
         GOTO1 CLUNPK,DMCB,GKEYCLT,PCLT                                         
*                                                                               
         L     R1,ADCLT                                                         
         LA    R1,CLIST-CLTHDR(R1)                                              
*                                                                               
PB12     CLC   GKEYPRD,3(R1)                                                    
         BE    PB14                                                             
         LA    R1,4(R1)                                                         
         CLI   0(R1),C' '                                                       
         BH    PB12                                                             
         LA    R1,=C'***'                                                       
*                                                                               
PB14     MVC   PPRD,0(R1)                                                       
*                                                                               
         SR    R0,R0                                                            
         ICM   R0,3,GKEYMKT                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PMKT,DUB                                                         
*                                                                               
         ZIC   R0,GKEYEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PEST,DUB                                                         
*                                                                               
         MVC   PDPT,GKEYDPT                                                     
*                                                                               
         SR    R0,R0                                                            
         IC    R0,GKEYSLN                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSLN,DUB                                                         
*                                                                               
         LA    R4,DELETES                                                       
         LA    R5,PDATES                                                        
*                                                                               
PB20     GOTO1 DATCON,DMCB,(2,(R4)),(4,(R5))                                    
         LA    R4,2(R4)                                                         
         LA    R5,6(R5)                                                         
         CLI   0(R4),0                                                          
         BNE   PB20                                                             
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
DELETES  DS    XL256                                                            
ELCDLO   DS    X                                                                
ELCDHI   DS    X                                                                
         DC    X'00'                                                            
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
PLINED   DSECT                                                                  
PMED     DS    CL1                                                              
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL2                                                              
PCLT     DS    CL3                                                              
         DS    CL2                                                              
PMKT     DS    CL4                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PDPT     DS    CL1                                                              
PSLN     DS    CL2                                                              
         DS    CL2                                                              
PDATES   DS    CL8                                                              
         DS    CL1                                                              
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPFXBN 01/22/99'                                      
         END                                                                    
