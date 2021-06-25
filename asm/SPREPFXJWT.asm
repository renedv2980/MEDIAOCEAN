*          DATA SET SPREPFXJWT AT LEVEL 009 AS OF 07/28/97                      
*PHASE SPFX02B                                                                  
         TITLE 'SPFX02 - FIND JWT BAD BUYS'                                     
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
*                                                                               
*                                                                               
FX10     DS    0H                                                               
         XC    KEY,KEY                                                          
         L     R6,ADCLT                                                         
         MVC   KEY(3),1(R6)        MOVE A-M/CLT                                 
         MVI   KEY+3,X'FF'                                                      
         GOTO1 HIGH                                                             
         B     FX14                                                             
*                                                                               
FX12     GOTO1 SEQ                                                              
*                                                                               
FX14     CLC   KEY(4),KEYSAVE                                                   
         BNE   FX100                                                            
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         LA    R7,BDELEM                                                        
*                                                                               
FX20     SR    R0,R0                                                            
         ICM   R0,1,1(R7)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R7,R0                                                            
         CLI   0(R7),0                                                          
         BE    FX12                                                             
         CLI   0(R7),X'0B'                                                      
         BL    FX20                                                             
         CLI   0(R7),X'0D'                                                      
         BH    FX20                                                             
*                                                                               
         CLI   2(R7),X'C5'         TEST PAST 98                                 
         BL    FX20                                                             
*                                                                               
         BAS   RE,PRTBUY                                                        
         B     FX12                                                             
*                                                                               
FX100    GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
MYTRACE  NTR1                                                                   
         L     R6,ADBUY                                                         
         SR    R0,R0                                                            
         ICM   R0,3,13(R6)                                                      
         GOTO1 PRNTBL,DMCB,=C'BUYREC',(R6),C'DUMP',(R0),=C'1D00'                
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
* S/R TO PRINT BUY RECORDS                                                      
*                                                                               
PRTBUY   NTR1                                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R5,P+2                                                           
         USING PLINED,R5                                                        
         MVC   BYTE,BUYKAM                                                      
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
PB10     MVC   PAGY(2),BUYALPHA                                                 
         GOTO1 CLUNPK,DMCB,BUYKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,BUYMSTA,PMKT,WORK                                    
         MVC   PSTA(5),WORK                                                     
*                                                                               
         ZIC   R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   PEST+3,C'-'                                                      
*                                                                               
         ZIC   R0,BUYKBUY                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PLIN,WORK+7                                                      
*                                                                               
         GOTO1 HEXOUT,DMCB,KEY+14,PLDA,4,=C'N'                                  
*                                                                               
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
*                                                                               
         B     EXIT                                                             
*                                                                               
BADEL    DS    F                                                                
         LTORG                                                                  
         DC    X'FFFF'                                                          
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
PSTA     DS    CL7                                                              
         DS    CL2                                                              
PPRD     DS    CL3                                                              
         DS    CL2                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PLIN     DS    CL3                                                              
         DS    CL2                                                              
PLDA     DS    CL8                                                              
         DS    CL1                                                              
PLELDSP  DS    CL10                                                             
         EJECT                                                                  
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPFXJWT07/28/97'                                      
         END                                                                    
