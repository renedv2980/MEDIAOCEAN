*          DATA SET SPI2REST   AT LEVEL 013 AS OF 07/09/96                      
*PHASE SP0102,+0                                                                
         TITLE 'SPI2REST - RESTORE INVOICE RECORDS'                             
SP0102   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SP01**                                                       
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         A     R9,=F'4096'                                                      
         USING SPWORKD,RA,R9                                                    
         CLI   MODE,REQFRST                                                     
         BE    PROCESS                                                          
         CLI   MODE,REQLAST                                                     
         BE    EXIT                                                             
         CLI   MODE,RUNFRST                                                     
         BE    EXIT                                                             
         CLI   MODE,RUNLAST                                                     
         BE    EXIT                                                             
         GOTO1 AENDREQ                                                          
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
PROCESS  DS    0H                                                               
*                                                                               
         OPEN  (SPOTIN,(INPUT))                                                 
         OPEN  (SPOTOUT,(OUTPUT))                                               
*                                                                               
GETTAP   LA    R0,TPREC-4                                                       
         LA    R1,SPOTIN                                                        
         GET   (1),(0)                                                          
*                                                                               
         CLI   TPREC,X'0B'         INVOICE RECS                                 
         BH    EOT                                                              
         BL    GETTAP                                                           
         MVC   BYTE,TPREC+1                                                     
         NI    BYTE,X'F0'                                                       
         CLI   BYTE,X'20'          WT                                           
         BE    GT03                                                             
         CLI   BYTE,X'30'          WR                                           
         BH    EOT                                                              
         BL    GETTAP                                                           
*                                                                               
GT03     DS    0H                                                               
         CLC   TPREC+7(2),=X'BCC1'   JUN94                                      
         BH    GETTAP                                                           
         TM    TPREC+15,X'C0'      TEST CLOSED OUT                              
         BNO   GETTAP                                                           
*                                                                               
         CLI   QOPT1,C'T'          TRACE?                                       
         BNE   GT04                                                             
*                                                                               
         GOTO1 HEXOUT,DMCB,TPREC,P,18,=C'N'                                     
         NI    TPREC+15,X'3F'                                                   
         GOTO1 HEXOUT,DMCB,TPREC,P+40,18,=C'N'                                  
         GOTO1 REPORT                                                           
*                                                                               
GT04     DS    0H                                                               
         AP    COUNT,=P'1'                                                      
         LA    R0,TPREC-4                                                       
         LA    R1,SPOTOUT                                                       
         PUT   (1),(0)                                                          
*                                                                               
         B     GETTAP                                                           
*                                                                               
EOT      DS    0H                                                               
         MVC   P(19),=C'COPIED RECORD COUNT'                                    
         EDIT  (P6,COUNT),(6,P+21),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
*                                                                               
         CLOSE SPOTIN                                                           
         CLOSE SPOTOUT                                                          
         B     EXIT                                                             
         SPACE 3                                                                
         SPACE 2                                                                
SPOTIN   DCB   DDNAME=SPOTIN,                                          X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4000,                                             X        
               MACRF=GM,                                               X        
               BUFNO=1,                                                X        
               EODAD=EOT                                                        
*                                                                               
SPOTOUT  DCB   DDNAME=SPOTOUT,                                         X        
               DSORG=PS,                                               X        
               RECFM=VB,                                               X        
               LRECL=4000,                                             X        
               MACRF=PM,                                               X        
               BUFNO=1                                                          
*                                                                               
         LTORG                                                                  
         DS    0D                                                               
COUNT    DC    PL6'0'                                                           
         DS    0D                                                               
         DS    F                                                                
TPREC    DC    4000X'00'                                                        
*                                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
         PRINT OFF                                                              
         EJECT                                                                  
       ++INCLUDE SPREPMODES                                                     
         EJECT                                                                  
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPI2REST  07/09/96'                                      
         END                                                                    
