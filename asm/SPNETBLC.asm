*          DATA SET SPNETBLC   AT LEVEL 007 AS OF 03/06/95                      
*PHASE SP0102,+0                                                                
         TITLE 'SPNETBLC - NETPAK BILLING CHECK'                                
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
         LA    R2,TPREC                                                         
         USING NURECD,R2                                                        
*                                                                               
         CLC   NUKEY(4),=X'04139160'                                            
         BH    EOT                                                              
         BL    GETTAP                                                           
         CLI   NUKEST,10           EST 10                                       
         BNE   GETTAP                                                           
*                                                                               
         SR    R7,R7                                                            
         ICM   R7,3,NURLEN                                                      
         LA    R7,TPREC(R7)                                                     
         LA    R4,NUDATA                                                        
*                                                                               
MP04     DS    0H                                                               
         CR    R4,R7               TEST VS END                                  
         BNL   MP12                                                             
         CLI   0(R4),0                                                          
         BE    MP12                                                             
         CLI   0(R4),X'10'         BILLING ELEM                                 
         BE    GETTAP              SKIP IF BILLED                               
*                                                                               
MP06     DS    0H                                                               
         ZIC   R0,1(R4)                                                         
         LTR   R0,R0                                                            
         BZ    GETTAP                                                           
         AR    R4,R0                                                            
         B     MP04                                                             
*                                                                               
MP10     DS    0H                                                               
         USING NUBILD,R4                                                        
*                                                                               
         CLC   NUBILDAT,=X'BD95'   DEC21/94                                     
         BNE   MP06                                                             
         CLI   NUBILPRD,02         PRODUCT SP                                   
         BNE   MP06                                                             
         CLC   NUBILNUM,=C'0139'                                                
         BNE   MP06                                                             
*                                                                               
MP12     DS    0H                                                               
         CLC   NUACTUAL,=F'406000'                                              
         BNE   GETTAP                                                           
*                                                                               
         ICM   R5,15,NUACTUAL                                                   
         A     R5,COUNT                                                         
         ST    R5,COUNT                                                         
*                                                                               
         GOTO1 HEXOUT,DMCB,TPREC,P,7,=C'N'                                      
         GOTO1 DATCON,DMCB,(2,NUKDATE),(11,P+16)                                
         MVC   P+26(4),NUKNET                                                   
         MVC   P+31(6),NUKPROG                                                  
         GOTO1 HEXOUT,DMCB,NUKEST,P+40,3,=C'N'                                  
         GOTO1 HEXOUT,DMCB,NUUNITST,P+47,1,=C'N'                                
*                                                                               
         EDIT  (B4,NUACTUAL),(11,P+50),2,FLOAT=- ,ZERO=NOBLANK                  
*                                                                               
         GOTO1 REPORT                                                           
*                                                                               
         LA    R0,TPREC-4                                                       
         LA    R1,SPOTOUT                                                       
         PUT   (1),(0)                                                          
*                                                                               
         B     GETTAP                                                           
*                                                                               
EOT      DS    0H                                                               
         MVC   P(12),=C'TOTAL BILLED'                                           
         EDIT  (B4,COUNT),(6,P+21),ZERO=NOBLANK                                 
         GOTO1 REPORT                                                           
*                                                                               
         CLOSE SPOTIN                                                           
         CLOSE SPOTOUT                                                          
         B     EXIT                                                             
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
COUNT    DC    F'0'                                                             
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
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPNETBLC  03/06/95'                                      
         END                                                                    
