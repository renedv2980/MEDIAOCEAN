*          DATA SET SPBXREST   AT LEVEL 002 AS OF 01/24/95                      
*PHASE SP0102X,+0                                                               
         TITLE 'SPBXREST - RESTORE BOX ADDRESSES'                               
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
         CLC   TPREC(5),=C'ATBOX'                                               
         BH    EOT                                                              
         BL    GETTAP                                                           
*                                                                               
         MVC   P(17),TPREC                                                      
         GOTO1 REPORT                                                           
         AP    COUNT,=P'1'                                                      
*                                                                               
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
**PAN#1  DC    CL21'002SPBXREST  01/24/95'                                      
         END                                                                    
