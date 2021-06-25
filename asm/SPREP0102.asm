*          DATA SET SPREP0102  AT LEVEL 017 AS OF 08/29/00                      
*PHASE SP0102A                                                                  
         TITLE 'SP0102 - TEST SPFILCON ESTBILL ROUTINE'                         
SP0102   CSECT                                                                  
         DS    8000C                                                            
         ORG   SP0102                                                           
         NMOD1 0,**SP01**                                                       
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         A     R9,=F'4096'                                                      
         USING SPWORKD,RA,R9                                                    
         CLI   MODE,CLTFRST                                                     
         BE    PROC                                                             
         CLI   MODE,OFCFRST                                                     
         BE    PROC                                                             
         CLI   MODE,CLTLAST                                                     
         BE    PROC                                                             
         CLI   MODE,OFCLAST                                                     
         BE    PROC                                                             
EXIT     XIT1                                                                   
*                                                                               
PROC     LA    R2,P                                                             
         USING LINED,R2                                                         
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         MVC   P(1),COFFICE                                                     
         GOTO1 CLUNPK,DMCB,CKEYCLT,P+3                                          
         MVC   P+6(5),=C'MODE='                                                 
         ZIC   R0,MODE                                                          
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  P+11(3),DUB                                                      
         GOTO1 REPORT                                                           
         B     EXIT                                                             
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
LINED    DSECT                                                                  
*                                                                               
         DS    CL3                                                              
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
         DS    CL1                                                              
LLIN     DS    CL3                                                              
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPGENEST                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREP0102 08/29/00'                                      
         END                                                                    
