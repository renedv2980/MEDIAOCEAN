*          DATA SET NENETTEST  AT LEVEL 007 AS OF 08/10/00                      
*PHASE NETTESTA                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE WRIPEVAL                                                               
*INCLUDE REGSAVE                                                                
NETTEST  CSECT                                                                  
         NBASE 0,NETTEST,=V(REGSAVE)                                            
         MVC   P(18),=C'TESTING PERIOD VAL'                                     
         GOTO1 =V(PRINT),DMCB,P-1,=C'BL01'                                      
         MVC   P(18),=C'------------------'                                     
         GOTO1 =V(PRINT),DMCB,P-1,=C'BL03'                                      
         MVC   P(18),=C'                 '                                      
         SPACE 1                                                                
LOOP     GOTO1 =V(CARDS),DMCB,P,=C'RE00'                                        
         CLC   P(2),=C'/*'                                                      
         BNE   LOOP2                                                            
         XBASE                                                                  
         SPACE 1                                                                
LOOP2    GOTO1 =V(WRIPEVAL),DMCB,P,BYTE                                         
         EDIT  (1,BYTE),(3,P+14)                                                
         GOTO1 =V(PRINT),DMCB,P-1,=C'BL01'                                      
         B     LOOP                                                             
DUB      DS    D                                                                
WORK     DS    CL32                                                             
DMCB     DS    CL32                                                             
BYTE     DS    XL1                                                              
         DS    CL1                                                              
P        DC    CL132' '                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007NENETTEST 08/10/00'                                      
         END                                                                    
