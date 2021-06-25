*          DATA SET PPREPOM01  AT LEVEL 019 AS OF 08/09/00                      
*PHASE PPOM01A                                                                  
         TITLE 'PPOM01 - OGILVY PRINT CONVERSION SPECS'                         
PPOM01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,76,AGYADD                                                     
         PSPEC H1,2,REQUESTOR                                                   
         PSPEC H2,2,RUN                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019PPREPOM01 08/09/00'                                      
         END                                                                    
