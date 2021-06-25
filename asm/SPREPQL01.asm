*          DATA SET SPREPQL01  AT LEVEL 024 AS OF 08/29/00                      
*PHASE SPQL01A                                                                  
         TITLE 'SPQL01 - SPOT EXTRACT'                                          
SPQL01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         SPACE 1                                                                
         SPROG 1,2,3                                                            
         SSPEC H1,57,C'SPOT EXTRACT'                                            
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,57,C'------------'                                            
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,3,PAGE                                                        
         SSPEC H3,100,REPORT                                                    
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024SPREPQL01 08/29/00'                                      
         END                                                                    
