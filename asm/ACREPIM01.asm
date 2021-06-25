*          DATA SET ACREPIM01  AT LEVEL 003 AS OF 08/17/00                      
*PHASE ACIM01A                                                                  
         TITLE 'ELECTRONIC VENDOR ESTIMATE INFORMATION'                         
ACIM01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         FSPEC READ,ACCOUNTS                                                    
*                                                                               
         ACDEF SPROG,1                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPIM01 08/17/00'                                      
         END                                                                    
