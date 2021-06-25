*          DATA SET ACREPXT01  AT LEVEL 003 AS OF 08/16/00                      
*PHASE ACXT01A                                                                  
ACXT01   TITLE '- READ RECOVERY FILE AND COMPARE TO REAL FILE'                  
ACXT01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,REQUEST                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,81,REPORT                                                     
         ACDEF H1,104,PAGE                                                      
         ACDEF RESET                                                            
         ACDEF UPDATE,ACCFIL                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPXT01 08/16/00'                                      
         END                                                                    
