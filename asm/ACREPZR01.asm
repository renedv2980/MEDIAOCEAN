*          DATA SET ACREPZR01  AT LEVEL 005 AS OF 08/16/00                      
*PHASE ACZR01A                                                                  
ACZR01   TITLE '- READ RECOVERY FILE AND PRINT OUT RECORD TYPES'                
ACZR01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,REQUEST                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,81,REPORT                                                     
         ACDEF H1,104,PAGE                                                      
         ACDEF RESET                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPZR01 08/16/00'                                      
         END                                                                    
