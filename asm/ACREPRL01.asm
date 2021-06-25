*          DATA SET ACREPRL01  AT LEVEL 002 AS OF 10/21/15                      
*PHASE ACRL01A,+0                                                               
         TITLE 'REPORT GENERATOR FOR SCRIBE'                                    
ACRL01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF WIDTH,198                                                        
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF SPROG,0                                                          
         ACDEF SPROG,1                                                          
         ACDEF H1,2,RUN                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPRL01 10/21/15'                                      
         END                                                                    
