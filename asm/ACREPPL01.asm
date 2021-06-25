*          DATA SET ACREPPL01  AT LEVEL 001 AS OF 09/16/04                      
*PHASE ACPL01A                                                                  
         TITLE 'REPORT GENERATOR FOR SCRIBE PAYABLES INTERCEPT'                 
ACPL01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF WIDTH,198                                                        
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF SPROG,0                                                          
         ACDEF SPROG,1                                                          
         ACDEF H1,2,RUN                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPPL01 09/16/04'                                      
         END                                                                    
