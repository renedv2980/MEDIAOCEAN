*          DATA SET ACREPXQT01 AT LEVEL 001 AS OF 10/17/00                      
*PHASE ACQT01A                                                                  
ACQT01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF NOSUM                                                            
         ACDEF H1,2,RUN                                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
                                                                                
         ACDEF H1,39,C'CHANGE LIVE TO DRAFT'                                    
         ACDEF H2,39,C'--------------------'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPXQT0110/17/00'                                      
         END                                                                    
