*          DATA SET ACREPRA01  AT LEVEL 022 AS OF 04/30/03                      
*PHASE ACRA01A,+0                                                               
         TITLE 'INTERNAL TRANSACTION AGING REPORT'                              
ACRA01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF MAXLINES,56                                                      
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,45,C'INTERNAL TRANSACTION AGING REPORT'                       
         ACDEF H1,100,REPORT                                                    
         ACDEF H1,115,PAGE                                                      
         ACDEF H2,100,REQUESTOR                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACREPRA01 04/30/03'                                      
         END                                                                    
