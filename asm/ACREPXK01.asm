*          DATA SET ACREPXK01  AT LEVEL 034 AS OF 03/07/14                      
*PHASE ACXK01A                                                                  
         TITLE 'PUT PIDELS ON TEMPO XREF RECORDS'                               
ACX401   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF READ,ACCOUNTS                                                    
*                                                                               
         ACDEF SPROG,0,1,2,3,4                                                  
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H8,2,C'COMPANY LOGO'                                             
         ACDEF H9,2,C'------------'                                             
         ACDEF H8,19,C'1R ACCOUNT'                                              
         ACDEF H9,19,C'----------'                                              
         ACDEF H8,33,C'NAME'                                                    
         ACDEF H9,33,C'----'                                                    
         ACDEF H8,71,C'ELEMENT COUNT'                                           
         ACDEF H9,71,C'-------------'                                           
         ACDEF H8,89,C'BRANDO START DATE'                                       
         ACDEF H9,89,C'-----------------'                                       
         ACDEF H8,109,C'BRANDO END DATE'                                        
         ACDEF H9,109,C'---------------'                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'034ACREPXK01 03/07/14'                                      
         END                                                                    
