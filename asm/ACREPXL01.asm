*          DATA SET ACREPXL01  AT LEVEL 012 AS OF 08/16/00                      
*PHASE ACXL01A,+0                                                               
         TITLE 'PERSON RECORD FIX'                                              
ACX801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC UPDATE,ACCFILE                                                   
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,38,C'PERSON RECORD FIX'                                       
         ACDEF H2,38,C'-----------------'                                       
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,83,REQUESTOR                                                  
         ACDEF H9,1,C'PERSON'                                                   
         ACDEF H10,1,C'------'                                                  
         ACDEF H9,12,C'OF'                                                      
         ACDEF H10,12,C'--'                                                     
         ACDEF H9,17,C'DEPT'                                                    
         ACDEF H10,17,C'----'                                                   
         ACDEF H9,26,C'SUB'                                                     
         ACDEF H10,26,C'---'                                                    
         ACDEF H9,35,C'TERM/SALLOCK DATE'                                       
         ACDEF H10,35,C'----'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012ACREPXL01 08/16/00'                                      
         END                                                                    
