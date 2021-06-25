*          DATA SET ACREPZB01  AT LEVEL 042 AS OF 12/07/99                      
*PHASE ACZB01A,*                                                                
         TITLE 'ACZB01 - CHECK PEELED TRANSACTIONS'                             
ACZB01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANS                                                       
         ACDEF WIDTH,198                                                        
*                                                                               
         SPROG 0,1                                                              
         ACDEF H1,2,RUN                                                         
         ACDEF H1,88,REPORT                                                     
         ACDEF H1,102,PAGE                                                      
         ACDEF H3,88,REQUESTOR                                                  
         ACDEF H3,2,ORIGIN                                                      
*                                                                               
         SPROG 1                                                                
         ACDEF H1,45,C'CHECK PEELED TRANSACTIONS'                               
         ACDEF H2,45,C'-------------------------'                               
         ACDEF H8,02,C'ACCOUNT'                                                 
         ACDEF H9,02,C'-------'                                                 
         ACDEF H8,17,C'OFF'                                                     
         ACDEF H9,17,C'---'                                                     
         ACDEF H8,21,C'CONTRA'                                                  
         ACDEF H9,21,C'------'                                                  
         ACDEF H8,41,C'MOA'                                                     
         ACDEF H9,41,C'---'                                                     
         ACDEF H8,58,C'DEBIT'                                                   
         ACDEF H9,58,C'-----'                                                   
         ACDEF H8,83,C'CREDIT'                                                  
         ACDEF H9,83,C'------'                                                  
         ACDEF H8,108,C'TEXT'                                                   
         ACDEF H9,108,C'----'                                                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042ACREPZB01 12/07/99'                                      
         END                                                                    
