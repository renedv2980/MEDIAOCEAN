*          DATA SET ACREPBU01  AT LEVEL 001 AS OF 09/25/02                      
*PHASE ACBU01A,*                                                                
ACBU01   TITLE '- SPECS FOR BUCKET UPDATER'                                     
ACBU01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF GETOPT,N                                                         
                                                                                
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,40,C'BUCKET UPDATER'                                          
         ACDEF H2,40,14X'BF'                                                    
         ACDEF H1,70,REPORT                                                     
         ACDEF H1,84,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H4,70,REQUESTOR                                                  
                                                                                
         SPROG 0                                                                
         ACDEF M2,2,C'ACCOUNT'                                                  
         ACDEF M1,17,C'O'                                                       
         ACDEF M2,17,C'F'                                                       
         ACDEF M2,20,C'CONTRA ACCOUNT'                                          
         ACDEF M1,35,C'B'                                                       
         ACDEF M2,35,C'T'                                                       
         ACDEF M2,37,C'MONTH '                                                  
         ACDEF M1,45,C' HIGH '                                                  
         ACDEF M2,45,C'MONTH '                                                  
         ACDEF M1,54,C'OLD BUCKET'                                              
         ACDEF M2,54,C'    DEBITS'                                              
         ACDEF M1,70,C'NEW BUCKET'                                              
         ACDEF M2,70,C'    DEBITS'                                              
         ACDEF M1,86,C'OLD BUCKET'                                              
         ACDEF M2,86,C'    CREDITS'                                             
         ACDEF M1,102,C'NEW BUCKET'                                             
         ACDEF M2,102,C'    CREDITS'                                            
         ACDEF M2,120,C'ACTION'                                                 
*                                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPBU01 09/25/02'                                      
         END                                                                    
