*          DATA SET ACREP2501  AT LEVEL 002 AS OF 01/14/97                      
*PHASE AC2501B                                                                  
AC2501   TITLE '- SPECS FOR GENERAL LEDGER UPDATE'                              
AC2501   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF GETOPT,N                                                         
         ACDEF SET,ACCTBAL                                                      
         ACDEF GENBUCK,OFFICE                                                   
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6,7,8,9                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H4,2,COMPANY                                                     
*                                                                               
         ACDEF SPROG,0,1,2,3,6,7,8                                              
         ACDEF H1,45,AC#GLGUP,34,L C'GENERAL LEDGER UPDATE'                     
         ACDEF H2,45,AC#GLGUP,34,LU  ---------------------                      
         ACDEF H8,72,AC#DRS,6,R    C'DEBITS'                                    
         ACDEF H9,72,AC#DRS,6,RU   C'------'                                    
         ACDEF H8,89,AC#CRS,7,R    C'CREDITS'                                   
         ACDEF H9,89,AC#CRS,7,RU   C'-------'                                   
*                                                                               
         ACDEF SPROG,0,1,2,6,7,8                                                
         ACDEF H4,83,REQUESTOR                                                  
*                                                                               
         ACDEF SPROG,0,1,2                                                      
         ACDEF H5,2,LEDGER                                                      
*                                                                               
         ACDEF SPROG,0,1,8                                                      
         ACDEF H8,42,AC#TOACC,10,L C'TO ACCOUNT'                                
         ACDEF H9,42,AC#TOACC,10,LU '----------'                                
         ACDEF H8,99,AC#SRC,8,R    C'SOURCE'                                    
         ACDEF H9,99,AC#SRC,8,RU   C'------'                                    
*                                                                               
         ACDEF SPROG,0,8                                                        
         ACDEF H8,2,AC#FROMA,12,L  C'FROM ACCOUNT'                              
         ACDEF H9,2,AC#FROMA,12,LU C'------------'                              
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H8,2,AC#MED,5,L     C'MEDIA'                                     
         ACDEF H9,2,AC#MED,5,LU    C'-----'                                     
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H6,2,AC#SMYFL,33,L  C'SUMMARY FOR LEDGER'                        
*                                                                               
         ACDEF SPROG,7                                                          
         ACDEF H6,2,33C' '                                                      
         ACDEF H6,2,AC#OVRSU,15,L  C'OVERALL SUMMARY   '                        
*                                                                               
         ACDEF SPROG,2,7                                                        
         ACDEF H8,2,AC#ACC,7,L     C'ACCOUNT'                                   
         ACDEF H9,2,AC#ACC,7,LU    C'-------'                                   
         ACDEF H8,42,AC#CTR,14,L   C'CONTRA-ACCOUNT'                            
         ACDEF H9,42,AC#CTR,14,LU  C'--------------'                            
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H6,2,C'DDS CONTROL TOTALS'                                       
*                                                                               
         ACDEF SPROG,4,5                                                        
         ACDEF H1,50,AC#TRLB,13,R  C'TRIAL BALANCE'                             
         ACDEF H2,50,AC#TRLB,13,RU C'-------------'                             
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H8,2,AC#ACC,12,L    C'ACCOUNT CODE'                              
         ACDEF H9,2,AC#ACC,12,LU   C'------------'                              
         ACDEF H8,18,AC#ACCN,17,L  C'ACCOUNT NAME'                              
         ACDEF H9,18,AC#ACCN,17,LU C'------------'                              
*                                                                               
         ACDEF SPROG,4,9                                                        
         ACDEF H8,57,AC#BAL,7,R    C'BALANCE'                                   
         ACDEF H9,57,AC#FRWD,7,R   C'FORWARD'                                   
         ACDEF H8,73,AC#DRS,6,R    C'DEBITS'                                    
         ACDEF H9,73,AC#DRS,6,RU   C'------'                                    
         ACDEF H8,87,AC#CRS,7,R    C'CREDITS'                                   
         ACDEF H9,87,AC#CRS,7,RU   C'-------'                                   
         ACDEF H8,102,AC#PRE,7,R   C'PRESENT'                                   
         ACDEF H9,102,AC#BAL,7,R   C'BALANCE'                                   
*                                                                               
         ACDEF SPROG,5,10                                                       
         ACDEF H8,73,C'-'                                                       
         ACDEF H8,82,AC#ACTY,9,R   C'ACTIVITY'                                  
         ACDEF H8,93,C'-'                                                       
         ACDEF H9,57,AC#BAL,7,R    C'BALANCE'                                   
         ACDEF H9,73,AC#DRS,6,R    C'DEBITS'                                    
         ACDEF H9,87,AC#CRS,7,R    C'CREDITS'                                   
         ACDEF H9,102,AC#BAL,7,R   C'BALANCE'                                   
*                                                                               
         ACDEF SPROG,9,10                                                       
         ACDEF H1,50,AC#TRLB,13,R  C'TRIAL BALANCE'                             
         ACDEF H2,50,AC#TRLB,13,RU C'-------------'                             
         ACDEF H4,50,C'OFFICE RECAP'                                            
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H8,2,C'OFFICE CODE     OFFICE NAME'                              
         ACDEF H9,2,C'-----------     -----------'                              
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREP2501 01/14/97'                                      
         END                                                                    
