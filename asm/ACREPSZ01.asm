*          DATA SET ACREPSZ01  AT LEVEL 038 AS OF 08/16/00                      
*PHASE ACSZ01A,+0                                                               
         TITLE 'SZ LEDGER BALANCE'                                              
ACSZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,070,C'SZ LEDGER BALANCE'                                      
         ACDEF H2,143,PAGE                                                      
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H5,003,C'SZ'                                                     
         ACDEF H6,003,C'ACCOUNT'                                                
         ACDEF H7,003,C'CODE'                                                   
         ACDEF H5,017,C'SZ'                                                     
         ACDEF H6,017,C'ACCOUNT'                                                
         ACDEF H7,017,C'NAME'                                                   
         ACDEF H5,044,C'SZ'                                                     
         ACDEF H6,044,C'CLI'                                                    
         ACDEF H7,044,C'CDE'                                                    
         ACDEF H6,049,C'SZ LEDGER'                                              
         ACDEF H7,049,C'CREDITS'                                                
         ACDEF H6,067,C'SZ LEDGER'                                              
         ACDEF H7,067,C'DEBITS'                                                 
         ACDEF H6,074,C'SZ LEDGER'                                              
         ACDEF H7,085,C'BALANCE (CR-DB)'                                        
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H5,003,C'SZ'                                                     
         ACDEF H6,003,C'CLI'                                                    
         ACDEF H7,003,C'CDE'                                                    
         ACDEF H5,007,C'SZ'                                                     
         ACDEF H6,007,C'ACCOUNT'                                                
         ACDEF H7,007,C'CODE'                                                   
         ACDEF H5,022,C'SZ'                                                     
         ACDEF H6,022,C'ACCOUNT'                                                
         ACDEF H7,022,C'NAME'                                                   
         ACDEF H6,054,C'SZ LEDGER'                                              
         ACDEF H7,054,C'CREDITS'                                                
         ACDEF H6,070,C'SZ LEDGER'                                              
         ACDEF H7,070,C'DEBITS'                                                 
         ACDEF H6,089,C'SZ LEDGER'                                              
         ACDEF H7,086,C'BALANCE (CR-DB)'                                        
         ACDEF H6,103,C'MEDIA'                                                  
         ACDEF H7,103,C'SYS/MED'                                                
         ACDEF H6,112,C'MEDIA'                                                  
         ACDEF H7,112,C'PAID'                                                   
         ACDEF H6,130,C'MEDIA'                                                  
         ACDEF H7,130,C'BILLED'                                                 
         ACDEF H6,149,C'MEDIA'                                                  
         ACDEF H7,148,C'BALANCE'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'038ACREPSZ01 08/16/00'                                      
         END                                                                    
