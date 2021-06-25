*          DATA SET ACREP7301  AT LEVEL 013 AS OF 11/03/92                      
*PHASE AC7301A,+0                                                               
         TITLE 'SPECS FOR ACCOUNT LISTING'                                      
AC7301   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF MODE,PROCLEV                                                     
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,49,C'ACCOUNT LISTING'                                         
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H2,49,15C'-'                                                     
         ACDEF H4,86,REQUESTOR                                                  
         ACDEF H4,2,C'COMPANY'                                                  
         ACDEF H4,86,REQUESTOR                                                  
         ACDEF H5,2,C'UNIT'                                                     
         ACDEF H6,2,C'LEDGER'                                                   
         ACDEF H8,2,C'--------ACCOUNT--------'                                  
         ACDEF H9,2,C'CODE         FILT  NAME'                                  
         ACDEF SPROG,0                                                          
         ACDEF H8,57,C'--------ACCOUNT--------'                                 
         ACDEF H9,57,C'CODE         FILT  NAME'                                 
         ACDEF SPROG,1                                                          
         ACDEF H8,58,C'PROFILE INFORMATION'                                     
         ACDEF H9,58,19C'-'                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREP7301 11/03/92'                                      
         END                                                                    
