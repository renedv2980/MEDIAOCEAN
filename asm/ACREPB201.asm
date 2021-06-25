*          DATA SET ACREPB201  AT LEVEL 009 AS OF 03/28/89                      
*PHASE ACB201A,+0                                                               
         TITLE 'SPECS FOR BUDGET COMPARISON'                                    
         PRINT NOGEN                                                            
ACB201   CSECT                                                                  
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF MODE,PROCLEV                                                     
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
*&&UK                                                                           
         ACDEF H1,48,C'BUDGET COMPARISON'                                       
         ACDEF H2,48,17C'-'                                                     
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,100,PAGE                                                      
         ACDEF H4,86,REQUESTOR                                                  
*&&                                                                             
*&&US                                                                           
         ACDEF H1,59,C'BUDGET COMPARISON'                                       
         ACDEF H2,59,17C'-'                                                     
         ACDEF H1,108,REPORT                                                    
         ACDEF H1,122,PAGE                                                      
         ACDEF H4,108,REQUESTOR                                                 
*&&                                                                             
         ACDEF H4,2,COMPANY                                                     
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
         ACDEF H8,2,C'ACCOUNT'                                                  
         ACDEF H9,2,C'-------'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREPB201 03/28/89'                                      
         END                                                                    
