*          DATA SET ACREPR501  AT LEVEL 001 AS OF 10/17/95                      
*PHASE ACR501A,*                                                                
         TITLE 'SPECS FOR WEEKLY REVENUE SUMMARY'                               
ACR501   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,HISTORY                                                     
         FSPEC READ,TRANSACTIONS                                                
         ACDEF READ,TIME                                                        
         ACDEF RESET                                                            
*                                                                               
         ACDEF H1,57,C'WEEKLY FLASH SUMMARY'                                    
         ACDEF H2,57,20C'-'                                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,117,PAGE                                                      
         ACDEF H3,2,COMPANY                                                     
         ACDEF H8,25,C'WEEK 1'                                                  
         ACDEF H9,27,C'DAY'                                                     
         ACDEF H8,36,C'WEEK 2'                                                  
         ACDEF H9,38,C'DAYS'                                                    
         ACDEF H8,47,C'WEEK 3'                                                  
         ACDEF H9,49,C'DAYS'                                                    
         ACDEF H8,58,C'WEEK 4'                                                  
         ACDEF H9,60,C'DAYS'                                                    
         ACDEF H8,69,C'WEEK 5'                                                  
         ACDEF H9,71,C'DAY'                                                     
         ACDEF H8,80,C'TOTAL'                                                   
         ACDEF H8,90,C'RUN RATE'                                                
         ACDEF H8,101,C'BUDGET $'                                               
         ACDEF H9,100,C'DAY TO BUD'                                             
         ACDEF H8,113,C'TOTAL '                                                 
         ACDEF H9,113,C'R TIME'                                                 
         SPACE 1                                                                
         SPROG 1                                                                
         ACDEF H8,123,C'NEW BUS'                                                
         ACDEF H9,123,C'PRO BONO'                                               
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPR501 10/17/95'                                      
         END                                                                    
