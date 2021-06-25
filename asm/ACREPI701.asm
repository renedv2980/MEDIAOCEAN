*          DATA SET ACREPI701  AT LEVEL 045 AS OF 08/17/00                      
*PHASE ACI701A                                                                  
ACI701   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
*                                                                               
         ACDEF SPROG,0,1,2,3,4,5,6,7,8,9,10                                     
         ACDEF H1,2,RUN                                                         
         ACDEF H1,93,REPORT                                                     
         ACDEF H1,109,PAGE                                                      
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,93,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H02,55,C'  GENERAL LEDGER FEED  '                                
         ACDEF H03,55,C'SUMMARY BY JOURNAL TYPE'                                
         ACDEF H04,55,C'-----------------------'                                
*                                                                               
         ACDEF SPROG,5                                                          
         ACDEF H02,55,C'    PRODUCTION FEED    '                                
         ACDEF H03,55,C'SUMMARY BY JOURNAL TYPE'                                
         ACDEF H04,55,C'-----------------------'                                
         ACDEF H08,29,C'COST'                                                   
         ACDEF H09,29,C'CODE'                                                   
         ACDEF H10,29,C'----'                                                   
         ACDEF H09,44,C'JOB NUM'                                                
         ACDEF H10,44,C'-------'                                                
*                                                                               
         ACDEF SPROG,8                                                          
         ACDEF H02,55,C'     PAYABLE FEED      '                                
         ACDEF H03,55,C'SUMMARY BY JOURNAL TYPE'                                
         ACDEF H04,55,C'-----------------------'                                
*                                                                               
         ACDEF SPROG,1,5,8                                                      
         ACDEF H01,55,CL24' O&&M  ACCPAK INTERFACE '                            
         ACDEF H09,2,C'J-TYPE'                                                  
         ACDEF H10,2,C'------'                                                  
         ACDEF H09,10,C'U/L'                                                    
         ACDEF H10,10,C'---'                                                    
         ACDEF H09,15,C'ACCOUNT'                                                
         ACDEF H10,15,C'-------'                                                
         ACDEF H09,35,C'OFFICE'                                                 
         ACDEF H10,35,C'------'                                                 
         ACDEF H09,65,C'DEBIT'                                                  
         ACDEF H10,65,C'-----'                                                  
         ACDEF H09,83,C'CREDIT'                                                 
         ACDEF H10,83,C'------'                                                 
*                                                                               
         ACDEF SPROG,2                                                          
         ACDEF H02,56,C'GENERAL LEDGER FEED'                                    
         ACDEF H03,56,C' SUMMARY BY OFFICE '                                    
         ACDEF H04,56,C'-------------------'                                    
         ACDEF SPROG,9                                                          
         ACDEF H02,56,C'   PAYABLES FEED   '                                    
         ACDEF H03,56,C' SUMMARY BY OFFICE '                                    
         ACDEF H04,56,C'-------------------'                                    
         ACDEF SPROG,2,9                                                        
         ACDEF H01,55,CL22'O&&M  ACCPAK INTERFACE'                              
         ACDEF H09,2,C'OFFICE'                                                  
         ACDEF H10,2,C'------'                                                  
         ACDEF H09,10,C'J-TYPE'                                                 
         ACDEF H10,10,C'------'                                                 
         ACDEF H09,18,C'ACCOUNT'                                                
         ACDEF H10,18,C'-------'                                                
         ACDEF H08,32,C'COST'                                                   
         ACDEF H09,32,C'CODE'                                                   
         ACDEF H10,32,C'----'                                                   
         ACDEF H09,50,C'DEBIT'                                                  
         ACDEF H10,50,C'-----'                                                  
         ACDEF H09,68,C'CREDIT'                                                 
         ACDEF H10,68,C'------'                                                 
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H02,56,C'GENERAL LEDGER FEED'                                    
         ACDEF H03,56,C'SUMMARY BY ACCOUNT '                                    
         ACDEF H04,56,C'-------------------'                                    
         ACDEF SPROG,6                                                          
         ACDEF H02,56,C'  PRODUCTION FEED  '                                    
         ACDEF H03,56,C'SUMMARY BY ACCOUNT '                                    
         ACDEF H04,56,C'-------------------'                                    
         ACDEF SPROG,10                                                         
         ACDEF H02,56,C'   PAYABLES FEED   '                                    
         ACDEF H03,56,C'SUMMARY BY ACCOUNT '                                    
         ACDEF H04,56,C'-------------------'                                    
         ACDEF SPROG,3,6,10                                                     
         ACDEF H01,56,CL21'O&&M ACCPAK INTERFACE'                               
         ACDEF H09,2,C'U/L'                                                     
         ACDEF H10,2,C'---'                                                     
         ACDEF H09,7,C'ACCOUNT'                                                 
         ACDEF H10,7,C'-------'                                                 
         ACDEF H09,21,C'ACCOUNT NAME'                                           
         ACDEF H10,21,C'------------'                                           
         ACDEF H09,67,C'DEBIT'                                                  
         ACDEF H10,67,C'-----'                                                  
         ACDEF H09,85,C'CREDIT'                                                 
         ACDEF H10,85,C'------'                                                 
*                                                                               
         ACDEF SPROG,4                                                          
         ACDEF H01,56,CL21'O&&M ACCPAK INTERFACE'                               
         ACDEF H02,57,C' PRODUCTION FEED '                                      
         ACDEF H03,57,C'SUMMARY BY OFFICE'                                      
         ACDEF H04,57,C'-----------------'                                      
         ACDEF H09,2,C'OFFICE'                                                  
         ACDEF H10,2,C'------'                                                  
         ACDEF H09,10,C'J-TYPE'                                                 
         ACDEF H10,10,C'------'                                                 
         ACDEF H09,18,C'U/L'                                                    
         ACDEF H10,18,C'---'                                                    
         ACDEF H09,23,C'ACCOUNT'                                                
         ACDEF H10,23,C'-------'                                                
         ACDEF H09,37,C'DEPT'                                                   
         ACDEF H10,37,C'----'                                                   
         ACDEF H08,48,C'COST'                                                   
         ACDEF H09,48,C'CODE'                                                   
         ACDEF H10,48,C'----'                                                   
         ACDEF H09,79,C'AMOUNT'                                                 
         ACDEF H10,79,C'------'                                                 
*                                                                               
         ACDEF SPROG,7                                                          
         ACDEF H01,57,C'UNIT/LEDGER SUMMARY'                                    
         ACDEF H02,57,C'-------------------'                                    
         ACDEF H09,2,C'U/L'                                                     
         ACDEF H10,2,C'---'                                                     
         ACDEF H09,27,C'DEBIT'                                                  
         ACDEF H10,27,C'-----'                                                  
         ACDEF H09,45,C'CREDIT'                                                 
         ACDEF H10,45,C'------'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'045ACREPI701 08/17/00'                                      
         END                                                                    
