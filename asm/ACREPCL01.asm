*          DATA SET ACREPCL01  AT LEVEL 073 AS OF 08/16/00                      
*PHASE ACCL01A,+0                                                               
         TITLE 'BATCH REFERNECE SORT'                                           
ACCL01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,72,C'CLIENT INVESTMENTS'                                      
         ACDEF H1,128,REPORT                                                    
         ACDEF H2,2,COMPANY                                                     
         ACDEF H2,128,PAGE                                                      
         ACDEF H3,2,C'LOCATION: '                                               
         ACDEF H3,128,C'POSTINGS THROUGH: '                                     
         ACDEF H4,2,C'DEPARTMENT: '                                             
         ACDEF H4,128,REQUESTOR                                                 
*                                                                               
         ACDEF H7,3,C'CLIENT'                                                   
*                                                                               
         ACDEF H7,41,C'CODE'                                                    
*                                                                               
         ACDEF H6,54,C'AR     '                                                 
         ACDEF H7,51,C'BALANCE'                                                 
*                                                                               
         ACDEF H6,70,C'WIP    '                                                 
         ACDEF H7,68,C'BALANCE'                                                 
*                                                                               
         ACDEF H6,87,C'TOTAL  '                                                 
         ACDEF H7,84,C'OUTSTANDING'                                             
*                                                                               
         ACDEF H6,103,C'CREDIT'                                                 
         ACDEF H7,103,C'LIMIT'                                                  
*                                                                               
         ACDEF H7,120,C'BUDGET'                                                 
*                                                                               
         ACDEF H7,133,C'CONTRACT'                                               
*                                                                               
         ACDEF H7,145,C'LETTER'                                                 
*                                                                               
         ACDEF H7,155,C'UPDATE'                                                 
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'073ACREPCL01 08/16/00'                                      
         END                                                                    
