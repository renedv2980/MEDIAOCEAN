*          DATA SET ACREPRU01  AT LEVEL 029 AS OF 08/16/00                      
*PHASE ACRU01A,+0                                                               
         PRINT NOGEN                                                            
         TITLE 'SPECS FOR AUTO RECEIVABLE UPLOAD'                               
ACRU01   CSECT                                                                  
*                                                                               
         ACDEF WIDTH,198                                                        
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,70,C'RECEIVABLE UPLOAD'                                       
         ACDEF H1,136,REPORT                                                    
         ACDEF H2,136,PAGE                                                      
         ACDEF H3,2,COMPANY                                                     
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H6,2,C'ITEM #'                                                   
         ACDEF H6,9,C'TYPE'                                                     
         ACDEF H6,14,C'ACCOUNT CODE'                                            
         ACDEF H6,32,C'ACCOUNT NAME'                                            
         ACDEF H5,77,C'INVOICE'                                                 
         ACDEF H6,77,C'NUMBER '                                                 
         ACDEF H6,87,C'DATE'                                                    
         ACDEF H6,101,C'DEBIT'                                                  
         ACDEF H6,119,C'CREDIT'                                                 
         ACDEF H6,141,C'MEMO'                                                   
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H6,5,C'RECORD TOTALS'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'029ACREPRU01 08/16/00'                                      
         END                                                                    
