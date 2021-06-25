*          DATA SET ACREPIS01  AT LEVEL 057 AS OF 08/17/00                      
*PHASE ACIS01A                                                                  
         TITLE 'INTEREP INTERFACE CHECKS'                                       
ACIS01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF RESET                                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,80,C'INTEREP INTERFACE'                                       
         ACDEF H2,80,C'-----------------'                                       
         ACDEF H1,2,RUN                                                         
         ACDEF H1,135,REPORT                                                    
         ACDEF H1,150,PAGE                                                      
         ACDEF H2,2,COMPANY                                                     
         ACDEF H2,135,REQUESTOR                                                 
         ACDEF H3,135,MOSFILT                                                   
*                                                                               
         ACDEF H6,3,C'NETWORK'                                                  
         ACDEF H7,3,C'CONTRACT'                                                 
         ACDEF H8,3,C'NUMBER'                                                   
*                                                                               
         ACDEF H7,13,C'CLIENT/'                                                 
         ACDEF H8,13,C'PRODUCT'                                                 
*                                                                               
         ACDEF H6,22,C'STATION'                                                 
         ACDEF H7,22,C'CALL'                                                    
         ACDEF H8,22,C'LETTERS'                                                 
*                                                                               
         ACDEF H6,31,C'STATION'                                                 
         ACDEF H7,31,C'INTERFACE'                                               
         ACDEF H8,31,C'CODE'                                                    
*                                                                               
         ACDEF H7,42,C'INVOICE '                                                
         ACDEF H8,42,C'NUMBER '                                                 
*                                                                               
         ACDEF H7,55,C'B/CAST'                                                  
         ACDEF H8,55,C'MTH/YR'                                                  
*                                                                               
         ACDEF H7,70,C'NET'                                                     
         ACDEF H8,68,C'AMOUNT'                                                  
*                                                                               
         ACDEF H8,82,C'COMMENTS'                                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'057ACREPIS01 08/17/00'                                      
         END                                                                    
