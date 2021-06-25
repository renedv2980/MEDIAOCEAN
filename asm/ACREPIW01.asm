*          DATA SET ACREPIW01  AT LEVEL 002 AS OF 08/17/00                      
*PHASE ACIW01A                                                                  
         TITLE 'SPECS FOR WPP - VALUTECH TAPES'                                 
ACIW01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF READ,TRANSACTIONS                                                
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,98,REPORT                                                     
         ACDEF H1,114,PAGE                                                      
         ACDEF H4,2,COMPANY                                                     
         ACDEF H4,98,REQUESTOR                                                  
         ACDEF H5,2,UNIT                                                        
         ACDEF H6,2,LEDGER                                                      
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,48,C'PAID INVOICE DETAIL'                                     
         ACDEF H2,48,C'-------------------'                                     
         ACDEF H10,2,C'VENDOR CODE   '                                          
         ACDEF H11,2,C'--------------'                                          
         ACDEF H10,19,C'INVOICE NUMBER'                                         
         ACDEF H11,19,C'--------------'                                         
         ACDEF H10,37,C'       GROSS   '                                        
         ACDEF H11,37,C'    ---------- '                                        
         ACDEF H10,53,C'     DISCOUNT  '                                        
         ACDEF H11,53,C'    ---------- '                                        
         ACDEF H10,69,C'     NET PAID  '                                        
         ACDEF H11,69,C'    ---------- '                                        
         ACDEF H10,86,C'INVOICE'                                                
         ACDEF H11,86,C' DATE  '                                                
         ACDEF H10,95,C' PAID  '                                                
         ACDEF H11,95,C' DATE  '                                                
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,48,C'VENDOR FILE'                                             
         ACDEF H2,48,C'-----------'                                             
         ACDEF H10,2,C'VENDOR CODE   '                                          
         ACDEF H11,2,C'--------------'                                          
         ACDEF H10,19,C'VENDOR NAME'                                            
         ACDEF H11,19,32C'-'                                                    
         ACDEF H10,52,C'    PURCHASES  '                                        
         ACDEF H11,52,C'    TOTAL 1990 '                                        
         ACDEF H10,68,C'DISCNT'                                                 
         ACDEF H11,68,C'   %  '                                                 
         ACDEF H10,77,C'TYPE'                                                   
         ACDEF H11,77,C'----'                                                   
         ACDEF H10,82,C'DISC'                                                   
         ACDEF H11,82,C'DAYS'                                                   
         ACDEF H10,88,C'NET '                                                   
         ACDEF H11,88,C'TYPE'                                                   
         ACDEF H10,93,C'NET '                                                   
         ACDEF H11,93,C'DAYS'                                                   
         ACDEF H10,98,C'  TAX ID  '                                             
         ACDEF H11,98,C'----------'                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPIW01 08/17/00'                                      
         END                                                                    
