*          DATA SET ACREPID01  AT LEVEL 006 AS OF 08/17/00                      
*PHASE ACID01A                                                                  
         TITLE 'MCKIM INTERFACE TAPE/REPORT'                                    
ACID01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF MAXLINES,56                                                      
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN,WIDE=198                                                
         ACDEF H1,140,PAGE,WIDE=198                                             
         ACDEF H3,2,C'REPORT ACID',WIDE=198                                     
         ACDEF H3,140,REQUESTOR,WIDE=198                                        
         ACDEF H5,140,PERIOD,WIDE=198                                           
         ACDEF H1,65,C'W.B. DONER INTERFACE',WIDE=198                           
         ACDEF H2,65,20C'_',WIDE=198                                            
*                                                                               
*        COLUMN HEADINGS                                                        
*                                                                               
         ACDEF H8,3,C'CHECK',WIDE=198                                           
         ACDEF H9,3,C'NUMBER',WIDE=198                                          
         ACDEF H8,12,C'CHECK',WIDE=198                                          
         ACDEF H9,12,C'DATE',WIDE=198                                           
         ACDEF H9,20,C'CMP',WIDE=198                                            
         ACDEF H9,25,C'OF',WIDE=198                                             
         ACDEF H9,30,C'ACCT',WIDE=198                                           
         ACDEF H9,42,C'DESCRIPTION',WIDE=198                                    
         ACDEF H8,63,C'BANK',WIDE=198                                           
         ACDEF H9,63,C'CODE',WIDE=198                                           
         ACDEF H8,68,C'CLASS',WIDE=198                                          
         ACDEF H9,68,C'  #  ',WIDE=198                                          
         ACDEF H9,74,C'CLIENT',WIDE=198                                         
         ACDEF H9,84,C'PAYEE $',WIDE=198                                        
         ACDEF H9,98,C'GST $',WIDE=198                                          
         ACDEF H9,111,C'QST $',WIDE=198                                         
         ACDEF H9,123,C'HST $',WIDE=198                                         
         ACDEF H9,136,C'CD $',WIDE=198                                          
         ACDEF H9,149,C'CASH $',WIDE=198                                        
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREPID01 08/17/00'                                      
         END                                                                    
