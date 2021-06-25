*          DATA SET ACREPIA01  AT LEVEL 007 AS OF 08/17/00                      
*PHASE ACIA01A                                                                  
         TITLE 'MCKIM INTERFACE TAPE/REPORT'                                    
ACIA01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,TRANSACTIONS                                                
         ACDEF MAXLINES,56                                                      
*                                                                               
         ACDEF SPROG,0,1                                                        
         ACDEF H1,2,RUN,WIDE=198                                                
         ACDEF H1,122,PAGE,WIDE=198                                             
         ACDEF H3,122,C'REPORT ACIA',WIDE=198                                   
         ACDEF H4,122,REQUESTOR,WIDE=198                                        
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,52,C'MCKIM INTERFACE ERROR REPORT',WIDE=198                   
         ACDEF H2,52,31C'_',WIDE=198                                            
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,52,C'MCKIM INTERFACE TAPE/REPORT',WIDE=198                    
         ACDEF H2,52,30C'_',WIDE=198                                            
         ACDEF H4,2,C'UNIT:',WIDE=198                                           
*                                                                               
*        COLUMN HEADINGS - ERROR RPT                                            
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H8,11,C'ACCT',WIDE=198                                           
         ACDEF H9,11,C'----',WIDE=198                                           
         ACDEF H8,28,C'C/A',WIDE=198                                            
         ACDEF H9,28,C'---',WIDE=198                                            
         ACDEF H8,43,C'STAT',WIDE=198                                           
         ACDEF H9,43,C'----',WIDE=198                                           
         ACDEF H8,51,C'TYPE',WIDE=198                                           
         ACDEF H9,51,C'----',WIDE=198                                           
         ACDEF H8,59,C'BATCH',WIDE=198                                          
         ACDEF H9,59,C'-----',WIDE=198                                          
         ACDEF H8,71,C'DATE',WIDE=198                                           
         ACDEF H9,71,C'----',WIDE=198                                           
         ACDEF H8,84,C'REF',WIDE=198                                            
         ACDEF H9,84,C'---',WIDE=198                                            
         ACDEF H8,91,C'CODE',WIDE=198                                           
         ACDEF H9,91,C'----',WIDE=198                                           
         ACDEF H8,101,C'AMOUNT',WIDE=198                                        
         ACDEF H9,101,C'------',WIDE=198                                        
*                                                                               
*        COLUMN HEADINGS                                                        
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H9,3,C'JE#',WIDE=198                                             
         ACDEF H9,9,C' MOA  ',WIDE=198                                          
         ACDEF H8,18,C'  G/L  ',WIDE=198                                        
         ACDEF H9,18,C'ACCOUNT',WIDE=198                                        
         ACDEF H8,28,C'CLIENT',WIDE=198                                         
         ACDEF H9,28,C' CODE ',WIDE=198                                         
         ACDEF H8,38,C'TRANS.',WIDE=198                                         
         ACDEF H9,38,C' DATE ',WIDE=198                                         
         ACDEF H8,47,C'HDS BATCH',WIDE=198                                      
         ACDEF H9,47,C'  TYPE   ',WIDE=198                                      
         ACDEF H8,61,C'  HDS  ',WIDE=198                                        
         ACDEF H9,61,C'ACCOUNT',WIDE=198                                        
         ACDEF H8,84,C'TRANSACTION',WIDE=198                                    
         ACDEF H9,84,C'DESCRIPTION',WIDE=198                                    
         ACDEF H9,112,C'AMOUNT',WIDE=198                                        
         ACDEF H8,129,C' TAPE ',WIDE=198                                        
         ACDEF H9,129,C'AMOUNT',WIDE=198                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACREPIA01 08/17/00'                                      
         END                                                                    
