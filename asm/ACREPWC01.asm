*          DATA SET ACREPWC01  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACWC01A,+0                                                               
         TITLE 'WORKING INVESTMENT #2 LINE OF BUSINESS RPT'                     
ACWC01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF WIDTH,198                                                        
         ACDEF RESET                                                            
         FSPEC READ,ACCOUNTS                                                    
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,070,C'WORKING INVESTMENT'                                     
         ACDEF H1,130,PAGE                                                      
         ACDEF H2,059,C'LINE OF BUSINESS - CLIENT GROUPING REPORT'              
*                                                                               
         ACDEF SPROG,1                                                          
         ACDEF H7,003,C'LOB'                                                    
         ACDEF H8,003,C'CODE'                                                   
         ACDEF H7,010,C'CLIENT'                                                 
         ACDEF H8,010,C'CODE'                                                   
         ACDEF H7,018,C'DIV'                                                    
         ACDEF H8,018,C'CODE'                                                   
         ACDEF H7,026,C'DIVISION'                                               
         ACDEF H8,026,C'NAME'                                                   
         ACDEF H7,063,C'CAT'                                                    
         ACDEF H8,063,C'CODE'                                                   
         ACDEF H7,072,C'CATEGORY'                                               
         ACDEF H8,072,C'NAME'                                                   
         ACDEF H7,109,C'RUN'                                                    
         ACDEF H8,109,C'DATE'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPWC01 08/16/00'                                      
         END                                                                    
