*          DATA SET ACREPR101  AT LEVEL 013 AS OF 09/18/96                      
*PHASE ACR101A,+0                                                               
         TITLE 'REALIZATION REPORT  - CLIENTS'                                  
ACR101   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF READ,ACCOUNTS                                                    
         ACDEF READ,HISTORY                                                     
         ACDEF MODE,PROCLEV                                                     
         ACDEF PRORATA,BILL                                                     
         ACDEF WIDTH,198                                                        
         ACDEF MAXLINES,56                                                      
*                                                                               
         ACDEF SPROG,0,1,2,3,4                                                  
         ACDEF H1,2,RUN,WIDE=198                                                
         ACDEF H1,125,REPORT,WIDE=198                                           
         ACDEF H1,138,PAGE,WIDE=198                                             
         ACDEF H3,125,REQUESTOR,WIDE=198                                        
         ACDEF H4,125,MOSFILT                                                   
         ACDEF H5,125,PERIOD,WIDE=198                                           
         SPACE 1                                                                
         ACDEF H3,2,C'COMPANY',WIDE=198                                         
         ACDEF H1,69,C'CLIENT REALIZATION REPORT',WIDE=198                      
         ACDEF SPROG,4                                                          
         ACDEF H4,71,C'** CLIENT SUMMARY **',WIDE=198                           
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013ACREPR101 09/18/96'                                      
         END                                                                    
