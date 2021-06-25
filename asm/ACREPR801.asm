*          DATA SET ACREPR801  AT LEVEL 058 AS OF 08/16/00                      
*PHASE ACR801A,*                                                                
         TITLE 'SPECS FOR NEGATIVE ACCURAL REPORT'                              
ACR801   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
         ACDEF RESET                                                            
         ACDEF WIDTH,198                                                        
         ACDEF H1,2,RUN                                                         
         ACDEF H1,101,REPORT                                                    
         ACDEF H1,117,PAGE                                                      
         ACDEF H3,2,COMPANY                                                     
         ACDEF H3,101,MOSFILT                                                   
         ACDEF H4,2,C'OFFICE'                                                   
         SPACE 2                                                                
         SPROG 0                                                                
         ACDEF H1,54,C'AGEING UNBILLED TIME REPORT'                             
         ACDEF H2,54,C'---------------------------'                             
         SPACE 2                                                                
         SPROG 1                                                                
         ACDEF H1,54,C'OFFICE INCOME REPORT'                                    
         ACDEF H2,54,C'--------------------'                                    
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'058ACREPR801 08/16/00'                                      
         END                                                                    
