*          DATA SET ACREP8A01  AT LEVEL 005 AS OF 10/11/96                      
*PHASE AC8A01A,+0                                                               
         TITLE 'ANALYSIS OF INCOME SUSPENSE'                                    
AC8A01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
*                                                                               
         RSPEC MAXLINES,56                                                      
         SPROG 0,1                                                              
         ASPEC H1,2,RUN,WIDE=198                                                
         ASPEC H1,125,REPORT,WIDE=198                                           
         ASPEC H1,138,PAGE,WIDE=198                                             
         ASPEC H3,125,REQUESTOR,WIDE=198                                        
         ASPEC H5,125,PERIOD,WIDE=198                                           
         SPACE 1                                                                
         ASPEC H3,2,C'COMPANY',WIDE=198                                         
         ASPEC H1,68,C'ANALYSIS OF INCOME SUSPENSE',WIDE=198                    
         ASPEC H2,68,27C'_',WIDE=198                                            
         SPACE 1                                                                
         ASPEC H10,65,C'TRANS.',WIDE=198                                        
         ASPEC H11,66,C'DATE',WIDE=198                                          
         SPACE 1                                                                
         ASPEC H10,75,C'REF.',WIDE=198                                          
         ASPEC H11,74,C'NUMBER',WIDE=198                                        
         SPACE 1                                                                
         ASPEC H10,83,C'TRANSACTION',WIDE=198                                   
         ASPEC H11,85,C'AMOUNT',WIDE=198                                        
         SPACE 1                                                                
         ASPEC H10,98,C'UNBILLED',WIDE=198                                      
         ASPEC H11,99,C'AMOUNT',WIDE=198                                        
         SPACE 1                                                                
         ASPEC H10,115,C'SK',WIDE=198                                           
         ASPEC H11,113,C'AMOUNT',WIDE=198                                       
         SPACE 1                                                                
         ASPEC H10,125,C'DIFFERENCE',WIDE=198                                   
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H10,2,C'JOB NUMBER -- NAME',WIDE=198                             
         ASPEC H10,50,C'SK CONTRA',WIDE=198                                     
         ASPEC H11,51,C'ACCOUNT',WIDE=198                                       
         ASPEC H4,2,C'CLIENT',WIDE=198                                          
         ASPEC H5,2,C'PRODUCT',WIDE=198                                         
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H10,2,C'CLIENT/PROD/JOB -- NAME',WIDE=198                        
         ASPEC H4,2,C'TYPE',WIDE=198                                            
         ASPEC H5,2,C'INCOME',WIDE=198                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREP8A01 10/11/96'                                      
         END                                                                    
