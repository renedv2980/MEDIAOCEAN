*          DATA SET ACREPAP01  AT LEVEL 022 AS OF 08/16/00                      
*PHASE ACAP01A,*                                                                
         TITLE 'ACAP01 - LIST APG SUPERLEDGER - SPECS'                          
ACAP01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         ASPEC H1,40,C'APG SUPERLEDGER LISTING'                                 
         ASPEC H2,40,C'-----------------------'                                 
         ASPEC H1,1,RUN                                                         
         ASPEC H1,81,REPORT                                                     
         ASPEC H1,97,PAGE                                                       
         ASPEC H3,81,REQUESTOR                                                  
         ASPEC H3,1,COMPANY                                                     
         ASPEC H4,1,UNIT                                                        
         ASPEC H5,1,LEDGER                                                      
         ASPEC H8,2,C'RECEIVING ACCOUNT'                                        
         ASPEC H8,46,C'GIVING ACCOUNT(S)'                                       
         ASPEC H7,75,C'FLTRS'                                                   
         ASPEC H8,75,C'12345'                                                   
         ASPEC H8,82,C'GIVING CONTRA-ACCOUNT(S)'                                
         ASPEC H8,39,C'ACTN'                                                    
         ASPEC H7,109,C'B'                                                      
         ASPEC H8,109,C'T'                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022ACREPAP01 08/16/00'                                      
         END                                                                    
