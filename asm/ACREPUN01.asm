*          DATA SET ACREPUN01  AT LEVEL 005 AS OF 07/07/07                      
*PHASE ACUN01A                                                                  
         TITLE 'SPECS FOR UNMARKING REPORT'                                     
ACUN01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF RESET                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
*                                                                               
         ACDEF UPDATE,ACCFIL                                                    
*                                                                               
         ASPEC H1,2,RUN                                                         
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,100,PAGE                                                      
         ASPEC H4,2,COMPANY                                                     
         ASPEC F1,2,REQDETS                                                     
         ASPEC H1,48,C'UNMARKING REPORT'                                        
         ASPEC H2,48,C'----------------'                                        
         ASPEC H5,2,C'LEDGER'                                                   
         ASPEC H8,2,C'ACCOUNT CODE    ACCOUNT NAME'                             
         ASPEC H9,2,C'------------    ------------'                             
         ASPEC H8,61,C'UNMARKED VALUE'                                          
         ASPEC H9,61,C'--------------'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPUN01 07/07/07'                                      
         END                                                                    
