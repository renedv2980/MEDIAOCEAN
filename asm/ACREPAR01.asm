*          DATA SET ACREPAR01  AT LEVEL 005 AS OF 08/16/00                      
*PHASE ACAR01A,+0                                                               
         TITLE 'SPECS FOR NEW ACCOUNTS RECEIVABLE'                              
ACAR01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPROG 0                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACREPAR01 08/16/00'                                      
         END                                                                    
