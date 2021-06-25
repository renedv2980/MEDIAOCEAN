*          DATA SET ACREPC501  AT LEVEL 003 AS OF 08/16/00                      
*PHASE ACC501A,+0                                                               
         TITLE 'SPECS FOR AYER DAILY TIME CARDS'                                
ACC501   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPC501 08/16/00'                                      
         END                                                                    
