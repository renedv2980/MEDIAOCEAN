*          DATA SET ACREPCS01  AT LEVEL 002 AS OF 08/16/00                      
*PHASE ACCS01A,+0                                                               
         TITLE 'SPECS FOR CLIENT STATEMENT'                                     
ACCS01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREPCS01 08/16/00'                                      
         END                                                                    
