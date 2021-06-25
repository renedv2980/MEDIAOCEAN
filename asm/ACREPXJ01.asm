*          DATA SET ACREPXJ01  AT LEVEL 003 AS OF 08/16/00                      
*PHASE ACXJ01A,+0                                                               
         TITLE 'SPECS FOR DDS-BILLING DOWNLOAD RECORDS'                         
         PRINT NOGEN                                                            
ACXJ01   CSECT                                                                  
         SPROG 0,1                                                              
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACREPXJ01 08/16/00'                                      
         END                                                                    
