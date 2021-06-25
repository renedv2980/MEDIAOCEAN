*          DATA SET ACREP1101  AT LEVEL 004 AS OF 09/08/99                      
*PHASE AC1101A,+0                                                               
         TITLE 'SPECS FOR DDS-BILLING DOWNLOAD RECORDS'                         
         PRINT NOGEN                                                            
AC1101   CSECT                                                                  
         SPROG 0,1                                                              
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREP1101 09/08/99'                                      
         END                                                                    
