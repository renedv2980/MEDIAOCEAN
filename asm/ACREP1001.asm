*          DATA SET ACREP1001  AT LEVEL 016 AS OF 10/21/03                      
*PHASE AC1001A,+0                                                               
         TITLE 'SPECS FOR DDS-BILLING DOWNLOAD RECORDS'                         
         PRINT NOGEN                                                            
AC1101   CSECT                                                                  
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPROG 0,1                                                              
         ASPEC H1,55,C'DONOVAN DATA SYSTEMS, INC.'                              
         ASPEC H2,55,C'115 W 18TH ST.  NY, NY  10011'                           
         ASPEC H4,55,REPORT                                                     
         ASPEC H5,55,RUN                                                        
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H1,2,COMPANY                                                     
         ASPEC H2,2,COMPADD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACREP1001 10/21/03'                                      
         END                                                                    
