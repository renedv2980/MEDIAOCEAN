*          DATA SET SPREPYT01  AT LEVEL 044 AS OF 08/29/00                      
*PHASE SPYT01A                                                                  
         TITLE 'SPYT01 - SPOTPAK CONVERSION TO TBS BUY RECORDS'                 
SPYT01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,55,C'A FILE CONVERSION'                                       
         SSPEC H2,55,C'--------------------'                                    
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044SPREPYT01 08/29/00'                                      
         END                                                                    
