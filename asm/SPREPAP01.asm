*          DATA SET SPREPAP01  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPAP01A                                                                  
         TITLE 'SPAP01 - LOCKS AND COPIES APPROVED BG STATUS RECS'              
SPAP01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,55,C'APPROVED BG STATUS RECORDS'                              
         SSPEC H2,55,C'--------------------------'                              
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,PAGE                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPAP01 08/29/00'                                      
         END                                                                    
