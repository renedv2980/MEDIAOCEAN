*          DATA SET SPREPKD01  AT LEVEL 001 AS OF 06/14/06                      
*PHASE SPKD01A                                                                  
         TITLE 'SPECS FOR SBS DSTA DELTA REPORT'                                
SPKD01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC READ,BUYS                                                        
         SSPEC H1,57,C'SBS DSTA DELTA'                                          
         SSPEC H2,57,C'--------------'                                          
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPKD01 06/14/06'                                      
         END                                                                    
