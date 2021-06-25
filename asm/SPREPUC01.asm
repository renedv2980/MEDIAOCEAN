*          DATA SET SPREPUC01  AT LEVEL 002 AS OF 07/03/07                      
*PHASE SPUC01A                                                                  
SPUC01   TITLE 'SPREPUC01 - BASE FOR CABLE UPDATE RUN'                          
SPUC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
***      FSPEC UPDATE,SPTDIR                                                    
***      FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,STATION                                                   
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC READ,BUYS                                                        
         SSPEC H1,57,C'CABLE UPDATE RUN'                                        
         SSPEC H2,57,C'----------------'                                        
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
*****    FSPEC OPEN,DEMFILES                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPUC01 07/03/07'                                      
         END                                                                    
