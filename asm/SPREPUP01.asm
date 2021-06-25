*          DATA SET SPREPUP01  AT LEVEL 023 AS OF 08/29/00                      
*PHASE SPUP01A                                                                  
SPUP01   TITLE 'SPREPUP01 - BASE FOR CABLE UPLOAD RUN'                          
SPUP01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC READ,BUYS                                                        
         SSPEC H1,57,C'CABLE UPLOAD RUN'                                        
         SSPEC H2,57,C'----------------'                                        
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
*****    FSPEC OPEN,DEMFILES                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPREPUP01 08/29/00'                                      
         END                                                                    
