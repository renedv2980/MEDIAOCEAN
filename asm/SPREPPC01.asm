*          DATA SET SPREPPC01  AT LEVEL 002 AS OF 08/04/04                      
*PHASE SPPC01A                                                                  
SPPC01   TITLE 'SPREPPC01 - BASE FOR CABLE PRINT RUN'                           
SPPC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC READ,BUYS                                                        
         SSPEC H1,57,C'CABLE PRINT RUN'                                         
         SSPEC H2,57,C'---------------'                                         
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
*****    FSPEC OPEN,DEMFILES                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPPC01 08/04/04'                                      
         END                                                                    
