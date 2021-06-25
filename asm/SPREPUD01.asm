*          DATA SET SPREPUD01  AT LEVEL 004 AS OF 04/14/14                      
*PHASE SPUD01A                                                                  
SPUD01   TITLE 'SPREPUD01 - BASE FOR CABLE UPDATE RUN'                          
SPUD01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
***      FSPEC UPDATE,SPTDIR                                                    
***      FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,STATION                                                   
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC READ,BUYS                                                        
         SSPEC H1,57,C'UPDATE CABLE DSTA'                                       
         SSPEC H2,57,C'-----------------'                                       
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
*****    FSPEC OPEN,DEMFILES                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPUD01 04/14/14'                                      
         END                                                                    
