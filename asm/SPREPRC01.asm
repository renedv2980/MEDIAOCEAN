*          DATA SET SPREPRC01  AT LEVEL 003 AS OF 06/08/20                      
*PHASE SPRC01A                                                                  
SPRC01   TITLE 'SPREPRC01 - BASE FOR CABLE REPURPOSE'                           
SPRC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
***      FSPEC UPDATE,SPTDIR                                                    
***      FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,STATION                                                   
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC READ,BUYS                                                        
         SSPEC H1,57,C'CABLE REPURPOSE'                                         
         SSPEC H2,57,C'---------------'                                         
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
*****    FSPEC OPEN,DEMFILES                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPRC01 06/08/20'                                      
         END                                                                    
