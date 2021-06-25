*          DATA SET SPREPNC01  AT LEVEL 003 AS OF 08/27/07                      
*PHASE SPNC01A                                                                  
SPNC01   TITLE 'SPREPNC01 - BASE FOR NEW CABLE AGENCY'                          
SPNC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
***      FSPEC UPDATE,SPTDIR                                                    
***      FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,STATION                                                   
         FSPEC UPDATE,TRAFFIC                                                   
         FSPEC READ,BUYS                                                        
         SSPEC H1,57,C'NEW CABLE AGENCY'                                        
         SSPEC H2,57,C'----------------'                                        
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
*****    FSPEC OPEN,DEMFILES                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPNC01 08/27/07'                                      
         END                                                                    
