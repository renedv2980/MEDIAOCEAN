*          DATA SET SPREPCU01  AT LEVEL 003 AS OF 08/29/00                      
*PHASE SPCU01A                                                                  
         TITLE 'SPCU01 - SPECS FOR CCUSA COPY COMMERCIAL RECS TO CLT'           
SPCU01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC READ,BUYS                                                        
         SSPEC H1,55,C'COPY CCUSA CMMLS TO NEW CLIENT'                          
         SSPEC H2,55,C'------------------------------'                          
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         FSPEC OPEN,DEMFILES                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003SPREPCU01 08/29/00'                                      
         END                                                                    
