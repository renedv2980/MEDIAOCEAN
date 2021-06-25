*          DATA SET SPREPIF01  AT LEVEL 004 AS OF 08/29/00                      
*PHASE SPIF01A                                                                  
         TITLE 'SPIF01 - SPECS'                                                 
SPIF01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,50,C'INVOICE MATCH STATUS RECORD CHECK'                       
         SSPEC H2,50,C'---------------------------------'                       
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPIF01 08/29/00'                                      
         END                                                                    
