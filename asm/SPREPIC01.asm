*          DATA SET SPREPIC01  AT LEVEL 002 AS OF 08/29/00                      
*          DATA SET SPREPIC01  AT LEVEL 040 AS OF 12/14/95                      
*PHASE SPIC01A                                                                  
         TITLE 'SPIC01 - SPECS'                                                 
SPIC01   CSECT                                                                  
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
**PAN#1  DC    CL21'002SPREPIC01 08/29/00'                                      
         END                                                                    
