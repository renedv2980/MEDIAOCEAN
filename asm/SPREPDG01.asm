*          DATA SET SPREPDG01  AT LEVEL 001 AS OF 07/26/04                      
*PHASE SPDG01A                                                                  
         TITLE 'SPDG01 - IQ/DDS BUY UPLOAD - SPECS'                             
SPDG01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         SSPEC H1,53,C'GENERATE IQ OUTPUT DATASETS'                             
         SSPEC H2,53,C'---------------------------'                             
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPDG01 07/26/04'                                      
         END                                                                    
