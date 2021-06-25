*          DATA SET SPREPIQ01  AT LEVEL 001 AS OF 04/23/03                      
*PHASE SPIQ01A                                                                  
         TITLE 'SPIQ01 - IQ/DDS BUY UPLOAD - SPECS'                             
SPIQ01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,55,C'IQ TO DDS FILE TRANSFER'                                 
         SSPEC H2,55,C'-----------------------'                                 
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPIQ01 04/23/03'                                      
         END                                                                    
