*          DATA SET SPREPWF01  AT LEVEL 002 AS OF 06/03/03                      
*PHASE SPWF01A                                                                  
         TITLE 'SPWF01 - SPOT WRITER FORMAT EXTRACT - SPECS'                    
SPWF01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,55,C'SPOT WRITER FORMAT EXTRACT'                              
         SSPEC H2,55,C'--------------------------'                              
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPWF01 06/03/03'                                      
         END                                                                    
