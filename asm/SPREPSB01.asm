*          DATA SET SPREPSB01  AT LEVEL 002 AS OF 07/29/14                      
*          DATA SET SPREPSA01  AT LEVEL 009 AS OF 07/29/14                      
*PHASE SPSB01A                                                                  
         TITLE 'SPSA01 - SPOT SAP EXTRACT - SPECS'                              
SPSB01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         SSPEC H1,43,C'SPOT SAP CODES'                                          
         SSPEC H2,43,C'--------------'                                          
         SSPEC H1,1,REPORT                                                      
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,26,PAGE                                                       
         SSPEC H1,80,AGYNAME                                                    
         SSPEC H2,80,AGYADD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPSB01 07/29/14'                                      
         END                                                                    
