*          DATA SET ACREP7701  AT LEVEL 002 AS OF 08/16/00                      
*PHASE AC7701A                                                                  
         TITLE 'SPECS FOR STICKY LABELS'                                        
AC7701   CSECT                                                                  
         PRINT GEN                                                              
         ACDEF NOREP,REQDETS                                                    
         ACDEF NOREP,REQDETS                                                    
         ACDEF READ,ACCOUNTS                                                    
         ACDEF MODE,PROCLEV                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002ACREP7701 08/16/00'                                      
         END                                                                    
