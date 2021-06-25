*          DATA SET REREPCV01  AT LEVEL 003 AS OF 08/31/00                      
*          DATA SET REREPCV01  AT LEVEL 002 AS OF 03/26/86                      
*PHASE RECV01A                                                                  
         TITLE 'RECV01 - DUMMY SPECS FOR MPI CONVERSION PROGRAM'                
RECV01   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC REQUEST,NOREP                                                    
         SPROG 0,1                                                              
         ASPEC H1,58,C'MINIPAK CONVERSION'                                      
         ASPEC H2,58,C'------------------'                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003REREPCV01 08/31/00'                                      
         END                                                                    
