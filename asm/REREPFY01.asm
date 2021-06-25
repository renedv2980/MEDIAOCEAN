*          DATA SET REREPFY01  AT LEVEL 002 AS OF 11/11/02                      
*          DATA SET REREPAG01  AT LEVEL 010 AS OF 02/19/96                      
*PHASE REFY01A,*                                                                
         TITLE 'SPECS FOR RADIO CODE SEED'                                      
REFY01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         FSPEC H1,2,RUN                                                         
         FSPEC H1,40,C'RADIO CODE SEED REPORT'                                  
         SPROG 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REREPFY01 11/11/02'                                      
         END                                                                    
