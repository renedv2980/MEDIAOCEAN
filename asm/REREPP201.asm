*          DATA SET REREPP201  AT LEVEL 014 AS OF 02/26/97                      
*PHASE REP201A,*                                                                
         TITLE 'SPECS FOR NEW WORLD DATA CONVERSION'                            
*                                                                               
*- REREPK101 -- PHASE REP201 -- SPECS MODULE FOR NEW WORLD CONVERSION           
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REP201   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,38,C'NEW WORLD DATA STRIP PROGRAM'                            
         SPROG 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014REREPP201 02/26/97'                                      
         END                                                                    
