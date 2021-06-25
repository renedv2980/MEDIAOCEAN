*          DATA SET REREPRR01  AT LEVEL 001 AS OF 09/10/98                      
*PHASE RERR01A,*                                                                
         TITLE 'SPECS FOR REP REPORT SPECS RECORD CREATION'                     
*                                                                               
*- REREPRR01 -- PHASE RERR01 -- SPECS MODULE FOR RSPCREC CREATION               
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  09/10/98  NRK  INITIAL CREATION                                              
*                                                                               
RERR01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'RSPCREC FILE INFORMATION'                                
         SPROG 1                                                                
         SPROG 2                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'DI/HN FILE MERGE FACILITY'                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001REREPRR01 09/10/98'                                      
         END                                                                    
