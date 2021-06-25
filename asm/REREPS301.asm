*          DATA SET REREPS301  AT LEVEL 022 AS OF 04/08/97                      
*PHASE RES301A,*                                                                
         TITLE 'SPECS FOR STATION TAKEOVER INITIALIZER'                         
*                                                                               
*- REREPS301 -- PHASE RES301 -- SPECS MODULE FOR STATION TAKEOVER               
*                                  INITIALIZER                                  
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
RES301   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'STATION TAKEOVER FILE INITIALIZER'                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022REREPS301 04/08/97'                                      
         END                                                                    
