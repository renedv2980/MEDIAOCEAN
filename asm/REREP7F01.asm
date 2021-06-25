*          DATA SET REREP7F01  AT LEVEL 016 AS OF 01/15/96                      
*PHASE RE7F01A,*                                                                
         TITLE 'SPECS FOR KATZ DATA CONVERSION'                                 
*                                                                               
*- REREPKS01 -- PHASE RE7F01 -- SPECS MODULE FOR SUPER SWITCHER                 
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
RE7F01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,27,C'AGENCY SUPER SWITCHER PREP REPORT'                       
         SPROG 0                                                                
         ASPEC H2,1,C' KATZ     DDS       AGENCY'                               
         SPROG 0                                                                
         ASPEC H3,1,C' CODE     CODE      NAME  '                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016REREP7F01 01/15/96'                                      
         END                                                                    
