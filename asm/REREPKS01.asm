*          DATA SET REREPKS01  AT LEVEL 012 AS OF 12/09/95                      
*PHASE REKS01A,*                                                                
         TITLE 'SPECS FOR KATZ DATA CONVERSION'                                 
*                                                                               
*- REREPKS01 -- PHASE REKS01 -- SPECS MODULE FOR SUPER SWITCHER                 
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REKS01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'KATZ SUPER SWITCHER'                                     
         SPROG 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012REREPKS01 12/09/95'                                      
         END                                                                    
