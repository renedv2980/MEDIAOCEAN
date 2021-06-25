*          DATA SET REREPSP01  AT LEVEL 002 AS OF 10/08/02                      
*PHASE RESP01A,*                                                                
         TITLE 'SPECS FOR REP REPORT SPECS S/P GENDIR/GENFIL'                   
*                                                                               
*- REREPSP01 -- PHASE RESP01 -- SPECS MODULE FOR RSPTSPREC CREATION             
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  10/10/02  BU   INITIAL CREATION                                              
*                                                                               
RESP01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,38,C'RSPTSPREC FILE INFORMATION'                              
         SPROG 1                                                                
         SPROG 2                                                                
         ASPEC H1,2,RUN                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REREPSP01 10/08/02'                                      
         END                                                                    
