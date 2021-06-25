*          DATA SET ACREPXV01  AT LEVEL 004 AS OF 08/11/00                      
*PHASE ACXV01A,+0                                                               
         TITLE 'SPECS FOR RL SCRIBE FORMAT FIX'                                 
ACXV01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,ACCFILE                                                   
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,83,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H1,36,C'RL FORMAT FIX'                                           
         ACDEF H2,36,C'--------------'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACREPXV01 08/11/00'                                      
         END                                                                    
