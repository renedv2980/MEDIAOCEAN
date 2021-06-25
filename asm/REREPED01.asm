*          DATA SET REREPED01  AT LEVEL 012 AS OF 01/22/96                      
*PHASE REED01A,*                                                                
         TITLE 'SPECS FOR KATZ EDI DATA TRANSFER'                               
*                                                                               
*- REREPED01 -- PHASE REED01 -- SPECS MODULE FOR KATZ EDI TRANSFER              
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REED01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         RSPEC REQUEST,NOREP                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'KATZ EDI TRANSFER PROGRAM'                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012REREPED01 01/22/96'                                      
         END                                                                    
