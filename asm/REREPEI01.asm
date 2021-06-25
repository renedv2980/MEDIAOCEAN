*          DATA SET REREPEI01  AT LEVEL 013 AS OF 02/12/96                      
*PHASE REEI01A,*                                                                
         TITLE 'SPECS FOR KATZ BULK EDI LOADER'                                 
*                                                                               
*- REREPED01 -- PHASE REEI01 -- SPECS MODULE FOR KATZ BULK EDI LOADER           
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REEI01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         RSPEC REQUEST,NOREP                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'KATZ BULK EDI LOADER PROGRAM'                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013REREPEI01 02/12/96'                                      
         END                                                                    
