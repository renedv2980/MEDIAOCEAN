*          DATA SET REREPFKT01 AT LEVEL 013 AS OF 01/26/96                      
*PHASE REFK01A,*                                                                
         TITLE 'SPECS FOR KATZ EDI DATA COPY, CHANGE AND RETURNT'               
*                                                                               
*- REREPFKT01 -- PHASE REFK01 -- SPECS MODULE FOR KATZ EDI RETURN               
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REFK01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         RSPEC REQUEST,NOREP                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'KATZ EDI RETURN PROGRAM'                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013REREPFKT0101/26/96'                                      
         END                                                                    
