*          DATA SET REREPCK01  AT LEVEL 014 AS OF 02/18/96                      
*PHASE RECK01A,*                                                                
         TITLE 'SPECS FOR KATZ EDI DATA TRANSFER'                               
*                                                                               
*- REREPCK01 -- PHASE RECK01 -- SPECS MODULE FOR KATZ EDI TRANSFER              
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
RECK01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         RSPEC REQUEST,NOREP                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'BUILD KATZ EDI ACKNOWLEDGMENTS RECORDS'                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014REREPCK01 02/18/96'                                      
         END                                                                    
