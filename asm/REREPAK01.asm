*          DATA SET REREPAK01  AT LEVEL 013 AS OF 01/31/96                      
*PHASE REAK01A,*                                                                
         TITLE 'SPECS FOR KATZ EDI DATA TRANSFER'                               
*                                                                               
*- REREPAK01 -- PHASE REAK01 -- SPECS MODULE FOR KATZ EDI TRANSFER              
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REAK01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         RSPEC REQUEST,NOREP                                                    
         ASPEC H1,2,RUN                                                         
         ASPEC H1,39,C'BUILD KATZ EDI ACKNOWLEDGMENTS RECORDS'                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013REREPAK01 01/31/96'                                      
         END                                                                    
