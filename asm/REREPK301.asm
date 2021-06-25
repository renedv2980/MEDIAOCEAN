*          DATA SET REREPK301  AT LEVEL 009 AS OF 07/21/95                      
*PHASE REK301A,*                                                                
         TITLE 'SPECS FOR KATZ DATA CONVERSION'                                 
*                                                                               
*- REREPK301 -- PAHSE REK301 -- SPECS MODULE FOR KATZ CONVERSION                
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REK301   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,36,C'KATZ STATION CONVERSION PROGRAM'                         
         SPROG 1                                                                
         ASPEC H3,15,C'***RECORD CODES***'                                      
         ASPEC H3,60,C'***RECORD COUNTS***'                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009REREPK301 07/21/95'                                      
         END                                                                    
