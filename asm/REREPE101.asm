*          DATA SET REREPE101  AT LEVEL 011 AS OF 02/01/96                      
*PHASE REE101A,*                                                                
         TITLE 'SPECS FOR KATZ EDI TURNAROUND '                                 
*                                                                               
*- REREPE101 -- PHASE REE101 -- SPECS MODULE FOR KATZ EDI T/A                   
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REE101   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'KATZ EDI TURNAROUND    '                                 
         SPROG 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011REREPE101 02/01/96'                                      
         END                                                                    
