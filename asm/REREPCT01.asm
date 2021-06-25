*          DATA SET REREPCT01  AT LEVEL 010 AS OF 11/13/95                      
*PHASE RECT01A,*                                                                
         TITLE 'SPECS FOR COUNTING RECORDS FOR EAHC REP'                        
*                                                                               
*- REREPCT01 -- PHASE RECT01 -- SPECS MODULE FOR REP RECORD COUNTING            
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  11/10/94 SKU NEW                                                             
*                                                                               
RECT01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'FILE COUNTING REPORT'                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REREPCT01 11/13/95'                                      
         END                                                                    
