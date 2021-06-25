*          DATA SET REREPP101  AT LEVEL 014 AS OF 02/28/97                      
*PHASE REP101A,*                                                                
         TITLE 'SPECS FOR NEW WORLD DATA CONVERSION'                            
*                                                                               
*- REREPK101 -- PHASE REP101 -- SPECS MODULE FOR NEW WORLD CONVERSION           
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*                                                                               
REP101   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'NEW WORLD CONVERSION PROGRAM'                            
         SPROG 1                                                                
         ASPEC H3,01,C'-----JDS-----'                                           
         ASPEC H3,15,C'-----DDS-----'                                           
         ASPEC H4,01,C'S/P  TM OF   '                                           
         ASPEC H4,15,C'S/P  TM OF   '                                           
         ASPEC H5,01,C'CODE CD CD   '                                           
         ASPEC H5,15,C'CODE CD CD   '                                           
         SPROG 2                                                                
         ASPEC H3,01,C'MISSING STATION LISTING'                                 
         ASPEC H4,04,C' RECORDS ADDED'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014REREPP101 02/28/97'                                      
         END                                                                    
