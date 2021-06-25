*          DATA SET REREPAG01  AT LEVEL 010 AS OF 02/19/96                      
*PHASE REAG01A,*                                                                
         TITLE 'SPECS FOR NEW REP FILE EXTRACT/CREATION'                        
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  02/19/96 SKU NEW                                                             
*                                                                               
REKH01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'FILE EXTRACT INFORMATION'                                
         SPROG 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REREPAG01 02/19/96'                                      
         END                                                                    
