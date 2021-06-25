*          DATA SET REREPBK01  AT LEVEL 011 AS OF 12/09/99                      
*PHASE REBK01A,*                                                                
         TITLE 'SPECS FOR BACK BILLING EXTRACT/CREATION'                        
*                                                                               
*- REREPBK01 -- PHASE REBK01 -- SPECS MODULE FOR REP EXTRACT/CREATION           
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  12/09/99 BU  NEW                                                             
*                                                                               
REBK01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H1,36,C'BACK BILLING EXTRACT INFORMATION'                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011REREPBK01 12/09/99'                                      
         END                                                                    
