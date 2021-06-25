*          DATA SET REREP1501  AT LEVEL 018 AS OF 12/06/07                      
*PHASE RE1501A,*                                                                
         TITLE 'SPECS FOR REP TAKEOVER TAPE EXTRACT/UPDATE'                     
*                                                                               
*- REREPTK01 -- PHASE RETK01 -- SPECS MODULE FOR REP EXTRACT/UPDATE             
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  02/18/97 SKU NEW                                                             
*                                                                               
RETK01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,REPFIL                                                    
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,40,C'DARE TAKEOVER TAPE EXTRACT/UPDATE'                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018REREP1501 12/06/07'                                      
         END                                                                    
