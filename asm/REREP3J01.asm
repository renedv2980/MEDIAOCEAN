*          DATA SET REREP3J01  AT LEVEL 019 AS OF 02/09/05                      
*PHASE RE3J01A,*                                                                
         TITLE 'RE3J01 - REREP3J01 - RER LOTUS INTERFACE TAPE'                  
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3J01 - RE3J01 - RER LOTUS INTRFACE TAPE REPORT             *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* FEB09/05 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3J01   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,1                                                              
         ASPEC H01,002,REP                                                      
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H01,045,C'RER LOTUS INTERFACE TAPE REPORT'                       
         ASPEC H02,045,30C'-'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019REREP3J01 02/09/05'                                      
         END                                                                    
