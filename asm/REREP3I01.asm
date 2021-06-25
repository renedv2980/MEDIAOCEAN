*          DATA SET REREP3I01  AT LEVEL 019 AS OF 01/09/01                      
*PHASE RE3I01A,*                                                                
         TITLE 'RE3I01 - REREP3I01 - RER KATZ INTERFACE TAPE'                   
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3I01 - RE3I01 - RER KATZ INTERFACE TAPE REPORT             *           
*               CLEAR CHANNEL VERSION                               *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JAN09/01 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3I01   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,1                                                              
         ASPEC H01,002,REP                                                      
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H01,045,C'RER KATZ INTERFACE TAPE REPORT'                        
         ASPEC H02,045,30C'-'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019REREP3I01 01/09/01'                                      
         END                                                                    
