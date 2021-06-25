*          DATA SET REREP3H01  AT LEVEL 018 AS OF 01/20/98                      
*PHASE RE3H01A,*                                                                
         TITLE 'RE3H01 - REREP3H01 - RER KATZ INTERFACE TAPE'                   
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3H01 - RE3H01 - RER KATZ INTERFACE TAPE REPORT             *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* FEB04/95 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3H01   CSECT                                                                  
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
**PAN#1  DC    CL21'018REREP3H01 01/20/98'                                      
         END                                                                    
