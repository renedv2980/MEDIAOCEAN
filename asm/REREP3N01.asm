*          DATA SET REREP3N01  AT LEVEL 002 AS OF 02/09/05                      
*          DATA SET REREP3G01  AT LEVEL 017 AS OF 02/04/95                      
*PHASE RE3N01A,*                                                                
         TITLE 'RE3N01 - REREP3N01 - LOTUS INTERFACE TAPE'                      
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3N01 - RE3K01 - LOTUS INTERFACE TAPE REPORT                *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* FEB09/05 (BU ) --- INITIAL ENTRY (FROM 3K)                        *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3N01   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,1                                                              
         ASPEC H01,002,REP                                                      
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE                                                                  
         SPROG 0                                                                
         ASPEC H01,048,C'LOTUS INTERFACE TAPE REPORT'                           
         ASPEC H02,048,25C'-'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002REREP3N01 02/09/05'                                      
         END                                                                    
