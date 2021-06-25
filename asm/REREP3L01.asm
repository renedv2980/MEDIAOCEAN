*          DATA SET REREP3L01  AT LEVEL 001 AS OF 10/05/95                      
*          DATA SET REREP3K01  AT LEVEL 001 AS OF 09/19/95                      
*          DATA SET REREP3G01  AT LEVEL 017 AS OF 02/04/95                      
*PHASE RE3L01A,*                                                                
         TITLE 'RE3L01 - REREP3L01 - KATZ INTERFACE TAPE'                       
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3L01 - RE3G01 - KATZ INTERFACE TAPE REPORT                 *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* OCT05/95 (BG ) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3L01   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,1                                                              
         ASPEC H01,002,REP                                                      
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE                                                                  
         SPROG 0                                                                
         ASPEC H01,048,C'KATZ INTERFACE 1640 TAPE REPORT'                       
         ASPEC H02,048,25C'-'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001REREP3L01 10/05/95'                                      
         END                                                                    
