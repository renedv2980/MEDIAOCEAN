*          DATA SET REREP4G01  AT LEVEL 017 AS OF 07/13/95                      
*PHASE RE4G01A,*                                                                
         TITLE 'RE4G01 - REREP4G01 - SPECS FOR REP INTERFACE TAPE'              
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP4G01 - RE4G01 - SPECS FOR REP INTERFACE TAPE REPORT        *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUL/89 -(SNS) INITIAL RELEASE                                     *           
*                                                                   *           
* JAN03/90 (MRR) --- CHANGE REPORT NAME FROM INTEREP TAPE REPORT    *           
*                     TO INTERFACE TAPE REPORT                      *           
*                                                                   *           
* JUL13/95 (BU ) --- NEW VERSION FOR TYPE N REPORTS                 *           
*                                                                   *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE4G01   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
         SPROG 0,1                                                              
         ASPEC H01,002,REP                                                      
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H01,050,C'INTERFACE TAPE REPORT'                                 
         ASPEC H02,050,21C'-'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017REREP4G01 07/13/95'                                      
         END                                                                    
