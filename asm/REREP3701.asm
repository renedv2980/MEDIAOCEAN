*          DATA SET REREP3701  AT LEVEL 016 AS OF 01/03/90                      
*PHASE RE3701A,*                                                                
         TITLE 'RE3701 - REREP3701 - SPECS FOR REP INTERFACE TAPE'              
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3701 - RE3701 - SPECS FOR REP INTERFACE TAPE REPORT        *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUL/89 -(SNS) INITIAL RELEASE                                     *           
*                                                                   *           
* JAN03/90 (MRR) --- CHANGE REPORT NAME FROM INTEREP TAPE REPORT    *           
*                     TO INTERFACE TAPE REPORT                      *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3701   CSECT                                                                  
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
**PAN#1  DC    CL21'016REREP3701 01/03/90'                                      
         END                                                                    
