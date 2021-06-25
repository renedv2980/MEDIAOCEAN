*          DATA SET REREP3Z01  AT LEVEL 005 AS OF 02/02/10                      
*PHASE RE3Z01A                                                                  
         TITLE 'RE3Z01 - REREP3Z01 - MEDIA OCEAN DEMO FEED'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3Z01 - RE3Z01 - MEDIA OCEAN DEMO FEED (KATZ TV)            *           
*                                                                   *           
*               KATZ "MILENNIUM" (SZ) ONLY                          *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* DEC22/09 (SMY) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3Z01   CSECT                                                                  
***      FSPEC READ,CONTRACTS                                                   
***      FSPEC READ,CONTRACTS                                                   
         RSPEC REQUEST,NOREP                                                    
         RSPEC REQUEST,NOSUM                                                    
***      SPROG 0,1                                                              
***      ASPEC H01,002,REP                                                      
***      ASPEC H01,100,RENUM                                                    
***      ASPEC H01,120,PAGE                                                     
***      ASPEC H02,002,REQUESTOR                                                
***      ASPEC H02,100,RUN                                                      
***      SPACE 1                                                                
***      SPROG 0                                                                
***      ASPEC H01,048,C'  MEDIA OCEAN DEMO FEED    '                           
***      ASPEC H02,048,25C'-'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005REREP3Z01 02/02/10'                                      
         END                                                                    
