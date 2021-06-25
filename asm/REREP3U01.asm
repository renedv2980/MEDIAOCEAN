*          DATA SET REREP3U01  AT LEVEL 021 AS OF 04/18/06                      
*PHASE RE3U01A,*                                                                
         TITLE 'RE3U01 - REREP3U01 - MEDIA OCEAN DEMO FEED'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3U01 - RE3U01 - MEDIA OCEAN DEMO FEED (UNIVISION)          *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* APR18/06 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3U01   CSECT                                                                  
         FSPEC READ,CONTRACTS                                                   
         FSPEC READ,CONTRACTS                                                   
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
**PAN#1  DC    CL21'021REREP3U01 04/18/06'                                      
         END                                                                    
