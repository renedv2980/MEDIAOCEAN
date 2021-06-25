*          DATA SET REREP3C01  AT LEVEL 021 AS OF 05/02/02                      
*PHASE RE3C01A,*                                                                
         TITLE 'RE3C01 - REREP3C01 - MEDIA OCEAN SUPPORT FEED'                  
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3C01 - RE3C01 - MEDIA OCEAN SUPPORT FEED: BDE              *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* MAY02/02 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3C01   CSECT                                                                  
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
**PAN#1  DC    CL21'021REREP3C01 05/02/02'                                      
         END                                                                    
