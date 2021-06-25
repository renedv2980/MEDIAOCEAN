*          DATA SET REREP3F01  AT LEVEL 001 AS OF 03/27/09                      
*PHASE RE3F01A,*                                                                
         TITLE 'RE3F01 - REREP3F01 - MEDIA OCEAN DEMO FEED'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3P01 - RE3F01 - MEDIA OCEAN DEMO FEED (FSSNY    )          *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* MAR27/09 (KUI) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3F01   CSECT                                                                  
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
**PAN#1  DC    CL21'001REREP3F01 03/27/09'                                      
         END                                                                    
