*          DATA SET REREP3P01  AT LEVEL 022 AS OF 09/05/07                      
*PHASE RE3P01A,*                                                                
         TITLE 'RE3P01 - REREP3P01 - MEDIA OCEAN DEMO FEED'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3P01 - RE3P01 - MEDIA OCEAN DEMO FEED (UNIVISION)          *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* SEP05/07 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3P01   CSECT                                                                  
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
**PAN#1  DC    CL21'022REREP3P01 09/05/07'                                      
         END                                                                    
