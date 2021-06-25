*          DATA SET REREP3B01  AT LEVEL 020 AS OF 04/11/02                      
*PHASE RE3B01A,*                                                                
         TITLE 'RE3B01 - REREP3B01 - MEDIA OCEAN DEMO FEED'                     
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP3B01 - RE3B01 - MEDIA OCEAN DEMO FEED                      *           
*                                                                   *           
*********************************************************************           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* MAR27/02 (BU ) --- INITIAL ENTRY                                  *           
*                                                                   *           
*                    ***  END TOMBSTONE  ***                        *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3B01   CSECT                                                                  
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
**PAN#1  DC    CL21'020REREP3B01 04/11/02'                                      
         END                                                                    
