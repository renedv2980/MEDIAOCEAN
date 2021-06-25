*          DATA SET REREP3401  AT LEVEL 017 AS OF 02/11/92                      
*PHASE RE3401A,*                                                                
         TITLE 'REREP3401(RE3401) - SPECS FOR STATION MONTHLY SUMMARY'          
*                                                                               
*********************************************************************           
*                                                                   *           
*        REREP4701 --- SPECS FOR COMMISSION REPORT                  *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* FEB07/92 (MRR) --- REVAMP HEADER TO MATCH OTHERS, REMOVE BASIS    *           
*                                                                   *           
*                                                                   *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE3401   CSECT                                                                  
*                                                                               
         FSPEC READ,CONTRACTS                                                   
         RSPEC MAXLINES,56                                                      
*                                                                               
         SPROG 0,1,3                                                            
         ASPEC H01,002,REP                                                      
         ASPEC H01,045,C'STATION MONTHLY SUMMARY'                               
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,045,C'-----------------------'                               
         ASPEC H02,100,RUN                                                      
         ASPEC H03,046,PERIOD                                                   
         ASPEC H07,002,C'STATION/MARKET'                                        
         ASPEC H08,002,C'        OFFICE'                                        
         ASPEC H09,002,C'------------------------'                              
         ASPEC H08,105,C'TOTAL'                                                 
         ASPEC H09,105,C'-----'                                                 
         ASPEC F01,002,REQDETS                                                  
         SPROG 0                                                                
         ASPEC H04,002,GROUP                                                    
         ASPEC H05,002,SUBGROUP                                                 
         ASPEC H04,100,C'STATIONS BY GROUP'                                     
         SPROG 1                                                                
         ASPEC H05,002,C'ALL STATIONS'                                          
         SPROG 2                                                                
         ASPEC F01,002,REQDETS                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017REREP3401 02/11/92'                                      
         END                                                                    
