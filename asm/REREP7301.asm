*          DATA SET REREP7301  AT LEVEL 021 AS OF 06/06/95                      
*PHASE RE7301A,*                                                                
         TITLE 'REREP7001 (RE7301) --- SPECS FOR STATION LISTING'               
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP7301 - RE7301 - SPECS FOR STATION LISTING                  *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* 26MAY95 SKU ORIGINATION                                           *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE7301   CSECT                                                                  
         SPROG 0                                                                
         ASPEC H01,055,C'CONCISE STATION LISTING'                               
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,001,REQUESTOR                                                
         ASPEC H02,055,23C'-'                                                   
         ASPEC H02,100,RUN                                                      
         ASPEC H04,004,C'STATION'                                               
         ASPEC H05,004,7C'-'                                                    
         ASPEC H04,012,C'MARKET'                                                
         ASPEC H05,012,20C'-'                                                   
         ASPEC H04,034,C'REP'                                                   
         ASPEC H05,034,23C'-'                                                   
         ASPEC H04,059,C'JOIN'                                                  
         ASPEC H05,059,8C'-'                                                    
         ASPEC H04,069,C'LEAVE'                                                 
         ASPEC H05,069,8C'-'                                                    
         ASPEC H04,079,C'OWNER'                                                 
         ASPEC H05,079,24C'-'                                                   
         ASPEC H04,105,C'TVB'                                                   
         ASPEC H05,105,3C'-'                                                    
         ASPEC H04,109,C'AFF'                                                   
         ASPEC H05,109,3C'-'                                                    
         ASPEC H04,114,C'RANK'                                                  
         ASPEC H05,114,4C'-'                                                    
         ASPEC H04,120,C'INTERFACE'                                             
         ASPEC H05,120,10C'-'                                                   
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021REREP7301 06/06/95'                                      
         END                                                                    
