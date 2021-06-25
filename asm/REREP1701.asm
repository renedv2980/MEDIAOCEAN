*          DATA SET REREP1701  AT LEVEL 003 AS OF 03/12/90                      
*PHASE RE1701A,*                                                                
         TITLE 'REREP1701 (RE1701)- STATION ADV COMPARISON SPECS'               
***********************************************************************         
*                                                                     *         
*  REREP1701 (RE1701) --- SPECS FOR THE STATION ADVERTISER COMPARISON *         
*                         (AKA DONUT REPORT)                          *         
*                                                                     *         
* ------------------------------------------------------------------- *         
*  MOD LOG:                                                           *         
*  -------                                                            *         
*                                                                     *         
*  02/13/90  PJS  ORINGINAL DEVELOPMENT                               *         
*                                                                     *         
*  MAR12/90 (MRR) --- CHANGE REPORT NAME                              *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
*                                                                     *         
***********************************************************************         
*                                                                               
RE1701   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         ASPEC H01,002,REP                                                      
         ASPEC H01,048,C'STATION ADVERTISER COMPARISON REPORT'                  
         ASPEC H01,099,RENUM                                                    
         ASPEC H01,115,PAGE                                                     
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,048,C'------------------------------------'                  
         ASPEC H02,099,RUN                                                      
*                                                                               
         ASPEC H06,002,C'ADVERTISER CODE-NAME'                                  
         ASPEC H08,002,C'----'                                                  
         ASPEC H07,008,C'OFFICE'                                                
         ASPEC H08,008,C'------'                                                
         ASPEC H07,016,C'AGENCY'                                                
         ASPEC H08,016,C'------'                                                
         DC    X'00'               END MARKER FOR ASPECS                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003REREP1701 03/12/90'                                      
         END                                                                    
