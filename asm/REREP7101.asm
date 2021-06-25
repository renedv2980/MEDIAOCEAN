*          DATA SET REREP7101  AT LEVEL 008 AS OF 12/08/95                      
*PHASE RE7101A,*                                                                
         TITLE 'REREP7101 (RE7101) --- SPECS FOR OWNER LISTING'                 
*                                                                               
*********************************************************************           
*                                                                   *           
*   REREP7101 - RE7101 - SPECS FOR OWNER LISTING                    *           
*                                                                   *           
*-------------------------------------------------------------------*           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JAN06/94 (SKU) --- INITIAL ENTRY                                  *           
*                                                                   *           
*********************************************************************           
*                                                                               
         PRINT NOGEN                                                            
RE7101   CSECT                                                                  
                                                                                
         SPROG 0,1                                                              
         ASPEC H01,002,REP1                                                     
         ASPEC H01,060,C'OWNER LISTING'                                         
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,RUN                                                      
         ASPEC H02,059,15C'-'                                                   
         ASPEC H02,100,REQUESTOR                                                
         ASPEC H03,060,C'CODE SEQUENCE'                                         
                                                                                
         SPROG 2,3                                                              
         ASPEC H01,002,REP1                                                     
         ASPEC H01,060,C'OWNER LISTING'                                         
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         ASPEC H02,002,RUN                                                      
         ASPEC H02,059,15C'-'                                                   
         ASPEC H02,100,REQUESTOR                                                
         ASPEC H03,060,C'NAME SEQUENCE'                                         
                                                                                
         SPROG 0,2                                                              
         ASPEC H07,002,C'OWNER    NAME'                                         
         ASPEC H08,002,5C'-'                                                    
         ASPEC H08,011,4C'-'                                                    
         ASPEC H07,040,C'OWNER    NAME'                                         
         ASPEC H08,040,5C'-'                                                    
         ASPEC H08,049,4C'-'                                                    
                                                                                
         SPROG 1,3                                                              
         ASPEC H07,002,C'OWNER      NAME'                                       
         ASPEC H07,039,C'STATION'                                               
         ASPEC H07,052,C'MARKET'                                                
         ASPEC H08,002,5C'-'                                                    
         ASPEC H08,013,4C'-'                                                    
         ASPEC H08,039,7C'-'                                                    
         ASPEC H08,052,6C'-'                                                    
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008REREP7101 12/08/95'                                      
         END                                                                    
