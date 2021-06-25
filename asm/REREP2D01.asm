*          DATA SET REREP2D01  AT LEVEL 025 AS OF 06/06/97                      
*PHASE RE2D01A,+0                                                               
         TITLE 'SPECS FOR DARE SPOT COUNT REPORT'                               
RE2D01   CSECT                                                                  
*        PRINT NOGEN                                                            
         SPROG 0,1                                                              
         ASPEC H1,001,REP                                                       
         ASPEC H1,100,RENUM                                                     
         ASPEC H2,001,REQUESTOR                                                 
         ASPEC H2,100,RUN                                                       
*                                                                               
         SPROG 0                                                                
         ASPEC H1,48,C'DARE SPOT COUNT REPORT'                                  
         ASPEC H2,48,22C'-'                                                     
         ASPEC H5,001,C'AGENCY'                                                 
         ASPEC H5,011,C'ADV'                                                    
         ASPEC H5,019,C'STATION'                                                
         ASPEC H5,031,C'MONTH'                                                  
         ASPEC H5,053,C'DOLLAR$'                                                
         ASPEC H5,071,C'SPOTS'                                                  
         ASPEC H6,001,76C'-'                                                    
*                                                                               
         SPROG 1                                                                
         ASPEC H1,48,C'DARE SPOT COUNT SUMMARY'                                 
         ASPEC H2,48,23C'-'                                                     
         ASPEC H5,031,C'MONTH'                                                  
         ASPEC H5,053,C'DOLLAR$'                                                
         ASPEC H5,071,C'SPOTS'                                                  
         ASPEC H6,031,45C'-'                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025REREP2D01 06/06/97'                                      
         END                                                                    
