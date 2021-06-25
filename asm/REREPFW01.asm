*          DATA SET REREPFW01  AT LEVEL 015 AS OF 09/23/03                      
*PHASE REFW01A,*                                                                
         TITLE 'SPECS FOR NEW REP UNIQUE ID REPORT     '                        
*                                                                               
*- REREPFW01 -- PHASE REFW01 -- SPECS MODULE FOR REP UNIQUE ID REPORT           
*                                                                               
*  MOD LOG                                                                      
*  -------                                                                      
*  09/23/03 SKU NEW                                                             
*                                                                               
REFW01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0                                                                
         ASPEC H1,2,RUN                                                         
         ASPEC H01,002,REP                                                      
         ASPEC H01,035,C'UNIQUE ID REPORT: DDS VS MSTREET'                      
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,100,RUN                                                      
         SPACE 1                                                                
         ASPEC H03,002,C'DESCRIPTION'                                           
*                        3         4         5         6                        
*                        5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.                     
         ASPEC H03,035,C'REP --ID--    DDS       MSTREET'                       
*                        7         8         9         0                        
*                        1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.                 
         ASPEC H03,071,C'    HISTORY 1           HISTORY 2'                     
         SPACE 1                                                                
*                        3         4         5         6                        
*                        5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.                     
         ASPEC H04,035,C'            STATION     STATION'                       
*                        7         8         9         0                        
*                        1.3.5.7.9.1.3.5.7.9.1.3.5.7.9.1.3.5.7.                 
         ASPEC H04,071,C' STATION   DATE       STATION   DATE'                  
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015REREPFW01 09/23/03'                                      
         END                                                                    
