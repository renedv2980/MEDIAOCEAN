*          DATA SET REREPFZ01  AT LEVEL 011 AS OF 07/16/03                      
*PHASE REFZ01A                                                                  
         TITLE 'SPECS FOR MSTREET COMPARE'                                      
REFZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         ASPEC H01,002,REP                                                      
         ASPEC H01,055,C'MSTREET GENERATIONAL REPORT'                           
         ASPEC H01,100,RENUM                                                    
         ASPEC H01,120,PAGE                                                     
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,057,C'STATION/DDS ID SEQUENCE'                               
         ASPEC H02,100,RUN                                                      
         SPROG 1                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,057,C'DDS ID/STATION SEQUENCE'                               
         ASPEC H02,100,RUN                                                      
         SPROG 2                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,057,C'STATION/UID    SEQUENCE'                               
         ASPEC H02,100,RUN                                                      
         SPROG 3                                                                
         ASPEC H02,002,REQUESTOR                                                
         ASPEC H02,057,C'UID   /STATION SEQUENCE'                               
         ASPEC H02,100,RUN                                                      
         SPROG 0                                                                
         ASPEC H04,001,C'STATN'                                                 
         ASPEC H04,008,C'DDS ID'                                                
         SPROG 1                                                                
         ASPEC H04,001,C'DDS ID'                                                
         ASPEC H04,008,C'STATN'                                                 
         SPROG 2                                                                
         ASPEC H04,001,C'STATN'                                                 
         ASPEC H04,008,C'UID   '                                                
         SPROG 3                                                                
         ASPEC H04,001,C'UID   '                                                
         ASPEC H04,008,C'STATN'                                                 
         SPROG 0,1                                                              
         ASPEC H04,015,C'UID   '                                                
         SPROG 2,3                                                              
         ASPEC H04,015,C'DDS ID'                                                
         SPROG 0,1,2,3                                                          
         ASPEC H04,022,C'FREQ'                                                  
         ASPEC H04,028,C'CITY'                                                  
         ASPEC H04,053,C'ST'                                                    
         ASPEC H04,056,C'FOR'                                                   
         ASPEC H04,060,C'OWNER'                                                 
         ASPEC H04,066,C'REP1'                                                  
         ASPEC H04,071,C'REP2'                                                  
         ASPEC H04,076,C'AM'                                                    
         ASPEC H04,080,C'WARNING'                                               
         ASPEC H04,105,C'COMMENT'                                               
         SPACE 1                                                                
         ASPEC H05,056,C'MAT'                                                   
         ASPEC H05,076,C'CDE'                                                   
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011REREPFZ01 07/16/03'                                      
         END                                                                    
