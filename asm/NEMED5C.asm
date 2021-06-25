*          DATA SET NEMED5C    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E5CA                                                                  
         TITLE 'T31E5C - SPECS FOR NETWORK SCHEDULE'                            
T31E5C   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,46,C'NETWORK SCHEDULE'                                        
         SSPEC H2,46,16C'-'                                                     
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,75,NETREP                                                     
         SSPEC H5,102,PAGE                                                      
         SSPEC H10,1,C'DATE  PROGRAM NAME   NETWORK  DAY TIME'                  
         SSPEC H11,1,C'----  ------------   -------  --- ----'                  
         SPROG 1                                                                
         SSPEC H10,51,C' LEN    COST    HOMES    HOMES'                         
         SSPEC H11,51,C' ---    ----    (GRP)    (000)'                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED5C   08/10/00'                                      
         END                                                                    
