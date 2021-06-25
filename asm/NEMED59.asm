*          DATA SET NEMED59    AT LEVEL 004 AS OF 08/10/00                      
*PHASE T31E59A                                                                  
         TITLE 'T31E59 - SPECS FOR NETWORK ESTIMATE'                            
T31E59   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 1,2                                                              
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,47,C'NETWORK ESTIMATE'                                        
         SSPEC H2,47,C'----------------'                                        
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,76,NETREP                                                     
         SSPEC H5,104,PAGE                                                      
         SSPEC H11,49,C'UNITS'                                                  
         SSPEC H12,49,C'-----'                                                  
         SPROG 1                                                                
         SSPEC H11,1,C'DATE  PROGRAM'                                           
         SSPEC H12,1,C'----  -------'                                           
         SSPEC H11,24,C'DAY TIME       LEN BRAND'                               
         SSPEC H12,24,C'--- ----       --- -----'                               
         SPROG 2                                                                
         SSPEC H11,24,C'NETWORK    MONTH'                                       
         SSPEC H12,24,C'-------    -----'                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004NEMED59   08/10/00'                                      
         END                                                                    
