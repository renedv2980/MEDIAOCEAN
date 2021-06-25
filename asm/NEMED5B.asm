*          DATA SET NEMED5B    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E5BA                                                                  
         TITLE 'T31E5B - SPECS FOR MONTHLY SUMMARIES'                           
T31E5B   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,77,NETREP                                                     
         SSPEC H5,102,PAGE                                                      
         SPROG 1                                                                
         SSPEC H11,34,C'COST UNITS HOME  HOMES TARGET'                          
         SSPEC H12,34,C'           GRPS  (000)  (000)'                          
         SPROG 2                                                                
         SSPEC H10,30,C'AVE SHR AVE UNITS ---HOMES---  ----XXXXXXX----'         
         SSPEC H11,30,C'HUT --- RTG ----- IMPS.  GRPS  VPH  IMPS  GRPS'         
         SPROG 3                                                                
         SSPEC H10,49,C'UNITS'                                                  
         SSPEC H11,49,C'-----'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED5B   08/10/00'                                      
         END                                                                    
