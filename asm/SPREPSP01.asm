*          DATA SET SPREPSP01  AT LEVEL 013 AS OF 08/29/00                      
*PHASE SPSP01A                                                                  
         TITLE 'STATION FILE PURGE - PRINT SPECS'                               
SPSP01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SPSP03                                                       
         FSPEC UPDATE,STATION                                                   
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,TRAFFIC                                                   
         SPROG 1,2,3                                                            
         SSPEC H1,3,MEDIA                                                       
         SSPEC H1,49,C'STATION FILE ACTIVITY REPORT'                            
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,49,C'------- ---- -------- ------'                            
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,3,PAGE                                                        
         SSPEC H4,47,PERIOD                                                     
         SSPEC H4,98,REPORT                                                     
         SPROG 1                                                                
         SSPEC H4,48,C'INACTIVE STATIONS DENOTED BY *'                          
         SSPEC H5,48,C'-------- -------- ------- -- -'                          
         SPROG 2                                                                
         SSPEC H4,55,C'INACTIVE MARKETS'                                        
         SSPEC H5,55,C'-------- -------'                                        
         SPROG 3                                                                
         SSPEC H4,40,C'INACTIVE TRAFFIC STATION ADDRESSES DENOTED BY *'         
         SSPEC H5,40,C'-------- ------- ------- --------- ------- -- -'         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPREPSP01 08/29/00'                                      
         END                                                                    
