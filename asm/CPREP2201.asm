*          DATA SET CPREP2201  AT LEVEL 006 AS OF 09/01/00                      
*PHASE CP2201A                                                                  
         TITLE 'CPREP2201-CPP MONTHLY TREND REPORT'                             
         PRINT NOGEN                                                            
CP2201   CSECT                                                                  
         SPROG 0,THRU,8                                                         
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,77,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,77,AGYADD                                                     
         PSPEC H1,45,C'TREND REPORT'                                            
         PSPEC H2,45,C'------------'                                            
         PSPEC H3,1,RANGE                                                       
         PSPEC H4,42,PERIOD                                                     
         PSPEC H4,77,REPORT                                                     
         PSPEC H4,103,PAGE                                                      
         PSPEC H8,1,C'MARKET'                                                   
         PSPEC H9,1,C'------'                                                   
         PSPEC H8,26,C'DPT-LN'                                                  
         PSPEC H9,26,C'------'                                                  
         PSPEC H8,34,C'DATA'                                                    
         PSPEC H9,34,C'----'                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CPREP2201 09/01/00'                                      
         END                                                                    
