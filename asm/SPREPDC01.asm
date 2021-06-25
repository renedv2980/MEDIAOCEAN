*          DATA SET SPREPDC01  AT LEVEL 015 AS OF 08/29/00                      
*PHASE SPDC01A                                                                  
         TITLE 'SPDC01 - MEDIA CALENDAR - PRINT SPECS'                          
SPDC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         SPROG 1,2                                                              
         SSPEC H1,3,MEDIA                                                       
         SSPEC H1,48,C'MEDIA CALENDAR'                                          
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,48,C'--------------'                                          
         SSPEC H2,76,AGYADD                                                     
         SSPEC H3,39,PERIOD                                                     
         SSPEC H4,3,CLIENT                                                      
         SSPEC H4,76,PAGE                                                       
         SSPEC H4,86,REPORT                                                     
         SSPEC H5,3,PRODUCT                                                     
         SSPEC H6,3,ESTIMATE                                                    
         SSPEC H4,3,PGROUP                                                      
         SPACE 1                                                                
         SPROG 1                                                                
         SSPEC H4,45,MGROUP                                                     
         SPACE 1                                                                
         SPROG 2                                                                
         SSPEC H7,3,MGROUP                                                      
         SPROG 3                                                                
         SSPEC H1,3,MEDIA                                                       
         SSPEC H1,42,C'MEDIA CALENDAR RECAP'                                    
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,3,REQUESTOR                                                   
         SSPEC H2,42,C'--------------------'                                    
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,3,CLIENT                                                      
         SSPEC H4,76,PAGE                                                       
         SSPEC H4,86,REPORT                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015SPREPDC01 08/29/00'                                      
         END                                                                    
