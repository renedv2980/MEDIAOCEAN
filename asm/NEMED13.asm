*          DATA SET NEMED13    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E13A                                                                  
         TITLE 'T31E13 - SPECS FOR PACKAGE LIST'                                
T31E13   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,75,AGYNAME                                                    
         SSPEC H2,75,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,75,NETREP                                                     
         SSPEC H4,102,PAGE                                                      
         SSPEC H5,75,C'DAYPART'                                                 
         SSPEC H10,1,C'NETWORK  ESTIMATE  PACKAGE   PACKAGE NAME'               
         SSPEC H11,1,C'-------   NUMBER    NUMBER   ------------'               
         SSPEC H10,48,C'DAYPART    PACKAGE   STATUS'                            
         SSPEC H11,48,C'-------    COST($)   ------'                            
         SSPEC H10,80,C' FEED   UNIVERSE   INTEG.'                              
         SSPEC H11,80,C'PERCENT --------   AMOUNT'                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED13   08/10/00'                                      
         END                                                                    
