*          DATA SET NEMED14    AT LEVEL 009 AS OF 08/10/00                      
*PHASE T31E14A                                                                  
         TITLE 'SPECS FOR NETWORK SCHEDULE EFFICIENCY ANALYSIS REPORT'          
T31E14   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H1,48,C'PROGRAM EFFICIENCY ANALYSIS'                             
         SSPEC H2,48,27C'-'                                                     
         SSPEC H1,96,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,96,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,49,PERIOD                                                     
         SSPEC H4,96,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H5,49,C'PACKAGE='                                                
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H6,96,PAGE                                                       
         SSPEC H7,1,C'DAYPART'                                                  
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009NEMED14   08/10/00'                                      
         END                                                                    
