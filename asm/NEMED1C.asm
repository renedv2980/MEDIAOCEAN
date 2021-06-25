*          DATA SET NEMED1C    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E1CA                                                                  
         TITLE 'T31E1C - SPECS FOR FIS'                                         
T31E1C   CSECT                                                                  
         PRINT NOGEN                                                            
         SPROG 0,1,2                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,36,C'FIS'                                                     
         SSPEC H2,36,C'---'                                                     
         SSPEC H1,57,NETREP                                                     
         SSPEC H2,57,PAGE                                                       
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H7,1,C'OPTIONS'                                                  
         SSPEC H4,57,PERIOD                                                     
         SSPEC H5,57,C'NETWORK'                                                 
         SSPEC H6,57,C'DAYPART'                                                 
         SSPEC H7,57,C'PACKAGE'                                                 
         SSPEC H10,1,C'MONTH  PERCENT     ORDERED      CLEARED'                 
         SSPEC H11,1,C'-----  CLEARED     -------      -------'                 
         SSPEC H10,44,C'UNCLEARED       BILLED     BILLABLE'                    
         SSPEC H11,44,C'---------       ------     --------'                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED1C   08/10/00'                                      
         END                                                                    
