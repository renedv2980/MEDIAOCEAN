*          DATA SET NEMED5A    AT LEVEL 002 AS OF 08/10/00                      
*PHASE T31E5AA                                                                  
         TITLE 'T31E5A - SPECS FOR BRAND ALLOCATION '                           
T31E5A   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,41,C'BRAND ALLOCATION FLOWCHART'                              
         SSPEC H2,41,26C'-'                                                     
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,76,NETREP                                                     
         SSPEC H5,104,PAGE                                                      
         SSPEC H10,1,C'LIN PROGRAM NAME    LEN SHARE AVE DAY TIME'              
         SSPEC H11,1,C'--- ------- ----    --- ----- RTG --- ----'              
         SSPEC H10,50,C'UNITS'                                                  
         SSPEC H11,50,C'-----'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED5A   08/10/00'                                      
         END                                                                    
