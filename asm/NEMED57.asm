*          DATA SET NEMED57    AT LEVEL 005 AS OF 08/10/00                      
*PHASE T31E57A                                                                  
         TITLE 'T31E57 - SPECS FOR BUDGET COMPARISON'                           
T31E57   CSECT                                                                  
         PRINT NOGEN                                                            
         SSPEC H1,1,C'MEDIA     NETWORK T.V.'                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,41,C' WEEKLY PERFORMANCE REPORT'                              
         SSPEC H2,42,25C'-'                                                     
         SSPEC H1,76,AGYNAME                                                    
         SSPEC H2,76,AGYADD                                                     
         SSPEC H4,1,C'CLIENT'                                                   
         SSPEC H4,41,PERIOD                                                     
         SSPEC H4,76,NETREP                                                     
         SSPEC H5,1,C'PRODUCT'                                                  
         SSPEC H6,1,C'ESTIMATE'                                                 
         SSPEC H5,76,C'NETWORK -'                                               
         SSPEC H5,101,PAGE                                                      
         SSPEC H6,76,C'DAYPART -'                                               
         SSPEC H9,16,C'------GOAL-------   ---------ESTIMATED---------'         
         SSPEC H10,16,C'DOLLARS    TARGET   DOLLARS    TARGET     HOME'         
         SSPEC H10,62,C'S'                                                      
         SSPEC H11,16,C'         GRPS  CPP           GRPS  CPP  GRPS  '         
         SSPEC H9,64,C'   -----------ACTUAL----------  '                        
         SSPEC H10,64,C'   DOLLARS    TARGET     HOMES  '                       
         SSPEC H11,62,C'CPP           GRPS  CPP  GRPS  CPP'                     
         SSPEC H9,97,C'---INDICES---'                                           
         SSPEC H10,97,C'GOAL GOAL EST '                                         
         SSPEC H11,97,C'/EST /ACT /ACT'                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMED57   08/10/00'                                      
         END                                                                    
