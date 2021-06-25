*          DATA SET SPREPJ701  AT LEVEL 004 AS OF 02/03/03                      
*PHASE SPJ701A                                                                  
         TITLE 'SPJ701 - P&&G CAMPAIGN SCHEDULE - SPECS'                        
SPJ701   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
                                                                                
         SPROG 0,THRU,99                                                        
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,C'REQUESTOR'                                                
         SSPEC H2,11,REQUESTOR                                                  
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H6,44,MARKET                                                     
*                                                                               
         SSPEC H1,56,CL21'P&&G CAMPAIGN SCHEDULE'                               
         SSPEC H2,56,C'---------------------'                                   
         SSPEC H3,50,PERIOD                                                     
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,REPORT                                                    
         SSPEC H4,123,PAGE                                                      
*                                                                               
         SSPEC H8,1,C'EST-LIN  LEN  DAYS    ---TIME---   '                      
         SSPEC H9,1,C'CONTRACT    DPT  PROGRAMMING       '                      
         SSPEC H10,1,C'===================================='                    
         SSPEC H9,42,C'JAN01 JAN08 JAN15 JAN22 JAN29 FEB04 FEB11'               
         SSPEC H10,42,C'===== ===== ===== ===== ===== ===== ====='              
         SSPEC H9,84,C'FEB18 FEB25 MAR04 MAR11 MAR18 MAR25 APR02'               
         SSPEC H10,84,C'===== ===== ===== ===== ===== ===== ====='              
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPREPJ701 02/03/03'                                      
         END                                                                    
