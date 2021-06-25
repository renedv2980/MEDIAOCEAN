*          DATA SET SPREPMK01  AT LEVEL 012 AS OF 08/29/00                      
*PHASE SPMK01A                                                                  
         TITLE 'SPMK01 - PRINT SPECS FOR KRAFT PRODUCT SUMMARY'                 
         PRINT NOGEN                                                            
SPMK01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,1,PGROUP                                                      
         SSPEC H4,100,PAGE                                                      
         SSPEC H5,100,REPORT                                                    
         SSPEC H3,50,PERIOD                                                     
         SSPEC H4,50,MGROUP                                                     
         SSPEC H10,1,C'WEEK   ------------GOAL GRPS------------'                
         SSPEC H11,1,C'----   DAY  EFR  LFR  ACC  LNW  PRI  TOT'                
         SSPEC H10,67,C'WEEK   ------------GOAL GRPS------------'               
         SSPEC H11,67,C'----   DAY  EFR  LFR  ACC  LNW  PRI  TOT'               
         SPROG 1                                                                
         SSPEC H10,42,C'PRCH     ---DOLLARS---'                                 
         SSPEC H11,42,C'GRPS     GOAL     PRCH'                                 
         SSPEC H10,108,C'PRCH     ---DOLLARS---'                                
         SSPEC H11,108,C'GRPS     GOAL     PRCH'                                
         SPROG 1                                                                
         SSPEC H1,54,C'STATUS OF SCHEDULE REPORT'                               
         SSPEC H2,54,25C'-'                                                     
         SPROG 2                                                                
         SSPEC H1,55,C'KRAFT PRODUCT SCHEDULE'                                  
         SSPEC H2,55,22C'-'                                                     
         SPROG 3                                                                
         SSPEC H1,55,C'KRAFT PURCHASED REPORT'                                  
         SSPEC H2,55,22C'-'                                                     
         SPROG 2                                                                
         SSPEC H10,48,C' GOAL  '                                                
         SSPEC H11,48,C'DOLLARS'                                                
         SSPEC H10,114,C' GOAL  '                                               
         SSPEC H11,114,C'DOLLARS'                                               
         SPROG 3                                                                
         SSPEC H10,20,C'PRCH'                                                   
         SSPEC H10,86,C'PRCH'                                                   
         SSPEC H10,48,C' PRCH  '                                                
         SSPEC H11,48,C'DOLLARS'                                                
         SSPEC H10,114,C' PRCH  '                                               
         SSPEC H11,114,C'DOLLARS'                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPMK01 08/29/00'                                      
         END                                                                    
