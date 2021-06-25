*          DATA SET SPREPMB01  AT LEVEL 005 AS OF 08/29/00                      
*PHASE SPMB01A                                                                  
         TITLE 'SPMB01 - SPECS FOR SOB REPORT'                                  
SPMB01   CSECT                                                                  
         PRINT NOGEN                                                            
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
         SSPEC H6,100,C'(G=GOAL P=PURCHASED)'                                   
         SSPEC H1,59,C'STATUS OF EXPENDITURES'                                  
         SSPEC H2,59,22C'-'                                                     
         SSPEC H4,54,PERIOD                                                     
         SSPEC H10,120,C' GOAL  BUDGET'                                         
         SSPEC H11,120,C'PURCH   TOTAL'                                         
         SSPEC H12,120,C'TOTAL   DIFF.'                                         
         SPROG 1                                                                
         SSPEC H10,1,C'MARKET CODE AND NAME'                                    
         SSPEC H11,1,20C'-'                                                     
         SPROG 2                                                                
         SSPEC H6,64,C'(CLIENT RECAP)'                                          
         SSPEC H10,1,C'PRODUCT NAME'                                            
         SSPEC H11,1,C'------------'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREPMB01 08/29/00'                                      
         END                                                                    
