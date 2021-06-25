*          DATA SET SPREPM801  AT LEVEL 017 AS OF 07/12/94                      
*PHASE SPM801A                                                                  
         TITLE 'PRINT SPECS FOR MARKET MEDIA PLAN'                              
SPM801   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,1,2,3                                                          
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,50,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H4,1,PGROUP                                                      
         SSPEC H4,100,PAGE                                                      
         SSPEC H6,100,REPORT                                                    
         SPROG 1                                                                
         SSPEC H1,58,C'MARKET MEDIA PLAN'                                       
         SSPEC H4,50,MGROUP                                                     
         SPROG 2                                                                
         SSPEC H1,50,C'MEDIA PLAN MARKET GROUP SUMMARY'                         
         SSPEC H4,50,MGROUP                                                     
         SPROG 3                                                                
         SSPEC H1,54,C'MEDIA PLAN CLIENT SUMMARY'                               
         SSPEC H4,50,MGROUP                                                     
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'5001ASUPPRESS SPILL'                                        
         DC    CL25'5801AAFFILIATE'                                             
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017SPREPM801 07/12/94'                                      
         END                                                                    
