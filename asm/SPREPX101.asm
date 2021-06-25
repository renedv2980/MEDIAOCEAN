*          DATA SET SPREPX101  AT LEVEL 047 AS OF 08/29/00                      
*PHASE SPX101A                                                                  
         TITLE 'SPX101 - COKE BRDCAST PLANNING BUDGET RPT'                      
SPX101   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
         FSPEC USE,SP0003                                                       
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,2,4                                                            
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,47,C'BROADCAST PLANNING BUDGET REPORT'                        
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,47,C'--------------------------------'                        
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,PAGE                                                      
         SSPEC H4,111,REPORT                                                    
         SSPEC H3,47,PERIOD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SPROG 2                                                                
         SSPEC H8,31,C'********** PLANNED **********'                           
         SSPEC H8,67,C'********* PURCHASED *********'                           
         SSPEC H8,103,C'******** DIFFERENCE *********'                          
         SSPEC H9,31,C'GROSS  COMMISSION         NET'                           
         SSPEC H9,67,C'GROSS  COMMISSION         NET'                           
         SSPEC H9,103,C'GROSS  COMMISSION         NET'                          
         SSPEC H10,31,C'-----  ----------         ---'                          
         SSPEC H10,67,C'-----  ----------         ---'                          
         SSPEC H10,103,C'-----  ----------         ---'                         
         SPROG 4                                                                
         SSPEC H8,31,C'********* PURCHASED *********'                           
         SSPEC H9,31,C'GROSS  COMMISSION         NET'                           
         SSPEC H10,31,C'-----  ----------         ---'                          
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'6201AOPTION 1'                                              
         DC    CL25'6301AOPTION 2'                                              
         DC    CL25'6401AOPTION 3'                                              
         DC    CL25'6501AOPTION 4'                                              
         DC    CL25'6601AOPTION 5'                                              
         DC    CL25'6701AOPTION 6'                                              
         DC    CL25'6801AOPTION 7'                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'047SPREPX101 08/29/00'                                      
         END                                                                    
