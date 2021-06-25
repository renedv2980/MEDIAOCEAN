*          DATA SET SPREPA301  AT LEVEL 029 AS OF 08/29/00                      
*PHASE SPA301A                                                                  
         TITLE 'SPA301 - SPOTPAK CLIENT SUMMARIES - SPECS'                      
SPA301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,GOALS                                                       
         FSPEC GET,MARKET                                                       
*                                                                               
         SPROG 0,1,2,3,4,5,6,7,8                                                
*                                                                               
         SSPEC H1,54,C'C L I E N T  S U M M A R Y'                              
         SSPEC H2,54,C'--------------------------'                              
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,CLIENT                                                      
         SSPEC H3,1,PRDGRP                                                      
         SSPEC H3,51,PERIOD                                                     
         SSPEC H6,51,MKTGRP                                                     
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,REQUESTOR                                                 
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SPROG 1,8                                                              
         SSPEC H5,1,ESTIMATE                                                    
*                                                                               
         SPROG 2,3                                                              
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
*                                                                               
         SPROG 4,5                                                              
         SSPEC H5,1,MARKET                                                      
*                                                                               
         SPROG 6                                                                
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,MARKET                                                      
*                                                                               
         SPROG 8                                                                
         SSPEC H6,1,MARKET                                                      
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'3001ABILL/PAY DATES'                                        
         DC    CL25'3101ADOWNLOAD'                                              
         DC    CL25'3401APRINT COMMENTS'                                        
         DC    CL25'5012AOTHER'                                                 
         DC    CL25'6201AGROSS/NET DOLLARS'                                     
         DC    CL25'6301ADETAIL FORMAT'                                         
         DC    CL25'6601ADOLLAR FORMAT'                                         
         DC    CL25'6401APERIOD FORMAT'                                         
         DC    CL25'6501AANALYSIS TYPE'                                         
         DC    CL25'6701AMONTH OPTION'                                          
         DC    X'00'                                                            
 END                                                                            
