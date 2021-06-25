*          DATA SET SPREPMD01  AT LEVEL 005 AS OF 08/29/00                      
*PHASE SPMD01A                                                                  
         TITLE 'SPMD01 - SPOTPAK MARKET COMMERCIAL PERFORMANCE SPECS'           
SPMD01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
         SPROG 0,1                                                              
*                                                                               
         SSPEC H1,49,C'MARKET COMMERCIAL PERFORMANCE REPORT'                    
         SSPEC H2,49,C'------------------------------------'                    
         SSPEC H3,50,PERIOD                                                     
*                                                                               
         SSPEC H1,1,AGYNAME                                                     
         SSPEC H2,1,AGYADD                                                      
         SSPEC H1,100,REPORT                                                    
         SSPEC H2,100,PAGE                                                      
         SSPEC H2,111,REQUESTOR                                                 
*                                                                               
         SPROG 0                                                                
*                                                                               
         SSPEC H5,49,C'BUY RECORDS WITH UNKNOWN COMMERCIALS'                    
         SSPEC H6,49,C'------------------------------------'                    
         SSPEC H8,49,C'CLT  PRD  MARKET  STATION  EST  LINE'                    
         SSPEC H9,49,C'---  ---  ------  -------  ---  ----'                    
*                                                                               
         SPROG 1                                                                
*                                                                               
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRDGRP                                                      
         SSPEC H7,1,PRODUCT                                                     
         SSPEC H8,1,ESTIMATE                                                    
         SSPEC H5,53,MGROUP                                                     
*                                                                               
         SSPEC H4,100,RATING                                                    
         SSPEC H5,100,BOOK                                                      
         SSPEC H6,100,EQUIV                                                     
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'6401ASUMMARY OPTION'                                        
         DC    CL25'6501AUNKNOWN CML DETAIL'                                    
         DC    CL25'6601ADOWNLOAD'                                              
         DC    CL25'6801AFILM FILTER'                                           
         DC    X'00'                                                            
*                                                                               
 END                                                                            
