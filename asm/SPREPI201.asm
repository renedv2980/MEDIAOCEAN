*          DATA SET SPREPI201  AT LEVEL 020 AS OF 11/16/06                      
*PHASE SPI201A                                                                  
         TITLE 'SPI201 - INVOICE MATCHING REPORT SPECS'                         
SPI201   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SPI203                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
*        SPROG USAGE - 0 =MATCHES, NO DEMOS                                     
*                    - 1 =MATCHES, WITH DEMOS                                   
*                    - 10=SUMMARIES, WITH COSTS                                 
*                    - 11=SUMMARIES, NO COSTS                                   
*                    - 12=SUMMARIES, WITH COSTS, WITH RESPONSES                 
*                    - 13=SUMMARIES, NO COSTS, WITH RESPONSES                   
*                    ADD 50 FOR NETPAK INSTEAD OF SPOTPAK                       
*                                                                               
         SPROG 0,1,10,11,12,13,50,51,60,61,62,63                                
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H3,1,CLIENT                                                      
*                                                                               
         SSPEC H1,55,C'INVOICE MATCHING REPORT'                                 
         SSPEC H2,55,C'-----------------------'                                 
*                                                                               
         SSPEC H1,97,AGYNAME                                                    
         SSPEC H2,97,AGYADD                                                     
         SSPEC H4,97,PAGE                                                       
         SSPEC H5,97,REPORT                                                     
*                                                                               
         SPROG 0,1,50,51           MATCHES ONLY - NOT SUMMARY                   
*                                                                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,PRODUCT                                                     
         SSPEC H5,1,ESTIMATE                                                    
         SSPEC H7,1,MKTGRP                                                      
*                                                                               
         SSPEC H3,50,PERIOD                                                     
         SSPEC H6,50,STATION                                                    
*                                                                               
         SPROG 0,1                 MARKET FOR SPOT ONLY                         
         SSPEC H5,50,MARKET                                                     
*                                                                               
         SPROG 50,51               NETPAK                                       
         SSPEC H6,50,C'NETWORK'                                                 
*                                                                               
         SPROG 1,51                                                             
         SSPEC H6,97,RATING                                                     
         SSPEC H7,97,BOOK                                                       
*                                                                               
*                                  SUMMARIES                                    
         SPROG 10,12,60,62         WITH COSTS                                   
         SSPEC H7,86,C'---------O R D E R E D---------'                         
         SSPEC H8,1,C'PERIOD         STATION   M A R K E T             X        
                    PRODUCT     EST     STATUS     SPOTS   GROSS COST  X        
                  NET COST    PAID    PAGE'                                     
         SSPEC H9,1,C'------         -------  ------------             X        
                    -------     ---     ------     -----   ----------  X        
                  --------    ----    ----'                                     
*                                                                               
         SPROG 11,13,61,63         WITHOUT COSTS                                
         SSPEC H8,1,C'PERIOD         STATION   M A R K E T             X        
                    PRODUCT     EST     STATUS     SPOTS               X        
                              PAID    PAGE'                                     
         SSPEC H9,1,C'------         -------  ------------             X        
                    -------     ---     ------     -----               X        
                              ----    ----'                                     
*                                                                               
         SPROG 12,13,62,63         WITH RESPONSES                               
         SSPEC H8,119,C'RESPONSES'                                              
         SSPEC H9,119,C'---------'                                              
*                                                                               
         SPROG 60,61,62,63         NETPAK SUMMARIES                             
         SSPEC H8,17,C'NETWORK'                                                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'3003AREP'                                                   
         DC    CL25'3503APIGGYBACK PRODUCT'                                     
         DC    CL25'5004ARATING BOOK'                                           
         DC    CL25'5804APARTIAL MATCH OPTION'                                  
         DC    CL25'6201AFILM REPORT OPTION'                                    
         DC    CL25'6301ASORT BY EST-LIN'                                       
         DC    CL25'6401ASPACING CONTROL'                                       
         DC    CL25'6501APRINTING OPTION'                                       
         DC    CL25'6601AAFFID POSTING OPTION'                                  
         DC    CL25'6701ADAYPART FILTER'                                        
         DC    X'00'                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPREPI201 11/16/06'                                      
         END                                                                    
