*          DATA SET SPREPA601  AT LEVEL 031 AS OF 08/29/00                      
*PHASE SPA601A                                                                  
         TITLE 'SPREPA601 - STATION SUMMARY SPECS'                              
         PRINT NOGEN                                                            
SPA601   CSECT                                                                  
         FSPEC USE,SPA603                                                       
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,41,C'S T A T I O N   S U M M A R Y'                           
         SSPEC H1,77,AGYNAME                                                    
*                                                                               
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,41,C'-----------------------------'                           
         SSPEC H2,77,AGYADD                                                     
*                                                                               
         SSPEC H4,1,MGROUP         THIS PRINTS ON 4/5/6/7                       
         SSPEC H4,77,REPORT                                                     
         SSPEC H4,100,PAGE                                                      
*NOTE*   SSPEC H4,40,'* CLIENT XXX ONLY *'                                      
*NOTE*   SSPEC H4,40,'* CLIENT EXCLUDE CODE = X *'                              
*                                                                               
*NOTE*   SSPEC H5,40,'* UNPAID ITEMS ONLY *'                                    
*NOTE*   SSPEC H5,40,'* UNPAID ITEMS + ORDERED TOTALS *'                        
*                                                                               
*NOTE*   SSPEC H6,40,MARKET                                                     
*NOTE*   SSPEC H6,77,C'PAY REP 999 ABCDEFGHIJKLMNOPQRSTU'                       
*NOTE*   SSPEC H7,1,C'** NBC AFFILIATES ONLY **'                                
*                                                                               
         SSPEC H8,2,C'STATION  CLIENT  PERIOD     SPOTS'                        
         SSPEC H9,2,C'-------  ------  ------     -----'                        
         SSPEC H8,39,C'GROSS ORDERED      NET ORDERED'                          
         SSPEC H9,39,C'-------------      -----------'                          
         SSPEC H8,75,C'NET CLEARED  PCT CLR  TO BE CLEARED'                     
         SSPEC H9,75,C'-----------  -------  -------------'                     
*                                                                               
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'3003NREP NUMBER'                                            
         DC    CL25'3301AREP TYPE'                                              
         DC    CL25'5801AAFFILIATE FILTER'                                      
         DC    CL25'6101ASUMMARIES ONLY'                                        
         DC    CL25'6201APRIOR-SUBSEQUENT'                                      
         DC    CL25'6401AANALYZE BY OFF'                                        
         DC    CL25'6501ASUPPRESS NET'                                          
         DC    CL25'6601ASUPPRESS CLT'                                          
         DC    CL25'6701ATOTAL OPTION'                                          
         DC    CL25'6801AEXCLUDE CODE'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031SPREPA601 08/29/00'                                      
         END                                                                    
