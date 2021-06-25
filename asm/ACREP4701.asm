*          DATA SET ACREP4701  AT LEVEL 009 AS OF 08/16/00                      
*PHASE AC4701A                                                                  
         TITLE 'SPECS FOR RETAIL DISTRIBUTION'                                  
AC4701   CSECT                                                                  
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         SPROG 0                                                                
         ASPEC H1,43,C'BILLS FOR DISTRIBUTION'                                  
         ASPEC H2,43,22C'-'                                                     
         SPROG 0,1,2                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,85,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,85,REQUESTOR                                                  
         SPROG 0,1                                                              
         ASPEC H8,53,C'BILLING DETAILS'                                         
         ASPEC H8,31,22C'-'                                                     
         ASPEC H8,68,21C'-'                                                     
         ASPEC H9,31,C'MEDIA PRODUCT EST    MONTH'                              
         ASPEC H10,32,C'CODE  CODE   NO.   SERVICE'                             
         ASPEC H9,59,C'INVOICE    INVOICE    INVOICE'                           
         ASPEC H10,60,C'NUMBER     DATE      AMOUNT'                            
         SPROG 1                                                                
         ASPEC H1,41,C'BILLING DISTRIBUTION REPORT'                             
         ASPEC H2,41,27C'-'                                                     
         ASPEC H8,2,C'STORE  STORE NAME'                                        
         ASPEC H8,97,C'STORE'                                                   
         ASPEC H8,90,7C'-'                                                      
         ASPEC H8,102,7C'-'                                                     
         ASPEC H9,2,C'NUMBER'                                                   
         ASPEC H9,90,C'PERCENT    AMOUNT'                                       
         SPROG 2                                                                
         ASPEC H1,41,C'BILLING DISTRIBUTION SUMMARY'                            
         ASPEC H2,41,28C'-'                                                     
         ASPEC H8,32,C'NETWORK   NETWORK     SPOT      SPOT'                    
         ASPEC H8,83,C'NEWS-'                                                   
         ASPEC H9,1,C'MARKET NUMBER AND NAME'                                   
         ASPEC H9,34,C'TV       RADIO       TV       RADIO  MAGAZINES'          
         ASPEC H9,83,C'PAPERS    OTHERS    TOTAL'                               
         ASPEC H10,1,22C'-'                                                     
         ASPEC H10,31,9C'-'                                                     
         ASPEC H10,41,9C'-'                                                     
         ASPEC H10,51,9C'-'                                                     
         ASPEC H10,61,9C'-'                                                     
         ASPEC H10,71,9C'-'                                                     
         ASPEC H10,81,9C'-'                                                     
         ASPEC H10,91,9C'-'                                                     
         ASPEC H10,101,9C'-'                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACREP4701 08/16/00'                                      
         END                                                                    
