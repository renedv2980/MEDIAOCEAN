*          DATA SET SPREPBU01  AT LEVEL 026 AS OF 07/11/16                      
*PHASE SPBU01A                                                                  
         TITLE 'SPBU01 - NETWORK UNIT BILLING SPECS'                            
SPBU01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC READ,BUYS                                                        
*                                                                               
         SPROG 10                  NORMAL                                       
         SPACE 2                                                                
         SPROG 20,21               INVOICE REGISTER                             
         SPACE 2                                                                
         SSPEC H4,1,AGYNAME                                                     
         SSPEC H5,1,AGYADD                                                      
         SSPEC H5,37,REPORT                                                     
         SSPEC H9,1,MEDIA                                                       
         SSPEC H13,132,C'*'                                                     
*                                                                               
         SSPEC M1,27,C'BILL'                                                    
         SSPEC M1,44,C'ACTUAL'                                                  
         SSPEC M1,77,C'AGENCY'                                                  
         SSPEC  M2,2,C'INVOICE  PRD EST  PERIOD TYPE          BILL AMOUX        
               NT        NET COST      COMMISSION        GROSS COST'            
         SPACE 2                                                                
         SPROG 20                  INVOICE REGISTER                             
         SSPEC H4,37,C'NETWORK BILLING INVOICE REGISTER'                        
         SPROG 21                  AOR INVOICE REGISTER                         
         SSPEC H4,35,C'NETWORK AOR BILLING INVOICE REGISTER'                    
         SPACE 2                                                                
         SPROG 100                 DUMMY SPROG TO CATEGORY LINES                
         SPACE 2                                                                
         SSPEC H1,1,CLIENT                                                      
         SSPEC H2,1,PRDGRP                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H7,1,MKTGRP                                                      
         SSPEC H10,1,MARKET                                                     
         SSPEC H11,1,STATION                                                    
         SPACE 2                                                                
*                                                                               
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'3602ADUE DAYS'                                              
         DC    CL25'5012AMANUAL AMOUNT'                                         
         DC    CL25'6201AOPTION 1'                                              
         DC    CL25'6301AOPTION 2'                                              
         DC    CL25'6401AOPTION 3'                                              
         DC    CL25'6501AOPTION 4'                                              
         DC    CL25'6601AOPTION 5'                                              
         DC    X'00'                                                            
*                                                                               
         DC    C'REQ2LST='                                                      
         DC    CL25'2106DINVOICE DATE'                                          
         DC    X'00'                                                            
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPREPBU01 07/11/16'                                      
         END                                                                    
