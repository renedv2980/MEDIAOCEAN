*          DATA SET ACREP7801  AT LEVEL 006 AS OF 08/16/00                      
*PHASE AC7801A                                                                  
         TITLE 'SPECS FOR MEDIA CATEGORY LISTING'                               
AC7801   CSECT                                                                  
         PRINT NOGEN                                                            
         ASPEC H1,2,RUN                                                         
         ASPEC H1,48,C'MEDIA CATEGORY LISTING'                                  
         ASPEC H2,48,22C'-'                                                     
         ASPEC H1,86,REPORT                                                     
         ASPEC H1,99,PAGE                                                       
         ASPEC H4,2,COMPANY                                                     
         ASPEC H4,86,REQUESTOR                                                  
         ASPEC H7,13,C'---------MEDIA------------'                              
         ASPEC H8,13,C'CODE           DESCRIPTION'                              
*&&UK*&& ASPEC H7,45,C'---------BILLING INFORMATION--------'                    
*&&UK*&& ASPEC H8,45,C'COMMISSION ACCOUNT       VAT ACCOUNT'                    
*&&US*&& ASPEC H7,45,C'COMMISSION ACCOUNT'                                      
*&&US*&& ASPEC H8,45,C'------------------'                                      
*&&UK                                                                           
         ASPEC H7,83,C'BILL RESET'                                              
         ASPEC H8,83,C'  NUMBER'                                                
         ASPEC H7,94,C'ANALYSIS'                                                
         ASPEC H8,96,C'CODE'                                                    
*&&                                                                             
*&&US                                                                           
         ASPEC H7,65,C'BILL RESET'                                              
         ASPEC H8,65,C'  NUMBER'                                                
         ASPEC H7,76,C'ANALYSIS'                                                
         ASPEC H8,78,C'CODE'                                                    
*&&                                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006ACREP7801 08/16/00'                                      
         END                                                                    
