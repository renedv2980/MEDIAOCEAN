*          DATA SET ACREPSB01  AT LEVEL 001 AS OF 11/08/16                      
*PHASE ACSB01A,*                                                                
ACSB01   CSECT                                                                  
         PRINT NOGEN                                                            
         ACDEF UPDATE,ACCFIL                                                    
         ACDEF RESET                                                            
         ACDEF WIDTH,198                                                        
*                                                                               
         ACDEF SPROG,0                                                          
         ACDEF H1,2,RUN                                                         
         ACDEF H1,48,C'Cash Receipts import'                                    
         ACDEF H2,48,C'--------------------'                                    
         ACDEF H1,86,REPORT                                                     
         ACDEF H1,99,PAGE                                                       
         ACDEF H2,86,COMPANY                                                    
         ACDEF H3,86,REQUESTOR                                                  
         ACDEF H2,02,C'Clearing Doc # :'                                        
         ACDEF H3,02,C'Doc #          :'                                        
         ACDEF H3,30,C'Company Code   :'                                        
         ACDEF H3,58,C'Fiscal Year    :'                                        
         ACDEF H4,02,C'Check #        :'                                        
         ACDEF H4,30,C'Posting Date   :'                                        
         ACDEF H4,58,C'Check Date     :'                                        
                                                                                
         ACDEF SPROG,1                                                          
         ACDEF H6,3,C'KEY:'                                                     
         ACDEF SPROG,2                                                          
         ACDEF H7,3,C'Voucher'                                                  
         ACDEF H7,18,C'Item_#'                                                  
         ACDEF H7,26,C'Net_Amount'                                              
         ACDEF H7,38,C'Tax_Amount'                                              
         ACDEF H7,50,C'SAP_Vendor'                                              
         ACDEF H7,61,C'SAPSales_ORG'                                            
         ACDEF H7,74,C'Media_Type'                                              
         ACDEF H7,85,C'Med_Sub_Type'                                            
         ACDEF H7,99,C'MOS'                                                     
*                                                                               
         ACDEF SPROG,3                                                          
         ACDEF H6,125,C'Errors'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001ACREPSB01 11/08/16'                                      
         END                                                                    
