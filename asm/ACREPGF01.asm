*          DATA SET ACREPGF01  AT LEVEL 018 AS OF 08/17/00                      
*PHASE ACGF01A                                                                  
         TITLE 'KRAFT FOODS BILLING INTERFACE TAPE'                             
ACGF01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 1                                                                
         SPROG 0,1                                                              
         FSPEC READ,ACCOUNTS                                                    
         FSPEC READ,TRANSACTIONS                                                
         ACDEF PRORATA,BILL                                                     
         SPACE 1                                                                
         SPROG 0,1                                                              
         ASPEC H1,2,RUN                                                         
         ASPEC H1,87,REPORT                                                     
         ASPEC H1,102,PAGE                                                      
         ASPEC H1,55,C'KRAFT FOODS INTERFACE'                                   
         ASPEC H2,55,C'---------------------'                                   
         ASPEC H3,2,COMPANY                                                     
         ASPEC H3,87,PERIOD                                                     
         ASPEC H8,2,C'COMPANY'                                                  
         SPACE 1                                                                
         SPROG 0                                                                
         ASPEC H8,10,C'DIVISION'                                                
         ASPEC H8,19,C'PRODUCT'                                                 
         ASPEC H8,27,C'ESTIMATE'                                                
         ASPEC H8,40,C'MONTH'                                                   
         ASPEC H8,47,C'INVOICE'                                                 
         ASPEC H8,57,C'TYPE'                                                    
         ASPEC H8,62,C'NATURAL'                                                 
         ASPEC H8,70,C'SUB-NATURAL'                                             
         ASPEC H8,82,C'  NET AMOUNT'                                            
         ASPEC H8,98,C'  COMMISSION'                                            
         ASPEC H8,115,C' TOTAL AMOUNT'                                          
         SPACE 1                                                                
         SPROG 1                                                                
         ASPEC H8,47,C'  NET AMOUNT'                                            
         ASPEC H8,63,C'  COMMISSION'                                            
         ASPEC H8,80,C' TOTAL AMOUNT'                                           
         SPACE 1                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018ACREPGF01 08/17/00'                                      
         END                                                                    
