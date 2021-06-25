*          DATA SET PPREPL201  AT LEVEL 027 AS OF 06/10/91                      
*PHASE PPL201A,+0                                                               
         TITLE 'PPL201 EST SUMMARY SPECS'                                       
PPL201   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         FSPEC READ,BUYS                                                        
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         RSPEC SINGLE,SPACING                                                   
         SPROG 0,10,20,30                                                       
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,51,C'ESTIMATE SUMMARY'                                        
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,51,C'----------------'                                        
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,1,CLIENT                                                      
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         PSPEC H8,1,C'PERIOD'                                                   
         PSPEC H8,62,C'CLEARED    B4 BILLED    B5 BILLED'                       
         PSPEC H8,99,C'B6 BILLED    B7 BILLED'                                  
         PSPEC H8,125,C'BILLABLE'                                               
         PSPEC H9,1,C'------'                                                   
         PSPEC H9,22,C'-----------  --------   ----------'                      
         PSPEC H9,62,C'-------    ---------    ---------'                       
         PSPEC H9,99,C'---------    ---------'                                  
         PSPEC H9,125,C'--------'                                               
         PSPEC H7,9,C'INSERT MNTS  BILL MONTHS'                                 
         PSPEC H9,9,C'-----------'                                              
         SPROG 0,10                                                             
         PSPEC H5,1,PRODUCT                                                     
         SPROG 0,20                                                             
         PSPEC H7,37,C'CASH     GROSS LESS'                                     
         PSPEC H8,22,C'GROSS ORDER  DISCOUNT   CASH DSCNT'                      
         PSPEC H8,9,C'GROSS ORDER'                                              
         SPROG 10,30                                                            
         PSPEC H7,37,C'CASH      NET LESS '                                     
         PSPEC H8,22,C'  NET ORDER  DISCOUNT   CASH DSCNT'                      
         PSPEC H8,9,C'  NET ORDER'                                              
         PSPEC H7,64,C'NET'                                                     
         PSPEC H7,76,C'NET'                                                     
         PSPEC H7,89,C'NET'                                                     
         PSPEC H7,102,C'NET'                                                    
         PSPEC H7,115,C'NET'                                                    
         PSPEC H7,125,C'NET'                                                    
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'027PPREPL201 06/10/91'                                      
         END                                                                    
