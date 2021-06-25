*          DATA SET PPREP5401  AT LEVEL 015 AS OF 06/06/91                      
*PHASE PP5401A,+0                                                               
         TITLE 'PP5401 EST SUMMARY SPECS'                                       
PP5401   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         FSPEC READ,BUYS                                                        
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         RSPEC SINGLE,SPACING                                                   
         SPROG 0,10                                                             
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,51,C'ESTIMATE SUMMARY'                                        
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,51,C'----------------'                                        
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,1,CLIENT                                                      
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,1,PRODUCT                                                     
         PSPEC H5,98,RUN                                                        
         PSPEC H7,95,C'SUMMARY         DETAIL'                                  
         PSPEC H8,1,C'PERIOD'                                                   
         PSPEC H8,80,C'CLEARED         BILLED         BILLED'                   
         PSPEC H8,124,C'BILLABLE'                                               
         PSPEC H9,1,C'------'                                                   
         PSPEC H9,32,C'-------------     --------  -------------'               
         PSPEC H9,80,C'-------         ------         ------'                   
         PSPEC H9,124,C'--------'                                               
         PSPEC H7,17,C'INSERT MONTHS   BILL MONTHS'                             
         PSPEC H9,17,C'-------------'                                           
         SPROG 0                                                                
         PSPEC H7,52,C'CASH     GROSS LESS'                                     
         PSPEC H8,32,C'GROSS ORDERED     DISCOUNT  CASH DISCOUNT'               
         PSPEC H8,17,C'GROSS ORDERED'                                           
         SPROG 10                                                               
         PSPEC H7,52,C'CASH       NET LESS'                                     
         PSPEC H8,32,C'  NET ORDERED     DISCOUNT  CASH DISCOUNT'               
         PSPEC H8,17,C'  NET ORDERED'                                           
         PSPEC H7,82,C'NET'                                                     
         PSPEC H6,97,C'NET'                                                     
         PSPEC H6,113,C'NET'                                                    
         PSPEC H7,126,C'NET'                                                    
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015PPREP5401 06/06/91'                                      
         END                                                                    
