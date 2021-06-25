*          DATA SET PPREPAE01  AT LEVEL 002 AS OF 08/07/13                      
*PHASE PPAE01A                                                                  
         TITLE 'PPAE01 - PRODUCTION XML TRANSMISSION'                           
PPAE01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         SPROG 0,2                                                              
         PSPEC H1,45,C'PRODUCTION XML TRANSMISSION'                             
         PSPEC H2,45,C'---------------------------'                             
*                                                                               
         PSPEC H2,98,RUN                                                        
         PSPEC H3,98,C'AE REPORT'                                               
         PSPEC H4,98,PAGE                                                       
*                                                                               
         PSPEC H6,21,C'INVOICES'                                                
         PSPEC H6,36,C'BILLED AMOUNT'                                           
         PSPEC H7,21,C'--------'                                                
         PSPEC H7,36,C'-------------'                                           
         SPROG 0                                                                
         PSPEC H6,04,C'SUPPLIER ID'                                             
         PSPEC H7,04,C'-----------'                                             
         SPROG 2                                                                
         PSPEC H6,51,C'GST/HST TAXES'                                           
         PSPEC H7,51,C'-------------'                                           
         PSPEC H6,66,C'AMOUNT DUE'                                              
         PSPEC H7,66,C'-----------'                                             
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPAE01 08/07/13'                                      
         END                                                                    
