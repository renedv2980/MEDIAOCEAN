*          DATA SET PPREPXT01  AT LEVEL 006 AS OF 07/18/16                      
*PHASE PPXT01A                                                                  
         TITLE 'PPXT01 - STANDARD EXTRACT - HEADLINES'                          
PPXT01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0                                                                
         PSPEC H1,22,CL26'BILLING INTERFACE CREATION'                           
         PSPEC H1,70,AGYNAME                                                    
         PSPEC H2,22,26C'-'                                                     
         PSPEC H2,70,AGYADD                                                     
         PSPEC H3,25,PERIOD                                                     
         PSPEC H3,70,RUN                                                        
         PSPEC H4,70,REPORT                                                     
         PSPEC H4,95,PAGE                                                       
         PSPEC H6,8,C'RECORD TYPE            TOTAL'                             
         PSPEC H7,8,C'-----------            -----'                             
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREPXT01 07/18/16'                                      
         END                                                                    
