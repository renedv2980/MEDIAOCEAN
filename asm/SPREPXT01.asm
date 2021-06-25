*          DATA SET SPREPXT01  AT LEVEL 007 AS OF 08/29/00                      
*PHASE SPXT01A                                                                  
SPXT01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         RSPEC LINEUP PATTERN                                                   
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0                                                                
         PSPEC H1,22,CL26'BILLING INTERFACE CREATION'                           
         PSPEC H1,70,AGYNAME                                                    
         PSPEC H2,22,26C'-'                                                     
         PSPEC H2,70,AGYADD                                                     
         PSPEC H3,19,PERIOD                                                     
         PSPEC H4,70,REPORT                                                     
         PSPEC H4,95,PAGE                                                       
         PSPEC H6,8,C'RECORD TYPE            TOTAL'                             
         PSPEC H7,8,C'-----------            -----'                             
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPXT01 08/29/00'                                      
         END                                                                    
