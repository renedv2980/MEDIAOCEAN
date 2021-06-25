*          DATA SET PPREPCL01  AT LEVEL 001 AS OF 04/01/98                      
*PHASE PPCL01A,+0                                                               
         TITLE 'PPCL01 - COLGATE (Y/R) EXTRACT - HEADLINES'                     
PPCL01   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC LINEUP PATTERN                                                   
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,90                                                          
         PSPEC H1,47,CL26'BILLING INTERFACE CREATION'                           
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,47,26C'-'                                                     
         PSPEC H2,95,AGYADD                                                     
         PSPEC H3,50,PERIOD                                                     
         PSPEC H3,95,RUN                                                        
         PSPEC H4,95,REPORT                                                     
         PSPEC H4,120,PAGE                                                      
         SPROG 0                                                                
         PSPEC H3,05,CL21'RECORDS INPUT TO SORT'                                
         PSPEC H4,05,21C'-'                                                     
         SPROG 10                                                               
         PSPEC H3,05,CL21'*FINAL OUTPUT LISTING'                                
         PSPEC H4,05,21C'-'                                                     
         SPROG 90                                                               
         PSPEC H6,8,C'RECORD TYPE            TOTAL'                             
         PSPEC H7,8,C'-----------            -----'                             
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001PPREPCL01 04/01/98'                                      
         END                                                                    
