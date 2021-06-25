*          DATA SET PPREPSE01  AT LEVEL 025 AS OF 07/18/16                      
*PHASE PPSE01A,+0                                                               
         TITLE 'PPSE01 - SPRINT EXTRACT - HEADLINES'                            
PPSE01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
         FSPEC GET,REGIONS                                                      
         FSPEC GET,DISTRICTS                                                    
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20                                                          
         PSPEC H1,49,CL16'SPRINT INTERFACE'                                     
         PSPEC H1,100,AGYNAME                                                   
         PSPEC H2,49,16C'-'                                                     
         PSPEC H2,100,AGYADD                                                    
         PSPEC H3,46,PERIOD                                                     
         PSPEC H3,100,RUN                                                       
         PSPEC H4,100,REPORT                                                    
         PSPEC H4,125,PAGE                                                      
         SPROG 0                                                                
         PSPEC H6,20,C'RECORD TYPE              TOTAL'                          
         PSPEC H7,20,C'-----------              -----'                          
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025PPREPSE01 07/18/16'                                      
         END                                                                    
