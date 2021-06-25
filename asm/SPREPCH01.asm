*          DATA SET SPREPCH01  AT LEVEL 002 AS OF 07/18/16                      
*PHASE SPCH01A,+0                                                               
         TITLE 'SPCH01 - CHOICE HOTELS- HEADLINES'                              
SPCH01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPCH03                                                       
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20                                                          
         PSPEC H1,42,CL23'CHOICE HOTELS INTERFACE'                              
         PSPEC H1,100,AGYNAME                                                   
         PSPEC H2,49,16C'-'                                                     
         PSPEC H2,100,AGYADD                                                    
         PSPEC H3,41,PERIOD                                                     
         PSPEC H3,100,RUN                                                       
         PSPEC H4,100,REPORT                                                    
         PSPEC H4,125,PAGE                                                      
         SPROG 0                                                                
         PSPEC H6,20,C'RECORD TYPE              TOTAL'                          
         PSPEC H7,20,C'-----------              -----'                          
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPCH01 07/18/16'                                      
         END                                                                    
