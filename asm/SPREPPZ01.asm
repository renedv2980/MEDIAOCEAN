*          DATA SET SPREPPZ01  AT LEVEL 002 AS OF 07/18/16                      
*PHASE SPPZ01A,+0                                                               
         TITLE 'SPPZ01 - PFIZER EXTRACT - HEADLINES'                            
SPPZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPPZ03                                                       
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20                                                          
         PSPEC H1,49,CL16'PFIZER INTERFACE'                                     
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
**PAN#1  DC    CL21'002SPREPPZ01 07/18/16'                                      
         END                                                                    
