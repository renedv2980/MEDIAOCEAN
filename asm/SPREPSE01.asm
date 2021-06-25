*          DATA SET SPREPSE01  AT LEVEL 006 AS OF 05/23/05                      
*PHASE SPSE01A,+0                                                               
         TITLE 'SPSE01 - SPRINT EXTRACT - HEADLINES'                            
SPSE01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         FSPEC USE,SPSE03                                                       
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20                                                          
         SSPEC H1,49,CL16'SPRINT INTERFACE'                                     
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,49,16C'-'                                                     
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,41,PERIOD                                                     
         SSPEC H3,100,RUN                                                       
         SSPEC H4,100,REPORT                                                    
         SSPEC H4,125,PAGE                                                      
         SPROG 0                                                                
         SSPEC H6,20,C'RECORD TYPE              TOTAL'                          
         SSPEC H7,20,C'-----------              -----'                          
         DC    X'00'                                                            
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPSE01 05/23/05'                                      
         END                                                                    
