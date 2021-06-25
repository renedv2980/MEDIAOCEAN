*          DATA SET PPREPCH01  AT LEVEL 006 AS OF 07/18/16                      
*PHASE PPCH01A,+0                                                               
         TITLE 'PPCH01 - CHOICE HOTELS - HEADLINES'                             
PPCH01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BILLS                                                       
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10,20                                                          
         PSPEC H1,01,REQUESTOR                                                  
         PSPEC H2,01,MEDIA                                                      
         PSPEC H3,01,CLIENT                                                     
         PSPEC H1,46,CL23'CHOICE HOTELS INTERFACE'                              
         PSPEC H1,100,AGYNAME                                                   
         PSPEC H2,46,23C'-'                                                     
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
**PAN#1  DC    CL21'006PPREPCH01 07/18/16'                                      
         END                                                                    
