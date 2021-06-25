*          DATA SET PPREPIX01  AT LEVEL 004 AS OF 07/18/16                      
*PHASE PPIX01A                                                                  
PPIX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0                                                                
         PSPEC H1,23,C'INSERTION INTERFACE TAPE'                                
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PPREPIX01 07/18/16'                                      
         END                                                                    
