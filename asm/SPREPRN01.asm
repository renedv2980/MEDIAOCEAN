*          DATA SET SPREPRN01  AT LEVEL 006 AS OF 08/29/00                      
*PHASE SPRN01A                                                                  
         TITLE 'SPREPRN01 - NETWORK SCHEDULE'                                   
         PRINT NOGEN                                                            
SPRN01   CSECT                                                                  
         FSPEC USE,SPRN03                                                       
         FSPEC READ,BUYS                                                        
         FSPEC READ,PACKAGES                                                    
         SPROG 0,THRU,8                                                         
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,77,AGYADD                                                     
         SSPEC H3,1,PAGE                                                        
         SSPEC H3,77,REPORT                                                     
         SSPEC H3,30,PERIOD                                                     
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,1,ESTIMATE                                                    
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPRN01 08/29/00'                                      
         END                                                                    
