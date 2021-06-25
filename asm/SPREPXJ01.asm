*          DATA SET SPREPXJ01  AT LEVEL 006 AS OF 08/29/00                      
*PHASE SPXJ01A                                                                  
         TITLE 'SPREPXJ01 - BOZELL AND JACOBS INTERFACE TAPE'                   
SPXJ01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1,2,3,4                                                        
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,50,C'ESTIMATE INTERFACE TAPE'                                 
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,50,C'-----------------------'                                 
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,PAGE                                                      
         SSPEC H4,111,REPORT                                                    
         SSPEC H4,1,CLIENT                                                      
         SPROG 2,3                                                              
         SSPEC H5,1,PRODUCT                                                     
         SPROG 3,4                                                              
         SSPEC H6,1,ESTIMATE                                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPXJ01 08/29/00'                                      
         END                                                                    
