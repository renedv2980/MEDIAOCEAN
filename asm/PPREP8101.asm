*          DATA SET PPREP8101  AT LEVEL 024 AS OF 08/09/00                      
*PHASE PP8101A                                                                  
         TITLE 'PP8101 - PRINTPAK USER REPORT'                                  
PP8101   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
         RSPEC LINEUP,PATTERN                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1                                                              
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,76,AGYADD                                                     
         PSPEC H3,44,PERIOD                                                     
         PSPEC H4,76,RUN                                                        
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H3,1,CLIENT                                                      
         PSPEC H5,76,REPORT                                                     
         PSPEC H5,98,PAGE                                                       
         SPROG 1                                                                
         PSPEC H4,1,DIVISION                                                    
         SPACE 2                                                                
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024PPREP8101 08/09/00'                                      
         END                                                                    
