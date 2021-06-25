*          DATA SET PPREP5201  AT LEVEL 006 AS OF 07/18/16                      
*PHASE PP5201A,+0                                                               
         TITLE 'PP5201 - PRINTPAK ESTIMATE SPECS'                               
PP5201   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUB                                                          
         FSPEC GET,CONTRACT                                                     
         FSPEC GET,REGION                                                       
         FSPEC GET,DISTRICT                                                     
*                                                                               
         RSPEC REQUEST,SUMMARY                                                  
*                                                                               
         PSPEC H2,2,CLIENT                                                      
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,76,AGYADD                                                     
         PSPEC H3,2,DIVISION                                                    
         PSPEC H4,2,PRODUCT                                                     
         PSPEC H3,44,PERIOD                                                     
         PSPEC H5,2,ESTIMATE                                                    
         PSPEC H4,76,RUN                                                        
         PSPEC H1,2,REQUESTOR                                                   
         PSPEC H5,37,REGION                                                     
         PSPEC H5,76,REPORT                                                     
         PSPEC H5,98,PAGE                                                       
         PSPEC H6,37,DISTRICT                                                   
         SPACE 2                                                                
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'010203041F05060708090A0B0C0D0E0F16131400'                      
         DC    CL25'5803DCONTROL DATE'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREP5201 07/18/16'                                      
         END                                                                    
