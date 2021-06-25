*          DATA SET PPREPL101  AT LEVEL 066 AS OF 01/06/88                      
*PHASE PPL101C,+0                                                               
         TITLE 'PPL101C - PRINTPAK USER REPORT'                                 
PPL101C  CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,1,2,3                                                          
         XSPEC H1,99,AGYNAME                                                    
         XSPEC H2,1,CLIENT                                                      
         XSPEC H2,99,AGYADD                                                     
         XSPEC H3,50,PERIOD                                                     
         XSPEC H4,1,ESTIMATE                                                    
         XSPEC H4,99,RUN                                                        
         XSPEC H5,99,REPORT                                                     
         XSPEC H5,120,PAGE                                                      
         XSPEC H7,99,REQUESTOR                                                  
         SPROG 1,3                                                              
         XSPEC H5,1,DIVISION                                                    
         SPROG 3                                                                
         XSPEC H6,1,REGION                                                      
         XSPEC H7,1,DISTRICT                                                    
*                                                                               
         SPROG 10,11,12,13                                                      
         XSPEC H1,99,AGYNAME                                                    
         XSPEC H2,99,AGYADD                                                     
         XSPEC H3,50,PERIOD                                                     
         XSPEC H4,1,ESTIMATE                                                    
         XSPEC H4,99,RUN                                                        
         XSPEC H5,99,REPORT                                                     
         XSPEC H5,120,PAGE                                                      
         XSPEC H7,99,REQUESTOR                                                  
         SPROG 11,13                                                            
         XSPEC H5,1,DIVISION                                                    
         SPROG 13                                                               
         XSPEC H6,1,REGION                                                      
         XSPEC H7,1,DISTRICT                                                    
*                                                                               
         SPROG 20,21,22,23                                                      
         XSPEC H1,99,AGYNAME                                                    
         XSPEC H2,1,CLIENT                                                      
         XSPEC H2,99,AGYADD                                                     
         XSPEC H3,1,PRODUCT                                                     
         XSPEC H3,50,PERIOD                                                     
         XSPEC H4,1,ESTIMATE                                                    
         XSPEC H4,99,RUN                                                        
         XSPEC H5,99,REPORT                                                     
         XSPEC H5,120,PAGE                                                      
         XSPEC H7,99,REQUESTOR                                                  
         SPROG 21,23                                                            
         XSPEC H5,1,DIVISION                                                    
         SPROG 23                                                               
         XSPEC H6,1,REGION                                                      
         XSPEC H7,1,DISTRICT                                                    
*                                                                               
         SPROG 30,31,32,33                                                      
         XSPEC H1,99,AGYNAME                                                    
*****    XSPEC H2,1,CLIENT                                                      
         XSPEC H2,99,AGYADD                                                     
         XSPEC H3,1,PRODUCT                                                     
         XSPEC H3,50,PERIOD                                                     
         XSPEC H4,1,ESTIMATE                                                    
         XSPEC H4,99,RUN                                                        
         XSPEC H5,99,REPORT                                                     
         XSPEC H5,120,PAGE                                                      
         XSPEC H7,99,REQUESTOR                                                  
         SPROG 31,33                                                            
         XSPEC H5,1,DIVISION                                                    
         SPROG 33                                                               
         XSPEC H6,1,REGION                                                      
         XSPEC H7,1,DISTRICT                                                    
*                                                                               
*                                                                               
*                                                                               
         SPROG 50,51,52,53                                                      
         XSPEC H1,99,AGYNAME                                                    
         XSPEC H2,1,CLIENT                                                      
         XSPEC H2,99,AGYADD                                                     
         XSPEC H3,50,PERIOD                                                     
         XSPEC H4,1,ESTIMATE                                                    
         XSPEC H4,99,RUN                                                        
         XSPEC H5,120,PAGE                                                      
         XSPEC H5,99,REPORT                                                     
         SPROG 51,53                                                            
         XSPEC H5,1,DIVISION                                                    
         SPROG 53                                                               
         XSPEC H6,1,REGION                                                      
         XSPEC H7,1,DISTRICT                                                    
*                                                                               
         SPROG 60,61,62,63                                                      
         XSPEC H1,99,AGYNAME                                                    
         XSPEC H2,99,AGYADD                                                     
         XSPEC H3,50,PERIOD                                                     
         XSPEC H4,1,ESTIMATE                                                    
         XSPEC H4,99,RUN                                                        
         XSPEC H5,120,PAGE                                                      
         XSPEC H5,99,REPORT                                                     
         SPROG 61,63                                                            
         XSPEC H5,1,DIVISION                                                    
         SPROG 63                                                               
         XSPEC H6,1,REGION                                                      
         XSPEC H7,1,DISTRICT                                                    
*                                                                               
         SPROG 70,71,72,73                                                      
         XSPEC H1,99,AGYNAME                                                    
         XSPEC H2,1,CLIENT                                                      
         XSPEC H2,99,AGYADD                                                     
         XSPEC H3,1,PRODUCT                                                     
         XSPEC H3,50,PERIOD                                                     
         XSPEC H4,1,ESTIMATE                                                    
         XSPEC H4,99,RUN                                                        
         XSPEC H5,120,PAGE                                                      
         XSPEC H5,99,REPORT                                                     
         SPROG 71,73                                                            
         XSPEC H5,1,DIVISION                                                    
         SPROG 73                                                               
         XSPEC H6,1,REGION                                                      
         XSPEC H7,1,DISTRICT                                                    
*                                                                               
         SPROG 80,81,82,83                                                      
         XSPEC H1,99,AGYNAME                                                    
         XSPEC H2,99,AGYADD                                                     
         XSPEC H3,1,PRODUCT                                                     
         XSPEC H3,50,PERIOD                                                     
         XSPEC H4,1,ESTIMATE                                                    
         XSPEC H4,99,RUN                                                        
         XSPEC H5,120,PAGE                                                      
         XSPEC H5,99,REPORT                                                     
         SPROG 81,83                                                            
         XSPEC H5,1,DIVISION                                                    
         SPROG 83                                                               
         XSPEC H6,1,REGION                                                      
         XSPEC H7,1,DISTRICT                                                    
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020304050607090A0B0C0D0E0F13141C00'                          
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066PPREPL101 01/06/88'                                      
         END                                                                    
