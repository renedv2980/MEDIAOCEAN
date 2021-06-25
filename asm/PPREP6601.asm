*          DATA SET PPREP6601  AT LEVEL 017 AS OF 07/18/16                      
*PHASE PP6601A                                                                  
         TITLE 'PP6601 - PRINTPAK ESTIMATE SPECS'                               
PP6601   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
* SPROG VALUES ARE                                                              
*        1     NEWSPAPERS                                                       
*        2     MAGAZINES                                                        
*        3     INSERTION MONTH SUMMARY                                          
*        4     BILLING MONTH SUMMARY                                            
*                                                                               
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUB                                                          
         FSPEC GET,CONTRACT                                                     
         FSPEC GET,REGION                                                       
         FSPEC GET,DISTRICT                                                     
         EJECT                                                                  
         RSPEC REQUEST,SUMMARY                                                  
         EJECT                                                                  
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,76,AGYADD                                                     
         PSPEC H3,44,PERIOD                                                     
         PSPEC H4,76,RUN                                                        
         PSPEC H5,2,REQUESTOR                                                   
         PSPEC H5,76,REPORT                                                     
         PSPEC H5,98,PAGE                                                       
         SPACE 2                                                                
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'0102030405060708090A0B0C0D0E0F16131400'                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017PPREP6601 07/18/16'                                      
         END                                                                    
