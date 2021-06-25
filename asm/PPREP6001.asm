*          DATA SET PPREP6001  AT LEVEL 003 AS OF 08/09/00                      
*PHASE PP6001A                                                                  
         TITLE 'PRINTPAK - MEDIA SCHEDULE REPORT - SPCES'                       
PP6001   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         FSPEC READ,BUYS                                                        
         FSPEC GET,DIVISIONS                                                    
         FSPEC GET,REGIONS                                                      
         FSPEC GET,DISTRICTS                                                    
         FSPEC GET,PUBLICATIONS                                                 
         SPACE 2                                                                
         PSPEC H1,1,MEDIA                                                       
         PSPEC H3,1,CLIENT                                                      
         PSPEC H4,1,DIVISION                                                    
         PSPEC H5,1,PRODUCT                                                     
         PSPEC H6,1,ESTIMATE                                                    
         PSPEC H7,1,REQUESTOR                                                   
*                                                                               
         PSPEC H3,55,PERIOD                                                     
*                                                                               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,122,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREP6001 08/09/00'                                      
         END                                                                    
