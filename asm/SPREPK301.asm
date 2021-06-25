*          DATA SET SPREPK301  AT LEVEL 022 AS OF 10/18/06                      
*PHASE SPK301A                                                                  
SPK301   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE                                                                  
         FSPEC READ,GOALS                                                       
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC GET,MARKET                                                       
         SPACE                                                                  
         SPROG 0,2                 ** SPROG 0,2 **                              
         SSPEC H1,70,AGYNAME                                                    
         SSPEC H2,70,AGYADD                                                     
         SSPEC H3,70,REPORT                                                     
         SSPEC H4,70,PAGE                                                       
         SPACE                                                                  
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,31,C'SPOTPAK GOAL DELETION REPORT'                            
         SSPEC H2,31,C'----------------------------'                            
         SSPEC H7,31,MGROUP                                                     
         SPACE                                                                  
         SSPEC H4,29,PERIOD                                                     
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,1,ESTIMATE                                                    
         SPACE                                                                  
         SSPEC H11,1,C'MARKET'                                                  
         SSPEC H12,1,C'NUMBER'                                                  
         SSPEC H11,10,C'MARKET NAME'                                            
         SSPEC H12,10,C'-----------'                                            
         SSPEC H11,38,C'EST'                                                    
         SSPEC H12,38,C'---'                                                    
         SSPEC H11,44,C'DPT'                                                    
         SSPEC H12,44,C'---'                                                    
         SSPEC H11,50,C'LEN'                                                    
         SSPEC H12,50,C'---'                                                    
         SSPEC H11,55,C'FIRST DEL'                                              
         SSPEC H12,55,C'---------'                                              
         SSPEC H11,67,C'LAST DEL'                                               
         SSPEC H12,67,C'--------'                                               
         SSPEC H11,83,C'DOLLARS'                                                
         SSPEC H12,83,C'-------'                                                
         SSPEC H11,95,C'POINTS'                                                 
         SSPEC H12,95,C'------'                                                 
         SPACE                                                                  
         SPROG 2                   ** SPROG2 **                                 
         SSPEC H3,28,C'*** TEST RUN - FILE NOT MARKED ***'                      
         SPACE                                                                  
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'6201ADAYPART'                                               
         DC    CL25'6303NSPOTLENGTH'                                            
         DC    CL25'6601AWRITE TO FILES'                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPREPK301 10/18/06'                                      
         END                                                                    
