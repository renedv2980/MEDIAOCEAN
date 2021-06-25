*          DATA SET SPREPKB01  AT LEVEL 009 AS OF 08/19/97                      
*PHASE SPKB01A,+0                                                               
SPKB01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE                                                                  
         FSPEC READ,GOALS                                                       
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC GET,MARKET                                                       
         SPACE                                                                  
         SPROG 0,1,2,3,4           ** SPROG 0,1,2,3,4 **                        
         SSPEC H1,70,AGYNAME                                                    
         SSPEC H2,70,AGYADD                                                     
         SSPEC H3,70,REPORT                                                     
         SSPEC H4,70,PAGE                                                       
         SPACE                                                                  
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,28,C'SPOTPAK GOAL PERIOD TRANSFER REPORT'                     
         SSPEC H2,28,C'-----------------------------------'                     
         SSPEC H7,31,MGROUP                                                     
         SPACE                                                                  
         SSPEC H4,29,PERIOD                                                     
         SSPEC H5,1,CLIENT                                                      
         SSPEC H6,1,PRODUCT                                                     
         SSPEC H7,1,ESTIMATE                                                    
         SPACE                                                                  
         SSPEC H12,36,C'------------------DELETED-----------------'             
         SSPEC H12,80,C'-------------ADDED-------------'                        
         SSPEC H13,1,C'MARKET'                                                  
         SSPEC H14,1,C'NUMBER'                                                  
         SSPEC H13,10,C'MARKET NAME'                                            
         SSPEC H14,10,C'-----------'                                            
         SSPEC H13,27,C'EST'                                                    
         SSPEC H14,27,C'---'                                                    
         SSPEC H13,36,C'DOLLARS'                                                
         SSPEC H14,36,C'-------'                                                
         SSPEC H13,46,C'POINTS'                                                 
         SSPEC H14,46,C'------'                                                 
         SSPEC H13,54,C'FRST DEL'                                               
         SSPEC H14,54,C'--------'                                               
         SSPEC H13,64,C'LAST DEL'                                               
         SSPEC H14,64,C'--------'                                               
         SSPEC H13,74,C'D/'                                                     
         SSPEC H14,74,C'--'                                                     
         SSPEC H13,76,C'LN'                                                     
         SSPEC H14,76,C'--'                                                     
         SSPEC H13,80,C'PRD'                                                    
         SSPEC H14,80,C'---'                                                    
         SSPEC H13,89,C'DOLLARS'                                                
         SSPEC H14,89,C'-------'                                                
         SSPEC H13,99,C'POINTS'                                                 
         SSPEC H14,99,C'------'                                                 
         SSPEC H13,107,C'D/'                                                    
         SSPEC H14,107,C'--'                                                    
         SSPEC H13,109,C'LN'                                                    
         SSPEC H14,109,C'--'                                                    
         SPACE                                                                  
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'6201ADRAFT MODE'                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPKB01 08/19/97'                                      
         END                                                                    
