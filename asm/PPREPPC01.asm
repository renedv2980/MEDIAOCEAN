*          DATA SET PPREPPC01  AT LEVEL 007 AS OF 01/25/96                      
*PHASE PPPC01A,+0                                                               
         TITLE 'PPPC01 - PRINTPAK PUBFILE  ACCROSS AGY LISTING'                 
PPPC01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         SPROG 0,1,2,4,10,12,14,30,31                                           
         PSPEC H1,2,MEDIA                                                       
         PSPEC H2,2,REQUESTOR                                                   
         PSPEC H1,95,AGYNAME                                                    
         PSPEC H2,95,AGYADD                                                     
         PSPEC H4,95,REPORT                                                     
         PSPEC H5,95,RUN                                                        
         PSPEC H6,95,PAGE                                                       
         SPROG 0,2,4                                                            
         PSPEC H1,45,C'ACROSS AGENCY PUBLICATION LISTING'                       
         PSPEC H2,45,C'---------------------------------'                       
         SPROG 10,12,14                                                         
         PSPEC H1,45,C'ACROSS AGENCY REP LISTING'                               
         PSPEC H2,45,C'-------------------------'                               
         SPROG 1,30,31                                                          
         PSPEC H1,45,C'ACROSS AGENCY CLIENT LISTING'                            
         PSPEC H2,45,C'----------------------------'                            
         SPROG 2                                                                
         PSPEC H7,2,C'VENDOR NO.'                                               
         PSPEC H8,2,C'----------'                                               
         PSPEC H7,25,C'PUB NAME'                                                
         PSPEC H8,25,C'---------'                                               
         PSPEC H7,69,C'PAYREP'                                                  
         PSPEC H8,69,C'------'                                                  
         PSPEC H7,79,C'AGENCY'                                                  
         PSPEC H8,79,C'------'                                                  
         SPROG 4                                                                
         PSPEC H7,2,C'PUB NAME'                                                 
         PSPEC H8,2,C'---------'                                                
         PSPEC H7,48,C'VENDOR NO.'                                              
         PSPEC H8,48,C'----------'                                              
         PSPEC H7,69,C'PAYREP'                                                  
         PSPEC H8,69,C'------'                                                  
         PSPEC H7,79,C'AGENCY'                                                  
         PSPEC H8,79,C'------'                                                  
         SPROG 12                                                               
         PSPEC H7,2,C'REP CODE'                                                 
         PSPEC H8,2,C'--------'                                                 
         PSPEC H7,15,C'REP NAME'                                                
         PSPEC H8,15,C'---------'                                               
         PSPEC H7,50,C'AGENCY'                                                  
         PSPEC H8,50,C'------'                                                  
         SPROG 14                                                               
         PSPEC H7,2,C'REP NAME'                                                 
         PSPEC H8,2,C'---------'                                                
         PSPEC H7,38,C'REP CODE'                                                
         PSPEC H8,38,C'--------'                                                
         PSPEC H7,50,C'AGENCY'                                                  
         PSPEC H8,50,C'------'                                                  
         SPROG 30                                                               
         PSPEC H7,2,C'CLIENT CODE'                                              
         PSPEC H8,2,C'-----------'                                              
         PSPEC H7,17,C'CLIENT NAME'                                             
         PSPEC H8,17,C'-----------'                                             
         PSPEC H7,50,C'AGENCY'                                                  
         PSPEC H8,50,C'------'                                                  
         SPROG 31                                                               
         PSPEC H7,2,C'CLIENT NAME'                                              
         PSPEC H8,2,C'-----------'                                              
         PSPEC H7,27,C'CLIENT CODE'                                             
         PSPEC H8,27,C'-----------'                                             
         PSPEC H7,50,C'AGENCY'                                                  
         PSPEC H8,50,C'------'                                                  
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020415131400'                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007PPREPPC01 01/25/96'                                      
         END                                                                    
