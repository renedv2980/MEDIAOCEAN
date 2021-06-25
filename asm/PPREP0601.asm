*          DATA SET PPREP0601  AT LEVEL 010 AS OF 08/09/00                      
*PHASE PP0601A                                                                  
         TITLE 'PP0601 - PRINTPAK DETAIL BILLING SPECS'                         
PP0601   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC MAXLINES,59                                                      
         SPACE 2                                                                
         FSPEC READ,BUYS                                                        
         FSPEC GET,DIVISIONS                                                    
         FSPEC GET,REGIONS                                                      
         FSPEC GET,DISTRICTS                                                    
         FSPEC GET,PUBLICATIONS                                                 
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
         SPACE 2                                                                
*                                  BILLING                                      
         SPACE 2                                                                
         SPROG 2                                                                
         PSPEC P1,39,MEDIANAME                                                  
         SPROG 3                                                                
         PSPEC P1,70,PAGE                                                       
         SPROG 4                                                                
         PSPEC P1,2,CLIENT                                                      
         SPROG 5                                                                
         PSPEC P1,2,DIVISION                                                    
         SPROG 7                                                                
         PSPEC P1,2,ESTIMATE                                                    
         SPROG 9                                                                
         PSPEC P1,2,REGION                                                      
         SPROG 10                                                               
         PSPEC P1,2,DISTRICT                                                    
         SPACE 2                                                                
*                                  INV REGISTER                                 
         SPACE 2                                                                
         SPROG 101                                                              
         PSPEC P1,2,AGYNAME                                                     
         PSPEC P1,48,C'DETAIL BILLING INVOICE REGISTER'                         
         SPROG 102                                                              
         PSPEC P1,2,AGYADD                                                      
         PSPEC P1,48,REPORT                                                     
         SPROG 103                                                              
         PSPEC P1,48,RUN                                                        
         PSPEC P1,73,PAGE                                                       
         SPROG 104                                                              
         PSPEC P1,2,MEDIA                                                       
         SPACE 2                                                                
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020304050607080C0D0E0F10131400'                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREP0601 08/09/00'                                      
         END                                                                    
