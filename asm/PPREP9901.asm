*          DATA SET PPREP9901  AT LEVEL 008 AS OF 09/26/02                      
*PHASE  PP9901A                                                                 
         TITLE 'PP9901 - PRINTPAK STATISTICS SPECS'                             
PP9901   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         RSPEC REQUEST,NOREP                                                    
*                                                                               
         FSPEC READ,BUYS                                                        
         FSPEC READ,ACTIVE                                                      
         SPACE 2                                                                
         SPROG 0,10                                                             
         PSPEC H1,52,C'PRINTPAK STATISTICS REPORT'                              
         PSPEC H2,52,C'--------------------------'                              
*                                                                               
         PSPEC H1,100,REPORT                                                    
         PSPEC H1,121,PAGE                                                      
         PSPEC H2,100,RUN                                                       
*                                                                               
         PSPEC H1,1,REQUESTOR                                                   
*                                                                               
         SPROG 10                                                               
         PSPEC H5,1,C'                TOTAL  PCT OF  PCT OF        TOTAX        
               L    PCT OF  PCT OF      COST    INDEX VS  INDEX VS'             
         PSPEC H6,1,C'AGY MED         BUYS    MEDIA   TOTAL        DOLLX        
               ARS   MEDIA   TOTAL     PER BUY    MEDIA     TOTAL '             
         PSPEC H7,1,C'--- ---         -----  ------  ------        ----X        
               ---  ------  ------     -------  --------  --------'             
*                                                                               
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008PPREP9901 09/26/02'                                      
         END                                                                    
