*          DATA SET PPREPF301  AT LEVEL 006 AS OF 04/02/97                      
*PHASE PPF301A,+0                                                               
         TITLE 'PPF301 - PRINT ACTIVITY SPECS'                                  
PPF301   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
         SPACE 2                                                                
         SPROG 0,10,20                                                          
         PSPEC H1,44,C'PRINTPAK DAILY ACTIVITY'                                 
         PSPEC H2,44,C'-----------------------'                                 
         PSPEC H1,90,REPORT                                                     
         PSPEC H2,90,RUN                                                        
         PSPEC H3,90,PAGE                                                       
         SPACE 2                                                                
         SPROG 10,20                                                            
         PSPEC  H7,47,C'------------O R D E R E D-------------   ------X        
               --P A I D--------    ----PAID---'                                
         PSPEC  H8,47,C'GROSS AMOUNT   GROSS - CD     NET - CD   GROSS X        
               - CD     NET - CD    GST/HST/PST'                                
         PSPEC  H9,47,C'------------   ----------     --------   ------X        
               ----     --------    -----------'                                
         SPROG 10                                                               
         PSPEC  H8,1,C'AG M CLT PRD EST PUBLICATION     INS DATE'               
         PSPEC  H9,1,C'-- - --- --- --- -----------     --------'               
*                                                                               
         SPROG 20                                                               
         PSPEC H8,1,C'AG M     A G E N C Y'                                     
         PSPEC H9,1,C'-- -     -----------'                                     
*                                                                               
         DC    X'0000'                                                          
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREPF301 04/02/97'                                      
         END                                                                    
