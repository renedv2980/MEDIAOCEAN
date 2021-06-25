*          DATA SET PPREPZZ01  AT LEVEL 003 AS OF 08/09/00                      
*PHASE PPZZ01A                                                                  
         TITLE 'PPZZ01 - PRINT ZZ PUB UPDATE'                                   
PPZZ01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PUBDIR                                                    
         SPACE 2                                                                
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,122,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         SPACE 2                                                                
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,47,C'PRINTPAK - ZZ PUBFILE TRANSFER UPDATE'                   
         PSPEC H2,47,C'-------------------------------------'                   
         SPACE 2                                                                
         PSPEC H7,3,C'MED   VENDOR CODE        VENDOR NAME'                     
         PSPEC H8,3,C'---   -----------        -----------'                     
         SPACE 2                                                                
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREPZZ01 08/09/00'                                      
         END                                                                    
