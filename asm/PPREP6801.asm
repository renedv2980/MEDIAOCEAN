*          DATA SET PPREP6801  AT LEVEL 003 AS OF 08/09/00                      
*PHASE PP6801A                                                                  
         TITLE 'PP6801 - OUTDOOR VENDOR REPORT SPECS'                           
PP6801   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC REQUEST,NOREP                                                    
         SPACE 2                                                                
         PSPEC H1,21,C'OUTDOOR VENDOR REPORT'                                   
         PSPEC H2,21,C'------- ------ ------'                                   
         PSPEC H1,78,AGYNAME                                                    
         PSPEC H2,78,AGYADD                                                     
         PSPEC H4,78,REPORT                                                     
         PSPEC H4,98,PAGE                                                       
         PSPEC H5,78,RUN                                                        
         SPACE 2                                                                
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPREP6801 08/09/00'                                      
         END                                                                    
