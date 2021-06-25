*          DATA SET PPLNK19    AT LEVEL 003 AS OF 04/07/06                      
*PHASE T41419A                                                                  
PPLNK19  TITLE '- PPLNK19 - ENHANCED SPACE RESERVATION REQUEST'                 
         PRINT NOGEN                                                            
ORDUPL   LKSVR TYPE=US,REQUEST=*,WORKERKEY=PPIO,SYSTEM=PRTSYSQ,        *        
               IBLOCK=*,IFROM=(PRI,LIN),ITO=(PRI,SR2),ILEN=1500                 
                                                                                
       ++INCLUDE PPMAPSR2                                                       
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003PPLNK19   04/07/06'                                      
         END                                                                    
