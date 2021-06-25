*          DATA SET PPLNK17    AT LEVEL 008 AS OF 05/22/06                      
*PHASE T41417A                                                                  
PPLNK17  TITLE '- PPLNK17 - INSERTION ORDER 2 REQUEST'                          
         PRINT NOGEN                                                            
ORDUPL   LKSVR TYPE=US,REQUEST=*,WORKERKEY=PPIO,SYSTEM=PRTSYSQ,        *        
               IBLOCK=*,IFROM=(PRI,LIN),ITO=(PRI,IO2),ILEN=1500                 
                                                                                
       ++INCLUDE PPMAPINS                                                       
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008PPLNK17   05/22/06'                                      
         END                                                                    
