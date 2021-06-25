*          DATA SET PPLNK21    AT LEVEL 004 AS OF 06/16/08                      
*PHASE T41421B                                                                  
PPLNK21  TITLE '- PPLNK21 - ADFILE UPLOAD'                                      
         PRINT NOGEN                                                            
ADCUPL   LKSVR TYPE=US,REQUEST=*,WORKERKEY=PPAD,SYSTEM=PRTSYSQ,        *        
               IBLOCK=*,IFROM=(PRI,LIN),ITO=(PRI,ADF),ILEN=1500                 
                                                                                
       ++INCLUDE PPMAPJOB                                                       
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004PPLNK21   06/16/08'                                      
         END                                                                    
