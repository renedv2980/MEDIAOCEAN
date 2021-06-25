*          DATA SET SPLNK15    AT LEVEL 020 AS OF 11/03/20                      
*PHASE T21E15C                                                                  
SPLNK15  TITLE '- SPOT DESKTOP BUY UPLOAD MAPS MAKEGOODS'                       
         PRINT NOGEN                                                            
BUYUPL2  CSECT                                                                  
         LKSVR TYPE=US,REQUEST=*,WORKERKEY=SPB2,SYSTEM=SPTSYSQ,        *        
               IBLOCK=*,IFROM=(SPO,LIN),ITO=(SPO,BUY),ILEN=3400,IDIAL=Y         
                                                                                
       ++INCLUDE SPMAPBUY                                                       
                                                                                
* INCLUDED BOOKS FOLLOW                                                         
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
       ++INCLUDE SPBUYFFD                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'020SPLNK15   11/03/20'                                      
         END                                                                    
