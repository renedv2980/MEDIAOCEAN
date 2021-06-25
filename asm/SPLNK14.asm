*          DATA SET SPLNK14    AT LEVEL 013 AS OF 11/03/20                      
*PHASE T21E14C                                                                  
SPLNK14  TITLE '- SPOT DESKTOP BUY UPLOAD MAPS'                                 
         PRINT NOGEN                                                            
BUYUPL   CSECT                                                                  
         LKSVR TYPE=US,REQUEST=*,WORKERKEY=SPBU,SYSTEM=SPTSYSQ,        *        
               IBLOCK=*,IFROM=(SPO,LIN),ITO=(SPO,BUY),ILEN=1250,IDIAL=Y         
                                                                                
       ++INCLUDE SPMAPBUY                                                       
                                                                                
* INCLUDED BOOKS FOLLOW                                                         
         PRINT OFF                                                              
       ++INCLUDE SPLNKWRK                                                       
       ++INCLUDE SPBUYFFD                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013SPLNK14   11/03/20'                                      
         END                                                                    
