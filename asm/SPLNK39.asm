*          DATA SET SPLNK39    AT LEVEL 010 AS OF 10/03/17                      
*PHASE T21E39B                                                                  
SPLNK39  TITLE '- SPOT GOALS BUY UPLOAD MAPS'                                   
         PRINT NOGEN                                                            
SPGOLUPL CSECT                                                                  
         LKSVR TYPE=US,REQUEST=*,WORKERKEY=SPGL,SYSTEM=SPTSYSQ,        *        
               IBLOCK=*,IFROM=(SPO,LIN),ITO=(SPO,GOA),ILEN=1500                 
                                                                                
       ++INCLUDE SPMAPGOL                                                       
                                                                                
* INCLUDED BOOKS FOLLOW                                                         
*SPGOLFFD                                                                       
*SPDDEQUS                                                                       
*SPMAPEQUS                                                                      
*SPLNKWRK                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPGOLFFD                                                       
       ++INCLUDE SPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPLNK39   10/03/17'                                      
         END                                                                    
