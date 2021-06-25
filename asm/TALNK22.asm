*          DATA SET TALNK22    AT LEVEL 001 AS OF 05/29/15                      
*PHASE T70422A                                                                  
TALNK22  TITLE '- TALENT TIMESHEET UPLOAD MAPS'                                 
         PRINT NOGEN                                                            
TIMUPL   CSECT                                                                  
         LKSVR TYPE=US,REQUEST=*,WORKERKEY=TATU,SYSTEM=TALSYSQ,        *        
               IBLOCK=*,IFROM=(TAL,LIN),ITO=(TAL,GEN),ILEN=6000                 
                                                                                
       ++INCLUDE TAMAPTIM                                                       
                                                                                
* INCLUDED BOOKS FOLLOW                                                         
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE TAWBDSECT                                                      
       ++INCLUDE TAWADSECT                                                      
       ++INCLUDE TASCRE7D                                                       
       ++INCLUDE TAPYS69D                                                       
       ++INCLUDE TAPYS78D                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK22   05/29/15'                                      
         END                                                                    
