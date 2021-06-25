*          DATA SET TALNK12    AT LEVEL 001 AS OF 05/29/15                      
*PHASE T70412A                                                                  
TALNK12  TITLE '- TALENT PAY UPLOAD MAPS'                                       
         PRINT NOGEN                                                            
PAYUPL   CSECT                                                                  
         LKSVR TYPE=US,REQUEST=*,WORKERKEY=TAPU,SYSTEM=TALSYSQ,        *        
               IBLOCK=*,IFROM=(TAL,LIN),ITO=(TAL,GEN),ILEN=6000                 
                                                                                
       ++INCLUDE TAMAPPAY                                                       
                                                                                
* INCLUDED BOOKS FOLLOW                                                         
         PRINT OFF                                                              
       ++INCLUDE TALNKWRK                                                       
       ++INCLUDE TAWBDSECT                                                      
       ++INCLUDE TAWADSECT                                                      
       ++INCLUDE TAPYS50D                                                       
       ++INCLUDE TAPYS69D                                                       
       ++INCLUDE TAPYS78D                                                       
       ++INCLUDE TASCRE7D                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001TALNK12   05/29/15'                                      
         END                                                                    
