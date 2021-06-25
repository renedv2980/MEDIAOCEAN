*          DATA SET TALNK19    AT LEVEL 001 AS OF 05/29/15                      
*PHASE T70419A                                                                  
TALNK19  TITLE '- TALENT AUTH/PO ESTIMATE JOB DOWNLOAD MAP'                     
         PRINT NOGEN                                                            
PJVUPL   CSECT                                                                  
         LKSVR TYPE=US,REQUEST=*,WORKERKEY=TAPJ,SYSTEM=TALSYSQ,        *        
               IBLOCK=*,IFROM=(TAL,LIN),ITO=(TAL,GEN),ILEN=6000                 
                                                                                
       ++INCLUDE TAMAPPJV                                                       
                                                                                
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
**PAN#1  DC    CL21'001TALNK19   05/29/15'                                      
         END                                                                    
