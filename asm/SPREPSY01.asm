*          DATA SET SPREPSY01  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPSY01A                                                                  
         TITLE 'SPSY01 - CHILD SPOT SYNDICATED PROGRAM LIST'                    
SPSY01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H7,1,CLIENT                                                      
         SSPEC H8,1,ESTIMATE                                                    
*                                                                               
         SSPEC H1,38,C'SYNDICATED PROGRAM STATION LISTING'                      
         SSPEC H2,38,C'----------------------------------'                      
*                                                                               
         SSPEC H3,38,PERIOD                                                     
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002SPREPSY01 08/29/00'                                      
         END                                                                    
