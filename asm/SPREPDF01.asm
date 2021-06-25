*          DATA SET SPREPDF01  AT LEVEL 001 AS OF 05/20/09                      
*PHASE SPDF01A                                                                  
         TITLE 'SPDF01 - CONVERTS DFORM RECORDS TO WORKER FILE'                 
SPDF01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
*                                                                               
         SSPEC H1,38,C'DFORM'                                                   
         SSPEC H2,38,C'-------'                                                 
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
**PAN#1  DC    CL21'001SPREPDF01 05/20/09'                                      
         END                                                                    
