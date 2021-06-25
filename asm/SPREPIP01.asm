*          DATA SET SPREPIP01  AT LEVEL 005 AS OF 06/29/07                      
*PHASE SPIP01A                                                                  
         TITLE 'SPIP01 - CONVERTS 12 AUTOPAY RECORDS TO WORKER FILE'            
SPIP01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
*                                                                               
         SSPEC H1,38,C'AUTOPAY'                                                 
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
**PAN#1  DC    CL21'005SPREPIP01 06/29/07'                                      
         END                                                                    
