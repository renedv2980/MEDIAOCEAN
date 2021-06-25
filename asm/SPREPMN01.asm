*          DATA SET SPREPMN01  AT LEVEL 002 AS OF 08/29/00                      
*PHASE SPMN01A                                                                  
         TITLE 'SPMN01 - CLIENT COKE CONVERSION TO WORKER FILE'                 
SPMM01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H7,1,CLIENT                                                      
         SSPEC H8,1,ESTIMATE                                                    
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
**PAN#1  DC    CL21'002SPREPMN01 08/29/00'                                      
         END                                                                    
