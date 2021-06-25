*          DATA SET SPREPC901  AT LEVEL 001 AS OF 02/18/98                      
*PHASE SPC901A                                                                  
         TITLE 'SPC901 - COKE BUY LOAD TURNAROUND REPORT'                       
SPC901   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,CLIENT                                                      
*                                                                               
         SSPEC H1,49,C'COKE BUY LOAD TURNAROUND'                                
         SSPEC H2,49,C'------------------------'                                
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H5,100,PAGE                                                      
         SSPEC H5,111,REPORT                                                    
*                                                                               
         SSPEC H7,1,C'PRD'                                                      
         SSPEC H7,6,C'PRODUCT NAME'                                             
         SSPEC H7,28,C'EST'                                                     
         SSPEC H7,33,C'ESTIMATE NAME'                                           
         SSPEC H7,55,C'MARKET'                                                  
         SSPEC H7,63,C'MARKET NAME'                                             
         SSPEC H7,85,C'STATION'                                                 
         SSPEC H8,1,C'---'                                                      
         SSPEC H8,6,C'------------'                                             
         SSPEC H8,28,C'---'                                                     
         SSPEC H8,33,C'-------------'                                           
         SSPEC H8,55,C'------'                                                  
         SSPEC H8,63,C'-----------'                                             
         SSPEC H8,85,C'-------'                                                 
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPC901 02/18/98'                                      
         END                                                                    
