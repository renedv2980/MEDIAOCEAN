*          DATA SET SPREPBY01  AT LEVEL 001 AS OF 10/13/98                      
*PHASE SPBY01A                                                                  
         TITLE 'SPBY01 - SPOT BUY COPY TO MEDIA + X08 '                         
SPBY01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC READ,BUYS                                                        
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC UPDATE,SPTFILE                                                   
         SSPEC H1,24,C'BUY COPY'                                                
         SSPEC H2,24,C'--------'                                                
         SSPEC H1,60,REPORT                                                     
         SSPEC H2,60,REQUESTOR                                                  
         SSPEC H1,3,PAGE                                                        
         SSPEC H4,3,C'AGY'                                                      
         SSPEC H5,3,C'---'                                                      
         SSPEC H4,7,C'MED'                                                      
         SSPEC H5,7,C'---'                                                      
         SSPEC H4,11,C'CLT'                                                     
         SSPEC H5,11,C'---'                                                     
         SSPEC H4,16,C'MKT'                                                     
         SSPEC H5,16,C'---'                                                     
         SSPEC H4,22,C'STATION'                                                 
         SSPEC H5,22,C'-------'                                                 
         SSPEC H4,30,C'EST-LINE'                                                
         SSPEC H5,30,C'--------'                                                
         SSPEC H4,40,C'OLD'                                                     
         SSPEC H5,40,C'---'                                                     
         SSPEC H4,45,C'STATUS'                                                  
         SSPEC H5,45,C'------'                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPBY01 10/13/98'                                      
         END                                                                    
