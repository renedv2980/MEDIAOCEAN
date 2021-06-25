*          DATA SET SPREPDN01  AT LEVEL 012 AS OF 01/17/94                      
*PHASE SPDN01A,+0                                                               
SPDN01   CSECT                                                                  
         FSPEC USE,SPDN03                                                       
         SPROG 0,1,10,12                                                        
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H1,1,C'NETWK TV'                                                 
         SSPEC H1,51,C'NETWORK ROTATION SCHEME'                                 
         SSPEC H2,51,C'-----------------------'                                 
         SSPEC H2,100,AGYADD                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H3,47,PERIOD                                                     
         SSPEC H4,100,PAGE                                                      
         SSPEC H4,111,REPORT                                                    
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SPROG 0,1                                                              
         SSPEC H9,1,C'EST-LIN   PERIOD      DAYS     NPW TIME      DP LX        
               EN PROGRAMMING         COST'                                     
         SPROG 10,12                                                            
         SSPEC H9,34,C'GROSS UNPD  NET UNPD       TAX'                          
         SSPEC H10,34,C'----------  --------       ---'                         
         SPROG 12                                                               
         SSPEC H9,71,C'GST       PST'                                           
         SSPEC H10,71,C'---       ---'                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPDN01 01/17/94'                                      
         END                                                                    
