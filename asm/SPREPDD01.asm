*          DATA SET SPREPDD01  AT LEVEL 005 AS OF 12/02/03                      
*PHASE SPDD01A                                                                  
         TITLE 'SPDD01 - DEMOGRAHIC CONVERSION REPORT PRINT SPECS'              
SPDD01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,MARKET                                                       
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
         SPROG 0,1,2                                                            
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H4,1,CLIENT                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
*                                                                               
         SSPEC H3,47,PERIOD                                                     
         SSPEC H5,54,MARKET                                                     
         SSPEC H6,56,STATION                                                    
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H4,100,RATING                                                    
         SSPEC H5,100,BOOK                                                      
         SSPEC H6,100,REPORT                                                    
         SSPEC H7,100,PAGE                                                      
*                                                                               
         SSPEC H1,49,C'DEMOGRAPHIC COMPARISON REPORT'                           
         SSPEC H2,49,C'-----------------------------'                           
         SSPEC H10,1,C'EST-LIN    DATES     DAY(S)     TIME    '                
         SSPEC H11,1,C'------- ----------- -------- -----------'                
         SSPEC H10,42,C'   PROGRAMMING    '                                     
         SSPEC H11,42,C'------------------'                                     
*                                                                               
         SPROG 0,1                                                              
         SSPEC H10,67,C'AVERAGE WEEKLY '                                        
         SSPEC H11,67,C'------- -------'                                        
         SSPEC H10,84,C'AVERAGE WEEKLY '                                        
         SSPEC H11,84,C'------- -------'                                        
         SSPEC H10,101,C'AVERAGE WEEKLY '                                       
         SSPEC H11,101,C'------- -------'                                       
         SSPEC H10,118,C'AVERAGE WEEKLY '                                       
         SSPEC H11,118,C'------- -------'                                       
*                                                                               
         SPROG 2                                                                
         SSPEC H11,67,C'------- -------'                                        
         SSPEC H11,83,C'------- -------'                                        
         SSPEC H11,99,C'------- -------'                                        
         SSPEC H11,115,C'------- -------'                                       
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'6201AACTUAL BOOK'                                           
         DC    CL25'6301ARERATE'                                                
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005SPREPDD01 12/02/03'                                      
         END                                                                    
