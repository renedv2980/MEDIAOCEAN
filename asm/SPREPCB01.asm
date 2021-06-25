*          DATA SET SPREPCB01  AT LEVEL 004 AS OF 08/29/00                      
*PHASE SPCB01A                                                                  
         TITLE 'SPCB01 - SPOT COMPARATIVE BUYING REPORT - SPECS'                
SPCB01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
         FSPEC READ,BUYS                                                        
         FSPEC GET,STATION                                                      
         FSPEC OPEN,DEMFILES                                                    
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
*                                                                               
         SSPEC H3,51,PERIOD                                                     
*                                                                               
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,AGYADD                                                    
         SSPEC H3,100,PAGE                                                      
         SSPEC H3,111,REPORT                                                    
*                                                                               
         SSPEC H1,55,C'COMPARATIVE BUYING REPORT'                               
         SSPEC H2,55,C'-------------------------'                               
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'5006DPAYMENTS FROM'                                         
         DC    CL25'5606DPAYMENTS TO'                                           
         DC    CL25'6201AREPORT TYPE'                                           
         DC    X'00'                                                            
 END                                                                            
