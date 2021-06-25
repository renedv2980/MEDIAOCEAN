*          DATA SET SPREPNV01  AT LEVEL 009 AS OF 08/29/00                      
*PHASE SPNV01A                                                                  
         TITLE 'SPREPNV01 - INVOICE CONTROL REPORT SPECS'                       
         PRINT NOGEN                                                            
SPNV01   CSECT                                                                  
         FSPEC USE,SPNV03                                                       
*                                                                               
         SPROG 0                                                                
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,41,C'STATION INVOICE CONTROL REPORT'                          
         SSPEC H1,77,AGYNAME                                                    
*                                                                               
         SSPEC H2,41,C'------------------------------'                          
         SSPEC H2,77,AGYADD                                                     
*                                                                               
         SSPEC H4,77,REPORT                                                     
*                                                                               
         SSPEC H5,77,PAGE                                                       
         SSPEC H5,87,REQUESTOR                                                  
*                                                                               
         DC    X'00'                SET END-OF-SPECS                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    CL25'3001APOL PRODUCT BREAKOUT'                                  
         DC    CL25'5006ALETTER DATE'                                           
         DC    CL25'6001AINCLUDE EIX STATIONS'                                  
         DC    CL25'6201AGROSS/NET/BOTH'                                        
         DC    CL25'6301APRINT REPORT/LETTERS'                                  
         DC    CL25'6401ASUPPRESS MATCHED'                                      
         DC    CL25'6501AEXCLUDE SPCL REP $'                                    
         DC    CL25'6801ACLIENT EXCLUSION'                                      
         DC    X'00'                                                            
 END                                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPNV01 08/29/00'                                      
         END                                                                    
