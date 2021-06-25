*          DATA SET CPREP0201  AT LEVEL 004 AS OF 09/01/00                      
*PHASE CP0201A                                                                  
         TITLE 'SPECS FOR CPP DATA LIST'                                        
CP0201   CSECT                                                                  
         PRINT NOGEN                                                            
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H1,49,C'CPP DATA LIST'                                           
         PSPEC H2,49,13C'-'                                                     
         PSPEC H1,77,AGYNAME                                                    
         PSPEC H2,77,AGYADD                                                     
         PSPEC H4,1,RANGE                                                       
         PSPEC H4,45,PERIOD                                                     
         PSPEC H4,77,REPORT                                                     
         PSPEC H4,103,PAGE                                                      
         PSPEC H7,1,C'MARKET NAME             ----MARKET---  TARGET'            
         PSPEC H8,1,C'-----------             CODE RANK PCT  ------'            
         PSPEC H7,48,C'DEMO.   DAY- SPT P A EQUV MONTH SPOTS'                   
         PSPEC H8,48,C'-----   PART LEN C F ---- ----- -----'                   
         PSPEC H7,88,C'CASH  GRPS  IMPS   CPP'                                  
         PSPEC H8,88,C'----  ----  ----   ---'                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CPREP0201 09/01/00'                                      
         END                                                                    
