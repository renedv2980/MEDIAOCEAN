*          DATA SET CPREP7601  AT LEVEL 018 AS OF 09/01/00                      
*PHASE CP7601A                                                                  
         TITLE 'CPREP7601-COST PER POINT GUIDE'                                 
         PRINT NOGEN                                                            
CP7601   CSECT                                                                  
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,77,AGYNAME                                                    
         PSPEC H1,41,C'COST PER POINT GUIDE'                                    
         PSPEC H2,41,C'--------------------'                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,77,AGYADD                                                     
         PSPEC H3,1,RANGE                                                       
         PSPEC H4,41,PERIOD                                                     
         PSPEC H4,77,REPORT                                                     
         PSPEC H4,103,PAGE                                                      
*                                                                               
         PSPEC H9,1,C'RNK MARKET NAME'                                          
         PSPEC H10,1,C'--- -----------'                                         
         PSPEC H9,29,C'DATA'                                                    
         PSPEC H10,29,C'----'                                                   
         PSPEC H9,35,C' TOT   JAN   FEB   MAR   APR   MAY   JUN '               
         PSPEC H10,35,C'----- ----- ----- ----- ----- ----- -----'              
         PSPEC H9,77,C' JUL   AUG   SEP   OCT   NOV   DEC '                     
         PSPEC H10,77,C'----- ----- ----- ----- ----- -----'                    
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018CPREP7601 09/01/00'                                      
         END                                                                    
