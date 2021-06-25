*          DATA SET CPREP7301  AT LEVEL 013 AS OF 09/01/00                      
*PHASE CP7301                                                                   
         TITLE 'CPREP7301-COST PER POINT GUIDE'                                 
         PRINT NOGEN                                                            
CP7301   CSECT                                                                  
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
         PSPEC H9,41,C'  10    15    20    30    45    60  OTHER'               
         PSPEC H10,41,C'----- ----- ----- ----- ----- ----- -----'              
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013CPREP7301 09/01/00'                                      
         END                                                                    
