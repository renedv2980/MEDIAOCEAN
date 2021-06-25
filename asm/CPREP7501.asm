*          DATA SET CPREP7501  AT LEVEL 015 AS OF 09/01/00                      
*PHASE CP7501A                                                                  
         TITLE 'CPREP7501-COST PER POINT GUIDE'                                 
         PRINT NOGEN                                                            
CP7501   CSECT                                                                  
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
         PSPEC H9,41,C' PRI   EAM   DAY   WEM   WEA   ELY   PAC '               
         PSPEC H10,41,C'----- ----- ----- ----- ----- ----- -----'              
         PSPEC H9,82,C' LTE   LLT  OTHER'                                       
         PSPEC H10,82,C'----- ----- -----'                                      
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015CPREP7501 09/01/00'                                      
         END                                                                    
