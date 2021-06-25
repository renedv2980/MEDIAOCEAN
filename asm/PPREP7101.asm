*          DATA SET PPREP7101  AT LEVEL 012 AS OF 07/18/16                      
*PHASE PP7101A                                                                  
         TITLE 'PP7101 - PRINTPAK INSERTION ORDERS - PPG SPECS'                 
PP7101   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         RSPEC MAXLINES,75                                                      
         RSPEC REQUEST,NOREP                                                    
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC READ,BUYS                                                        
         FSPEC READ,ACTIVE                                                      
         FSPEC GET,PUB                                                          
         FSPEC GET,TRAFRE                                                       
*                                                                               
*                                                                               
*                                  SPECS FOR JOBLIST                            
         SPROG 75                                                               
*                                                                               
         PSPEC H1,2,MEDIA                                                       
         PSPEC H2,2,CLIENT                                                      
         PSPEC H3,2,PRODUCT                                                     
         PSPEC H5,2,REQUESTOR                                                   
*                                                                               
         PSPEC H1,45,C'AD RECORD LISTING'                                       
         PSPEC H2,45,C'-----------------'                                       
         PSPEC H3,42,PERIOD                                                     
*                                                                               
         PSPEC H1,75,AGYNAME                                                    
         PSPEC H2,75,AGYADD                                                     
         PSPEC H4,75,REPORT                                                     
         PSPEC H4,101,PAGE                                                      
         PSPEC H5,75,RUN                                                        
*                                                                               
         PSPEC H7,4,C'AD'                                                       
         PSPEC H8,4,C'NO.    CAPTION                   COPY NUMBER     X        
                  SPACE               START     END     SIGNATURE'              
         PSPEC H9,4,C'---    -------                   -----------     X        
                  -----              -------- --------  ---------'              
*                                                                               
*                                                                               
         SPROG 85                                                               
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,47,AGYNAME                                                    
         PSPEC H2,47,AGYADD                                                     
         PSPEC H3,47,REPORT                                                     
         PSPEC H3,68,PAGE                                                       
         PSPEC H4,47,RUN                                                        
*                                                                               
         PSPEC H7,28,C'INSERT'                                                  
         PSPEC H7,52,C'INSERTION'                                               
         PSPEC H7,74,C'REPEAT'                                                  
         PSPEC H8,1,C'CLT PRD PUBLICATION         DATE       EST'               
         PSPEC H8,44,C'AD NO.  ORDER NO.   FROM TYP  OF DATE'                   
         PSPEC H9,1,C'--- --- -----------       --------     ---'               
         PSPEC H9,44,C'------ ------------ ---- --- --------'                   
*                                                                               
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012PPREP7101 07/18/16'                                      
         END                                                                    
