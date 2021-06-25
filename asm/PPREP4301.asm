*          DATA SET PPREP4301  AT LEVEL 018 AS OF 07/18/16                      
*PHASE PP4301A,+0                                                               
PP4301   CSECT                                                                  
         PRINT NOGEN                                                            
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         SPROG 0,10                                                             
         PSPEC H1,1,MEDIANAME                                                   
         PSPEC H2,1,REQUESTOR                                                   
         SPROG 0                                                                
         PSPEC H6,4,C'CODE'                                                     
         PSPEC H7,4,4C'-'                                                       
         PSPEC H1,43,C'REP LISTING'                                             
         PSPEC H2,43,11C'-'                                                     
         PSPEC H6,11,C'NAME'                                                    
         PSPEC H6,43,C'ADDRESS'                                                 
         PSPEC H7,11,4C'-'                                                      
         PSPEC H7,43,7C'-'                                                      
*                                                                               
         PSPEC H1,77,AGYNAME                                                    
         PSPEC H2,77,AGYADD                                                     
         PSPEC H4,77,RUN                                                        
         PSPEC H5,77,REPORT                                                     
         PSPEC H5,102,PAGE                                                      
*                                                                               
         SPROG 10                                                               
         PSPEC H7,4,C'CODE'                                                     
         PSPEC H8,4,4C'-'                                                       
         PSPEC H1,40,C'PUBLISHER LISTING'                                       
         PSPEC H2,40,C'-----------------'                                       
         PSPEC H7,10,C'NAME/ADDRESS'                                            
         PSPEC H8,10,C'------------'                                            
         PSPEC H6,40,C'I/O RPT'                                                 
         PSPEC H7,40,C' CHECK '                                                 
         PSPEC H8,40,C'-------'                                                 
         PSPEC H7,49,C'PUB CODES'                                               
         PSPEC H8,49,C'---------'                                               
         PSPEC H7,67,C'PUBLICATIONS'                                            
         PSPEC H8,67,C'------------'                                            
         PSPEC H1,71,AGYNAME                                                    
         PSPEC H2,71,AGYADD                                                     
         PSPEC H4,71,RUN                                                        
         PSPEC H5,71,REPORT                                                     
         PSPEC H5,96,PAGE                                                       
*                                                                               
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018PPREP4301 07/18/16'                                      
         END                                                                    
