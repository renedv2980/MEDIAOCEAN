*          DATA SET PPREP7801  AT LEVEL 005 AS OF 08/09/00                      
*PHASE PP7801A                                                                  
         TITLE 'PP7801 - PRINTPAK TRAFFIC LIST - SPECS'                         
PP7801   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         RSPEC REQUEST,NOREP                                                    
*                                                                               
         FSPEC READ,BUYS                                                        
         FSPEC GET,PUBS                                                         
*                                                                               
         SPROG 0,1,2,3,10,11,12,13                                              
*                                                                               
         PSPEC H1,2,MEDIA                                                       
         PSPEC H2,2,REQUESTOR                                                   
*                                                                               
         PSPEC H1,53,C'TRAFFIC TURNAROUND'                                      
         PSPEC H2,53,C'------------------'                                      
         PSPEC H3,51,PERIOD                                                     
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,122,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         PSPEC H9,67,C'MATERIALS'                                               
         PSPEC H10,47,C'INSERT'                                                 
         PSPEC H10,59,C'CLOSING'                                                
         PSPEC H10,68,C'CLOSING'                                                
         PSPEC H10,95,C'INS ORDR'                                               
         PSPEC H10,122,C'REPEAT'                                                
         PSPEC H11,2,C'AD NO. PUBLICATION'                                      
         PSPEC H12,2,C'------ -----------'                                      
         PSPEC H11,25,C'PUB NAME               DATE  '                          
         PSPEC H12,25,C'--------             --------'                          
         PSPEC H11,59,C'  DATE     DATE   SPACE DESCRIPTION   DATE'             
         PSPEC H12,59,C'-------- -------- ----------------- --------'           
         PSPEC H11,104,C'TYP   ORDER NO.     OF   '                             
         PSPEC H12,104,C'--- ------------ --------'                             
*                                                                               
*                                                                               
         SPROG 10,11,12,13                                                      
         PSPEC H11,77,C'    GROSS COST   '                                      
         PSPEC H12,77,C'    ----------   '                                      
         SPROG 1,2,3,11,12,13                                                   
         PSPEC H4,2,CLIENT                                                      
*                                                                               
         SPROG 2,3,12,13                                                        
         PSPEC H5,2,PRODUCT                                                     
*                                                                               
         SPROG 3,13                                                             
         PSPEC H6,2,ESTIMATE                                                    
         DC    X'00'                                                            
*                                                                               
         DC    C'REQLST='                                                       
         DC    X'010204070815090A0B0C0D0E0F131400'                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005PPREP7801 08/09/00'                                      
         END                                                                    
