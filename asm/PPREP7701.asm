*          DATA SET PPREP7701  AT LEVEL 023 AS OF 08/07/02                      
*PHASE PP7701A,+0                                                               
         TITLE 'PP7701 - PRINTPAK TRAFFIC LIST - SPECS'                         
*   4/21/88 ALLOW FOR DIFFERENT HEADING FOR EXPANDED STANDARD COMMENTS          
*           SUBPROGRAM 20                                                       
*                                                                               
PP7701   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
*                                                                               
         SPROG 0,1,2,3,10,11,12,13,20                                           
*                                                                               
         PSPEC H1,2,MEDIA                                                       
         PSPEC H2,2,REQUESTOR                                                   
*                                                                               
         PSPEC H1,56,C'TRAFFIC LIST'                                            
         PSPEC H2,56,C'------------'                                            
         PSPEC H3,51,PERIOD                                                     
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,122,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         SPROG 20                                                               
         PSPEC H5,47,C'** STANDARD COMMENT GLOSSARY **'                         
         PSPEC H7,3,C'CODE'                                                     
         PSPEC H7,12,C'DESCRIPTION'                                             
         PSPEC H8,3,C'----'                                                     
         PSPEC H8,12,C'-----------'                                             
*                                                                               
         SPROG 0,1,2,3,10,11,12,13                                              
         PSPEC H10,55,C'INSERT'                                                 
         PSPEC H10,67,C'CLOSING'                                                
         PSPEC H10,98,C'INS ORDR'                                               
         PSPEC H10,125,C'REPEAT'                                                
         PSPEC H11,2,C'CLT PRD AD NO. PUBLICATION'                              
         PSPEC H12,2,C'--- --- ------ -----------'                              
         PSPEC H11,33,C'PUB NAME               DATE  '                          
         PSPEC H12,33,C'--------             --------'                          
         PSPEC H11,67,C'  DATE   EST SPACE DESCRIPTION   DATE  '                
         PSPEC H12,67,C'-------- --- ----------------- --------'                
         PSPEC H11,107,C'TYP   ORDER NO.     OF   '                             
         PSPEC H12,107,C'--- ------------ --------'                             
*                                                                               
*                                                                               
         SPROG 10,11,12,13                                                      
         PSPEC H11,80,C'    GROSS COST   '                                      
         PSPEC H12,80,C'    ----------   '                                      
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
         DC    X'0102041F070815090A0B0C0D0E0F131E1400'                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023PPREP7701 08/07/02'                                      
         END                                                                    
