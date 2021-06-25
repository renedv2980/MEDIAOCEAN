*          DATA SET PPREPRA01  AT LEVEL 042 AS OF 07/22/93                      
*PHASE PPRA01A,+0                                                               
         TITLE 'PPRA01 REBATE ANALYSIS'                                         
PPRA01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         FSPEC READ,BUYS                                                        
*        FSPEC GET,PUBS                                                         
         RSPEC REQUEST,REPORT                                                   
         RSPEC REQUEST,SUMMARY                                                  
         RSPEC SINGLE,SPACING                                                   
         SPROG 0,10,11,20,21                                                    
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,58,C'REBATE ANALYSIS'                                         
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,58,C'---------------'                                         
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,1,CLIENT                                                      
         PSPEC H3,54,PERIOD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         PSPEC H5,1,ESTIMATE                                                    
         PSPEC H6,1,C'ADCODE'                                                   
         PSPEC H7,60,C'INSERTION'                                               
         PSPEC H8,9,C'ADCODE'                                                   
         PSPEC H8,18,C'CAPTION'                                                 
         PSPEC H8,46,C'PUBLICATION'                                             
         PSPEC H8,63,C'DATE'                                                    
         PSPEC H8,74,C'DOC#'                                                    
         SPROG 0,10,11                                                          
         PSPEC H8,90,C'OPEN'                                                    
         PSPEC H8,103,C'CONTRACT'                                               
         PSPEC H8,118,C'DIFFERENCE'                                             
         PSPEC H9,9,C'------   -------'                                         
         PSPEC H9,46,C'-----------   ---------     ----'                        
         PSPEC H9,90,C'----         --------       ----------'                  
         SPROG 20,21                                                            
         PSPEC H7,90,C'OPEN'                                                    
         PSPEC H7,103,C'CONTRACT'                                               
         PSPEC H8,90,C'COST'                                                    
         PSPEC H8,103,C'  COST  '                                               
         PSPEC H8,118,C'DIFFERENCE'                                             
         PSPEC H9,9,C'------   -------'                                         
         PSPEC H9,46,C'-----------   ---------     ----'                        
         PSPEC H9,90,C'----         --------       ----------'                  
         SPROG 10,20                                                            
         PSPEC H4,1,PRODUCT                                                     
         SPROG 11,21                                                            
         PSPEC H4,1,C'PRODUCT  ALL PRODUCTS'                                    
         PSPEC H8,2,C'PRD'                                                      
         PSPEC H9,2,C'---'                                                      
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'010203040708090A0B0C0D0E0F1314'                                
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'042PPREPRA01 07/22/93'                                      
         END                                                                    
