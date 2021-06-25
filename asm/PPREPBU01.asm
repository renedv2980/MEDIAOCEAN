*          DATA SET PPREPBU01  AT LEVEL 021 AS OF 08/09/00                      
*PHASE PPBU01A                                                                  
         TITLE 'PPBU01 - PRINTPAK BILLING SPECS'                                
PPBU01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         RSPEC LINEUP,PATTERN                                                   
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC READ,BUYS                                                        
         FSPEC GET,REGIONS                                                      
         FSPEC GET,DISTRICTS                                                    
         FSPEC GET,PUBS                                                         
*                                                                               
         SPROG 10                  NORMAL                                       
         SPACE 2                                                                
         SPROG 20                  INVOICE REGISTER                             
         SPACE 2                                                                
         PSPEC H4,1,AGYNAME                                                     
         PSPEC H5,1,AGYADD                                                      
         PSPEC H4,40,C'PRINT BILLING INVOICE REGISTER'                          
         PSPEC H5,40,REPORT                                                     
         PSPEC H9,1,MEDIA                                                       
         PSPEC H13,132,C'*'                                                     
*                                                                               
         PSPEC M1,27,C'BILL'                                                    
         PSPEC M1,44,C'ACTUAL'                                                  
         PSPEC M1,77,C'AGENCY'                                                  
         PSPEC  M2,2,C'INVOICE  PRD EST  PERIOD TYPE          BILL AMOUX        
               NT        NET COST      COMMISSION        GROSS COST'            
         SPACE 2                                                                
         SPROG 30                  RETAIL DRAFT                                 
         SPACE 2                                                                
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H1,70,AGYNAME                                                    
         PSPEC H2,70,REPORT                                                     
         PSPEC H2,96,PAGE                                                       
         PSPEC H1,35,C'RETAIL DISTRIBUTOR SUMMARY'                              
         PSPEC H2,35,C'--------------------------'                              
         SPACE 2                                                                
         SPROG 100                 DUMMY SPROG TO CATEGORY LINES                
         SPACE 2                                                                
         PSPEC H1,1,CLIENT                                                      
         PSPEC H2,1,DIVISION                                                    
         PSPEC H3,1,PRODUCT                                                     
         PSPEC H4,1,ESTIMATE                                                    
         PSPEC H5,1,REGION                                                      
         PSPEC H6,1,DISTRICT                                                    
         PSPEC H7,1,PUBLICATION                                                 
         SPACE 2                                                                
*                                                                               
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'3006AINVOICE DATE'                                          
         DC    CL25'3602ADUE DAYS'                                              
         DC    CL25'5012AMANUAL AMOUNT'                                         
         DC    CL25'6201AOPTION 1'                                              
         DC    CL25'6301AOPTION 2'                                              
         DC    CL25'6401AOPTION 3'                                              
         DC    CL25'6501AOPTION 4'                                              
         DC    CL25'6601AOPTION 5'                                              
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021PPREPBU01 08/09/00'                                      
         END                                                                    
