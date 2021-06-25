*          DATA SET SPREPB101  AT LEVEL 026 AS OF 07/08/16                      
*PHASE SPB101A                                                                  
         TITLE 'SPB101 - NEW SPOT BILLING SPECS'                                
SPB101   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,SPTFILE                                                   
         FSPEC UPDATE,SPTDIR                                                    
         FSPEC READ,BUYS                                                        
*                                                                               
         SPROG 10                  NORMAL                                       
         SPACE 2                                                                
         SPROG 20,21               INVOICE REGISTER                             
         SPACE 2                                                                
         SSPEC H4,1,AGYNAME                                                     
         SSPEC H5,1,AGYADD                                                      
         SSPEC H5,40,REPORT                                                     
         SSPEC H9,1,MEDIA                                                       
         SSPEC H13,132,C'*'                                                     
*                                                                               
*--->    SSPEC M1,27,C'BILL'                                                    
         SSPEC M1,27,SP#BILL,4                                                  
*--->    SSPEC M1,44,C'ACTUAL'                                                  
         SSPEC M1,44,SP#ACTL,6                                                  
*--->    SSPEC M1,77,C'AGENCY'                                                  
         SSPEC M1,77,SP#AGY,6                                                   
*--->    SSPEC  M2,2,C'INVOICE  PRD EST  PERIOD TYPE MARKET   BILL AMOUX        
               NT        NET COST      COMMISSION        GROSS COST'            
         SPACE 2                                                                
         SSPEC M2,2,SP#INV,7                                                    
         SSPEC M2,11,SP#PRO,3                                                   
         SSPEC M2,15,SP#EST,3                                                   
         SSPEC M2,19,SP#PERD,7,R                                                
         SSPEC M2,27,SP#TYPE,4                                                  
         SSPEC M2,32,SP#MRKT,6                                                  
         SSPEC M2,41,SP#BILAM,11                                                
         SSPEC M2,58,SP#NETAM,11                                                
         SSPEC M2,74,SP#COMMS,10                                                
         SSPEC M2,90,SP#GRSAM,13                                                
         SPACE 2                                                                
         SPROG 20                  REGULAR BILLS                                
*--->    SSPEC H4,40,C'SPOT BILLING INVOICE REGISTER'                           
         SSPEC H4,31,SP#SPIR,40,C                                               
         SPROG 21                  AOR INVOICE REGISTER                         
*--->    SSPEC H4,38,C'SPOT AOR BILLING INVOICE REGISTER'                       
         SSPEC H4,31,SP#SPAIR,40,C                                              
         SPACE 2                                                                
         SPROG 30                  RETAIL DRAFT                                 
         SPACE 2                                                                
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H1,70,AGYNAME                                                    
         SSPEC H2,70,REPORT                                                     
         SSPEC H2,96,PAGE                                                       
*--->    SSPEC H1,35,C'RETAIL DISTRIBUTOR SUMMARY'                              
         SSPEC H4,28,SP#RDS,40,C                                                
         SPACE 2                                                                
         SPROG 100                 DUMMY SPROG TO CATEGORY LINES                
         SPACE 2                                                                
         SSPEC H1,1,CLIENT                                                      
         SSPEC H2,1,PRDGRP                                                      
         SSPEC H5,1,PRODUCT                                                     
         SSPEC H6,1,ESTIMATE                                                    
         SSPEC H7,1,MKTGRP                                                      
         SSPEC H10,1,MARKET                                                     
         SSPEC H11,1,STATION                                                    
         SPACE 2                                                                
*                                                                               
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    CL25'3602ADUE DAYS'                                              
         DC    CL25'5012AMANUAL AMOUNT'                                         
         DC    CL25'6201AOPTION 1'                                              
         DC    CL25'6301AOPTION 2'                                              
         DC    CL25'6401AOPTION 3'                                              
         DC    CL25'6501AOPTION 4'                                              
         DC    CL25'6601AOPTION 5'                                              
         DC    X'00'                                                            
*                                                                               
         DC    C'REQ2LST='                                                      
         DC    CL25'2106DINVOICE DATE'                                          
         DC    X'00'                                                            
*                                                                               
* INCLUDE SPDDEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE SPDDEQUS                                                       
         PRINT ON                                                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'026SPREPB101 07/08/16'                                      
         END                                                                    
