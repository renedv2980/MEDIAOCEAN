*          DATA SET PPREPB101  AT LEVEL 036 AS OF 07/08/16                      
*PHASE PPB101A,+0,NOAUTO                                                        
         TITLE 'PPB101 - NEW PRINT BILLING SPECS'                               
PPB101   CSECT                                                                  
         PRINT NOGEN                                                            
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
         SPROG 20,21               INVOICE REGISTER                             
*                                  21 FOR AOR                                   
         SPACE 2                                                                
         PSPEC H4,1,AGYNAME                                                     
         PSPEC H5,1,AGYADD                                                      
         PSPEC H5,40,REPORT                                                     
         PSPEC H6,40,RUN                                                        
         PSPEC H6,65,PAGE                                                       
         PSPEC H13,132,C'*'                                                     
*                                                                               
         PSPEC M1,1,C'INV.'                                                     
         PSPEC M1,20,C'BILL'                                                    
         PSPEC M1,28,C'ACTUAL'                                                  
         PSPEC M1,60,C'AGENCY'                                                  
         PSPEC M1,96,C'CASH'                                                    
         PSPEC M2,2,C'NO. PRD EST MONTH TYP  BILL AMOUNT        NET COSX        
               T      COMMISSION      GROSS COST          DISCOUNT'             
         SPROG 22,23                                                            
*                                23 FOR AOR                                     
         PSPEC H4,1,AGYNAME                                                     
         PSPEC H5,1,AGYADD                                                      
         PSPEC H5,40,REPORT                                                     
         PSPEC H6,40,RUN                                                        
         PSPEC H6,65,PAGE                                                       
         PSPEC H13,132,C'*'                                                     
*                                                                               
         PSPEC M1,1,C'INV.'                                                     
         PSPEC M1,20,C'BILL'                                                    
         PSPEC M1,28,C'ACTUAL'                                                  
         PSPEC M1,60,C'AGENCY'                                                  
         PSPEC M1,96,C'CASH'                                                    
         PSPEC M1,44,C'NET LESS'                                                
         PSPEC M2,2,C'NO. PRD EST MONTH TYP  BILL AMOUNT       CASH DISX        
               C      COMMISSION      GROSS COST          DISCOUNT'             
         SPACE 2                                                                
         SPROG 20,22                                                            
         PSPEC H4,40,C'PRINT BILLING INVOICE REGISTER'                          
         SPROG 21,23                                                            
         PSPEC H4,40,C'PRINT AOR BILLING INVOICE REGISTER'                      
         SPROG 100,101             DUMMY SPROG TO CATEGORY LINES                
         SPACE 2                                                                
         PSPEC H2,1,DIVISION                                                    
         PSPEC H5,1,PRODUCT                                                     
         PSPEC H6,1,ESTIMATE                                                    
         PSPEC H8,1,REGION                                                      
         PSPEC H9,1,DISTRICT                                                    
         SPACE 2                                                                
         SPROG 100                                                              
         PSPEC H1,1,CLIENT                                                      
         SPROG 101                                                              
*                                SPECIAL FOR LONG CLIENT NAMES                  
*                                DISPLAYS CLIENT NAME AND                       
*                                FIRST LINE OF OLD BILL RECEIPT NAME            
         PSPEC H1,1,CLIADD                                                      
*                                                                               
         DC    X'00'                                                            
         DC    C'REQLST='                                                       
         DC    X'01020304050607081D0C0D131400'                                  
         DC    CL25'5306DINVOICE DATE'                                          
         DC    CL25'5902NDUE DAYS'                                              
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'036PPREPB101 07/08/16'                                      
         END                                                                    
