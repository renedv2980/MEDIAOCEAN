*          DATA SET PPREPFX01  AT LEVEL 015 AS OF 08/09/00                      
*PHASE PPFX01A                                                                  
         TITLE 'PPFX01 - PRINTPAK INVOICE FIXING SPECS'                         
PPFX01   CSECT                                                                  
         PRINT NOGEN                                                            
         SPACE 2                                                                
         FSPEC READ,BILLS                                                       
         FSPEC UPDATE,PRTFILE                                                   
         SPACE 2                                                                
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,55,C'PRINTPAK INVOICE FIXING'                                 
         PSPEC H2,55,C'-----------------------'                                 
         PSPEC H3,56,PERIOD                                                     
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,122,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         PSPEC H3,1,REQUESTOR                                                   
*                                                                               
         PSPEC  H8,1,C'                BILLING      INV.    RUN    BILLX        
                                                             CASH      X        
                  AGENCY        ACTUAL   '                                      
         PSPEC  H9,1,C'CLT PRD EST       JOB       NUMBER   DATE   DATEX        
                  TYPE   GROSS AMOUNT     NET AMOUNT       DISCOUNT    X        
                COMMISSION    BILL AMOUNT'                                      
         PSPEC H10,1,C'--- --- ---     -------     ------ -------- ----X        
               - ------- ------------     ----------       --------    X        
                ----------    -----------'                                      
*                                                                               
         DC    X'0000'                                                          
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'015PPREPFX01 08/09/00'                                      
         END                                                                    
