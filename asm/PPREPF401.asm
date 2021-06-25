*          DATA SET PPREPF401  AT LEVEL 010 AS OF 04/27/07                      
*PHASE PPF401A,+0                                                               
         TITLE 'PPF401 - PRINTPAK BUCKET CREATION SPECS'                        
PPF401   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         PSPEC H1,38,C'PRINTPAK ESTIMATE BUCKET CREATION'                       
         PSPEC H2,38,C'---------------------------------'                       
*                                                                               
         PSPEC H1,1,MEDIA                                                       
         PSPEC H3,1,CLIENT                                                      
*                                                                               
         PSPEC H1,76,AGYNAME                                                    
         PSPEC H2,76,AGYADD                                                     
         PSPEC H4,76,REPORT                                                     
         PSPEC H5,76,RUN                                                        
         PSPEC H6,76,PAGE                                                       
*                                                                               
         PSPEC H8,21,C'  ORDERED GROSS    ORDERED NET     ORDERED CD   X        
                  PAID GROSS      PAID NET        PAID CD'                      
         PSPEC H9,21,C'  -------------    -----------     ----------   X        
                  ----------      --------        -------'                      
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREPF401 04/27/07'                                      
         END                                                                    
