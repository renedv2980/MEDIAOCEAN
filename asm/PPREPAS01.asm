*          DATA SET PPREPAS01  AT LEVEL 021 AS OF 08/09/00                      
*PHASE PPAS01A                                                                  
         TITLE 'PPAS01 - AGENCUY COPY SPECS'                                    
PPAS01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
*                                                                               
         SPROG 0,10,20                                                          
         PSPEC H1,52,C'PRINTPAK AGENCY COPY PROGRAM'                            
         PSPEC H3,49,PERIOD                                                     
         PSPEC H2,52,C'----------------------------'                            
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         SPROG 10                                                               
         PSPEC H7,2,C'CLT PRD  EST  PUBLICATION'                                
         PSPEC H8,2,C'--- ---  ---  -----------'                                
         PSPEC H7,42,C'NET             CD            TAX      GST BASISX        
                           GST       GST PAID'                                  
         PSPEC H8,42,C'---             --            ---      ---------X        
                           ---       --------'                                  
         SPROG 20                                                               
         PSPEC H7,5,C'MED  CLT  PUBLICATION'                                    
         PSPEC H8,5,C'---  ---  -----------'                                    
         PSPEC H7,42,C'NET             CD            TAX      GST BASISX        
                           GST       GST PAID'                                  
         PSPEC H8,42,C'---             --            ---      ---------X        
                           ---       --------'                                  
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021PPREPAS01 08/09/00'                                      
         END                                                                    
