*          DATA SET PPREP0201F AT LEVEL 021 AS OF 01/30/97                      
*PHASE PP0201F,+0                                                               
         TITLE 'PP0201 - PRTFIX SPECS'                                          
PP0201   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
*                                                                               
         SPROG 0,10,20,30                                                       
         PSPEC H1,52,C'PRINTPAK RECORD FIX PROGRAM'                             
         PSPEC H3,49,PERIOD                                                     
         PSPEC H2,52,C'---------------------------'                             
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
         SPROG 30                                                               
         PSPEC H7,13,C'-------  OLD  -------'                                   
         PSPEC H7,37,C'-------  NEW  -------'                                   
         PSPEC H8,2,C'AGY  CLT   OFF  ACC OFF  ACC AGY'                         
         PSPEC H8,37,C'OFF  ACC OFF  ACC AGY'                                   
         PSPEC H9,2,C'---  ---   ---  -------  -------'                         
         PSPEC H9,37,C'---  -------  -------'                                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021PPREP0201F01/30/97'                                      
         END                                                                    
