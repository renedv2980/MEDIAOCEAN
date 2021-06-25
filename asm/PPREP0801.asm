*          DATA SET PPREP0801  AT LEVEL 010 AS OF 08/09/00                      
*PHASE PP0801A                                                                  
         TITLE 'PP0801 - I/O PURGE SPECS'                                       
PP0801   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         PSPEC H1,52,C'PRINTPAK I/O PURGE PROGRAM'                              
         PSPEC H2,52,C'--------------------------'                              
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         PSPEC H7,1,C'CLT  PRD  EST   PUBLICATION   INSERTION DATE'             
         PSPEC H8,1,C'---  ---  ---   -----------   --------------'             
         PSPEC H7,48,C'I/O NUMBER  TYPE  SOURCE'                                
         PSPEC H8,48,C'----------  ----  ------'                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREP0801 08/09/00'                                      
         END                                                                    
