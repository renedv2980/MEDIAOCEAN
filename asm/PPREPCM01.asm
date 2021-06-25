*          DATA SET PPREPCM01  AT LEVEL 002 AS OF 02/09/01                      
*PHASE PPCM01A                                                                  
         TITLE 'PP0301 - PRTFIX SPECS'                                          
PPCM01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
*                                                                               
         SPROG 0,10,20                                                          
         PSPEC H1,53,C'PRINTPAK PRTFILE HEADER COPY'                            
         PSPEC H2,53,C'----------------------------'                            
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H3,118,PAGE                                                      
         PSPEC H4,98,RUN                                                        
*                                                                               
         SPROG 10                                                               
         PSPEC H5,59,C'CLIENT HEADERS'                                          
         PSPEC H6,59,C'--------------'                                          
         PSPEC H07,6,C'TO'                                                      
         PSPEC H08,1,C'AGY  MED CLT       CLIENT NAME'                          
         PSPEC H09,1,C'---  --- ---       -----------'                          
*                                                                               
         SPROG 20                                                               
         PSPEC H5,59,C'PRODUCT HEADERS'                                         
         PSPEC H6,59,C'---------------'                                         
         PSPEC H07,6,C'TO'                                                      
         PSPEC H08,1,C'AGY  MED CLT  PRD  PRODUCT NAME'                         
         PSPEC H09,1,C'---  --- ---  ---  ------------'                         
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PPREPCM01 02/09/01'                                      
         END                                                                    
