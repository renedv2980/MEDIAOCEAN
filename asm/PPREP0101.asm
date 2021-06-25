*          DATA SET PPREP0101  AT LEVEL 018 AS OF 09/12/07                      
*PHASE PP0101A,+0,NOAUTO                                                        
         TITLE 'PP0101 - PRINT PURGE PROGRAM SPECS'                             
PP0101   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PUBDIR                                                    
         FSPEC UPDATE,PUBFILE                                                   
*                                                                               
         SPROG 10,11,20,21,30,31,40,41,50,51,60,61                              
         PSPEC H1,51,C'PRINTPAK RECORD PURGE PROGRAM'                           
         PSPEC H2,51,C'-----------------------------'                           
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         SPROG 10,11                                                            
         PSPEC H3,1,C'** PUB RECORD PURGE **'                                   
         PSPEC H4,4,C'---'                                                      
*                                                                               
         SPROG 20,21                                                            
         PSPEC H3,1,C'** REP RECORD PURGE **'                                   
         PSPEC H4,4,C'---'                                                      
*                                                                               
         SPROG 30,31                                                            
         PSPEC H3,1,C'** JOB RECORD PURGE **'                                   
         PSPEC H4,4,C'---'                                                      
*                                                                               
         SPROG 40,41                                                            
         PSPEC H3,1,C'** COMMENT RECORD PURGE **'                               
         PSPEC H4,4,C'-------'                                                  
*                                                                               
         SPROG 50,51                                                            
         PSPEC H3,1,C'** CLIENT/PRODUCT RECORD PURGE **'                        
         PSPEC H4,4,C'--------------'                                           
*                                                                               
         SPROG 60,61                                                            
         PSPEC H3,1,C'** PUB LIST RECORD PURGE **'                              
         PSPEC H4,4,C'--------'                                                 
*                                                                               
         SPROG 10,20,30,40,50,60                                                
         PSPEC H6,1,C'KEYS OF RECORDS USING DATA TO BE PURGED'                  
*                                                                               
         SPROG 11,21,31,41,51,61                                                
         PSPEC H6,1,C'LIST OF RECORDS PURGED'                                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'018PPREP0101 09/12/07'                                      
         END                                                                    
