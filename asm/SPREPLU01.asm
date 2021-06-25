*          DATA SET SPREPLU01  AT LEVEL 008 AS OF 08/29/00                      
*PHASE SPLU01A                                                                  
         TITLE 'USER DEMO MENU LISTING REPORT HEADINGS'                         
         PRINT NOGEN                                                            
SPLU01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,39,C'USER DEMO MENU LISTING'                                  
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H2,39,C'---- ---- ---- -------'                                  
         SSPEC H2,77,AGYADD                                                     
         SSPEC H4,1,CLIENT                                                      
         SSPEC H4,77,PAGE                                                       
         SSPEC H4,85,REPORT                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008SPREPLU01 08/29/00'                                      
         END                                                                    
