*          DATA SET PPREP0201P AT LEVEL 010 AS OF 03/27/97                      
*PHASE PP0201P,+0                                                               
         TITLE 'PP0201 - PRTFIX SPECS - READS PUBS'                             
PP0201   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PUBFILE                                                   
         FSPEC UPDATE,PUBDIR                                                    
         FSPEC READ,PUBS                                                        
*                                                                               
         SPROG 0,10                                                             
         PSPEC H1,52,C'PRINTPAK PUBFILE RECORD FIX PROGRAM'                     
         PSPEC H3,49,PERIOD                                                     
         PSPEC H2,52,C'-----------------------------------'                     
         PSPEC H1,1,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
*                                                                               
         SPROG 10                                                               
         PSPEC H7,2,C'AG CLT PRD  EST  PUB'                                     
         PSPEC H8,2,C'-- --- ---  ---  -----------'                             
         PSPEC H7,37,C'BUY DATE    PAID  BILL  PST'                             
         PSPEC H8,37,C'----------- ----  ----'                                  
         PSPEC H8,61,C'------------------------'                                
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREP0201P03/27/97'                                      
         END                                                                    
