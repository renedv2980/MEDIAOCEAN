*          DATA SET PPREPFP01  AT LEVEL 009 AS OF 02/18/97                      
*PHASE PPFP01A,+0                                                               
         TITLE 'PPFP01 - PRTFIX SPECS'                                          
PPFP01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
         FSPEC UPDATE,PUBFILE                                                   
         FSPEC UPDATE,PUBDIR                                                    
         FSPEC READ,BUYS                                                        
*                                                                               
         PSPEC H1,52,C'PRINTPAK RECORD FIX PROGRAM'                             
         PSPEC H3,54,PERIOD                                                     
         PSPEC H2,52,C'---------------------------'                             
         PSPEC H1,2,MEDIA                                                       
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H2,2,C'PUB NUM='                                                 
         PSPEC H2,11,PUBNUM                                                     
         PSPEC H3,11,PUBNAME                                                    
         PSPEC H4,11,ZONENAME                                                   
         PSPEC H4,98,REPORT                                                     
         PSPEC H4,118,PAGE                                                      
         PSPEC H5,98,RUN                                                        
         PSPEC H7,2,C'CLT  PRD  EST  INS DATE  BILLED  PAID'                    
         PSPEC H8,2,C'---  ---  ---  --------  ------  ----'                    
         PSPEC H7,41,C'GST  PST CODES   DISK ADD'                               
         PSPEC H8,41,C'---  ----------  --------'                               
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009PPREPFP01 02/18/97'                                      
         END                                                                    
