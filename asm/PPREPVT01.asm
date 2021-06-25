*          DATA SET PPREPVT01  AT LEVEL 010 AS OF 04/11/91                      
*PHASE PPVT01A,+0,NOAUTO                                                        
         TITLE 'PPREPVT01 - VALUTECH INTERFACE'                                 
PPVT01   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC READ,BUYS                                                        
         FSPEC READ,ACTIVE                                                      
         FSPEC GET,PUBS                                                         
*                                                                               
         SPROG 0                                                                
         PSPEC H1,51,C'VALUTECH VENDOR FILE'                                    
         PSPEC H2,51,C'--------------------'                                    
*                                                                               
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H4,98,REPORT                                                     
         PSPEC H5,98,PAGE                                                       
*                                                                               
         PSPEC H2,1,MEDIA                                                       
*                                                                               
         PSPEC H7,02,C'VENDOR CODE       VENDOR CODE                   X        
                          C.D.%  DAYS         GROSS            NET  CASX        
               H DISCOUNT'                                                      
         PSPEC H8,02,C'-----------       -----------                   X        
                          -----  ----         -----            ---  ---X        
               ----------'                                                      
         DC    X'00'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010PPREPVT01 04/11/91'                                      
         END                                                                    
