*          DATA SET PPREPAX01  AT LEVEL 012 AS OF 07/02/07                      
*PHASE PPAX01A                                                                  
         TITLE 'PPAX01 - PRINT CLEARNACE UPDATE'                                
PPAX01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC UPDATE,PRTFILE                                                   
         FSPEC UPDATE,PRTDIR                                                    
*                                                                               
         PSPEC H1,1,REPORT                                                      
         PSPEC H2,1,REQUESTOR                                                   
         PSPEC H2,26,PAGE                                                       
         PSPEC H1,57,C'PRINT CLEARANCE UPDATE'                                  
         PSPEC H2,57,C'----------------------'                                  
         PSPEC H4,2,C' ALPHA ID    MEDIA   CLIENT       PUB'                    
         PSPEC H4,44,C'     DATE    CHECK NUMBER   SEQ NUM  BANK CLEAR'         
         PSPEC H5,2,C' --------    -----   ------       ---'                    
         PSPEC H5,44,C'     ----    ------------   -------  ----------'         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012PPREPAX01 07/02/07'                                      
         END                                                                    
