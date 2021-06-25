*          DATA SET SPREPVK01  AT LEVEL 001 AS OF 07/07/20                      
*******20200113:042615: NEW MEMBER ADDED BY HWON FOR PROJ# SPEC-36764           
*******        :042615: SPREPVK01/02 - NEW SPOT VENDOR LOCK REQUEST             
*PHASE SPVK01C                                                                  
         TITLE 'SPREPVK01-SPOT BUY VENDOR LOCK SPECS'                           
         PRINT NOGEN                                                            
SPVK01   CSECT                                                                  
         FSPEC USE,SP0003                                                       
         FSPEC UPDATE,STATION                                                   
         SPROG 0,1                                                              
         SSPEC H1,1,MEDIA                                                       
         SSPEC H1,60,C'SPOT BUY VENDOR LOCK'                                    
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,1,REQUESTOR                                                   
         SSPEC H3,100,PAGE                                                      
         SSPEC H3,111,REPORT                                                    
         SSPEC H5,5,C'MEDIA     STATION   CLT-SPECIFIC'                         
         SSPEC H6,5,C'-----     -------   ------------'                         
         SPROG 0                                                                
         SPROG 1                                                                
         SSPEC H1,52,C'*DRAFT*'                                                 
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPVK01 07/07/20'                                      
         END                                                                    
