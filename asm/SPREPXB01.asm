*          DATA SET SPREPXB01  AT LEVEL 010 AS OF 08/29/00                      
*PHASE SPXB01A                                                                  
         TITLE 'SPREPXB01 - BRAND AGENCY INTERFACE'                             
SPXB01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,1,MEDIA                                                       
         SSPEC H2,1,CLIENT                                                      
         SSPEC H3,1,REQUESTOR                                                   
         SSPEC H1,50,C'BRAND AGENCY INTERFACE'                                  
         SSPEC H2,50,C'----------------------'                                  
         SSPEC H1,100,AGYNAME                                                   
         SSPEC H2,100,PAGE                                                      
         SSPEC H3,100,REPORT                                                    
         SSPEC H4,1,C' SYS  MED  CLT  PRD  EST  AGY CODE'                       
         SSPEC H5,1,C' ---  ---  ---  ---  ---  --------'                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010SPREPXB01 08/29/00'                                      
         END                                                                    
