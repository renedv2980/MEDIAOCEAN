*          DATA SET SPREPSD01  AT LEVEL 007 AS OF 08/29/00                      
*PHASE SPSD01A                                                                  
         TITLE 'SPSD01 --  SID RECORD REPORT - SPECS'                           
SPSD01   CSECT                                                                  
         PRINT NOGEN                                                            
         FSPEC USE,SP0003                                                       
*                                                                               
         SSPEC H1,2,REQUESTOR                                                   
         SSPEC H1,45,C'SID RECORD REPORT'                                       
         SSPEC H1,77,AGYNAME                                                    
         SSPEC H2,45,C'--- ------ ------'                                       
         SSPEC H2,77,REPORT                                                     
         SSPEC H2,100,PAGE                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007SPREPSD01 08/29/00'                                      
         END                                                                    
