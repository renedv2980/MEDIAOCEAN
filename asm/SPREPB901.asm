*          DATA SET SPREPB901  AT LEVEL 006 AS OF 08/29/00                      
*PHASE SPB901A                                                                  
         TITLE 'SPREPB901 - SPOT/ RETAIL BILLING REPORT - SPECS'                
         PRINT NOGEN                                                            
SPB901   CSECT                                                                  
         SPACE 2                                                                
         FSPEC USE,SP0003                                                       
*                                                                               
*                                                                               
         SPROG 0,10,20,30                                                       
         PSPEC H1,1,MEDIA                                                       
         PSPEC H2,1,CLIENT                                                      
         PSPEC H1,98,AGYNAME                                                    
         PSPEC H2,98,AGYADD                                                     
         PSPEC H3,98,REPORT                                                     
         PSPEC H3,122,PAGE                                                      
*                                                                               
*                                                                               
         SSPEC H1,48,C'MEDIA/RETAIL BILLING REFERENCE LIST'                     
         SSPEC H2,48,C'-----------------------------------'                     
*                                                                               
         SPROG 10                                                               
         SSPEC H4,51,C'** SCHEME PERCENTAGE TABLE **'                           
*                                                                               
         SPROG 20                                                               
         SSPEC H4,53,C'** MEDIA ESTIMATE LIST **'                               
*                                                                               
         SPROG 30                                                               
         SSPEC H4,53,C' ** SCHEME USAGE LIST ** '                               
*                                                                               
         DC    X'0000'                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006SPREPB901 08/29/00'                                      
         END                                                                    
