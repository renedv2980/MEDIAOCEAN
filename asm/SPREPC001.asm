*          DATA SET SPREPC001  AT LEVEL 022 AS OF 09/19/02                      
*PHASE SPC001A                                                                  
         TITLE 'SPREPC001 - COLGATE INTERFACE'                                  
SPC001   CSECT                                                                  
         PRINT NOGEN                                                            
*                                                                               
         FSPEC USE,SP0003                                                       
*                                                                               
         SPROG 0                                                                
         SSPEC H1,51,C'COLGATE SPOTPAK BILLING EXTRACT'                         
         SSPEC H2,51,C'-------------------------------'                         
*                                                                               
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,98,REPORT                                                     
         SSPEC H5,98,PAGE                                                       
*                                                                               
         DC    X'00'                                                            
         SPACE 2                                                                
         DC    C'REQLST='                                                       
         DC    CL25'5004ASTART MOS'                                             
         DC    CL25'5404AEND MOS'                                               
         DC    CL25'6201APRODUCE TAPE?'                                         
         DC    X'00'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022SPREPC001 09/19/02'                                      
         END                                                                    
