*          DATA SET SPREPAN01  AT LEVEL 001 AS OF 09/04/07                      
*PHASE SPAN01A                                                                  
SPAN01   TITLE '- Spec Phase for Network Agency Summary For Accent'             
SPAN01   CSECT                                                                  
         PRINT NOGEN                                                            
                                                                                
         FSPEC USE,SPAN03                                                       
                                                                                
         SSPEC H1,98,AGYNAME                                                    
         SSPEC H2,98,AGYADD                                                     
         SSPEC H4,98,REPORT                                                     
         SSPEC H5,98,PAGE                                                       
                                                                                
         DC    X'00'                                                            
                                                                                
         DC    C'REQLST='                                                       
                                                                                
         DC    CL25'6201AGross/Net'                                             
         DC    CL25'6301ATrace'                                                 
         DC    X'00'                                                            
                                                                                
         DC    X'00'                                                            
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001SPREPAN01 09/04/07'                                      
         END                                                                    
