*          DATA SET DDLOADTAPE AT LEVEL 016 AS OF 07/24/91                      
         TITLE 'CREATE GENERIC LOADTAPE'                                        
*PHASE LOADTAPE,*                                                               
         PRINT NOGEN                                                            
LOADTAPE CSECT                                                                  
         NBASE 0,LOADTAPE,WORK=A(WORK)                                          
         LA    R2,TAPEOUT                                                       
         OPEN  ((2),OUTPUT)                                                     
         PUT   TAPEOUT,HRECH                                                    
*                                                                               
         SPACE 2                                                                
         CLOSE ((2))                                                            
         XBASE                                                                  
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
TAPEOUT  DCB   DDNAME=TAPEOUT,DSORG=PS,MACRF=(PM),RECFM=VB,            X        
               LRECL=4004,BUFNO=2                                               
*                                                                               
WORK     DS    200D                                                             
*                                                                               
         EJECT                                                                  
HRECH    DC    AL2(HRECX-HRECH)                                                 
         DC    AL2(0)                                                           
HRECK    DC    X'D4000000010000E2D281',15X'00'                                  
         DC    AL2(HRECX-HRECK),X'0000',XL4'00'                                 
         DC    X'01040000'                                                      
         DC    X'00'                                                            
HRECX    DC    X'00'                                                            
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016DDLOADTAPE07/24/91'                                      
         END                                                                    
