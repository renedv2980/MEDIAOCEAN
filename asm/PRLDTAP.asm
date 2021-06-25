*          DATA SET PRLDTAP    AT LEVEL 002 AS OF 08/08/91                      
         TITLE 'CREATE PRTFILE LOADTAPE'                                        
*PHASE PRLDTAP,*                                                                
         PRINT NOGEN                                                            
LOADTAPE CSECT                                                                  
         NBASE 0,LOADTAPE,WORK=A(WORK)                                          
         LA    R2,TAPEOUT                                                       
         OPEN  ((2),OUTPUT)                                                     
*                                                                               
         LA    R4,900                                                           
         LA    R5,20                                                            
*                                                                               
PT04     DS    0H                                                               
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HRECK+4(4),DUB                                                   
         UNPK  HRELM+5(4),DUB                                                   
         PUT   TAPEOUT,HRECH                                                    
*                                                                               
         LA    R4,1(R4)                                                         
         BCT   R5,PT04                                                          
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
DUB      DS    D                                                                
WK       DS    64X                                                              
*                                                                               
WORK     DS    200D                                                             
*                                                                               
         EJECT                                                                  
HRECH    DC    AL2(HRECX-HRECH)                                                 
         DC    AL2(0)                                                           
HRECK    DC    C'SJM',X'11',C'NNNN',17X'00'                                     
         DC    AL2(HRECX-HRECK),X'0000',XL4'00'                                 
HRELM    DC    X'11A4',CL30'REPNNNN',XL132'00'                                  
HRECX    DC    X'00'                                                            
         SPACE 2                                                                
*                                                                               
PUBNAMD  DSECT                                                                  
       ++INCLUDE PUBNAMEL                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002PRLDTAP   08/08/91'                                      
         END                                                                    
