*          DATA SET PULDTAP    AT LEVEL 013 AS OF 08/12/91                      
         TITLE 'CREATE PUBFILE LOADTAPE'                                        
*PHASE PULDTAP,*                                                                
         PRINT NOGEN                                                            
LOADTAPE CSECT                                                                  
         NBASE 0,LOADTAPE,WORK=A(WORK)                                          
         LA    R2,TAPEOUT                                                       
         OPEN  ((2),OUTPUT)                                                     
*                                                                               
         LA    R4,900                                                           
         LA    R5,50                                                            
*                                                                               
PT04     DS    0H                                                               
         CVD   R4,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  HRELP1(4),DUB                                                    
         UNPK  HRELP2(4),DUB                                                    
         PACK  WK(5),HRELP1(5)                                                  
         MVC   HRECK+1(4),WK                                                    
*                                                                               
         CLI   HRECK,C'M'          ROTATE MEDIA M,N,T                           
         BNE   *+12                                                             
         MVI   HRECK,C'N'                                                       
         B     PT05                                                             
         CLI   HRECK,C'N'                                                       
         BNE   *+12                                                             
         MVI   HRECK,C'T'                                                       
         B     PT05                                                             
         MVI   HRECK,C'M'                                                       
*                                                                               
PT05     DS    0H                                                               
         CLI   HRECK+7,C'S'        ROTATE AGY BETWEEN DJ AND SJ                 
         BNE   *+12                                                             
         MVI   HRECK+7,C'D'                                                     
         B     *+8                                                              
         MVI   HRECK+7,C'S'                                                     
*                                                                               
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
HRECK    DC    X'D4000000010000E2D181',15X'00'                                  
         DC    AL2(HRECX-HRECK),X'0000',XL4'00'                                 
HRELM    DC    X'10C4',C'ABCD ',C'PUB'                                          
HRELP1   DC    CL4' ',CL8' ',C'ZONE'                                            
HRELP2   DC    CL4' ',CL12' '                                                   
         DC    XL91'00'                                                         
         DC    CL4'1234'                                                        
         DC    XL59'00'                                                         
*                                                                               
         DC    X'00'                                                            
HRECX    DC    X'00'                                                            
         SPACE 2                                                                
*                                                                               
PUBNAMD  DSECT                                                                  
       ++INCLUDE PUBNAMEL                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'013PULDTAP   08/12/91'                                      
         END                                                                    
