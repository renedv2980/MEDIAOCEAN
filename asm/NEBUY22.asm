*          DATA SET NEBUY22    AT LEVEL 014 AS OF 12/13/10                      
*PHASE T31122A,+0                                                               
         TITLE 'NETPAK BUY PROGRAM PROFLIE DISPLAY- T31122'                     
T31122   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BUY10*,RA,RR=RE                                              
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING T31122+8192,RC                                                   
         L     R9,0(R1)            R9 POINTS TO GLOBAL WORKING STORAGE          
         USING BUYWRKD,R9                                                       
         L     R8,ATWA                                                          
         USING TWAD,R8                                                          
         L     R7,AOVWORK          R7 POINTS TO LOCAL WORKING STORAGE           
         USING TEMPD,R7                                                         
         ST    RE,MYRELO                                                        
         ST    R1,MYPARM                                                        
         LA    R6,NEBLOCKA         R6 POINTS TO NETBLOCK                        
         USING NEBLOCKD,R6                                                      
*                                                                               
         BAS   RE,DIS                                                           
         B     PROFEX                                                           
*                                                                               
PROFEX   MVC   BUYMSG(18),=CL18'PROFILES DISPLAYED'                             
         LA    R2,BUYACTH                                                       
         ST    R2,FADDR                                                         
         B     EXXMOD                                                           
         EJECT                                                                  
*                                                                               
* DISPLAY PROFILES                                                              
*                                                                               
DIS      NTR1                                                                   
*                                                                               
*  DISPLAY N0 PROFLIE                                                           
         LA    R2,PRON0P1                                                       
         LA    R3,NBUSER                                                        
         LA    R4,N0EDIT                                                        
         LA    R5,16                                                            
DIS20    CLI   0(R4),C'C'                                                       
         BE    DIS25                                                            
         EDIT  (B1,(R3)),(3,(R2)),ZERO=NOBLANK                                  
         B     *+10                                                             
DIS25    MVC   2(1,R2),0(R3)                                                    
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,DIS20                                                         
         OI    PRON0P1H+6,X'80'     TRANSMIT                                    
*                                                                               
*  DISPLAY N1 PROFLIE                                                           
         LA    R2,PRON1P1                                                       
         LA    R3,NBUSER1                                                       
         LA    R4,N1EDIT                                                        
         LA    R5,16                                                            
DIS40    CLI   0(R4),C'C'                                                       
         BE    DIS45                                                            
         EDIT  (B1,(R3)),(3,(R2)),ZERO=NOBLANK                                  
         B     *+10                                                             
DIS45    MVC   2(1,R2),0(R3)                                                    
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,DIS40                                                         
         OI    PRON1P1H+6,X'80'     TRANSMIT                                    
*                                                                               
*  DISPLAY N2 PROFLIE                                                           
         LA    R2,PRON2P1                                                       
         LA    R3,NBUSER2                                                       
         LA    R4,N2EDIT                                                        
         LA    R5,16                                                            
DIS60    CLI   0(R4),C'C'                                                       
         BE    DIS65                                                            
         EDIT  (B1,(R3)),(3,(R2)),ZERO=NOBLANK                                  
         B     *+10                                                             
DIS65    MVC   2(1,R2),0(R3)                                                    
         LA    R2,4(R2)                                                         
         LA    R3,1(R3)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,DIS60                                                         
         OI    PRON2P1H+6,X'80'     TRANSMIT                                    
         B     EXXMOD                                                           
*                                                                               
EXXMOD   XMOD1 1                                                                
*                                                                               
*   C=CHARACTER DISPLAY N=NUMERIC DISPLAY                                       
N0EDIT   DC    CL16'CNCCNCCCCNNCCCCC'                                           
N1EDIT   DC    CL16'CCCCCCCCCCCCCCNC'                                           
N2EDIT   DC    CL16'NCCCCCCCCCCCCCCC'                                           
*                                                                               
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
       ++INCLUDE NEBUYWRK                                                       
         EJECT                                                                  
* PROFILE SCREEN                                                                
*                                                                               
         ORG   BUYLAST                                                          
         PRINT GEN                                                              
       ++INCLUDE NEBUYE3D                                                       
         PRINT NOGEN                                                            
         EJECT                                                                  
* LOCAL WORKING STORAGE                                                         
*                                                                               
TEMPD    DSECT                                                                  
MYRELO   DS    A                                                                
MYPARM   DS    A                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'014NEBUY22   12/13/10'                                      
         END                                                                    
