*          DATA SET GELNK02    AT LEVEL 001 AS OF 11/16/09                      
*PHASE TA0602A                                                                  
GELNK02  TITLE '- Control system server support routines 2'                     
GELNK02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**GL02**,RR=RE                                                 
         USING WORKD,R9            R9=A(Global w/s)                             
         LARL  R8,GLOBALS                                                       
         USING GLOBALS,R8          R8=A(global literals)                        
         L     RA,ATWA                                                          
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,ROU2RELO         Save my relocation factor                    
         SR    RE,RE                                                            
         SLDL  RE,8                Branch index in hob of RF                    
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          Ensure good index value                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         BNZ   *+6                                                              
         DC    H'0'                Routine not defined                          
         AR    RF,RB               RF=A(routine)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=temporary w/s amount                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                Round amount to doublewords                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               Acquire storage from w/s pool                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               and clear it                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
                                                                                
ROUTAB   DS    0XL4                                                             
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
EXITN    DS    0H                  SET CC NOT EQUAL                             
EXITL    LHI   RE,0                SET CC LOW                                   
         J     EXITCC                                                           
EXITH    LHI   RE,2                SET CC HIGH                                  
         J     EXITCC                                                           
EXITY    LHI   RE,1                SET CC EQUAL                                 
EXITCC   CHI   RE,1                                                             
EXIT     XIT1  ,                                                                
         EJECT                                                                  
GLOBALS  DS    0D                                                               
         LTORG                                                                  
                                                                                
* GELNKWRK                                                                      
         PRINT OFF                                                              
       ++INCLUDE GELNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001GELNK02   11/16/09'                                      
         END                                                                    
