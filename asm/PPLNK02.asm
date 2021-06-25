*          DATA SET PPLNK02    AT LEVEL 001 AS OF 10/05/01                      
*PHASE T41402A                                                                  
PPLNK02  TITLE '- PRINT SYSTEM SERVER SUPPORT ROUTINES 2'                       
PPLNK02  CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**PL02**,RR=RE                                                 
         USING WORKD,R9            R9=A(GLOBAL W/S)                             
         USING TWAD,RA             RA=A(TWA)                                    
         ST    RE,ROU2RELO         SAVE MY RELOCATION FACTOR                    
         SR    RE,RE                                                            
         SLDL  RE,8                BRANCH INDEX HELD IN HOB RF                  
         SLL   RE,2                                                             
         CHI   RE,ROUTABL          ENSURE GOOD INDEX VALUE                      
         BL    *+6                                                              
         DC    H'0'                                                             
         LA    RE,ROUTAB(RE)                                                    
         SR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         AR    RF,RB               RF=A(ROUTINE)                                
                                                                                
         SR    R5,R5                                                            
         ICM   R5,3,2(RE)          R5=TEMPORARY W/S AMOUNT                      
         BZR   RF                                                               
                                                                                
         AHI   R5,7                ROUND AMOUNT TO DOUBLEWORDS                  
         SRL   R5,3                                                             
         SLL   R5,3                                                             
         LR    R3,RD               ACQUIRE STORAGE FROM W/S POOL                
         AR    R3,R5                                                            
         L     R4,4(RD)                                                         
         ST    R4,4(R3)                                                         
         ST    R3,8(R4)                                                         
         LR    RC,RD                                                            
         LR    RD,R3                                                            
         LR    R4,RC               AND CLEAR IT                                 
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         MVCL  R4,R2                                                            
         BR    RF                                                               
         DROP  RB                                                               
                                                                                
         LTORG                                                                  
                                                                                
ROUTAB   DS    0XL4                                                             
ROUTABL  EQU   *-ROUTAB                                                         
         EJECT                                                                  
EXITN    DS    0H                  SET CC NOT EQUAL                             
EXITL    MVI   DUB1,0              SET CC LOW                                   
         J     EXITCC                                                           
EXITH    MVI   DUB1,2              SET CC HIGH                                  
         J     EXITCC                                                           
EXITY    MVI   DUB1,1              SET CC EQUAL                                 
EXITCC   CLI   DUB1,1                                                           
EXIT     XIT1  ,                                                                
                                                                                
         PRINT OFF                                                              
       ++INCLUDE PPLNKWRK                                                       
         PRINT ON                                                               
                                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'001PPLNK02   10/05/01'                                      
         END                                                                    
