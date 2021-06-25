*          DATA SET T40800SQ   AT LEVEL 044 AS OF 05/01/02                      
*PHASE T40800C,+0,NOAUTO                                                        
*INCLUDE SQUART                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE NUMVAL                                                                 
T40800   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1200,T40800                                                      
         USING GENOLD,RC                                                        
         USING T408FFD,RA                                                       
         SPACE 2                                                                
         BAS   RE,INITL                                                         
*                                                                               
         L     RF,=V(NUMVAL)                                                    
         AR    RF,RB                                                            
         ST    RF,NUMVAL                                                        
         L     RF,=V(SQUART)                                                    
         AR    RF,RB                                                            
         ST    RF,SQUART                                                        
*                                                                               
         LA    R2,CMTCM1H                                                       
         LA    R4,20(R2)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R0,7(R2)                                                         
         SR    R4,R0                                                            
         BNP   EXIT                                                             
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
*                                                                               
         B     SQ4                                                              
         SR    R7,R7                                                            
         LA    R2,CMTCM1H+78                                                    
         LA    R4,20(R2)                                                        
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R0,7(R2)                                                         
         SR    R4,R0                                                            
         BNP   SQ4                                                              
         BCTR  R4,R0                                                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   RF,DUB                                                           
         MR    R0,RF                                                            
*                                                                               
SQ4      DS    0H                                                               
         ST    R1,DUB                                                           
         GOTO1 SQUART,DMCB,DUB,FULL                                             
         L     RF,DMCB+4                                                        
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         LA    R2,CMTCM1H+78*2                                                  
         UNPK  8(10,R2),DUB                                                     
         FOUT  (R2)                                                             
*                                                                               
         B     EXIT                                                             
*                                                                               
NUMVAL   DS    A                                                                
SQUART   DS    A                                                                
*                                                                               
         EJECT                                                                  
*                  INITIALISATION CODE                                          
         SPACE 3                                                                
INITL    LR    R4,RC               SET UP TO CLEAR WORK SPACE                   
         LR    R5,RD                                                            
         SR    R5,R4                                                            
         LR    R0,RE                                                            
         BAS   RE,CLEARWRK                                                      
         LM    R2,R4,0(R1)                                                      
         PACK  AGYNUM,0(1,R1)      AGENCY NUMBER                                
         LH    R5,0(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,FRSTFLD .        A(FIRST INPUT FIELD HEADER)                  
         LH    R5,2(R2)                                                         
         AR    R5,R3                                                            
         ST    R5,LASTFLD .        A(LAST INPUT FIELD)                          
         MVC   NUMFLD,4(R2) .      NUMBER OF FIELDS                             
         ST    R3,VTWA .           A(TWA)                                       
         MVC   VDATAMGR(36),0(R4)  FACILITY LIST                                
         LR    RA,R3                                                            
         MVC   TERMNAL,0(RA) .     TERMINAL NUMBER                              
         MVC   AGYALPHA,14(RA)     ALPHA AGENCY CODE                            
         LA    R3,64(R3)                                                        
         ST    R3,ERRAREA          PRESET ERRAREA TO A(FIRST HEADER)            
         MVI   DMINBTS,X'C0'       PRESET DATAMGR CONTROL BITS                  
         MVI   DMOUTBTS,X'FD'      PRESET DATAMGR ERROR CHECK BITS              
         LR    RE,R0                                                            
         BR    RE                                                               
         SPACE 2                                                                
CLEARWRK LTR   R5,R5               CLEAR STORAGE TO ZEROS                       
         BCR   8,RE                                                             
         CH    R5,=H'250'                                                       
         BNH   CLEAREST                                                         
         XC    0(250,R4),0(R4)                                                  
         LA    R4,250(R4)                                                       
         SH    R5,=H'250'                                                       
         B     CLEARWRK                                                         
         SPACE 2                                                                
CLEAREST BCTR  R5,R0                                                            
         EX    R5,VARCLEAR                                                      
         BR    RE                                                               
         SPACE 2                                                                
VARCLEAR XC    0(0,R4),0(R4)                                                    
         EJECT                                                                  
*                  FARMABLE CODE                                                
         SPACE 3                                                                
ANY      CLI   5(R2),0                                                          
         BNE   ANY2                                                             
         LA    R3,1                                                             
         B     ERROR                                                            
         SPACE 2                                                                
ANY2     TM    4(R2),X'10' .       IS IT VALID NUMERIC                          
         BCR   8,RE .              IF APPLICABLE                                
         LA    R3,3                                                             
         B     ERROR                                                            
*                  EXITS FROM PROGRAM                                           
         SPACE 3                                                                
LOCK     OI    6(R2),X'02'         LOCK SCREEN                                  
         SPACE 2                                                                
ERROR    L     R4,ERRAREA                                                       
         MVI   ERRAREA,X'FF'                                                    
         MVC   DMCB+20(4),VDATAMGR                                              
         MVC   DMCB+20(1),TERMNAL                                               
         GOTO1 VGETMSG,DMCB+12,((R3),8(R4)),(4,DMCB)                            
         SPACE 2                                                                
EXIT     OI    6(R2),OI1C .        INSERT CURSOR                                
         L     R4,ERRAREA                                                       
         FOUT  (R4)                                                             
*                                                                               
*                                                                               
EXXMOD   MVC   SVKEY(10),KEYSAVE                                                
         MVI   CHSW,0                                                           
         XMOD1 1                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
*                                                                               
OUTLIN   DS    CL200                                                            
HXLIN    DS    CL280                                                            
INLIN    DS    CL700                                                            
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE GENOLD                                                         
         SPACE 2                                                                
         ORG   IOAREA                                                           
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE T408FFD                                                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE PCMTTWA                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'044T40800SQ  05/01/02'                                      
         END                                                                    
