*          DATA SET T40800EX   AT LEVEL 040 AS OF 05/01/02                      
*PHASE T40800C,+0,NOAUTO                                                        
*INCLUDE GPEXPVAL                                                               
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
T40800   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1200,T40800                                                      
         USING GENOLD,RC                                                        
         USING T408FFD,RA                                                       
MDERR    EQU   13                  INVALID MEDIA                                
ACERR    EQU   12                  INVALID ACTION                               
CHGERR   EQU   142                                                              
RSIZERR  EQU   227                                                              
DUPERR   EQU   52                  DUPLICATE KEY ON ADD                         
MTCHERR  EQU   53                  RECORD NOT FOUND                             
MSERR    EQU   1                   MISSING INPUT                                
         SPACE 2                                                                
         BAS   RE,INITL                                                         
*                                  ROUTINE FOR EXPVAL                           
         XC    INLIN(200),INLIN                                                 
         XC    INLIN+200(80),INLIN+200                                          
         XC    OUTLIN,OUTLIN                                                    
         MVC   INLIN(70),CMTCM1                                                 
         MVC   INLIN+70(70),CMTCM1+78                                           
         GOTO1 =V(EXPVAL),DMCB,(140,INLIN),OUTLIN,RR=RB                         
         ZIC   R4,DMCB+4                                                        
         GOTO1 =V(HEXOUT),WORK,OUTLIN,INLIN,(R4),=C'N',RR=RB                    
         MVC   CMTCM1+(78*3),INLIN                                              
         MVC   CMTCM1+(78*4),INLIN+70                                           
         LA    R4,CMTCM1+(78*5)                                                 
         MVC   0(6,R4),=C'ERROR='                                               
         ZIC   RF,DMCB+12                                                       
         CVD   RF,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  8(3,R4),DUB                                                      
         MVC   20(7,R4),=C'RESULT='                                             
         L     RF,DMCB+8                                                        
         EDIT  (RF),(10,30(R4)),FLOAT=-,ALIGN=LEFT                              
         LA    R4,CMTCM1H                                                       
         LA    R0,6                                                             
MP15     DS    0H                                                               
         FOUT  (R4)                                                             
         LA    R4,78(R4)                                                        
         BCT   R0,MP15                                                          
         LA    R2,CMTCM1H                                                       
         B     EXIT                                                             
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
NRESP    DC    F'2800'                                                          
DIRNM    DC    C'PUBDIR  '                                                      
FLENM    DC    C'PUBFILE '                                                      
BSLEN    DC    H'350'                                                           
*                                                                               
UDMGR    DS    F                                                                
UAREC    DS    A                                                                
UDMWRK   DS    A                                                                
*                                                                               
OUTLIN   DS    CL200                                                            
HXLIN    DS    CL280                                                            
INLIN    DS    CL700                                                            
*                                                                               
         ORG   T40800+4070                                                      
*                                                                               
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE GENOLD                                                         
         SPACE 2                                                                
         ORG   IOAREA                                                           
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
BSIO     DS    1000X                                                            
BSDMWRK  DS    100D                                                             
BSSTKA   DS    6000X                                                            
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE T408FFD                                                        
*                                                                               
         EJECT                                                                  
       ++INCLUDE PCMTTWA                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'040T40800EX  05/01/02'                                      
         END                                                                    
