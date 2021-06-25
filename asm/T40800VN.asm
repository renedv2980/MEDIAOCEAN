*          DATA SET T40800VN   AT LEVEL 053 AS OF 05/01/02                      
*PHASE T40800A,+0,NOAUTO                                                        
*INCLUDE SQUART                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
*INCLUDE NUMVAL                                                                 
*INCLUDE SCANNER                                                                
*INCLUDE STNORM                                                                 
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
         L     RF,=V(SCANNER)                                                   
         AR    RF,RB                                                            
         ST    RF,SCANNER                                                       
         L     RF,=V(STNORM)                                                    
         AR    RF,RB                                                            
         ST    RF,STNORM                                                        
*                                                                               
         XC    VNWRK,VNWRK                                                      
         XC    IPVAR,IPVAR                                                      
         XC    OPVAR,OPVAR                                                      
         XC    SCBLK(256),SCBLK                                                 
         XC    SCBLK+256(256),SCBLK+256                                         
         LA    R6,VNWRK                                                         
         USING STNORMD,R6                                                       
         LA    RF,IPVAR                                                         
         ST    RF,STNIPVAR                                                      
         LA    RF,OPVAR                                                         
         ST    RF,STNOPVAR                                                      
         MVC   STNSQRT,SQUART                                                   
         MVI   STNMASK,X'A0'                                                    
         MVI   STNPREC,0                                                        
*                                                                               
         LA    R3,1                                                             
         LA    R2,CMTCM1H                                                       
         LA    R7,CMTCM1+67                                                     
         CLI   0(R7),C' '                                                       
         BH    *+8                                                              
         BCT   R7,*-8                                                           
         LA    R0,CMTCM1-1                                                      
         SR    R7,R0                                                            
         BNP   ERROR                                                            
         STC   R7,5(R2)                                                         
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),SCBLK                                          
         LA    R4,SCBLK                                                         
         L     R5,STNIPVAR                                                      
*                                                                               
VN2      DS    0H                                                               
         CLI   0(R4),0                                                          
         BE    VN6                                                              
*                                                                               
         CLI   12(R4),C'P'                                                      
         BNE   VN2B                                                             
         MVC   STNPREC,11(R4)                                                   
         B     VN3                                                              
*                                                                               
VN2B     DS    0H                                                               
         L     RF,STNVCNT                                                       
         LA    RF,1(RF)                                                         
         ST    RF,STNVCNT                                                       
*                                                                               
         TM    2(R4),X'80'                                                      
         BNZ   *+12                                                             
         LA    R3,2                                                             
         B     ERROR                                                            
*                                                                               
         MVC   0(4,R5),4(R4)                                                    
         LA    R5,4(R5)                                                         
VN3      DS    0H                                                               
         LA    R4,32(R4)                                                        
         B     VN2                                                              
*                                                                               
VN6      DS    0H                                                               
         GOTO1 STNORM,DMCB,(R6)                                                 
*                                                                               
         LA    R2,CMTCM1H+78*2                                                  
         MVC   8(5,R2),=C'MEAN='                                                
         EDIT  STNMEAN,(8,18(R2)),2,FLOAT=-                                     
         LA    R2,78(R2)                                                        
         MVC   8(9,R2),=C'VARIANCE='                                            
         EDIT  STNVRNCE,(8,18(R2)),2                                            
         LA    R2,78(R2)                                                        
         MVC   8(8,R2),=C'STD DEV='                                             
         EDIT  STNSD,(8,18(R2)),2                                               
         LA    R2,78(R2)                                                        
         MVC   8(7,R2),=C'ACTIVE='                                              
         EDIT  STNACNT,(8,18(R2))                                               
*                                                                               
         LA    R2,8+78*2(R2)                                                    
         L     R5,STNOPVAR                                                      
         L     R7,STNVCNT                                                       
*                                                                               
VN8      DS    0H                                                               
         EDIT  (B4,0(R5)),(8,0(R2)),2,FLOAT=-                                   
         MVI   8(R2),C','                                                       
         LA    R2,9(R2)                                                         
         LA    R5,4(R5)                                                         
         BCT   R7,VN8                                                           
*                                                                               
         LA    RF,8                                                             
         LA    R2,CMTCM1H                                                       
*                                                                               
VN9      DS    0H                                                               
         FOUT  (R2)                                                             
         LA    R2,78(R2)                                                        
         BCT   RF,VN9                                                           
*                                                                               
         LA    R2,CMTCM1H                                                       
         B     EXIT                                                             
*                                                                               
NUMVAL   DS    A                                                                
SQUART   DS    A                                                                
SCANNER  DS    A                                                                
STNORM   DS    A                                                                
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
         DS    0F                                                               
VNWRK    DS    XL200                                                            
IPVAR    DS    XL80                                                             
OPVAR    DS    CL80                                                             
SCBLK    DS    XL512                                                            
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
*                                                                               
       ++INCLUDE DDSTNORMD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'053T40800VN  05/01/02'                                      
         END                                                                    
