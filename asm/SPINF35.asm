*          DATA SET SPINF35    AT LEVEL 004 AS OF 05/18/07                      
*PHASE T21A35A                                                                  
         TITLE 'T21A35 - SPOTPAK INFO EQUIVALENCE HEADER DISPLAY'               
T21A35   CSECT                                                                  
         PRINT NOGEN                                                            
EQSOLO   EQU   EQUSECT1                                                         
EQPIG    EQU   EQUSECT1+2                                                       
         NMOD1 0,T21A35                                                         
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T21AFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         LA    R2,SINIFLTH                                                      
         TM    FLDIIND,X'20'                                                    
         BZ    *+10                                                             
         XC    PREVKEY,PREVKEY                                                  
         LA    R2,SINHDRH                                                       
*                                                                               
HL0      XC    8(79,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),9                                                          
         BH    HL0                                                              
*                                                                               
         LA    R2,SINHDRH                                                       
         MVC   FLDDATA(16),=C'CLIENT CODE/NAME'                                 
         MVC   FLDDATA+29(15),=C'SECTION CONTROL'                               
         LA    R2,LINLEN(R2)                                                    
         LA    R4,SECTAB                                                        
         LA    RE,FLDDATA+5                                                     
         SR    R9,R9                                                            
HL1      CLI   0(R4),0             MOVE IN SECONDS HEADINGS                     
         BE    HL2                                                              
         LA    R9,1(R9)                                                         
         MVC   0(3,RE),0(R4)                                                    
         LA    RE,5(RE)                                                         
         LA    R4,3(R4)                                                         
         B     HL1                                                              
HL2      STH   R9,NUMSL                                                         
         LA    R2,LINLEN(R2)                                                    
         LA    RE,FLDDATA+4                                                     
         LA    R4,SECTAB                                                        
HL3      CLI   0(R4),0             UNDERLINE SECONDS HEADINGS                   
         BE    HL4                                                              
         MVC   0(4,RE),=C'----'                                                 
         LA    RE,5(RE)                                                         
         LA    R4,3(R4)                                                         
         B     HL3                                                              
HL4      LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         EJECT                                                                  
         LA    R5,KEY                                                           
         USING EQUHDRD,R5                                                       
         XC    KEY,KEY                                                          
         MVI   EQUKTYPE,9                                                       
         MVC   EQUKAGY,AGYALPHA                                                 
         MVC   EQUKMED,SVEBCMED                                                 
         MVC   EQUKCLT(2),SVCLT                                                 
         LA    R8,MAXLIN                                                        
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+10                                                             
         MVC   KEY,PREVKEY         RESTORE PREVIOUS KEY                         
         XC    PREVKEY,PREVKEY                                                  
GR10     GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BE    HAVREC                                                           
         LA    R5,KEYSAVE                                                       
         CLC   EQUKAGY,=C'00'                                                   
         BE    MODEXIT                                                          
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         MVI   EQUKTYPE,9                                                       
         MVC   EQUKAGY,=C'00'                                                   
         MVC   EQUKMED,SVEBCMED                                                 
         B     GR10                                                             
*                                                                               
GESEQ    GOTO1 HIGH                RESET SEQUENTIAL READ                        
         GOTO1 SEQ                                                              
HAVREC   CLC   KEY(4),KEYSAVE                                                   
         BNE   MODEXIT                                                          
         GOTO1 GETREC                                                           
         LA    R5,REC                                                           
         OC    EQUKCLT,EQUKCLT                                                  
         BNE   RDCLT                                                            
         MVC   FLDDATA(07),=C'DEFAULT'                                          
         B     HAVCLT                                                           
RDCLT    MVC   WORK2,KEY           READ CLIENT HEADER                           
         LA    R6,KEY                                                           
         USING CLTHDRD,R6                                                       
         LA    R7,REC2                                                          
         ST    R7,AREC                                                          
         XC    KEY,KEY                                                          
         MVC   CKEYAM,SVAGYMD                                                   
         MVC   CKEYCLT,EQUKCLT                                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         L     R6,AREC                                                          
*                                                                               
         MVC   DMCB+4(4),=X'D9000A15'                                           
         GOTO1 VCALLOV,DMCB,0                                                   
         L     RF,DMCB                                                          
         GOTO1 (RF),(R1),CKEYCLT,FLDDATA                                        
****     MVC   FLDDATA(3),CKEYCLT  MOVE CLIENT NAME TO DATE LINE                
*                                                                               
         MVI   FLDDATA+3,C'/'                                                   
         MVC   FLDDATA+4(20),CNAME                                              
         MVC   KEY(40),WORK2                                                    
         ST    R5,AREC                                                          
*                                                                               
HAVCLT   LA    RE,EQUDPT                                                        
         LA    RF,15                                                            
         OI    0(RE),X'F0'            SET EBCDIC CODES                          
         LA    RE,1(RE)                                                         
         BCT   RF,*-8                                                           
         MVC   FLDDATA+29(15),EQUDPT                                            
         LA    R2,LINLEN(R2)                                                    
         BCTR  R8,0                                                             
* CHECK FOR ANY EQUIVALENCE REQUIRED                                            
         LA    RE,EQSOLO                                                        
         LA    RF,60                                                            
CEQU     CLC   0(2,RE),=C'1000'                                                 
         BNE   EQUOK                                                            
         LA    RE,2(RE)                                                         
         BCT   RF,CEQU                                                          
         B     EQDEND                                                           
         EJECT                                                                  
* CALCULATE AND DISPLAY EQUIVALENCE FACTORS                                     
EQUOK    LA    RE,EQSOLO                                                        
         LA    RF,15                                                            
EQS10    CLC   0(2,RE),=H'1000'    CHECK SOLO GROUP 1                           
         BNE   EQS12                                                            
         LA    RE,4(RE)                                                         
         BCT   RF,EQS10                                                         
         B     EQS2                                                             
EQS12    LA    R7,EQSOLO           DISPLAY S1 EQUIVALENCES                      
         LH    R0,NUMSL                                                         
         LA    R6,FLDDATA+4                                                     
EQS14    MVC   FLDDATA(2),=C'S1'                                                
         BAS   R9,CNVEQU                                                        
         LA    R6,5(R6)                                                         
         LA    R7,4(R7)                                                         
         BCT   R0,EQS14                                                         
EQS15    LA    R2,LINLEN(R2)                                                    
         BCTR  R8,0                                                             
* DISPLAY S2 EQUIVALENCES                                                       
EQS2     LA    RE,EQSOLO+60                                                     
         LA    RF,15                                                            
EQS20    CLC   0(2,RE),=H'1000'    CHECK SOLO GROUP 2                           
         BNE   EQS22                                                            
         LA    RE,4(RE)                                                         
         BCT   RF,EQS20                                                         
         B     EQP1                                                             
EQS22    LA    R7,EQSOLO+60        DISPLAY S2 EQUIVALENCES                      
         LH    R0,NUMSL                                                         
         LA    R6,FLDDATA+4                                                     
EQS24    MVC   FLDDATA(2),=C'S2'                                                
         BAS   R9,CNVEQU                                                        
         LA    R6,5(R6)                                                         
         LA    R7,4(R7)                                                         
         BCT   R0,EQS24                                                         
EQS25    LA    R2,LINLEN(R2)                                                    
         BCTR  R8,0                                                             
*                                                                               
* DISPLAY P1 EQUIVALENCES                                                       
EQP1     LA    RE,EQPIG                                                         
         LA    RF,15                                                            
EQP10    CLC   0(2,RE),=H'1000'    CHECK PIGGY GROUP 1                          
         BNE   EQP12                                                            
         LA    RE,4(RE)                                                         
         BCT   RF,EQP10                                                         
         B     EQP2                                                             
EQP12    LA    R7,EQPIG            DISPLAY P1 EQUIVALENCES                      
         LH    R0,NUMSL                                                         
         LA    R6,FLDDATA+4                                                     
EQP14    MVC   FLDDATA(2),=C'P1'                                                
         BAS   R9,CNVEQU                                                        
         LA    R6,5(R6)                                                         
         LA    R7,4(R7)                                                         
         BCT   R0,EQP14                                                         
EQP15    LA    R2,LINLEN(R2)                                                    
         BCTR  R8,R0                                                            
*                                                                               
* DISPLAY P2 EQUIVALENCES                                                       
EQP2     LA    RE,EQPIG+60                                                      
         LA    RF,15                                                            
EQP20    CLC   0(2,RE),=H'1000'    CHECK PIGGY GROUP 2                          
         BNE   EQP22                                                            
         LA    RE,4(RE)                                                         
         BCT   RF,EQP20                                                         
         B     EQDEND                                                           
EQP22    LA    R7,EQPIG+60         DISPLAY P2 EQUIVALENCES                      
         LH    R0,NUMSL                                                         
         LA    R6,FLDDATA+4                                                     
EQP24    MVC   FLDDATA(2),=C'P2'                                                
         BAS   R9,CNVEQU                                                        
         LA    R6,5(R6)                                                         
         LA    R7,4(R7)                                                         
         BCT   R0,EQP24                                                         
EQP25    LA    R2,LINLEN(R2)                                                    
         BCTR  R8,0                                                             
*                                                                               
         EJECT                                                                  
EQDEND   LA    R2,LINLEN(R2)                                                    
         BCTR  R8,0                                                             
         CH    R8,=H'4'                                                         
         BNL   GESEQ                                                            
         GOTO1 HIGH                SET NEXT KEY                                 
         GOTO1 SEQ                                                              
         CLC   KEY(4),KEYSAVE                                                   
         BNE   *+10                                                             
         MVC   PREVKEY,KEY                                                      
MODEXIT  LA    R2,SINENDH                                                       
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINIKEYH                                                      
         OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
         EJECT                                                                  
CNVEQU   LR    R4,R0               SAVE R0                                      
         CLC   0(2,R7),=H'1000'                                                 
         BNL   CNVEQUH                                                          
         LH    RE,0(R7)            DISPLAY FRACTION                             
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  1(3,R6),DUB+6(2)                                                 
         MVI   0(R6),C'.'                                                       
         BR    R9                                                               
CNVEQUH  LH    RE,0(R7)            DISPLAY WHOLE NUMBER + FRACTION              
         SR    RF,RF                                                            
         MH    RE,=H'100'                                                       
         SRDA  RE,31                                                            
         D     RE,=F'1000'                                                      
         AH    RF,=H'1'                                                         
         SRL   RF,1                                                             
         LR    RE,R0                                                            
         EDIT  (RF),(4,0(R6)),2                                                 
         LR    R0,R4                                                            
         BR    R9                                                               
         LTORG                                                                  
         EJECT                                                                  
MAXLIN   EQU   15                                                               
LINLEN   EQU   88                                                               
         EJECT                                                                  
SECTAB   DC    C' 10'                                                           
         DC    C' 15'                                                           
         DC    C' 20'                                                           
         DC    C' 30'                                                           
         DC    C' 40'                                                           
         DC    C' 45'                                                           
         DC    C' 50'                                                           
         DC    C' 60'                                                           
         DC    C' 90'                                                           
         DC    C'120'                                                           
         DC    C'105'                                                           
         DC    C'150'                                                           
         DC    C' 75'                                                           
         DC    X'00'                                                            
         SPACE 1                                                                
NUMSL    DC    H'0'                                                             
         EJECT                                                                  
* SPINFWORK                                                                     
       ++INCLUDE SPINFWORK                                                      
         EJECT                                                                  
EQUHDRD  DSECT                                                                  
*       +INCLUDE ECEQUHDR                                                       
       ++INCLUDE SPGENEQU                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPINF35   05/18/07'                                      
         END                                                                    
