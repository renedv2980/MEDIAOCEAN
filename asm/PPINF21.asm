*          DATA SET PPINF21    AT LEVEL 018 AS OF 05/01/02                      
*PHASE T41A21A,+0,NOAUTO       NOTE: "A" PHASE                                  
*                                                                               
* KWAN 05/99    CORRECT FILTER ERROR DISPLAY                                    
*                                                                               
         TITLE 'T41A21   PRINTPAK  INFO  COMMENTS'                              
T41A21   CSECT                                                                  
         NMOD1 40,T41A21                                                        
         L     RC,0(R1)                                                         
         USING GENOLD,RC                                                        
         L     RA,4(R1)                                                         
         USING T41AFFD,RA                                                       
         USING FLDHDRD,R2                                                       
         LA    R5,REC              SET RECORD ADDRESS                           
         ST    R5,AREC                                                          
         SPACE 2                                                                
*                                                                               
         MVI   FLTSW,0             INITIALIZE FILTER SWITCH                     
*                                                                               
SETFLTR  DS    0H                  SET FILTER                                   
         MVC   COMFLTR,=6C' '                                                   
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(7,=C'FILTER=')                        
         OC    4(4,R1),4(R1)                                                    
         BZ    SETLINES                                                         
         L     R4,4(R1)                                                         
         LR    R7,R4                                                            
         LA    R4,7(R4)                                                         
         LA    RE,7                                                             
         XC    WORK,WORK                                                        
         MVC   WORK(11),=C'FILTER=1-6X'                                         
         XC    ELEM(8),ELEM                                                     
         MVC   ELEM+2(6),=6C' '                                                 
         LA    R6,ELEM+2                                                        
         LA    R5,6                                                             
*                                                                               
FLTRV3   CLI   0(R4),C','                                                       
         BE    FLTRVO                                                           
         CLI   0(R4),X'00'                                                      
         BE    FLTRVO                                                           
         CLI   0(R4),C'-'          NEGATIVE FILTER                              
         BNE   FLTRV5                                                           
         MVI   ELEM,1                                                           
         LA    R4,1(R4)                                                         
FLTRV5   MVC   0(1,R6),0(R4)                                                    
         CLI   0(R6),C'*'                                                       
         BNE   FLTRV7                                                           
         CLI   ELEM,0                                                           
         BNE   FLTRVE                                                           
         B     FLTRV10                                                          
*                                                                               
FLTRV7   CLI   0(R6),C'A'                                                       
         BL    FLTRVE                                                           
         CLI   0(R6),C'9'                                                       
         BH    FLTRVE                                                           
         CLI   ELEM,1                                                           
         BNE   *+8                                                              
         NI    0(R6),X'BF'         NEGATIVE FILTER                              
*                                                                               
FLTRV10  LA    R6,1(R6)                                                         
         MVI   ELEM,0                                                           
         LA    R4,1(R4)                                                         
         BCT   R5,FLTRV3                                                        
         CLI   0(R4),C','          MUST BE END OF STRING                        
         BE    FLTRVO                                                           
         CLI   0(R4),X'00'                                                      
         BE    FLTRVO                                                           
         B     FLTRVE                                                           
FLTRVO   DS    0H                                                               
         MVC   COMFLTR,ELEM+2                                                   
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
*                                                                               
         B     SETLINES                                                         
*                                                                               
FLTRVE   LR    R6,R7               RESTORE,R6                                   
         B     FLTERR                                                           
*                                                                               
SETLINES MVI   LINES,1             SET DEFAULT                                  
         XC    WORK,WORK                                                        
         MVC   WORK(12),=C'LINES=NN,ALL'                                        
         GOTO1 GETFLTR,DMCB,(64,SINIFLT),(6,=C'LINES=')                         
         OC    4(4,R1),4(R1)                                                    
         BZ    SETSCRN                                                          
         L     R4,4(R1)                                                         
         LA    R4,6(R4)                                                         
         CLC   0(3,R4),=C'ALL'     ACCEPT ALL SET TO 14                         
         BNE   SETL5                                                            
         MVI   LINES,14                                                         
         B     SETLX                                                            
SETL5    BAS   R9,GETNUM                                                        
         L     R4,4(R1)            RESET R4                                     
         LA    R4,6(R4)                                                         
         L     R6,4(R1)                                                         
         LA    RE,6                                                             
         LTR   R5,R5                                                            
         BZ    FLTERR                                                           
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R4)       * EXECUTED *                                   
         CP    DUB,=P'14'                                                       
         BH    FLTERR                                                           
         CVB   R0,DUB                                                           
         STC   R0,LINES                                                         
*                                                                               
SETLX    DS    0H                                                               
*                                                                               
         MVI   FLTSW,1             CONTAIN A VALID FIELD                        
*                                                                               
SETSCRN  DS    0H                                                               
*                                                                               
*                                                                               
*                                                                               
         CLI   SINIFLTH+5,0        ANY BAD INPUTS?                              
         BE    SETSCR30                                                         
         CLI   FLTSW,1                                                          
         BE    SETSCR30                                                         
         LA    R2,SINIFLTH         POINT TO FILTER FIELD                        
         LA    R3,2                FIELD INVALID ERR MSG                        
         B     ERROR                                                            
*                                                                               
*                                                                               
*                                                                               
SETSCR30 LA    R2,SINHDRH                                                       
         MVC   FLDDATA+1(7),=C'COMMENT'                                         
         MVC   FLDDATA+9(10),=C'FIRST LINE'                                     
         CLI   LINES,1                                                          
         BE    SETS5                                                            
         ZIC   R0,LINES                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  FLDDATA+15(2),DUB                                                
         MVC   FLDDATA+17(6),=C' LINES'                                         
*                                                                               
SETS5    DS    0H                                                               
         FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         MVC   FLDDATA+1(7),DASH                                                
         MVC   FLDDATA+9(10),DASH                                               
         CLI   LINES,1                                                          
         BNH   SETS8                                                            
         MVC   FLDDATA+9(14),DASH                                               
SETS8    FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         LA    R2,LINLEN(R2)                                                    
         EJECT                                                                  
         LA    R9,14                                                            
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING COMRECD,R5                                                       
         MVC   KEY,SVKEY                                                        
         OC    PREVKEY,PREVKEY                                                  
         BZ    GBHIGH                                                           
         MVC   KEY,PREVKEY                                                      
         XC    PREVKEY,PREVKEY                                                  
GBHIGH   BAS   RE,HIGH                                                          
         B     HAVREC                                                           
*                                                                               
GBSEQ    BAS   RE,SEQ                                                           
HAVREC   LA    R5,KEY                                                           
         USING COMRECD,R5                                                       
         CLC   KEY(4),KEYSAVE                                                   
         BNE   BEND                                                             
         CLC   COMFLTR,=6C' '                                                   
         BE    PRO2X               NO FILTER                                    
         LA    R5,5                LEFT ALIGN KEY+4 IN WORK                     
         LA    R6,KEY+4                                                         
         XC    WORK(6),WORK                                                     
*                                                                               
PRO2B    CLI   0(R6),C' '                                                       
         BNE   PRO2D              NON SPACE ENCOUNTERED                         
         LA    R6,1(R6)                                                         
         BCTR  R5,0                                                             
         B     PRO2B                                                            
*                                                                               
PRO2D    EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),0(R6)                                                    
         LA    R6,COMFLTR                                                       
         LA    R4,WORK                                                          
         LA    R5,6                                                             
*                                                                               
PRO2F    CLI   0(R6),C'*'          ANY CHAR IS OK                               
         BE    PRO2J                                                            
         CLI   0(R6),C' '                                                       
         BE    PRO2J                                                            
         TM    0(R6),X'40'         TEST NEGATIVE FILTER                         
         BZ    PRO2H                                                            
         CLC   0(1,R4),0(R6)                                                    
         BE    PRO2J                                                            
         B     GBSEQ               BYPASS                                       
*                                                                               
PRO2H    MVC   DUB(1),0(R6)                                                     
         OI    DUB,X'40'                                                        
         CLC   DUB(1),0(R4)                                                     
         BE    GBSEQ               BYPASS                                       
*                                                                               
PRO2J    LA    R6,1(R6)                                                         
         LA    R4,1(R4)                                                         
         BCT   R5,PRO2F                                                         
*                                                                               
PRO2X    DS    0H                                                               
         L     R5,AREC                                                          
         BAS   RE,GETREC              GET A RECORD                              
         EJECT                                                                  
* END OF FILTERS SO DISPLAY RECORD                                              
         MVC   FLDDATA+1(6),PCOMKNUM                                            
         LA    R6,PCOMCELM                                                      
         ZIC   R8,LINES                                                         
COM4     CLI   0(R6),0             END OF REC                                   
         BE    COM15                                                            
         CLI   0(R6),X'40'                                                      
         BNE   COM5                                                             
         CLI   1(R6),3             SKIP SPACE LINES                             
         BNH   COM5                                                             
         ZIC   R7,1(R6)                                                         
         SH    R7,=H'3'                                                         
         EX    R7,*+8                                                           
         B     COM10                                                            
*                                                                               
         MVC   FLDDATA+9(0),2(R6)      EXECUTED                                 
*                                                                               
COM5     ZIC   R0,1(R6)                                                         
         AR    R6,R0                                                            
         B     COM4                                                             
*                                                                               
COM10    FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCTR  R9,0                                                             
         BCT   R8,COM5                                                          
         B     COM20                                                            
COM15    FOUT  (R2)                                                             
         LA    R2,LINLEN(R2)                                                    
         BCTR  R9,0                                                             
*                                                                               
COM20    DS    0H                                                               
         LTR   R9,R9                                                            
         BNP   COM30                                                            
         ZIC   R8,LINES                                                         
         CR    R9,R8                                                            
         BL    COM30               NEXT COMMENT MAY NOT FIT                     
         B     GBSEQ                                                            
COM30    BAS   RE,SEQ                                                           
         CLC   KEY(4),KEYSAVE                                                   
         BNE   MODEXIT                                                          
         MVC   PREVKEY,KEY                                                      
         B     MODEXIT                                                          
*                                                                               
BEND     DS    0H                                                               
         B     MODEXIT                                                          
         EJECT                                                                  
FLTERR   BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SINMSG(0),0(R6)                                                  
         LA    RE,3(RE)                                                         
         LA    RF,SINMSG(RE)                                                    
         MVC   0(22,RF),=C'- INVALID FILTER FIELD'                              
         LA    RF,23(RF)                                                        
         MVC   0(20,RF),WORK                                                    
         LA    R2,SINIFLTH                                                      
         MVI   ERRCD,X'FF'                                                      
         MVI   ERRAREA,X'FF'                                                    
         B     MODEXIT2                                                         
*                                                                               
GETNUM   LA    R5,0                                                             
GN1      CLI   0(R4),C'-'                                                       
         BER   R9                                                               
         CLI   0(R4),C','                                                       
         BER   R9                                                               
         CLI   0(R4),X'00'                                                      
         BER   R9                                                               
         CLI   0(R4),C' '                                                       
         BER   R9                                                               
         CLI   0(R4),C'0'                                                       
         BL    GNERR                                                            
         CLI   0(R4),C'9'                                                       
         BH    GNERR                                                            
         LA    R4,1(R4)                                                         
         LA    R5,1(R5)                                                         
         B     GN1                                                              
GNERR    SR    R5,R5                                                            
         BR    R9                                                               
         SPACE 2                                                                
LINES    DS    CL1                                                              
COMFLTR  DS    CL6                 FILTER                                       
FLTSW    DS    XL1                                                              
         EJECT                                                                  
MODEXIT  LA    R2,SINIKEYH                                                      
         OC    PREVKEY,PREVKEY                                                  
         BZ    *+8                                                              
         LA    R2,SINENDH                                                       
MODEXIT2 OI    6(R2),X'C0'                                                      
         XMOD1 1                                                                
       ++INCLUDE PPGENEROL                                                      
DASH     DC    40C'-'                                                           
HIGHK    DC    10X'FF'                                                          
LINLEN   EQU   88                                                               
         LTORG                                                                  
         EJECT                                                                  
COMRECD DSECT                                                                   
       ++INCLUDE PCOMREC                                                        
*                                                                               
       ++INCLUDE PPSINFOWRK                                                     
