*          DATA SET MPTST00    AT LEVEL 093 AS OF 05/01/02                      
*PHASE T73000,+0,NOAUTO                                                         
*INCLUDE MPXTFAC                                                                
*INCLUDE CCVAL                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
T73000   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (WRKX-GENOLD),MPTST00                                            
         USING GENOLD,RC                                                        
         USING T730FFD,RA                                                       
         SPACE 2                                                                
         BAS   RE,INITL                                                         
         RELOC (R3)                                                             
*                                                                               
         XC    TSTMSG,TSTMSG                                                    
         FOUT  TSTMSGH                                                          
*                                                                               
*                                                                               
MP02     DS    0H                                                               
         LA    R9,WKA1                                                          
         ST    R9,AMPXB                                                         
         USING MPXBLKD,R9                                                       
*                                                                               
         MVC   MPXBDIR,=CL8'XVTDIR'                                             
         MVC   MPXBFLE,=CL8'XVTFILE'                                            
         MVC   MPXBNCRD,=H'98'                                                  
         MVC   MPXBNCOL,=H'80'                                                  
         L     RF,=V(CCVAL)                                                     
         AR    RF,R3                                                            
         ST    RF,CCVAL                                                         
         L     RF,=V(MPXTFAC)                                                   
         AR    RF,R3                                                            
         ST    RF,MPXTFAC                                                       
         L     RF,=V(HEXIN)                                                     
         AR    RF,R3                                                            
         ST    RF,HEXIN                                                         
         L     RF,=V(HEXOUT)                                                    
         AR    RF,R3                                                            
         ST    RF,HEXOUT                                                        
*                                                                               
         LA    R2,TSTLN7H                                                       
         LA    R0,17                                                            
MP2      DS    0H                                                               
         XC    8(70,R2),8(R2)                                                   
         FOUT  (R2)                                                             
         LA    R2,78(R2)                                                        
         BCT   R0,MP2                                                           
*                                                                               
         MVC   MPXBDMGR,VDATAMGR                                                
         L     RF,=A(BSIO-GENOLD)                                               
         AR    RF,RC                                                            
         ST    RF,MPXBAREC                                                      
         L     RF,=A(BSSTK-GENOLD)                                              
         AR    RF,RC                                                            
         ST    RF,MPXBSTK                                                       
*                                                                               
         LA    R2,TSTLN1H                                                       
         LA    R0,5                                                             
MP3      DS    0H                                                               
         BAS   RE,EDLIN                                                         
         CLI   ERR,0                                                            
         BE    MP3B                                                             
         ZIC   R3,ERR                                                           
         B     ERROR                                                            
MP3B     DS    0H                                                               
         LA    R2,78(R2)                                                        
         BCT   R0,MP3                                                           
         LA    R2,TSTLN1H                                                       
         B     EXIT                                                             
         SPACE 3                                                                
EDLIN    NTR1                                                                   
         MVI   ERR,0                                                            
         LA    RF,70(R2)                                                        
         CLI   0(RF),C' '                                                       
         BH    *+8                                                              
         BCT   RF,*-8                                                           
*                                                                               
         LA    R0,7(R2)                                                         
         SR    RF,R0                                                            
         STC   RF,5(R2)                                                         
         BNM   *+8                                                              
         MVI   5(R2),0                                                          
*                                                                               
         CLI   5(R2),0                                                          
         BE    EDLX                                                             
*                                                                               
EDL4     DS    0H                                                               
         CLI   8(R2),C'*'          IGNORE IF *                                  
         BE    EDLX                                                             
         CLC   =C'WGT',8(R2)        WEIGHT CARD,COL,LEN,PRE                     
*                                  NN,NN,NN,NN                                  
         BNE   EDL5                                                             
         LA    R4,12(R2)                                                        
         PACK  DUB,0(2,R4)                                                      
         CVB   R0,DUB                                                           
         STH   R0,MPXBCRDW                                                      
         PACK  DUB,3(2,R4)                                                      
         CVB   R0,DUB                                                           
         STH   R0,MPXBCOLW                                                      
         PACK  DUB,6(2,R4)                                                      
         CVB   R0,DUB                                                           
         STC   R0,MPXBLENW                                                      
         PACK  DUB,9(2,R4)                                                      
         CVB   R0,DUB                                                           
         STC   R0,MPXBWPRE                                                      
         B     EDLX                                                             
*                                                                               
EDL5     DS    0H                                                               
         CLC   =C'NRES',8(R2)      NRES                                         
         BNE   EDL10                                                            
         LA    R6,13(R2)                                                        
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'5'                                                         
         BAS   RE,NUMIN                                                         
         BNE   EDLERR                                                           
         MVC   MPXBNRES,FULL                                                    
         L     RF,FULL                                                          
         LA    RF,7(RF)                                                         
         SRA   RF,3                                                             
         STH   RF,MPXBBSLN                                                      
         B     EDLX                                                             
*                                                                               
EDL10    DS    0H                                                               
         CLC   =C'MAXSTK',8(R2)    MAXSTK                                       
         BNE   EDL14                                                            
*                                                                               
         LA    R6,15(R2)                                                        
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'7'                                                         
         BAS   RE,NUMIN                                                         
         BNE   EDLERR                                                           
         MVC   MPXBSTMX,FULL+3                                                  
         B     EDLX                                                             
*                                                                               
EDL14    DS    0H                                                               
         CLC   =C'CLEAR',8(R2)     CLEAR                                        
         BNE   EDL16                                                            
*                                                                               
         XC    MPXBNRES(6),MPXBNRES                                             
         XC    MPXBUWC(MPXBX-MPXBUWC),MPXBUWC                                   
         B     EDLX                                                             
*                                                                               
EDL16    DS    0H                                                               
         CLC   =C'DUMP',8(R2)      DUMP                                         
         BNE   EDL20                                                            
         MVI   AMPXB,0                                                          
         CLI   13(R2),C'Y'                                                      
         BNE   *+8                                                              
         MVI   AMPXB,1                                                          
         CLI   13(R2),C'F'         FORCE DUMP                                   
         BNE   *+8                                                              
         MVI   AMPXB,X'FF'                                                      
         B     EDLX                                                             
*                                                                               
EDL20    DS    0H                                                               
         CLC   =C'PROC',8(R2)      PROC                                         
         BNE   EDL22                                                            
*                                                                               
         LA    R6,13(R2)                                                        
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'5'                                                         
         BAS   RE,HXIN                                                          
         BNE   EDLERR                                                           
*                                                                               
EDL20A   DS    0H                                                               
         GOTO1 MPXTFAC,DMCB,=C'PROC',AMPXB,HXLIN                                
EDL20B   DS    0H                                                               
         CLI   MPXBERR,0                                                        
         BNE   EDL21                                                            
         GOTO1 (RF),(R1),=C'XMULT',AMPXB,(C'B',MPXBSTK),0                       
*                                                                               
EDL21    DS    0H                                                               
         LA    R5,TSTLN6H+8                                                     
         MVC   0(70,R5),COUNTLN                                                 
         EDIT  MPXBUWC,(7,05(R5))                                               
         EDIT  MPXBWTC,(7,18(R5))                                               
*                                                                               
         SH    R5,=H'8'                                                         
         FOUT  (R5)                                                             
         LH    R5,MPXBBSLN                                                      
         L     R6,MPXBSTK                                                       
         BAS   RE,HXPRT                                                         
         B     EDLX                                                             
*                                                                               
EDL22    DS    0H                                                               
         CLC   =C'XX',8(R2)                                                     
         BNE   EDL26                                                            
         DC    H'0'                                                             
*                                                                               
EDL26    DS    0H                                                               
         CLC   =C'CC',8(R2)        CARD COL SPEC                                
         BNE   EDL29                                                            
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'3'                                                         
         XC    HXLIN,HXLIN                                                      
*                                                                               
         GOTO1 CCVAL,DMCB,((R5),8(R2)),HXLIN,MPXBLKD,0                          
*                                                                               
         LA    R4,TSTLN3                                                        
         MVC   0(4,R4),=C'ERR='                                                 
         GOTO1 HEXOUT,DMCB,MPXBERR,4(R4),1,=C'N'                                
         MVC   7(20,R4),MPXBEMSG                                                
         CLI   MPXBERR,0                                                        
         BNE   EDLX                                                             
*                                                                               
         GOTO1 HEXOUT,DMCB,HXLIN,OUTLIN,70,=C'N'                                
         MVC   TSTLN4,OUTLIN                                                    
         MVC   TSTLN5,OUTLIN+70                                                 
*                                                                               
         B     EDL20A         PROCESS STRING                                    
*                                                                               
EDL29    DS    0H                                                               
EDLERR   DS    0H                                                               
         MVI   ERR,2                                                            
EDLX     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
HXPRT    NTR1                                                                   
         LA    RE,OUTLIN                                                        
         LA    RF,70*17                                                         
         XCEF                                                                   
         GOTO1 HEXOUT,DMCB,MPXBERR,DUB,1                                        
         LA    R7,TSTLN7H+8                                                     
         MVC   0(6,R7),=C'ERROR='                                               
         MVC   6(2,R7),DUB                                                      
*                                                                               
HXP4     DS    0H                                                               
         CH    R5,=Y(L'OUTLIN/2)                                                
         BNH   *+8                                                              
         LA    R5,L'OUTLIN/2                                                    
         LA    RE,OUTLIN                                                        
         LA    RF,L'OUTLIN                                                      
         XCEF                                                                   
         GOTO1 HEXOUT,DMCB,(R6),OUTLIN,(R5),=C'N'                               
         LA    R4,OUTLIN                                                        
         LA    R3,TSTLN9H+8                                                     
         LA    R0,17                                                            
HXP5     DS    0H                                                               
         MVC   0(70,R3),0(R4)                                                   
         LA    R3,78(R3)                                                        
         LA    R4,70(R4)                                                        
         FOUT (R3)                                                              
         BCT   R0,HXP5                                                          
*                                                                               
HXPX     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
HXIN     NTR1                                                                   
         GOTO1 HEXIN,DMCB,(R6),HXLIN,(R5)                                       
         SR    R0,R0                                                            
         OC    DMCB+12(4),DMCB+12                                               
         BZ    HXIN2                                                            
         SR    R0,R0                                                            
         B     *+6                                                              
HXIN2    DS    0H                                                               
         LTR   RE,RE                                                            
         XIT1                                                                   
         SPACE 3                                                                
NUMIN    NTR1                                                                   
         XC    FULL,FULL                                                        
         SH    R5,=H'1'                                                         
         BM    NUMX                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         PACK  DUB,0(0,R6)                                                      
         CVB   R0,DUB                                                           
         ST    R0,FULL                                                          
         SR    R0,R0                                                            
NUMX     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
COUNTLN  DC    CL70'RAW= NNNNNNN WGTD=NNNNNNN'                                  
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
EXXMOD   DS    0H                                                               
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
         SPACE 2                                                                
*                                                                               
*                                                                               
         ORG   T73000+4070                                                      
         DC    X'0000'                                                          
*                                                                               
       ++INCLUDE FLDIND                                                         
*                                                                               
       ++INCLUDE GENOLD                                                         
         SPACE 2                                                                
ERR      DS    X                                                                
AMPXB    DS    A                                                                
HEXIN    DS    A                                                                
HEXOUT   DS    A                                                                
MPXTFAC  DS    A                                                                
CCVAL    DS    A                                                                
HXLIN    DS    CL70                                                             
OUTLIN   DS    CL1190              17*70                                        
         ORG                                                                    
BSIO     DS    4000X                                                            
BSSTK    DS    12500X                                                           
WRKX     EQU   *                                                                
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE MPTSTFFD                                                       
*                                                                               
WKA1     DS    XL200                                                            
TWAX     EQU   *                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE MPXBLKD                                                        
*                                                                               
       ++INCLUDE MPXVARD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093MPTST00   05/01/02'                                      
         END                                                                    
