*          DATA SET MPTST00S   AT LEVEL 089 AS OF 05/01/02                      
*PHASE T73000,+0,NOAUTO                                                         
*INCLUDE MPXTFAC                                                                
*INCLUDE CCVAL                                                                  
*INCLUDE HEXOUT                                                                 
*INCLUDE HEXIN                                                                  
T73000   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 1700,MPTST00                                                     
         USING GENOLD,RC                                                        
         USING T730FFD,RA                                                       
         SPACE 2                                                                
         BAS   RE,INITL                                                         
         RELOC (R3)                                                             
*                                                                               
         XC    TSTMSG,TSTMSG                                                    
         FOUT  TSTMSGH                                                          
*                                                                               
         CLI   FIRSTSW,0                                                        
         BNE   MP01                                                             
         MVI   FIRSTSW,1                                                        
         B     MP02                                                             
*                                  READ TEMPSTR                                 
MP01     DS    0H                                                               
         XC    DMCB+8(4),DMCB+8                                                 
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    TERM NO.                                     
         LA    R4,TMPWRK                                                        
         MVI   DMCB+9,X'FF'                                                     
         MVI   DMCB+8,1                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,(R4),(TERMNAL,0)           
         MVI   DMCB+9,X'FF'                                                     
         MVI   DMCB+8,2                                                         
         LA    R4,2500(R4)                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMRDIR',=C'TEMPSTR',,(R4),(TERMNAL,0)           
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
MP02     DS    0H                                                               
         LA    R9,WKA1                                                          
         ST    R9,AMPXB                                                         
         USING MPXBLKD,R9                                                       
*                                                                               
         MVC   MPXBDIR,=CL8'XVTDIR'                                             
         MVC   MPXBFLE,=CL8'XVTFILE'                                            
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
         LA    RF,FLTLST                                                        
         ST    RF,MPXBFLST                                                      
         LA    RF,VARLST                                                        
         ST    RF,MPXBVLST                                                      
         LA    RF,BVLST                                                         
         ST    RF,MPXBBLST                                                      
*                                                                               
         LA    R2,TSTLN7H                                                       
         LA    R0,10                                                            
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
         L     R3,MPXBFLST                                                      
         LA    RF,FILT0                                                         
         ST    RF,0(R3)                                                         
         LA    RF,FILT1                                                         
         ST    RF,4(R3)                                                         
         LA    RF,FILT2                                                         
         ST    RF,8(R3)                                                         
         MVI   12(R3),X'FF'         SET EOL                                     
*                                                                               
         L     R3,MPXBVLST                                                      
         USING MPXVARD,R3                                                       
         LA    RF,VARTB0                                                        
         ST    RF,MPXVSTR                                                       
         LA    R3,MPXVARL(R3)                                                   
         LA    RF,VARTB1                                                        
         ST    RF,MPXVSTR                                                       
         LA    R3,MPXVARL(R3)                                                   
         LA    RF,VARTB2                                                        
         ST    RF,MPXVSTR                                                       
         MVI   MPXVARL(R3),X'FF'                                                
*                                                                               
         L     R3,MPXBBLST         BITVARS                                      
         USING MPXVARD,R3                                                       
         LA    RF,BVTB0                                                         
         ST    RF,MPXVSTR                                                       
         LA    R3,MPXVARL(R3)                                                   
         LA    RF,BVTB1                                                         
         ST    RF,MPXVSTR                                                       
         LA    R3,MPXVARL(R3)                                                   
         LA    RF,BVTB2                                                         
         ST    RF,MPXVSTR                                                       
         MVI   MPXVARL(R3),X'FF'                                                
*                                                                               
         LA    R2,TSTLN1H                                                       
         LA    R0,6                                                             
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
*                                                                               
*                                  WRITE TEMPSTR                                
         XC    DMCB+8(4),DMCB+8                                                 
         L     RF,VTWA                                                          
         MVC   DMCB+10(2),2(RF)    TERM NO.                                     
         LA    R4,TMPWRK                                                        
         MVI   DMCB+9,X'FF'                                                     
         MVI   DMCB+8,1                                                         
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R4),(TERMNAL,0)            
         MVI   DMCB+9,X'FF'                                                     
         MVI   DMCB+8,2                                                         
         LA    R4,2500(R4)                                                      
         GOTO1 VDATAMGR,DMCB,=C'DMWRT',=C'TEMPSTR',,(R4),(TERMNAL,0)            
         TM    DMCB+8,X'FF'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
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
         CLC   =C'WV',8(R2)        WEIGHT VAR SET                               
         BNE   EDL5                                                             
         MVC   MPXBWV+1(1),11(R2)  VAR NO                                       
         NI    MPXBWV+1,X'0F'                                                   
         CLI   MPXBWV+1,2                                                       
         BH    EDLERR                                                           
         CLI   MPXBWV+1,1                                                       
         BL    EDLERR                                                           
         B     EDLX                                                             
*                                                                               
EDL5     DS    0H                                                               
         CLC   =C'WQ',8(R2)        WEIGHT QUESTION SET                          
         BNE   EDL5D                                                            
         MVC   MPXBWQ(4),11(R2)    Q                                            
         OC    MPXBWQ,=4C' '                                                    
         XC    MPXBWV,MPXBWV       CLEAR VARIABLE WGT                           
         B     EDLX                                                             
*                                                                               
EDL5D    DS    0H                                                               
         CLC   =C'WS',8(R2)        WEIGHT SECTION SET                           
         BNE   EDL6                                                             
         XC    MPXBWSCT,MPXBWSCT                                                
         CLI   11(R2),C' '                                                      
         BNE   EDLX                                                             
         MVC   MPXBWSCT(4),11(R2)    SECTION                                    
         OC    MPXBWSCT,=4C' '                                                  
         B     EDLX                                                             
*                                                                               
EDL6     DS    0H                                                               
         CLI   8(2),C'V'           VARIABLE SET                                 
         BNE   EDL7                                                             
         CLI   9(R2),C'0'                                                       
         BL    EDL7                                                             
         CLI   9(R2),C'2'                                                       
         BH    EDL7                                                             
         MVC   BYTE,9(R2)                                                       
         NI    BYTE,X'0F'                                                       
         ZIC   R7,BYTE                                                          
         LR    RF,R7                                                            
         MH    R7,=Y(MPXVARL)                                                   
         A     R7,MPXBVLST                                                      
         USING MPXVARD,R7                                                       
         STH   RF,MPXVNO                                                        
         CLI   11(R2),C'0'         TYPE                                         
         BL    EDLERR                                                           
         CLI   11(R2),C'4'                                                      
         BH    EDLERR                                                           
         MVC   MPXVDLEN,11(R2)                                                  
         NI    MPXVDLEN,X'0F'                                                   
         L     RE,MPXVSTR          CLEAR VARIABLE AREA                          
         LA    RF,L'VARTB1                                                      
         XCEF                                                                   
*                                                                               
EDL6B    DS    0H                                                               
         CLI   13(R2),C'0'         PREC                                         
         BL    EDLERR                                                           
         CLI   13(R2),C'5'                                                      
         BH    EDLERR                                                           
         MVC   MPXVPREC,13(R2)                                                  
         NI    MPXVPREC,X'0F'                                                   
         B     EDLX                                                             
*                                                                               
*                                                                               
EDL7     DS    0H                                                               
         CLI   8(2),C'B'           BITVAR SET                                   
         BNE   EDL8                                                             
         CLI   9(R2),C'0'                                                       
         BL    EDL8                                                             
         CLI   9(R2),C'2'                                                       
         BH    EDL8                                                             
         MVC   BYTE,9(R2)                                                       
         NI    BYTE,X'0F'                                                       
         ZIC   R7,BYTE                                                          
         LR    RF,R7                                                            
         MH    R7,=Y(MPXVARL)                                                   
         A     R7,MPXBBLST                                                      
         USING MPXVARD,R7                                                       
         STH   RF,MPXVNO                                                        
         MVI   MPXVDLEN,0                                                       
         L     RE,MPXVSTR          CLEAR VARIABLE AREA                          
         LA    RF,L'BVTB1                                                       
         XCEF                                                                   
         B     EDLX                                                             
*                                                                               
EDL8     DS    0H                                                               
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
         BNE   EDL12                                                            
*                                                                               
         LA    R6,15(R2)                                                        
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'7'                                                         
         BAS   RE,NUMIN                                                         
         BNE   EDLERR                                                           
         MVC   MPXBSTMX,FULL+3                                                  
         B     EDLX                                                             
*                                                                               
EDL12    DS    0H                  VAR PTR                                      
         CLC   =C'VP',8(R2)                                                     
         BNE   EDL14                                                            
         MVC   MPXBVPTR+1,11(R2)                                                
         NI    MPXBVPTR+1,X'0F'                                                 
         B     EDLX                                                             
*                                                                               
EDL14    DS    0H                                                               
         CLC   =C'CLEAR',8(R2)     CLEAR                                        
         BNE   EDL16                                                            
*                                                                               
         XC    MPXBNRES(6),MPXBNRES                                             
         LA    RE,FILT1                                                         
         LH    RF,=Y(TMPWRKX-FILT1)                                             
         XCEF                                                                   
         L     RF,MPXBFLST         DE-ACTIVATE FILTERS                          
*                                                                               
         CLI   0(RF),X'FF'                                                      
         BE    *+16                                                             
         MVI   0(RF),0                                                          
         LA    RF,4(RF)                                                         
         B     *-16                                                             
*                                                                               
         L     RF,MPXBVLST                                                      
         LA    R0,3                                                             
         XC    0(4,RF),0(RF)                                                    
         LA    RF,MPXVARL(RF)                                                   
         BCT   R0,*-10                                                          
         L     RF,MPXBBLST                                                      
         LA    R0,3                                                             
         XC    0(4,RF),0(RF)                                                    
         LA    RF,MPXVARL(RF)                                                   
         BCT   R0,*-10                                                          
*                                                                               
         XC    MPXBUWC(30),MPXBUWC                                              
         B     EDLX                                                             
*                                                                               
EDL16    DS    0H                                                               
         CLC   =C'DUMP',8(R2)      DUMP                                         
         BNE   EDL18                                                            
         MVI   AMPXB,0                                                          
         CLI   13(R2),C'Y'                                                      
         BNE   *+8                                                              
         MVI   AMPXB,1                                                          
         CLI   13(R2),C'F'         FORCE DUMP                                   
         BNE   *+8                                                              
         MVI   AMPXB,X'FF'                                                      
         B     EDLX                                                             
*                                                                               
EDL18    DS    0H                                                               
         CLC   =C'PRTVAR',8(R2)    PRTVAR                                       
         BNE   EDL20                                                            
         CLI   15(R2),C'0'                                                      
         BL    EDLERR                                                           
         CLI   15(R2),C'2'                                                      
         BH    EDLERR                                                           
         XC    WORK,WORK                                                        
         MVC   WORK+1(1),15(R2)                                                 
         NI    WORK+1,X'0F'                                                     
         LH    R6,WORK                                                          
         MH    R6,=Y(MPXVARL)                                                   
         A     R6,MPXBVLST                                                      
         L     R6,MPXVSTR-MPXVARD(R6)                                           
         LA    R5,L'VARTB1                                                      
         BAS   RE,HXPRT                                                         
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
         GOTO1 MPXTFAC,DMCB,=C'PROC',AMPXB,HXLIN                                
EDL20B   DS    0H                                                               
         CLI   MPXBERR,0                                                        
         BNE   EDL21                                                            
         GOTO1 (RF),(R1),=C'XMULT',AMPXB,(C'B',MPXBSTK),0                       
*                                                                               
EDL21    DS    0H                                                               
         LA    R5,TSTLN7H+8                                                     
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
         CLC   =C'XMULT',8(R2)     XMULT                                        
         BNE   EDL28                                                            
*                                                                               
         XC    DMCB,DMCB                                                        
         LA    R4,14(R2)                                                        
         LA    R5,DMCB+8                                                        
         BAS   RE,EDL26S                                                        
         LA    R4,17(R2)                                                        
         LA    R5,DMCB+12                                                       
         BAS   RE,EDL26S                                                        
*                                                                               
         GOTO1 MPXTFAC,DMCB,=C'XMULT',AMPXB                                     
         B     EDL21                                                            
*                                                                               
EDL26S   DS    0H                                                               
         CLI   0(R4),C' '                                                       
         BNHR  RE                                                               
         MVC   0(1,R5),0(R4)                                                    
         CLI   0(R4),C'B'                                                       
         BE    EDL26S4                                                          
         CLI   0(R4),C'A'                                                       
         BE    EDL26S6                                                          
         B     EDLERR                                                           
EDL26S4  DS    0H                                                               
         MVC   BYTE,1(R4)                                                       
         NI    BYTE,X'0F'                                                       
         ZIC   R6,BYTE                                                          
         MH    R6,=Y(MPXVARL)                                                   
         A     R6,MPXBBLST                                                      
         MVC   1(3,R5),MPXVSTR+1-MPXVARD(R6)                                    
         BR    RE                                                               
*                                                                               
EDL26S6  DS    0H                                                               
         MVC   BYTE,1(R4)                                                       
         NI    BYTE,X'0F'                                                       
         ZIC   R6,BYTE                                                          
         MH    R6,=Y(MPXVARL)                                                   
         A     R6,MPXBVLST                                                      
         STCM  R6,7,1(R5)                                                       
         BR    RE                                                               
*                                                                               
EDL28    DS    0H                                                               
         CLC   =C'CC',8(R2)        CARD COL SPEC                                
         BNE   EDL29                                                            
         ZIC   R5,5(R2)                                                         
         SH    R5,=H'3'                                                         
*                                                                               
         GOTO1 CCVAL,DMCB,((R5),8(R2)),BSIO,MPXBLKD,0                           
*                                                                               
         LA    R6,BSIO                                                          
         MVC   MPXBERR,12(R1)                                                   
         ZIC   R5,8(R1)                                                         
         BAS   RE,HXPRT                                                         
         B     EDLX                                                             
*                                                                               
EDLERR   DS    0H                                                               
         MVI   ERR,2                                                            
EDLX     DS    0H                                                               
         XIT1                                                                   
         SPACE 3                                                                
HXPRT    NTR1                                                                   
         LA    RE,OUTLIN                                                        
         LA    RF,70*17                                                         
         XCEF                                                                   
         GOTO1 HEXOUT,DMCB,MPXBERR,OUTLIN,2                                     
         LA    R7,TSTLN8H+8                                                     
         MVC   0(6,R7),=C'ERROR='                                               
         MVC   6(2,R7),OUTLIN                                                   
         CLI   MPXBERR2,0                                                       
         BE    HXP4                                                             
         MVI   8(R7),C','                                                       
         MVC   9(2,R7),OUTLIN+2                                                 
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
COUNTLN  DC    CL70'RAW= NNNNNNN WGT1=NNNNNNN'                                  
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
HXLIN    DS    CL35                                                             
OUTLIN   DS    CL1190              17*70                                        
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
TMPWRK   DS    5000X                                                            
         ORG   TMPWRK                                                           
WKA1     DS    XL100                                                            
WKA2     DS    XL100                                                            
FLTLST   DS    XL41                                                             
VARLST   DS    XL201                                                            
BVLST    DS    XL201                                                            
FILT0    DS    XL33                                                             
FILT1    DS    XL33                                                             
FILT2    DS    XL33                                                             
BVTB0    DS    XL33                                                             
BVTB1    DS    XL33                                                             
BVTB2    DS    XL33                                                             
VARTB0   DS    XL1200                                                           
VARTB1   DS    XL1200                                                           
VARTB2   DS    XL1200                                                           
TMPWRKX  DS    X                                                                
         ORG                                                                    
BSIO     DS    1000X                                                            
BSSTK    DS    6000X                                                            
*                                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE MPTSTFFD                                                       
*                                                                               
FIRSTSW  DS    X                                                                
TWAX     EQU   *                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE MPXBLKD                                                        
*                                                                               
       ++INCLUDE MPXVARD                                                        
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'089MPTST00S  05/01/02'                                      
         END                                                                    
