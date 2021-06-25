*          DATA SET SPGOL22    AT LEVEL 014 AS OF 12/18/17                      
*PHASE T20222B                                                                  
*INCLUDE HEXOUT                                                                 
         TITLE 'SPGOL22 - BRDDOL/BRDPCT RECALL   T20222'                        
T20222   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T20222                                                         
         L     RC,0(R1)                                                         
         LA    R7,2048(RC)                                                      
         LA    R7,2048(R7)                                                      
         USING GENOLD,RC,R7                                                     
*                                                                               
         L     RA,4(R1)                                                         
         USING T202FFD,RA                                                       
*                                                                               
         LA    R2,GOLMSGH                                                       
         XC    GOLMSG,GOLMSG                                                    
         MVC   8(29,R2),=C'THIS PROGRAM NO LONGER IN USE'                       
         OI    6(R2),X'80'                                                      
*                                                                               
         XC    BDATA,BDATA         CLEAR OLD DATA VALUES                        
         LA    R2,GOLACT1H                                                      
*                                                                               
         LA    R9,ACTV                                                          
         CLC   =C'RA',8(R2)                                                     
         BE    RCLEDT                                                           
         LA    R9,WKS                                                           
         CLC   =C'RW',8(R2)                                                     
         BE    RCLEDT                                                           
         LA    R9,RCL                                                           
         CLC   =C'RM',8(R2)                                                     
         BE    RCLEDT                                                           
         CLI   8(R2),C'R'                                                       
         BE    RCLEDT                                                           
         LA    R9,TOT                                                           
         CLI   8(R2),C'T'                                                       
         BE    RCLEDT                                                           
         CLI   8(R2),C'N'          TEST 'NEXT' REQUEST                          
         BE    RCLNEXT                                                          
         MVI   ERRCD,INVERR                                                     
         B     GLERR                                                            
         SPACE 2                                                                
RCLNEXT  MVI   ERRCD,SEQERR                                                     
         LA    R2,ELEM             LAST ACTION SAVED HERE BY BASE               
         LA    R9,RCL                                                           
         CLC   =C'RM',8(R2)        MUST BE RM OR T                              
         BE    RCLNEXT2                                                         
         LA    R9,TOT                                                           
         CLI   8(R2),C'T'                                                       
         BE    RCLNEXT2                                                         
         LA    R2,GOLACT1H                                                      
         B     GLERR                                                            
*                                                                               
RCLNEXT2 MVI   ERRCD,NOMORERR                                                   
         OC    SVKEY,SVKEY         WERE WE FINISHED                             
         BZ    GLERR               YES - ERROR                                  
*                                                                               
         MVC   GOLACT0H(GOLACT1H-GOLACT0H),ELEM   RESTORE SAVED DATA            
         LA    R2,GOLPER0H                                                      
         MVI   5(R2),13            SET LENGTH TO MAX                            
         CLC   8(13,R2),SPACES     IS ANYTHING THERE                            
         BNE   *+8                 YES                                          
         MVI   5(R2),0             RESET LENGTH                                 
         GOTO1 USER3               'EDTPER'                                     
*                                                                               
         B     RCLEDT30                                                         
         SPACE 2                                                                
RCLEDT   DS    0H                                                               
         XC    SVKEY,SVKEY         NOT 'NEXT' - CLEAR THIS                      
         FOUT  GOLACT0H,GOLACT1,3                                               
* CLEAR THE SCREEN                                                              
         LA    R2,GOLMKT1H                                                      
         TM    4(R2),X'20'         TEST SAME MARKET                             
         BO    RCLEDT4             YES                                          
         FOUT  GOLNAM1H,SPACES,16                                               
*                                                                               
RCLEDT4 XC     GOLDOL1,GOLDOL1                                                  
         FOUT  GOLDOL1H                                                         
*                                                                               
         XC    GOLPTS1,GOLPTS1                                                  
         FOUT  GOLPTS1H                                                         
* CLEAR LINES 2-14                                                              
         LA    R4,GOLPER1H                                                      
         SR    R5,R5                                                            
         IC    R5,0(R4)                                                         
         AR    R4,R5                                                            
*                                                                               
RCLEDT6 IC     R5,0(R4)                                                         
         SH    R5,=H'9'                                                         
         EX    R5,RCLEDTOC                                                      
         BZ    RCLEDT8                                                          
         EX    R5,RCLEDTXC                                                      
         FOUT  (R4)                                                             
*                                                                               
RCLEDT8 LA     R4,9(R5,R4)                                                      
         CLI   0(R4),0                                                          
         BNE   RCLEDT6                                                          
*                                                                               
         B     RCLEDT10                                                         
*                                                                               
RCLEDTOC OC    8(0,R4),8(R4)                                                    
RCLEDTXC XC    8(0,R4),8(R4)                                                    
         EJECT                                                                  
RCLEDT10 DS    0H                                                               
         MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLMKT1H                                                      
         CLI   5(R2),0                                                          
         BNZ   RCLEDT12                                                         
         GOTO1 ERROR                                                            
*                                                                               
RCLEDT12 GOTO1 USER1               'EDTMKT'                                     
*                                                                               
         TM    4(R2),X'20'                                                      
         BO    RCLEDT14                                                         
         FOUT  GOLNAM1H,SVMKTNAM,16                                             
*                                                                               
RCLEDT14 LA    R2,GOLDPT1H                                                      
         CLI   5(R2),0                                                          
         BE    RCLEDT20                                                         
         CLI   SVSCRN,X'FB'        TEST BRDPCT                                  
         BE    RCLEDT16                                                         
         GOTO1 USER2               'EDTDPTLN'                                   
         B     RCLEDT20                                                         
*                                                                               
RCLEDT16 MVI   ERRCD,SLNERR                                                     
         TM    4(R2),X'08'         TEST NUMERIC                                 
         BZ    GLERR                                                            
         ZIC   RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
*                                                                               
         CVB   R0,DUB                                                           
         STC   R0,BSLN                                                          
         MVI   BDPT,C'Z'                                                        
*                                                                               
         SR    R1,R1                                                            
         IC    R1,BSLN                                                          
         GOTO1 USER5               'CHKSLN'                                     
*                                                                               
RCLEDT20 CLC   =C'RM',GOLACT0                                                   
         BE    RCLEDT24                                                         
         CLI   GOLACT0,C'T'                                                     
         BE    RCLEDT24                                                         
         CLC   =C'RA',GOLACT0                                                   
         BE    RCLEDT26                                                         
* SINGLE LINE RECALL MUST SPECIFY DPT/LEN                                       
         MVI   ERRCD,MSSNGERR                                                   
         CLI   BDPT,0                                                           
         BE    GLERR                                                            
         CLI   SVPRD,X'FF'         TEST CPP                                     
         BE    RCLEDT22                                                         
* NOT CPP                                                                       
         CLI   BSLN,0                                                           
         BE    GLERR                                                            
         B     RCLEDT24                                                         
* CPP                                                                           
RCLEDT22 MVI   ERRCD,INVERR                                                     
         CLI   BSLN,30             ** THIS IS FOR COMPATIBILITY **              
         BE    RCLEDT24                                                         
         CLI   BSLN,0                                                           
         BNE   GLERR                                                            
         MVI   BSLN,30                                                          
*                                                                               
RCLEDT24 DS    0H                                                               
         LA    R2,GOLPER1H                                                      
         GOTO1 USER3                                                            
*                                                                               
RCLEDT26 DS    0H                                                               
*                                                                               
         OC    GOLMKT1,SPACES                                                   
         FOUT  GOLMKT0H,GOLMKT1,4                                               
*                                                                               
         OC    GOLDPT1,SPACES                                                   
         FOUT  GOLDPT0H,GOLDPT1,4                                               
*                                                                               
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BNE   RCLEDT28            NO                                           
         XC    GOLPER1,GOLPER1     CLEAR OUT GARBAGE FROM RA                    
         FOUT  GOLPER1H                                                         
*                                                                               
RCLEDT28 TM    GOLPER1H+1,X'20'    TEST DATE IS PROTECTED FIELD                 
         BO    RCLEDT30            YES - DONT MOVE DATA                         
*                                                                               
         OC    GOLPER1,SPACES                                                   
         FOUT  GOLPER0H,GOLPER1,13                                              
         EJECT                                                                  
RCLEDT30 DS    0H                                                               
         MVI   FRSTSW,0            RESET FIRST TIME SWITCH                      
         MVC   KEY,SVKEY           MOVE KEY FOR 'NEXT'                          
         OC    KEY(13),KEY         IS IT THERE                                  
         BNZ   RCLHI               YES                                          
*                                                                               
* BUILD GOAL KEY                                                                
*                                                                               
         XC    GKEY,GKEY                                                        
         MVI   GKEYTYPE,2                                                       
         MVC   GKEYAM,SVAGYMD                                                   
         MVC   GKEYCLT,SVCLT                                                    
         MVC   GKEYPRD,SVPRD                                                    
         MVC   GKEYMKT,BMKT                                                     
         MVC   GKEYEST,SVEST                                                    
         MVC   GKEYDPT,BDPT                                                     
         MVC   GKEYSLN,BSLN                                                     
         MVC   GKEYSEC,BSLN                                                     
*                                                                               
         CLI   SVSCRN,X'FB'        IF BRDPCT                                    
         BNE   RCLEDT40                                                         
         MVI   GKEYAGY,X'40'                                                    
         PACK  DUB,GOLTG+2(2)                                                   
         CVB   R1,DUB                                                           
         STC   R1,GKEYPRD2                                                      
*                                                                               
RCLEDT40 MVC   KEY,GKEY                                                         
*                                                                               
RCLHI    GOTO1 HIGH                                                             
*                                                                               
         LA    RE,8                SET FOR A/M/CLT/PRD/MKT/EST COMPARE          
         CLC   GOLACT0(2),=C'RA'                                                
         BE    RCLHI2                                                           
         CLC   GOLACT0(2),=C'RM'                                                
         BE    RCLHI2                                                           
         CLI   GOLACT0,C'T'                                                     
         BE    RCLHI2                                                           
         LA    RE,10               ADD DPT/LEN                                  
*                                                                               
RCLHI2   MVI   ERRCD,NODTAERR                                                   
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE  ** EXECUTED **                                   
         BNE   RCLERR                                                           
*                                                                               
         CLI   SVSCRN,X'FB'        IF NOT BRDPCT                                
         BNE   RCLGET              THEN MATCH IS DONE                           
*                                                                               
         CLC   KEY+11(2),GKEY+11   'AGY' AND TIER #                             
         BE    RCLGET                                                           
         B     RCLSEQ                                                           
RCLERR   XC    SVKEY,SVKEY         DISABLE 'NEXT'                               
         LA    R2,GOLACT1H                                                      
         B     GLERR                                                            
*                                                                               
RCLSEQ   MVC   KEYSAVE,KEY                                                      
         GOTO1 SEQ                                                              
*                                                                               
         CLC   KEY(8),KEYSAVE      A/M/CLT/PRD/MKT/EST                          
         BNE   RCLTOT                                                           
         CLI   SVSCRN,X'FB'        IF NOT BRDPCT                                
         BNE   RCLGET              THEN MATCH IS DONE                           
*                                                                               
         CLC   KEY+11(2),GKEY+11   'AGY' AND TIER #                             
         BE    RCLGET                                                           
         B     RCLSEQ                                                           
*                                                                               
RCLTOT   XC    SVKEY,SVKEY         INDICATE NO MORE FOR 'NEXT'                  
         CLI   GOLACT0,C'T'        TEST 'TOT'                                   
         BE    TOTLAST                                                          
EXXMOD   XMOD1 1                                                                
*                                                                               
         EJECT                                                                  
RCLGET   LA    RE,GOALREC                                                       
         ST    RE,AREC                                                          
         GOTO1 GETREC                                                           
*                                                                               
         MVC   SVKEY,KEY           SAVE FOR 'NEXT'                              
         LA    R8,BWEEKS           POINT TO WEEK LIST                           
         BR    R9                  GO TO FORMATTING ROUTINE                     
         EJECT                                                                  
RCL      CLI   FRSTSW,0            TEST FIRST LINE                              
         BNE   RCL2                                                             
         MVI   FRSTSW,1                                                         
         XC    KEYSAVE,KEYSAVE     CLEAR FOR FORMAT LOGIC                       
         LA    R2,GOLACT1H                                                      
*                                                                               
         CLI   GOLACT1,C'N'        TEST 'NEXT'                                  
         BNE   *+8                                                              
         AH    R8,SVWKDSP          POINT TO START WEEK FOR 'NEXT'               
*                                                                               
*                                                                               
RCL2     LA    R6,GDELEM                                                        
         MVI   ELCODE,X'40'                                                     
         CLI   SVSCRN,X'FC'                                                     
         BE    *+8                                                              
         MVI   ELCODE,X'41'                                                     
         BAS   RE,NEXTEL                                                        
         BNE   RCL14                                                            
*                                                                               
RCL6     ZAP   HALF,=P'0'          RESET WEEK COUNTER                           
         BAS   RE,FMT                                                           
*                                                                               
RCL8     AP    HALF,=P'1'                                                       
         LA    RE,2(R8)                                                         
         MVC   HALF2,0(RE)         SAVE NEXT BWEEK ENTRY                        
         LR    R3,R6               SAVE ELEMENT ADDRESS                         
*                                                                               
         BAS   RE,NEXTEL                                                        
         BNE   RCL10                                                            
         CLC   0(2,R8),HALF2       IS THIS THE NEXT WEEK                        
         BNE   RCL10               NO-FINISH LAST DISPLAY                       
         CLI   SVSCRN,X'FC'        TEST BUDGET DATA                             
         BE    RCL10               YES - IGNORE                                 
         CLC   4(8,R6),4(R3)       SAME POINTS AND DOLLARS                      
         BNE   RCL10               NO                                           
         B     RCL8                                                             
*                                                                               
* FINISH PREVIOUS DISPLAY LINE                                                  
*                                                                               
RCL10    CP    HALF,=P'0'          TEST NO DATA THIS LINE                       
         BE    RCL12               GO CHECK E-O-R                               
*                                                                               
         CLI   SVSCRN,X'FC'        TEST BUDGET DATA                             
         BE    RCL11               YES                                          
*                                                                               
         EDIT  (P2,HALF),(3,WORK2),ALIGN=LEFT                                   
         MVI   13(R2),C'-'                                                      
         MVC   14(3,R2),WORK2                                                   
         LA    RE,14(R2)                                                        
         AR    RE,R0                                                            
         MVI   0(RE),C'W'                                                       
* POINT TO NEXT DISPLAY LINE                                                    
RCL11    SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         SPACE 2                                                                
RCL12    CLI   0(R2),0             TEST END OF SCREEN                           
         BE    RCLX                                                             
         CLI   0(R6),0             TEST END OF REC                              
         BNE   RCL6                                                             
RCL14    CLC   =C'RM',GOLACT0                                                   
         BE    RCLSEQ                                                           
         XC    SVKEY,SVKEY         CLEAR 'NEXT'                                 
         B     EXXMOD                                                           
*                                                                               
RCLX     LA    R0,BWEEKS           SAVE BWEEK DISPLACEMENT                      
         SR    R8,R0                                                            
         STH   R8,SVWKDSP                                                       
         B     EXXMOD                                                           
         EJECT                                                                  
TOT      CLI   FRSTSW,0            TEST FIRST TIME                              
         BNE   TOT1                                                             
         MVI   FRSTSW,1                                                         
         XC    KEYSAVE,KEYSAVE     CLEAR FOR FORMAT LOGIC                       
         LA    R2,GOLACT1H                                                      
         XC    ELEM(8),ELEM        CLEAR MARKET TOTAL AREA                      
TOT1     LA    R6,GDELEM                                                        
         MVI   ELCODE,X'40'        BRDDOL                                       
         CLI   SVSCRN,X'FC'                                                     
         BE    *+8                                                              
         MVI   ELCODE,X'41'        BRDPCT                                       
         XC    WORK,WORK           CLEAR TOTAL AREA                             
*                                                                               
TOT2     BAS   RE,NEXTEL                                                        
         BNE   TOT4                                                             
         ST    R6,WORK+12          SAVE LAST ELEM ADDRESS                       
         OC    WORK+8(4),WORK+8                                                 
         BNZ   *+8                                                              
         ST    R6,WORK+8           SAVE FIRST ELEM ADDRESS                      
* ADD UP PCTS OR DOLS                                                           
         L     R0,8(R6)            DOLS                                         
         CLI   ELCODE,X'40'                                                     
         BE    *+8                                                              
         L     R0,4(R6)            PCTS                                         
         A     R0,WORK                                                          
         ST    R0,WORK                                                          
         B     TOT2                                                             
*                                                                               
TOT4     L     R6,WORK+8           GET FIRST ELEM ADDRESS                       
         LTR   R6,R6               TEST ACTIVE                                  
         BZ    TOT10                                                            
         LA    RE,8(R6)                                                         
         CLI   SVSCRN,X'FC'                                                     
         BE    *+8                                                              
         LA    RE,4(R6)                                                         
         MVC   0(4,RE),WORK        MOVE TOTALS TO FIRST ELEMENT                 
* ADD TO MARKET TOTALS                                                          
         LM    RE,RF,WORK                                                       
         A     RE,ELEM                                                          
         A     RF,ELEM+4                                                        
         STM   RE,RF,ELEM                                                       
*                                                                               
         MVC   FULL,WORK+12        SAVE LAST ELEMENT ADDRESS                    
         BAS   RE,FMT                                                           
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BNE   TOT6                                                             
         L     RE,BLNADDR          POINT TO ACTION FIELD                        
         XC    9(2,RE),9(RE)       CLEAR TIER                                   
         B     TOT8                                                             
* FORMAT END DATE                                                               
TOT6     L     R6,FULL             GET LAST ELEM ADDRESS                        
         GOTO1 VDATCON,DMCB,(2,2(R6)),(4,WORK2)                                 
         MVI   13(R2),C'-'                                                      
         MVC   14(5,R2),WORK2                                                   
*                                                                               
TOT8     SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BE    EXXMOD                                                           
*                                                                               
TOT10    B     RCLSEQ                                                           
*                                                                               
* DO MARKET TOTALS                                                              
*                                                                               
TOTLAST  CLI   SVSCRN,X'FB'        TEST BRDPCT                                  
         BE    EXXMOD              YES - DONE                                   
         LA    R2,GOLNAM1H         POSITION TO NEXT MARKET NAME FIELD           
         LA    RE,7                                                             
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         BCT   RE,*-6                                                           
*                                                                               
         L     R0,ELEM                                                          
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(8,WORK2),ALIGN=LEFT                                        
         MVC   8(8,R2),=C'TOT DOL='                                             
         MVC   16(8,R2),WORK2                                                   
*                                                                               
         B     EXXMOD                                                           
         EJECT                                                                  
WKS      MVI   ERRCD,MSSNGERR                                                   
         LA    R2,GOLDPT1H                                                      
         CLI   BDPT,0                                                           
         BE    GLERR                                                            
         CLI   BSLN,0                                                           
         BE    GLERR                                                            
         LA    R2,GOLACT1H                                                      
         MVI   ERRCD,INVERR                                                     
         CLI   SVSCRN,X'FC'        TEST BRDDOL INPUT                            
         BE    GLERR                                                            
*                                                                               
         XC    KEYSAVE,KEYSAVE     CLEAR FOR FORMAT LOGIC                       
         LA    R6,GDELEM                                                        
         MVI   ELCODE,X'41'                                                     
         BAS   RE,NEXTEL                                                        
         BE    WKS4                                                             
         B     WKSX                                                             
*                                                                               
WKS2     BAS   RE,NEXTEL                                                        
         BNE   EXXMOD                                                           
*                                                                               
WKS4     BAS   RE,FMT                                                           
         MVC   13(3,R2),=C'-1W'                                                 
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   WKS2                                                             
WKSX     XC    SVKEY,SVKEY                                                      
         B     EXXMOD              NO MORE ROOM                                 
         EJECT                                                                  
* ACTIVITY DATE DISPLAY                                                         
*                                                                               
ACTV     DS    0H                                                               
*                                                                               
         CLI   GKEYDPT,C'Z'        TEST BRDPCT REC                              
         BNE   *+12                NO                                           
         CLI   SVSCRN,X'FC'        TEST BRDDOL DISPLAY                          
         BE    RCLSEQ              YES - SKIP (Z IS A DUMMY DPT)                
*                                                                               
         CLI   FRSTSW,0            TEST FIRST LINE                              
         BNE   ACTV2                                                            
         MVI   FRSTSW,1                                                         
         XC    KEYSAVE,KEYSAVE                                                  
         LA    R2,GOLACT1H                                                      
*                                                                               
ACTV2    BAS   RE,FMT                                                           
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO DOLLARS                             
         XC    8(11,R2),8(R2)                                                   
         FOUT  (R2)                                                             
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO POINTS                              
         XC    8(11,R2),8(R2)                                                   
         FOUT  (R2)                                                             
*                                                                               
ACTV4    DS    0H                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO DATE                                
         MVC   8(5,R2),=C'ACTV='                                                
         GOTO1 VDATCON,DMCB,(2,GACTDATE),(5,13(R2))                             
         FOUT  (R2)                                                             
*                                                                               
         IC    R0,0(R2)            POINT TO NEXT LINE                           
         AR    R2,R0                                                            
         CLI   0(R2),0                                                          
         BNE   RCLSEQ                                                           
         B     EXXMOD                                                           
         EJECT                                                                  
FMT      NTR1                                                                   
         SR    R0,R0                                                            
         ST    R2,BLNADDR          SAVE LINE START ADDRESS                      
         MVC   8(3,R2),=C'*  '     ACTION                                       
         FOUT  (R2)                                                             
*                                                                               
         IC    R0,0(R2)                                                         
         AR    R2,R0               SKIP MARKET                                  
         OI    4(R2),X'20'         SET FLAG                                     
         IC    R0,0(R2)                                                         
         AR    R2,R0               SKIP MARKET NAME                             
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
         CLC   =C'TEST',GOLPLNR                                                 
         BNE   FMT1                                                             
*                                                                               
* FORMAT DISK ADDRESS AT END OF MARKET NAME                                     
*                                                                               
         SR    R2,R0               BACK UP                                      
         MVI   19(R2),C' '                                                      
         GOTO1 =V(HEXOUT),DMCB,KEY+14,16(R2),4,=C'TOG',RR=RB                    
*                                                                               
         AR    R2,R0                                                            
*                                                                               
FMT1     DS    0H                                                               
         CLI   SVSCRN,X'FC'        TEST BRDDOL                                  
         BNE   FMT10               NO                                           
         L     RE,BLNADDR          POINT TO ACTION FIELD                        
         MVC   9(2,RE),6(R6)       MOVE TIER TO ACTION FIELD                    
* BRDDOL                                                                        
         CLC   KEY(11),KEYSAVE     TEST SAME DPT/LN                             
         BE    FMT2                                                             
         MVC   KEYSAVE,KEY         FORCE EQUAL NEXT TIME                        
         MVC   8(1,R2),GKEYDPT                                                  
         MVC   9(3,R2),SPACES                                                   
         SR    R0,R0                                                            
         IC    R0,GKEYSLN                                                       
         EDIT  (R0),(3,WORK2),ALIGN=LEFT                                        
         MVC   9(3,R2),WORK2                                                    
FMT1X    FOUT  (R2)                                                             
*                                                                               
FMT2     OI    4(R2),X'20'         SET FLAG                                     
         CLC   =C'RA',GOLACT0                                                   
         BE    FMTX                                                             
*                                                                               
         SR    R0,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         L     R0,8(R6)            DOLLARS                                      
         SRDA  R0,32                                                            
         D     R0,=F'100'                                                       
         LR    R0,R1                                                            
         EDIT  (R0),(9,WORK2),ALIGN=LEFT,ZERO=NOBLANK                           
         MVC   8(9,R2),WORK2                                                    
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0               POINT TO PERIOD                              
         B     FMTX                                                             
         SPACE 2                                                                
* BRDPCT DATA                                                                   
*                                                                               
FMT10    CLC   =C'RA',GOLACT0                                                   
         BE    FMTX                                                             
*                                                                               
         MVC   8(4,R2),SPACES      FORMAT SLN                                   
         SR    R0,R0                                                            
         IC    R0,GKEYSLN                                                       
         EDIT  (R0),(3,WORK2),ALIGN=LEFT                                        
         MVC   8(3,R2),WORK2                                                    
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET FLAG                                     
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0               POINT TO PERCENT                             
*                                                                               
         L     R0,4(R6)            GET WEEKLY PCT                               
         EDIT  (R0),(7,WORK2),2,ALIGN=LEFT                                      
         LA    R4,8(R2)                                                         
         CLI   GOLACT0,C'T'                                                     
         BNE   FMT12                                                            
         MVC   0(4,R4),=C'TOT='                                                 
         LA    R4,4(R4)                                                         
FMT12    MVC   0(7,R4),WORK2                                                    
         OC    8(11,R2),SPACES                                                  
         FOUT  (R2)                                                             
         OI    4(R2),X'20'                                                      
*                                                                               
         ZIC   R0,0(R2)                                                         
         AR    R2,R0                                                            
         IC    R0,0(R2)                                                         
         AR    R2,R0                                                            
*                                                                               
         GOTO1 VDATCON,DMCB,(2,2(R6)),(4,WORK2)                                 
         MVC   8(5,R2),WORK2                                                    
         OC    8(13,R2),SPACES                                                  
         FOUT  (R2)                                                             
         OI    4(R2),X'20'         SET FLAG                                     
*                                                                               
FMTX     XIT1  REGS=(R2)                                                        
         EJECT                                                                  
* R6 POINTS TO PREVIOUS ELEMENT                                                 
* R8 POINTS TO PREVIOUS BWEEKS ENTRY                                            
*                                                                               
NEXTEL   SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BE    NEXTELX                                                          
*                                                                               
         CLC   ELCODE,0(R6)                                                     
         BNE   NEXTEL                                                           
*                                                                               
NEXTEL2  OC    0(2,R8),0(R8)       TEST END OF WEEK LIST                        
         BZ    NEXTEL              YES - SKIP TO END OF REC                     
*                                                                               
         CLC   2(2,R6),0(R8)       THIS WEEK TO WEEK LIST                       
         BL    NEXTEL                                                           
         BER   RE                                                               
         LA    R8,2(R8)            NEXT WEEK IN LIST                            
         B     NEXTEL2                                                          
*                                                                               
NEXTELX  LTR   RE,RE               EXIT WITH CC NOT=                            
         BR    RE                                                               
         SPACE 2                                                                
GLERR    GOTO1 ERROR                                                            
         LTORG                                                                  
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE SPGOLWRK                                                       
         PRINT ON                                                               
 END                                                                            
